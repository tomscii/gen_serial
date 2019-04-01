#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef WIN32
# include <windows.h>
 typedef u_long	uint32_t;
#else
# include <netinet/in.h>
#endif

#include "erlang_serial.h"

#ifdef _DEBUG
FILE*	debug_log_file = NULL;
#endif

int serial_init_struct (struct serial_channel* chan, struct serial_cfg *cfg)
{
	memcpy(&chan->config, cfg, sizeof(struct serial_cfg));

	return 0;
}

int serial_destroy_struct (struct serial_channel* chan)
{
	return 0;
}

static void packetscan_none (struct serial_port* port)
{
	int		avail;

	avail = port->incomingLen - port->incomingSent;
	if (avail > 0)
	{
		uint32_t	plen;

		port->incomingCurHeaderLen = 5;
		port->incomingCurHeaderLeft = 5;
		port->incomingCurPacketLeft = avail;

		plen = htonl(avail + 1);
		memcpy(port->incomingHeader, &plen, 4);
		port->incomingHeader[4] = PACKET_DATA;
	}

	port->incomingScan += avail;
}

struct serial_port* serial_port_create (
	struct serial_channel*	chan,
	struct pipe_channel*	erts,
	int						bufsz)
{
	struct serial_port*		port;

	port = (struct serial_port*)malloc(sizeof(struct serial_port));
	memset(port, 0, sizeof(struct serial_port));

	port->chan = chan;
	port->erts = erts;
	port->packet_scanner = packetscan_none;

	port->incomingHeader = (char*)malloc(512);

	port->incomingBufSz = bufsz;
	port->incoming = (char*)malloc(port->incomingBufSz);
	
	port->outgoingBufSz = bufsz;
	port->outgoing = (char*)malloc(port->outgoingBufSz);

	if (chan)
	{
		port->sendOkFor = PACKET_OPEN;

	} else
	{
		port->lastError.error_code = serial_last_error(port->chan);
		serial_format_error(
			port->chan,
			port->lastError.error_code,
			port->lastError.error_msg,
			sizeof(port->lastError.error_msg));
	}

	return port;
}

void serial_port_destroy (struct serial_port* port)
{
	if (port->erts)
	{
		pipe_close(port->erts);
	}

	if (port->chan)
	{
		serial_close(port->chan);
	}

	free(port->outgoing);
	free(port->incoming);
	free(port->incomingHeader);
	free(port);
}

/* Frees space in the incoming buffer, trying to make room for at a chunk
 * of data at least target bytes in size.
 *
 * Target cannot be larger than the buffer size itself, or else the truncate
 * will never get enough room.
 */
static void truncate_incoming (struct serial_port* port, int target)
{
	int		freespc;
	int		avail;

	/* If the incoming buffer is already truncated, we can't do
	 * anything more.
	 */
	if (port->incomingSent == 0)
	{
		return;
	}

	/* If the buffer is empty, reset it so it really is in fact fully
	 * empty.  This makes the maximum amount of room for reading.
	 */
	if (port->incomingSent == port->incomingLen)
	{
		port->incomingSent = 0;
		port->incomingLen = 0;
		port->incomingScan = 0;
		port->incomingMatch = 0;
		return;
	}

	/* If the buffer does not have a packet active in it, both Left fields
	 * will be 0.  The buffer cannot be truncated if there is an active
	 * packet stored in the buffer, as the pipe_write call may be performing
	 * IO from the buffer.
	 */
	if (   port->incomingCurHeaderLeft > 0
		|| port->incomingCurPacketLeft > 0)
	{
		return;
	}

	avail = port->incomingLen - port->incomingSent;
	freespc = port->incomingBufSz - port->incomingLen;

	if ((target - avail) > freespc)
	{
		memcpy(
			port->incoming,
			port->incoming + port->incomingSent,
			avail);

		port->incomingMatch -= port->incomingSent;
		port->incomingScan -= port->incomingSent;
		port->incomingLen = avail;
		port->incomingSent = 0;
	}
}

static void packetscan_fixedsize (struct serial_port* port)
{
	int		avail;
	int		sz;

	/* packet_format is negative when using fixedsize scanning.
	 * A packet can be sent if there is enough data for it.
	 */
	sz = - port->chan->config.packet_format;
	avail = port->incomingLen - port->incomingSent;
	if (avail >= sz)
	{
		uint32_t	plen;

		port->incomingCurHeaderLen = 5;
		port->incomingCurHeaderLeft = 5;
		port->incomingCurPacketLeft = sz;
		port->incomingScan += sz;

		plen = htonl(sz + 1);
		memcpy(port->incomingHeader, &plen, 4);
		port->incomingHeader[4] = PACKET_DATA;

	} else
	{
		port->incomingScan += avail;
		truncate_incoming(port, sz);
	}
}

static void packetscan_delim (struct serial_port* port)
{
	char*		delim;
	int			delim_len;
	int			avail;
	int			a;
	int			b;
	uint32_t	plen;

	delim_len = port->chan->config.packet_delim_len;

	/* If a packet was just sent, update incomingSent to include
	 * the delimiter which was just skipped over.
	 */
	if (port->incomingSentPacket)
	{
		port->incomingSent += delim_len;
		port->incomingMatch += delim_len;
		port->incomingScan += delim_len;
		port->incomingSentPacket = 0;
	}

	/* Unlike most of the scanners, avail here counts from the match
	 * marker (the last position examined for a match) rather than
	 * from the sent marker.
	 */
	avail = port->incomingLen - port->incomingMatch;
	DBG_LOG(fprintf(debug_log_file,
		"packetscan_delim:\n"
		"    Config: delim_len=%i\n"
		"    Buffer: avail=%i match=%i\n",
		delim_len,
		avail, port->incomingMatch));

	/* If there is not enough data in the buffer to hold the
	 * delimiter, try to truncate the buffer.  Make sure to increment
	 * the scan pointer otherwise the event loop will keep rescanning
	 * the buffer.
	 */
	if (avail < delim_len)
	{
		port->incomingScan += avail;
		truncate_incoming(
			port,
			(port->incomingLen - port->incomingSent) + delim_len
		);
		return;
	}

	delim = port->chan->config.packet_delim;
	a = port->incomingMatch;

	/* Scan through the available input bytes between where this scanner
	 * last left off (incomingMatch) and the end of the available data.
	 * Each byte is checked against the delimiter to see if it matches.
	 */
outer:
	while (avail >= delim_len)
	{
		/* Loop through the delimiter checking to see if it matches
		 * against the input buffer.
		 */
		for (b = 0; b < delim_len; b++)
		{
			/* At the first nonmatching delimiter character, move
			 * incomingMatch to point to the first nonmatching position
			 * in the incoming buffer.  The scan will restart at that
			 * position.
			 */
			if (port->incoming[a + b] != delim[b])
			{
				b++;
				a = (port->incomingMatch += b);
				avail -= b;
				goto outer;
			}
		}

		/* A match was found.  Create the packet.  The end of the packet
		 * is at offset 'a'.
		 */
		plen = a - port->incomingSent;
		DBG_LOG(fprintf(debug_log_file,
			"    Packet: start=%i end=%i len=%i\n",
			port->incomingSent, a, plen));

		port->incomingCurHeaderLen = 5;
		port->incomingCurHeaderLeft = 5;
		port->incomingCurPacketLeft = plen;

		plen = htonl(plen + 1);
		memcpy(port->incomingHeader, &plen, 4);
		port->incomingHeader[4] = PACKET_DATA;
		
		/* Update scan and match to indicate where in the buffer this
		 * scanner stopped off at.  They should now be at the same
		 * position, as the scanner will need to restart scanning
		 * again after the current packet is sent.
		 */
		port->incomingScan = a;
		port->incomingMatch = a;
		port->incomingSentPacket = 1;

		/* Break out of the outer loop and return to the caller.  The
		 * scanner cannot continue when a packet exists and is being
		 * sent.
		 */
		return;
	}

	/* No match was found in the buffer thus far.  Sent incomingScan
	 * to incomingLen to show that we have scanned all data available,
	 * but are not ready to transmit yet.
	 */
	truncate_incoming(
		port,
		(port->incomingLen - port->incomingSent) + delim_len
	);
	port->incomingScan = port->incomingLen;
}

static void scan_incoming (struct serial_port* port)
{
	/* If there is no data to scan, or there currently is a packet being
	 * transmitted to Erlang, the scanner must abort, as there is nothing
	 * that can be done at this time.
	 */
	if (   port->incomingLen == port->incomingScan
		|| port->incomingLen == port->incomingSent
		|| port->incomingCurHeaderLeft > 0
		|| port->incomingCurPacketLeft > 0)
	{
		return;
	}

DBG_LOG(fprintf(debug_log_file,
		"scan incoming:\n"
		"       Buffer: len=%i sent=%i scan=%i avail=%i toscan=%i\n",
		port->incomingLen, port->incomingSent,
		port->incomingScan,
		port->incomingLen - port->incomingSent,
		port->incomingLen - port->incomingScan));

	port->packet_scanner(port);
}

/* Frees space in the outgoing buffer, trying to make room for at a chunk
 * of data at least target bytes in size.
 *
 * The truncate won't occur if the current packet is a DRAIN or a DATA
 * packet, as these cannot be moved around within the buffer.  Target
 * cannot be larger than the buffer size itself, or else the truncate
 * will never get enough room.
 */
static void truncate_outgoing (struct serial_port* port, int target)
{
	int		freespc;
	int		avail;

	/* If the outgoing buffer is already truncated, we can't do
	 * anything more.
	 */
	if (port->outgoingSent == 0)
	{
		return;
	}

	/* If the buffer is empty, reset it so it really is in fact fully
	 * empty.  This makes the maximum amount of room for reading.
	 */
	if (port->outgoingSent == port->outgoingLen)
	{
		port->outgoingSent = 0;
		port->outgoingLen = 0;
		return;
	}

	/* If the buffer is not currently looking at a data packet, the buffer
	 * won't be in use by the serial port async IO subsystem.  So we can
	 * move the buffer data around to make more free space if the buffer
	 * is completely full.  This is important to do, as control packets
	 * must be put entirely into the buffer before they can get processed.
	 *
	 * For performance reasons, try to only do the move when we need to
	 * in order to fit the target packet size supplied in target.  This is
	 * the expected number of bytes the caller wants to have in the buffer
	 * all at once.  Take into account the number of bytes already available
	 * in the buffer, as the caller might not have wanted those yet since
	 * the buffer is short.
	 */
	if (    PACKET_DATA == port->outgoingPacketType
		|| PACKET_DRAIN == port->outgoingPacketType)
	{
		return;
	}

	avail = port->outgoingLen - port->outgoingSent;
	freespc = port->outgoingBufSz - port->outgoingLen;

	if (0 == target)
	{
		target = port->outgoingPacketLen;
	}

	if ((target - avail) > freespc)
	{
		memcpy(
			port->outgoing,
			port->outgoing + port->outgoingSent,
			avail);

		port->outgoingLen = avail;
		port->outgoingSent = 0;
	}
}

static void process_outgoing (struct serial_port* port)
{
	int			avail;
	int			r;

process_type:
	avail = port->outgoingLen - port->outgoingSent;
	if ((0 == avail) && (port->outgoingPacketType == PACKET_NONE))
	{
		return;
	}

DBG_LOG(fprintf(debug_log_file,
		"process_outgoing:\n"
		"       Buffer: len=%i sent=%i avail=%i\n"
		"       Packet: type=%c len=%i left=%i\n",
		port->outgoingLen, port->outgoingSent, avail,
		port->outgoingPacketType, port->outgoingPacketLen,
		port->outgoingPacketLeft
		));

	switch (port->outgoingPacketType)
	{
	case PACKET_NONE:
		/* If the number of bytes available in the buffer is at least 5,
		 * we can begin to process this packet.  We don't use avail > 5 here
		 * because some packets may in fact have no payload, just the type.
		 */
		if (avail >= 5)
		{
			uint32_t	plen;

			memcpy(&plen, port->outgoing + port->outgoingSent, 4);
			port->outgoingSent += 4;
			
			port->outgoingPacketLen = (int)(ntohl(plen) - 1);
			port->outgoingPacketLeft = port->outgoingPacketLen;
			port->outgoingPacketType = port->outgoing[port->outgoingSent++];

			/* If for some reason we get a PACKET_NONE type packet, this
			 * is an error.  Switch it to PACKET_DRAIN so we can drain it
			 * off the line.
			 */
			if (PACKET_NONE == port->outgoingPacketType)
			{
				port->outgoingPacketType = PACKET_DRAIN;
			}

			/* Loop to process the newly discovered packet type.  We
			 * may have enough data to process part of the packet by
			 * now.
			 */
			goto process_type;

		} else
		{
			truncate_outgoing(port, 5);
		}
		break;

	case PACKET_ACTIVE:
		if (port->outgoingPacketLen != 1)
		{
			port->lastError.error_code = ERROR_BAD_LENGTH;
			serial_format_error(
				port->chan,
				port->lastError.error_code,
				port->lastError.error_msg,
				sizeof(port->lastError.error_msg));

			port->outgoingPacketType = PACKET_DRAIN;
			goto process_type;
		}

		if (avail >= 1)
		{
			port->erlangPortActive = port->outgoing[port->outgoingSent];
			port->outgoingSent++;
			port->outgoingPacketType = PACKET_NONE;
			port->outgoingPacketLen = 0;
			port->outgoingPacketLeft = 0;
			goto process_type;

		} else
		{
			truncate_outgoing(port, 1);
		}
		break;

	case PACKET_CONFIG:
		/* If the packet length does not exactly match the size of
		 * the serial_cfg structure this program uses, drain the packet
		 * off the line.  There's no way it can be processed by this
		 * code.
		 */
		if (port->outgoingPacketLen != sizeof(struct serial_cfg))
		{
			port->lastError.error_code = ERROR_BAD_LENGTH;
			serial_format_error(
				port->chan,
				port->lastError.error_code,
				port->lastError.error_msg,
				sizeof(port->lastError.error_msg));

			port->outgoingPacketType = PACKET_DRAIN;
			goto process_type;
		}

		/* If the configuration packet is fully contained within the
		 * outgoing buffer (and it always will fit as the buffer is
		 * always bigger), process the config packet now.
		 */
		if (avail >= sizeof(struct serial_cfg))
		{
			struct serial_cfg	cfg;
			char*				magic = SERIAL_CFG_MAGIC;

			memcpy(
				&cfg,
				port->outgoing + port->outgoingSent,
				sizeof(struct serial_cfg));

			if (memcmp(cfg.magic, magic, strlen(magic) + 1) == 0)
			{
				if (serial_configure(port->chan, &cfg) == -1)
				{
					port->lastError.error_code = serial_last_error(port->chan);
					serial_format_error(
						port->chan,
						port->lastError.error_code,
						port->lastError.error_msg,
						sizeof(port->lastError.error_msg));

				} else
				{
					/* If the caller supplied a valid value for the active
					* setting, update to it.
					*/
					switch (cfg.initially_active)
					{
					case PORT_ACTIVE_FALSE:
					case PORT_ACTIVE_TRUE:
					case PORT_ACTIVE_ONCE:
						port->erlangPortActive = cfg.initially_active;
						break;
					}

					/* Modify the packet scanner to match the setting supplied.
					*/
					if (SERIAL_PACKET_NONE == cfg.packet_format)
					{
						port->packet_scanner = packetscan_none;
						port->incomingScan = port->incomingSent;
						DBG_LOG(fprintf(debug_log_file, "scanner = none\n"));

					} else if (SERIAL_PACKET_DELIM == cfg.packet_format)
					{
						port->packet_scanner = packetscan_delim;
						port->incomingScan = port->incomingSent;
						DBG_LOG(fprintf(debug_log_file, "scanner = delim len=%i\n", port->chan->config.packet_delim_len));

					} else if (cfg.packet_format < 0)
					{
						port->packet_scanner = packetscan_fixedsize;
						port->incomingScan = port->incomingSent;
						DBG_LOG(fprintf(debug_log_file, "scanner = fixedsize %i\n", -port->chan->config.packet_format));
					}

					port->sendOkFor = PACKET_CONFIG;
				}

			} else
			{
				/* Magic header doesn't match what we use for a
				 * config packet. The packet is the wrong version.
				 * This is an error.
				 */
				port->lastError.error_code = ERROR_INVALID_DATA;
				serial_format_error(
					port->chan,
					port->lastError.error_code,
					port->lastError.error_msg,
					sizeof(port->lastError.error_msg));
			}

			port->outgoingSent += sizeof(struct serial_cfg);
			port->outgoingPacketType = PACKET_NONE;
			port->outgoingPacketLen = 0;
			port->outgoingPacketLeft = 0;
			goto process_type;

		} else
		{
			truncate_outgoing(port, sizeof(struct serial_cfg));
		}
		break;

	case PACKET_CLOSE:
	case PACKET_FLUSH:
		/* If either a close or a flush messages sent sent down, send a
		 * reply.  erts is basically just waiting for the reply before
		 * it will close the pipes down, and this process gets EOF on
		 * the pipes.  Don't attempt to close down anything until
		 * EOF is received on the erts pipes.
		 */
		port->sendOkFor = port->outgoingPacketType;
		port->outgoingPacketType = PACKET_NONE;
		port->outgoingPacketLen = 0;
		port->outgoingPacketLeft = 0;
		goto process_type;

	case PACKET_DATA:
		/* If the buffer has more data than the packet has remaining, only
		 * send the number of bytes in the packet.  Otherwise, send what
		 * is currently in the buffer.
		 */
		if (avail > port->outgoingPacketLeft)
		{
			r = port->outgoingPacketLeft;
		} else
		{
			r = avail;
		}

		/* Attempt to write to the serial port.  This may fail and return -1
		 * (EAGAIN), meaning the port is too busy to write this data now.
		 * A future call to this function will recall write with the same
		 * parameter values.
		 */
		r = serial_write(port->chan, port->outgoing + port->outgoingSent, r);
		if (r >= 0)
		{
			port->outgoingSent += r;
			port->outgoingPacketLeft -= r;

		} else if (serial_last_error(port->chan))
		{
			port->lastError.error_code = serial_last_error(port->chan);
			serial_format_error(
				port->chan,
				port->lastError.error_code,
				port->lastError.error_msg,
				sizeof(port->lastError.error_msg));

			port->outgoingPacketType = PACKET_DRAIN;
		}

		/* If the packet is completed, continue to process what is in the
		 * outgoing buffer.
		 */
		if (0 == port->outgoingPacketLeft)
		{
			port->outgoingPacketType = PACKET_NONE;
			port->outgoingPacketLen = 0;
			goto process_type;
		}
		break;

	case PACKET_DRAIN:
	default:
		/* If the packet type was not recognized, we need to throw the packet
		 * away.  This requires consuming all of its data, but not actually
		 * handling it.  Here we attempt to throw out as much of the packet
		 * as we can.  If we've thrown all of it, we switch back to
		 * PACKET_NONE and try to process the buffer again.
		 */
		if (port->outgoingPacketLeft > 0)
		{
			if (avail >= port->outgoingPacketLeft)
			{
				port->outgoingSent += port->outgoingPacketLeft;
				port->outgoingPacketType = PACKET_NONE;
				port->outgoingPacketLen = 0;
				port->outgoingPacketLeft = 0;
				goto process_type;

			} else
			{
				port->outgoingSent += avail;
				port->outgoingPacketLeft -= avail;
			}

		} else if (0 == port->outgoingPacketLeft)
		{
			port->outgoingPacketType = PACKET_NONE;
			port->outgoingPacketLen = 0;
			goto process_type;
		}
		break;
	}
}

void serial_port_ertsr (struct serial_port* port)
{
	int			r;

	DBG_LOG(fprintf(debug_log_file, "event: ertsr\n"));

	/* Try to read data from erts.  This event was called because
	 * we believe there may be data available now which needs to
	 * be copied into the outgoing buffer.
	 */
	r = pipe_read(
		port->erts,
		port->outgoing + port->outgoingLen,
		port->outgoingBufSz - port->outgoingLen);
	DBG_LOG(fprintf(debug_log_file, "ertsr: pipe_read = %i\n", r));

	/* If new data was received during this call, update the buffer
	 * state.  If the serial port is writable, the packets will be
	 * processed, but not until that event is fired.  Its bad form
	 * to try and write to the serial port if its not ready (at least
	 * on the Windows implementations).
	 */
	if (r > 0)
	{
		port->outgoingLen += r;
		return;
	}

	port->isDead = 1; /* EOF or read error */
	if (pipe_last_error(port->erts))
	{
		int	ecode;
		char	msg[256];

		ecode = pipe_last_error(port->erts);
		pipe_format_error(port->erts, ecode, msg, sizeof(msg));
		DBG_LOG(fprintf(debug_log_file, "pipe_read error: %i %s\n",
			ecode, msg));
	}
}

void serial_port_ertsw (struct serial_port* port)
{
	int		r;

	DBG_LOG(fprintf(debug_log_file, "event: ertsw\n"));

	/* If no packet is in transit, scan the incoming buffer to try
	 * and get another packet ready for transmission.  This is done
	 * before attempting to do the writes below, so that if a packet
	 * is scanned out it can be sent immediately.
	 */
	if (   0 == port->incomingCurHeaderLeft
		&& 0 == port->incomingCurPacketLeft)
	{
		if (port->lastError.error_code)
		{
			uint32_t	plen;

			DBG_LOG(fprintf(debug_log_file, "sending error %i %s\n",
				port->lastError.error_code,
				port->lastError.error_msg));

			plen = sizeof(port->lastError.error_code);
			plen += (uint32_t)strlen(port->lastError.error_msg);
			
			port->incomingCurHeaderLen = 5 + plen;
			port->incomingCurHeaderLeft = 5 + plen;
			port->incomingCurPacketLeft = 0;

			plen = htonl(plen + 1);
			memcpy(port->incomingHeader, &plen, 4);
			port->incomingHeader[4] = PACKET_ERROR;
			memcpy(
				port->incomingHeader + 5,
				&port->lastError,
				sizeof(port->lastError));

			/* In case of a non-recoverable error, signal the port as dead.
			 * This will cause the port driver process to exit.
			 */
			if (is_fatal_error(port->lastError.error_code)) {
			        port->isDead = 1;
			}

			port->lastError.error_code = 0;
			port->lastError.error_msg[0] = 0;

			DBG_LOG(fprintf(debug_log_file, "    Packet: hleft=%i dleft=%i\n",
				port->incomingCurHeaderLeft, port->incomingCurPacketLeft));

		} else if (port->sendOkFor)
		{
			uint32_t	plen;

			port->incomingCurHeaderLen = 6;
			port->incomingCurHeaderLeft = 6;
			port->incomingCurPacketLeft = 0;

			plen = htonl(2);
			memcpy(port->incomingHeader, &plen, 4);
			port->incomingHeader[4] = PACKET_OK;
			port->incomingHeader[5] = port->sendOkFor;

			port->sendOkFor = 0;
			
		} else
		{
			scan_incoming(port);
		}
	}

	/* If the header has not finished writing yet, try to write
	 * both the header and some packet data at once.  This is
	 * faster on OS implementations that allow us to use a
	 * gather write event, or something like it.
	 */
	if (port->incomingCurHeaderLeft > 0)
	{
		int		headerSent = port->incomingCurHeaderLen - port->incomingCurHeaderLeft;

		r = pipe_write2(
			port->erts,
			port->incomingHeader + headerSent,
			port->incomingCurHeaderLeft,
			port->incoming + port->incomingSent,
			port->incomingCurPacketLeft);
		DBG_LOG(fprintf(debug_log_file, "pipe_write2=%i\n", r));

		if (r >= 0)
		{
			if (r >= port->incomingCurHeaderLeft)
			{
				/* If the number of bytes written is more than what
				 * was left of the header, the header has been
				 * finished and only the data part remains.
				 */
				r -= port->incomingCurHeaderLeft;

				port->incomingCurHeaderLeft = 0;
				port->incomingCurPacketLeft -= r;
				port->incomingSent += r;

			} else
			{
				/* Less than the header was written, so just update
				 * the header count.  All of the data remains.
				 */
				port->incomingCurHeaderLeft -= r;
			}

		} else if (pipe_last_error(port->erts))
		{
			int		ecode;
			char	msg[256];

			ecode = pipe_last_error(port->erts);
			pipe_format_error(port->erts, ecode, msg, sizeof(msg));
			DBG_LOG(fprintf(debug_log_file, "pipe_write2 error: %i %s\n",
				ecode, msg));
			port->isDead = 1;
			return;
		}

	/* Only packet data is left now.  Its suitable to use just
	 * a single write operation on the one buffer at this
	 * point.
	 */
	} else if (port->incomingCurPacketLeft > 0)
	{
		r = pipe_write(
			port->erts,
			port->incoming + port->incomingSent,
			port->incomingCurPacketLeft);
		DBG_LOG(fprintf(debug_log_file, "    pipe_write=%i\n", r));

		if (r > 0)
		{
			port->incomingCurPacketLeft -= r;
			port->incomingSent += r;

		} else if (pipe_last_error(port->erts))
		{
			int		ecode;
			char	msg[256];

			ecode = pipe_last_error(port->erts);
			pipe_format_error(port->erts, ecode, msg, sizeof(msg));
			DBG_LOG(fprintf(debug_log_file, "pipe_write error: %i %s\n",
				ecode, msg));
			port->isDead = 1;
			return;
		}
	}

	/* If the buffer is totally empty, or has been fully
	 * transmitted, mark it empty.  This prevents a condition
	 * occuring where the incoming buffer fills up and won't
	 * get cleared.
	 */
	if (   port->incomingLen > 0
		&& port->incomingSent == port->incomingLen)
	{
		port->incomingSent = 0;
		port->incomingLen = 0;
		port->incomingMatch = 0;
		port->incomingScan = 0;
	}
}

void serial_port_serialr (struct serial_port* port)
{
	int			r;

	DBG_LOG(fprintf(debug_log_file, "event: serialr\n"));

	/* The serial port has data ready.  Load into the incoming buffer.
	 * Only make one call here, as some operating systems (like the
	 * Windows implementation) won't quite work properly if multiple
	 * serial_read calls were made in one pass in an attempt to fill
	 * the buffer.  On systems like UNIX where you can make read calls
	 * until errno == EAGAIN the loop should be implemented within
	 * serial_read itself.
	 */
	r = serial_read(
		port->chan,
		port->incoming + port->incomingLen,
		port->incomingBufSz - port->incomingLen);

	if (r >= 0)
	{
		port->incomingLen += r;

	} else if (serial_last_error(port->chan))
	{
		port->lastError.error_code = serial_last_error(port->chan);
		serial_format_error(
			port->chan,
			port->lastError.error_code,
			port->lastError.error_msg,
			sizeof(port->lastError.error_msg));
	}
}

void serial_port_serialw (struct serial_port* port)
{
	DBG_LOG(fprintf(debug_log_file, "event: serialw\n"));

	/* Serial port is ready to be written to. This means that
	 * either the current outgoing packet has been fully transmitted
	 * and can be removed from the buffer, or the port is ready for
	 * writing and the pending packet(s) can be written out.
	 */
	process_outgoing(port);
	
	/* If the buffer has been fully processed, empty it.  This makes
	 * maximum room for the read about to happen.
	 */
	if (   port->outgoingLen > 0
		&& port->outgoingSent == port->outgoingLen)
	{
		port->outgoingLen = 0;
		port->outgoingSent = 0;
	}
}

int serial_port_watch_ertsr (struct serial_port* port)
{
	/* Reading from erts is only allowed when there
	 * is free space in the buffer to put the data into.
	 */
	return port->outgoingLen < port->outgoingBufSz
		|| port->outgoingSent == port->outgoingLen;
}

int serial_port_watch_ertsw (struct serial_port* port)
{
	int		canSend;
	int		inPacket;
	int		haveError;

	canSend = port->erlangPortActive
		&& port->incomingScan < port->incomingLen;
	
	inPacket = port->incomingCurHeaderLeft > 0
		|| port->incomingCurPacketLeft > 0;

	haveError = port->lastError.error_code || port->sendOkFor;

	return canSend || inPacket || haveError;
}

int serial_port_watch_serialr (struct serial_port* port)
{
	if ( ! port->chan)
	{
		return 0;
	}

	/* Serial port reading is only allowed when there is
	 * free space within the incoming buffer to store the
	 * data for scanning and transmission.
	 */
	return port->incomingLen < port->incomingBufSz
		|| port->incomingSent == port->incomingLen;
}

int serial_port_watch_serialw (struct serial_port* port)
{
	if ( ! port->chan)
	{
		return 0;
	}

	/* Writing to the serial port should be monitored if there
	 * is data in the outgoing buffer.  Not all of the data in
	 * the buffer may be sent to the serial port, but typically
	 * that is what the buffer contains.
	 */
	return port->outgoingSent < port->outgoingLen;
}

int serial_port_isdead (struct serial_port* port)
{
	return port->isDead;
}
