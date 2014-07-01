#include "stdafx.h"

#include "erlang_serial.h"
#include "win32serial.h"

#define RXSTATE_NONE	0
#define RXSTATE_WAIT	1
#define RXSTATE_READ	2

#define TXSTATE_NONE	0
#define TXSTATE_WRITE	1

struct serial_win32 {
	struct serial_channel	channel;

	/* Memory address of the buffer currently being written out, or NULL if no
	 * write operation is in progress.  This is a user provided buffer, the
	 * channel does not allocate or free it.
	 */
	char*					txBuf;

	/* Memory address of the read buffer.  Allocated within the channel and
	 * is kept private to the channel.
	 */
	char*					rxBuf;

	/* The serial port.  The port is opened in overlapped IO mode, so all IO
	 * operations must be done with OVERLAPPED structures.
	 */
	HANDLE					hPort;

	/* The serial port has data ready for reading.  The application should
	 * use ReadFile to pull the data from the port.  This is a manual reset
	 * event which is reset before WaitCommEvent or ReadFile.
	 */
	HANDLE					eRxReady;

	/* The serial port has finished writing the user's data buffer.  The
	 * user can now recycle the buffer, and can send a new buffer to
	 * serial_write if necessary.
	 */
	HANDLE					eTxDone;

	/* The following two OVERLAPPED structures are used for receive and
	 * transmit operations.  There are only two structures here as we only
	 * permit one outgoing and one incoming operation at a time.
	 */
	OVERLAPPED				rxOverlapped;
	OVERLAPPED				txOverlapped;

	/* Error of the last call.  This is directly from GetLastError().
	 */
	DWORD					lastError;

	/* Mask parameter used by WaitCommEvent.  This is stored in a struct
	 * rather than on the local stack to prevent corruption due to the
	 * background processing.  We don't know if Windows modifies this
	 * field while the IO operation is pending.
	 */
	DWORD					rxMask;

	/* Last number of bytes read by the last ReadFile call.  This is in
	 * a struct rather than on the stack to prevent corruption if the
	 * ReadFile goes into a pending state rather than returning immediately.
	 * We can't be sure the Windows kernel won't update this field on us
	 * while the operation is pending.
	 */
	DWORD					rxLastCount;

	/* Last number of bytes read by the last WriteFile call.  This is in
	 * a struct rather than on the stack to prevent corruption if the
	 * WriteFile goes into a pending state rather than returning immediately.
	 * We can't be sure the Windows kernel won't update this field on us
	 * while the operation is pending.
	 */
	DWORD					txLastCount;

	/* Current state of the receive operations.  Tells us which operation we
	 * is currently pending, or if no operation is running.  See RXSTATE_*
	 * constants in this file.
	 */
	int						rxState;

	/* Current state of the transmit operations.  Tells us which operation
	 * is currently pending, or if no operation is running.  See TXSTATE_*
	 * constants in this file.
	 */
	int						txState;

	/* Total number of bytes which are in txBuf that must be written out.
	 */
	int						txBufLen;

	/* Total number of bytes already sent and confirmed.  This is updated each
	 * time eTxDone is signaled, and the write ends when txSentLen == txBufLen.
	 * When txSentLen == txBufLen txBuf is set to NULL and eTxDone is manually
	 * fired.
	 */
	int						txSentLen;

	/* Total size of the rxBuf in bytes.  This is how many bytes were allocated
	 * to rxBuf when rxBuf was created.
	 */
	int						rxBufSz;

	/* Number of bytes recently loaded into rxBuf.  This is not the number of
	 * bytes available to the application, to get that subtract rxRecvLen from
	 * rxBufLen.
	 */
	int						rxBufLen;

	/* Number of bytes in rxBuf which have been consumed by the application.
	 * This indicates the offset within rxBuf where the next byte will be.
	 */
	int						rxRecvLen;
};

HANDLE serial_os_readable (struct serial_channel* _chan)
{
	struct serial_win32*	chan = (struct serial_win32*)_chan;

	return chan->eRxReady;
}

HANDLE serial_os_writeable (struct serial_channel* _chan)
{
	struct serial_win32*	chan = (struct serial_win32*)_chan;

	return chan->eTxDone;
}

int serial_last_error (struct serial_channel* _chan)
{
	struct serial_win32*	chan = (struct serial_win32*)_chan;

	if ( ! chan)
	{
		return GetLastError();
	}

	return ERROR_SUCCESS == chan->lastError ? 0 : chan->lastError;
}

void serial_format_error (
	struct serial_channel*	chan,
	int						ecode,
	char*					msgbuf,
	int						msgbuf_len)
{
	DWORD	r;
	
	r = FormatMessage(
		  FORMAT_MESSAGE_FROM_SYSTEM
		| FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		(DWORD)ecode,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPTSTR)msgbuf,
		msgbuf_len - 1,
		NULL);
	msgbuf[r] = 0;

	/* If the message ends with a CRLF pair, which is common with
	 * Windows error messages, remove the CRLF pair.
	 */
	if (r > 2)
	{
		if (   msgbuf[r - 2] == '\r'
			&& msgbuf[r - 1] == '\n')
		{
			msgbuf[r - 2] = 0;
		}
	}
}

struct serial_channel* serial_open (char* port_name, int bufsz)
{
	struct serial_win32*	chan;
	
	chan = (struct serial_win32*)malloc(sizeof(struct serial_win32));
	memset(chan, 0, sizeof(struct serial_win32));

	chan->hPort = CreateFile(
		port_name,
		GENERIC_READ | GENERIC_WRITE,
		0,
		NULL,
		OPEN_EXISTING,
		FILE_FLAG_OVERLAPPED,
		NULL);
	if (INVALID_HANDLE_VALUE == chan->hPort)
	{
		DWORD	ecode = GetLastError();

		free(chan);
		SetLastError(ecode);
		return NULL;
	}

	/* Create the two events which are used to signal the state of the
	 * overlapped (asynchronous) operations.  These are manually reset
	 * by the serial_read and serial_write functions when needed.
	 */
	chan->eRxReady = CreateEvent(NULL, TRUE, FALSE, NULL);
	chan->eTxDone = CreateEvent(NULL, TRUE, TRUE, NULL);

	/* Build the two overlapped structures.  The only thing they need in them
	 * is the event handle they will signal when the operation is complete.
	 * Otherwise they should be zeroed out, which was done when the chan
	 * struct was zeroed.
	 */
	chan->rxOverlapped.hEvent = chan->eRxReady;
	chan->txOverlapped.hEvent = chan->eTxDone;

	/* Create the reception buffer.  All data read from the serial
	 * port arrives in this buffer first.
	 */
	chan->rxBufSz = bufsz;
	chan->rxBuf = (char*)malloc(chan->rxBufSz);

	return (struct serial_channel*)chan;
}

int serial_configure (struct serial_channel* _chan, struct serial_cfg* cfg)
{
	struct serial_win32*	chan = (struct serial_win32*)_chan;
	DCB						port_dcb;
	COMMTIMEOUTS			port_cto;

	/* Allocate buffers in the structure, and load the configuration into
	 * the structure.  This is a basic activity used on all platforms.
	 */
	if (serial_init_struct(_chan, cfg) != 0)
	{
		chan->lastError = ERROR_INVALID_DATA;
		return -1;
	}

	/* Configure the event mask for the port so the reader thread knows
	 * when a byte of data is available to be removed from the input
	 * queue.
	 */
	if ( ! SetCommMask(chan->hPort, EV_RXCHAR))
	{
		chan->lastError = GetLastError();
		return -1;
	}

	/* Tell Windows to reset the device, and set a new port buffer.  This
	 * reset is only done if requested by the caller, allowing some users
	 * to bypass the reset if necessary.
	 */
	if (   cfg->rcvbuf_size > 0
		&& cfg->sndbuf_size > 0
		&& RXSTATE_NONE == chan->rxState)
	{
		if ( ! SetupComm(chan->hPort, cfg->rcvbuf_size, cfg->sndbuf_size))
		{
			chan->lastError = GetLastError();
			return -1;
		}

		/* Tell the OS to empty all data buffers currently assocaited with
		 * the port.  This prevents us from getting data that we may not
		 * have wanted, etc.  Really an OS should do this no matter what...
		 */
		if ( ! PurgeComm(chan->hPort,
				PURGE_TXABORT	| PURGE_RXABORT
				| PURGE_TXCLEAR	| PURGE_RXCLEAR))
		{
			chan->lastError = GetLastError();
			return -1;
		}

		/* Configure a set of timeouts for this communications port which
		 * ensures that both ReadFile and WriteFile do not block when we
		 * perform IO on them.  Ideally set this to all zeros, except for
		 * ReadIntervalTimeout, which according to the Platform SDK docs
		 * will make ReadFile return immediately with a lastCount of 0
		 * if no data is immediately available.
		 */
		memset(&port_cto, 0, sizeof(port_cto));
		port_cto.ReadIntervalTimeout = MAXDWORD;
		if ( ! SetCommTimeouts(chan->hPort, &port_cto))
		{
			chan->lastError = GetLastError();
			return -1;
		}
	}

	/* Load the port's current state into the DCB structure.  This
	 * sets some good defaults, especially for fields which we are not going
	 * to modify.  Apparently it is not a good idea to use the DCB structure
	 * directly, as it may change in the future.  BuildDCB which uses a string
	 * format may be the more optimal approach, but this requires building a
	 * string dynamically here and then running BuildDCB on that.
	 */
	memset(&port_dcb, 0, sizeof(port_dcb));
	port_dcb.DCBlength = sizeof(port_dcb);
	if ( ! GetCommState(chan->hPort, &port_dcb))
	{
		chan->lastError = GetLastError();
		return -1;
	}
	
	/* Configuration required by default under this application.  These are
	 * settings required by Windows for serial ports, or that this application
	 * requires be set this way for one reason or another.
	 */
	port_dcb.fBinary = TRUE;
	port_dcb.fParity = TRUE;
	port_dcb.fErrorChar = FALSE;
	port_dcb.fNull = FALSE;
	port_dcb.fAbortOnError = FALSE;
	port_dcb.EofChar = 0;
	port_dcb.EvtChar = 0;
	port_dcb.ErrorChar = 0;
	
	/* Copy the serial_cfg data into the DCB.  This may require doing some value
	 * translations between what the application uses for serial port configuration
	 * and what Windows wants in its DCB structure.
	 */
	port_dcb.BaudRate = cfg->baud_rate;
	port_dcb.ByteSize = cfg->byte_size;
	
	if (cfg->use_xonxoff)
	{
		port_dcb.fOutX = TRUE;
		port_dcb.fInX = TRUE;
	} else
	{
		port_dcb.fOutX = FALSE;
		port_dcb.fInX = FALSE;
	}
	
	port_dcb.fOutxCtsFlow = cfg->use_cts;
	port_dcb.fOutxDsrFlow = cfg->use_dsr;
	port_dcb.fDsrSensitivity = cfg->use_dsr;
	port_dcb.fDtrControl = cfg->use_dtr ? DTR_CONTROL_HANDSHAKE : DTR_CONTROL_DISABLE;
	port_dcb.fRtsControl = cfg->use_rts ? RTS_CONTROL_HANDSHAKE : RTS_CONTROL_DISABLE;

	switch (cfg->parity)
	{
	case SERIAL_PARITY_NONE:	port_dcb.Parity = NOPARITY; break;
	case SERIAL_PARITY_ODD:		port_dcb.Parity = ODDPARITY; break;
	case SERIAL_PARITY_EVEN:	port_dcb.Parity = EVENPARITY; break;
	}

	switch (cfg->stop_bits)
	{
	case SERIAL_STOPBITS_1:		port_dcb.StopBits = ONESTOPBIT; break;
	case SERIAL_STOPBITS_1_5:	port_dcb.StopBits = ONE5STOPBITS; break;
	case SERIAL_STOPBITS_2:		port_dcb.StopBits = TWOSTOPBITS; break;
	}

	/* Reconfigure the com port now that the Windows specific DCB structure has
	 * been packed with the correct data for this port.
	 */
	if ( ! SetCommState(chan->hPort, &port_dcb))
	{
		chan->lastError = GetLastError();
		return -1;
	}

	/* Start the loading of data into the buffer by calling read with a
	 * 0 length.  This should start a wait event pending, and if it
	 * completes immediately will fill the buffer.  The call should
	 * return immediately without blocking.  With a 0 length no data
	 * will be consumed from the rx buffer, so the application will
	 * have all of it available.
	 */
	if (RXSTATE_NONE == chan->rxState && chan->rxBufLen == 0)
	{
		serial_read(_chan, NULL, 0);
	}

	return 0;
}

int serial_flush (struct serial_channel* _chan)
{
	struct serial_win32*	chan = (struct serial_win32*)_chan;

	/* If a write operation is currently pending, try to
	 * finish the event.  So long as it is still pending,
	 * block the caller.
	 */
	switch (chan->txState)
	{
	case TXSTATE_WRITE:
		while (serial_write(_chan, NULL, 0) == -1)
		{
			WaitForSingleObject(chan->eTxDone, INFINITE);
		}
		break;
	}

	FlushFileBuffers(chan->hPort);

	return 0;
}

static BOOL start_write (struct serial_win32* chan)
{
	BOOL	rok;

	do
	{
		ResetEvent(chan->eTxDone);
		chan->txState = TXSTATE_WRITE;

		rok = WriteFile(
			chan->hPort,
			chan->txBuf + chan->txSentLen,
			(DWORD)chan->txBufLen - chan->txSentLen,
			&chan->txLastCount,
			&chan->txOverlapped);

		if (rok)
		{
			chan->txSentLen += chan->txLastCount;
			chan->txState = TXSTATE_NONE;
			chan->lastError = 0;

		} else
		{
			DWORD	ecode = GetLastError();

			if (ERROR_IO_PENDING == ecode)
			{
				/* Ok, good, the IO operation is now pending.  Let it pend.
				 * when its done, we'll get chan->eTxDone signaled.  The
				 * loop must be broken as txLastCount cannot be trusted.
				 */
				chan->lastError = 0;
				return FALSE;

			} else
			{
				chan->lastError = ecode;
				return FALSE;
			}
		}

	} while (chan->txLastCount > 0 && chan->txSentLen < chan->txBufLen);

	return TRUE;
}

static BOOL finish_write (struct serial_win32* chan)
{
	BOOL	rok;
	DWORD	lastCount;

	rok = GetOverlappedResult(
		chan->hPort,
		&chan->txOverlapped, 
		&lastCount,
		FALSE);

	if (rok)
	{
		/* The wait is done, we have written data from the application buffer.
		 * lastCount bytes have been placed there.  Don't perform
		 * any other IO operations, as start_write should really
		 * get started up.  But this is done by serial_write.
		 */
		chan->txSentLen += lastCount;
		chan->txState = TXSTATE_NONE;
		chan->lastError = 0;

		/* Always return TRUE to the caller, as there is new data in
		 * the application buffer.  Don't attempt to restart start_write
		 * as that could cause an ugly loop.
		 */
		return TRUE;

	} else
	{
		DWORD	ecode = GetLastError();

		if (ERROR_IO_PENDING == ecode || ERROR_IO_INCOMPLETE == ecode)
		{
			/* The wait is still pending.  Somehow the signal was sent
			 * when it wasn't ready.  Return FALSE to let the caller know
			 * no data is ready yet.
			 */
			chan->lastError = 0;
			return FALSE;

		} else
		{
			chan->lastError = ecode;
			return FALSE;
		}
	}
}

int serial_write (
	struct serial_channel*	_chan,
	void*					buf,
	int						len)
{
	struct serial_win32*	chan = (struct serial_win32*)_chan;

	for (;;)
	{
		switch (chan->txState)
		{
		case TXSTATE_NONE:
			if (   chan->txBuf == (char*)buf
				&& chan->txSentLen == chan->txBufLen)
			{
				/* The caller has tried to send this buffer before, but
				 * this function returned -1 and claimed to reject it
				 * before.  In reality, this function has finished writing
				 * the buffer out using overlapped IO, and the caller is
				 * calling because the final overlapped IO event has
				 * completed.  Clear out txbuf and return back the total
				 * number of bytes sent.
				 */
				int			actSent = chan->txSentLen;

				chan->txBuf = NULL;
				chan->txBufLen = 0;
				chan->txSentLen = 0;

				SetEvent(chan->eTxDone);
				return actSent;

			} else if (NULL == chan->txBuf)
			{
				/* The output buffer doesn't exist, so no IO is currently
				 * in progress.  Accept the caller's buffer and try to get
				 * the IO started.
				 */
				if (len > 0 && buf != NULL)
				{
					chan->txBuf = (char*)buf;
					chan->txBufLen = len;
					chan->txSentLen = 0;

					/* If the first write goes into a pending state, return
					 * -1 to the caller.  The caller will think no data was
					 * transmitted yet (due to buffers being full) but really
					 * the data is already on its way.
					 */
					if ( ! start_write(chan))
					{
						return -1;
					}

					/* If we fall through, the write completed, but there may
					 * still be data in the output buffer left for
					 * transmission.  Loop and try to start another IO event.
					 */

				} else
				{
					/* No IO to perform.  Manually set the eTxDone event so
					 * the caller will get signaled and know its safe to
					 * provide another buffer of data to the write call.
					 */
					SetEvent(chan->eTxDone);
					return 0;
				}

			} else if (   chan->txBuf != NULL
					   && chan->txSentLen < chan->txBufLen)
			{
				/* The tx buffer has not been full transmitted, and no IO
				 * opertion is pending.  Start a new write.  If it goes into
				 * pending state, return to the caller.
				 */
				if ( ! start_write(chan))
				{
					return -1;
				}
			}
			break;

		case TXSTATE_WRITE:
			/* The serial port write may have just finished.  If it is
			 * really done, run the loop again to try and start a new
			 * write, or to discover the buffer is complete and either
			 * start the new buffer supplied by the user, or make eTxDone
			 * manually set.
			 */
			if ( ! finish_write(chan))
			{
				return -1;
			}
			break;
		}
	}
}

static BOOL start_read (struct serial_win32* chan)
{
	BOOL	rok;

	do
	{
		/* Start the overlapped read.  This call should complete immediately
		 * but may return data, and may not.  If it does, the loop will make
		 * sure it gets called again immediately, effectively draining the
		 * serial port buffer until it is empty.
		 */
		ResetEvent(chan->eRxReady);
		chan->rxState = RXSTATE_READ;

		rok = ReadFile(
			chan->hPort,
			chan->rxBuf + chan->rxBufLen,
			(DWORD)chan->rxBufSz - chan->rxBufLen,
			&chan->rxLastCount,
			&chan->rxOverlapped);

		if (rok)
		{
			chan->rxBufLen += chan->rxLastCount;
			chan->rxState = RXSTATE_NONE;
			chan->lastError = 0;

		} else
		{
			DWORD	ecode = GetLastError();
			
			if (ERROR_IO_PENDING == ecode)
			{
				/* Ok, good, the IO operation is now pending.  Let it pend.
				 * when its done, we'll get chan->eRxReady signaled.  The
				 * loop must be broken as rxLastCount cannot be trusted.
				 */
				chan->lastError = 0;
				return FALSE;

			} else
			{
				chan->lastError = ecode;
				return FALSE;
			}
		}
	} while (chan->rxLastCount > 0 && chan->rxBufLen < chan->rxBufSz);

	/* Return true to the caller to let it know that the application
	 * buffer may contain more data now than it did before.  Do not
	 * attempt to restart start_wait_rx, as that may create an infinite
	 * loop in the code during high amounts of data traffic.
	 */
	return TRUE;
}

static BOOL finish_read (struct serial_win32* chan)
{
	BOOL	rok;
	DWORD	lastCount;

	rok = GetOverlappedResult(
		chan->hPort,
		&chan->rxOverlapped, 
		&lastCount,
		FALSE);

	if (rok)
	{
		/* The wait is done, we have data in the application buffer.
		 * lastCount bytes have been placed there.  Don't perform
		 * any other IO operations, as start_wait_ex should really
		 * get started up.  But this is done by serial_read.
		 */
		chan->rxBufLen += lastCount;
		chan->rxState = RXSTATE_NONE;
		chan->lastError = 0;

		/* Always return TRUE to the caller, as there is new data in
		 * the application buffer.  Don't attempt to restart start_wait_rx
		 * as that could cause an ugly loop.
		 */
		return TRUE;

	} else
	{
		DWORD	ecode = GetLastError();

		if (ERROR_IO_PENDING == ecode || ERROR_IO_INCOMPLETE == ecode)
		{
			/* The wait is still pending.  Somehow the signal was sent
			 * when it wasn't ready.  Return FALSE to let the caller know
			 * no data is ready yet.
			 */
			chan->lastError = 0;
			return FALSE;

		} else
		{
			chan->lastError = ecode;
			return FALSE;
		}
	}
}

static BOOL start_wait_rx (struct serial_win32* chan)
{
	chan->rxMask = EV_RXCHAR;
	chan->rxState = RXSTATE_WAIT;

	ResetEvent(chan->eRxReady);
	if (WaitCommEvent(chan->hPort, &chan->rxMask, &chan->rxOverlapped))
	{
		/* If the event completes immediately, there must be data in the
		 * operating system buffer, so read it into the application
		 * buffer now.
		 */
		return start_read(chan);

	} else
	{
		DWORD	ecode = GetLastError();

		if (ERROR_IO_PENDING == ecode)
		{
			/* The RXCHAR event is pending.  We'll let it wait in the
			 * background and return back to the caller.  eRxReady will
			 * be signaled when there is at least one character in the
			 * serial port buffer.  Return FALSE to let the caller know
			 * no data is ready now.
			 */
			chan->lastError = 0;
			return FALSE;

		} else
		{
			chan->lastError = ecode;
			return FALSE;
		}
	}
}

static BOOL finish_wait_rx (struct serial_win32* chan)
{
	BOOL	rok;
	DWORD	lastCount;	// undefined with WaitCommEvent.

	rok = GetOverlappedResult(
		chan->hPort,
		&chan->rxOverlapped, 
		&lastCount,
		FALSE);

	if (rok)
	{
		/* The wait is done, we have data in the OS buffer.  Start
		 * reading from the buffer into the application buffer now.
		 */
		chan->lastError = 0;
		return start_read(chan);

	} else
	{
		DWORD	ecode = GetLastError();

		if (ERROR_IO_PENDING == ecode || ERROR_IO_INCOMPLETE == ecode)
		{
			/* The wait is still pending.  Somehow the signal was sent
			 * when it wasn't ready.  Return FALSE to let the caller know
			 * no data is ready yet.
			 */
			chan->lastError = 0;
			return FALSE;
	
		} else
		{
			chan->lastError = ecode;
			return FALSE;
		}
	}
}

int serial_read (
	struct serial_channel*	_chan,
	void*					_buf,
	int						len)
{
	struct serial_win32*	chan = (struct serial_win32*)_chan;
	char*					buf = (char*)_buf;
	int						bytesCopied = 0;
	int						bytesAvail;

	for (;;)
	{
		/* Get the number of bytes remaining in the read buffer.  This is
		 * the total number of bytes immediately available without making
		 * the caller wait.
		 */
		bytesAvail = chan->rxBufLen - chan->rxRecvLen;

		if (bytesAvail > 0)
		{
			/* If no space remains in the buffer provided by the caller, return
			 * the number of bytes which have been copied into the user's buffer.
			 */
			if (0 == len)
			{
				/* If there is still more data available in the application buffer,
				 * but no pending IO operation is active, there won't be a pending
				 * operation when this return is made.  Therefore manually signal
				 * the eRxReady event.  This ensures that the caller will get
				 * the signal immediately and call again to get more data from
				 * the buffer.  The later call may start an IO operation.
				 */
				if (RXSTATE_NONE == chan->rxState)
				{
					SetEvent(chan->eRxReady);
				}

				goto ok_return;
			}

			if (len < bytesAvail)
			{
				/* The rx buffer actually has more data in it than what the
				 * caller asked for.  Copy only what the caller asked for.
				 */
				memcpy(buf, chan->rxBuf + chan->rxRecvLen, len);
				chan->rxRecvLen += len;
				bytesCopied += len;
				len = 0;

			} else
			{
				/* Copy what's available in the serial port buffer.  There may
				 * still be more data imemdiately available from the OS, but we
				 * don't know that yet.  This might actually empty the buffer,
				 * but we cannot update rxBufLen or reset rxRecvLen until we know
				 * if a pending operation is outstanding or not.
				 */
				memcpy(buf, chan->rxBuf + chan->rxRecvLen, bytesAvail);
				chan->rxRecvLen += bytesAvail;
				bytesCopied += bytesAvail;
				buf += bytesAvail;
				len -= bytesAvail;
			}
		}

		switch (chan->rxState)
		{
		case RXSTATE_NONE:
			/* No operation is currently pending.  If the buffer has been
			 * completely drained by the caller, start up a wait operation.
			 */
			if (chan->rxRecvLen == chan->rxBufLen)
			{
				/* The buffer is completely empty, so zero out the lengths
				 * so the entire buffer is available for more data.  This
				 * is the only way the buffer truncates back to its start.
				 */
				chan->rxBufLen = 0;
				chan->rxRecvLen = 0;

				/* If start_wait_rx returns FALSE, an IO operation is now
				 * pending and the caller will get signaled with eRxReady
				 * when the pending operation completes.  Return the current
				 * number of bytes processed, or -1.
				 */
				if ( ! start_wait_rx(chan))
				{
					goto ok_return;
				}

				/* No operation is pending.  Most likely there is now new
				 * data in the read buffer, and thus the receive loop should
				 * keep running to copy all available data into the user
				 * buffer.
				 */
			}

			/* If the buffer is not empty, don't try to get an operation
			 * started.  This allows the caller to completely empty the
			 * application buffer on the next call (which should happen
			 * immediately as SetEvent is used to manually signal eRxReady).
			 * Only after the caller has emptied the buffer will the IO
			 * get started up again.
			 */
			break;

		case RXSTATE_WAIT:
			/* The serial port needs to finish the wait call.  If the wait
			 * call is not finished yet, return the current number of
			 * bytes processed (most likely will be 0, so the actual
			 * return code will be -1).
			 */
			if ( ! finish_wait_rx(chan))
			{
				goto ok_return;
			}
			
			/* If we fall through, finish_wait_rx returned TRUE, indicating
			 * that the start_read call put new data into the buffer.  Go
			 * through the loop above and try to consume it.
			 */
			break;

		case RXSTATE_READ:
			/* The serial port read just finished, or maybe not.  If the
			 * read is not yet finished, return the number of bytes put into
			 * the user's buffer, or -1 if no data was copied.
			 */
			if ( ! finish_read(chan))
			{
				goto ok_return;
			}

			/* If we fall through, the read may have put more data into the
			 * buffer, so loop and try to copy it into the user's buffer.
			 */
			break;
		}
	}

ok_return:
	/* If no bytes were copied, return -1.  This is how UNIX read(2) works
	 * when in nonblocking IO mode.  We don't however set an error code
	 * to EAGAIN like UNIX systems do.
	 */
	return bytesCopied > 0 ? bytesCopied : -1;
}

void serial_close (struct serial_channel* _chan)
{
	struct serial_win32*	chan = (struct serial_win32*)_chan;

	serial_destroy_struct(_chan);

	if ( ! CloseHandle(chan->hPort))
	{
		windows_error("serial_close, CloseHandle(hPort)");
	}

	if ( ! CloseHandle(chan->eRxReady))
	{
		windows_error("serial_close, CloseHandle(eRxReady)");
	}

	if ( ! CloseHandle(chan->eTxDone))
	{
		windows_error("serial_close, CloseHandle(eTxDone)");
	}

	if (chan->rxBuf)
	{
		free(chan->rxBuf);
	}

	free(chan);
}