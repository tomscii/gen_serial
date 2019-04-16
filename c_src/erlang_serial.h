#ifndef _INC_erlang_serial_h
#define _INC_erlang_serial_h

/* translate error constants to POSIX values */
#ifndef WIN32
#include <errno.h>
#define ERROR_SUCCESS      0
#define ERROR_INVALID_DATA EINVAL
#define ERROR_BAD_LENGTH   EBADMSG
#endif

#ifdef __QNXNTO__
/* On QNX, POLLIN is defined as (POLLRDNORM | POLLRDBAND)
 * but what we really want is just POLLRDNORM.
 */
#undef POLLIN
#define POLLIN POLLRDNORM
#endif /* __QNXNTO__ */

#define SERIAL_PARITY_NONE	0
#define SERIAL_PARITY_ODD	1
#define SERIAL_PARITY_EVEN	2

#define SERIAL_STOPBITS_1		1
#define SERIAL_STOPBITS_2		2
#define SERIAL_STOPBITS_1_5		-1

#define SERIAL_PACKET_NONE		0
#define SERIAL_PACKET_1			1
#define SERIAL_PACKET_2			2
#define SERIAL_PACKET_4			4
#define SERIAL_PACKET_DELIM		'd'

/****************************************************************
 * Types of packets.  Each packet type indicator is a single byte
 * placed at the start of the packet.  ASCII characters are used
 * here to help debug the protocol stream.
 */

/* No packet type.  Used when a value is needed in a packet type
 * field of a struct, but there is no packet currently in existance.
 * This type of packet should never be sent across the pipe.
 */
#define PACKET_NONE		0x00

/* Change the port active flag.  The packet data is a single byte
 * holding one of the PORT_ACTIVE_* constants below.  No reply is
 * sent.  (Erlang -> Driver)
 */
#define PACKET_ACTIVE	'a'

/* The port is closed.   There is no packet data.  Sent by Erlang
 * before it really closes the port down to make sure the output
 * queue is flushed.  (Driver -> Erlang)
 */
#define PACKET_CLOSE	'q'

/* Packet used to open a port.  Only set as part of a PACKET_OK's
 * payload to indicate the port was opened.  (Driver -> Erlang).
 */
#define	PACKET_OPEN		'o'

/* Configure the serial port.  The packet data is a struct serial_cfg
 * defined below.  All fields are in native byte order for this machine
 * (not network byte order).  (Erlang -> Driver)
 */
#define PACKET_CONFIG	'c'

/* Pass data between Erlang and the serial port, or from the serial
 * port to Erlang.  The data is a single packet worth of data, which
 * has been decoded from the protocol framing used by the serial port.
 * (Erlang -> Driver, Driver -> Erlang)
 */
#define PACKET_DATA		'd'

/* Send an error to Erlang.  Sent when an error occurs within the
 * port driver.  The data is a struct serial_error, in native byte
 * order.  (Driver -> Erlang).
 */
#define PACKET_ERROR	'e'

/* Request that the driver empty its output queue, and reply with
 * PACKET_OK when the output queue is empty.  There is no packet
 * data.  (Erlang -> Driver)
 */
#define PACKET_FLUSH	'f'

/* Sent as a success message after a PACKET_CONFIG is processed, or
 * after the serial port is opened successfully.  Tells Erlang that
 * the last operation was good.  Has a data structure of 1 byte, the
 * packet type of the last good packet.  (Driver -> Erlang; after
 * PACKET_OPEN, PACKET_CONFIG, PACKET_FLUSH, PACKET_CLOSE)
 */
#define PACKET_OK		'k'

/* Special packet type used internally by the port driver when it
 * needs to finish reading all of a packet, because the packet
 * cannot be processed.  This type should never be sent across the
 * pipe.
 */
#define PACKET_DRAIN	'-'


/****************************************************************
 * Erlang port active flags.  When the port is active, the driver
 * process will send data to the node, which will receive it and
 * send it as messages to the port owner.  If the port owner
 * wishes to have this data held back (so as not to flood the
 * message queue), it can set the port to FALSE or ONCE.
 */
#define	PORT_ACTIVE_FALSE	0
#define PORT_ACTIVE_TRUE	1
#define PORT_ACTIVE_ONCE	2

/* Log file where all debugging messages are sent to.  This file is written
 * by the port process, rather than Erlang itself, as it allows the port
 * process to debug the communications channels between Erlang and the
 * serial port.
 */
#ifdef _DEBUG
  extern FILE*	debug_log_file;
# define DBG_LOG(x)		if (debug_log_file) { x; fflush(debug_log_file); }
#else
# define DBG_LOG(x)
#endif

/* Magic string placed into serial_cfg.magic by Erlang prior to sending
 * the configuration struct.  If this struct changes in format, the
 * magic string must be modified.  This string is used to prevent a mismatch
 * in protocol between Erlang and the port program.
 */
#define SERIAL_CFG_MAGIC	"gens001"

struct serial_cfg
{
	/* Special string to make sure Erlang driver code and this program are
	 * compatible.
	 */
	char	magic[8];

	/* Size of the buffer the operating system should allocate for this
	 * serial channel.  Specified in bytes.  May not be supported by the
	 * specific operating system.
	 */
	int		rcvbuf_size;
	int		sndbuf_size;

	/* Number of bits per second that the serial line will transmit data
	 * at.  No index constants are used, the exact bits per second rate is
	 * used instead.
	 */
	int		baud_rate;

	/* Number of bits in a byte.  Normally this is 8.
	 */
	char	byte_size;

	/* One of the SERIAL_PARITY_* constants defined above.
	 */
	char	parity;

	/* One of the SERIAL_STOPBITS_* constants defined above, or the numeric
	 * values 1 or 2.
	 */
	char	stop_bits;

	/* Use software flow control (XON/XOFF inline characters). 
	 * 1 == use XON/XOFF, 0 == don't use XON/XOFF.
	 */
	char	use_xonxoff;

	/* Use the Clear-To-Send signal line.  1 == use CTS, 0 == don't use CTS.
	 * Set to 1 for hardware flow control.
	 */
	char	use_cts;	

	/* Use the Data-Set-Ready signal line.  1 == use DSR, 0 == don't use DSR.
	 * Set to 1 for hardware flow control.
	 */
	char	use_dsr;
	
	/* Use the Data-Terminal-Ready signal line.  1 == use DTR, 0 == don't use DTR.
	 * Set to 1 for hardware flow control.
	 */
	char	use_dtr;
	
	/* Use the Ready-To-Send signal line.  1 == use RTS, 0 == don't use RTS.
	 * Set to 1 for hardware flow control.
	 */
	char	use_rts;

	/* Number of bytes in the data packet headers.  Data packets may have 1,
	 * 2 or 4 bytes for the header.  If the header is used, the header
	 * stores the number of bytes in the packet, in network byte order.  This
	 * is typical Erlang packet encoding.  One of the SERIAL_PACKET_* constants
	 * can also be used.
	 *
	 * If negative, the packet is a fixed width format, with the absolute value
	 * of this field indicating the length, in bytes, of the packets.
	 */
	short	packet_format;
	
	/* Should the port become active initially?
	 */
	char	initially_active;
	
	/* Number of bytes in the packet delimiter sequence.
	 */
	char	packet_delim_len;

	/* Sequence of bytes which delimit a packet.
	 */
	char	packet_delim[8];
};

struct serial_error
{
	int			error_code;
	char		error_msg[256];
};

struct serial_channel
{
	struct serial_cfg	config;
};

struct pipe_channel;
struct serial_port;

/* Type of a function which can be used to scan for packets on the incoming
 * buffer (data read from the serial port).
 */
typedef void (*scan_packetf) (struct serial_port* port);

struct serial_port
{
	/* Reference to the data necessary for the serial port at the OS level.
	 * This structure holds all of the OS specific data fields needed to
	 * successfully manage the serial port in an async IO environment.
	 */
	struct serial_channel*	chan;

	/* Reference to the data necessary for the pipe at the OS level.  The pipe
	 * is connected to erts, the Erlang runtime process.  All data is between
	 * Erlang and this program moves through the pipe.  The structure holds all
	 * data necessary to use the pipe in an async IO environment.
	 */
	struct pipe_channel*	erts;

	/* Function which scans packets out of the incoming buffer.  This function
	 * updates incomingScan after it has scanned the buffer.
	 */
	scan_packetf			packet_scanner;

	/* Micro buffer used to send the packet header to erts.  The header
	 * is formatted directly into this buffer, and the buffer is written
	 * out before the actual data.  Large enough to hold control packets
	 * like struct serial_error (including its header).
	 */
	char*					incomingHeader;

	/* Buffer of data coming in from the serial port, and which needs
	 * to be passed on to erts.  The buffer is allocated by this struct.
	 */
	char*					incoming;

	/* Number of valid bytes in incoming.
	 */
	int						incomingLen;

	/* Total size of incoming.  This is set by the Erlang user code to allow
	 * for packet level processing within the driver.
	 */
	int						incomingBufSz;

	/* Number of bytes sent from incoming.  Always less than incomingLen,
	 * as this represents the number of bytes transmitted to erts already.
	 */
	int						incomingSent;

	/* Number of bytes which have been scanned.  This is always going to
	 * be between incomingSent and incomingLen, as it represents the marker
	 * within the buffer of where the scanner left off when trying to form
	 * a packet.
	 */
	int						incomingScan;

	/* Last position of packetscan_delim within the buffer.  This is different
	 * from incomingScan, and may be before or equal to incomingScan.  Since
	 * incomingScan controls the writeable event, and thus the scanner, it
	 * must be updated to incomingLen on every scan operation.  incomingMatch
	 * on the other hand points to the next likely position of the delimiter.
	 */
	int						incomingMatch;

	/* Flag used by packetscan_delim to know if a packet was just sent or not.
	 * If a packet was just sent, packetscan_delim will update incomingSent on
	 * the next call to reflect the delimiter it just skipped over in the
	 * buffer.
	 */
	int						incomingSentPacket;

	/* Number of bytes left in the current packet, excluding the
	 * header bytes.  This is the number of data bytes we are copying
	 * from incoming over to erts by the pipe.
	 */
	int						incomingCurPacketLeft;

	/* Number of header bytes left needing to be sent.
	 */
	int						incomingCurHeaderLeft;

	/* Number of header bytes total in incomingHeader needing to be sent.
	 */
	int						incomingCurHeaderLen;

	/* Buffer of data coming in from Erlang and heading out the serial
	 * port.  Contains the packet header as well as the data.  Might
	 * have more than one packet in it at a time.
	 */
	char*					outgoing;

	/* Number of valid bytes in outgoing.
	 */
	int						outgoingLen;

	/* Total size of outgoing. This is set by the Erlang user code to
	 * allow for packet level processing.
	 */
	int						outgoingBufSz;

	/* Total number of bytes sent from outgoing.  Always less than
	 * outgoingLen as this represents the number of bytes sent to
	 * the serial port.
	 */
	int						outgoingSent;

	/* Total number of bytes in the current packet.
	 */
	int						outgoingPacketLen;

	/* Number of bytes remaining to be processed in the current packet.
	 */
	int						outgoingPacketLeft;

	/* Type of the current packet.
	 */
	int						outgoingPacketType;

	/* Current status of the Erlang port.  Determines when this driver sends
	 * data to Erlang, and allows Erlang to throttle data transmission to
	 * prevent overflowing message queues.
	 */
	int						erlangPortActive;

	/* Is this port dead?  If so, the port process will exit.
	 */
	int						isDead;
	
	/* Flag indicating if the PACKET_OK message needs to be sent.
	 * This is set to the PACKET_* type of the packet which we are
	 * sending OK for.
	 */
	char					sendOkFor;
	char					_pad1[3];

	/* The current error message which needs to be sent to erts.
	 */
	struct serial_error		lastError;
};


/* Open the serial port.
 */
extern struct serial_channel* serial_open (char* port_name, int bufsz);

/* Close an open serial port.
 */
extern void serial_close (struct serial_channel* chan);

/* Get the current error code number from the serial port, or 0 if there
 * is no error.  Unlike the POSIX APIs, EAGAIN is not considered an error.
 */
extern int serial_last_error (struct serial_channel* chan);

/* Get a text message describing the error code.
 */
extern void serial_format_error (
	struct serial_channel*	chan,
	int						ecode,
	char*					msgbuf,
	int						msgbuf_len);

/* Configure the serial port based on the supplied configuration data.
 */
extern int serial_configure (
	struct serial_channel*	chan,
	struct serial_cfg*		cfg);

/* Read data from the serial port.
 *
 * Returns the number of bytes placed into buf, or -1 if no data could be
 * aquired without blocking the caller.
 */
extern int serial_read (
	struct serial_channel*	chan,
	void*					buf,
	int						len);

/* Writes data to the serial port.
 *
 * Returns the number of bytes successfully sent so far, or -1 if no
 * data could be sent and the caller should try again with this buffer.
 *
 * Once a caller calls this function with a buffer, the caller must
 * keep calling the function to output that buffer until all the bytes
 * in the buffer have been sent.  The caller must not modify the buffer
 * until all data has been sent, as implementations are allowed to use
 * background async-IO operations to transmit the buffer, while returning
 * -1 to the caller claiming that the buffer has not be sent yet.
 */
extern int serial_write (
	struct serial_channel*	chan,
	void*					buf,
	int						len);

/* Flush any data which the OS may have currently buffered in the output
 * buffer. Only applies on Windows.  UNIX OSes don't buffer their output
 * like Windows does, and therefore this call may be a no-op on UNIX.
 * THIS CALL BLOCKS!  Making this call is not a great idea, unless you
 * are going to be expecting input from the device right after this flush
 * call and want try to speed up transfer to ensure the data is written.
 */
extern int serial_flush (struct serial_channel* chan);

/* Init the channel structure, should be called by serial_configure.
 */
extern int serial_init_struct (
	struct serial_channel*	chan,
	struct serial_cfg*		cfg);

/* Deallocate the channel buffers.  Should be called by serial_close.
 */
extern int serial_destroy_struct (struct serial_channel* chan);

/* Get the current error code number from the pipe, or 0 if there
 * is no error.  Unlike the POSIX APIs, EAGAIN is not considered an error.
 */
extern int pipe_last_error (struct pipe_channel* chan);

/* Get a text message describing the error code.
 */
extern void pipe_format_error (
	struct pipe_channel*	chan,
	int						ecode,
	char*					msgbuf,
	int						msgbuf_len);

/* Write data to erts.  There is no packet handling at this level, this is a
 * raw data transfer.  The caller is expected to create the packet header and
 * write it through this call.
 */
extern int pipe_write (
	struct pipe_channel*	_chan,
	void*					buf,
	int						len);
extern int pipe_write2 (
	struct pipe_channel*	_chan,
	void*					buf1,
	int						len1,
	void*					buf2,
	int						len2);

	/* Read data from erts.  This is a raw read, there is no packet handling
 * being done.
 */
extern int pipe_read (
	struct pipe_channel*	_chan,
	void*					buf,
	int						len);

/* Close the pipe to erts.
 */
extern void pipe_close (struct pipe_channel* _chan);

/* Create a serial port structure to shuttle data between erts and the
 * serial port.
 */
extern struct serial_port* serial_port_create (
	struct serial_channel*	chan,
	struct pipe_channel*	erts,
	int						bufsz);

/* Deallocate the serial port structure.
 */
extern void serial_port_destroy (struct serial_port* port);

/* Handle the event which indicates we can read data from erts.
 */
extern void serial_port_ertsr (struct serial_port* port);

/* Handle the event which indicates we can write data to erts.
 */
extern void serial_port_ertsw (struct serial_port* port);

/* Handle the event which indicates we can read data from the serial port.
 */
extern void serial_port_serialr (struct serial_port* port);

/* Handle the event which indicates we can write data to the serial port.
 */
extern void serial_port_serialw (struct serial_port* port);

/* Determine if erts needs to be watched for readablility.
 */
extern int serial_port_watch_ertsr (struct serial_port* port);

/* Determine if erts needs to be watched for writeablility.
 */
extern int serial_port_watch_ertsw (struct serial_port* port);

/* Determine if the serial port needs to be watched for readability.
 */
extern int serial_port_watch_serialr (struct serial_port* port);

/* Determine if the serial port needs to be watched for writeability.
 */
extern int serial_port_watch_serialw (struct serial_port* port);

/* Determine if the port needs to be shutdown now.
 * If true, the processs should call serial_port_destroy, and then exit.
 */
extern int serial_port_isdead (struct serial_port* port);

/* Determine if a given error code (received via errno on UNIX or
 * GetLastError() on Windows) is fatal. If true, the port will be
 * flagged as dead and the driver will terminate.
 */
int is_fatal_error (int errcode);

#endif
