#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <poll.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <termios.h>
#include <unistd.h>

#include "../erlang_serial.h"

int is_fatal_error(int errcode)
{
        if (errcode == EIO || errcode == EPIPE) {
                return 1;
        }
        return 0;
}

struct serial_posix {
	struct serial_channel channel;
	int fd;
	int lastError; /* Error of the last call. This is directly from errno */
};

struct serial_channel *serial_open(char *port_name, int bufsz)
{
	struct serial_posix *chan;

	chan = (struct serial_posix *)malloc(sizeof(struct serial_posix));
	memset(chan, 0, sizeof(struct serial_posix));

        chan->fd = open(port_name, O_RDWR | O_NOCTTY);
        if (chan->fd < 0) {
		chan->lastError = errno;
		DBG_LOG(fprintf(debug_log_file, "serial_open failed: %s\n", strerror(errno)));
		free(chan);
		return NULL;
	}
	return (struct serial_channel *)chan;
}

speed_t serial_get_baud_const(int speed) {
	switch (speed) {
		case 50: return B50;
		case 75: return B75;
		case 110: return B110;
		case 134: return B134;
		case 150: return B150;
		case 200: return B200;
		case 300: return B300;
		case 600: return B600;
		case 1200: return B1200;
		case 2400: return B2400;
		case 4800: return B4800;
		case 9600: return B9600;
		case 19200: return B19200;
		case 38400: return B38400;
		case 57600: return B57600;
		case 115200: return B115200;
#ifndef __QNXNTO__
		case 230400: return B230400;
#ifndef __APPLE__
		case 460800: return B460800;
		case 500000: return B500000;
		case 576000: return B576000;
		case 921600: return B921600;
		case 1000000: return B1000000;
		case 1152000: return B1152000;
		case 1500000: return B1500000;
		case 2000000: return B2000000;
		case 2500000: return B2500000;
		case 3000000: return B3000000;
		case 3500000: return B3500000;
		case 4000000: return B4000000;
#endif // !__APPLE__ 
#endif // !__QNXNTO__
		default: return B0;
	}
}

int serial_configure(struct serial_channel *_chan, struct serial_cfg *cfg)
{
	struct serial_posix *chan = (struct serial_posix *)_chan;
	struct termios tio;

	/* Allocate buffers in the structure, and load the configuration into
	 * the structure.  This is a basic activity used on all platforms.
	 */
	if (serial_init_struct(_chan, cfg) != 0) {
		chan->lastError = ERROR_INVALID_DATA;
		return -1;
	}

	/* configure serial device */
	bzero(&tio, sizeof(tio));

        tio.c_cc[VTIME] = 0;
        tio.c_cc[VMIN] = 1;

        tio.c_cflag = CLOCAL | CREAD;

	speed_t baud_value = serial_get_baud_const(cfg->baud_rate);
	if (baud_value == B0) {
		chan->lastError = EINVAL;
		return -1;
	}
	cfsetispeed(&tio, baud_value);
	cfsetospeed(&tio, baud_value);

	switch (cfg->byte_size) {
	case 5: tio.c_cflag |= CS5; break;
	case 6: tio.c_cflag |= CS6; break;
	case 7: tio.c_cflag |= CS7; break;
	case 8: tio.c_cflag |= CS8; break;
	default: chan->lastError = EINVAL; return -1;
	}

	if (cfg->use_xonxoff) {
		tio.c_iflag |= IXON | IXOFF | IXANY;
		tio.c_cc[VSTART] = 0x11; // XON / DC1;
		tio.c_cc[VSTOP] = 0x13; // XOFF / DC3;
	} else if (cfg->use_rts && cfg->use_cts) {
		// not in POSIX, so differs between systems
#ifdef __QNXNTO__
		tio.c_cflag |= (IHFLOW | OHFLOW);
#else
		tio.c_cflag |= CRTSCTS;
#endif
	}

	switch (cfg->parity) {
	case SERIAL_PARITY_NONE:
		tio.c_iflag |= IGNPAR;
		break;
	case SERIAL_PARITY_EVEN:
		tio.c_cflag |= PARENB;
		tio.c_iflag |= INPCK | ISTRIP;
		break;
	case SERIAL_PARITY_ODD:
		tio.c_cflag |= PARENB | PARODD;
		tio.c_iflag |= INPCK | ISTRIP;
		break;
	}

	switch (cfg->stop_bits)	{
	case SERIAL_STOPBITS_1: break;
	case SERIAL_STOPBITS_1_5: /* TODO: is this possible at all? */ break;
	case SERIAL_STOPBITS_2: tio.c_cflag |= CSTOPB; break;
	}

        tcflush(chan->fd, TCIFLUSH);
        tcsetattr(chan->fd, TCSANOW, &tio);

	return 0;
}

int serial_flush(struct serial_channel *_chan)
{
	return 0;
}

int serial_write(struct serial_channel *_chan, void *buf, int len)
{
	struct serial_posix *chan = (struct serial_posix *)_chan;
	int ret;

	if ((ret = write(chan->fd, buf, len)) < 0) {
		chan->lastError = errno;
	}
	return ret;
}

int serial_read(struct serial_channel *_chan, void *buf, int len)
{
	struct serial_posix *chan = (struct serial_posix *)_chan;
	int ret;
	
	if ((ret = read(chan->fd, buf, len)) < 0) {
		chan->lastError = errno;
	}
	return ret;
}

void serial_close(struct serial_channel *_chan)
{
	struct serial_posix *chan = (struct serial_posix *)_chan;

	serial_destroy_struct(_chan);
	close(chan->fd);
	free(chan);
}

struct pollfd serial_os_readable(struct serial_channel *_chan)
{
	struct serial_posix *chan = (struct serial_posix *)_chan;
	struct pollfd ret;
	ret.fd = chan->fd;
	ret.events = POLLIN;
	return ret;
}

struct pollfd serial_os_writeable(struct serial_channel *_chan)
{
	struct serial_posix *chan = (struct serial_posix *)_chan;
	struct pollfd ret;
	ret.fd = chan->fd;
	ret.events = POLLOUT;
	return ret;
}

int serial_last_error(struct serial_channel *_chan)
{
	struct serial_posix *chan = (struct serial_posix *)_chan;

	if (!chan) {
		return errno;
	}
	return ERROR_SUCCESS == chan->lastError ? 0 : chan->lastError;
}

void serial_format_error(struct serial_channel *chan, int ecode, char *msgbuf, int msgbuf_len)
{
	strerror_r(ecode, msgbuf, msgbuf_len);
}


struct pipe_posix {
	int lastError; /* Error of the last call. This is directly from errno */
};

struct pipe_channel *pipe_os_open(int bufsz)
{
	struct pipe_posix *chan;

	chan = (struct pipe_posix *)malloc(sizeof(struct pipe_posix));
	memset(chan, 0, sizeof(struct pipe_posix));

	/* NB. stdin and stdout are already open and available as the fds
	 * STDIN_FILENO and STDOUT_FILENO.
	 */

	return (struct pipe_channel *)chan;
}

int pipe_write(struct pipe_channel *_chan, void *buf, int len)
{
	struct pipe_posix *chan = (struct pipe_posix *)_chan;
	int ret;

	if ((ret = write(STDOUT_FILENO, buf, len)) < 0) {
		chan->lastError = errno;
	}
	return ret;
}

int pipe_write2(struct pipe_channel *_chan,
		void *buf1, int len1, void *buf2, int len2)
{
	struct pipe_posix *chan = (struct pipe_posix *)_chan;
	int ret1, ret2;

	if ((ret1 = write(STDOUT_FILENO, buf1, len1)) < 0) {
		chan->lastError = errno;
		return ret1;
	}
	if ((ret2 = write(STDOUT_FILENO, buf2, len2)) < 0) {
		chan->lastError = errno;
		return ret2;
	}
	return ret1 + ret2;
}

int pipe_read(struct pipe_channel *_chan, void *buf, int len)
{
	struct pipe_posix *chan = (struct pipe_posix *)_chan;
	int ret;

	if ((ret = read(STDIN_FILENO, buf, len)) < 0) {
		chan->lastError = errno;
	}
	return ret;
}

void pipe_close(struct pipe_channel *_chan)
{
	struct pipe_posix *chan = (struct pipe_posix *)_chan;
	/* No need to explicitly close stdin and stdout. */
	free(chan);
}

struct pollfd pipe_os_readable(struct pipe_channel *_chan)
{
	struct pollfd ret;
	ret.fd = STDIN_FILENO;
	ret.events = POLLIN;
	return ret;
}

struct pollfd pipe_os_writeable(struct pipe_channel *_chan)
{
	struct pollfd ret;
	ret.fd = STDOUT_FILENO;
	ret.events = POLLOUT;
	return ret;
}

int pipe_last_error(struct pipe_channel *_chan)
{
	struct pipe_posix *chan = (struct pipe_posix *)_chan;

	if (!chan) {
		return errno;
	}
	return ERROR_SUCCESS == chan->lastError ? 0 : chan->lastError;
}

void pipe_format_error(struct pipe_channel *chan, int ecode, char *msgbuf, int msgbuf_len)
{
	strerror_r(ecode, msgbuf, msgbuf_len);
}

static void main_loop(struct serial_port *port)
{
	int nfds;
	struct pollfd pollfds[4];

#ifdef _DEBUG
	char dbgmsg[256];
#endif

	int serial_fd = ((struct serial_posix*)(port->chan))->fd;
	int i;

	while (1) {
		nfds = 0;

		DBG_LOG(strcpy(dbgmsg, "main_loop: wait for"));

		if (serial_port_watch_ertsr(port)) {
			pollfds[nfds++] = pipe_os_readable(port->erts);
			DBG_LOG(strcat(dbgmsg, " ertsr"));
		}

		if (serial_port_watch_ertsw(port)) {
			pollfds[nfds++] = pipe_os_writeable(port->erts);
			DBG_LOG(strcat(dbgmsg, " ertsw"));
		}

		if (serial_port_watch_serialr(port)) {
			pollfds[nfds++] = serial_os_readable(port->chan);
			DBG_LOG(strcat(dbgmsg, " serialr"));
		}

		if (serial_port_watch_serialw(port)) {
			pollfds[nfds++] = serial_os_writeable(port->chan);
			DBG_LOG(strcat(dbgmsg, " serialw"));
		}

		DBG_LOG(fprintf(debug_log_file, "%s\n", dbgmsg));

		/* If no events are enabled, we are really up a creek. Shutdown the
		 * process right now.
		 */
		if (0 == nfds) {
			serial_port_destroy(port);
			exit(1);
		}

		/* Wait for any of the above events to become true.  Break out
		 * as soon as one of them is in fact true.
		 */
		if (poll(pollfds, nfds, -1) < 0) {
			port->isDead = 1;
		}

		for (i = 0; i < nfds; i++) {
			if (pollfds[i].revents & (POLLERR | POLLHUP | POLLNVAL)) {
				port->isDead = 1;
			}

			if ((pollfds[i].fd == STDIN_FILENO) && (pollfds[i].revents & POLLIN)) {
				serial_port_ertsr(port);
			} else if ((pollfds[i].fd == STDOUT_FILENO) && (pollfds[i].revents & POLLOUT)) {
				serial_port_ertsw(port);
			} else if (pollfds[i].fd == serial_fd) {
				if (pollfds[i].revents & POLLIN) {
					serial_port_serialr(port);
				} else if (pollfds[i].revents & POLLOUT) {
					serial_port_serialw(port);
				}
			}
		}

		/* If something went horribly wrong, the generic code should
		 * have sent a message to Erlang indicating the error.  It
		 * also flagged the port dead, so we know to terminate the
		 * process.
		 */
		if (serial_port_isdead(port)) {
			serial_port_destroy(port);
			exit(1);
		}
	}
}

int main(int argc, char **argv)
{
	struct pipe_channel *erts;
	struct serial_channel *chan;
	struct serial_port *port;
	char *port_name;
	int buffer_size;

#ifdef _DEBUG
	char *debug_file_name;
#endif

	if (argc < 3) {
#ifdef _DEBUG
		fprintf(stderr,
			"usage: serial_esock device buffersz [debug.txt]\n"
			"       where\n"
			"           device = serial port device filename.\n"
			"           buffersz = size of buffers, in bytes.\n"
			"           debug.txt = debug log file.\n");
#else
		fprintf(stderr,
			"usage: serial_esock device buffersz\n"
			"       where\n"
			"           device = serial port device filename.\n"
			"           buffersz = size of buffers, in bytes.\n");
#endif
		return 1;
	}

	port_name = argv[1];
	buffer_size = atoi(argv[2]);
	if (buffer_size < 128) {
		buffer_size = 128;
	}
#ifdef _DEBUG
	if (argc == 4 && argv[3] && *argv[3]) {
		debug_file_name = argv[3];
	} else {
		debug_file_name = NULL;
	}

	if (debug_file_name) {
		debug_log_file = fopen(debug_file_name, "w");
		if (!debug_log_file) {
			fprintf(stderr, "Cannot open %s\n", debug_file_name);
			return 1;
		}
		fprintf(debug_log_file,
			"startup: port=%s buffersz=%i\n",
			port_name, buffer_size);
		fflush(debug_log_file);
	}
#endif

	erts = pipe_os_open(buffer_size);
	if (!erts) {
		DBG_LOG(fprintf(debug_log_file, "pipe_open failed.\n"));
		return 1;
	}

	chan = serial_open(port_name, 2048);
	if (!chan) {
		return 1;
	}
	port = serial_port_create(chan, erts, buffer_size);

	main_loop(port);

	return 0;
}
