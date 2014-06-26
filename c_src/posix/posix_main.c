#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <poll.h>
#include <errno.h>

#include "../erlang_serial.h"

struct serial_posix {
	struct serial_channel channel;
	int fd;

	/* Error of the last call. This is directly from errno */
	int lastError;

	/* more private fields... */
};

struct serial_channel *serial_open(char *port_name, int bufsz)
{
	struct serial_posix *chan;

	chan = (struct serial_posix *)malloc(sizeof(struct serial_posix));
	memset(chan, 0, sizeof(struct serial_posix));

	/* TODO alloc/init priv fields */

	return (struct serial_channel *)chan;
}

int serial_configure(struct serial_channel *_chan, struct serial_cfg *cfg)
{
	struct serial_posix *chan = (struct serial_posix *)_chan;

	/* Allocate buffers in the structure, and load the configuration into
	 * the structure.  This is a basic activity used on all platforms.
	 */
	if (serial_init_struct(_chan, cfg) != 0) {
		chan->lastError = ERROR_INVALID_DATA;
		return -1;
	}

	/* TODO init device */

	return 0;
}

int serial_flush(struct serial_channel *_chan)
{
	struct serial_posix *chan = (struct serial_posix *)_chan;

	/* TODO */
	return 0;
}

int serial_write(struct serial_channel *_chan, void *buf, int len)
{
	struct serial_posix *chan = (struct serial_posix *)_chan;
	/* TODO */
	return -1;
}

int serial_read(struct serial_channel *_chan, void *_buf, int len)
{
	struct serial_posix *chan = (struct serial_posix *)_chan;
	/* TODO */

	return -1;
}

void serial_close(struct serial_channel *_chan)
{
	struct serial_posix *chan = (struct serial_posix *)_chan;

	serial_destroy_struct(_chan);

	/* TODO */

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

	int fd;

	/* Error of the last call. This is directly from errno */
	int lastError;

	/* TODO */
};

struct pipe_channel *pipe_os_open(int bufsz)
{
	struct pipe_posix *chan;

	chan = (struct pipe_posix *)malloc(sizeof(struct pipe_posix));
	memset(chan, 0, sizeof(struct pipe_posix));

	/* Open channels to Erlang. These are stdin and stdout. */

	return (struct pipe_channel *)chan;
}

int pipe_write(struct pipe_channel *_chan, void *buf, int len)
{
	struct pipe_posix *chan = (struct pipe_posix *)_chan;

	/* TODO */
	return -1;
}

int pipe_write2(struct pipe_channel *_chan,
		void *buf1, int len1, void *buf2, int len2)
{
	struct pipe_posix *chan = (struct pipe_posix *)_chan;

	/* TODO */
	return -1;
}

int pipe_read(struct pipe_channel *_chan, void *buf, int len)
{
	struct pipe_posix *chan = (struct pipe_posix *)_chan;

	/* TODO */
	return -1;
}

void pipe_close(struct pipe_channel *_chan)
{
	struct pipe_posix *chan = (struct pipe_posix *)_chan;
	/* TODO */
	free(chan);
}

struct pollfd pipe_os_readable(struct pipe_channel *_chan)
{
	struct pipe_posix *chan = (struct pipe_posix *)_chan;
	struct pollfd ret;
	ret.fd = chan->fd;
	ret.events = POLLIN;
	return ret;
}

struct pollfd pipe_os_writeable(struct pipe_channel *_chan)
{
	struct pipe_posix *chan = (struct pipe_posix *)_chan;
	struct pollfd ret;
	ret.fd = chan->fd;
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
	int pres;

#ifdef _DEBUG
	char dbgmsg[256];
#endif

	int erts_fd = ((struct pipe_posix*)(port->erts))->fd;
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
		pres = poll(pollfds, nfds, -1);

		for (i = 0; i < nfds; i++) {
			if (pollfds[i].fd == erts_fd) {
				if (pollfds[i].revents & POLLIN) {
					serial_port_ertsr(port);
				} else if (pollfds[i].revents & POLLOUT) {
					serial_port_ertsw(port);
				}
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

	port = serial_port_create(serial_open(port_name, 2048), erts, buffer_size);
	main_loop(port);

	return 0;
}
