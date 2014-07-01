// win32serial.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include "erlang_serial.h"
#include "win32serial.h"

int is_fatal_error (int errcode)
{
        if (errcode == ERROR_OPERATION_ABORTED) {
                return 1;
        }
        return 0;
}

void windows_error (char* operation)
{
#ifdef _DEBUG
	windows_error2(operation, GetLastError());
#endif
}

void windows_error2 (char* operation, int errcode)
{
#ifdef _DEBUG
	LPVOID		lpMsgBuf;

	if (debug_log_file)
	{
		FormatMessage(
			FORMAT_MESSAGE_ALLOCATE_BUFFER
			| FORMAT_MESSAGE_FROM_SYSTEM
			| FORMAT_MESSAGE_IGNORE_INSERTS,
			NULL,
			errcode,
			MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			(LPTSTR)&lpMsgBuf,
			0,
			NULL);

		fprintf(debug_log_file, "ERROR in %s: %i: %s\n",
			operation, errcode, lpMsgBuf);
		fflush(debug_log_file);
		
		LocalFree(lpMsgBuf);
	}
#endif
}

static void main_loop (struct serial_port* port)
{
	DWORD		wres;
	DWORD		nEvents;
	HANDLE		theEvent;
	HANDLE		wEvents[8];

#ifdef _DEBUG
	char		dbgmsg[256];
#endif

	for (;;)
	{
		nEvents = 0;

		DBG_LOG(strcpy(dbgmsg, "main_loop: wait for"));
	
		if (serial_port_watch_ertsr(port))
		{
			wEvents[nEvents++] = pipe_os_readable(port->erts);
			DBG_LOG(strcat(dbgmsg, " ertsr"));
		}

		if (serial_port_watch_ertsw(port))
		{
			wEvents[nEvents++] = pipe_os_writeable(port->erts);
			DBG_LOG(strcat(dbgmsg, " ertsw"));
		}

		if (serial_port_watch_serialr(port))
		{
			wEvents[nEvents++] = serial_os_readable(port->chan);
			DBG_LOG(strcat(dbgmsg, " serialr"));
		}
		
		if (serial_port_watch_serialw(port))
		{
			wEvents[nEvents++] = serial_os_writeable(port->chan);
			DBG_LOG(strcat(dbgmsg, " serialw"));
		}

		DBG_LOG(fprintf(debug_log_file, "%s\n", dbgmsg));

		/* If no events are enabled, we are really up a creek.  Shutdown the
		 * process right now.
		 */
		if (0 == nEvents)
		{
			serial_port_destroy(port);
			ExitProcess(1);
		}

		/* Wait for any of the above events to become true.  Break out
		 * as soon as one of them is in fact true.
		 */
		wres = WaitForMultipleObjects(nEvents, wEvents, FALSE, INFINITE);
		if (wres < WAIT_OBJECT_0)
		{
			continue;
		}

		/* Determine the event which triggered this wakeup.  Unlike on
		 * UNIX, we only know that one event in fact occurred, all of
		 * the others require us to go wait for them again.
		 */
		theEvent = wEvents[wres - WAIT_OBJECT_0];

		if (theEvent == pipe_os_readable(port->erts))
		{
			serial_port_ertsr(port);

		} else if (theEvent == pipe_os_writeable(port->erts))
		{
			serial_port_ertsw(port);

		} else if (theEvent == serial_os_readable(port->chan))
		{
			serial_port_serialr(port);

		} else if (theEvent == serial_os_writeable(port->chan))
		{
			serial_port_serialw(port);
		}

		/* If something went horribly wrong, the generic code should
		 * have sent a message to Erlang indicating the error.  It
		 * also flagged the port dead, so we know to terminate the
		 * process.
		 */
		if (serial_port_isdead(port))
		{
			serial_port_destroy(port);
			ExitProcess(1);
		}
	}
}

int main(int argc, char* argv[])
{
	struct pipe_channel*		erts;
	struct serial_port*			port;
	char*						port_name;
	char portname_buf[128];
	int							buffer_size;

#ifdef _DEBUG
	char*						debug_file_name;
#endif

	if (argc < 3)
	{
#ifdef _DEBUG
		fprintf(stderr,
			"usage: serial_esock.exe COMn buffersz [debug.txt]\n"
			"       where\n"
			"           COMn = any Windows COM port name.\n"
			"           buffersz = size of buffers, in bytes.\n"
			"           debug.txt = debug log file.\n");
#else
		fprintf(stderr,
			"usage: serial_esock.exe COMn buffersz\n"
			"       where\n"
			"           COMn = any Windows COM port name.\n"
			"           buffersz = size of buffers, in bytes.\n");
#endif
		return 1;
	}

	port_name = argv[1];

	/* for COM10 and above, we need to prepend \\.\
	 * see: http://support.microsoft.com/kb/115831
	 */
	if (!strncmp(port_name, "COM", 3) && atoi(port_name+3) > 9) {
	        sprintf(portname_buf, "\\\\.\\%s", port_name);
	        port_name = portname_buf;
	}

	buffer_size = atoi(argv[2]);
	if (buffer_size < 128)
	{
		buffer_size = 128;
	}

#ifdef _DEBUG
	if (argc == 4 && argv[3] && *argv[3])
	{
		debug_file_name = argv[3];
	} else
	{
		debug_file_name = NULL;
	}

	if (debug_file_name)
	{
		debug_log_file = fopen(debug_file_name, "w");
		if ( ! debug_log_file)
		{
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
	if ( ! erts)
	{
		DBG_LOG(fprintf(debug_log_file, "pipe_open failed.\n"));
		return 1;
	}

	port = serial_port_create(serial_open(port_name, 2048), erts, buffer_size);
	main_loop(port);

	return 0;
}
