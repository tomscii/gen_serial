#include "stdafx.h"

#include "erlang_serial.h"
#include "win32serial.h"

#define TXBUF_CNT	2

struct pipe_win32
{
	/* Memory address of the read buffer.  Allocated within the channel and
	 * is kept private to the channel.
	 */
	char*					rxBuf;

	/* The input port.  Data coming to this program from erts comes in on
	 * this port.
	 */
	HANDLE					hIn;

	/* The output port.  Data leaving this program to go into erts goes
	 * out through this port.
	 */
	HANDLE					hOut;

	/* The in port has data ready for reading.  The application should
	 * use ReadFile to pull the data from the port.  This is a manual reset
	 * event which is reset before ReadFile.
	 */
	HANDLE					eRxReady;

	/* The out port has finished writing the user's data buffer.  The
	 * user can now recycle the buffer, and can send a new buffer to
	 * pipe_write if necessary.
	 */
	HANDLE					eTxReady;

	HANDLE					eRxStart;
	HANDLE					eTxStart;
	HANDLE					eRxExit;
	HANDLE					eTxExit;
	HANDLE					rxThread;
	HANDLE					txThread;

	/* The last error processed by pipe_read or pipe_write.  This is a copy of
	 * rxLastError or txLastError.
	 */
	DWORD					lastError;

	/* The last error received by the read thread.
	 */
	DWORD					rxLastError;

	/* The last error received by the write thread.
	 */
	DWORD					txLastError;

	/* Buffer structures used by the tx background thread.  Each buffer
	 * represents one chunk of data to be written.  Multiple chunks can
	 * be written at once by filling multiple structures.  Structures
	 * which are not used should have a len == 0.
	 */
	struct
	{
		char*				ptr;
		int					len;
	} txBuf[TXBUF_CNT];

	/* Total number of bytes already sent and confirmed.  This is updated each
	 * time eTxReady is signaled, and the write ends when txSentLen == txBufLen.
	 * When txSentLen == txBufLen txBuf is set to NULL and eTxReady is manually
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

HANDLE pipe_os_readable (struct pipe_channel* _chan)
{
	struct pipe_win32*		chan = (struct pipe_win32*)_chan;

	return chan->eRxReady;
}

HANDLE pipe_os_writeable (struct pipe_channel* _chan)
{
	struct pipe_win32*		chan = (struct pipe_win32*)_chan;

	return chan->eTxReady;
}

int pipe_last_error (struct pipe_channel* _chan)
{
	struct pipe_win32*		chan = (struct pipe_win32*)_chan;

	if ( ! chan)
	{
		return GetLastError();
	}

	return ERROR_SUCCESS == chan->lastError ? 0 : chan->lastError;
}

void pipe_format_error (
	struct pipe_channel*	chan,
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

static DWORD WINAPI windows_pipe_read_thread (LPVOID lpParameter)
{
	struct pipe_win32*		chan = (struct pipe_win32*)lpParameter;
	DWORD					recv;
	BOOL					rok;
	HANDLE					evts[2];

	evts[0] = chan->eRxExit;
	evts[1] = chan->eRxStart;

	for (;;)
	{
		if (WAIT_OBJECT_0 == WaitForMultipleObjects(2, evts, FALSE, INFINITE))
		{
			ExitThread(0);
		};

		chan->rxLastError = 0;

		while (chan->rxBufLen < chan->rxBufSz)
		{
			if (WaitForSingleObject(chan->eRxExit, 0) == WAIT_OBJECT_0)
			{
				ExitThread(0);
			}
		
			rok = ReadFile(
				chan->hIn,
				chan->rxBuf + chan->rxBufLen,
				chan->rxBufSz - chan->rxBufLen,
				&recv,
				NULL);

			if (rok)
			{
				chan->rxBufLen += recv;
				SetEvent(chan->eRxReady);

			} else
			{
				/* If an error occurred, record the error code and break
				 * from the buffer fill loop.  The read ready event will
				 * be signaled, and the error will get picked up.
				 */
				chan->rxLastError = GetLastError();
				break;
			}
		}

		SetEvent(chan->eRxReady);
	}
}

static DWORD WINAPI windows_pipe_write_thread (LPVOID lpParameter)
{
	struct pipe_win32*	chan = (struct pipe_win32*)lpParameter;
	HANDLE				evts[2];
	DWORD				sent;
	BOOL				rok;
	int					b;

	evts[0] = chan->eTxExit;
	evts[1] = chan->eTxStart;

	for (;;)
	{
		if (WAIT_OBJECT_0 == WaitForMultipleObjects(2, evts, FALSE, INFINITE))
		{
			ExitThread(0);
		};

		chan->txLastError = 0;

		for (b = 0; b < TXBUF_CNT; b++)
		{
			int		left = chan->txBuf[b].len;
			char*	ptr = chan->txBuf[b].ptr;

			while (left > 0)
			{
				if (WaitForSingleObject(chan->eTxExit, 0) == WAIT_OBJECT_0)
				{
					ExitThread(0);
				}

				rok = WriteFile(
					chan->hOut,
					ptr,
					(DWORD)left,
					&sent,
					NULL);

				if (rok)
				{
					ptr += sent;
					left -= sent;
					chan->txSentLen += sent;

				} else
				{
					chan->txLastError = GetLastError();
					goto signal_ready;
				}
			}
		}

signal_ready:
		SetEvent(chan->eTxReady);
	}
}

struct pipe_channel* pipe_os_open (int bufsz)
{
	struct pipe_win32*	chan;
	DWORD				threadid;
	
	chan = (struct pipe_win32*)malloc(sizeof(struct pipe_win32));
	memset(chan, 0, sizeof(struct pipe_win32));

	/* Open channels to Erlang. These are stdin and stdout.
	 */
	chan->hIn = GetStdHandle(STD_INPUT_HANDLE);
	if (INVALID_HANDLE_VALUE == chan->hIn)
	{
		DWORD	ecode = GetLastError();

		free(chan);
		SetLastError(ecode);
		return NULL;
	}

	chan->hOut = GetStdHandle(STD_OUTPUT_HANDLE);
	if (INVALID_HANDLE_VALUE == chan->hOut)
	{
		DWORD	ecode = GetLastError();

		free(chan);
		SetLastError(ecode);
		return NULL;
	}

	/* Create the two events which are used to signal the state of the
	 * overlapped (asynchronous) operations.  Read is intially not read
	 * (buffer is empty) and write is initially ready (no IO in progress
	 * so the buffer is available to be used).  These are manually reset
	 * by the pipe_read and pipe_write functions.
	 */
	chan->eRxReady = CreateEvent(NULL, TRUE, FALSE, NULL);
	chan->eTxReady = CreateEvent(NULL, TRUE, TRUE, NULL);

	/* Events used to tell the background threads when to start doing their
	 * magic.  These are signaled by the main thread to get the background
	 * threads going.  Read is initially set to allow the reader to begin
	 * loading data immediately, write is not set as no data is ready to
	 * go out.  These are automatically reset by the operating system so
	 * the background threads don't resume oprations automatically.
	 */
	chan->eRxStart = CreateEvent(NULL, FALSE, TRUE, NULL);
	chan->eTxStart = CreateEvent(NULL, FALSE, FALSE, NULL);

	/* Create the exit events.  These are signaled when the pipe_close
	 * function is called to tell the two background threads to wakeup
	 * and exit normally.  We only wait a short period of time for them
	 * to cleanup, hoping that they aren't blocked for very long in an
	 * IO method while doing so.
	 */
	chan->eRxExit = CreateEvent(NULL, TRUE, FALSE, NULL);
	chan->eTxExit = CreateEvent(NULL, TRUE, FALSE, NULL);

	/* Allocate the input buffer.  The buffer is larger than the
	 * caller asked for to make room for the packet header coming in from
	 * erts.  We round up here as the end user supplies bufsz and doesn't
	 * know that we have an overhead of 5 bytes per packet.
	 */
	chan->rxBufSz = bufsz + 256;
	chan->rxBuf = (char*)malloc(chan->rxBufSz);

	/* Create the background threads.  These threads perform the actual
	 * IO work, and allow the main thread to just run between Erlang
	 * and the IO threads.  This creates a non-blocking IO system from
	 * a standard blocking IO system in Windows.
	 */
	chan->rxThread = CreateThread(
		NULL,
		2048,
		windows_pipe_read_thread,
		(LPVOID)chan,
		0,
		&threadid);
	if ( ! chan->rxThread)
	{
		DWORD	ecode = GetLastError();

		free(chan);
		SetLastError(ecode);
		return NULL;
	}

	chan->txThread = CreateThread(
		NULL,
		2048,
		windows_pipe_write_thread,
		(LPVOID)chan,
		0,
		&threadid);
	if ( ! chan->txThread)
	{
		DWORD	ecode = GetLastError();

		free(chan);
		SetLastError(ecode);
		return NULL;
	}

	return (struct pipe_channel*)chan;
}

int pipe_write (
	struct pipe_channel*	_chan,
	void*					buf,
	int						len)
{
	struct pipe_win32*	chan = (struct pipe_win32*)_chan;

	/* If an error occurred in the background thread, copy it to
	 * the lastError field and return -1.  If no error has occurred,
	 * this will also reset lastError.
	 */
	if (chan->lastError = chan->txLastError)
	{
		return -1;
	}

	if (0 == chan->txBuf[0].len)
	{
		/* If no IO is in progress, load the caller's buffer into the
		 * txBuf, and signal the thread to begin writing.  Return -1 so
		 * the caller will call us again with the same buf parameter.
		 */
		int		b;

		for (b = 1; b < TXBUF_CNT; b++)
		{
			chan->txBuf[b].ptr = NULL;
			chan->txBuf[b].len = 0;
		}

		chan->txBuf[0].ptr = (char*)buf;
		chan->txBuf[0].len = len;
		chan->txSentLen = 0;

		ResetEvent(chan->eTxReady);
		SetEvent(chan->eTxStart);

		return -1;

	} else if (   buf == chan->txBuf[0].ptr
			   && NULL == chan->txBuf[1].ptr
			   && chan->txBuf[0].len == chan->txSentLen)
	{
		/* Buffer was fully written.  Return the size of the buffer
		 * to the caller so they know it was written out.
		 */
		chan->txBuf[0].ptr = NULL;
		chan->txBuf[0].len = 0;

		SetEvent(chan->eTxReady);
		return chan->txSentLen;

	} else
	{
		/* Any other state, we aren't done yet, so reset the event as
		 * it should not have been set, and return -1 to let the
		 * caller know they should call us again with the same buf
		 * parameter.
		 */
		ResetEvent(chan->eTxReady);
		return -1;
	}
}

int pipe_write2 (
	struct pipe_channel*	_chan,
	void*					buf1,
	int						len1,
	void*					buf2,
	int						len2)
{
	struct pipe_win32*	chan = (struct pipe_win32*)_chan;

	/* If an error occurred in the background thread, copy it to
	 * the lastError field and return -1.  If no error has occurred,
	 * this will also reset lastError.
	 */
	if (chan->lastError = chan->txLastError)
	{
		return -1;
	}

	if (0 == chan->txBuf[0].len)
	{
		/* If no IO is in progress, load the caller's buffer into the
		 * txBuf, and signal the thread to begin writing.  Return -1 so
		 * the caller will call us again with the same buf parameter.
		 */
		int			b;

		for (b = 2; b < TXBUF_CNT; b++)
		{
			chan->txBuf[b].ptr = NULL;
			chan->txBuf[b].len = 0;
		}

		chan->txBuf[0].ptr = (char*)buf1;
		chan->txBuf[0].len = len1;

		chan->txBuf[1].ptr = (char*)buf2;
		chan->txBuf[1].len = len2;

		chan->txSentLen = 0;

		ResetEvent(chan->eTxReady);
		SetEvent(chan->eTxStart);

		return -1;

	} else if (   buf1 == chan->txBuf[0].ptr
			   && buf2 == chan->txBuf[1].ptr
			   && (chan->txBuf[0].len + chan->txBuf[1].len) == chan->txSentLen)
	{
		/* Buffer was fully written.  Return the size of the buffer
		 * to the caller so they know it was written out.
		 */
		chan->txBuf[0].ptr = NULL;
		chan->txBuf[0].len = 0;

		chan->txBuf[1].ptr = NULL;
		chan->txBuf[1].len = 0;

		SetEvent(chan->eTxReady);
		return chan->txSentLen;

	} else
	{
		/* Any other state, we aren't done yet, so reset the event as
		 * it should not have been set, and return -1 to let the
		 * caller know they should call us again with the same buf
		 * parameter.
		 */
		ResetEvent(chan->eTxReady);
		return -1;
	}
}

extern int pipe_read (
	struct pipe_channel*	_chan,
	void*					buf,
	int						len)
{
	struct pipe_win32*		chan = (struct pipe_win32*)_chan;
	int						avail;
	int						retv;

	/* If an error occurred in the background thread, copy it to
	 * the lastError field and return -1.  If no error has occurred,
	 * this will also reset lastError.
	 */
	if (chan->lastError = chan->rxLastError)
	{
		return -1;
	}

	/* Determine how many bytes are available in the buffer.  The actual
	 * return code will vary based on this value.
	 */
	avail = chan->rxBufLen - chan->rxRecvLen;

	if (0 == avail)
	{
		/* No data is immediately available, reset the event
		 * so the caller doesn't keep coming back for more until
		 * there is actually more data.
		 */
		ResetEvent(chan->eRxReady);
		retv = -1;

	} else if (avail <= len)
	{
		memcpy(buf, chan->rxBuf + chan->rxRecvLen, avail);
		chan->rxRecvLen += avail;

		/* No more data is immediately available, reset the event
		 * so the caller doesn't keep coming back for more until
		 * there is more.
		 */
		ResetEvent(chan->eRxReady);
		retv = avail;

	} else
	{
		memcpy(buf, chan->rxBuf + chan->rxRecvLen, len);
		chan->rxRecvLen += len;

		/* We still have more data immediately ready, so signal the
		 * event to make sure the caller comes back again for more
		 * data.
		 */
		SetEvent(chan->eRxReady);
		retv = len;
	}

	/* If the buffer is full, and we've read all bytes in the buffer,
	 * clear the buffer and signal the reader to start up again.
	 */
	if (   chan->rxBufLen == chan->rxBufSz
		&& chan->rxRecvLen == chan->rxBufLen)
	{
		chan->rxBufLen = 0;
		chan->rxRecvLen = 0;

		SetEvent(chan->eRxStart);
	}

	return retv;
}

void pipe_close (struct pipe_channel* _chan)
{
	struct pipe_win32*		chan = (struct pipe_win32*)_chan;

	if (chan->rxThread != INVALID_HANDLE_VALUE)
	{
		SetEvent(chan->eRxExit);
		WaitForSingleObject(chan->rxThread, 10000L);
		CloseHandle(chan->rxThread);
	}
	
	if (chan->txThread != INVALID_HANDLE_VALUE)
	{
		SetEvent(chan->eTxExit);
		WaitForSingleObject(chan->txThread, 10000L);
		CloseHandle(chan->txThread);
	}

	if ( ! CloseHandle(chan->eRxExit))
	{
		windows_error("pipe_close, CloseHandle(eRxExit)");
	}

	if ( ! CloseHandle(chan->eTxExit))
	{
		windows_error("pipe_close, CloseHandle(eTxExit)");
	}

	if ( ! CloseHandle(chan->eRxStart))
	{
		windows_error("pipe_close, CloseHandle(eRxStart)");
	}

	if ( ! CloseHandle(chan->eTxStart))
	{
		windows_error("pipe_close, CloseHandle(eTxStart)");
	}

	if ( ! CloseHandle(chan->eRxReady))
	{
		windows_error("pipe_close, CloseHandle(eRxReady)");
	}

	if ( ! CloseHandle(chan->eTxReady))
	{
		windows_error("pipe_close, CloseHandle(eTxReady)");
	}

	if (chan->rxBuf)
	{
		free(chan->rxBuf);
	}

	free(chan);
}