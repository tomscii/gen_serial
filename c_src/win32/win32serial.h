#pragma once

extern void windows_error (char* operation);
extern void windows_error2 (char* operation, int errcode);

extern HANDLE serial_os_readable (struct serial_channel* _chan);
extern HANDLE serial_os_writeable (struct serial_channel* _chan);

extern HANDLE pipe_os_readable (struct pipe_channel* _chan);
extern HANDLE pipe_os_writeable (struct pipe_channel* _chan);

extern struct pipe_channel* pipe_os_open (int bufsz);
