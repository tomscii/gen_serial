SOURCES = ../erlang_serial.c $(wildcard *.c)
HEADERS = ../erlang_serial.h $(wildcard *.h)
OBJS = $(SOURCES:%.c=%.o)

CFLAGS = -Wall -O2 -g -ggdb
# Using the below CFLAGS will add support for the
# {debug_log, Filename} option to gen_serial:open/2. Using this option
# enables low-level logging of the backend driver to the specified file.
#CFLAGS = -Wall -O2 -g -ggdb -D_DEBUG


all: ../../priv/bin/serial_esock

../../priv/bin/serial_esock: serial_esock
	cp serial_esock ../../priv/bin/

serial_esock: $(OBJS)
	$(CC) -o $@ $^

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -rf *~ $(OBJS) serial_esock ../../priv/bin/serial_esock

.PHONY: all clean
