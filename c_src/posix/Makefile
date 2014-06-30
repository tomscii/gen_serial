SOURCES = ../erlang_serial.c $(wildcard *.c)
HEADERS = ../erlang_serial.h $(wildcard *.h)
OBJS = $(SOURCES:%.c=%.o)
CFLAGS = -Wall -O2 -g -ggdb

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
