SOURCES = ../erlang_serial.c $(wildcard *.c)
HEADERS = ../erlang_serial.h $(wildcard *.h)
OBJS = $(SOURCES:%.c=%.o)
CFLAGS ?= -Wall -Wextra -O2 -g -ggdb
LIBS = 

all: serial_esock

serial_esock: $(OBJS)
	$(CC) -o $@ $^ $(LIBS)

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -rf *~ $(OBJS) serial_esock

.PHONY: all clean
