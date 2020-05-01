CC = gcc-9
CFLAGS = -Ofast -Wall -Wextra -Wno-char-subscripts -Wno-unused-result

.PHONY:	all clean

all: gc

vm: gc.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	$(RM) gc
