CC = gcc
CFLAGS = -O3 -Wall -Wno-char-subscripts

.PHONY:	all clean

all: befunge93

befunge93: befunge93.c
	$(CC) $(CFLAGS) -o befunge93 befunge93.c

clean:
	$(RM) befunge93