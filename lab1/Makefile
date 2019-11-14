CC = gcc
CFLAGS = -O3 -Wall

.PHONY:	all clean

all: befunge93

befunge93: befunge93.c
	$(CC) $(CFLAGS) -o befunge93 befunge93.c -DSTACK

clean:
	$(RM) befunge93