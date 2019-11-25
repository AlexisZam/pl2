CC = gcc
CFLAGS = -O3 -Wall -DSTACK -DIMMEDIATE_THREADING -DLINKED_LIST

.PHONY:	all clean

all: befunge93

befunge93: befunge93.c stack.c
	$(CC) $(CFLAGS) -o befunge93 befunge93.c stack.c

clean:
	$(RM) befunge93
