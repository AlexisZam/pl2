CC = gcc
CFLAGS = -O3 -Wall -Wno-char-subscripts -Wno-unused-result
CFLAGS += -D DIRECT_THREADING# -D INDIRECT_THREADING -D NO_THREADING
CFLAGS += -D STATIC_ARRAY# -D DYNAMIC_ARRAY -D LINKED_LIST
CFLAGS += -D PRINT_STACK

.PHONY:	all clean

all: befunge93 befunge93+

befunge93: befunge93.c stack.c
	$(CC) $(CFLAGS) -o befunge93 befunge93.c stack.c

befunge93+: befunge93.c stack.c heap.c
	$(CC) $(CFLAGS) -D BEFUNGE93PLUS -o befunge93+ befunge93.c stack.c heap.c

clean:
	$(RM) befunge93 befunge93+
