CC = gcc
CFLAGS = -g#-O3 -Wall -Wextra -Wno-char-subscripts -Wno-unused-result -Wno-missing-field-initializers -Wno-unused-label# -Wpedantic
CFLAGS +=# -D THREADING
CFLAGS +=# -D DIRECT_THREADING
CFLAGS +=# -D DYNAMIC_STACK
CFLAGS += -D PRINT_STACK
CFLAGS +=# -D DYNAMIC_HEAP
CFLAGS += -D PRINT_HEAP
CFLAGS +=# -D COPYING

.PHONY:	all clean

all: befunge93 befunge93+

befunge93: src/befunge93.c src/stack.c
	$(CC) $(CFLAGS) -o bin/befunge93 src/befunge93.c src/stack.c

befunge93+: src/befunge93.c src/stack.c src/heap.c
	$(CC) $(CFLAGS) -D BEFUNGE93PLUS -o bin/befunge93+ src/befunge93.c src/stack.c src/heap.c

clean:
	$(RM) bin/befunge93 bin/befunge93+
