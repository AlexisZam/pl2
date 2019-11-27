CC = gcc
CFLAGS = -g #-O3 -Wall -Wextra -Wno-char-subscripts -Wno-unused-result# -Wpedantic
THREADING ?= -D DIRECT_THREADING# -D INDIRECT_THREADING -D NO_THREADING
STACK ?= -D DYNAMIC_ARRAY# -D STATIC_ARRAY -D LINKED_LIST
PRINT_STACK ?= -D PRINT_STACK
HEAP ?= -D STATIC_HEAP#-D DYNAMIC_HEAP
GARBAGE_COLLECTOR ?= -D MARK_AND_SWEEP#-D COPYING
CFLAGS += $(THREADING) $(STACK) $(PRINT_STACK) $(HEAP) $(GARBAGE_COLLECTOR)

.PHONY:	all clean

all: befunge93 befunge93+

befunge93: src/befunge93.c src/stack.c
	$(CC) $(CFLAGS) -o bin/befunge93 src/befunge93.c src/stack.c

befunge93+: src/befunge93.c src/stack.c src/heap.c
	$(CC) $(CFLAGS) -D BEFUNGE93PLUS -o bin/befunge93+ src/befunge93.c src/stack.c src/heap.c

clean:
	$(RM) bin/befunge93 bin/befunge93+
