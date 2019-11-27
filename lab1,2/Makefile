CC = gcc
CFLAGS = -O3 -Wall -Wextra -Wno-char-subscripts -Wno-unused-result# -Wpedantic
THREADING ?= -D THREADING
DIRECT_THREADING ?= -D DIRECT_THREADING
DYNAMIC_STACK ?=# -D DYNAMIC_STACK
PRINT_STACK ?= -D PRINT_STACK
DYNAMIC_HEAP ?=# -D DYNAMIC_HEAP
PRINT_HEAP ?= -D PRINT_HEAP
GARBAGE_COLLECTOR ?=# -D COPYING
CFLAGS += $(THREADING) $(DYNAMIC_STACK) $(PRINT_STACK) $(DYNAMIC_HEAP) $(PRINT_HEAP) $(GARBAGE_COLLECTOR)

.PHONY:	all clean

all: befunge93 befunge93+

befunge93: src/befunge93.c src/stack.c
	$(CC) $(CFLAGS) -o bin/befunge93 src/befunge93.c src/stack.c

befunge93+: src/befunge93.c src/stack.c src/heap.c
	$(CC) $(CFLAGS) -D BEFUNGE93PLUS -o bin/befunge93+ src/befunge93.c src/stack.c src/heap.c

clean:
	$(RM) bin/befunge93 bin/befunge93+
