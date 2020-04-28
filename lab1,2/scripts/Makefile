CC = gcc
CFLAGS = -O3 -Wall -Wextra -Wno-char-subscripts -Wno-unused-result -Wno-missing-field-initializers -Wno-unused-label -Wno-format# -Wpedantic
CFLAGS += -D DIRECT_THREADING
CFLAGS += -D STACK_SIZE=1024*1024
CFLAGS +=# -D DYNAMIC_STACK -D INIT_STACK_SIZE=1024
CFLAGS +=# -D PRINT_STACK -D STACK_FD=3
CFLAGS += -D HEAP_SIZE=16*1024*1024
CFLAGS +=# -D PRINT_HEAP -D HEAP_FD=4
CFLAGS +=# -D COPYING

.PHONY:	all clean

all: befunge93 befunge93+

befunge93: ../src/befunge93.c ../src/stack.c
	$(CC) $(CFLAGS) -o ../bin/befunge93 ../src/befunge93.c ../src/stack.c

befunge93+: ../src/befunge93.c ../src/stack.c ../src/heap.c
	$(CC) $(CFLAGS) -D BEFUNGE93PLUS -o ../bin/befunge93+ ../src/befunge93.c ../src/stack.c ../src/heap.c

clean:
	$(RM) ../bin/befunge93 ../bin/befunge93+
