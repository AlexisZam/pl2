CC = gcc-9
CFLAGS = -Ofast -Wall -Wextra -Wno-char-subscripts -Wno-unused-result

.PHONY:	all clean

all: vm

vm: vm.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	$(RM) vm
