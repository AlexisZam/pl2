CFLAGS = -Ofast -Wall -Wextra -Wno-char-subscripts -Wno-unused-result

all: vm

vm: vm.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	$(RM) vm
