CFLAGS = -Ofast -Wall -Wextra -Wno-char-subscripts -Wno-unused-result

all: gc

vm: gc.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	$(RM) gc
