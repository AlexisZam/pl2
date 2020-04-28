#ifdef BEFUNGE93PLUS
#include "heap.h"
#endif /* BEFUNGE93PLUS */
#include "stack.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#ifdef DIRECT_THREADING
#define NEXT()     \
    next();        \
    print_stack(); \
    goto **pc;
#else
#define NEXT()     \
    next();        \
    print_stack(); \
    goto *labels[program[j * WIDTH + i]];
#endif /* DIRECT_STHREADING */

/* Global variables */

int i = 0, j = 0;
int di = 1, dj = 0;
void **pc;

#define HEIGHT 25
#define WIDTH 80
char program[] = {[0 ... HEIGHT * WIDTH - 1] = ' '};

void next() {
    int temp1 = j, temp2 = i;
    i = (i + di + WIDTH) % WIDTH;
    j = (j + dj + HEIGHT) % HEIGHT;
    pc += (j - temp1) * WIDTH + (i - temp2);
}

void parse_program(const char *filename) {
    FILE *stream = fopen(filename, "r");
    if (!stream) {
        fprintf(stderr, "Error: couldn't open '%s' for input.\n", filename);
        exit(EXIT_FAILURE);
    }
    for (;;) {
        int c = fgetc(stream);
        if (feof(stream))
            goto eof;
        if (c == '\n') {
            i = 0;
            j++;
            if (j >= HEIGHT)
                goto eof;
        } else {
            program[j * WIDTH + i] = c;
            i++;
            if (i >= WIDTH)
                for (;;) {
                    c = fgetc(stream);
                    if (feof(stream))
                        goto eof;
                    if (c == '\n') {
                        i = 0;
                        j++;
                        if (j >= HEIGHT)
                            goto eof;
                        break;
                    }
                }
        }
    }
eof:
    fclose(stream);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s [-s stack] [-h heap] foo.bf\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    parse_program(argv[1]);

#define ASCII 128
    void *labels[] = {[0 ... ASCII - 1] = &&unsupported};
    for (char c = '0'; c <= '9'; c++)
        labels[c] = &&digit;
    labels['+'] = &&add;
    labels['-'] = &&subtract;
    labels['*'] = &&multiply;
    labels['/'] = &&divide;
    labels['%'] = &&modulo;
    labels['!'] = &&not;
    labels['`'] = &&greater;
    labels['>'] = &&right;
    labels['<'] = &&left;
    labels['v'] = &&down;
    labels['^'] = &&up;
    labels['?'] = &&random;
    labels['_'] = &&horizontal_if;
    labels['|'] = &&vertical_if;
    labels['"'] = &&stringmode;
    labels[':'] = &&dup;
    labels['\\'] = &&swap;
    labels['$'] = &&pop;
    labels['.'] = &&output_int;
    labels[','] = &&output_char;
    labels['#'] = &&bridge;
    labels['g'] = &&get;
    labels['p'] = &&put;
    labels['&'] = &&input_int;
    labels['~'] = &&input_char;
    labels['@'] = &&end;
    labels[' '] = &&nop;
#ifdef BEFUNGE93PLUS
    labels['c'] = &&cons;
    labels['h'] = &&head;
    labels['t'] = &&tail;
#endif /* BEFUNGE93PLUS */

#ifdef DIRECT_THREADING
    void *program_as_labels[HEIGHT * WIDTH];
    for (int j = 0; j < HEIGHT; j++)
        for (int i = 0; i < WIDTH; i++)
            program_as_labels[j * WIDTH + i] = labels[program[j * WIDTH + i]];
    pc = program_as_labels;
#endif /* DIRECT_THREADING */

    i = j = 0;

    srand(time(NULL));

    init_stack();
#ifdef BEFUNGE93PLUS
    init_heap();
#endif /* BEFUNGE93PLUS */

    goto **pc;
digit:
    push((struct value){program[j * WIDTH + i] - '0'});
    NEXT()
add:
    push((struct value){pop().value + pop().value});
    NEXT()
subtract : {
    long_t temp = pop().value;
    push((struct value){pop().value - temp});
    NEXT()
}
multiply:
    push((struct value){pop().value * pop().value});
    NEXT()
divide : {
    long_t temp = pop().value;
    if (temp == 0) {
        pop();
        printf("What do you want %ld/0 to be? ", temp);
        scanf("%ld", &temp);
        push((struct value){temp});
    } else
        push((struct value){pop().value / temp});
    NEXT()
}
modulo : {
    long_t temp = pop().value;
    if (temp == 0) {
        pop();
        printf("What do you want %ld/0 to be? ", temp);
        scanf("%ld", &temp);
        push((struct value){temp});
    } else
        push((struct value){pop().value % temp});
    NEXT()
}
    not : push((struct value){!pop().value});
    NEXT()
greater : {
    long_t temp = pop().value;
    push((struct value){pop().value > temp});
    NEXT()
}
right:
    di = 1;
    dj = 0;
    NEXT()
left:
    di = -1;
    dj = 0;
    NEXT()
down:
    di = 0;
    dj = 1;
    NEXT()
up:
    di = 0;
    dj = -1;
    NEXT()
random:
    switch (rand() % 4) {
    case 0:
        goto right;
    case 1:
        goto left;
    case 2:
        goto down;
    case 3:
        goto up;
    }
horizontal_if:
    if (pop().value)
        goto left;
    goto right;
vertical_if:
    if (pop().value)
        goto up;
    goto down;
stringmode:
    for (;;) {
        next();
        print_stack();
        if (program[j * WIDTH + i] == '"')
            break;
        push((struct value){program[j * WIDTH + i]});
    }
    NEXT()
dup : {
    struct value temp = pop();
    push(temp);
    push(temp);
    NEXT()
}
swap : {
    struct value temp1 = pop();
    struct value temp2 = pop();
    push(temp1);
    push(temp2);
    NEXT()
}
pop:
    pop();
    NEXT()
output_int:
    printf("%ld ", pop().value);
    fflush(stdout);
    NEXT()
output_char:
    printf("%c", (char)pop().value);
    fflush(stdout);
    NEXT()
bridge:
    next();
    NEXT()
get : {
    long_t temp1 = pop().value;
    long_t temp2 = pop().value;
    if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
        fprintf(stderr, "g 'Get' instruction out of bounds (%ld,%ld)\n", temp2, temp1);
        push((struct value){0});
    } else
        push((struct value){program[temp1 * WIDTH + temp2]});
    NEXT()
}
put : {
    long_t temp1 = pop().value;
    long_t temp2 = pop().value;
    if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
        fprintf(stderr, "p 'Put' instruction out of bounds (%ld,%ld)\n", temp2, temp1);
        pop();
    } else {
        program[temp1 * WIDTH + temp2] = pop().value;
#ifdef DIRECT_THREADING
        program_as_labels[temp1 * WIDTH + temp2] = labels[program[temp1 * WIDTH + temp2]];
#endif /* DIRECT_THREADING */
    }
    NEXT()
}
input_int : {
    long_t temp = -1;
    scanf("%ld", &temp);
    push((struct value){temp});
    NEXT()
}
input_char:
    push((struct value){getchar()});
    NEXT()
end:
    exit(EXIT_SUCCESS);
nop:
    NEXT()
#ifdef BEFUNGE93PLUS
cons : {
    struct value temp = pop();
    push(allocate(pop(), temp));
    print_heap();
    NEXT()
}
head:
    push(head(pop()));
    NEXT()
tail:
    push(tail(pop()));
    NEXT()
#endif /* BEFUNGE93PLUS */
unsupported:
    fprintf(stderr, "Unsupported instruction '%c' (0x%02x) (maybe not Befunge-93?)\n", program[j * WIDTH + i], program[j * WIDTH + i]);
    NEXT()
}
