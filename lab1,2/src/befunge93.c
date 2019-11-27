#ifdef BEFUNGE93PLUS
#include "heap.h"
#endif /* BEFUNGE93PLUS */
#include "stack.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifdef THREADING
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
#else
#define NEXT()     \
    next();        \
    print_stack(); \
    break;
#endif /* THREADING */

/* Global variables */

int i = 0, j = 0;
int di = 1, dj = 0;
#ifdef THREADING
void **pc;
#endif /* THREADING */

#define HEIGHT 25
#define WIDTH 80
char program[] = {[0 ... HEIGHT * WIDTH - 1] = ' '};

void next() {
#ifdef THREADING
    int temp1 = j, temp2 = i;
#endif /* THREADING */
    i = (i + di + WIDTH) % WIDTH;
    j = (j + dj + HEIGHT) % HEIGHT;
#ifdef THREADING
    pc += (j - temp1) * WIDTH + (i - temp2);
#endif /* THREADING */
}

void parse_program(const char *filename) {
    FILE *stream = fopen(filename, "r");
    if (!stream) {
        fprintf(stderr, "Error: couldn't open '%s' for input.\n", filename);
        exit(-1);
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
    if (argc != 2) {
        fprintf(stderr, "Usage: ./befunge93 foo.bf\n");
        return -1;
    }

    parse_program(argv[1]);

#define ASCII 128
#ifdef THREADING
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
#endif /* THREADING */

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

    for (;;) {
        // goto **pc;
        switch (program[j * WIDTH + i]) {
        case '0' ... '9':
        digit:
            push((struct value){program[j * WIDTH + i] - '0'});
            NEXT()
        case '+':
        add:
            push((struct value){pop().value + pop().value});
            NEXT()
        case '-':
        subtract : {
            long_t temp = pop().value;
            push((struct value){pop().value - temp});
            NEXT()
        }
        case '*':
        multiply:
            push((struct value){pop().value * pop().value});
            NEXT()
        case '/':
        divide : {
            long_t temp1 = pop().value;
            long_t temp2 = pop().value;
            if (temp1 == 0) {
                printf("What do you want %ld/0 to be? ", temp1);
                scanf("%ld", &temp1);
                push((struct value){temp1});
            } else
                push((struct value){temp2 / temp1});
            NEXT()
        }
        case '%':
        modulo : { // TODO: division by 0
            long_t temp = pop().value;
            push((struct value){pop().value % temp});
            NEXT()
        }
        case '!':
            not : push((struct value){!pop().value});
            NEXT()
        case '`':
        greater : {
            long_t temp = pop().value;
            push((struct value){pop().value > temp});
            NEXT()
        }
        case '>':
        right:
            di = 1;
            dj = 0;
            NEXT()
        case '<':
        left:
            di = -1;
            dj = 0;
            NEXT()
        case 'v':
        down:
            di = 0;
            dj = 1;
            NEXT()
        case '^':
        up:
            di = 0;
            dj = -1;
            NEXT()
        case '?':
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
        case '_':
        horizontal_if:
            if (pop().value)
                goto left;
            goto right;
        case '|':
        vertical_if:
            if (pop().value)
                goto up;
            goto down;
        case '"':
        stringmode:
            for (;;) {
                next();
                print_stack();
                if (program[j * WIDTH + i] == '"')
                    break;
                push((struct value){program[j * WIDTH + i]});
            }
            NEXT()
        case ':':
        dup : {
            struct value temp = pop();
            push(temp);
            push(temp);
            NEXT()
        }
        case '\\':
        swap : {
            struct value temp1 = pop();
            struct value temp2 = pop();
            push(temp1);
            push(temp2);
            NEXT()
        }
        case '$':
        pop:
            pop();
            NEXT()
        case '.':
        output_int:
            printf("%ld ", pop().value);
            fflush(stdout);
            NEXT()
        case ',':
        output_char:
            printf("%c", (char)pop().value);
            fflush(stdout);
            NEXT()
        case '#':
        bridge:
            next();
            NEXT()
        case 'g':
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
        case 'p':
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
        case '&':
        input_int : {
            long_t temp = -1;
            scanf("%ld", &temp);
            push((struct value){temp});
            NEXT()
        }
        case '~':
        input_char:
            push((struct value){getchar()});
            NEXT()
        case '@':
        end:
            free_stack();
            return 0;
        case ' ':
        nop:
            NEXT()
#ifdef BEFUNGE93PLUS
        case 'c':
        cons : {
            struct value temp = pop();
            push(allocate(pop(), temp));
            print_heap();
            NEXT()
        }
        case 'h':
        head:
            push(head(pop()));
            NEXT()
        case 't':
        tail:
            push(tail(pop()));
            NEXT()
#endif /* BEFUNGE93PLUS */
        default:
        unsupported:
            fprintf(stderr, "Unsupported instruction '%c' (0x%02x) (maybe not Befunge-93?)\n", program[j * WIDTH + i], program[j * WIDTH + i]);
            NEXT()
        }
    }
}
