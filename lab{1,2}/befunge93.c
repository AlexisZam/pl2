#ifdef BEFUNGE93PLUS
#include "heap.h"
#endif
#include "stack.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifdef DIRECT_THREADING
#define NEXT()     \
    next();        \
    print_stack(); \
    goto **pc;
#elif defined INDIRECT_THREADING
#define NEXT()     \
    next();        \
    print_stack(); \
    goto *labels[program[j * WIDTH + i]];
#elif defined NO_THREADING
#define NEXT()     \
    next();        \
    print_stack(); \
    break;
#endif

/* Function prototypes */

void next();

/* Globals */

int i = 0, j = 0;
int di = 1, dj = 0;
void **pc;

/* Main program */

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: ./befunge93 foo.bf\n");
        return -1;
    }

#define HEIGHT 25
#define WIDTH 80
    char program[] = {[0 ... HEIGHT * WIDTH - 1] = ' '};

    FILE *fp = fopen(argv[1], "r");
    if (!fp) {
        fprintf(stderr, "Error: couldn't open '%s' for input.\n", argv[1]);
        return -1;
    }
    for (;;) {
        int c = fgetc(fp);
        if (feof(fp))
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
                    c = fgetc(fp);
                    if (feof(fp))
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
    fclose(fp);

#define ASCII 128
#if defined DIRECT_THREADING || defined INDIRECT_THREADING
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
#endif
#endif

#ifdef DIRECT_THREADING
    void *program_as_labels[HEIGHT * WIDTH];
    for (int j = 0; j < HEIGHT; j++)
        for (int i = 0; i < WIDTH; i++)
            program_as_labels[j * WIDTH + i] = labels[program[j * WIDTH + i]];
    pc = program_as_labels;
#endif

    j = 0;
    i = 0;

    srand(time(NULL));

    init_stack();

    for (;;) {
        // goto **pc;
        switch (program[j * WIDTH + i]) {
        case '0' ... '9':
        digit:
            push_value(program[j * WIDTH + i] - '0');
            NEXT()
        case '+':
        add:
            push_value(pop_value() + pop_value());
            NEXT()
        case '-':
        subtract : {
            long temp = pop_value();
            push_value(pop_value() - temp);
            NEXT()
        }
        case '*':
        multiply:
            push_value(pop_value() * pop_value());
            NEXT()
        case '/':
        divide : {
            long temp1 = pop_value();
            long temp2 = pop_value();
            if (temp1 == 0) {
                printf("What do you want %ld/0 to be? ", temp1);
                scanf("%ld", &temp1);
                push_value(temp1);
            } else
                push_value(temp2 / temp1);
            NEXT()
        }
        case '%':
        modulo : { // TODO: division by 0
            long temp = pop_value();
            push_value(pop_value() % temp);
            NEXT()
        }
        case '!':
            not : push_value(!pop_value());
            NEXT()
        case '`':
        greater : {
            long temp = pop_value();
            push_value(pop_value() > temp);
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
            if (pop_value())
                goto left;
            goto right;
        case '|':
        vertical_if:
            if (pop_value())
                goto up;
            goto down;
        case '"':
        stringmode:
            for (;;) {
                next();
                print_stack();
                if (program[j * WIDTH + i] == '"')
                    break;
                push_value(program[j * WIDTH + i]);
            }
            NEXT()
        case ':':
        dup : {
            long temp = pop_value();
            push_value(temp);
            push_value(temp);
            NEXT()
        }
        case '\\':
        swap : {
            long temp1 = pop_value();
            long temp2 = pop_value();
            push_value(temp1);
            push_value(temp2);
            NEXT()
        }
        case '$':
        pop:
            pop_value();
            NEXT()
        case '.':
        output_int:
            printf("%ld ", pop_value());
            fflush(stdout);
            NEXT()
        case ',':
        output_char:
            printf("%c", (char)pop_value());
            fflush(stdout);
            NEXT()
        case '#':
        bridge:
            next();
            NEXT()
        case 'g':
        get : {
            long temp1 = pop_value();
            long temp2 = pop_value();
            if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
                fprintf(stderr, "g 'Get' instruction out of bounds (%ld,%ld)\n", temp2, temp1);
                push_value(0);
            } else
                push_value(program[temp1 * WIDTH + temp2]);
            NEXT()
        }
        case 'p':
        put : {
            long temp1 = pop_value();
            long temp2 = pop_value();
            if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
                fprintf(stderr, "p 'Put' instruction out of bounds (%ld,%ld)\n", temp2, temp1);
                pop_value();
            } else {
                program[temp1 * WIDTH + temp2] = pop_value();
#ifdef DIRECT_THREADING
                program_as_labels[temp1 * WIDTH + temp2] = labels[program[temp1 * WIDTH + temp2]];
#endif
            }
            NEXT()
        }
        case '&':
        input_int : {
            long temp = -1;
            scanf("%ld", &temp);
            push_value(temp);
            NEXT()
        }
        case '~':
        input_char:
            push_value(getchar());
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
            long temp = pop_value();
            push_heap_address(allocate(pop_value(), temp));
            NEXT()
        }
        case 'h':
        head:
            push_heap_address(head(pop_heap_address()).value);
            NEXT()
        case 't':
        tail:
            push_heap_address(tail(pop_heap_address()).value);
            NEXT()
#endif
        default:
        unsupported:
            fprintf(stderr, "Unsupported instruction '%c' (0x%02x) (maybe not Befunge-93?)\n", program[j * WIDTH + i], program[j * WIDTH + i]);
            NEXT()
        }
    }
}

/* Program */

void next() {
    int temp1 = j, temp2 = i;
    i = (i + di + WIDTH) % WIDTH;
    j = (j + dj + HEIGHT) % HEIGHT;
    pc += (j - temp1) * WIDTH + (i - temp2);
}
