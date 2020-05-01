#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#define DIRECT_THREADING
// #define DYNAMIC_STACK
// #define PRINT_STACK

#define STACK_SIZE (1024 * 1024)
#define INIT_STACK_SIZE 1024
#define STACK_FD 3
#define HEAP_SIZE (16 * 1024 * 1024)

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

int top = 0;
#ifdef DYNAMIC_STACK
int stack_size = 0;
long *stack = NULL;
#else
long stack[STACK_SIZE];
#endif /* DYNAMIC_STACK */

void init_stack() {
#ifdef DYNAMIC_STACK
    stack_size = INIT_STACK_SIZE;
    stack = malloc(stack_size * sizeof);
#endif /* DYNAMIC_STACK */
}

static inline void push(long value) {
#ifdef DYNAMIC_STACK
    if (top == stack_size) {
        stack_size *= 2;
        stack = realloc(stack, stack_size * sizeof);
    }
#else
    if (top == STACK_SIZE) {
        fprintf(stderr, "Stack overflow\n");
        exit(EXIT_FAILURE);
    }
#endif /* DYNAMIC_STACK */
    stack[top++] = value;
}

static inline long pop() {
    if (!top)
        return 0;
    return stack[--top];
}

void print_stack() {
#ifdef PRINT_STACK
    for (int i = top - 1; i >= 0; i--)
        dprintf(STACK_FD, "%ld ", stack[i]);
    dprintf(STACK_FD, "\n");
#endif /* PRINT_STACK */
}

int i = 0, j = 0;
int di = 1, dj = 0;
#ifdef DIRECT_THREADING
void **pc;
#endif /* DIRECT_THREADING */

#define HEIGHT 25
#define WIDTH 80
char program[] = {[0 ... HEIGHT * WIDTH - 1] = ' '};

void next() {
#ifdef DIRECT_THREADING
    int temp1 = j, temp2 = i;
#endif /* DIRECT_THREADING */
    i = (i + di + WIDTH) % WIDTH;
    j = (j + dj + HEIGHT) % HEIGHT;
#ifdef DIRECT_THREADING
    pc += (j - temp1) * WIDTH + (i - temp2);
#endif /* DIRECT_THREADING */
}

void read_program(const char *filename) {
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
    long temp, temp1, temp2;

    if (argc != 2) {
        fprintf(stderr, "Usage: vm foo.bf\n");
        exit(EXIT_FAILURE);
    }

    read_program(argv[1]);

#define ASCII 128
    void *labels[] = {[0 ... ASCII - 1] = &&unsupported};
    for (char c = '0'; c <= '9'; c++)
        labels[c] = &&digit;
    labels['+'] = &&add;
    labels['-'] = &&subtract;
    labels['*'] = &&multiply;
    labels['/'] = &&divide;
    labels['%'] = &&modulo;
    labels['!'] = &&negate;
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

    goto **pc;
digit:
    push(program[j * WIDTH + i] - '0');
    NEXT()
add:
    push(pop() + pop());
    NEXT()
subtract:
    temp = pop();
    push(pop() - temp);
    NEXT()
multiply:
    push(pop() * pop());
    NEXT()
divide:
    temp = pop();
    if (temp == 0) {
        pop();
        printf("What do you want %ld/0 to be? ", temp);
        scanf("%ld", &temp);
        push(temp);
    } else
        push(pop() / temp);
    NEXT()
modulo:
    temp = pop();
    if (temp == 0) {
        pop();
        printf("What do you want %ld/0 to be? ", temp);
        scanf("%ld", &temp);
        push(temp);
    } else
        push(pop() % temp);
    NEXT()
negate:
    push(!pop());
    NEXT()
greater:
    temp = pop();
    push(pop() > temp);
    NEXT()
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
    if (pop())
        goto left;
    goto right;
vertical_if:
    if (pop())
        goto up;
    goto down;
stringmode:
    for (;;) {
        next();
        print_stack();
        if (program[j * WIDTH + i] == '"')
            break;
        push(program[j * WIDTH + i]);
    }
    NEXT()
dup:
    temp = pop();
    push(temp);
    push(temp);
    NEXT()
swap:
    temp1 = pop();
    temp2 = pop();
    push(temp1);
    push(temp2);
    NEXT()
pop:
    pop();
    NEXT()
output_int:
    printf("%ld ", pop());
    fflush(stdout);
    NEXT()
output_char:
    printf("%c", (char)pop());
    fflush(stdout);
    NEXT()
bridge:
    next();
    NEXT()
get:
    temp1 = pop();
    temp2 = pop();
    if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
        fprintf(stderr, "g 'Get' instruction out of bounds (%ld,%ld)\n", temp2, temp1);
        push(0);
    } else
        push(program[temp1 * WIDTH + temp2]);
    NEXT()
put:
    temp1 = pop();
    temp2 = pop();
    if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
        fprintf(stderr, "p 'Put' instruction out of bounds (%ld,%ld)\n", temp2, temp1);
        pop();
    } else {
        program[temp1 * WIDTH + temp2] = pop();
#ifdef DIRECT_THREADING
        program_as_labels[temp1 * WIDTH + temp2] = labels[program[temp1 * WIDTH + temp2]];
#endif /* DIRECT_THREADING */
    }
    NEXT()
input_int:
    temp = -1;
    scanf("%ld", &temp);
    push(temp);
    NEXT()
input_char:
    push(getchar());
    NEXT()
end:
    exit(EXIT_SUCCESS);
nop:
    NEXT()
unsupported:
    fprintf(stderr, "Unsupported instruction '%c' (0x%02x) (maybe not Befunge-93?)\n", program[j * WIDTH + i], program[j * WIDTH + i]);
    NEXT()
}