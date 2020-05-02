#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

// FIXME
#define PRINT_STACK

#define PROGRAM_HEIGHT 25
#define PROGRAM_WIDTH 80
#define STACK_SIZE (1024 * 1024)

// FIXME
#define MOVE()     \
    move();        \
    print_stack(); \
    goto **pc

/* Program */

char program[PROGRAM_HEIGHT][PROGRAM_WIDTH];
struct {
    int x, y;
} position = {0, 0};
struct {
    int x, y;
} direction = {1, 0};
void **pc;

void init_program() {
    memset(program, ' ', PROGRAM_HEIGHT * PROGRAM_WIDTH);
}

void read_program(const char *pathname) {
    FILE *stream = fopen(pathname, "r");
    if (!stream) {
        printf("Error: couldn't open '%s' for input.\n", pathname); // FIXME
        exit(EXIT_FAILURE);
    }
    char *lineptr = NULL;
    size_t n = 0;
    for (int y = 0; y < PROGRAM_HEIGHT; y++) {
        ssize_t ssize = getline(&lineptr, &n, stream);
        if (ssize == -1)
            break;
        if (lineptr[ssize - 1] == '\n')
            ssize--;
        if (ssize > PROGRAM_WIDTH)
            ssize = PROGRAM_WIDTH;
        memcpy(program[y], lineptr, ssize);
    }
    free(lineptr);
    fclose(stream);
}

void move() {
    int x = position.x, y = position.y;
    position.x = (position.x + direction.x + PROGRAM_WIDTH) % PROGRAM_WIDTH;
    position.y = (position.y + direction.y + PROGRAM_HEIGHT) % PROGRAM_HEIGHT;
    pc += (position.y - y) * PROGRAM_WIDTH + (position.x - x);
}

/* Stack */

long stack[STACK_SIZE];
int top = 0;

void push(long l) {
    stack[top++] = l;
}

long pop() {
    if (!top)
        return 0;
    return stack[--top];
}

void print_stack() { // FIXME
#ifdef PRINT_STACK
    for (int i = top - 1; i >= 0; i--)
        dprintf(3, "%ld ", stack[i]);
    dprintf(3, "\n");
#endif /* PRINT_STACK */
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("USAGE: vm foo.bf\n");
        exit(EXIT_FAILURE);
    }

    init_program();
    read_program(argv[1]);

    srand(42); // FIXME

    void *labels[] = {[0 ... 127] = &&unsupported};
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
    labels['^'] = &&up;
    labels['v'] = &&down;
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

    void *program_as_labels[PROGRAM_HEIGHT][PROGRAM_WIDTH];
    for (int y = 0; y < PROGRAM_HEIGHT; y++)
        for (int x = 0; x < PROGRAM_WIDTH; x++)
            program_as_labels[y][x] = labels[program[y][x]];

    long l, l1, l2;
    pc = *program_as_labels;
    goto **pc;
digit:
    push(program[position.y][position.x] - '0');
    MOVE();
add:
    push(pop() + pop());
    MOVE();
subtract:
    l = pop();
    push(pop() - l);
    MOVE();
multiply:
    push(pop() * pop());
    MOVE();
divide:
    l = pop();
    push(pop() / l);
    MOVE();
modulo:
    l = pop();
    push(pop() % l);
    MOVE();
negate:
    push(!pop());
    MOVE();
greater:
    l = pop();
    push(pop() > l);
    MOVE();
right:
    direction.x = 1;
    direction.y = 0;
    MOVE();
left:
    direction.x = -1;
    direction.y = 0;
    MOVE();
up:
    direction.x = 0;
    direction.y = -1;
    MOVE();
down:
    direction.x = 0;
    direction.y = 1;
    MOVE();
random:
    switch ((rand() / 32) % 4) { // FIXME
    case 0:
        goto right;
    case 1:
        goto left;
    case 2:
        goto up;
    case 3:
        goto down;
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
        move();
        print_stack(); // FIXME
        if (program[position.y][position.x] == '"')
            break;
        push(program[position.y][position.x]);
    }
    MOVE();
dup:
    l = pop();
    push(l);
    push(l);
    MOVE();
swap:
    l1 = pop();
    l2 = pop();
    push(l1);
    push(l2);
    MOVE();
pop:
    pop();
    MOVE();
output_int:
    printf("%ld ", pop());
    fflush(stdout);
    MOVE();
output_char:
    printf("%c", (char)pop());
    fflush(stdout);
    MOVE();
bridge:
    move();
    MOVE();
get:
    l1 = pop();
    l2 = pop();
    if (l1 < 0 || l1 >= PROGRAM_HEIGHT || l2 < 0 || l2 >= PROGRAM_WIDTH) {
        fprintf(stderr, "g 'Get' instruction out of bounds (%ld,%ld)\n", l2, l1); // FIXME
        push(0);
    } else
        push(program[l1][l2]);
    MOVE();
put:
    l1 = pop();
    l2 = pop();
    if (l1 < 0 || l1 >= PROGRAM_HEIGHT || l2 < 0 || l2 >= PROGRAM_WIDTH) {
        fprintf(stderr, "p 'Put' instruction out of bounds (%ld,%ld)\n", l2, l1); // FIXME
        pop();
    } else {
        program[l1][l2] = pop();
        program_as_labels[l1][l2] = labels[program[l1][l2]];
    }
    MOVE();
input_int:
    l = -1;
    scanf("%ld", &l);
    push(l);
    MOVE();
input_char:
    push(getchar());
    MOVE();
end:
    exit(EXIT_SUCCESS);
nop:
    MOVE();
unsupported: // FIXME
    fprintf(stderr, "Unsupported instruction '%c' (0x%02x) (maybe not Befunge-93?)\n", program[position.y][position.x], program[position.y][position.x]);
    MOVE();
}
