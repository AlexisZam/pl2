#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define PROGRAM_HEIGHT 25
#define PROGRAM_WIDTH 80
#define STACK_SIZE (1024 * 1024)
#define HEAP_SIZE (16 * 1024 * 1024)

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
        printf("Error: couldn't open '%s' for input.\n", pathname);
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

struct value {
    long value : 62;
    bool pointer : 1;
    bool marked : 1;
};
struct value stack[STACK_SIZE];
int top = 0;

void push(long l) {
    stack[top++] = (struct value){l, false, false};
}

long pop() {
    if (!top)
        return 0;
    return stack[--top].value;
}

void push_value(struct value v) {
    stack[top++] = v;
}

struct value pop_value() {
    if (!top)
        return (struct value){0, false, false};
    return stack[--top];
}

/* Heap */

struct cons_cell {
    struct value head;
    struct value tail;
};
struct cons_cell heap[HEAP_SIZE];
int freelist = 0;

void init_heap() {
    for (int i = 0; i < HEAP_SIZE; i++)
        heap[i].head.value = i + 1;
}

/* GC */

void dfs(struct value v) {
    if (v.pointer) {
        if (!heap[v.value].head.marked) {
            heap[v.value].head.marked = true;
            dfs(heap[v.value].head);
            dfs(heap[v.value].tail);
        }
    }
}

void gc() {
    /* mark */
    for (int i = 0; i < top; i++)
        dfs(stack[i]);
    /* sweep */
    for (int i = HEAP_SIZE - 1; i >= 0; i--) {
        if (heap[i].head.marked)
            heap[i].head.marked = false;
        else {
            heap[i].head.value = freelist;
            freelist = i;
        }
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("USAGE: vm foo.bf\n");
        exit(EXIT_FAILURE);
    }

    init_program();
    read_program(argv[1]);

    init_heap();

    srand(time(NULL));

    void *labels[] = {[0 ... 127] = &&nop};
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
    labels['c'] = &&cons;
    labels['h'] = &&head;
    labels['t'] = &&tail;

    void *program_as_labels[PROGRAM_HEIGHT][PROGRAM_WIDTH];
    for (int y = 0; y < PROGRAM_HEIGHT; y++)
        for (int x = 0; x < PROGRAM_WIDTH; x++)
            program_as_labels[y][x] = labels[program[y][x]];

    long l, l1, l2;
    struct value v, v1, v2;
    pc = *program_as_labels;
    goto **pc;
digit:
    push(program[position.y][position.x] - '0');
    move();
    goto **pc;
add:
    push(pop() + pop());
    move();
    goto **pc;
subtract:
    l = pop();
    push(pop() - l);
    move();
    goto **pc;
multiply:
    push(pop() * pop());
    move();
    goto **pc;
divide:
    l = pop();
    push(pop() / l);
    move();
    goto **pc;
modulo:
    l = pop();
    push(pop() % l);
    move();
    goto **pc;
negate:
    push(!pop());
    move();
    goto **pc;
greater:
    l = pop();
    push(pop() > l);
    move();
    goto **pc;
right:
    direction.x = 1;
    direction.y = 0;
    move();
    goto **pc;
left:
    direction.x = -1;
    direction.y = 0;
    move();
    goto **pc;
up:
    direction.x = 0;
    direction.y = -1;
    move();
    goto **pc;
down:
    direction.x = 0;
    direction.y = 1;
    move();
    goto **pc;
random:
    switch (rand() % 4) {
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
        if (program[position.y][position.x] == '"')
            break;
        push(program[position.y][position.x]);
    }
    move();
    goto **pc;
dup:
    v = pop_value();
    push_value(v);
    push_value(v);
    move();
    goto **pc;
swap:
    v1 = pop_value();
    v2 = pop_value();
    push_value(v1);
    push_value(v2);
    move();
    goto **pc;
pop:
    pop();
    move();
    goto **pc;
output_int:
    printf("%ld ", pop());
    fflush(stdout);
    move();
    goto **pc;
output_char:
    printf("%c", (char)pop());
    fflush(stdout);
    move();
    goto **pc;
bridge:
    move();
    move();
    goto **pc;
get:
    l1 = pop();
    l2 = pop();
    if (l1 < 0 || l1 >= PROGRAM_HEIGHT || l2 < 0 || l2 >= PROGRAM_WIDTH)
        push(0);
    else
        push(program[l1][l2]);
    move();
    goto **pc;
put:
    l1 = pop();
    l2 = pop();
    if (l1 < 0 || l1 >= PROGRAM_HEIGHT || l2 < 0 || l2 >= PROGRAM_WIDTH)
        pop();
    else {
        program[l1][l2] = pop();
        program_as_labels[l1][l2] = labels[program[l1][l2]];
    }
    move();
    goto **pc;
input_int:
    l = -1;
    scanf("%ld", &l);
    push(l);
    move();
    goto **pc;
input_char:
    push(getchar());
    move();
    goto **pc;
end:
    exit(EXIT_SUCCESS);
nop:
    move();
    goto **pc;
cons:
    if (freelist == HEAP_SIZE)
        gc();
    l = heap[freelist].head.value;
    v = pop_value();
    heap[freelist] = (struct cons_cell){pop_value(), v};
    push_value((struct value){freelist, true, false});
    freelist = l;
    move();
    goto **pc;
head:
    push_value(heap[pop()].head);
    move();
    goto **pc;
tail:
    push_value(heap[pop()].tail);
    move();
    goto **pc;
}
