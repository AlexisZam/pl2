#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#define HEIGHT 25
#define WIDTH 80
#define STACK_SIZE (1024 * 1024)
#define HEAP_SIZE (16 * 1024 * 1024)

#define NEXT() \
    next();    \
    goto **pc;

/* Program */

char program[HEIGHT][WIDTH];
struct {
    int x, y;
} position = {0, 0};
struct {
    int x, y;
} direction = {1, 0};
void **pc;

void next() {
    int x = position.x, y = position.y;
    position.x = (position.x + direction.x + WIDTH) % WIDTH;
    position.y = (position.y + direction.y + HEIGHT) % HEIGHT;
    pc += (position.y - y) * WIDTH + (position.x - x);
}

void init_program() {
    for (int y = 0; y < HEIGHT; y++)
        for (int x = 0; x < WIDTH; x++)
            program[y][x] = ' ';
}

void read_program(const char *filename) {
    FILE *stream = fopen(filename, "r");
    if (!stream) {
        printf("Error: couldn't open '%s' for input.\n", filename);
        exit(EXIT_FAILURE);
    }
    char *lineptr = NULL;
    size_t n = 0;
    for (int y = 0; y < HEIGHT; y++) {
        ssize_t cnt = getline(&lineptr, &n, stream);
        if (cnt == -1)
            break;
        if (lineptr[cnt - 1] == '\n')
            cnt--;
        if (cnt > WIDTH)
            cnt = WIDTH;
        memcpy(program[y], lineptr, cnt);
    }
    fclose(stream);
}

/* Stack */

struct value {
    long value : 62;
    bool pointer : 1;
    bool marked : 1;
};
struct value stack[STACK_SIZE];
int top = 0;

static inline void push(long l) {
    if (top == STACK_SIZE) {
        fprintf(stderr, "Stack overflow\n");
        exit(EXIT_FAILURE);
    }
    stack[top++] = (struct value){l, false, false};
}

static inline long pop() {
    if (!top)
        return 0;
    return stack[--top].value;
}

static inline void push_value(struct value value) {
    if (top == STACK_SIZE) {
        fprintf(stderr, "Stack overflow\n");
        exit(EXIT_FAILURE);
    }
    stack[top++] = value;
}

static inline struct value pop_value() {
    if (!top)
        return (struct value){0};
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

void DFS(struct value *x) { // TODO: improve DFS
    if (!x->marked) {
        x->marked = true;
        if (x->pointer) {
            DFS(&heap[x->value].head);
            DFS(&heap[x->value].tail);
        }
    }
}

void gc() {
    /* mark */
    for (int i = 0; i < top; i++)
        if (stack[i].pointer)
            DFS(&stack[i]);
    /* sweep */
    for (int i = HEAP_SIZE - 1; i >= 0; i--) {
        if (heap[i].head.marked)
            heap[i].head.marked = heap[i].tail.marked = false;
        else {
            heap[i].head.value = freelist;
            freelist = i;
        }
    }
}

int main(int argc, char *argv[]) {
    long l, l1, l2;
    struct value v, v1, v2;

    if (argc != 2) {
        printf("Usage: vm foo.bf\n");
        exit(EXIT_FAILURE);
    }

    init_program();
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
    labels['c'] = &&cons;
    labels['h'] = &&head;
    labels['t'] = &&tail;

    void *program_as_labels[HEIGHT * WIDTH];
    for (int j = 0; j < HEIGHT; j++)
        for (int i = 0; i < WIDTH; i++)
            program_as_labels[j * WIDTH + i] = labels[program[j][i]];
    pc = program_as_labels;

    srand(time(NULL));

    init_heap();

    goto **pc;
digit:
    push(program[position.y][position.x] - '0');
    NEXT()
add:
    push(pop() + pop());
    NEXT()
subtract:
    l = pop();
    push(pop() - l);
    NEXT()
multiply:
    push(pop() * pop());
    NEXT()
divide:
    l = pop();
    if (l == 0) {
        pop();
        printf("What do you want %ld/0 to be? ", l);
        scanf("%ld", &l);
        push(l);
    } else
        push(pop() / l);
    NEXT()
modulo:
    l = pop();
    if (l == 0) {
        pop();
        printf("What do you want %ld/0 to be? ", l);
        scanf("%ld", &l);
        push(l);
    } else
        push(pop() % l);
    NEXT()
negate:
    push(!pop());
    NEXT()
greater:
    l = pop();
    push(pop() > l);
    NEXT()
right:
    direction.x = 1;
    direction.y = 0;
    NEXT()
left:
    direction.x = -1;
    direction.y = 0;
    NEXT()
down:
    direction.x = 0;
    direction.y = 1;
    NEXT()
up:
    direction.x = 0;
    direction.y = -1;
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
        if (program[position.y][position.x] == '"')
            break;
        push(program[position.y][position.x]);
    }
    NEXT()
dup:
    v = pop_value();
    push_value(v);
    push_value(v);
    NEXT()
swap:
    v1 = pop_value();
    v2 = pop_value();
    push_value(v1);
    push_value(v2);
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
    l1 = pop();
    l2 = pop();
    if (l1 < 0 || l1 >= HEIGHT || l2 < 0 || l2 >= WIDTH) {
        fprintf(stderr, "g 'Get' instruction out of bounds (%ld,%ld)\n", l2, l1);
        push(0);
    } else
        push(program[l1][l2]);
    NEXT()
put:
    l1 = pop();
    l2 = pop();
    if (l1 < 0 || l1 >= HEIGHT || l2 < 0 || l2 >= WIDTH) {
        fprintf(stderr, "p 'Put' instruction out of bounds (%ld,%ld)\n", l2, l1);
        pop();
    } else {
        program[l1][l2] = pop();
        program_as_labels[l1 * WIDTH + l2] = labels[program[l1][l2]];
    }
    NEXT()
input_int:
    l = -1;
    scanf("%ld", &l);
    push(l);
    NEXT()
input_char:
    push(getchar());
    NEXT()
end:
    exit(EXIT_SUCCESS);
nop:
    NEXT()
cons:
    if (freelist == HEAP_SIZE) {
        gc();
        if (freelist == HEAP_SIZE) {
            fprintf(stderr, "Heap overflow\n");
            exit(EXIT_FAILURE);
        }
    }
    v = pop_value();
    heap[freelist] = (struct cons_cell){pop_value(), v};
    push_value((struct value){freelist, true, false});
    freelist = heap[freelist].head.value;
    NEXT()
head:
    push_value(heap[pop()].head);
    NEXT()
tail:
    push_value(heap[pop()].tail);
    NEXT()
unsupported:
    fprintf(stderr, "Unsupported instruction '%c' (0x%02x) (maybe not Befunge-93?)\n", program[position.y][position.x], program[position.y][position.x]);
    NEXT()
}
