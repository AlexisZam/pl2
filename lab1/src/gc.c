#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#define DIRECT_THREADING

#define HEIGHT 25
#define WIDTH 80
#define STACK_SIZE (1024 * 1024)
#define HEAP_SIZE (16 * 1024 * 1024)

#ifdef DIRECT_THREADING
#define NEXT() \
    next();    \
    goto **pc;
#else
#define NEXT() \
    next();    \
    goto *labels[program[j * WIDTH + i]];
#endif /* DIRECT_STHREADING */

/* Program */

int i = 0, j = 0;
int di = 1, dj = 0;
#ifdef DIRECT_THREADING
void **pc;
#endif /* DIRECT_THREADING */

char program[] = {[0 ... HEIGHT * WIDTH - 1] = ' '};

void next() {
#ifdef DIRECT_THREADING
    int l1 = j, l2 = i;
#endif /* DIRECT_THREADING */
    i = (i + di + WIDTH) % WIDTH;
    j = (j + dj + HEIGHT) % HEIGHT;
#ifdef DIRECT_THREADING
    pc += (j - l1) * WIDTH + (i - l2);
#endif /* DIRECT_THREADING */
}

void read_program(const char *filename) {
    FILE *stream = fopen(filename, "r");
    if (!stream) {
        printf("Error: couldn't open '%s' for input.\n", filename);
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

void mark() {
    for (int i = 0; i < top; i++)
        if (stack[i].pointer)
            DFS(&stack[i]);
}

void sweep() {
    for (int i = HEAP_SIZE - 1; i >= 0; i--) {
        if (heap[i].head.marked)
            heap[i].head.marked = heap[i].tail.marked = false;
        else {
            heap[i].head.value = freelist;
            freelist = i;
        }
    }
}

void gc() {
    mark();
    sweep();
}

// TODO: copying

int main(int argc, char *argv[]) {
    long l, l1, l2;
    struct value v, v1, v2;

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
    labels['c'] = &&cons;
    labels['h'] = &&head;
    labels['t'] = &&tail;

#ifdef DIRECT_THREADING
    void *program_as_labels[HEIGHT * WIDTH];
    for (int j = 0; j < HEIGHT; j++)
        for (int i = 0; i < WIDTH; i++)
            program_as_labels[j * WIDTH + i] = labels[program[j * WIDTH + i]];
    pc = program_as_labels;
#endif /* DIRECT_THREADING */

    i = j = 0;

    srand(time(NULL));

    init_heap();

    goto **pc;
digit:
    push(program[j * WIDTH + i] - '0');
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
        if (program[j * WIDTH + i] == '"')
            break;
        push(program[j * WIDTH + i]);
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
        push(program[l1 * WIDTH + l2]);
    NEXT()
put:
    l1 = pop();
    l2 = pop();
    if (l1 < 0 || l1 >= HEIGHT || l2 < 0 || l2 >= WIDTH) {
        fprintf(stderr, "p 'Put' instruction out of bounds (%ld,%ld)\n", l2, l1);
        pop();
    } else {
        program[l1 * WIDTH + l2] = pop();
#ifdef DIRECT_THREADING
        program_as_labels[l1 * WIDTH + l2] = labels[program[l1 * WIDTH + l2]];
#endif /* DIRECT_THREADING */
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
    fprintf(stderr, "Unsupported instruction '%c' (0x%02x) (maybe not Befunge-93?)\n", program[j * WIDTH + i], program[j * WIDTH + i]);
    NEXT()
}