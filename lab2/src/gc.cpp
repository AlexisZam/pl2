#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <vector>

#define NEXT()     \
    next();        \
    print_stack(); \
    goto **pc;

/* Types */

typedef int64_t long_t;
struct Value {
    long_t value : 62;
    bool pointer : 1;
    bool marked : 1;

    Value() {}
    explicit Value(long_t v) : value(v), pointer(0), marked(0) {}
    Value(long_t v, bool p, bool m) : value(v), pointer(p), marked(m) {}
};

/* Stack */

static void DFS(Value *);

static std::vector<Value> stack;

static void init_stack() {
    stack.reserve(1024 * 1024);
}

static void push(Value value) {
    stack.push_back(value);
}

static Value pop() {
    if (stack.empty())
        return Value(0);
    Value value = stack.back(); // FIXME reference
    stack.pop_back();
    return value;
}

static void print_stack() {
    for (int i = stack.size() - 1; i >= 0; i--) {
        if (stack[i].marked)
            dprintf(3, "\033[1;31m");
        dprintf(3, "%ld:%d ",
                stack[i].value, stack[i].pointer);
        dprintf(3, "\033[0m");
    }
    dprintf(3, "\n");
}

// TODO: stack used as adresses are implicitly cast, and conversely
// TODO: should we type check?

void mark() {
    for (std::size_t i = 0; i < stack.size(); i++)
        if (stack[i].pointer)
            DFS(&stack[i]);
    // print_heap();
    // print_stack();
}

/* Heap */

#define HEAP_SIZE (16 * 1024 * 1024)

struct ConsCell {
    Value head;
    Value tail;

    ConsCell() {}
    ConsCell(Value h, Value t) : head(h), tail(t) {}
};

static std::size_t freelist = 0;
static ConsCell heap[HEAP_SIZE];

static void DFS(Value *x) { // TODO: improve DFS
    if (!x->marked) {
        x->marked = true;
        if (x->pointer) {
            DFS(&heap[x->value].head);
            DFS(&heap[x->value].tail);
        }
    }
}

static void sweep() {
    for (int i = HEAP_SIZE - 1; i >= 0; i--) {
        if (heap[i].head.marked)
            heap[i].head.marked = heap[i].tail.marked = false;
        else {
            heap[i].head.value = freelist;
            freelist = i;
        }
    }
}

static void gc() {
    mark();
    sweep();
}

static void init_heap() {
    for (std::size_t i = 0; i < HEAP_SIZE; i++)
        heap[i].head.value = i + 1;
}

static Value allocate(Value head, Value tail) {
    if (freelist == HEAP_SIZE) {
        push(head);
        push(tail);
        gc();
        pop();
        pop();
        if (freelist == HEAP_SIZE) {
            std::cerr << "Heap overflow\n";
            exit(EXIT_FAILURE);
        }
    }
    std::size_t temp = freelist;
    freelist = heap[freelist].head.value;
    heap[temp] = ConsCell(head, tail);
    return Value(temp, true, false);
}

static Value head(Value address) {
    return heap[address.value].head;
}

static Value tail(Value address) {
    return heap[address.value].tail;
}

static void print_heap() {
    std::size_t temp = freelist;
    dprintf(4, "(((%d))) ", temp);
    for (std::size_t i = 0; i < HEAP_SIZE; i++)
        if (i == temp) {
            dprintf(4, "(((%d))) ", temp);
            temp = heap[temp].head.value;
        } else {
            if (heap[i].head.marked || heap[i].tail.marked)
                dprintf(4, "\033[1;31m");
            dprintf(4, "(%ld:%d,%ld:%d) ",
                    heap[i].head.value, heap[i].head.pointer, heap[i].tail.value, heap[i].tail.pointer);
            dprintf(4, "\033[0m");
        }
    dprintf(4, "\n");
}

// TODO: copying

/* Global variables */

static std::size_t i = 0, j = 0;
static int di = 1, dj = 0;
static void **pc;

#define HEIGHT 25
#define WIDTH 80
static char program[HEIGHT * WIDTH] = {' '};

static void next() {
    std::size_t temp1 = j, temp2 = i;
    i = (i + di + WIDTH) % WIDTH;
    j = (j + dj + HEIGHT) % HEIGHT;
    pc += (j - temp1) * WIDTH + (i - temp2);
}

static void parse_program(const char *filename) {
    FILE *stream = fopen(filename, "r");
    if (!stream) {
        std::cerr << "Error: couldn't open '" << filename << "' for input.\n";
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
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " foo.bf\n";
        exit(EXIT_FAILURE);
    }

    parse_program(argv[1]);

    void *labels[128] = {&&unsupported};
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
    labels['~'] = &&input_character;
    labels['@'] = &&end;
    labels[' '] = &&nop;
    labels['c'] = &&cons;
    labels['h'] = &&head;
    labels['t'] = &&tail;

    void *program_as_labels[HEIGHT * WIDTH];
    for (std::size_t j = 0; j < HEIGHT; j++)
        for (std::size_t i = 0; i < WIDTH; i++)
            program_as_labels[j * WIDTH + i] = labels[program[j * WIDTH + i]];
    pc = program_as_labels;

    i = j = 0;

    srand(time(NULL));

    init_stack();
    init_heap();

    goto **pc;
digit:
    push(Value(program[j * WIDTH + i] - '0'));
    NEXT()
add:
    push(Value(pop().value + pop().value));
    NEXT()
subtract : {
    long_t temp = pop().value;
    push(Value(pop().value - temp));
    NEXT()
}
multiply:
    push(Value(pop().value * pop().value));
    NEXT()
divide : {
    long_t temp = pop().value;
    if (temp == 0) {
        pop();
        std::cout << "What do you want " << temp << "/0 to be? ";
        scanf("%ld", &temp);
        push(Value(temp));
    } else
        push(Value(pop().value / temp));
    NEXT()
}
modulo : {
    long_t temp = pop().value;
    if (temp == 0) {
        pop();
        std::cout << "What do you want " << temp << "/0 to be? ";
        scanf("%ld", &temp);
        push(Value(temp));
    } else
        push(Value(pop().value % temp));
    NEXT()
}
negate:
    push(Value(!pop().value));
    NEXT()
greater : {
    long_t temp = pop().value;
    push(Value(pop().value > temp));
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
up:
    di = 0;
    dj = -1;
    NEXT()
down:
    di = 0;
    dj = 1;
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
        push(Value(program[j * WIDTH + i]));
    }
    NEXT()
dup : {
    Value temp = pop();
    push(temp);
    push(temp);
    NEXT()
}
swap : {
    Value temp1 = pop();
    Value temp2 = pop();
    push(temp1);
    push(temp2);
    NEXT()
}
pop:
    pop();
    NEXT()
output_int:
    std::cout << pop().value << std::flush;
    NEXT()
output_char:
    std::cout << char(pop().value) << std::flush;
    NEXT()
bridge:
    next();
    NEXT()
get : {
    long_t temp1 = pop().value;
    long_t temp2 = pop().value;
    if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
        std::cerr << "g 'Get' instruction out of bounds (" << temp2 << "," << temp1 << ")\n";
        push(Value(0));
    } else
        push(Value(program[temp1 * WIDTH + temp2]));
    NEXT()
}
put : {
    long_t temp1 = pop().value;
    long_t temp2 = pop().value;
    if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
        std::cerr << "p 'Put' instruction out of bounds (" << temp2 << "," << temp1 << ")\n";
        pop();
    } else {
        program[temp1 * WIDTH + temp2] = char(pop().value);
        program_as_labels[temp1 * WIDTH + temp2] = labels[program[temp1 * WIDTH + temp2]];
    }
    NEXT()
}
input_int : {
    long_t temp = -1;
    scanf("%ld", &temp);
    push(Value(temp));
    NEXT()
}
input_character:
    push(Value(getchar()));
    NEXT()
end:
    exit(0);
nop:
    NEXT()
cons : {
    Value temp = pop();
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
unsupported:
    fprintf(stderr, "Unsupported instruction '%c' (0x%02x) (maybe not Befunge-93?)\n", program[j * WIDTH + i], program[j * WIDTH + i]);
    NEXT()
}
