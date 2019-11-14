#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define NEXT()        \
    next();           \
    printStack(head); \
    goto *program_as_labels[i][j];

/* Function prototypes */

void initProgram();
void readProgram(char *filename);
void initStack();
void run();

/* Main program */

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: ./befunge93 foo.bf\n");
        return -1;
    }

    initProgram();
    readProgram(argv[1]);

    initStack();

    srand(time(NULL));

    run();
}

/* Stack */

struct stack {
    long x;
    struct stack *next;
} * head;

void initStack() {
    head = NULL;
}

void push(long x) {
    struct stack *s = malloc(sizeof(struct stack));
    s->x = x;
    s->next = head;
    head = s;
}

long pop() {
    if (!head)
        return 0;
    struct stack *s = head;
    long x = head->x;
    head = s->next;
    free(s);
    return x;
}

#define STACK_FD 3
void printStack() {
    for (struct stack *t = head; t; t = t->next)
        dprintf(STACK_FD, "%ld ", t->x);
    dprintf(STACK_FD, "\n");
}

/* Program */

#define HEIGHT 25
#define WIDTH 80
char program[HEIGHT][WIDTH];

void initProgram() {
    for (int i = 0; i < HEIGHT; i++)
        for (int j = 0; j < WIDTH; j++)
            program[i][j] = ' ';
}

void readProgram(char *filename) {
    // TODO: input too large
    FILE *fp = fopen(filename, "r");
    for (int i = 0; i < HEIGHT; i++)
        for (int j = 0; j < WIDTH; j++) {
            int c = fgetc(fp);
            if (c == '\n')
                break;
            if (c == EOF) {
                fclose(fp);
                return;
            }
            program[i][j] = c;
        }
    fclose(fp);
}

void printProgram() {
    for (int i = 0; i < HEIGHT; i++) {
        for (int j = 0; j < WIDTH; j++)
            fprintf(stderr, "%c", program[i][j]);
        fprintf(stderr, "\n");
    }
}

int i = 0, j = 0;
enum direction { right,
                 left,
                 down,
                 up } dir = 0;

void next() {
    switch (dir) {
    case 0:
        j = (j + 1) % WIDTH;
        break;
    case 1:
        j = (j - 1 + WIDTH) % WIDTH;
        break;
    case 2:
        i = (i + 1) % HEIGHT;
        break;
    case 3:
        i = (i - 1 + HEIGHT) % HEIGHT;
    }
}

/* VM */

void run() {
    //TODO: top of stack caching
    //TODO: direct threading

#define ASCII 128
    static void *labels[ASCII];
    for (int i = 0; i < ASCII; i++)
        labels[i] = &&unsupported;
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

    void *program_as_labels[HEIGHT][WIDTH];
    for (int i = 0; i < HEIGHT; i++)
        for (int j = 0; j < WIDTH; j++)
            program_as_labels[i][j] = labels[program[i][j]];

    goto *program_as_labels[i][j];
digit:
    push(program[i][j] - '0');
    NEXT()
add:
    push(pop() + pop());
    NEXT()
subtract : {
    long temp = pop();
    push(pop() - temp);
    NEXT()
}
multiply:
    push(pop() * pop());
    NEXT()
divide : { // TODO: division by 0
    long temp = pop();
    push(pop() / temp);
    NEXT()
}
modulo : { // TODO: division by 0
    long temp = pop();
    push(pop() % temp);
    NEXT()
}
    not : push(!pop());
    NEXT()
greater : {
    long temp = pop();
    push(pop() > temp);
    NEXT()
}
right:
    dir = right;
    NEXT()
left:
    dir = left;
    NEXT()
down:
    dir = down;
    NEXT()
up:
    dir = up;
    NEXT()
random:
    dir = rand() % 4;
    NEXT()
horizontal_if:
    dir = pop() ? left : right;
    NEXT()
vertical_if:
    dir = pop() ? up : down;
    NEXT()
stringmode: // move printStack
    for (;;) {
        next();
        printStack(head);
        if (program[i][j] == '"')
            break;
        push(program[i][j]);
    }
    NEXT()
dup : {
    long temp = pop(head);
    push(temp);
    push(temp);
    NEXT()
}
swap : {
    long temp1 = pop();
    long temp2 = pop();
    push(temp1);
    push(temp2);
    NEXT()
}
pop:
    pop();
    NEXT()
output_int:
    printf("%ld ", pop());
    NEXT()
output_char:
    printf("%c", (char)pop());
    NEXT()
bridge:
    next();
    NEXT()
get : {
    long temp1 = pop();
    long temp2 = pop();
    if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
        fprintf(stderr, "g 'Get' instruction out of bounds (%ld,%ld)\n", temp2, temp1);
        push(0);
    } else
        push(program[temp1][temp2]);
    NEXT()
}
put : {
    long temp1 = pop();
    long temp2 = pop();
    if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
        fprintf(stderr, "p 'Put' instruction out of bounds (%ld,%ld)\n", temp2, temp1);
        pop();
    } else {
        program[temp1][temp2] = pop();
        program_as_labels[temp1][temp2] = labels[program[temp1][temp2]];
    }
    NEXT()
}
input_int : {
    long temp;
    if (scanf("%ld", &temp) != 1) {
        fprintf(stderr, "Read failed");
        exit(-1);
    }
    push(temp);
    NEXT()
}
input_char:
    push(getchar());
    NEXT()
end:
    exit(0);
nop:
    NEXT()
unsupported:
    fprintf(stderr, "Unsupported instruction '%c' (0x%x) (maybe not Befunge-93?)\n", program[i][j], program[i][j]);
    NEXT()
}