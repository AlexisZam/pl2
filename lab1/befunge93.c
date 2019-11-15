#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define NEXT()        \
    next();           \
    printStack(head); \
    goto **pc;

/* Function prototypes */

void next();
void push();
long pop();
void printStack();

/* Globals */

int i = 0, j = 0;
int di = 1, dj = 0;
void **pc;

struct stack {
    long x;
    struct stack *next;
} *head = NULL;

/* Main program */

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: ./befunge93 foo.bf\n");
        return -1;
    }

#define HEIGHT 25
#define WIDTH 80
    char program[] = {[0 ... HEIGHT * WIDTH - 1] ' '};

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

    void *program_as_labels[HEIGHT * WIDTH];
    for (int j = 0; j < HEIGHT; j++)
        for (int i = 0; i < WIDTH; i++)
            program_as_labels[j * WIDTH + i] = labels[program[j * WIDTH + i]];
    pc = program_as_labels;
    j = 0;
    i = 0;

    srand(time(NULL));

    goto **pc;
digit:
    push(program[j * WIDTH + i] - '0');
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
divide : {
    long temp1 = pop();
    long temp2 = pop();
    if (temp1 == 0) {
        printf("What do you want %ld/0 to be? ", temp1);
        scanf("%ld", &temp1);
        push(temp1);
    } else
        push(temp2 / temp1);
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
        printStack(head);
        if (program[j * WIDTH + i] == '"')
            break;
        push(program[j * WIDTH + i]);
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
    fflush(stdout);
    NEXT()
output_char:
    printf("%c", (char)pop());
    fflush(stdout);
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
        push(program[temp1 * WIDTH + temp2]);
    NEXT()
}
put : {
    long temp1 = pop();
    long temp2 = pop();
    if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
        fprintf(stderr, "p 'Put' instruction out of bounds (%ld,%ld)\n", temp2, temp1);
        pop();
    } else {
        program[temp1 * WIDTH + temp2] = pop();
        program_as_labels[temp1 * WIDTH + temp2] = labels[program[temp1 * WIDTH + temp2]];
    }
    NEXT()
}
input_int : {
    long temp = -1;
    scanf("%ld", &temp);
    push(temp);
    NEXT()
}
input_char:
    push(getchar());
    NEXT()
end:
    while (head)
        pop();
    return 0;
nop:
    NEXT()
unsupported:
    fprintf(stderr, "Unsupported instruction '%c' (0x%02x) (maybe not Befunge-93?)\n", program[j * WIDTH + i], program[j * WIDTH + i]);
    NEXT()
}

/* Program */

void next() {
    int temp1 = j, temp2 = i;
    i = (i + di + WIDTH) % WIDTH;
    j = (j + dj + HEIGHT) % HEIGHT;
    pc += (j - temp1) * WIDTH + (i - temp2);
}

/* Stack */

//TODO: top of stack caching
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

void printStack() {
#ifdef STACK
#define STACK_FD 3
    for (struct stack *t = head; t; t = t->next)
        dprintf(STACK_FD, "%ld ", t->x);
    dprintf(STACK_FD, "\n");
#endif /* STACK */
}