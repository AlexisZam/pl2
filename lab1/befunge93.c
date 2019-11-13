#include <stdio.h>
#include <stdlib.h>
#include <time.h>

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
    // todo: input too large
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
int direction = 0;

void next() {
    switch (direction) {
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

void printStack() {
    for (struct stack *t = head; t; t = t->next)
        fprintf(stderr, "%ld ", t->x);
    fprintf(stderr, "\n");
}

/* VM */

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: ./befunge93 program\n");
        return -1;
    }

    initProgram();
    readProgram(argv[1]);
    initStack();
    srand(time(NULL));

    for (;;) {
        char command = program[i][j];
        switch (command) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            push(command - '0');
            break;
        case '+':
            push(pop() + pop());
            break;
        case '-': {
            long temp = pop();
            push(pop() - temp);
            break;
        }
        case '*':
            push(pop() * pop());
            break;
        case '/': {
            // todo: division by 0
            long temp = pop();
            push(pop() / temp);
            break;
        }
        case '%': {
            // todo: division by 0
            long temp = pop();
            push(pop() % temp);
            break;
        }
        case '!':
            push(!pop());
            break;
        case '`': {
            long temp = pop();
            push(pop() > temp);
            break;
        }
        case '>':
            direction = 0;
            break;
        case '<':
            direction = 1;
            break;
        case 'v':
            direction = 2;
            break;
        case '^':
            direction = 3;
            break;
        case '?':
            direction = rand() % 4;
            break;
        case '_':
            direction = pop() ? 1 : 0;
            break;
        case '|':
            direction = pop() ? 3 : 2;
            break;
        case '"':
            for (;;) {
                next();
                if (program[i][j] == '"')
                    break;
                push(program[i][j]);
            }
            break;
        case ':': {
            long temp = pop(head);
            push(temp);
            push(temp);
            break;
        }
        case '\\': {
            long temp1 = pop();
            long temp2 = pop();
            push(temp1);
            push(temp2);
            break;
        }
        case '$':
            pop();
            break;
        case '.':
            printf("%ld ", pop());
            break;
        case ',':
            printf("%c", (char)pop());
            break;
        case '#':
            next();
            break;
        case 'g': {
            long i = pop();
            long j = pop();
            push(program[i][j]);
            break;
        }
        case 'p': {
            long i = pop();
            long j = pop();
            long command = pop();
            if (command <= 10)
                command += '0';
            program[i][j] = command;
            break;
        }
        case '&': {
            long temp;
            if (scanf("%ld", &temp) != 1) {
                fprintf(stderr, "Read failed\n");
                return -1;
            }
            push(temp);
            break;
        }
        case '~':
            push(getchar());
            break;
        case '@':
            return 0;
        case ' ':
            break;
        default:
            fprintf(stderr, "Command %c not found\n", command);
            return -1;
        }
        next();
        // fprintf(stderr, "%c: ", command);
        // printStack(head);
    }
}