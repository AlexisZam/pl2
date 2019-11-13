#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define M 25
#define N 80

int direction = 0;
int i = 0, j = 0;

void next() {
    switch (direction) {
    case 0:
        j = (j + 1) % N;
        break;
    case 1:
        j = (j - 1) % N;
        break;
    case 2:
        i = (i + 1) % M;
        break;
    case 3:
        i = (i - 1) % M;
    }
}

struct entry {
    long x;
    struct entry *next;
};

struct stack {
    struct entry *head;
};

void push(struct stack *s, long a) {
    struct entry *e = malloc(sizeof(struct entry));
    e->x = a;
    e->next = s->head;
    s->head = e;
}

long peek(struct stack *s) {
    if (!s->head)
        return 0;
    return s->head->x;
}

long pop(struct stack *s) {
    if (!s->head)
        return 0;
    struct entry *e = s->head;
    long x = s->head->x;
    s->head = e->next;
    free(e);
    return x;
}

void print(struct stack s) {
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: ./befunge93 program\n");
        return -1;
    }

    char program[M][N];
    for (int i = 0; i < M; i++)
        for (int j = 0; j < N; j++)
            program[i][j] = ' ';

    // todo: input too large
    FILE *fp = fopen(argv[1], "r");
    for (int i = 0; i < M; i++)
        for (int j = 0; j < N; j++) {
            int c = fgetc(fp);
            if (c == '\n')
                break;
            if (c == EOF)
                goto eof;
            program[i][j] = c;
        }
eof:
    fclose(fp);

    char cmd;
    long tmp1, tmp2;
    struct stack *s = malloc(sizeof(struct stack));
    s->head = NULL;

    srand(time(NULL));

    for (;;) {
        cmd = program[i][j];
        printf("%c: ", cmd);
        for (struct entry *t = s->head; t; t = t->next)
            printf("%ld ", t->x);
        printf("\n");
        switch (cmd) {
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
            push(s, cmd - '0');
            next();
            break;
        case '+':
            tmp1 = pop(s);
            push(s, pop(s) + tmp1);
            next();
            break;
        case '-':
            tmp1 = pop(s);
            push(s, pop(s) - tmp1);
            next();
            break;
        case '*':
            tmp1 = pop(s);
            push(s, pop(s) * tmp1);
            next();
            break;
        case '/':
            tmp1 = pop(s);
            push(s, pop(s) / tmp1);
            next();
            break;
        case '%':
            tmp1 = pop(s);
            push(s, pop(s) % tmp1);
            next();
            break;
        case '!':
            push(s, pop(s));
            next();
            break;
        case '`':
            tmp1 = pop(s);
            push(s, pop(s) > tmp1);
            next();
            break;
        case '>':
            direction = 0;
            next();
            break;
        case '<':
            direction = 1;
            next();
            break;
        case 'v':
            direction = 2;
            next();
            break;
        case '^':
            direction = 3;
            next();
            break;
        case '?':
            direction = rand() % 4;
            next();
            break;
        case '_':
            direction = pop(s) ? 1 : 0;
            next();
            break;
        case '|':
            direction = pop(s) ? 3 : 2;
            next();
            break;
        case '"':
            for (;;) {
                next();
                if (program[i][j] == '"')
                    break;
                push(s, program[i][j]);
            }
            next();
            break;
        case ':':
            push(s, peek(s));
            next();
            break;
        case '\\':
            tmp1 = pop(s);
            tmp2 = pop(s);
            push(s, tmp2);
            push(s, tmp1);
            next();
            break;
        case '$':
            pop(s);
            next();
            break;
        case '.':
            printf("%d ", (int)pop(s));
            next();
            break;
        case ',':
            printf("%c", (char)pop(s));
            next();
            break;
        case '#':
            next();
            next();
            break;
        case 'g':
            tmp1 = pop(s);
            push(s, program[pop(s)][tmp1]);
            next();
            break;
        case 'p':
            tmp1 = pop(s);
            tmp2 = pop(s);
            program[tmp2][tmp1] = pop(s);
            next();
            break;
        case '&':
            if (scanf("%ld", &tmp1) != 1) {
                fprintf(stderr, "Read failed\n");
                return -1;
            }
            push(s, tmp1);
            next();
            break;
        case '~':
            push(s, getchar());
            next();
            break;
        case '@':
            // printf("\n");
            return 0;
        case ' ':
            break;
        default:
            fprintf(stderr, "Command %c not found\n", cmd);
            return -1;
        }
    }
}