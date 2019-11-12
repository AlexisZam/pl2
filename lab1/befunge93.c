#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define M 80
#define N 25

struct stack {
    long x;
    struct stack *next;
};

void init(struct stack *s) {
    s = NULL;
}

void push(struct stack *s, long x) {
    struct stack *t = malloc(sizeof(struct stack));
    t->x = x;
    t->next = s;
}

long pop(struct stack *s) {
    if (s == NULL)
        return 0;
    struct stack *t = s;
    long x = s->x;
    s = s->next;
    free(t);
    return x;
}

int main(int argc, char *argv[]) {
    if (argc != 1) {
        fprintf(stderr, "Usage: ./befunge93 program");
        return -1;
    }

    char program[M][N];
    for (int i = 1; i < M; i++)
        for (int j = 1; j < N; j++)
            program[i][j] = ' ';

    FILE *fp = fopen(argv[1], "r");
    for (int i = 1; i < M; i++)
        for (int j = 1; j < N; j++) {
            int c = fgetc(fp);
            if (c == '\n')
                break;
            if (c == EOF)
                goto end;
            program[i][j] = c;
        }
end:
    fclose(fp);

    char cmd;
    int i = 0, j = 0;

    struct stack *s;
    long acc;
    init(s);

    srand(time(NULL));

    for (;;) {
        cmd = program[i][j];
        switch (cmd) {
        case 0:
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
        case 6:
        case 7:
        case 8:
        case 9:
            push(s, acc);
            acc = cmd;
            i = (i + 1) % N;
            break;
        case '+':
            acc = pop(s) + acc;
            i = (i + 1) % N;
            break;
        case '-':
            acc = pop(s) - acc;
            i = (i + 1) % N;
            break;
        case '*':
            acc = pop(s) * acc;
            i = (i + 1) % N;
            break;
        case '/':
            acc = pop(s) / acc;
            i = (i + 1) % N;
            break;
        case '%':
            acc = pop(s) % acc;
            i = (i + 1) % N;
            break;
        case '!':
            acc = !acc;
            i = (i + 1) % N;
            break;
        case '`':
            acc = pop(s) > acc;
            i = (i + 1) % N;
            break;
        case '>':
            i = (i + 1) % N;
            break;
        case '<':
            i = (i - 1) % N;
            break;
        case '^':
            i = (j + 1) % M;
            break;
        case 'v':
            i = (j - 1) % M;
            break;
        case '?':
            switch (rand() % 4) {
            case 0:
                i = (i + 1) % N;
                break;
            case 1:
                i = (i - 1) % N;
                break;
            case 2:
                i = (j + 1) % M;
                break;
            case 3:
                i = (j - 1) % M;
            }
            break;
        case '_':
            i = acc ? (i - 1) % N : (i + 1) % N;
            acc = pop(s);
            break;
        case '|':
            j = acc ? (j - 1) % M : (j + 1) % M;
            acc = pop(s);
            break;
        case '"':
            break;
        case ':':
            push(s, acc);
            i = (i + 1) % N;
            break;
        case '\\':
            break;
        case '$':
            pop(s);
            break;
        case '.':
            break;
        case ',':
            break;
        case '#':
            i = (i + 2) % N;
            break;
        case 'g':
            acc = program[pop(s)][acc];
            i = (i + 1) % N;
            break;
        case 'p':
            long i = pop(s);
            program[i][acc] = pop(s);
            acc = pop(s);
            break;
        case '&':
            break;
        case '~':
            push(s, getchar());
            i = (i + 1) % N;
            break;
        case '@':
            return 0;
        case ' ':
            break;
        default:
            fprintf(stderr, "Command %c not found", cmd);
            return -1;
        }
    }
}