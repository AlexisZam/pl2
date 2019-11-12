#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define M 25
#define N 80

struct stack {
    long x;
    struct stack *next;
};

void push(struct stack *s, long x) {
    struct stack *t = malloc(sizeof(struct stack));
    t->x = x;
    t->next = s;
}

long pop(struct stack *s) {
    if (s == NULL)
        return 0;
    long x = s->x;
    struct stack *t = s;
    s = s->next;
    free(t);
    return x;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: ./befunge93 program");
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
    int i = 0, j = 0;

    struct stack *s = NULL;
    long acc = -1;

    srand(time(NULL));

    long tmp;

    for (;;) {
        cmd = program[i][j];
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
            push(s, acc);
            acc = (int)cmd;
            j = (j + 1) % N;
            break;
        case '+':
            acc = pop(s) + acc;
            j = (j + 1) % N;
            break;
        case '-':
            acc = pop(s) - acc;
            j = (j + 1) % N;
            break;
        case '*':
            acc = pop(s) * acc;
            j = (j + 1) % N;
            break;
        case '/':
            acc = pop(s) / acc;
            j = (j + 1) % N;
            break;
        case '%':
            acc = pop(s) % acc;
            j = (j + 1) % N;
            break;
        case '!':
            acc = !acc;
            j = (j + 1) % N;
            break;
        case '`':
            acc = pop(s) > acc;
            j = (j + 1) % N;
            break;
        case '>':
            j = (j + 1) % N;
            break;
        case '<':
            j = (j - 1) % N;
            break;
        case '^':
            i = (i + 1) % M;
            break;
        case 'v':
            i = (i - 1) % M;
            break;
        case '?':
            switch (rand() % 4) {
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
            for (;;) {
                j = (j + 1) % N;
                if (program[i][j] == '"')
                    break;
                push(s, acc);
                acc = (char)program[i][j];
            }
            j = (j + 1) % N;
            break;
        case ':':
            push(s, acc);
            j = (j + 1) % N;
            break;
        case '\\':
            tmp = acc;
            acc = s->x;
            s->x = tmp;
            break;
        case '$':
            pop(s);
            j = (j + 1) % N;
            break;
        case '.':
            printf("%c", (char)acc);
            acc = pop(s);
            j = (j + 1) % N;
            break;
        case ',':
            printf("%d ", (int)acc);
            acc = pop(s);
            j = (j + 1) % N;
            break;
        case '#':
            i = (i + 2) % N;
            break;
        case 'g':
            acc = program[pop(s)][acc];
            j = (j + 1) % N;
            break;
        case 'p':
            tmp = pop(s);
            program[tmp][acc] = pop(s);
            acc = pop(s);
            j = (j + 1) % N;
            break;
        case '&':
            if (scanf("%ld", &tmp) != 1) {
                fprintf(stderr, "Read failed");
                return -1;
            }
            push(s, acc);
            acc = tmp;
            j = (j + 1) % N;
            break;
        case '~':
            push(s, getchar());
            j = (j + 1) % N;
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