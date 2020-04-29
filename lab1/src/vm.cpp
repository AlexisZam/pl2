#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <vector>

/* Prototypes */

void push(long);
void pop(long);
void print_stack(long);

/* Stack */

std::vector<long> stack;

void push(long value) {
    stack.push_back(value);
}

long pop() {
    if (stack.empty())
        return 0;
    long value = stack.back(); // FIXME reference
    stack.pop_back();
    return value;
}

void print_stack() {
    for (int i = stack.size() - 1; i >= 0; i--)
        dprintf(3, "%ld ", stack[i]);
    dprintf(3, "\n");
}

std::size_t i = 0, j = 0;
int di = 1, dj = 0;
void **pc;

#define HEIGHT 25
#define WIDTH 80
char program[HEIGHT * WIDTH] = {' '};

void next() {
    std::size_t temp1 = j, temp2 = i;
    i = (i + di + WIDTH) % WIDTH;
    j = (j + dj + HEIGHT) % HEIGHT;
    pc += (j - temp1) * WIDTH + (i - temp2);
}

void parse_program(const char *filename) {
    FILE *stream = fopen(filename, "r");
    if (!stream) {
        std::cerr << "Error: couldn't open '" << filename << "' for input.\n";
        exit(1);
    }
    std::size_t i = 0, j = 0;
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
        exit(1);
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

    void *program_as_labels[HEIGHT * WIDTH];
    for (std::size_t j = 0; j < HEIGHT; j++)
        for (std::size_t i = 0; i < WIDTH; i++)
            program_as_labels[j * WIDTH + i] = labels[program[j * WIDTH + i]];
    pc = program_as_labels;

    srand(time(NULL));

    goto **pc;
digit:
    push(program[j * WIDTH + i] - '0');
    next();
    print_stack();
    goto **pc;
add:
    push(pop() + pop());
    next();
    print_stack();
    goto **pc;
subtract : {
    long temp = pop();
    push(pop() - temp);
    next();
    print_stack();
    goto **pc;
}
multiply:
    push(pop() * pop());
    next();
    print_stack();
    goto **pc;
divide : {
    long temp = pop();
    if (temp == 0) {
        pop();
        std::cout << "What do you want " << temp << "/0 to be? ";
        std::cin >> temp;
        push(temp);
    } else
        push(pop() / temp);
    next();
    print_stack();
    goto **pc;
}
modulo : {
    long temp = pop();
    if (temp == 0) {
        pop();
        std::cout << "What do you want " << temp << "/0 to be? ";
        std::cin >> temp;
        push(temp);
    } else
        push(pop() % temp);
    next();
    print_stack();
    goto **pc;
}
negate:
    push(!pop());
    next();
    print_stack();
    goto **pc;
greater : {
    long temp = pop();
    push(pop() > temp);
    next();
    print_stack();
    goto **pc;
}
right:
    di = 1;
    dj = 0;
    next();
    print_stack();
    goto **pc;
left:
    di = -1;
    dj = 0;
    next();
    print_stack();
    goto **pc;
up:
    di = 0;
    dj = -1;
    next();
    print_stack();
    goto **pc;
down:
    di = 0;
    dj = 1;
    next();
    print_stack();
    goto **pc;
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
        print_stack();
        if (program[j * WIDTH + i] == '"')
            break;
        push(program[j * WIDTH + i]);
    }
    next();
    print_stack();
    goto **pc;
dup : {
    long temp = pop();
    push(temp);
    push(temp);
    next();
    print_stack();
    goto **pc;
}
swap : {
    long temp1 = pop();
    long temp2 = pop();
    push(temp1);
    push(temp2);
    next();
    print_stack();
    goto **pc;
}
pop:
    pop();
    next();
    print_stack();
    goto **pc;
output_int:
    std::cout << pop() << std::flush;
    next();
    print_stack();
    goto **pc;
output_char:
    std::cout << char(pop()) << std::flush;
    next();
    print_stack();
    goto **pc;
bridge:
    next();
    next();
    print_stack();
    goto **pc;
get : {
    long temp1 = pop();
    long temp2 = pop();
    if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
        std::cerr << "g 'Get' instruction out of bounds (" << temp2 << "," << temp1 << ")\n";
        push(0);
    } else
        push(program[temp1 * WIDTH + temp2]);
    next();
    print_stack();
    goto **pc;
}
put : {
    long temp1 = pop();
    long temp2 = pop();
    if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
        std::cerr << "p 'Put' instruction out of bounds (" << temp2 << "," << temp1 << ")\n";
        pop();
    } else {
        program[temp1 * WIDTH + temp2] = char(pop());
        program_as_labels[temp1 * WIDTH + temp2] = labels[program[temp1 * WIDTH + temp2]];
    }
    next();
    print_stack();
    goto **pc;
}
input_int : {
    long temp = -1;
    std::cin >> temp;
    push(temp);
    next();
    print_stack();
    goto **pc;
}
input_character:
    push(getchar());
    next();
    print_stack();
    goto **pc;
end:
    exit(0);
nop:
    next();
    print_stack();
    goto **pc;
unsupported:
    fprintf(stderr, "Unsupported instruction '%c' (0x%02x) (maybe not Befunge-93?)\n", program[j * WIDTH + i], program[j * WIDTH + i]);
    next();
    print_stack();
    goto **pc;
}
