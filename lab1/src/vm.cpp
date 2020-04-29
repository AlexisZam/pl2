#include <array>
#include <iostream>
#include <vector>

inline void push(std::vector<long> &stack, const long &value) {
    stack.push_back(value);
}

inline long pop(std::vector<long> &stack) { // FIXME return ref
    if (stack.empty())
        return 0;
    long value = stack.back();
    stack.pop_back();
    return value;
}

inline void print_stack(const std::vector<long> &stack) {
    for (int i = stack.size() - 1; i >= 0; i--)
        dprintf(3, "%ld ", stack[i]);
    dprintf(3, "\n");
}

#define HEIGHT 25
#define WIDTH 80

inline void next(std::size_t &i, std::size_t &j, const int &di, const int &dj, void **&pc) {
    std::size_t temp1 = j, temp2 = i;
    i = (i + di + WIDTH) % WIDTH;
    j = (j + dj + HEIGHT) % HEIGHT;
    pc += (j - temp1) * WIDTH + (i - temp2);
}

void parse_program(std::array<char, HEIGHT * WIDTH> &program, const char *filename) {
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
    std::array<char, HEIGHT * WIDTH> program;
    void *program_as_labels[HEIGHT * WIDTH];
    std::vector<long> stack;
    std::size_t i = 0, j = 0;
    int di = 1, dj = 0;
    void **pc;

    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " foo.bf\n";
        exit(1);
    }

    program.fill(' ');
    parse_program(program, argv[1]);

    std::array<void *, 128> labels;
    labels.fill(&&unsupported);
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

    for (std::size_t j = 0; j < HEIGHT; j++)
        for (std::size_t i = 0; i < WIDTH; i++)
            program_as_labels[j * WIDTH + i] = labels[program[j * WIDTH + i]];
    pc = program_as_labels;

    srand(time(NULL));

    goto **pc;
digit:
    push(stack, program[j * WIDTH + i] - '0');
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
add:
    push(stack, pop(stack) + pop(stack));
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
subtract : {
    long temp = pop(stack);
    push(stack, pop(stack) - temp);
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
}
multiply:
    push(stack, pop(stack) * pop(stack));
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
divide : {
    long temp = pop(stack);
    if (temp == 0) {
        pop(stack);
        std::cout << "What do you want " << temp << "/0 to be? ";
        std::cin >> temp;
        push(stack, temp);
    } else
        push(stack, pop(stack) / temp);
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
}
modulo : {
    long temp = pop(stack);
    if (temp == 0) {
        pop(stack);
        std::cout << "What do you want " << temp << "/0 to be? ";
        std::cin >> temp;
        push(stack, temp);
    } else
        push(stack, pop(stack) % temp);
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
}
negate:
    push(stack, !pop(stack));
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
greater : {
    long temp = pop(stack);
    push(stack, pop(stack) > temp);
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
}
right:
    di = 1;
    dj = 0;
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
left:
    di = -1;
    dj = 0;
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
up:
    di = 0;
    dj = -1;
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
down:
    di = 0;
    dj = 1;
    next(i, j, di, dj, pc);
    print_stack(stack);
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
    if (pop(stack))
        goto left;
    goto right;
vertical_if:
    if (pop(stack))
        goto up;
    goto down;
stringmode:
    for (;;) {
        next(i, j, di, dj, pc);
        print_stack(stack);
        if (program[j * WIDTH + i] == '"')
            break;
        push(stack, program[j * WIDTH + i]);
    }
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
dup : {
    long temp = pop(stack);
    push(stack, temp);
    push(stack, temp);
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
}
swap : {
    long temp1 = pop(stack);
    long temp2 = pop(stack);
    push(stack, temp1);
    push(stack, temp2);
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
}
pop:
    pop(stack);
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
output_int:
    std::cout << pop(stack) << std::flush;
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
output_char:
    std::cout << char(pop(stack)) << std::flush;
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
bridge:
    next(i, j, di, dj, pc);
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
get : {
    long temp1 = pop(stack);
    long temp2 = pop(stack);
    if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
        std::cerr << "g 'Get' instruction out of bounds (" << temp2 << "," << temp1 << ")\n";
        push(stack, 0);
    } else
        push(stack, program[temp1 * WIDTH + temp2]);
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
}
put : {
    long temp1 = pop(stack);
    long temp2 = pop(stack);
    if (temp1 < 0 || temp1 >= HEIGHT || temp2 < 0 || temp2 >= WIDTH) {
        std::cerr << "p 'Put' instruction out of bounds (" << temp2 << "," << temp1 << ")\n";
        pop(stack);
    } else {
        program[temp1 * WIDTH + temp2] = char(pop(stack));
        program_as_labels[temp1 * WIDTH + temp2] = labels[program[temp1 * WIDTH + temp2]];
    }
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
}
input_int : {
    long temp = -1;
    std::cin >> temp;
    push(stack, temp);
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
}
input_character:
    push(stack, getchar());
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
end:
    exit(0);
nop:
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
unsupported:
    fprintf(stderr, "Unsupported instruction '%c' (0x%02x) (maybe not Befunge-93?)\n", program[j * WIDTH + i], program[j * WIDTH + i]);
    next(i, j, di, dj, pc);
    print_stack(stack);
    goto **pc;
}
