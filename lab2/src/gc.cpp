#include <array>
#include <boost/range/adaptor/reversed.hpp>
#include <fstream>
#include <iostream>
#include <vector>

#define WIDTH 80
#define HEIGHT 25
#define STACK_SIZE (1024 * 1024)
#define HEAP_SIZE (16 * 1024 * 1024)

class State {
public:
    State() {
        for (auto &row : program)
            row.fill(' ');
        stack.reserve(STACK_SIZE);
        for (std::size_t i = 0; i < heap.size(); i++)
            heap[i].head.value = i + 1;
        ofstream.open("vm.stack");
    }

    ~State() {
        ofstream.close();
    }

    void read_program(const char *filename) {
        std::ifstream ifstream;
        std::string s;

        ifstream.open(filename);
        if (!ifstream) {
            std::cerr << "Error: couldn't open '" << filename << "' for input." << std::endl;
            exit(1);
        }
        for (auto &a : program) {
            if (ifstream.eof())
                break;
            std::getline(ifstream, s);
            s.copy(a.data(), a.size());
        }
        ifstream.close();
    }

    void print_program() {
        for (auto a : program) {
            for (auto e : a)
                std::cout << e;
            std::cout << std::endl;
        }
    }

    void move() {
        position.i = (position.i + direction.di + HEIGHT) % HEIGHT;
        position.j = (position.j + direction.dj + WIDTH) % WIDTH;
    }

    char command() { // FIXME return reference
        return program[position.i][position.j];
    }

    char &command(std::size_t i, std::size_t j) {
        return program.at(i).at(j);
    }

    void set_direction(int di, int dj) {
        direction.di = di;
        direction.dj = dj;
    }

    void push(const int64_t &value) { // reference??
        stack.push_back(Value(value));
    }

    int64_t pop() { // FIXME return reference
        if (stack.empty())
            return 0;
        int64_t value = stack.back().value;
        stack.pop_back();
        return value;
    }

    void dup() {
        if (stack.empty()) {
            stack.push_back(Value(0));
            stack.push_back(Value(0));
        } else
            stack.push_back(stack.back());
    }

    void swap() {
        std::size_t size = stack.size();
        if (size == 0) {
            stack.push_back(Value(0));
            stack.push_back(Value(0));
        } else if (size == 1)
            stack.push_back(Value(0));
        else
            std::swap(stack.back(), *(stack.end() - 2));
    }

    void allocate() {
        if (freelist == heap.size()) {
            gc();
            if (freelist == heap.size()) {
                std::cerr << "Heap overflow" << std::endl;
                exit(1);
            }
        }
        Value tail = pop_value();
        Value head = pop_value();
        std::size_t previous = freelist;
        stack.push_back(Value(previous, true, false));
        freelist = heap[freelist].head.value;
        heap[previous] = ConsCell(head, tail);
    }

    void head() {
        stack.push_back(heap[pop()].head);
    }

    void tail() {
        stack.push_back(heap[pop()].tail);
    }

private:
    std::array<std::array<char, WIDTH>, HEIGHT> program;
    struct {
        std::size_t i = 0, j = 0;
    } position;
    struct {
        int di = 0, dj = 1;
    } direction;
    int cnt = 0;

    struct Value {
        int64_t value : 62;
        bool pointer : 1;
        bool marked : 1;

        Value() {}
        Value(int64_t v, bool p = 0, bool m = 0) : value(v), pointer(p), marked(m) {}
    };
    std::vector<Value> stack;

    struct ConsCell {
        Value head;
        Value tail;

        ConsCell() {}
        ConsCell(Value h, Value t) : head(h), tail(t) {}
    };
    std::array<ConsCell, HEAP_SIZE> heap;
    std::size_t freelist = 0;

    std::ofstream ofstream;

    Value pop_value() { // FIXME return reference
        if (stack.empty())
            return 0;
        Value value = stack.back();
        stack.pop_back();
        return value;
    }

    void DFS(Value &x) { // TODO: improve DFS
        if (!x.marked) {
            x.marked = true;
            if (x.pointer) {
                DFS(heap[x.value].head);
                DFS(heap[x.value].tail);
            }
        }
    }

    void mark() {
        for (auto &e : stack)
            if (e.pointer)
                DFS(e);
    }

    void sweep() {
        for (int i = heap.size() - 1; i >= 0; i--) {
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
};

int main(int argc, char *argv[]) {
    static State state;
    std::array<void *, 128> labels;
    int64_t temp, temp1, temp2;

    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " foo.bf" << std::endl;
        exit(1);
    }

    state.read_program(argv[1]);

    srand(42); // FIXME

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
    labels['c'] = &&cons;
    labels['h'] = &&head;
    labels['t'] = &&tail;

    goto *labels[state.command()];
digit:
    state.push(state.command() - '0');
    state.move();
    goto *labels[state.command()];
add:
    state.push(state.pop() + state.pop());
    state.move();
    goto *labels[state.command()];
subtract:
    temp = state.pop();
    state.push(state.pop() - temp);
    state.move();
    goto *labels[state.command()];
multiply:
    state.push(state.pop() * state.pop());
    state.move();
    goto *labels[state.command()];
divide:
    temp = state.pop();
    if (temp == 0) {
        state.pop();
        std::cout << "What do you want " << temp << "/0 to be? ";
        std::cin >> temp;
        state.push(temp);
    } else
        state.push(state.pop() / temp);
    state.move();
    goto *labels[state.command()];
modulo:
    temp = state.pop();
    if (temp == 0) {
        state.pop();
        std::cout << "What do you want " << temp << "/0 to be? ";
        std::cin >> temp;
        state.push(temp);
    } else
        state.push(state.pop() % temp);
    state.move();
    goto *labels[state.command()];
negate:
    state.push(!state.pop());
    state.move();
    goto *labels[state.command()];
greater:
    temp = state.pop();
    state.push(state.pop() > temp);
    state.move();
    goto *labels[state.command()];

right:
    state.set_direction(0, 1);
    state.move();
    goto *labels[state.command()];
left:
    state.set_direction(0, -1);
    state.move();
    goto *labels[state.command()];
up:
    state.set_direction(-1, 0);
    state.move();
    goto *labels[state.command()];
down:
    state.set_direction(1, 0);
    state.move();
    goto *labels[state.command()];
random:
    switch (rand() / 32 % 4) {
    case 0:
        goto right;
    case 1:
        goto left;
    case 2:
        goto up;
    case 3:
        goto down;
    }
horizontal_if:
    if (state.pop())
        goto left;
    goto right;
vertical_if:
    if (state.pop())
        goto up;
    goto down;

stringmode:
    for (;;) {
        state.move();
        if (state.command() == '"')
            break;
        state.push(state.command());
    }
    state.move();
    goto *labels[state.command()];

dup:
    state.dup();
    state.move();
    goto *labels[state.command()];
swap:
    state.swap();
    state.move();
    goto *labels[state.command()];
pop:
    state.pop();
    state.move();
    goto *labels[state.command()];

output_int:
    std::cout << state.pop() << ' ' << std::flush;
    state.move();
    goto *labels[state.command()];
output_char:
    std::cout << char(state.pop()) << std::flush;
    state.move();
    goto *labels[state.command()];

bridge:
    state.move();
    state.move();
    goto *labels[state.command()];

get:
    temp1 = state.pop();
    temp2 = state.pop();
    try {
        state.push(state.command(temp1, temp2));
    } catch (std::out_of_range) {
        std::cerr << "g 'Get' instruction out of bounds (" << temp2 << "," << temp1 << ")" << std::endl;
        state.push(0);
    }
    state.move();
    goto *labels[state.command()];
put:
    temp1 = state.pop();
    temp2 = state.pop();
    try {
        state.command(temp1, temp2) = char(state.pop());
    } catch (std::out_of_range) {
        std::cerr << "p 'Put' instruction out of bounds (" << temp2 << "," << temp1 << ")" << std::endl;
        state.pop();
    }
    state.move();
    goto *labels[state.command()];

input_int:
    std::cin >> temp;
    state.push(temp);
    state.move();
    goto *labels[state.command()];
input_character:
    state.push(getchar());
    state.move();
    goto *labels[state.command()];

end:
    exit(0);

nop:
    state.move();
    goto *labels[state.command()];

cons : {
    state.allocate();
    state.move();
    goto *labels[state.command()];
}
head:
    state.head();
    state.move();
    goto *labels[state.command()];
tail:
    state.tail();
    state.move();
    goto *labels[state.command()];

unsupported:
    fprintf(stderr, "Unsupported instruction '%c' (0x%02x) (maybe not Befunge-93?)\n", state.command(), state.command());
    state.move();
    goto *labels[state.command()];
}
