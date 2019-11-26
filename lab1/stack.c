#include <stdio.h>
#include <stdlib.h>

#define STACK_FD 3

#ifdef LINKED_LIST

#ifdef BEFUNGE93PLUS
emum type{LONG, ADDRESS};
#endif

struct stack {
#ifdef BEFUNGE93PLUS
    enum type type;
#endif
    long value; // TODO: int64_t
    struct stack *next;
} *stack = NULL;

void init_stack() {}

//TODO: top of stack caching
void push(long value) {
    struct stack *s = malloc(sizeof(struct stack));
    s->value = value;
    s->next = stack;
    stack = s;
}

long pop() {
    if (!stack)
        return 0;
    struct stack *s = stack;
    long value = stack->value;
    stack = stack->next;
    free(s);
    return value;
}

void free_stack() {
    struct stack *s;
    while (stack) {
        s = stack;
        stack = stack->next;
        free(s);
    }
}

void print_stack() {
#ifdef PRINT_STACK
    for (struct stack *s = stack; s; s = s->next)
        dprintf(STACK_FD, "%ld ", s->value);
    dprintf(STACK_FD, "\n");
#endif
}

#elif defined DYNAMIC_ARRAY

struct stack {
    int size; // TODO: size_t
    int top;
    long *values;
} stack = {.size = 0, .top = 0, .values = NULL};

void init_stack() {
#define INIT_SIZE 1024
    stack.size = INIT_SIZE;
    stack.values = malloc(stack.size * sizeof(long));
}

void push(long value) {
    if (stack.top == stack.size) {
        stack.size *= 2;
        stack.values = realloc(stack.values, stack.size * sizeof(long));
    }
    stack.values[stack.top++] = value;
}

long pop() {
    if (!stack.top)
        return 0;
    return stack.values[--stack.top];
}

void free_stack() {
    stack.size = 0;
    stack.top = 0;
    free(stack.values);
    stack.values = NULL;
}

void print_stack() {
#ifdef PRINT_STACK
    for (int i = stack.top - 1; i >= 0; i--)
        dprintf(STACK_FD, "%ld ", stack.values[i]);
    dprintf(STACK_FD, "\n");
#endif
}

#elif defined STATIC_ARRAY

#define STACK_SIZE 1024 * 1024
struct stack {
    int top;
    long values[STACK_SIZE];
} stack = {.top = 0};

void init_stack() {}

void push(long value) {
    if (stack.top == STACK_SIZE) {
        fprintf(stderr, "Stack overflow");
        exit(-1);
    }
    stack.values[stack.top++] = value;
}

long pop() {
    if (!stack.top)
        return 0;
    return stack.values[--stack.top];
}

void free_stack() {
    stack.top = 0;
}

void print_stack() {
#ifdef PRINT_STACK
    for (int i = stack.top - 1; i >= 0; i--)
        dprintf(STACK_FD, "%ld ", stack.values[i]);
    dprintf(STACK_FD, "\n");
#endif
}

#endif
