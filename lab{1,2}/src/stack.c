// #ifdef BEFUNGE93PLUS
#include "heap.h"
// #endif
#include "stack.h"
#include <stdio.h>
#include <stdlib.h>

#define STACK_FD 3

#ifdef LINKED_LIST

struct stack {
    value_t value;
    struct stack *next;
} *stack = NULL;

void init_stack() {}

//TODO: top of stack caching
void push(value_t value) {
    struct stack *s = malloc(sizeof(struct stack));
    s->value = value;
    s->next = stack;
    stack = s;
}

value_t pop() {
    if (!stack)
        return 0;
    struct stack *s = stack;
    value_t value = stack->value;
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
    value_t *values;
} stack = {0, 0, NULL};

void init_stack() {
#define INIT_SIZE 1024
    stack.size = INIT_SIZE;
    stack.values = malloc(stack.size * sizeof(value_t));
}

void push(value_t value) {
    if (stack.top == stack.size) {
        stack.size *= 2;
        stack.values = realloc(stack.values, stack.size * sizeof(value_t));
    }
    stack.values[stack.top++] = value;
}

value_t pop() {
    if (!stack.top)
        return (value_t){0};
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
    value_t values[STACK_SIZE];
} stack = {.top = 0};

void init_stack() {}

void push(value_t value) {
    if (stack.top == STACK_SIZE) {
        fprintf(stderr, "Stack overflow");
        exit(-1);
    }
    stack.values[stack.top++] = value;
}

value_t pop() {
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

#ifdef BEFUNGE93PLUS

void DFS(value_t v) { // TODO: improve DFS
    if (v.type == HEAP_ADDRESS && !v.marked) {
        v.marked = true;
        DFS(head(v.value));
        DFS(tail(v.value));
    }
}

#ifdef LINKED_LIST
void mark() {
    for (struct stack *s = stack; s; s = s->next)
        if (s->value.type == HEAP_ADDRESS)
            DFS(s->value);
}
#else
void mark() {
    for (int i = stack.top - 1; i >= 0; i--)
        if (stack.values[i].type == HEAP_ADDRESS)
            DFS(stack.values[i]);
}
#endif

void push_value(int64_t value) {
    push((value_t){value, INT64_T, true});
}

void push_heap_address(int64_t heap_address) {
    push((value_t){heap_address, HEAP_ADDRESS, false});
}

int64_t pop_value() {
    value_t v = pop();
    if (v.type != INT64_T) // TODO: should we type check?
        fprintf(stderr, "Type error");
    return v.value;
}

int64_t pop_heap_address() {
    value_t v = pop();
    if (v.type != HEAP_ADDRESS) // TODO: should we type check?
        fprintf(stderr, "Type error");
    return v.value;
}
#else
void push_value(long value) { push(value); }

long pop_value() { return pop(); }
#endif