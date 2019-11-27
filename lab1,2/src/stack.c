#include "heap.h"
#include <stdio.h>
#include <stdlib.h>

int top = 0;
#ifdef DYNAMIC_STACK
int size = 0;
struct value *stack = NULL;
#else
#define STACK_SIZE 1024 * 1024
struct value stack[STACK_SIZE];
#endif /* DYNAMIC_STACK */

void init_stack() {
#ifdef DYNAMIC_STACK
#define INIT_SIZE 1024
    size = INIT_SIZE;
    stack = malloc(size * sizeof(struct value));
#endif /* DYNAMIC_STACK */
}

void push(struct value value) {
#ifdef DYNAMIC_STACK
    if (top == size) {
        size *= 2;
        stack = realloc(stack, size * sizeof(struct value));
    }
#else
    if (top == STACK_SIZE) {
        fprintf(stderr, "Stack overflow\n");
        exit(-1);
    }
#endif /* DYNAMIC_STACK */
    stack[top++] = value;
}

struct value pop() {
    if (!top)
        return (struct value){0};
    return stack[--top];
}

void free_stack() {
    top = 0;
#ifdef DYNAMIC_STACK
    size = 0;
    free(stack);
    stack = NULL;
#endif /* DYNAMIC_STACK */
}

#define STACK_FD 3
void print_stack() {
#ifdef PRINT_STACK
    for (int i = top - 1; i >= 0; i--)
        dprintf(STACK_FD, "%ld ", stack[i].value);
    dprintf(STACK_FD, "\n");
#endif /* PRINT_STACK */
}

#ifdef BEFUNGE93PLUS

void DFS(struct value value) { // TODO: improve DFS
    if (value.type == HEAP_ADDRESS && !value.marked) {
        value.marked = true;
        DFS(head(value));
        DFS(tail(value));
    }
}

void mark() {
    for (int i = top - 1; i >= 0; i--)
        if (stack[i].type == HEAP_ADDRESS)
            DFS(stack[i]);
}

void push_value(long_t value) {
    push((struct value){value});
}

// TODO: stack used as adresses are implicitly cast, and conversely
// TODO: should we type check?
long_t pop_value() { return pop().value; }

#else

void push_value(long_t value) { push((struct value){value}); }

#endif /* BEFUNGE93PLUS */
