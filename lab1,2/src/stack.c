#include "heap.h"
#include <stdio.h>
#include <stdlib.h>

int top = 0;
#ifdef DYNAMIC_STACK
int stack_size = 0;
struct value *stack = NULL;
#else
#define STACK_SIZE 1024 * 1024
struct value stack[STACK_SIZE];
#endif /* DYNAMIC_STACK */

void init_stack() {
#ifdef DYNAMIC_STACK
#define INIT_SIZE 1024
    stack_size = INIT_SIZE;
    stack = malloc(stack_size * sizeof(struct value));
#endif /* DYNAMIC_STACK */
}

void push(struct value value) {
#ifdef DYNAMIC_STACK
    if (top == stack_size) {
        stack_size *= 2;
        stack = realloc(stack, stack_size * sizeof(struct value));
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

#define STACK_FD 3
void print_stack() {
#ifdef PRINT_STACK
    dprintf(STACK_FD, "(((%d))) ", top);
    for (int i = top - 1; i >= 0; i--) {
#ifdef BEFUNGE93PLUS
        if (stack[i].marked)
            dprintf(STACK_FD, "\033[1;31m");
        dprintf(STACK_FD, "%ld:%d ",
                stack[i].value, stack[i].pointer);
        dprintf(STACK_FD, "\033[0m");
#else
        dprintf(STACK_FD, "%ld ", stack[i].value);
#endif /* BEFUNGE93PLUS */
    }
    dprintf(STACK_FD, "\n");
#endif /* PRINT_STACK */
}

// TODO: stack used as adresses are implicitly cast, and conversely
// TODO: should we type check?

#ifdef BEFUNGE93PLUS
void mark() {
    for (int i = 0; i < top; i++)
        if (stack[i].pointer)
            DFS(&stack[i]);
    // print_heap();
    // print_stack();
}
#endif /* BEFUNGE93PLUS */
