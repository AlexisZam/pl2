#ifdef BEFUNGE93PLUS
#include "heap.h"
#endif /* BEFUNGE93PLUS */
#include "stack.h"
#include <stdio.h>
#include <stdlib.h>

// struct {
//     struct value value;
//     struct stack *next;
// } *stack = NULL;

// void init_stack() {}

// //TODO: top of stack caching
// void push(struct value value) {
//     struct stack *s = malloc(sizeof(struct stack));
//     s->value = value;
//     s->next = stack;
//     stack = s;
// }

// struct value pop() {
//     if (!stack)
//         return 0;
//     struct stack *s = stack;
//     struct value value = stack->value;
//     stack = stack->next;
//     free(s);
//     return value;
// }

// void free_stack() {
//     struct stack *s;
//     while (stack) {
//         s = stack;
//         stack = stack->next;
//         free(s);
//     }
// }

// void print_stack() {
// #ifdef PRINT_STACK
//     for (struct stack *s = stack; s; s = s->next)
//         dprintf(STACK_FD, "%ld ", s->value.value);
//     dprintf(STACK_FD, "\n");
// #endif /* PRINT_STACK */
// }

#ifdef DYNAMIC_STACK
struct {
    int size;
    int top;
    struct value *values;
} stack = {0, 0, NULL};
#else
#define STACK_SIZE 1024 * 1024
struct {
    int top;
    struct value values[STACK_SIZE];
} stack = {0};
#endif /* DYNAMIC_STACK */

void init_stack() {
#ifdef DYNAMIC_STACK
#define INIT_SIZE 1024
    stack.size = INIT_SIZE;
    stack.values = malloc(stack.size * sizeof(struct value));
#endif /* DYNAMIC_STACK */
}

void push(struct value value) {
#ifdef DYNAMIC_STACK
    if (stack.top == stack.size) {
        stack.size *= 2;
        stack.values = realloc(stack.values, stack.size * sizeof(struct value));
    }
#else
    if (stack.top == STACK_SIZE) {
        fprintf(stderr, "Stack overflow\n");
        exit(-1);
    }
#endif /* DYNAMIC_STACK */
    stack.values[stack.top++] = value;
}

struct value pop() {
    if (!stack.top)
        return (struct value){0};
    return stack.values[--stack.top];
}

void free_stack() {
    stack.top = 0;
#ifdef DYNAMIC_STACK
    stack.size = 0;
    free(stack.values);
    stack.values = NULL;
#endif /* DYNAMIC_STACK */
}

#define STACK_FD 3
void print_stack() {
#ifdef PRINT_STACK
    for (int i = stack.top - 1; i >= 0; i--)
        dprintf(STACK_FD, "%ld ", stack.values[i].value);
    dprintf(STACK_FD, "\n");
#endif /* PRINT_STACK */
}

#ifdef BEFUNGE93PLUS

void DFS(struct value value) { // TODO: improve DFS
    if (value.type == HEAP_ADDRESS && !value.marked) {
        value.marked = true;
        DFS(head(value.value));
        DFS(tail(value.value));
    }
}

// void mark() { // TODO: save roots
//     for (struct stack *s = stack; s; s = s->next)
//         if (s->value.type == HEAP_ADDRESS)
//             DFS(s->value);
// }

void mark() {
    for (int i = stack.top - 1; i >= 0; i--)
        if (stack.values[i].type == HEAP_ADDRESS)
            DFS(stack.values[i]);
}

void push_value(value_t value) {
    push((struct value){value, INT64_T, false});
}

void push_heap_address(value_t heap_address) {
    push((struct value){heap_address, HEAP_ADDRESS, false});
}

// TODO: values used as adresses are implicitly cast, and conversely
value_t pop_value() {
    struct value v = pop();
    if (v.type != INT64_T) { // TODO: should we type check?
        fprintf(stderr, "Type error\n");
        exit(-1);
    }
    return v.value;
}

value_t pop_heap_address() {
    struct value v = pop();
    if (v.type != HEAP_ADDRESS) { // TODO: should we type check?
        fprintf(stderr, "Type error\n");
        exit(-1);
    }
    return v.value;
}

#else

void push_value(value_t value) { push((struct value){value}); }

value_t pop_value() { return pop().value; }

#endif /* BEFUNGE93PLUS */
