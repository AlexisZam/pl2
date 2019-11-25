#include <stdio.h>
#include <stdlib.h>

#define STACK_FD 3

#ifdef LINKED_LIST

struct stack {
    long value;
    struct stack *next;
} *head = NULL;

void initStack() {}

//TODO: top of stack caching
void push(long value) {
    struct stack *s = malloc(sizeof(struct stack));
    s->value = value;
    s->next = head;
    head = s;
}

long pop() {
    if (!head)
        return 0;
    struct stack *s = head;
    long value = head->value;
    head = head->next;
    free(s);
    return value;
}

void freeStack() {
    struct stack *s;
    while (head) {
        s = head;
        head = head->next;
        free(s);
    }
}

void printStack() {
#ifdef STACK
    for (struct stack *s = head; s; s = s->next)
        dprintf(STACK_FD, "%ld ", s->value);
    dprintf(STACK_FD, "\n");
#endif /* STACK */
}

#else

struct stack {
    int size; // TODO: size_t
    int top;
    long *values;
} head = {.size = 0, .top = 0, .values = NULL};

// TODO: for (practically) static array change INIT_SIZE to a very large number
void initStack() {
#define INIT_SIZE 1024
    head.size = INIT_SIZE;
    head.top = 0;
    head.values = malloc(head.size * sizeof(long));
}

void push(long value) {
    if (head.top == head.size) {
        head.size *= 2;
        head.values = realloc(head.values, head.size * sizeof(long));
    }
    head.values[head.top++] = value;
}

long pop() {
    if (!head.top)
        return 0;
    return head.values[--head.top];
}

void freeStack() {
    head.size = 0;
    head.top = 0;
    free(head.values);
    head.values = NULL;
}

void printStack() {
#ifdef STACK
    for (int i = head.top - 1; i >= 0; i--)
        dprintf(STACK_FD, "%ld ", head.values[i]);
    dprintf(STACK_FD, "\n");
#endif /* STACK */
}

#endif
