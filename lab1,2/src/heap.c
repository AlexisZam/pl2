#include "stack.h"
#include <stdio.h>
#include <stdlib.h>

struct cons_cell {
    struct value head;
    struct value tail;
};

/* Global variables */

int freelist = 0;
#ifdef DYNAMIC_HEAP
int heap_size = 0;
struct value *stack = NULL;
#else
#define HEAP_SIZE 1024 * 1024 // TODO: fix heap_size
const int heap_size = HEAP_SIZE;
struct cons_cell heap[HEAP_SIZE];
#endif                        /* DYNAMIC_HEAP */

void DFS(struct value *value) { // TODO: improve DFS
    if (!value->marked) {
        value->marked = true;
        if (value->pointer) {
            DFS(&heap[value->value].head);
            DFS(&heap[value->value].tail);
        }
    }
}

void sweep() {
    for (int i = heap_size - 1; i >= 0; i--) {
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

void init_heap() {
#ifdef DYNAMIC_HEAP
#define INIT_SIZE 1024
    heap_size = INIT_SIZE;
    stack = malloc(heap_size * sizeof(struct value));
#endif /* DYNAMIC_HEAP */
    for (int i = 0; i < heap_size; i++)
        heap[i].head.value = i + 1;
}

struct value allocate(struct value head, struct value tail) {
    if (freelist == heap_size) {
        push(head);
        push(tail);
        gc();
        if (freelist == heap_size) {
#ifdef DYNAMIC_HEAP
            heap_size *= 2;
            stack = realloc(stack, heap_size * sizeof(struct cons_cell));
#else
            fprintf(stderr, "Heap overflow\n");
            exit(-1);
#endif /* DYNAMIC_HEAP */
        }
    }
    heap[freelist] = (struct cons_cell){head, tail};
    struct value address = {freelist, true, false};
    freelist = heap[freelist].head.value;
    return address;
}

struct value *head(struct value address) {
    return &heap[address.value].head;
}

struct value *tail(struct value address) {
    return &heap[address.value].tail;
}

#define HEAP_FD 4
void print_heap() {
#ifdef PRINT_HEAP
    int temp = freelist;
    dprintf(HEAP_FD, "(((%d))) ", temp);
    for (int i = 0; i < heap_size; i++)
        if (i == temp) {
            temp = heap[temp].head.value;
            // dprintf(HEAP_FD, "(((%d))) ", temp);
        } else {
            if (heap[i].head.marked || heap[i].tail.marked)
                dprintf(HEAP_FD, "\033[1;31m");
            dprintf(HEAP_FD, "(%ld:%d,%ld:%d) ",
                    heap[i].head.value, heap[i].head.pointer, heap[i].tail.value, heap[i].tail.pointer);
            dprintf(HEAP_FD, "\033[0m");
        }
    dprintf(HEAP_FD, "\n");
#endif /* PRINT_HEAP */
}

// TODO: copying