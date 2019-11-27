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
#define HEAP_SIZE 10 // TODO: fix heap_size
const int heap_size = HEAP_SIZE;
struct cons_cell heap[HEAP_SIZE];
#endif               /* DYNAMIC_HEAP */

void sweep() {
    for (int i = HEAP_SIZE - 1; i >= 0; i--) {
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
#define INIT_SIZE
    heap_size = INIT_SIZE;
    stack = malloc(heap_size * sizeof(struct value));
#endif /* DYNAMIC_HEAP */
    for (int i = 0; i < heap_size; i++)
        heap[i].head.value = i + 1;
}

struct value allocate(struct value head, struct value tail) {
    if (freelist == heap_size - 1) {
        gc();
        if (freelist == heap_size - 1) {
#ifdef DYNAMIC_HEAP
            heap_size *= 2;
            stack = realloc(stack, heap_size * sizeof(struct cons_cell));
#else
            fprintf(stderr, "Heap overflow\n");
            exit(-1);
#endif /* DYNAMIC_HEAP */
        }
    }
    struct value address = {freelist, HEAP_ADDRESS, false};
    freelist = heap[freelist].head.value;
    heap[freelist] = (struct cons_cell){head, tail};
    return address;
}

struct value head(struct value address) {
    return heap[address.value].head;
}

struct value tail(struct value address) {
    return heap[address.value].tail;
}

#define HEAP_FD 4
void print_heap() {
#ifdef PRINT_HEAP
    int temp = freelist;
    for (int i = 0; i < HEAP_SIZE; i++)
        if (i == temp)
            temp = heap[temp].head.value;
        else
            dprintf(HEAP_FD, "(%ld:%d,%ld:%d) ",
                    heap[i].head.value, heap[i].head.type,
                    heap[i].tail.value, heap[i].tail.type);
    dprintf(HEAP_FD, "\n");
    sleep(3);
#endif /* PRINT_HEAP */
}

// TODO: copying