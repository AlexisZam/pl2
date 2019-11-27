#include "stack.h"
#include <stdio.h>
#include <stdlib.h>

struct cons_cell {
    struct value head;
    struct value tail;
};

/* Global variables */

int freelist = 0;
#define HEAP_SIZE 10
struct cons_cell heap[HEAP_SIZE];

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

void init_heap() {
    for (int i = 0; i < HEAP_SIZE; i++)
        heap[i].head.value = i + 1;
}

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
    if (freelist == HEAP_SIZE - 1) {
#ifdef DYNAMIC_HEAP
        // TODO: realloc
#else
        fprintf(stderr, "Heap overflow\n");
        exit(-1);
#endif /* DYNAMIC_HEAP */
    }
}

struct value allocate(struct value head, struct value tail) {
    if (freelist == HEAP_SIZE - 1)
        gc();
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

// TODO: copying