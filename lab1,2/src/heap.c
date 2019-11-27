#include "stack.h"
#include <stdio.h>

struct cons_cell {
    value_t head;
    value_t tail;
};

#ifdef MARK_AND_SWEEP

#define HEAP_SIZE 1024 * 1024
struct cons_cell heap[HEAP_SIZE];

int64_t freelist = 0;

void sweep() {
    for (int i = 0; i < HEAP_SIZE; i++) {
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
    if (freelist == HEAP_SIZE) {
#ifdef DYNAMIC_HEAP
        // TODO: realloc
#else /* STATIC_HEAP */
        fprintf(stderr, "Heap out of memory");
#endif
    }
}

int64_t allocate(value_t head, value_t tail) {
    if (freelist == HEAP_SIZE)
        gc();
    int64_t address = freelist;
    freelist = heap[freelist].head.value;
    heap[freelist] = (struct cons_cell){head, tail};
    return address;
}

value_t head(int64_t address) {
    return heap[address].head;
}

value_t tail(int64_t address) {
    return heap[address].tail;
}

#elif defined COPYING

// TODO

#endif