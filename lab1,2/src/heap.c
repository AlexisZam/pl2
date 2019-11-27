#include "stack.h"
#include <stdio.h>
#include <stdlib.h>

struct cons_cell {
    struct value head;
    struct value tail;
};

#define HEAP_SIZE 10
struct {
    int freelist;
    struct cons_cell cons_cells[HEAP_SIZE];
} heap = {0};

#define HEAP_FD 4
void print_heap() {
#ifdef PRINT_HEAP
    int temp = heap.freelist;
    for (int i = 0; i < HEAP_SIZE; i++)
        if (i == temp)
            temp = heap.cons_cells[temp].head.value;
        else
            dprintf(HEAP_FD, "(%ld:%d,%ld:%d) ",
                    heap.cons_cells[i].head.value, heap.cons_cells[i].head.type,
                    heap.cons_cells[i].tail.value, heap.cons_cells[i].tail.type);
    dprintf(HEAP_FD, "\n");
    sleep(3);
#endif /* PRINT_HEAP */
}

void init_heap() {
    for (int i = 0; i < HEAP_SIZE; i++)
        heap.cons_cells[i].head.value = i + 1;
}

void sweep() {
    for (int i = HEAP_SIZE - 1; i >= 0; i--) {
        if (heap.cons_cells[i].head.marked)
            heap.cons_cells[i].head.marked = heap.cons_cells[i].tail.marked = false;
        else {
            heap.cons_cells[i].head.value = heap.freelist;
            heap.freelist = i;
        }
    }
}

void gc() {
    mark();
    sweep();
    if (heap.freelist == HEAP_SIZE - 1) {
#ifdef DYNAMIC_HEAP
        // TODO: realloc
#else
        fprintf(stderr, "Heap overflow\n");
        exit(-1);
#endif /* DYNAMIC_HEAP */
    }
}

int allocate(struct value head, struct value tail) {
    if (heap.freelist == HEAP_SIZE - 1)
        gc();
    int64_t address = heap.freelist;
    heap.freelist = heap.cons_cells[heap.freelist].head.value;
    heap.cons_cells[heap.freelist] = (struct cons_cell){head, tail};
    return address;
}

struct value head(int address) {
    return heap.cons_cells[address].head;
}

struct value tail(int address) {
    return heap.cons_cells[address].tail;
}

// TODO: copying