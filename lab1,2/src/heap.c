#include "stack.h"
#include <stdio.h>
#include <stdlib.h>

struct cons_cell {
    struct value head;
    struct value tail;
};

/* Global variables */

int freelist = 0;
const int heap_size = HEAP_SIZE;
struct cons_cell heap[HEAP_SIZE];

void DFS(struct value *x) { // TODO: improve DFS
    if (!x->marked) {
        x->marked = true;
        if (x->pointer) {
            DFS(&heap[x->value].head);
            DFS(&heap[x->value].tail);
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
    for (int i = 0; i < heap_size; i++)
        heap[i].head.value = i + 1;
}

struct value allocate(struct value head, struct value tail) {
    if (freelist == heap_size) {
        push(head);
        push(tail);
        gc();
        // printf("GC\n");
        pop();
        pop();
        if (freelist == heap_size) {
            fprintf(stderr, "Heap overflow\n");
            exit(EXIT_FAILURE);
        }
    }
    int temp = freelist;
    freelist = heap[freelist].head.value;
    heap[temp] = (struct cons_cell){head, tail};
    return (struct value){temp, true, false};
}

struct value head(struct value address) {
    return heap[address.value].head;
}

struct value tail(struct value address) {
    return heap[address.value].tail;
}

void print_heap() {
#ifdef PRINT_HEAP
    int temp = freelist;
    dprintf(HEAP_FD, "(((%d))) ", temp);
    for (int i = 0; i < heap_size; i++)
        if (i == temp) {
            dprintf(HEAP_FD, "(((%d))) ", temp);
            temp = heap[temp].head.value;
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