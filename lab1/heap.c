
struct cons_cell {
    long head;
    long tail;
};

#define HEAP_SIZE (1024 * 1024)
struct heap {
    long top; // TODO: size_t
    struct cons_cell cons_cells[HEAP_SIZE];
} heap;

void gc() {
}

long allocate(long head, long tail) {
    if (heap.top == HEAP_SIZE)
        gc();
    struct cons_cell cc = {.head = head, .tail = tail};
    heap.cons_cells[heap.top] = cc;
    return heap.top++;
}

long head(long address) {
    return heap.cons_cells[address].head;
}

long tail(long address) {
    return heap.cons_cells[address].tail;
}