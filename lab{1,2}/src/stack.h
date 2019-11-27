#pragma once

#include <stdbool.h>
#include <sys/types.h>

// typedef struct {
// #ifdef BEFUNGE93PLUS
//     int64_t value : 62;
//     enum type { INT64_T,
//                 HEAP_ADDRESS } type : 1;
//     bool marked : 1;
// #else
//     long value;
// #endif
// } value_t;

#ifdef BEFUNGE93PLUS
typedef struct {
    int64_t value : 62;
    enum type { INT64_T,
                HEAP_ADDRESS } type : 1;
    bool marked : 1;
} value_t;
#else
typedef long value_t;
#endif

void init_stack();
#ifdef BEFUNGE93PLUS
void push_value(int64_t);
void push_heap_address(int64_t);
value_t pop();
long pop_value();
long pop_heap_address();
#else
void push_value(long);
long pop_value();
#endif
void free_stack();
void print_stack();

#ifdef BEFUNGE93PLUS
void mark();
#endif
