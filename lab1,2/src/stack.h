#pragma once

#include <stdbool.h>
#include <sys/types.h>

#ifdef BEFUNGE93PLUS
typedef int64_t value_t;
struct value {
    // int64_t value : 62;
    // enum type {
    //     INT64_T,
    //     HEAP_ADDRESS
    // } type : 1;
    // bool marked : 1;
    value_t value;
    enum {
        INT64_T,
        HEAP_ADDRESS
    } type;
    bool marked;
};
#else
typedef long value_t;
struct value {
    value_t value;
};
#endif

void init_stack();
void push(struct value);
struct value pop();
void push_value(value_t);
value_t pop_value();
#ifdef BEFUNGE93PLUS
void push_heap_address(value_t);
value_t pop_heap_address();
#endif
void free_stack();
void print_stack();

#ifdef BEFUNGE93PLUS
void mark();
#endif
