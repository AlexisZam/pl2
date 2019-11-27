#pragma once

#include <stdbool.h>
#include <sys/types.h>

#ifdef BEFUNGE93PLUS
typedef int64_t long_t;
struct value {
    // int64_t value : 62;
    // enum type {
    //     INT64_T,
    //     HEAP_ADDRESS
    // } type : 1;
    // bool marked : 1;
    long_t value;
    enum {
        INT64_T,
        HEAP_ADDRESS
    } type;
    bool marked;
};
#else
typedef long long_t;
struct value {
    long_t value;
};
#endif
