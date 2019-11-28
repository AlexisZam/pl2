#pragma once

#include <stdbool.h>
#include <sys/types.h>

#ifdef BEFUNGE93PLUS
typedef int64_t long_t;
struct value {
    int64_t value : 62;
    bool pointer : 1;
    bool marked : 1;
};
#else
typedef long long_t;
struct value {
    long_t value;
};
#endif
