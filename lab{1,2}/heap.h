#pragma once

#include "stack.h"
#include <sys/types.h>

int64_t allocate(int64_t, int64_t);
value_t head(int64_t);
value_t tail(int64_t);
