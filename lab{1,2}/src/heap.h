#pragma once

#include "stack.h"
#include <sys/types.h>

int64_t allocate(value_t, value_t);
value_t head(int64_t);
value_t tail(int64_t);
