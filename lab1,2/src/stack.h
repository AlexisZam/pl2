#pragma once

#include "types.h"

void init_stack();
static inline void push(struct value);
static inline struct value pop();
void print_stack();

#ifdef BEFUNGE93PLUS
void mark();
#endif
