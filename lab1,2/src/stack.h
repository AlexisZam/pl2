#pragma once

#include "types.h"

void init_stack();
void push(struct value);
struct value pop();
void print_stack();

#ifdef BEFUNGE93PLUS
void mark();
#endif
