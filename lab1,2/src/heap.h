#pragma once

#include "stack.h"
#include <sys/types.h>

void init_heap();
void print_heap();
int allocate(struct value, struct value);
struct value head(int);
struct value tail(int);
