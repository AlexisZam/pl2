#pragma once

#include "types.h"

void init_heap();
void print_heap();
struct value allocate(struct value, struct value);
struct value head(struct value);
struct value tail(struct value);
