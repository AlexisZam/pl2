#pragma once

#include "types.h"

void init_heap();
static inline struct value allocate(struct value, struct value);
static inline struct value head(struct value);
static inline struct value tail(struct value);
void print_heap();

void DFS();
