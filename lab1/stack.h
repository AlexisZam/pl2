#pragma once

extern struct stack *head;

void initStack();
void push(long);
long pop();
void freeStack();
void printStack();