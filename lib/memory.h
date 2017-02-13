#ifndef _RUNTIME
#define _RUNTIME

#include "stdlib.h"
#include "stdio.h"
#include "string.h"

struct Type {
    int size;
    int num_fields;
};

void add_type(struct Type t);
void init();
void tear_down();

int copy(void *dest, void *src);
void garbage_collect();

void* alloc(size_t size);
void* stack_push(void *p);
void stack_reset(void *p);
void* get_stack();

#endif
