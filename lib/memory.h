#ifndef _RUNTIME
#define _RUNTIME

#include "stdlib.h"
#include "stdio.h"
#include "string.h"

struct _Type {
    int size;
    int num_fields;
};

void _add_type(struct _Type t);
void _init();
void _tear_down();

void* _alloc(size_t size);
void* _stack_push(void *p);
void _stack_reset(void *p);
void* _get_stack();

#endif
