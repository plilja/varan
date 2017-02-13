#include "memory.h"
#include "stdlib.h"
#include "stdio.h"
#include "string.h"

struct Type types[1024];
int num_types = 0;

int LIMIT = 1024 * 1024;
void* active;
void* passive;
void* free_ptr;
void* stack_start;
void* stack;
size_t ptr_size;

void add_type(struct Type t) {
    types[num_types] = t;
    num_types++;
}

void init() {
    active = malloc(LIMIT);
    passive = malloc(LIMIT);
    stack = malloc(LIMIT);
    stack_start = stack;
    free_ptr = active;
    ptr_size = sizeof(struct Type*);
    /*
    printf("stack start = %p stack end = %p\n", stack_start, stack_start + LIMIT);
    printf("active start = %p active end = %p\n", active, active + LIMIT);
    printf("passive start = %p passive end = %p\n", passive, passive + LIMIT);
    */
}

void tear_down() {
    free(active);
    free(passive);
}

int copy(void *dest, void *src) {
    // TODO if src is already in target space, then don't copy
    struct Type *type = &types[*((int*) src)];
    memcpy(dest, src, type->size);
    return type->size;
}

/**
 * Cheney's algorithm
 */
void garbage_collect() {
    free_ptr = passive;

    // Copy everything that's on the stack to the new active space
    void *t1 = stack_start;
    while (t1 < stack) {
        int d = copy(free_ptr, *((void**)t1));
        memcpy(t1, &free_ptr, ptr_size);
        t1 += ptr_size;
        free_ptr += d;
    }

    // Copy everything that points to the old space from 
    // the new active space
    void *t2 = passive;
    while (t2 < free_ptr) {
        struct Type *type = &types[*((int*) t2)];
        for (int i = 0; i < type->num_fields; ++i) {
            void** fieldPtr = (void**) (t2 + sizeof(int*) + (i * ptr_size));
            int d = copy(free_ptr, *fieldPtr);
            memcpy(fieldPtr, &free_ptr, ptr_size);
            free_ptr += d;
        }
        t2 += type->size;
    }

    // Swap active and passive space
    void* tmp = active;
    active = passive;
    passive = tmp;
}

void* alloc(size_t size) {
    if (free_ptr + size > active + LIMIT) {
        garbage_collect();
    }
    void* res = free_ptr;
    free_ptr += size;
    return res;
}

void* stack_push(void *p) {
    memcpy(stack, &p, ptr_size);
    void* res = stack;
    stack += ptr_size;
    return res;
}

void stack_reset(void *p) {
    stack = p;
}

void* get_stack() {
    return stack;
}

