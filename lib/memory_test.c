#include "memory.h"
#include "stdio.h"
#include "assert.h"

struct IntType {
    int type;
    int v;
};

struct DoubleType {
    int type;
    double v;
};

struct A {
    int type;
    struct DoubleType *d;
    struct IntType *i;
};

void register_types() {
    struct Type int_type;
    int_type.num_fields = 0;
    int_type.size = sizeof(struct IntType);
    add_type(int_type);

    struct Type double_type;
    double_type.num_fields = 0;
    double_type.size = sizeof(struct DoubleType);
    add_type(double_type);

    struct Type a_type;
    a_type.num_fields = 2;
    a_type.size = sizeof(struct A);
    add_type(a_type);
}

int main() {
    init();
    register_types();
    void* s = get_stack();

    // Allocate some data and store stack pointers to that data
    struct IntType **i = (struct IntType**) stack_push(alloc(sizeof(struct IntType)));
    (*i)->type = 0;
    struct A **a = (struct A**) stack_push(alloc(sizeof(struct A)));
    (*a)->type = 2;
    (*a)->i = *((struct IntType**)stack_push(alloc(sizeof(struct IntType))));
    (*a)->i->type = 0;
    (*a)->d = *((struct DoubleType**)stack_push(alloc(sizeof(struct DoubleType))));
    (*a)->d->type = 1;

    // Set values
    (*i)->v = 4;
    (*a)->d->v = 7;
    (*a)->i->v = 8;

    // Allocate a lot of data to force garbage collection to take place
    int values_have_been_moved = 0;
    struct IntType *iBefore = *i;
    for (int j = 0; j < 10000000; ++j) {
        // Allocate data without putting it on the stack
        alloc(sizeof(struct IntType));

        // Verify values are preserved
        assert((*i)->v == 4);
        assert((*a)->i->v == 8);
        assert((*a)->d->v == 7);

        values_have_been_moved = values_have_been_moved || (iBefore != *i);
    }

    assert(values_have_been_moved); // Check that garbage collection was performed at least once

    puts("Success");

    stack_reset(s);
    tear_down();
}
