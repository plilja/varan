#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "memory.h"
#include "assert.h"
#include "osfuncs.h"

void print(struct String **s) {
    printf("%s\n", (*s)->str);
}

void printInt(int i) {
    printf("%d\n", i);
}

struct String _intToString(int a) {
    struct String s;
    sprintf(s.str, "%d", a);
    return s;
}

struct String** _makeString(char *s) {
    struct String **res = (struct String**) _stack_push(_alloc(sizeof(struct String)));
    strcpy((*res)->str, s);
    return res;
}

