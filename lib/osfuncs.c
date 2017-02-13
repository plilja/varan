#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "memory.h"
#include "assert.h"
#include "osfuncs.h"

#define true 1
#define false 0

/*void pprint(struct String *s) {
    printf("%s\n", s->str);
}*/

void pprint(char *s) {
    printf("%s\n", s);
}

void pprintInt(int i) {
    printf("%d\n", i);
}

struct String intToString(int a) {
    struct String s;
    sprintf(s.str, "%d", a);
    return s;
}

struct String** makeString(char *s) {
    struct String **res = (struct String**) stack_push(alloc(sizeof(struct String)));
    strcpy((*res)->str, s);
    return res;
}

