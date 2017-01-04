#include "stdio.h"

struct String {
    char str[256];
}; 

void print(struct String *s) {
    printf("%s\n", s->str);
}

struct String intToString(int a) {
    struct String s;
    sprintf(s.str, "%d", a);
    return s;
}
