#include "stdio.h"
#include "string.h"
#include "stdlib.h"

struct String {
    char str[256];
};

/*void pprint(struct String *s) {
    printf("%s\n", s->str);
}*/

void pprint(char *s) {
    printf("%s\n", s);
}

struct String intToString(int a) {
    struct String s;
    sprintf(s.str, "%d", a);
    return s;
}

struct String* makeString(char *s) {
    struct String *res = (struct String*) malloc(sizeof(struct String));
    strcpy(res->str, s);
    return res;
}
