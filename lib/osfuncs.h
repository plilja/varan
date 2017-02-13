#ifndef _OSFUNCS
#define _OSFUNCS

#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "memory.h"
#include "assert.h"

#define true 1
#define false 0

struct String {
    char str[256];
};

/*void pprint(struct String *s) ;
    printf("%s\n", s->str);
}*/

void pprint(char *s);

void pprintInt(int i);

struct String intToString(int a);

struct String** makeString(char *s);

#endif
