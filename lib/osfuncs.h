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

void print(struct String **s);

void printInt(int i);

struct String _intToString(int a);

struct String** _makeString(char *s);

#endif
