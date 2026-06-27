#ifndef UTILS_H
#define UTILS_H

#include <stdlib.h>
#include <string.h>

/* 自定义 strdup 替代函数，使用 malloc 和 strcpy */
static inline char* my_strdup(const char* s) {
    if (!s) return NULL;
    size_t len = strlen(s) + 1;
    char* d = (char*)malloc(len);
    if (d) {
        memcpy(d, s, len);
    }
    return d;
}

#endif /* UTILS_H */