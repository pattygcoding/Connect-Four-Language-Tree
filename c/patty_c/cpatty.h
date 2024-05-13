#ifndef CPATTY_H
#define CPATTY_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef struct {
    char *data;
    size_t length;
    size_t capacity;
} String;

void estring(String *str) {
    str->data = (char*)malloc(1);
    str->data[0] = '\0';
    str->length = 0;
    str->capacity = 1;
}

void string(String *str, const char *cstr) {
    str->length = strlen(cstr);
    str->capacity = str->length + 1;
    str->data = (char*)malloc(str->capacity);
    strcpy(str->data, cstr);
}

void string_free(String *str) {
    free(str->data);
    str->data = NULL;
    str->length = 0;
    str->capacity = 0;
}

int string_length(const String *str) {
    return str->length;
}

void string_add(String *str, const char *cstr) {
    size_t newLength = str->length + strlen(cstr);
    if (newLength >= str->capacity) {
        str->capacity = newLength + 1;
        str->data = (char *)realloc(str->data, str->capacity);
    }
    strcat(str->data, cstr);
    str->length = newLength;
}

void string_addc(String *str, char c) {
    if (str->length + 1 >= str->capacity) {
        str->capacity = str->length + 2;
        str->data = (char *)realloc(str->data, str->capacity);
    }
    str->data[str->length] = c;
    str->data[str->length + 1] = '\0';
    str->length += 1;
}

void string_print(const String *str) {
    printf("%s", str->data);
}

void string_println(const String *str) {
    printf("%s\n", str->data);
}


#endif 