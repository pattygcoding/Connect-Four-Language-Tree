#include <stdio.h>
#include "cpatty.h"

/**
    This is not officaly part of the connect four program, I am testing
    my own library in C.
*/
int main() {
   
    String str; estring(&str);
    string_add(&str, "Hello, ");
    string_add(&str, "World!");
    string_println(&str);

    string_addc(&str, ' ');
    string_add(&str, "This is a dynamic string in C.");
    string_println(&str);

    string_free(&str);

    String str2; string(&str2, "Initial String");
    string_println(&str2);
    string_free(&str2);

    return 0;
}
