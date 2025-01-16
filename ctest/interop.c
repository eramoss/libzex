#include "libzex.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    regex_t preg;

    const char *pattern = "^[a-zA-Z]+$"; 
		printf("starting compile...\n");
    int32_t result = regcomp(&preg, pattern, 0b001001);
    if (result != 0) {
        char error_buffer[256];
        regerror(result, &preg, error_buffer, sizeof(error_buffer));
        fprintf(stderr, "Error compiling regex: %s\n", error_buffer);
        return EXIT_FAILURE;
    }

    printf("Regular expression compiled successfully.\n");

    regfree(&preg);
    return EXIT_SUCCESS;
}

