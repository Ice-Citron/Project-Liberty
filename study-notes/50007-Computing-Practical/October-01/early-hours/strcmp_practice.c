#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[]) {
    int cmp_value = strcmp(argv[1], argv[2]);
    if (cmp_value < 0) {
        printf("%s is bigger than %s\n", argv[2], argv[1]);
    } else if (cmp_value > 0) {
        printf("%s is bigger than %s\n", argv[1], argv[2]);
    } else {
        printf("The two strings are of the same value.\n");
    }
    return 0;
}
