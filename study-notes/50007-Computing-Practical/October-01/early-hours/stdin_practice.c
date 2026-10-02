#include <stdio.h>

int main(int argc, char *argv[]) {
    int value1;
    int x = scanf("The number is: %d", &value1);
    printf("The number inputted is: %d\n", value1);
    printf("The output of `scanf` is: %d\n", x);
    return 0;
}
