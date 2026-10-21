#include <stdio.h>

#define INT_SWAP(a, b) \
    do { \
        int temp = a; \
        a = b; \
        b = temp; \
    } while(0)

int main(void) {
    int value_1;
    int value_2;
    if (scanf("Numbers: %d %d", &value_1, &value_2) != 2) {
        printf("Input read failed, terminating program now...\n");
    }
    printf("value_1 = %d\n", value_1);
    printf("value_2 = %d\n", value_2);
    INT_SWAP(value_1, value_2);
}
