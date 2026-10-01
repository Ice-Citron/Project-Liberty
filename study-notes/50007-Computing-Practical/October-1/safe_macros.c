#include <stdio.h>

#define SQUARE(x) ((x) * (x))
#define SWAP_INT(a, b) \
    do { \
        int temp = a; \
        a = b; \
        b = temp; \
    } while(0)

int main(int argc, char *argv[]) {
    int left  = 10;
    int right = 50;
    if (left < right) {
        SWAP_INT(left, right);
        printf("Swap happened\n");
    } else {
        printf("No swap\n");
    }
    printf("The squared number is: %d\n", SQUARE(3));
    return 0;
}
