#include <stdio.h>
#include <inttypes.h>

int main(int argc, char **argv) {
    uint64_t number = -1;
    printf("The 64-bit unsigned number in question is: %" PRIu64 "\n", number);
    printf("Similarly, the max uint64_t is: %" PRIu64 "\n", UINT64_MAX);
}
