#include <stdio.h>
#include <stdlib.h>

double *make_samples(size_t count, double start, double step) {
    double *ret_array = malloc(sizeof(double) * count);
    if (ret_array == NULL) {
        perror("malloc failed");
        exit(EXIT_FAILURE);
    }
    for (int i = 0; i < count; i++) {
        ret_array[i] = start + i * step;
    }
    return ret_array;
}

double mean(const double *samples, size_t count) {
    if (samples == NULL) {
        perror("`samples` points to null");
        exit(EXIT_FAILURE);
    }
    double sum = 0;
    for (int i = 0; i < count; i++) {
        sum += samples[i];
    }
    return sum / count;
}

int main(int argc, char **argv) {
    int count = 5;
    double *arr = make_samples(count, 1.0f, 0.5f);
    for (double *ptr = arr; ptr != arr + count; ptr++) {
        printf("Value: %f\n", *ptr);
    }
    printf("\n\n");
    double mean_value = mean(arr, count);
    printf("The mean value is %f\n", mean_value);
    return 0;
}
