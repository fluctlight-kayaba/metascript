#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

// Metascript ORC Runtime (compile with: -I<metascript>/src/runtime)
#define MS_ORC_IMPLEMENTATION
#include "orc.h"
#include "ms_string.h"

double merge(double left, double right) {
    double result[0] = {};
    int32_t i = 0;
    int32_t j = 0;
    while (((i < left.length) && (j < right.length))) {
        if ((left[i] < right[j])) {
            result.push(left[i]);
            i = (i + 1);
        }
        else {
            result.push(right[j]);
            j = (j + 1);
        }
    }
    while ((i < left.length)) {
        result.push(left[i]);
        i = (i + 1);
    }
    while ((j < right.length)) {
        result.push(right[j]);
        j = (j + 1);
    }
    
    // scope cleanup start
    // scope cleanup end
    // scope cleanup start
    // scope cleanup end
    return result;
}
double mergeSort(double arr) {
    if ((arr.length <= 1)) {
        
        return arr;
    }
    double mid = floor((arr.length / 2));
    void* left = arr.slice(0, mid);
    void* right = arr.slice(mid);
    
    // scope cleanup start
    // scope cleanup end
    // scope cleanup start
    // scope cleanup end
    return merge(mergeSort(left), mergeSort(right));
}
int main(void) {
    return 0;
}
