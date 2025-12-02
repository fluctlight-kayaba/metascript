/*
 * Metascript String Realistic Performance Benchmark
 * Measures overhead in real-world string patterns
 */

#include "string.h"
#include <stdio.h>
#include <time.h>
#include <stdint.h>

#define ITERATIONS 100000

static inline uint64_t get_nanos() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

// Benchmark 1: Build comma-separated list
void bench_csv_building() {
    printf("Benchmark 1: CSV Building (repeated concatenation)\n");
    printf("  Build 100-item CSV, %d times\n", ITERATIONS);

    volatile int sum = 0;

    // Manual string building
    uint64_t start = get_nanos();
    for (int iter = 0; iter < ITERATIONS; iter++) {
        size_t capacity = 1000;
        char* result = (char*)malloc(capacity);
        size_t len = 0;

        for (int i = 0; i < 100; i++) {
            char buf[32];
            int n = snprintf(buf, 32, "%d", i);

            if (len + n + 1 >= capacity) {
                capacity *= 2;
                result = (char*)realloc(result, capacity);
            }

            memcpy(result + len, buf, n);
            len += n;

            if (i < 99) {
                result[len++] = ',';
            }
        }
        result[len] = '\0';

        sum += len;
        free(result);
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // msString building
    start = get_nanos();
    for (int iter = 0; iter < ITERATIONS; iter++) {
        msString* result = ms_string_empty();

        for (int i = 0; i < 100; i++) {
            char buf[32];
            int n = snprintf(buf, 32, "%d", i);
            msString* item = ms_string_new(buf, n);

            msString* temp = ms_string_concat(result, item);
            ms_string_decref(result);
            ms_string_decref(item);
            result = temp;

            if (i < 99) {
                msString* comma = ms_string_new(",", 1);
                temp = ms_string_concat(result, comma);
                ms_string_decref(result);
                ms_string_decref(comma);
                result = temp;
            }
        }

        sum += result->len;
        ms_string_decref(result);
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    printf("  Manual: %.2f ms\n", manual_ms);
    printf("  msString: %.2f ms\n", orc_ms);
    printf("  Overhead: %.2f%%\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0);
    printf("  (sum = %d)\n\n", (int)sum);
}

// Benchmark 2: String processing (split and process)
void bench_string_processing() {
    printf("Benchmark 2: String Processing (substring + comparison)\n");
    printf("  Process 'key=value' pairs, %d iterations\n", ITERATIONS);

    volatile int matches = 0;

    const char* input = "name=John,age=30,city=NYC,country=USA";

    // Manual processing
    uint64_t start = get_nanos();
    for (int iter = 0; iter < ITERATIONS; iter++) {
        const char* p = input;
        while (*p) {
            // Find key
            const char* key_start = p;
            while (*p && *p != '=') p++;
            size_t key_len = p - key_start;

            if (*p == '=') p++;

            // Find value
            const char* val_start = p;
            while (*p && *p != ',') p++;
            size_t val_len = p - val_start;

            if (*p == ',') p++;

            // Check if key == "age"
            if (key_len == 3 && memcmp(key_start, "age", 3) == 0) {
                matches++;
            }
        }
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // msString processing
    msString* input_str = ms_string_from_cstr("name=John,age=30,city=NYC,country=USA");

    start = get_nanos();
    for (int iter = 0; iter < ITERATIONS; iter++) {
        size_t pos = 0;
        while (pos < input_str->len) {
            // Find '='
            size_t eq_pos = pos;
            while (eq_pos < input_str->len && input_str->data[eq_pos] != '=') eq_pos++;

            msString* key = ms_string_substring(input_str, pos, eq_pos);

            if (eq_pos < input_str->len) eq_pos++;

            // Find ','
            size_t comma_pos = eq_pos;
            while (comma_pos < input_str->len && input_str->data[comma_pos] != ',') comma_pos++;

            msString* val = ms_string_substring(input_str, eq_pos, comma_pos);

            // Check if key == "age"
            if (ms_string_starts_with(key, "age", 3) && key->len == 3) {
                matches++;
            }

            ms_string_decref(key);
            ms_string_decref(val);

            pos = (comma_pos < input_str->len) ? comma_pos + 1 : input_str->len;
        }
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    ms_string_decref(input_str);

    printf("  Manual: %.2f ms\n", manual_ms);
    printf("  msString: %.2f ms\n", orc_ms);
    printf("  Overhead: %.2f%%\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0);
    printf("  (matches = %d)\n\n", (int)matches);
}

// Benchmark 3: Array of strings (allocation and access)
void bench_string_array() {
    printf("Benchmark 3: String Array (bulk alloc + sort + free)\n");
    printf("  1000 strings, %d iterations\n", ITERATIONS / 10);

    const int ARRAY_SIZE = 1000;
    volatile int sum = 0;

    // Manual string array
    uint64_t start = get_nanos();
    for (int iter = 0; iter < ITERATIONS / 10; iter++) {
        char** strings = (char**)malloc(ARRAY_SIZE * sizeof(char*));

        // Allocate
        for (int i = 0; i < ARRAY_SIZE; i++) {
            char buf[64];
            snprintf(buf, 64, "string_%d", i);
            strings[i] = strdup(buf);
            sum += strlen(strings[i]);
        }

        // Sort (bubble sort, simple)
        for (int i = 0; i < ARRAY_SIZE - 1; i++) {
            for (int j = 0; j < ARRAY_SIZE - i - 1; j++) {
                if (strcmp(strings[j], strings[j + 1]) > 0) {
                    char* temp = strings[j];
                    strings[j] = strings[j + 1];
                    strings[j + 1] = temp;
                }
            }
        }

        // Free
        for (int i = 0; i < ARRAY_SIZE; i++) {
            free(strings[i]);
        }
        free(strings);
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // msString array
    start = get_nanos();
    for (int iter = 0; iter < ITERATIONS / 10; iter++) {
        msString** strings = (msString**)malloc(ARRAY_SIZE * sizeof(msString*));

        // Allocate
        for (int i = 0; i < ARRAY_SIZE; i++) {
            char buf[64];
            int len = snprintf(buf, 64, "string_%d", i);
            strings[i] = ms_string_new(buf, len);
            sum += strings[i]->len;
        }

        // Sort (bubble sort)
        for (int i = 0; i < ARRAY_SIZE - 1; i++) {
            for (int j = 0; j < ARRAY_SIZE - i - 1; j++) {
                if (ms_string_compare(strings[j], strings[j + 1]) > 0) {
                    msString* temp = strings[j];
                    strings[j] = strings[j + 1];
                    strings[j + 1] = temp;
                }
            }
        }

        // Free
        for (int i = 0; i < ARRAY_SIZE; i++) {
            ms_string_decref(strings[i]);
        }
        free(strings);
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    printf("  Manual: %.2f ms\n", manual_ms);
    printf("  msString: %.2f ms\n", orc_ms);
    printf("  Overhead: %.2f%%\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0);
    printf("  (sum = %d)\n\n", (int)sum);
}

int main() {
    printf("===================================================\n");
    printf("Metascript String Realistic Performance Benchmark\n");
    printf("===================================================\n\n");

    bench_csv_building();
    bench_string_processing();
    bench_string_array();

    printf("===================================================\n");
    printf("Summary:\n");
    printf("- Week 3 target: <10%% overhead\n");
    printf("- Actual results show baseline overhead\n");
    printf("- Optimization opportunities:\n");
    printf("  1. String interning (reduce allocations)\n");
    printf("  2. Small string optimization (<24 bytes inline)\n");
    printf("  3. Copy-on-write for substrings\n");
    printf("  4. Compile-time string elimination\n");
    printf("===================================================\n");

    return 0;
}
