/*
 * Metascript String Performance Benchmark
 * Compares msString (with ORC) vs manual C strings
 */

#include "ms_string.h"
#include <stdio.h>
#include <time.h>
#include <stdint.h>

#define WARM_UP_ITERATIONS 1000
#define ITERATIONS 1000000

static inline uint64_t get_nanos() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

// Benchmark 1: String creation and destruction
void bench_string_new_free() {
    printf("Benchmark 1: String Creation + Free\n");
    printf("  %d iterations\n", ITERATIONS);

    volatile int sink = 0;

    // Manual malloc/free
    uint64_t start = get_nanos();
    for (int i = 0; i < ITERATIONS; i++) {
        char* s = (char*)malloc(12); // "hello world" + null
        strcpy(s, "hello world");
        sink += s[0];
        free(s);
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // msString alloc/decref
    start = get_nanos();
    for (int i = 0; i < ITERATIONS; i++) {
        msString* s = ms_string_new("hello world", 11);
        sink += s->data[0];
        ms_string_decref(s);
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    printf("  Manual: %.2f ms (%.2f ns/iter)\n",
           manual_ms, (double)manual_ns / ITERATIONS);
    printf("  msString: %.2f ms (%.2f ns/iter)\n",
           orc_ms, (double)orc_ns / ITERATIONS);
    printf("  Overhead: %.2f%%\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0);
    printf("  (sink = %d)\n\n", (int)sink);
}

// Benchmark 2: String concatenation
void bench_string_concat() {
    printf("Benchmark 2: String Concatenation\n");
    printf("  %d iterations\n", ITERATIONS);

    volatile int sink = 0;

    // Manual concat (malloc + strcpy)
    uint64_t start = get_nanos();
    for (int i = 0; i < ITERATIONS; i++) {
        const char* a = "hello";
        const char* b = " world";
        size_t len_a = 5;
        size_t len_b = 6;

        char* result = (char*)malloc(len_a + len_b + 1);
        memcpy(result, a, len_a);
        memcpy(result + len_a, b, len_b);
        result[len_a + len_b] = '\0';

        sink += result[0];
        free(result);
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // msString concat
    start = get_nanos();
    for (int i = 0; i < ITERATIONS; i++) {
        msString* a = ms_string_new("hello", 5);
        msString* b = ms_string_new(" world", 6);
        msString* result = ms_string_concat(a, b);

        sink += result->data[0];

        ms_string_decref(a);
        ms_string_decref(b);
        ms_string_decref(result);
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    printf("  Manual: %.2f ms\n", manual_ms);
    printf("  msString: %.2f ms\n", orc_ms);
    printf("  Overhead: %.2f%%\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0);
    printf("  (sink = %d)\n\n", (int)sink);
}

// Benchmark 3: String substring extraction
void bench_string_substring() {
    printf("Benchmark 3: Substring Extraction\n");
    printf("  %d iterations\n", ITERATIONS);

    volatile int sink = 0;

    // Manual substring (malloc + memcpy)
    uint64_t start = get_nanos();
    for (int i = 0; i < ITERATIONS; i++) {
        const char* str = "hello world, this is a test";
        size_t start_idx = 0;
        size_t end_idx = 5;
        size_t len = end_idx - start_idx;

        char* sub = (char*)malloc(len + 1);
        memcpy(sub, str + start_idx, len);
        sub[len] = '\0';

        sink += sub[0];
        free(sub);
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // msString substring
    start = get_nanos();
    for (int i = 0; i < ITERATIONS; i++) {
        msString* str = ms_string_new("hello world, this is a test", 27);
        msString* sub = ms_string_substring(str, 0, 5);

        sink += sub->data[0];

        ms_string_decref(str);
        ms_string_decref(sub);
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    printf("  Manual: %.2f ms\n", manual_ms);
    printf("  msString: %.2f ms\n", orc_ms);
    printf("  Overhead: %.2f%%\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0);
    printf("  (sink = %d)\n\n", (int)sink);
}

// Benchmark 4: String equality comparison
void bench_string_equals() {
    printf("Benchmark 4: String Equality\n");
    printf("  %d iterations\n", ITERATIONS * 10);

    volatile int sink = 0;

    // Manual strcmp
    uint64_t start = get_nanos();
    for (int i = 0; i < ITERATIONS * 10; i++) {
        const char* a = "hello world";
        const char* b = "hello world";
        int eq = (strcmp(a, b) == 0);
        sink += eq;
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // msString equals (amortize string creation cost)
    msString* a = ms_string_new("hello world", 11);
    msString* b = ms_string_new("hello world", 11);

    start = get_nanos();
    for (int i = 0; i < ITERATIONS * 10; i++) {
        bool eq = ms_string_equals(a, b);
        sink += eq;
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    ms_string_decref(a);
    ms_string_decref(b);

    printf("  Manual: %.2f ms\n", manual_ms);
    printf("  msString: %.2f ms\n", orc_ms);
    printf("  Overhead: %.2f%%\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0);
    printf("  (sink = %d)\n\n", (int)sink);
}

// Benchmark 5: Reference counting overhead (shared ownership)
void bench_string_refcount() {
    printf("Benchmark 5: Reference Counting (shared ownership)\n");
    printf("  %d iterations\n", ITERATIONS);

    volatile int sink = 0;

    // Manual (simulate RC with variable)
    uint64_t start = get_nanos();
    for (int i = 0; i < ITERATIONS; i++) {
        char* s = (char*)malloc(12);
        strcpy(s, "hello world");

        // Simulate 3 owners
        volatile int rc = 1;
        rc++; // Owner 1
        rc++; // Owner 2
        rc++; // Owner 3

        sink += s[0];

        // Simulate releases
        rc--;
        rc--;
        rc--;
        rc--;

        free(s);
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // msString with RC operations
    start = get_nanos();
    for (int i = 0; i < ITERATIONS; i++) {
        msString* s = ms_string_new("hello world", 11);

        // 3 owners
        ms_string_incref(s); // Owner 1
        ms_string_incref(s); // Owner 2
        ms_string_incref(s); // Owner 3

        sink += s->data[0];

        // Releases
        ms_string_decref(s);
        ms_string_decref(s);
        ms_string_decref(s);
        ms_string_decref(s); // Freed
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    printf("  Manual: %.2f ms\n", manual_ms);
    printf("  msString: %.2f ms\n", orc_ms);
    printf("  Overhead: %.2f%%\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0);
    printf("  (sink = %d)\n\n", (int)sink);
}

int main() {
    printf("===================================================\n");
    printf("Metascript String Performance Benchmark\n");
    printf("===================================================\n\n");

    bench_string_new_free();
    bench_string_concat();
    bench_string_substring();
    bench_string_equals();
    bench_string_refcount();

    printf("===================================================\n");
    printf("Summary:\n");
    printf("- Week 3 target: <10%% overhead\n");
    printf("- String operations include RefHeader (8 bytes)\n");
    printf("- UTF-8 validation ensures safe operations\n");
    printf("- ORC enables safe shared ownership\n");
    printf("===================================================\n");

    return 0;
}
