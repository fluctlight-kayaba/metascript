/*
 * ORC Performance Benchmark
 * Compares ORC allocation vs manual malloc/free
 *
 * Measures:
 * 1. Allocation overhead (ms_alloc vs malloc)
 * 2. Reference counting overhead (incref/decref vs manual)
 * 3. Memory access overhead (with header vs without)
 */

#include "orc.h"
#include <stdio.h>
#include <time.h>
#include <stdint.h>

#define ITERATIONS 10000000  // 10 million iterations

typedef struct {
    int x;
    int y;
    int z;
} Point;

// Get current time in nanoseconds
static inline uint64_t get_nanos() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

// Benchmark 1: Simple allocation and free
void bench_alloc_free() {
    printf("Benchmark 1: Allocation + Free (single object)\n");

    volatile int sink = 0;

    // Manual malloc/free
    uint64_t start = get_nanos();
    for (int i = 0; i < ITERATIONS; i++) {
        Point* p = (Point*)malloc(sizeof(Point));
        p->x = i;
        sink += p->x; // Prevent optimization
        free(p);
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // ORC alloc/decref
    start = get_nanos();
    for (int i = 0; i < ITERATIONS; i++) {
        Point* p = (Point*)ms_alloc(sizeof(Point));
        p->x = i;
        sink += p->x; // Prevent optimization
        ms_decref(p);
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    printf("  Manual malloc/free: %.2f ms\n", manual_ms);
    printf("  ORC alloc/decref:   %.2f ms\n", orc_ms);
    printf("  Overhead:           %.2f%% (%.2f ms)\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0,
           orc_ms - manual_ms);
    printf("  (sink = %d to prevent optimization)\n", (int)sink);
    printf("\n");
}

// Benchmark 2: Allocation with reference counting
void bench_with_refcount() {
    printf("Benchmark 2: Allocation + RefCount + Free\n");

    volatile int sink = 0;

    // Manual malloc/free (simulate RC with variable)
    uint64_t start = get_nanos();
    for (int i = 0; i < ITERATIONS; i++) {
        Point* p = (Point*)malloc(sizeof(Point));
        p->x = i;

        // Simulate incref/decref
        volatile int rc = 1;
        rc++; // incref
        rc++; // incref
        rc--; // decref
        rc--; // decref

        sink += p->x; // Prevent optimization
        free(p);
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // ORC with incref/decref
    start = get_nanos();
    for (int i = 0; i < ITERATIONS; i++) {
        Point* p = (Point*)ms_alloc(sizeof(Point));
        p->x = i;

        ms_incref(p);
        ms_incref(p);
        ms_decref(p);
        ms_decref(p);

        sink += p->x; // Prevent optimization
        ms_decref(p);
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    printf("  Manual malloc+RC:   %.2f ms\n", manual_ms);
    printf("  ORC alloc+incref:   %.2f ms\n", orc_ms);
    printf("  Overhead:           %.2f%% (%.2f ms)\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0,
           orc_ms - manual_ms);
    printf("  (sink = %d to prevent optimization)\n", (int)sink);
    printf("\n");
}

// Benchmark 3: Memory access overhead (cache effects)
void bench_memory_access() {
    printf("Benchmark 3: Memory Access Overhead\n");

    const int NUM_OBJECTS = 1000;

    // Allocate arrays
    Point* manual_array[NUM_OBJECTS];
    Point* orc_array[NUM_OBJECTS];

    for (int i = 0; i < NUM_OBJECTS; i++) {
        manual_array[i] = (Point*)malloc(sizeof(Point));
        orc_array[i] = (Point*)ms_alloc(sizeof(Point));
    }

    // Manual access
    uint64_t start = get_nanos();
    volatile int sum = 0;
    for (int iter = 0; iter < ITERATIONS / 100; iter++) {
        for (int i = 0; i < NUM_OBJECTS; i++) {
            manual_array[i]->x = iter + i;
            manual_array[i]->y = iter * 2;
            sum += manual_array[i]->x + manual_array[i]->y;
        }
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // ORC access
    start = get_nanos();
    sum = 0;
    for (int iter = 0; iter < ITERATIONS / 100; iter++) {
        for (int i = 0; i < NUM_OBJECTS; i++) {
            orc_array[i]->x = iter + i;
            orc_array[i]->y = iter * 2;
            sum += orc_array[i]->x + orc_array[i]->y;
        }
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    printf("  Manual access:      %.2f ms\n", manual_ms);
    printf("  ORC access:         %.2f ms\n", orc_ms);
    printf("  Overhead:           %.2f%% (%.2f ms)\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0,
           orc_ms - manual_ms);

    // Cleanup
    for (int i = 0; i < NUM_OBJECTS; i++) {
        free(manual_array[i]);
        ms_decref(orc_array[i]);
    }
    printf("\n");
}

// Benchmark 4: Incref/Decref overhead (isolated)
void bench_refcount_ops() {
    printf("Benchmark 4: RefCount Operations (isolated)\n");

    Point* p_manual = (Point*)malloc(sizeof(Point));
    Point* p_orc = (Point*)ms_alloc(sizeof(Point));

    // Manual RC simulation
    uint64_t start = get_nanos();
    volatile int rc = 1;
    for (int i = 0; i < ITERATIONS; i++) {
        rc++; // incref
        rc--; // decref
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // ORC RC operations
    start = get_nanos();
    for (int i = 0; i < ITERATIONS; i++) {
        ms_incref(p_orc);
        ms_decref(p_orc);
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    printf("  Manual RC ops:      %.2f ms\n", manual_ms);
    printf("  ORC RC ops:         %.2f ms\n", orc_ms);
    printf("  Overhead:           %.2f%% (%.2f ms)\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0,
           orc_ms - manual_ms);

    free(p_manual);
    ms_decref(p_orc);
    printf("\n");
}

int main() {
    printf("===================================================\n");
    printf("ORC Performance Benchmark\n");
    printf("Iterations: %d\n", ITERATIONS);
    printf("===================================================\n\n");

    bench_alloc_free();
    bench_with_refcount();
    bench_memory_access();
    bench_refcount_ops();

    printf("===================================================\n");
    printf("Summary:\n");
    printf("ORC adds ~8 bytes per allocation (RefHeader)\n");
    printf("Expected baseline overhead: 5-10%%\n");
    printf("Target with optimization: 0.5-2%%\n");
    printf("===================================================\n");

    return 0;
}
