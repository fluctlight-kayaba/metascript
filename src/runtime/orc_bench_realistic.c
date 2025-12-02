/*
 * ORC Realistic Performance Benchmark
 * Measures overhead in real-world patterns, not micro-benchmarks
 */

#include "orc.h"
#include <stdio.h>
#include <time.h>
#include <stdint.h>
#include <string.h>

#define WARM_UP_ITERATIONS 1000
#define ITERATIONS 100000

typedef struct Node_s {
    int value;
    struct Node_s* next;
} Node;

typedef struct {
    int id;
    char name[32];
    double score;
} User;

static inline uint64_t get_nanos() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

// Benchmark 1: Linked list creation and destruction
void bench_linked_list() {
    printf("Benchmark 1: Linked List (create + destroy)\n");
    printf("  Building lists of 100 nodes each, %d times\n", ITERATIONS);

    const int LIST_SIZE = 100;
    volatile int sum = 0;

    // Warm up allocator
    for (int i = 0; i < WARM_UP_ITERATIONS; i++) {
        Node* head = (Node*)malloc(sizeof(Node));
        free(head);
    }

    // Manual malloc/free
    uint64_t start = get_nanos();
    for (int iter = 0; iter < ITERATIONS; iter++) {
        Node* head = NULL;

        // Build list
        for (int i = 0; i < LIST_SIZE; i++) {
            Node* node = (Node*)malloc(sizeof(Node));
            node->value = i;
            node->next = head;
            head = node;
            sum += node->value;
        }

        // Destroy list
        while (head != NULL) {
            Node* next = head->next;
            free(head);
            head = next;
        }
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // ORC alloc/decref
    start = get_nanos();
    for (int iter = 0; iter < ITERATIONS; iter++) {
        Node* head = NULL;

        // Build list
        for (int i = 0; i < LIST_SIZE; i++) {
            Node* node = (Node*)ms_alloc(sizeof(Node));
            node->value = i;
            node->next = head;
            head = node;
            sum += node->value;
        }

        // Destroy list
        while (head != NULL) {
            Node* next = head->next;
            ms_decref(head);
            head = next;
        }
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    printf("  Manual: %.2f ms (%.2f ns/alloc)\n",
           manual_ms, (double)manual_ns / (ITERATIONS * LIST_SIZE));
    printf("  ORC:    %.2f ms (%.2f ns/alloc)\n",
           orc_ms, (double)orc_ns / (ITERATIONS * LIST_SIZE));
    printf("  Overhead: %.2f%%\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0);
    printf("  (sum = %d)\n\n", (int)sum);
}

// Benchmark 2: Array of structs (bulk allocation)
void bench_struct_array() {
    printf("Benchmark 2: Struct Array (bulk alloc + access + free)\n");
    printf("  Arrays of 1000 User structs, %d times\n", ITERATIONS);

    const int ARRAY_SIZE = 1000;
    volatile double sum = 0;

    // Manual malloc/free
    uint64_t start = get_nanos();
    for (int iter = 0; iter < ITERATIONS; iter++) {
        User** users = (User**)malloc(ARRAY_SIZE * sizeof(User*));

        // Allocate and initialize
        for (int i = 0; i < ARRAY_SIZE; i++) {
            users[i] = (User*)malloc(sizeof(User));
            users[i]->id = i;
            snprintf(users[i]->name, 32, "User%d", i);
            users[i]->score = i * 0.5;
            sum += users[i]->score;
        }

        // Free all
        for (int i = 0; i < ARRAY_SIZE; i++) {
            free(users[i]);
        }
        free(users);
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // ORC alloc/decref
    start = get_nanos();
    for (int iter = 0; iter < ITERATIONS; iter++) {
        User** users = (User**)malloc(ARRAY_SIZE * sizeof(User*));

        // Allocate and initialize
        for (int i = 0; i < ARRAY_SIZE; i++) {
            users[i] = (User*)ms_alloc(sizeof(User));
            users[i]->id = i;
            snprintf(users[i]->name, 32, "User%d", i);
            users[i]->score = i * 0.5;
            sum += users[i]->score;
        }

        // Free all
        for (int i = 0; i < ARRAY_SIZE; i++) {
            ms_decref(users[i]);
        }
        free(users);
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    printf("  Manual: %.2f ms\n", manual_ms);
    printf("  ORC:    %.2f ms\n", orc_ms);
    printf("  Overhead: %.2f%%\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0);
    printf("  (sum = %.2f)\n\n", sum);
}

// Benchmark 3: Reference counting simulation (shared ownership)
void bench_shared_ownership() {
    printf("Benchmark 3: Shared Ownership (incref/decref patterns)\n");
    printf("  Simulate shared ownership with RC, %d iterations\n", ITERATIONS * 10);

    volatile int sum = 0;

    // Manual RC simulation
    uint64_t start = get_nanos();
    for (int i = 0; i < ITERATIONS * 10; i++) {
        User* user = (User*)malloc(sizeof(User));
        user->id = i;
        user->score = i * 0.1;

        // Simulate 3 shared owners
        volatile int rc = 1;
        rc++; // Owner 1 takes ref
        rc++; // Owner 2 takes ref
        rc++; // Owner 3 takes ref

        sum += user->id;

        // Simulate releases
        rc--; // Owner 1 releases
        rc--; // Owner 2 releases
        rc--; // Owner 3 releases
        rc--; // Original owner releases

        free(user);
    }
    uint64_t manual_ns = get_nanos() - start;
    double manual_ms = manual_ns / 1000000.0;

    // ORC with actual RC operations
    start = get_nanos();
    for (int i = 0; i < ITERATIONS * 10; i++) {
        User* user = (User*)ms_alloc(sizeof(User));
        user->id = i;
        user->score = i * 0.1;

        // Simulate 3 shared owners
        ms_incref(user); // Owner 1 takes ref
        ms_incref(user); // Owner 2 takes ref
        ms_incref(user); // Owner 3 takes ref

        sum += user->id;

        // Simulate releases
        ms_decref(user); // Owner 1 releases
        ms_decref(user); // Owner 2 releases
        ms_decref(user); // Owner 3 releases
        ms_decref(user); // Original owner releases (freed)
    }
    uint64_t orc_ns = get_nanos() - start;
    double orc_ms = orc_ns / 1000000.0;

    printf("  Manual: %.2f ms\n", manual_ms);
    printf("  ORC:    %.2f ms\n", orc_ms);
    printf("  Overhead: %.2f%%\n",
           ((orc_ms - manual_ms) / manual_ms) * 100.0);
    printf("  (sum = %d)\n\n", (int)sum);
}

int main() {
    printf("===================================================\n");
    printf("ORC Realistic Performance Benchmark\n");
    printf("===================================================\n\n");

    bench_linked_list();
    bench_struct_array();
    bench_shared_ownership();

    printf("===================================================\n");
    printf("Summary:\n");
    printf("- Baseline ORC overhead: expect 5-15%%\n");
    printf("- With compile-time RC elimination: 2-3%%\n");
    printf("- With full optimization (Year 1): 0.5-2%%\n");
    printf("===================================================\n");

    return 0;
}
