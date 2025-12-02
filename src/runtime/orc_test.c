/*
 * ORC C API Test
 * Verifies that the C header works correctly
 */

#include "orc.h"
#include <stdio.h>
#include <assert.h>

typedef struct {
    int x;
    int y;
} Point;

void test_alloc_and_free() {
    printf("test_alloc_and_free...");

    Point* p = (Point*)ms_alloc(sizeof(Point));
    assert(p != NULL);
    assert(ms_getrc(p) == 1);

    p->x = 10;
    p->y = 20;

    ms_decref(p); // rc=0, freed

    printf("OK\n");
}

void test_incref_decref() {
    printf("test_incref_decref...");

    Point* p = (Point*)ms_alloc(sizeof(Point));
    assert(ms_getrc(p) == 1);

    ms_incref(p);
    assert(ms_getrc(p) == 2);

    ms_incref(p);
    assert(ms_getrc(p) == 3);

    ms_decref(p);
    assert(ms_getrc(p) == 2);

    ms_decref(p);
    assert(ms_getrc(p) == 1);

    ms_decref(p); // rc=0, freed

    printf("OK\n");
}

void test_multiple_allocations() {
    printf("test_multiple_allocations...");

    Point* p1 = (Point*)ms_alloc(sizeof(Point));
    Point* p2 = (Point*)ms_alloc(sizeof(Point));
    Point* p3 = (Point*)ms_alloc(sizeof(Point));

    p1->x = 1; p1->y = 2;
    p2->x = 3; p2->y = 4;
    p3->x = 5; p3->y = 6;

    assert(ms_getrc(p1) == 1);
    assert(ms_getrc(p2) == 1);
    assert(ms_getrc(p3) == 1);

    ms_incref(p2); // Only increment p2

    assert(ms_getrc(p1) == 1);
    assert(ms_getrc(p2) == 2);
    assert(ms_getrc(p3) == 1);

    ms_decref(p1);
    ms_decref(p2);
    ms_decref(p2);
    ms_decref(p3);

    printf("OK\n");
}

void test_clone_and_sink() {
    printf("test_clone_and_sink...");

    Point* p = (Point*)ms_alloc(sizeof(Point));
    assert(ms_getrc(p) == 1);

    // Clone: increment RC
    Point* p2 = (Point*)ms_clone(p);
    assert(p2 == p); // Same pointer
    assert(ms_getrc(p) == 2);

    // Sink: no RC change (move semantics)
    Point* p3 = (Point*)ms_sink(p);
    assert(p3 == p); // Same pointer
    assert(ms_getrc(p) == 2); // No change

    ms_decref(p2);
    ms_decref(p3);

    printf("OK\n");
}

void test_null_safety() {
    printf("test_null_safety...");

    // All operations should handle NULL gracefully
    ms_incref(NULL);
    ms_decref(NULL);
    assert(ms_getrc(NULL) == 0);
    assert(ms_is_in_roots(NULL) == 0);
    void* p = ms_sink(NULL);
    assert(p == NULL);
    p = ms_clone(NULL);
    assert(p == NULL);

    printf("OK\n");
}

int main() {
    printf("Running ORC C API tests...\n");

    test_alloc_and_free();
    test_incref_decref();
    test_multiple_allocations();
    test_clone_and_sink();
    test_null_safety();

    printf("\nAll tests passed!\n");
    return 0;
}
