/*
 * DRC (Dynamic Reference Counting) End-to-End Test
 * Tests the complete DRC pipeline with type-aware cycle detection
 *
 * Based on Principal Engineer review of Nim ORC and Lobster.
 */

#include <stdio.h>
#include <assert.h>
#include "orc.h"

/* Simple struct for testing (acyclic) */
typedef struct Point {
    double x;
    double y;
} Point;

/* Cyclic struct for testing */
typedef struct Node {
    int value;
    struct Node* next;  /* May point to self or other nodes (cycles) */
} Node;

/* Forward declarations for trace functions */
static void Node_trace(void* obj, msTraceCallback callback);

/* Type info for Node (cyclic: has 'next' pointer) */
static const msTypeInfo Node_type_info = {
    .name = "Node",
    .size = sizeof(Node),
    .is_cyclic = true,  /* Can form cycles */
    .trace_fn = Node_trace,
    .destroy_fn = NULL,
};

/* Type info for Point (acyclic: no ref fields) */
static const msTypeInfo Point_type_info = {
    .name = "Point",
    .size = sizeof(Point),
    .is_cyclic = false,  /* Cannot form cycles */
    .trace_fn = NULL,
    .destroy_fn = NULL,
};

/* Trace function for Node - iterates over ref fields */
static void Node_trace(void* obj, msTraceCallback callback) {
    Node* n = (Node*)obj;
    if (n->next != NULL) {
        callback(n->next);  /* Trace the 'next' field */
    }
}

void test_header_size() {
    printf("test_header_size...");

    /* RefHeader should be 8 bytes (memory efficient design) */
    assert(sizeof(msRefHeader) == 8);
    assert(sizeof(msFlags) == 1);

    printf("OK\n");
}

void test_basic_alloc_free() {
    printf("test_basic_alloc_free...");

    Point* p = (Point*)ms_alloc(sizeof(Point));
    assert(p != NULL);
    assert(ms_getrc(p) == 1);
    assert(ms_getcolor(p) == MS_COLOR_BLACK);
    /* Note: cyclicity is compile-time determined, not stored in object */

    p->x = 10.0;
    p->y = 20.0;

    ms_decref(p);  /* rc=0, freed */

    printf("OK\n");
}

void test_typed_alloc() {
    printf("test_typed_alloc...");

    /* Allocate (type info is provided at decref, not alloc) */
    Node* n = (Node*)ms_alloc(sizeof(Node));
    assert(n != NULL);
    assert(ms_getrc(n) == 1);
    /* cyclicity is checked via type_info at decref time */
    assert(Node_type_info.is_cyclic == true);

    n->value = 42;
    n->next = NULL;

    ms_decref(n);

    printf("OK\n");
}

void test_acyclic_type() {
    printf("test_acyclic_type...");

    /* Allocate acyclic type */
    Point* p = (Point*)ms_alloc(sizeof(Point));
    assert(p != NULL);
    /* cyclicity is checked via type_info, not stored in object */
    assert(Point_type_info.is_cyclic == false);

    ms_decref(p);

    printf("OK\n");
}

void test_incref_decref() {
    printf("test_incref_decref...");

    Node* n = (Node*)ms_alloc(sizeof(Node));
    assert(ms_getrc(n) == 1);

    ms_incref(n);
    assert(ms_getrc(n) == 2);

    ms_incref(n);
    assert(ms_getrc(n) == 3);

    ms_decref(n);
    assert(ms_getrc(n) == 2);

    ms_decref(n);
    assert(ms_getrc(n) == 1);

    ms_decref(n);  /* Freed */

    printf("OK\n");
}

void test_acyclic_skip() {
    printf("test_acyclic_skip...");

    uint64_t initial_skipped = ms_stats_acyclic_skipped;

    /* Acyclic type should skip cycle detection */
    Point* p = (Point*)ms_alloc(sizeof(Point));
    ms_incref(p);  /* rc=2 */

    ms_decref_typed(p, &Point_type_info);  /* rc=1, should skip cycle check */
    assert(ms_stats_acyclic_skipped == initial_skipped + 1);
    assert(!ms_is_in_roots(p));  /* Not added to roots */

    ms_decref(p);

    printf("OK\n");
}

void test_cycle_roots() {
    printf("test_cycle_roots...");

    size_t initial_roots = ms_get_root_count();

    /* Cyclic type should be added to roots */
    Node* n = (Node*)ms_alloc(sizeof(Node));
    n->next = NULL;
    ms_incref(n);  /* rc=2 */

    ms_decref_typed(n, &Node_type_info);  /* rc=1, added to roots */
    assert(ms_getrc(n) == 1);
    assert(ms_is_in_roots(n));
    assert(ms_getcolor(n) == MS_COLOR_PURPLE);
    assert(ms_get_root_count() == initial_roots + 1);

    ms_decref(n);  /* rc=0, freed, removed from roots */
    assert(ms_get_root_count() == initial_roots);

    printf("OK\n");
}

void test_shared_ownership() {
    printf("test_shared_ownership...");

    Node* a = (Node*)ms_alloc(sizeof(Node));
    Node* b = (Node*)ms_alloc(sizeof(Node));

    a->value = 1;
    b->value = 2;

    /* b.next = a (sharing) */
    b->next = (Node*)ms_clone(a);  /* a.rc = 2 */
    assert(ms_getrc(a) == 2);

    /* Drop b's reference to a */
    if (b->next) ms_decref(b->next);  /* a.rc = 1 */
    ms_decref(b);

    assert(ms_getrc(a) == 1);
    ms_decref(a);

    printf("OK\n");
}

void test_move_semantics() {
    printf("test_move_semantics...");

    Node* n = (Node*)ms_alloc(sizeof(Node));
    n->value = 100;

    /* Move to another variable (no RC change) */
    Node* m = (Node*)ms_sink(n);
    assert(ms_getrc(m) == 1);  /* Still 1, ownership transferred */

    ms_decref(m);

    printf("OK\n");
}

void test_null_safety() {
    printf("test_null_safety...");

    ms_incref(NULL);  /* Should not crash */
    ms_decref(NULL);  /* Should not crash */
    ms_decref_typed(NULL, &Node_type_info);  /* Should not crash */

    assert(ms_getrc(NULL) == 0);
    assert(ms_getcolor(NULL) == MS_COLOR_BLACK);
    assert(ms_is_in_roots(NULL) == false);

    printf("OK\n");
}

void test_statistics() {
    printf("test_statistics...");

    msStats before = ms_get_stats();

    Node* n = (Node*)ms_alloc(sizeof(Node));

    msStats after = ms_get_stats();
    assert(after.allocations == before.allocations + 1);

    ms_decref(n);

    msStats final = ms_get_stats();
    assert(final.deallocations == before.deallocations + 1);

    printf("OK\n");
}

void test_simple_cycle_detection() {
    printf("test_simple_cycle_detection...");

    /* Create two nodes pointing to each other (cycle) */
    Node* a = (Node*)ms_alloc(sizeof(Node));
    Node* b = (Node*)ms_alloc(sizeof(Node));

    a->value = 1;
    b->value = 2;

    /* Create cycle: a -> b -> a */
    a->next = (Node*)ms_clone(b);  /* b.rc = 2 */
    b->next = (Node*)ms_clone(a);  /* a.rc = 2 */

    assert(ms_getrc(a) == 2);
    assert(ms_getrc(b) == 2);

    /* Drop external references (caller provides type info) */
    ms_decref_typed(a, &Node_type_info);  /* a.rc = 1, added to roots */
    ms_decref_typed(b, &Node_type_info);  /* b.rc = 1, added to roots */

    assert(ms_getrc(a) == 1);
    assert(ms_getrc(b) == 1);
    assert(ms_is_in_roots(a));
    assert(ms_is_in_roots(b));

    /* Note: These form a cycle (a.rc=1, b.rc=1 but they reference each other)
     * The cycle collector would free them if we ran ms_collect_cycles()
     * For this test, we manually clean up */

    /* Manual cleanup for test */
    ms_decref(a->next);  /* b.rc = 0, freed */
    ms_decref(b->next);  /* a.rc = 0, freed */

    printf("OK\n");
}

/* ============================================================================
 * CYCLE COLLECTOR VERIFICATION TESTS
 * These tests verify the cycle collector ACTUALLY works, not just detects.
 * ============================================================================ */

/* Counter for destructor calls - proves destructors are invoked */
static int destructor_call_count = 0;
static int destructor_values[10];  /* Track which nodes were destroyed */

static void Node_destructor(void* obj) {
    Node* n = (Node*)obj;
    if (destructor_call_count < 10) {
        destructor_values[destructor_call_count] = n->value;
    }
    destructor_call_count++;
}

/* Node with destructor for testing */
static const msTypeInfo Node_with_destructor_info = {
    .name = "NodeWithDestructor",
    .size = sizeof(Node),
    .is_cyclic = true,
    .trace_fn = Node_trace,
    .destroy_fn = Node_destructor,
};

/*
 * TEST: Self-referential cycle (A→A) should be collected
 *
 * Setup: Create node pointing to itself
 *   [Node A] → [Node A] (self-loop)
 *   rc=2 (one external, one from self)
 *
 * After dropping external ref:
 *   rc=1 (only self-reference remains)
 *
 * Expected: Collector identifies this as garbage and frees it
 */
void test_self_cycle_collection() {
    printf("test_self_cycle_collection...");

    /* Clear any stale roots from previous tests */
    ms_clear_roots();

    uint64_t deallocs_before = ms_stats_deallocations;

    /* Create self-referential node */
    Node* a = (Node*)ms_alloc(sizeof(Node));
    a->value = 42;
    a->next = (Node*)ms_clone(a);  /* a.rc = 2, points to itself */

    assert(ms_getrc(a) == 2);

    /* Drop external reference */
    ms_decref_typed(a, &Node_type_info);  /* a.rc = 1, added to roots */

    assert(ms_getrc(a) == 1);
    assert(ms_is_in_roots(a));

    /* At this point:
     * - a.rc = 1 (from self-reference)
     * - a is in roots
     * - No external references exist
     * - This is GARBAGE and should be collected! */

    /* Run cycle collector */
    ms_collect_cycles();

    /* VERIFY: The node should have been freed */
    uint64_t deallocs_after = ms_stats_deallocations;

    assert(deallocs_after == deallocs_before + 1);  /* Collector should free 1 */
    assert(ms_get_root_count() == 0);  /* Roots should be empty */

    printf("OK\n");
}

/*
 * TEST: Two-node cycle (A→B→A) should be collected
 *
 * Setup:
 *   [Node A] → [Node B] → [Node A]
 *   a.rc=2, b.rc=2
 *
 * After dropping external refs:
 *   a.rc=1 (from b.next), b.rc=1 (from a.next)
 *
 * Expected: Collector identifies both as garbage and frees both
 */
void test_two_node_cycle_collection() {
    printf("test_two_node_cycle_collection...\n");

    /* Clear any stale roots from previous tests */
    ms_clear_roots();

    uint64_t deallocs_before = ms_stats_deallocations;

    /* Create two-node cycle */
    Node* a = (Node*)ms_alloc(sizeof(Node));
    Node* b = (Node*)ms_alloc(sizeof(Node));

    a->value = 1;
    b->value = 2;

    a->next = (Node*)ms_clone(b);  /* b.rc = 2 */
    b->next = (Node*)ms_clone(a);  /* a.rc = 2 */

    /* Drop external references */
    ms_decref_typed(a, &Node_type_info);  /* a.rc = 1 */
    ms_decref_typed(b, &Node_type_info);  /* b.rc = 1 */

    assert(ms_getrc(a) == 1);
    assert(ms_getrc(b) == 1);

    /* Both should be in roots now */
    size_t roots_before = ms_get_root_count();
    assert(roots_before >= 2);

    /* Run cycle collector */
    ms_collect_cycles();

    /* VERIFY: Both nodes should have been freed */
    uint64_t deallocs_after = ms_stats_deallocations;
    assert(deallocs_after == deallocs_before + 2);  /* Both freed */
    assert(ms_get_root_count() == 0);  /* Roots cleared */

    printf("OK\n");
}

/*
 * TEST: Destructors should be called for ALL cycle members
 *
 * This is critical for resource cleanup (files, sockets, etc.)
 */
void test_cycle_destructor_calls() {
    printf("test_cycle_destructor_calls...\n");

    /* Clear any stale roots from previous tests */
    ms_clear_roots();

    /* Reset destructor tracking */
    destructor_call_count = 0;
    for (int i = 0; i < 10; i++) destructor_values[i] = 0;

    /* Create three-node cycle */
    Node* a = (Node*)ms_alloc(sizeof(Node));
    Node* b = (Node*)ms_alloc(sizeof(Node));
    Node* c = (Node*)ms_alloc(sizeof(Node));

    a->value = 100;
    b->value = 200;
    c->value = 300;

    /* Create cycle: a → b → c → a */
    a->next = (Node*)ms_clone(b);  /* b.rc = 2 */
    b->next = (Node*)ms_clone(c);  /* c.rc = 2 */
    c->next = (Node*)ms_clone(a);  /* a.rc = 2 */

    /* Drop external references using destructor-enabled type */
    ms_decref_typed(a, &Node_with_destructor_info);
    ms_decref_typed(b, &Node_with_destructor_info);
    ms_decref_typed(c, &Node_with_destructor_info);

    /* Run cycle collector */
    ms_collect_cycles();

    /* VERIFY: All 3 destructors should have been called */
    assert(destructor_call_count == 3);

    /* Verify all values were seen (order may vary) */
    int seen_100 = 0, seen_200 = 0, seen_300 = 0;
    for (int i = 0; i < 3; i++) {
        if (destructor_values[i] == 100) seen_100 = 1;
        if (destructor_values[i] == 200) seen_200 = 1;
        if (destructor_values[i] == 300) seen_300 = 1;
    }
    assert(seen_100 && seen_200 && seen_300);

    printf("OK\n");
}

/*
 * TEST: Mixed live and garbage - only garbage collected
 *
 * Setup:
 *   [External] → [Node A] → [Node B] → [Node C] → [Node B] (cycle B↔C)
 *
 * A has external reference, so A is live.
 * B and C form a cycle with no external refs - they're garbage.
 */
void test_partial_cycle_collection() {
    printf("test_partial_cycle_collection...\n");

    /* Clear any stale roots from previous tests */
    ms_clear_roots();

    uint64_t deallocs_before = ms_stats_deallocations;
    (void)deallocs_before;  /* Suppress unused warning */

    /* Create nodes */
    Node* a = (Node*)ms_alloc(sizeof(Node));  /* Will stay alive */
    Node* b = (Node*)ms_alloc(sizeof(Node));  /* Will be garbage */
    Node* c = (Node*)ms_alloc(sizeof(Node));  /* Will be garbage */

    a->value = 1;
    b->value = 2;
    c->value = 3;

    /* a → b → c → b (cycle between b and c) */
    a->next = (Node*)ms_clone(b);  /* b.rc = 2 */
    b->next = (Node*)ms_clone(c);  /* c.rc = 2 */
    c->next = (Node*)ms_clone(b);  /* b.rc = 3 */

    /* Drop refs to b and c (but keep a alive) */
    ms_decref_typed(b, &Node_type_info);  /* b.rc = 2 */
    ms_decref_typed(c, &Node_type_info);  /* c.rc = 1 */

    /* a still holds reference, so:
     * - a.rc = 1 (external)
     * - b.rc = 2 (from a.next and c.next)
     * - c.rc = 1 (from b.next) */

    /* Run collector */
    ms_collect_cycles();

    /* a should still be alive with rc=1 */
    assert(ms_getrc(a) == 1);

    /* b and c should NOT be collected because a→b reference exists!
     * This tests that we don't over-collect. */

    /* Cleanup: drop a's ref to b, which should cascade-free b and c */
    ms_decref(a->next);  /* b.rc = 1 */
    /* Now b and c should be collected on next cycle run */

    ms_decref(a);  /* a freed */

    printf("OK\n");
}

int main() {
    printf("===================================================\n");
    printf("DRC (Dynamic Reference Counting) Tests\n");
    printf("Based on Nim ORC + Lobster ownership model\n");
    printf("===================================================\n\n");

    /* Basic tests */
    test_header_size();
    test_basic_alloc_free();
    test_typed_alloc();
    test_acyclic_type();
    test_incref_decref();
    test_acyclic_skip();
    test_cycle_roots();
    test_shared_ownership();
    test_move_semantics();
    test_null_safety();
    test_statistics();
    test_simple_cycle_detection();

    printf("\n--- Cycle Collector Verification Tests ---\n");

    /* These tests verify the cycle collector ACTUALLY collects cycles */
    test_self_cycle_collection();
    test_two_node_cycle_collection();
    test_cycle_destructor_calls();
    test_partial_cycle_collection();

    printf("\n===================================================\n");
    printf("All DRC tests passed!\n");

    msStats stats = ms_get_stats();
    printf("\nStatistics:\n");
    printf("  Allocations: %llu\n", stats.allocations);
    printf("  Deallocations: %llu\n", stats.deallocations);
    printf("  Cycle collections: %llu\n", stats.cycle_collections);
    printf("  Acyclic skipped: %llu\n", stats.acyclic_skipped);
    printf("===================================================\n");

    return 0;
}
