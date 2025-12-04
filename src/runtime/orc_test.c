/*
 * ORC (Optimized Reference Counting) - Comprehensive Test Suite
 *
 * PHILOSOPHY: Tests are derived from REAL-WORLD USE CASES.
 * Each test represents what a USER of this system expects to happen.
 * These tests are our "ground truth" - if they pass, the system works correctly.
 *
 * Use Cases Covered:
 * 1. Basic lifecycle - alloc, use, free
 * 2. Shared ownership - multiple refs to same object
 * 3. Self-referential cycles - node points to itself
 * 4. Two-node cycles - A↔B mutual references
 * 5. Three-node cycles - A→B→C→A
 * 6. Mixed live/garbage - some reachable, some in cycles
 * 7. Acyclic optimization - non-cyclic types skip cycle check
 * 8. Destructor guarantees - destructors called even in cycles
 * 9. Deep object graphs - chains and trees
 * 10. Memory safety - no UAF, no double-free, no leaks
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define MS_MAX_ROOTS 256
#define MS_CYCLE_THRESHOLD 50
#define MS_ORC_IMPLEMENTATION  /* This TU provides the storage */
#include "orc.h"

/* ============================================================================
 * TEST INFRASTRUCTURE
 * ============================================================================ */

static int tests_run = 0;
static int tests_passed = 0;
static int tests_failed = 0;

#define TEST(name) \
    static void test_##name(void); \
    static void run_test_##name(void) { \
        printf("  %-50s ", #name); \
        fflush(stdout); \
        ms_clear_roots(); \
        uint64_t allocs_before = ms_stats_allocations; \
        uint64_t deallocs_before = ms_stats_deallocations; \
        tests_run++; \
        test_##name(); \
        uint64_t allocs_after = ms_stats_allocations; \
        uint64_t deallocs_after = ms_stats_deallocations; \
        uint64_t leaked = (allocs_after - allocs_before) - (deallocs_after - deallocs_before); \
        if (leaked > 0) { \
            printf("LEAK (%llu objects)\n", leaked); \
            tests_failed++; \
        } else { \
            printf("OK\n"); \
            tests_passed++; \
        } \
    } \
    static void test_##name(void)

#define RUN_TEST(name) run_test_##name()

#define ASSERT(cond) do { \
    if (!(cond)) { \
        printf("FAIL at %s:%d: %s\n", __FILE__, __LINE__, #cond); \
        tests_failed++; \
        return; \
    } \
} while(0)

#define ASSERT_EQ(a, b) do { \
    if ((a) != (b)) { \
        printf("FAIL at %s:%d: %s != %s (%lld != %lld)\n", \
               __FILE__, __LINE__, #a, #b, (long long)(a), (long long)(b)); \
        tests_failed++; \
        return; \
    } \
} while(0)

/* ============================================================================
 * TEST DATA STRUCTURES
 * ============================================================================ */

/* Acyclic type: Point (no references to other objects) */
typedef struct Point {
    double x;
    double y;
    double z;
} Point;

/* Cyclic type: Node (can form reference chains/cycles) */
typedef struct Node {
    int id;
    struct Node* next;
} Node;

/* More complex cyclic type: TreeNode (parent + children) */
typedef struct TreeNode {
    int id;
    struct TreeNode* parent;
    struct TreeNode* left;
    struct TreeNode* right;
} TreeNode;

/* Destructor tracking */
static int destructor_count = 0;
static int destroyed_ids[100];

static void reset_destructor_tracking(void) {
    destructor_count = 0;
    memset(destroyed_ids, 0, sizeof(destroyed_ids));
}

static int was_destroyed(int id) {
    for (int i = 0; i < destructor_count; i++) {
        if (destroyed_ids[i] == id) return 1;
    }
    return 0;
}

/* Trace and destroy functions */
static void Node_trace(void* obj, msTraceCallback cb) {
    Node* n = (Node*)obj;
    if (n->next) cb(n->next);
}

static void Node_destroy(void* obj) {
    Node* n = (Node*)obj;
    if (destructor_count < 100) {
        destroyed_ids[destructor_count++] = n->id;
    }
}

static void TreeNode_trace(void* obj, msTraceCallback cb) {
    TreeNode* t = (TreeNode*)obj;
    if (t->parent) cb(t->parent);
    if (t->left) cb(t->left);
    if (t->right) cb(t->right);
}

static void TreeNode_destroy(void* obj) {
    TreeNode* t = (TreeNode*)obj;
    if (destructor_count < 100) {
        destroyed_ids[destructor_count++] = t->id;
    }
}

/* Type infos */
static const msTypeInfo Point_info = {
    .name = "Point",
    .size = sizeof(Point),
    .is_cyclic = false,  /* CANNOT form cycles */
    .trace_fn = NULL,
    .destroy_fn = NULL,
};

static const msTypeInfo Node_info = {
    .name = "Node",
    .size = sizeof(Node),
    .is_cyclic = true,  /* CAN form cycles */
    .trace_fn = Node_trace,
    .destroy_fn = Node_destroy,
};

static const msTypeInfo TreeNode_info = {
    .name = "TreeNode",
    .size = sizeof(TreeNode),
    .is_cyclic = true,
    .trace_fn = TreeNode_trace,
    .destroy_fn = TreeNode_destroy,
};

/* ============================================================================
 * USE CASE 1: Basic Lifecycle
 *
 * User expectation: "I allocate an object, use it, release it. Memory is freed."
 * ============================================================================ */

TEST(basic_alloc_and_free) {
    /* User allocates a point */
    Point* p = (Point*)ms_alloc(sizeof(Point));
    ASSERT(p != NULL);
    ASSERT_EQ(ms_getrc(p), 1);

    /* User sets values */
    p->x = 1.0;
    p->y = 2.0;
    p->z = 3.0;

    /* User releases - should be freed immediately */
    ms_decref(p);
    /* Test infrastructure checks for leaks automatically */
}

TEST(basic_alloc_typed_and_free) {
    reset_destructor_tracking();

    /* User allocates a node with type info */
    Node* n = (Node*)ms_alloc(sizeof(Node));
    ASSERT(n != NULL);
    n->id = 42;
    n->next = NULL;

    /* User releases with type info */
    ms_decref_typed(n, &Node_info);

    /* Destructor should be called */
    ASSERT_EQ(destructor_count, 1);
    ASSERT(was_destroyed(42));
}

/* ============================================================================
 * USE CASE 2: Shared Ownership
 *
 * User expectation: "Multiple parts of my code can hold references to the
 * same object. It stays alive until ALL references are dropped."
 * ============================================================================ */

TEST(shared_ownership_two_refs) {
    Node* n = (Node*)ms_alloc(sizeof(Node));
    n->id = 1;
    n->next = NULL;
    ASSERT_EQ(ms_getrc(n), 1);

    /* Another part of code takes a reference */
    ms_incref(n);
    ASSERT_EQ(ms_getrc(n), 2);

    /* First part releases */
    ms_decref(n);
    ASSERT_EQ(ms_getrc(n), 1);  /* Still alive! */

    /* Second part releases */
    ms_decref(n);
    /* Now freed */
}

TEST(shared_ownership_many_refs) {
    Node* n = (Node*)ms_alloc(sizeof(Node));
    n->id = 1;
    n->next = NULL;

    /* 10 parts of code take references */
    for (int i = 0; i < 10; i++) {
        ms_incref(n);
    }
    ASSERT_EQ(ms_getrc(n), 11);

    /* All but one release */
    for (int i = 0; i < 10; i++) {
        ms_decref(n);
    }
    ASSERT_EQ(ms_getrc(n), 1);  /* Still alive */

    ms_decref(n);  /* Final release */
}

TEST(shared_ownership_via_clone) {
    Node* original = (Node*)ms_alloc(sizeof(Node));
    original->id = 1;
    original->next = NULL;

    /* Clone creates shared reference */
    Node* shared = (Node*)ms_clone(original);
    ASSERT(shared == original);  /* Same pointer */
    ASSERT_EQ(ms_getrc(original), 2);

    ms_decref(shared);
    ms_decref(original);
}

/* ============================================================================
 * USE CASE 3: Self-Referential Cycles
 *
 * User expectation: "I create a node that points to itself. When I drop my
 * external reference, the cycle collector should free it - NO MEMORY LEAK."
 * ============================================================================ */

TEST(self_cycle_is_collected) {
    reset_destructor_tracking();

    /* Create self-referential node */
    Node* n = (Node*)ms_alloc(sizeof(Node));
    n->id = 42;
    n->next = (Node*)ms_clone(n);  /* Points to itself, rc=2 */

    ASSERT_EQ(ms_getrc(n), 2);

    /* Drop external reference */
    ms_decref_typed(n, &Node_info);  /* rc=1, but only self-ref remains */

    /* At this point: n.rc=1, but n is GARBAGE (only self-referential) */
    /* The cycle collector MUST free it */

    ms_collect_cycles();

    /* Verify destructor was called */
    ASSERT_EQ(destructor_count, 1);
    ASSERT(was_destroyed(42));
}

TEST(self_cycle_destructor_called) {
    reset_destructor_tracking();

    Node* n = (Node*)ms_alloc(sizeof(Node));
    n->id = 99;
    n->next = (Node*)ms_clone(n);

    ms_decref_typed(n, &Node_info);
    ms_collect_cycles();

    /* CRITICAL: Destructor MUST be called even for cyclic garbage */
    ASSERT_EQ(destructor_count, 1);
    ASSERT(was_destroyed(99));
}

/* ============================================================================
 * USE CASE 4: Two-Node Cycles
 *
 * User expectation: "A and B point to each other. When I drop both external
 * references, both objects should be collected - NO MEMORY LEAK."
 * ============================================================================ */

TEST(two_node_cycle_is_collected) {
    reset_destructor_tracking();

    Node* a = (Node*)ms_alloc(sizeof(Node));
    Node* b = (Node*)ms_alloc(sizeof(Node));

    a->id = 1;
    b->id = 2;

    /* Create cycle: A → B → A */
    a->next = (Node*)ms_clone(b);  /* b.rc = 2 */
    b->next = (Node*)ms_clone(a);  /* a.rc = 2 */

    ASSERT_EQ(ms_getrc(a), 2);
    ASSERT_EQ(ms_getrc(b), 2);

    /* Drop external references */
    ms_decref_typed(a, &Node_info);  /* a.rc = 1 */
    ms_decref_typed(b, &Node_info);  /* b.rc = 1 */

    /* Both have rc=1 but are GARBAGE (mutual refs only) */
    ms_collect_cycles();

    /* Both MUST be freed */
    ASSERT_EQ(destructor_count, 2);
    ASSERT(was_destroyed(1));
    ASSERT(was_destroyed(2));
}

TEST(two_node_cycle_both_destructors_called) {
    reset_destructor_tracking();

    Node* a = (Node*)ms_alloc(sizeof(Node));
    Node* b = (Node*)ms_alloc(sizeof(Node));
    a->id = 10;
    b->id = 20;
    a->next = (Node*)ms_clone(b);
    b->next = (Node*)ms_clone(a);

    ms_decref_typed(a, &Node_info);
    ms_decref_typed(b, &Node_info);
    ms_collect_cycles();

    /* BOTH destructors must be called */
    ASSERT_EQ(destructor_count, 2);
}

/* ============================================================================
 * USE CASE 5: Three-Node Cycles (and larger)
 *
 * User expectation: "A→B→C→A forms a cycle. All three should be collected."
 * ============================================================================ */

TEST(three_node_cycle_is_collected) {
    reset_destructor_tracking();

    Node* a = (Node*)ms_alloc(sizeof(Node));
    Node* b = (Node*)ms_alloc(sizeof(Node));
    Node* c = (Node*)ms_alloc(sizeof(Node));

    a->id = 1;
    b->id = 2;
    c->id = 3;

    /* Create cycle: A → B → C → A */
    a->next = (Node*)ms_clone(b);
    b->next = (Node*)ms_clone(c);
    c->next = (Node*)ms_clone(a);

    /* Drop all external refs */
    ms_decref_typed(a, &Node_info);
    ms_decref_typed(b, &Node_info);
    ms_decref_typed(c, &Node_info);

    ms_collect_cycles();

    /* All three must be freed */
    ASSERT_EQ(destructor_count, 3);
    ASSERT(was_destroyed(1));
    ASSERT(was_destroyed(2));
    ASSERT(was_destroyed(3));
}

TEST(five_node_cycle_is_collected) {
    reset_destructor_tracking();

    Node* nodes[5];
    for (int i = 0; i < 5; i++) {
        nodes[i] = (Node*)ms_alloc(sizeof(Node));
        nodes[i]->id = i + 1;
        nodes[i]->next = NULL;
    }

    /* Create cycle: 1→2→3→4→5→1 */
    for (int i = 0; i < 5; i++) {
        nodes[i]->next = (Node*)ms_clone(nodes[(i + 1) % 5]);
    }

    /* Drop all external refs */
    for (int i = 0; i < 5; i++) {
        ms_decref_typed(nodes[i], &Node_info);
    }

    ms_collect_cycles();

    /* All five must be freed */
    ASSERT_EQ(destructor_count, 5);
}

/* ============================================================================
 * USE CASE 6: Mixed Live and Garbage
 *
 * User expectation: "Some objects are reachable from my code, some are only
 * in cycles. The collector should ONLY free the unreachable ones."
 * ============================================================================ */

TEST(live_object_not_collected) {
    reset_destructor_tracking();

    /* A is live (we keep external ref) */
    Node* a = (Node*)ms_alloc(sizeof(Node));
    a->id = 1;
    a->next = NULL;

    /* B and C form a cycle, no external refs */
    Node* b = (Node*)ms_alloc(sizeof(Node));
    Node* c = (Node*)ms_alloc(sizeof(Node));
    b->id = 2;
    c->id = 3;
    b->next = (Node*)ms_clone(c);
    c->next = (Node*)ms_clone(b);

    /* Drop external refs to B and C only */
    ms_decref_typed(b, &Node_info);
    ms_decref_typed(c, &Node_info);

    ms_collect_cycles();

    /* B and C should be collected */
    ASSERT_EQ(destructor_count, 2);
    ASSERT(was_destroyed(2));
    ASSERT(was_destroyed(3));

    /* A should still be alive */
    ASSERT(!was_destroyed(1));
    ASSERT_EQ(ms_getrc(a), 1);

    /* Clean up A */
    ms_decref(a);
}

TEST(reachable_from_live_not_collected) {
    reset_destructor_tracking();

    /* A → B → C chain, we keep ref to A */
    Node* a = (Node*)ms_alloc(sizeof(Node));
    Node* b = (Node*)ms_alloc(sizeof(Node));
    Node* c = (Node*)ms_alloc(sizeof(Node));

    a->id = 1;
    b->id = 2;
    c->id = 3;

    a->next = (Node*)ms_clone(b);
    b->next = (Node*)ms_clone(c);
    c->next = NULL;

    /* Drop refs to B and C, but A still refs B which refs C */
    ms_decref_typed(b, &Node_info);
    ms_decref_typed(c, &Node_info);

    ms_collect_cycles();

    /* Nothing should be collected - all reachable from A */
    ASSERT_EQ(destructor_count, 0);

    /* Clean up properly: release chain from tail */
    ms_decref(b->next);  /* c, rc 1->0, freed */
    ms_decref(a->next);  /* b, rc 1->0, freed */
    ms_decref(a);        /* a, rc 1->0, freed */
}

TEST(cycle_reachable_from_live_not_collected) {
    reset_destructor_tracking();

    /* A (live) → B ↔ C (cycle) */
    /* The B↔C cycle is reachable from A, so should NOT be collected */
    Node* a = (Node*)ms_alloc(sizeof(Node));
    Node* b = (Node*)ms_alloc(sizeof(Node));
    Node* c = (Node*)ms_alloc(sizeof(Node));

    a->id = 1;
    b->id = 2;
    c->id = 3;

    a->next = (Node*)ms_clone(b);  /* a→b, b.rc=2 */
    b->next = (Node*)ms_clone(c);  /* b→c, c.rc=2 */
    c->next = (Node*)ms_clone(b);  /* c→b, b.rc=3 */

    /* Drop direct refs to B and C */
    ms_decref_typed(b, &Node_info);  /* b.rc=2 */
    ms_decref_typed(c, &Node_info);  /* c.rc=1 */

    ms_collect_cycles();

    /* B and C should NOT be collected - reachable from A */
    ASSERT_EQ(destructor_count, 0);
    ASSERT_EQ(ms_getrc(a), 1);

    /* Clean up: drop a's reference to b using ms_decref_typed to re-mark as potential root */
    /* Note: Using ms_decref_typed is required because after first collect_cycles(),
     * b and c were removed from roots (determined live). When A drops its ref to B,
     * we must use ms_decref_typed to re-add B to roots for cycle detection. */
    ms_decref_typed(a->next, &Node_info);  /* b.rc=1 (from c.next only), re-added to roots */
    a->next = NULL;

    /* Now b↔c is unreachable, collect it */
    ms_collect_cycles();

    /* Now B and C should be collected */
    ASSERT_EQ(destructor_count, 2);
    ASSERT(was_destroyed(2));
    ASSERT(was_destroyed(3));

    /* Clean up A */
    ms_decref(a);
}

/* ============================================================================
 * USE CASE 7: Acyclic Types Optimization
 *
 * User expectation: "Types that can't form cycles should be handled faster
 * (skip cycle detection entirely)."
 * ============================================================================ */

TEST(acyclic_type_skips_cycle_check) {
    uint64_t skipped_before = ms_stats_acyclic_skipped;

    Point* p = (Point*)ms_alloc(sizeof(Point));
    p->x = 1.0;
    p->y = 2.0;
    p->z = 3.0;

    ms_incref(p);  /* rc=2 */

    /* Decref with acyclic type info should skip cycle detection */
    ms_decref_typed(p, &Point_info);  /* rc=1 */

    uint64_t skipped_after = ms_stats_acyclic_skipped;
    ASSERT(skipped_after > skipped_before);

    /* Should NOT be in roots */
    ASSERT(!ms_is_in_roots(p));

    ms_decref(p);
}

TEST(acyclic_type_not_added_to_roots) {
    size_t roots_before = ms_get_root_count();

    Point* p = (Point*)ms_alloc(sizeof(Point));
    ms_incref(p);
    ms_decref_typed(p, &Point_info);

    /* Acyclic types should never be added to roots */
    ASSERT_EQ(ms_get_root_count(), roots_before);

    ms_decref(p);
}

/* ============================================================================
 * USE CASE 8: Destructor Guarantees
 *
 * User expectation: "My destructor MUST be called when object is freed,
 * even if it's part of a cycle. This is critical for resource cleanup."
 * ============================================================================ */

TEST(destructor_called_on_simple_free) {
    reset_destructor_tracking();

    Node* n = (Node*)ms_alloc(sizeof(Node));
    n->id = 777;
    n->next = NULL;

    ms_decref_typed(n, &Node_info);

    ASSERT_EQ(destructor_count, 1);
    ASSERT(was_destroyed(777));
}

TEST(destructor_called_for_all_cycle_members) {
    reset_destructor_tracking();

    /* Create 4-node cycle */
    Node* nodes[4];
    for (int i = 0; i < 4; i++) {
        nodes[i] = (Node*)ms_alloc(sizeof(Node));
        nodes[i]->id = 100 + i;
    }

    for (int i = 0; i < 4; i++) {
        nodes[i]->next = (Node*)ms_clone(nodes[(i + 1) % 4]);
    }

    for (int i = 0; i < 4; i++) {
        ms_decref_typed(nodes[i], &Node_info);
    }

    ms_collect_cycles();

    /* ALL 4 destructors must be called */
    ASSERT_EQ(destructor_count, 4);
    ASSERT(was_destroyed(100));
    ASSERT(was_destroyed(101));
    ASSERT(was_destroyed(102));
    ASSERT(was_destroyed(103));
}

/* ============================================================================
 * USE CASE 9: Complex Object Graphs
 *
 * User expectation: "Real applications have complex graphs with chains, trees,
 * and cycles mixed together. The system should handle them correctly."
 * ============================================================================ */

TEST(tree_with_parent_pointers_cycle) {
    reset_destructor_tracking();

    /* Tree where children point back to parent (common pattern) */
    /*       1
     *      / \
     *     2   3
     * Children have parent pointers → cycles!
     */
    TreeNode* root = (TreeNode*)ms_alloc(sizeof(TreeNode));
    TreeNode* left = (TreeNode*)ms_alloc(sizeof(TreeNode));
    TreeNode* right = (TreeNode*)ms_alloc(sizeof(TreeNode));

    root->id = 1;
    left->id = 2;
    right->id = 3;

    root->parent = NULL;
    root->left = (TreeNode*)ms_clone(left);
    root->right = (TreeNode*)ms_clone(right);

    left->parent = (TreeNode*)ms_clone(root);  /* Cycle! */
    left->left = NULL;
    left->right = NULL;

    right->parent = (TreeNode*)ms_clone(root);  /* Cycle! */
    right->left = NULL;
    right->right = NULL;

    /* Drop original refs */
    ms_decref_typed(root, &TreeNode_info);
    ms_decref_typed(left, &TreeNode_info);
    ms_decref_typed(right, &TreeNode_info);

    ms_collect_cycles();

    /* All 3 should be collected (they form cycles) */
    ASSERT_EQ(destructor_count, 3);
}

/*
 * Grandchildren test: Cycle collection when children are NOT directly decremented.
 *
 * CRITICAL PATTERN: When objects are only reachable through cycles (never
 * directly decremented), they MUST have ms_set_type() called after allocation.
 * Otherwise, the cycle collector can't trace their children.
 *
 * Scenario: A → B → C → A (cycle), but we only decref A
 */
TEST(grandchildren_cycle_with_set_type) {
    reset_destructor_tracking();

    /* Create three nodes: A → B → C → A */
    Node* a = (Node*)ms_alloc(sizeof(Node));
    Node* b = (Node*)ms_alloc(sizeof(Node));
    Node* c = (Node*)ms_alloc(sizeof(Node));

    a->id = 1;
    b->id = 2;
    c->id = 3;

    /*
     * CRITICAL: Set type on B and C immediately after allocation!
     * Without this, cycle collection can't trace through them.
     */
    ms_set_type(b, &Node_info);
    ms_set_type(c, &Node_info);

    /* Form cycle: A → B → C → A */
    a->next = (Node*)ms_clone(b);  /* b.rc = 2 */
    b->next = (Node*)ms_clone(c);  /* c.rc = 2 */
    c->next = (Node*)ms_clone(a);  /* a.rc = 2 */

    /*
     * Only drop refs to A, B, C (not via cycle tracing).
     * B and C were never passed to ms_decref_typed, but they have
     * type_id set from ms_set_type, so tracing will work.
     */
    ms_decref_typed(a, &Node_info);  /* a.rc = 1, added to roots */
    ms_decref(b);                     /* b.rc = 1, NOT added to roots (no type) */
    ms_decref(c);                     /* c.rc = 1, NOT added to roots (no type) */

    /* All three form a cycle with only internal refs */
    ms_collect_cycles();

    /* All 3 MUST be collected */
    ASSERT_EQ(destructor_count, 3);
    ASSERT(was_destroyed(1));
    ASSERT(was_destroyed(2));
    ASSERT(was_destroyed(3));
}

/*
 * Deep chain test: A → B → C → D → E → A
 * Verifies type_id propagates through multi-level chains.
 */
TEST(deep_chain_cycle_all_collected) {
    reset_destructor_tracking();

    /* Create 5-node chain */
    Node* nodes[5];
    for (int i = 0; i < 5; i++) {
        nodes[i] = (Node*)ms_alloc(sizeof(Node));
        nodes[i]->id = i + 1;
        ms_set_type(nodes[i], &Node_info);  /* CRITICAL: set type immediately */
    }

    /* Form cycle: 1 → 2 → 3 → 4 → 5 → 1 */
    for (int i = 0; i < 5; i++) {
        nodes[i]->next = (Node*)ms_clone(nodes[(i + 1) % 5]);
    }

    /* Only decref_typed the first node */
    ms_decref_typed(nodes[0], &Node_info);

    /* Plain decref for the rest (not added to roots) */
    for (int i = 1; i < 5; i++) {
        ms_decref(nodes[i]);
    }

    ms_collect_cycles();

    /* All 5 must be collected */
    ASSERT_EQ(destructor_count, 5);
}

/* ============================================================================
 * USE CASE 10: Memory Safety
 *
 * User expectation: "No use-after-free, no double-free, no memory corruption."
 * ============================================================================ */

TEST(null_safety) {
    /* These should not crash */
    ms_incref(NULL);
    ms_decref(NULL);
    ms_decref_typed(NULL, &Node_info);

    ASSERT_EQ(ms_getrc(NULL), 0);
    ASSERT_EQ(ms_getcolor(NULL), MS_COLOR_BLACK);
    ASSERT(!ms_is_in_roots(NULL));
}

TEST(double_incref_decref_balanced) {
    Node* n = (Node*)ms_alloc(sizeof(Node));
    n->id = 1;
    n->next = NULL;

    /* Many increfs */
    for (int i = 0; i < 100; i++) {
        ms_incref(n);
    }
    ASSERT_EQ(ms_getrc(n), 101);

    /* Same number of decrefs */
    for (int i = 0; i < 100; i++) {
        ms_decref(n);
    }
    ASSERT_EQ(ms_getrc(n), 1);

    ms_decref(n);
}

/* ============================================================================
 * EDGE CASES
 * ============================================================================ */

TEST(collect_cycles_with_no_roots) {
    /* Should not crash */
    ms_clear_roots();
    ms_collect_cycles();
    ms_collect_cycles();
    ms_collect_cycles();
}

TEST(multiple_collect_cycles_idempotent) {
    reset_destructor_tracking();

    Node* n = (Node*)ms_alloc(sizeof(Node));
    n->id = 1;
    n->next = (Node*)ms_clone(n);

    ms_decref_typed(n, &Node_info);

    /* Multiple collections should be safe */
    ms_collect_cycles();
    ms_collect_cycles();
    ms_collect_cycles();

    /* Should only free once */
    ASSERT_EQ(destructor_count, 1);
}

/* ============================================================================
 * MAIN TEST RUNNER
 * ============================================================================ */

int main(void) {
    printf("================================================================\n");
    printf("ORC Comprehensive Test Suite\n");
    printf("Testing REAL-WORLD USE CASES\n");
    printf("================================================================\n\n");

    printf("USE CASE 1: Basic Lifecycle\n");
    RUN_TEST(basic_alloc_and_free);
    RUN_TEST(basic_alloc_typed_and_free);

    printf("\nUSE CASE 2: Shared Ownership\n");
    RUN_TEST(shared_ownership_two_refs);
    RUN_TEST(shared_ownership_many_refs);
    RUN_TEST(shared_ownership_via_clone);

    printf("\nUSE CASE 3: Self-Referential Cycles\n");
    RUN_TEST(self_cycle_is_collected);
    RUN_TEST(self_cycle_destructor_called);

    printf("\nUSE CASE 4: Two-Node Cycles\n");
    RUN_TEST(two_node_cycle_is_collected);
    RUN_TEST(two_node_cycle_both_destructors_called);

    printf("\nUSE CASE 5: Three+ Node Cycles\n");
    RUN_TEST(three_node_cycle_is_collected);
    RUN_TEST(five_node_cycle_is_collected);

    printf("\nUSE CASE 6: Mixed Live and Garbage\n");
    RUN_TEST(live_object_not_collected);
    RUN_TEST(reachable_from_live_not_collected);
    RUN_TEST(cycle_reachable_from_live_not_collected);

    printf("\nUSE CASE 7: Acyclic Type Optimization\n");
    RUN_TEST(acyclic_type_skips_cycle_check);
    RUN_TEST(acyclic_type_not_added_to_roots);

    printf("\nUSE CASE 8: Destructor Guarantees\n");
    RUN_TEST(destructor_called_on_simple_free);
    RUN_TEST(destructor_called_for_all_cycle_members);

    printf("\nUSE CASE 9: Complex Object Graphs\n");
    RUN_TEST(tree_with_parent_pointers_cycle);
    RUN_TEST(grandchildren_cycle_with_set_type);
    RUN_TEST(deep_chain_cycle_all_collected);

    printf("\nUSE CASE 10: Memory Safety\n");
    RUN_TEST(null_safety);
    RUN_TEST(double_incref_decref_balanced);

    printf("\nEDGE CASES\n");
    RUN_TEST(collect_cycles_with_no_roots);
    RUN_TEST(multiple_collect_cycles_idempotent);

    printf("\n================================================================\n");
    printf("Results: %d/%d tests passed", tests_passed, tests_run);
    if (tests_failed > 0) {
        printf(" (%d FAILED)", tests_failed);
    }
    printf("\n");

    msStats stats = ms_get_stats();
    printf("\nRuntime Statistics:\n");
    printf("  Allocations:        %llu\n", stats.allocations);
    printf("  Deallocations:      %llu\n", stats.deallocations);
    printf("  Cycle collections:  %llu\n", stats.cycle_collections);
    printf("  Acyclic skipped:    %llu\n", stats.acyclic_skipped);

    if (stats.allocations != stats.deallocations) {
        printf("\n  WARNING: %lld objects leaked!\n",
               (long long)(stats.allocations - stats.deallocations));
    }

    printf("================================================================\n");

    return tests_failed > 0 ? 1 : 0;
}
