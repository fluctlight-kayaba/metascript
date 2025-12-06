/*
 * Metascript Promise Runtime Implementation
 *
 * TypeScript-compatible Promise<T> for C backend.
 * See promise.h for design documentation.
 */

#define MS_ORC_IMPLEMENTATION
#include "promise.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ═══════════════════════════════════════════════════════════════════════════
 * FORWARD DECLARATIONS
 * ═══════════════════════════════════════════════════════════════════════════ */

/* Microtask queue (simple implementation - will be replaced by dispatcher.h) */
static void ms_enqueue_microtask(msPromiseCallback callback, void* context);
static void ms_drain_microtasks(void);

/* ═══════════════════════════════════════════════════════════════════════════
 * MICROTASK QUEUE (Circular Buffer - O(1) enqueue/dequeue)
 *
 * Will be replaced by dispatcher.h in Week 2 for full event loop integration.
 * ═══════════════════════════════════════════════════════════════════════════ */

#define MS_MAX_MICROTASKS 1024
#define MS_MICROTASK_HEADROOM 256  /* Reserve for nested enqueues during drain */

typedef struct {
    msPromiseCallback callback;
    void* context;
} msMicrotask;

static msMicrotask ms_microtask_queue[MS_MAX_MICROTASKS];
static size_t ms_microtask_head = 0;  /* Index of first item to dequeue */
static size_t ms_microtask_tail = 0;  /* Index of next slot to enqueue */
static size_t ms_microtask_count = 0; /* Number of items in queue */
static bool ms_draining_microtasks = false;

static void ms_enqueue_microtask(msPromiseCallback callback, void* context) {
    /* During drain: allow full capacity (nested enqueues processed in same loop)
     * Outside drain: reserve headroom for nested enqueues that may happen during resolve */
    size_t limit = ms_draining_microtasks
        ? MS_MAX_MICROTASKS
        : (MS_MAX_MICROTASKS - MS_MICROTASK_HEADROOM);

    if (ms_microtask_count >= limit) {
        fprintf(stderr, "[PROMISE] PANIC: Microtask queue overflow (count=%zu, limit=%zu)\n",
                ms_microtask_count, limit);
        fprintf(stderr, "          This indicates runaway promise chains or too many concurrent promises.\n");
        abort();  /* CRITICAL: Silent callback loss is worse than crash */
    }

    /* Integer overflow protection (theoretical on 64-bit, possible on 32-bit) */
    if (ms_microtask_count == SIZE_MAX) {
        fprintf(stderr, "[PROMISE] PANIC: Microtask count overflow\n");
        abort();
    }

    ms_microtask_queue[ms_microtask_tail].callback = callback;
    ms_microtask_queue[ms_microtask_tail].context = context;
    ms_microtask_tail = (ms_microtask_tail + 1) % MS_MAX_MICROTASKS;
    ms_microtask_count++;
}

/* Maximum iterations to prevent infinite microtask loops */
#define MS_MAX_DRAIN_ITERATIONS 100000

static void ms_drain_microtasks(void) {
    if (ms_draining_microtasks) return;  /* Prevent reentrant drain */
    ms_draining_microtasks = true;

    /* Process all microtasks, including those added during processing */
    size_t iterations = 0;
    while (ms_microtask_count > 0) {
        if (iterations++ > MS_MAX_DRAIN_ITERATIONS) {
            fprintf(stderr, "[PROMISE] PANIC: Microtask drain exceeded %d iterations\n",
                    MS_MAX_DRAIN_ITERATIONS);
            fprintf(stderr, "          Possible infinite microtask loop detected.\n");
            abort();
        }

        /* Dequeue from head - O(1) */
        msMicrotask task = ms_microtask_queue[ms_microtask_head];
        ms_microtask_head = (ms_microtask_head + 1) % MS_MAX_MICROTASKS;
        ms_microtask_count--;

        /* Execute */
        if (task.callback) {
            task.callback(task.context);
        }
    }

    ms_draining_microtasks = false;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * TYPE INFO
 * ═══════════════════════════════════════════════════════════════════════════ */

const msTypeInfo msPromise_type = {
    .name = "Promise",
    .size = sizeof(msPromise),
    .is_cyclic = true,              /* Promise can form cycles via callbacks */
    .trace_fn = ms_promise_trace,
    .destroy_fn = ms_promise_destroy,
};

/* Exception trace - trace data field if it's a managed object */
static void ms_exception_trace(void* obj, msTraceCallback trace_cb) {
    msException* exc = (msException*)obj;
    if (exc == NULL || trace_cb == NULL) return;  /* Defensive */
    if (exc->data != NULL) {
        trace_cb(exc->data);
    }
}

/* Exception destroy - release data field if managed */
static void ms_exception_destroy(void* obj) {
    msException* exc = (msException*)obj;
    if (exc && exc->data) {
        ms_decref(exc->data);
        exc->data = NULL;
    }
    /* Note: message is assumed to be static/literal, not freed */
}

const msTypeInfo msException_type = {
    .name = "Exception",
    .size = sizeof(msException),
    .is_cyclic = false,             /* Exceptions don't form cycles */
    .trace_fn = ms_exception_trace,
    .destroy_fn = ms_exception_destroy,
};

/* ═══════════════════════════════════════════════════════════════════════════
 * EXCEPTION CREATION
 * ═══════════════════════════════════════════════════════════════════════════ */

msException* ms_exception_new(const char* message, void* data) {
    msException* exc = (msException*)ms_alloc(sizeof(msException));
    if (exc == NULL) return NULL;

    ms_set_type(exc, &msException_type);
    exc->message = message;  /* Borrowed pointer - caller ensures lifetime */
    exc->data = data;

    /* CRITICAL: If data is provided, incref it so exception owns a reference.
     * This allows caller to safely decref their reference if desired. */
    if (data != NULL) {
        ms_incref(data);
    }

    return exc;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * PROMISE CREATION
 * ═══════════════════════════════════════════════════════════════════════════ */

msPromise* ms_promise_new(const msTypeInfo* value_type) {
    msPromise* promise = (msPromise*)ms_alloc(sizeof(msPromise));
    if (promise == NULL) return NULL;

    /* Register type for ORC */
    ms_set_type(promise, &msPromise_type);

    /* Initialize state */
    promise->state = MS_PROMISE_PENDING;
    promise->value = NULL;
    promise->error = NULL;
    promise->_resolving = false;
    promise->has_rejection_handler = false;

    /* Initialize callback list (empty) */
    promise->callbacks.function = NULL;
    promise->callbacks.context = NULL;
    promise->callbacks.next = NULL;

    promise->value_type = value_type;

    return promise;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * CALLBACK MANAGEMENT
 * ═══════════════════════════════════════════════════════════════════════════ */

/* Maximum callbacks per promise (prevents OOM via adversarial callback spam) */
#define MS_MAX_CALLBACKS_PER_PROMISE 256

void ms_promise_add_callback(msPromise* promise,
                             msPromiseCallback callback,
                             void* context) {
    if (promise == NULL || callback == NULL) return;

    /* If already settled, schedule callback immediately via microtask */
    if (promise->state != MS_PROMISE_PENDING) {
        ms_enqueue_microtask(callback, context);
        return;
    }

    /* Count existing callbacks (prevent OOM via callback spam)
     * Also detects corrupted circular lists via iteration limit */
    size_t count = (promise->callbacks.function != NULL) ? 1 : 0;
    msCallbackNode* node = promise->callbacks.next;
    size_t iterations = 0;
    while (node && iterations < MS_MAX_CALLBACKS_PER_PROMISE + 1) {
        count++;
        node = node->next;
        iterations++;
    }

    /* Detect corrupted circular callback list */
    if (iterations > MS_MAX_CALLBACKS_PER_PROMISE) {
        fprintf(stderr, "[PROMISE] PANIC: Callback list corrupted (circular reference detected)\n");
        abort();
    }

    if (count >= MS_MAX_CALLBACKS_PER_PROMISE) {
        fprintf(stderr, "[PROMISE] PANIC: Too many callbacks on single promise (%d max)\n",
                MS_MAX_CALLBACKS_PER_PROMISE);
        fprintf(stderr, "          This may indicate a bug or adversarial input.\n");
        abort();
    }

    /* Add to callback list */
    if (promise->callbacks.function == NULL) {
        /* First callback - use inline slot */
        promise->callbacks.function = callback;
        promise->callbacks.context = context;
    } else {
        /* Additional callback - allocate node */
        msCallbackNode* node = (msCallbackNode*)malloc(sizeof(msCallbackNode));
        if (node == NULL) {
            fprintf(stderr, "[PROMISE] PANIC: Out of memory allocating callback node\n");
            fprintf(stderr, "          Silent callback loss is worse than crash.\n");
            abort();  /* CRITICAL: Never silently lose callbacks */
        }

        node->function = callback;
        node->context = context;
        node->next = promise->callbacks.next;
        promise->callbacks.next = node;
    }
}

void ms_promise_clear_callbacks(msPromise* promise) {
    if (promise == NULL) return;

    /* Clear inline callback */
    promise->callbacks.function = NULL;
    promise->callbacks.context = NULL;

    /* Free linked list */
    msCallbackNode* node = promise->callbacks.next;
    while (node != NULL) {
        msCallbackNode* next = node->next;
        free(node);
        node = next;
    }
    promise->callbacks.next = NULL;
}

void ms_promise_call_callbacks(msCallbackList* callbacks) {
    if (callbacks == NULL) return;

    /* Call inline callback first */
    if (callbacks->function != NULL) {
        ms_enqueue_microtask(callbacks->function, callbacks->context);
    }

    /* Call linked callbacks */
    msCallbackNode* node = callbacks->next;
    while (node != NULL) {
        if (node->function != NULL) {
            ms_enqueue_microtask(node->function, node->context);
        }
        node = node->next;
    }
}

/* ═══════════════════════════════════════════════════════════════════════════
 * RESOLVE / REJECT
 * ═══════════════════════════════════════════════════════════════════════════ */

void ms_promise_resolve(msPromise* promise, void* value) {
    if (promise == NULL) return;

    /* CRITICAL: Reentrancy guard FIRST to prevent race condition.
     * Must check/set guard before checking state to avoid window where
     * concurrent resolve could slip through. */
    if (promise->_resolving) {
        fprintf(stderr, "[PROMISE] PANIC: Reentrant promise resolution detected!\n");
        fprintf(stderr, "          This indicates a callback is resolving its own promise.\n");
        abort();
    }
    promise->_resolving = true;

    /* Idempotent - ignore if already settled (check AFTER guard set) */
    if (promise->state != MS_PROMISE_PENDING) {
        promise->_resolving = false;  /* Clean up guard */
        return;
    }

    /* Set state and value
     * CRITICAL: Take ownership of value by incrementing refcount */
    promise->state = MS_PROMISE_FULFILLED;
    promise->value = value;
    if (value != NULL) {
        ms_incref(value);  /* Promise now owns a reference to value */
    }

    /* Schedule all callbacks via microtask queue */
    ms_promise_call_callbacks(&promise->callbacks);

    /* CRITICAL: Clear callbacks to break cycles (Nim pattern) */
    ms_promise_clear_callbacks(promise);

    /* CRITICAL: Prevent use-after-free during microtask drain.
     * A callback might decref this promise to 0, freeing it.
     * We must keep it alive until we're done writing to it. */
    ms_incref(promise);

    /* Drain microtasks BEFORE clearing _resolving flag
     * This ensures any callback that tries to resolve this promise
     * will hit the reentrancy guard above */
    ms_drain_microtasks();

    promise->_resolving = false;

    /* Now safe to release our protective reference */
    ms_decref(promise);
}

void ms_promise_reject(msPromise* promise, msException* error) {
    if (promise == NULL) return;

    /* CRITICAL: Reentrancy guard FIRST (same as resolve) */
    if (promise->_resolving) {
        fprintf(stderr, "[PROMISE] PANIC: Reentrant promise rejection detected!\n");
        abort();
    }
    promise->_resolving = true;

    /* Idempotent - ignore if already settled (check AFTER guard set) */
    if (promise->state != MS_PROMISE_PENDING) {
        promise->_resolving = false;  /* Clean up guard */
        return;
    }

    /* Set state and error - take ownership */
    promise->state = MS_PROMISE_REJECTED;
    promise->error = error;
    if (error) ms_incref(error);  /* Promise owns a reference to error */

    /* Schedule all callbacks via microtask queue */
    ms_promise_call_callbacks(&promise->callbacks);

    /* CRITICAL: Clear callbacks to break cycles */
    ms_promise_clear_callbacks(promise);

    /* CRITICAL: Prevent use-after-free during microtask drain */
    ms_incref(promise);

    /* Drain microtasks BEFORE clearing flag (same as resolve) */
    ms_drain_microtasks();

    promise->_resolving = false;

    /* Now safe to release our protective reference */
    ms_decref(promise);
}

/* ═══════════════════════════════════════════════════════════════════════════
 * READ VALUE (for await)
 * ═══════════════════════════════════════════════════════════════════════════ */

void* ms_promise_read(msPromise* promise) {
    if (promise == NULL) {
        fprintf(stderr, "[PROMISE] ERROR: Reading NULL promise\n");
        return NULL;
    }

    switch (promise->state) {
        case MS_PROMISE_FULFILLED:
            /* CRITICAL: Return a new reference to caller.
             * Caller owns this reference and must decref when done.
             * This ensures value survives even if promise is freed. */
            if (promise->value != NULL) {
                ms_incref(promise->value);
            }
            return promise->value;

        case MS_PROMISE_REJECTED:
            /* Mark as handled (prevents unhandled rejection warning) */
            promise->has_rejection_handler = true;
            /* In real implementation, would throw/longjmp */
            {
                const char* msg = "(no message)";
                if (promise->error && promise->error->message) {
                    msg = promise->error->message;
                }
                fprintf(stderr, "[PROMISE] ERROR: Reading rejected promise: %s\n", msg);
            }
            return NULL;

        case MS_PROMISE_PENDING:
            fprintf(stderr, "[PROMISE] ERROR: Reading pending promise (await not complete)\n");
            return NULL;
    }

    return NULL;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * STATE QUERIES
 * ═══════════════════════════════════════════════════════════════════════════ */

bool ms_promise_is_settled(msPromise* promise) {
    if (promise == NULL) return false;
    return promise->state != MS_PROMISE_PENDING;
}

bool ms_promise_is_fulfilled(msPromise* promise) {
    if (promise == NULL) return false;
    return promise->state == MS_PROMISE_FULFILLED;
}

bool ms_promise_is_rejected(msPromise* promise) {
    if (promise == NULL) return false;
    return promise->state == MS_PROMISE_REJECTED;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * ORC INTEGRATION
 * ═══════════════════════════════════════════════════════════════════════════ */

void ms_promise_trace(void* obj, msTraceCallback trace_cb) {
    msPromise* promise = (msPromise*)obj;
    if (promise == NULL || trace_cb == NULL) return;  /* Defensive: check both */

    /* Trace value (promise owns a reference via incref in resolve) */
    if (promise->value != NULL) {
        trace_cb(promise->value);
    }

    /* Trace error (promise owns a reference via incref in reject) */
    if (promise->error != NULL) {
        trace_cb(promise->error);
    }

    /* IMPORTANT: Callback contexts are NOT traced here.
     *
     * Design decision: Callback contexts are owned by the CALLER, not the promise.
     * The caller (typically state machine codegen) is responsible for:
     * 1. Keeping the context alive until callback executes
     * 2. Releasing the context after callback completes
     *
     * This matches JavaScript semantics where closures capture variables
     * by reference, and those references are managed by the closure scope.
     *
     * For async state machines, the state struct holds its own reference
     * and is traced separately via its own TypeInfo.
     *
     * If we traced callback contexts here, we'd create spurious cycles
     * (promise → callback context → state machine → promise) that would
     * interfere with the intentional cycle-breaking in clear_callbacks().
     */
}

void ms_promise_destroy(void* obj) {
    msPromise* promise = (msPromise*)obj;
    if (promise == NULL) return;

    /* CRITICAL: Warn on unhandled rejection (helps debugging) */
    if (promise->state == MS_PROMISE_REJECTED && !promise->has_rejection_handler) {
        const char* msg = "(no message)";
        if (promise->error && promise->error->message) {
            msg = promise->error->message;
        }
        fprintf(stderr, "[PROMISE] WARNING: Unhandled promise rejection: %s\n", msg);
    }

    /* Clear callbacks (if any remain) */
    ms_promise_clear_callbacks(promise);

    /* Release value reference (promise took ownership in resolve) */
    if (promise->value != NULL) {
        ms_decref(promise->value);
        promise->value = NULL;
    }

    /* Release error reference (promise took ownership in reject) */
    if (promise->error != NULL) {
        ms_decref(promise->error);
        promise->error = NULL;
    }
}
