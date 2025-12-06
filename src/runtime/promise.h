/*
 * Metascript Promise Runtime
 *
 * TypeScript-compatible Promise<T> implementation for the C backend.
 * Integrates with ORC (Optimized Reference Counting) for cycle detection.
 *
 * Design adapted from Nim's asyncfutures.nim (battle-tested patterns).
 *
 * Key Features:
 * - Inline first callback (avoids allocation for common single-callback case)
 * - Callbacks cleared on resolve/reject (breaks promise→callback→promise cycles)
 * - Reentrancy guard (prevents use-after-free during callback execution)
 * - Unhandled rejection tracking (warns on GC if no .catch())
 *
 * ═══════════════════════════════════════════════════════════════════════════
 * THREAD SAFETY: This implementation is NOT thread-safe.
 * ═══════════════════════════════════════════════════════════════════════════
 * - All promise operations must occur on a single thread
 * - The microtask queue is global and unprotected
 * - For multi-threaded use, each thread needs its own event loop
 * - This matches JavaScript's single-threaded event loop model
 *
 * ═══════════════════════════════════════════════════════════════════════════
 * CALLBACK CONTEXT OWNERSHIP
 * ═══════════════════════════════════════════════════════════════════════════
 * Callback contexts (void* passed to ms_promise_add_callback) are NOT owned
 * by the promise. The CALLER is responsible for:
 *   1. Keeping the context alive until the callback executes
 *   2. Releasing the context after callback completes (if managed)
 *
 * For async state machines (codegen), the state struct holds its own reference
 * and is traced separately. The promise does NOT trace callback contexts to
 * avoid spurious cycles that would interfere with cycle-breaking on resolve.
 *
 * Safe patterns:
 *   - Static/global context: Always valid, no ownership concerns
 *   - Stack context: Only if promise resolves before stack frame exits
 *   - Heap context: Caller must ensure lifetime exceeds callback execution
 */

#ifndef MS_PROMISE_H
#define MS_PROMISE_H

#include "orc.h"
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ═══════════════════════════════════════════════════════════════════════════
 * CALLBACK TYPES
 * ═══════════════════════════════════════════════════════════════════════════ */

/* Forward declarations */
typedef struct msPromise msPromise;
typedef struct msCallbackNode msCallbackNode;
typedef struct msCallbackList msCallbackList;

/* Callback function signature */
typedef void (*msPromiseCallback)(void* context);

/* ═══════════════════════════════════════════════════════════════════════════
 * CALLBACK LIST (Nim-inspired inline-first pattern)
 *
 * Most promises have exactly 1 callback. Inline storage avoids allocation.
 * Additional callbacks form a linked list.
 * ═══════════════════════════════════════════════════════════════════════════ */

/* Linked list node for additional callbacks (2nd, 3rd, ...) */
struct msCallbackNode {
    msPromiseCallback function;
    void* context;
    msCallbackNode* next;           /* Owned pointer - freed on clear */
};

/* Callback list with inline first element */
struct msCallbackList {
    msPromiseCallback function;     /* First callback (inline, no alloc) */
    void* context;
    msCallbackNode* next;           /* Rest of chain (heap allocated) */
};

/* ═══════════════════════════════════════════════════════════════════════════
 * EXCEPTION TYPE
 *
 * OWNERSHIP MODEL:
 * - message: Borrowed pointer (assumed static/literal, NOT freed)
 * - data: OWNED by exception (exception calls ms_decref on destroy)
 *
 * CRITICAL: If you set exc->data, you are TRANSFERRING OWNERSHIP.
 * The exception will decref data when destroyed. If you need to keep a
 * reference, incref BEFORE assigning:
 *
 *   ms_incref(my_data);        // Keep our reference
 *   exc->data = my_data;       // Transfer one reference to exception
 *
 * Or use ms_exception_new() which handles this correctly.
 * ═══════════════════════════════════════════════════════════════════════════ */

/* Exception type for rejected promises */
typedef struct msException {
    msRefHeader header;
    const char* message;            /* Borrowed: static/literal string (NOT freed) */
    void* data;                     /* Owned: managed object (decref'd on destroy) */
} msException;

/* Exception type info - MUST be used when creating exceptions */
extern const msTypeInfo msException_type;

/* Create exception with optional managed data.
 * - message: borrowed pointer (must remain valid for exception lifetime)
 * - data: if non-NULL, exception takes ownership (increfs it)
 * Returns exception with rc=1. Caller must ms_decref when done. */
msException* ms_exception_new(const char* message, void* data);

/* Promise states */
typedef enum msPromiseState {
    MS_PROMISE_PENDING,
    MS_PROMISE_FULFILLED,
    MS_PROMISE_REJECTED
} msPromiseState;

/* Promise type - integrates with ORC */
struct msPromise {
    msRefHeader header;             /* ORC header (refcount, type info) */

    /* State */
    msPromiseState state;           /* pending, fulfilled, or rejected */

    /* Result (union would save space but void* is simpler) */
    void* value;                    /* Result when fulfilled */
    msException* error;             /* Error when rejected */

    /* Safety flags */
    bool _resolving;                /* Reentrancy guard (prevents use-after-free) */
    bool has_rejection_handler;     /* For unhandled rejection warning on destroy */

    /* Callbacks */
    msCallbackList callbacks;       /* Inline first + linked rest */

    /* Type info for value cleanup */
    const msTypeInfo* value_type;
};

/* ═══════════════════════════════════════════════════════════════════════════
 * PROMISE OPERATIONS - CORE (Phase 1)
 * ═══════════════════════════════════════════════════════════════════════════ */

/* Create new pending promise */
msPromise* ms_promise_new(const msTypeInfo* value_type);

/* Add callback - if already settled, schedules via microtask queue */
void ms_promise_add_callback(msPromise* promise,
                             msPromiseCallback callback,
                             void* context);

/* Resolve with value
 * - Calls all callbacks via microtask queue
 * - CLEARS callbacks after calling (breaks cycles)
 * - Idempotent (second call is no-op)
 * - Panics on reentrant call (during callback execution)
 */
void ms_promise_resolve(msPromise* promise, void* value);

/* Reject with error
 * - Same semantics as resolve but sets error instead of value
 */
void ms_promise_reject(msPromise* promise, msException* error);

/* Read value (for await)
 * - Returns value if fulfilled (with NEW REFERENCE - caller must decref)
 * - Returns NULL if pending or rejected
 * - Marks has_rejection_handler = true if rejected (error was observed)
 *
 * OWNERSHIP: Caller receives a new reference to the value.
 *            Caller MUST call ms_decref() on the returned value when done.
 *            This ensures value survives even if promise is freed.
 */
void* ms_promise_read(msPromise* promise);

/* Check if promise is settled (fulfilled or rejected) */
bool ms_promise_is_settled(msPromise* promise);

/* Check if promise is fulfilled */
bool ms_promise_is_fulfilled(msPromise* promise);

/* Check if promise is rejected */
bool ms_promise_is_rejected(msPromise* promise);

/* ═══════════════════════════════════════════════════════════════════════════
 * ORC INTEGRATION
 * ═══════════════════════════════════════════════════════════════════════════ */

/* Trace function for ORC cycle detection
 * Traverses: value, error, and callback chain contexts
 */
void ms_promise_trace(void* obj, msTraceCallback trace_cb);

/* Destroy function - warns on unhandled rejection */
void ms_promise_destroy(void* obj);

/* Type info for Promise (is_cyclic = true) */
extern const msTypeInfo msPromise_type;

/* ═══════════════════════════════════════════════════════════════════════════
 * INTERNAL HELPERS
 * ═══════════════════════════════════════════════════════════════════════════ */

/* Clear all callbacks (breaks cycles) - called after resolve/reject */
void ms_promise_clear_callbacks(msPromise* promise);

/* Call all callbacks via microtask queue */
void ms_promise_call_callbacks(msCallbackList* callbacks);

#ifdef __cplusplus
}
#endif

#endif /* MS_PROMISE_H */
