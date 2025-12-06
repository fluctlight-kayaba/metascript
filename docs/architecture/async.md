# Async/Await Architecture

Metascript's async/await is **TypeScript/JavaScript compatible by design**. Same syntax, same semantics, same `Promise<T>` type.

## Design Principle: Backend-Appropriate Implementation

```
┌─────────────────────────────────────────────────────────────────┐
│  SAME METASCRIPT CODE → 3 BACKEND-APPROPRIATE IMPLEMENTATIONS   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  async function fetchUser(id: number): Promise<User> {          │
│      const resp = await fetch(`/users/${id}`);                  │
│      return await resp.json();                                  │
│  }                                                              │
│                                                                 │
├──────────────┬──────────────────┬───────────────────────────────┤
│  C Backend   │  JS Backend      │  Erlang Backend               │
├──────────────┼──────────────────┼───────────────────────────────┤
│  State       │  PASSTHROUGH!    │  gen_server /                 │
│  machine +   │  Emit native     │  process +                    │
│  ORC + event │  async/await     │  message passing              │
│  loop        │  (zero work)     │                               │
└──────────────┴──────────────────┴───────────────────────────────┘
```

**Key Insight:** JavaScript backend requires NO transformation - just emit the async/await code directly. The runtime already has native Promise support!

## Full Promise API (JavaScript Compatible)

### Constructor
```typescript
// Create promise with executor
const p = new Promise<T>((resolve, reject) => {
    // async work...
    resolve(value);  // or reject(error)
});
```

### Instance Methods
```typescript
promise
    .then(onFulfilled, onRejected)  // Returns new Promise
    .catch(onRejected)               // Sugar for .then(undefined, onRejected)
    .finally(onFinally);             // Runs regardless, returns same result
```

### Static Methods
```typescript
// Wrap values
Promise.resolve(value)    // → fulfilled Promise<T>
Promise.reject(error)     // → rejected Promise<never>

// Combinators
Promise.all([p1, p2])        // All must fulfill, fail-fast on first reject
Promise.allSettled([p1, p2]) // Wait for all, return {status, value/reason}[]
Promise.race([p1, p2])       // First to settle (fulfill or reject) wins
Promise.any([p1, p2])        // First to fulfill wins, AggregateError if all reject
```

### Full API Table

| API | JS Backend | C Backend | Erlang Backend |
|-----|------------|-----------|----------------|
| `new Promise(executor)` | Native | `ms_promise_new()` + executor call | `spawn` process |
| `.then(onF, onR)` | Native | `ms_promise_then()` | `receive` + transform |
| `.catch(onR)` | Native | Sugar for `.then(nil, onR)` | `receive` with error match |
| `.finally(onF)` | Native | `ms_promise_finally()` | After `receive` |
| `Promise.resolve(v)` | Native | `ms_promise_resolved(v)` | `{ok, V}` tuple |
| `Promise.reject(e)` | Native | `ms_promise_rejected(e)` | `{error, E}` tuple |
| `Promise.all([])` | Native | `ms_promise_all()` | Parallel spawns + collect |
| `Promise.allSettled([])` | Native | `ms_promise_all_settled()` | Parallel + tag results |
| `Promise.race([])` | Native | `ms_promise_race()` | First process wins |
| `Promise.any([])` | Native | `ms_promise_any()` | First success wins |

**Result:** TypeScript developers use Promise exactly as they expect. Zero learning curve.

## Backend Strategies

### JavaScript Backend (Simplest)
```typescript
// Input (Metascript)
async function fetchUser(id: number): Promise<User> {
    const resp = await fetch(`/users/${id}`);
    return await resp.json();
}

// Output (JavaScript) - IDENTICAL!
async function fetchUser(id) {
    const resp = await fetch(`/users/${id}`);
    return await resp.json();
}
```
**Work required:** Strip types, emit as-is. Native Promise handles everything.

### C Backend (State Machine + ORC)
Uses Nim-inspired state machine transformation with ORC memory management.
See detailed implementation below.

### Erlang Backend (Process-Based)
```erlang
%% Maps to Erlang's natural concurrency model
fetch_user(Id) ->
    spawn(fun() ->
        Resp = http:get("/users/" ++ integer_to_list(Id)),
        Json = json:decode(Resp),
        {ok, Json}
    end).
```
**Work required:** Map Promise to process, await to receive.

---

## C Backend: Detailed Design

The C backend needs explicit implementation since C has no native async. We adapt Nim's proven patterns.

## Reference: Nim's Proven Patterns

Studied from `~/projects/Nim/lib/pure/`:
- `asyncfutures.nim` - Future type (our `Promise`), callback management
- `asyncmacro.nim` - async/await transformation
- `asyncdispatch.nim` - Event loop, dispatcher
- `tests/arc/tasyncorc.nim`, `tasyncleak*.nim` - ORC memory tests

## Core Architecture

```
┌─────────────────────────────────────────────────────────┐
│           ASYNC + ORC = 3 KEY ADDITIONS                 │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  1. COMPILE: async/await → Closure Iterator             │
│              (yield at await, callback resumes)         │
│                                                         │
│  2. DRC:     Track captures across await                │
│              (captured = shared, not elided)            │
│                                                         │
│  3. ORC:     Clear callbacks on Promise.resolve()       │
│              (breaks promise→callback→promise cycles)   │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

## Nim's Key Design Decisions (Adopted)

### 1. CallbackList - Inline First Element

```nim
# Nim's design: First callback inline, rest linked
type
  CallbackList = object
    function: CallbackFunc           # Inline (no alloc for 1 callback)
    next: owned(ref CallbackList)    # Owned pointer for chain
```

**Why:** Most promises have 1 callback. Inline avoids allocation.

### 2. Clear Callbacks Immediately

```nim
proc call(callbacks: var CallbackList) =
  var current = callbacks
  while true:
    if not current.function.isNil:
      callSoon(current.function)
    if current.next.isNil:
      break
    else:
      current = current.next[]
  # CRITICAL: Break cycles immediately
  callbacks.next = nil
  callbacks.function = nil
```

**Why:** Callbacks often capture the promise → cycle. Clear immediately.

### 3. Closure Iterator Pattern

```nim
# Nim transforms async proc to:
# 1. Closure iterator (yields at each await)
# 2. Continue callback (resumes iterator)
# 3. Return promise immediately

proc asyncProc(): Promise[T] =
  var retPromise = newPromise[T]()

  iterator asyncProcIter(): PromiseBase {.closure.} =
    # ... body with yield at each await ...
    resolve(retPromise, result)

  proc asyncProcContinue() {.closure.} =
    if not iter.finished:
      var next = iter()
      if next != nil and not next.finished:
        next.addCallback(asyncProcContinue)

  asyncProcContinue()
  return retPromise
```

### 4. ORC Annotations

| Annotation | Purpose | Metascript Equivalent |
|------------|---------|----------------------|
| `owned(ref T)` | Explicit ownership | Default (shared by default) |
| `{.cursor.}` | Borrow without RC | DRC elision / `move` |
| `{.acyclic.}` | Skip cycle detection | `is_cyclic = false` in TypeInfo |

---

## Metascript Implementation

### Phase 1: Promise Type (src/runtime/promise.h)

```c
/* Callback function type */
typedef void (*msPromiseCallback)(void* context);

/* Callback list - inline first, linked rest (Nim pattern) */
typedef struct msCallbackNode {
    msPromiseCallback function;
    void* context;
    struct msCallbackNode* next;  /* owned pointer */
} msCallbackNode;

typedef struct msCallbackList {
    msPromiseCallback function;   /* Inline first callback */
    void* context;
    msCallbackNode* next;         /* Rest of chain */
} msCallbackList;

/* Promise type */
typedef struct msPromise {
    msObjHeader header;           /* ORC header */
    void* value;                  /* Result when resolved */
    msException* error;           /* Error if rejected */
    bool settled;                 /* resolved or rejected */
    bool _resolving;              /* 🔴 Reentrancy guard (prevents use-after-free) */
    bool has_rejection_handler;   /* 🔴 For unhandled rejection warning */
    msCallbackList callbacks;     /* Inline callback list */
    const msTypeInfo* value_type; /* For cleanup */
} msPromise;

/* Type info - Promise is cyclic */
static const msTypeInfo msPromise_type = {
    .name = "Promise",
    .size = sizeof(msPromise),
    .is_cyclic = true,
    .trace_fn = ms_promise_trace,
    .destroy_fn = ms_promise_destroy,
};
```

### Phase 2: Promise Operations

```c
/* ═══════════════════════════════════════════════════════════════
   CORE OPERATIONS
   ═══════════════════════════════════════════════════════════════ */

/* Create new pending promise */
msPromise* ms_promise_new(const msTypeInfo* value_type);

/* Create with executor: new Promise((resolve, reject) => {...}) */
msPromise* ms_promise_with_executor(
    void (*executor)(msPromiseCallback resolve,
                     msPromiseCallback reject,
                     void* context),
    void* context,
    const msTypeInfo* value_type);

/* Resolve with value - calls all callbacks, then CLEARS them */
void ms_promise_resolve(msPromise* promise, void* value);

/* Reject with error */
void ms_promise_reject(msPromise* promise, msException* error);

/* Read value (throws if pending or rejected) */
void* ms_promise_read(msPromise* promise);

/* ═══════════════════════════════════════════════════════════════
   INSTANCE METHODS (.then, .catch, .finally)
   ═══════════════════════════════════════════════════════════════ */

/* .then(onFulfilled, onRejected) - returns NEW promise */
msPromise* ms_promise_then(
    msPromise* promise,
    void* (*onFulfilled)(void* value),     /* nullable */
    void* (*onRejected)(msException* err), /* nullable */
    const msTypeInfo* result_type);

/* .catch(onRejected) - sugar for .then(nil, onRejected) */
msPromise* ms_promise_catch(
    msPromise* promise,
    void* (*onRejected)(msException* err),
    const msTypeInfo* result_type);

/* .finally(onFinally) - runs regardless, passes through result */
msPromise* ms_promise_finally(
    msPromise* promise,
    void (*onFinally)(void));

/* ═══════════════════════════════════════════════════════════════
   STATIC METHODS (Promise.resolve, Promise.all, etc.)
   ═══════════════════════════════════════════════════════════════ */

/* Promise.resolve(value) - wrap value in fulfilled promise */
msPromise* ms_promise_resolved(void* value, const msTypeInfo* type);

/* Promise.reject(error) - wrap error in rejected promise */
msPromise* ms_promise_rejected(msException* error);

/* Promise.all([p1, p2, ...]) - all must fulfill, fail-fast */
msPromise* ms_promise_all(
    msPromise** promises,
    size_t count,
    const msTypeInfo* element_type);

/* Promise.allSettled([...]) - wait for all, return status array */
msPromise* ms_promise_all_settled(
    msPromise** promises,
    size_t count);

/* Promise.race([...]) - first to settle wins */
msPromise* ms_promise_race(
    msPromise** promises,
    size_t count,
    const msTypeInfo* element_type);

/* Promise.any([...]) - first to fulfill wins */
msPromise* ms_promise_any(
    msPromise** promises,
    size_t count,
    const msTypeInfo* element_type);

/* ═══════════════════════════════════════════════════════════════
   INTERNAL (used by codegen)
   ═══════════════════════════════════════════════════════════════ */

/* Add raw callback (for state machine continuation) */
void ms_promise_add_callback(msPromise* promise,
                             msPromiseCallback cb,
                             void* context);

/* Clear callbacks (break cycles) */
void ms_promise_clear_callbacks(msPromise* promise);
```

### Phase 3: State Machine Codegen

**Input (Metascript):**
```typescript
async function fetchUser(id: number): Promise<User> {
    const response = await fetch(`/users/${id}`);
    const data = await response.json();
    return new User(data);
}
```

**Output (C):**
```c
/* State machine struct */
typedef struct fetchUser_State {
    msObjHeader header;
    int _state;
    msPromise* _retPromise;
    /* Parameters */
    int64_t id;
    /* Captures (live across await) */
    msResponse* response;
    void* data;
    /* Pending promises */
    msPromise* _pending;
} fetchUser_State;

/* Trace function for ORC */
void fetchUser_State_trace(void* obj, msTraceCallback cb) {
    fetchUser_State* s = (fetchUser_State*)obj;
    if (s->_retPromise) cb(s->_retPromise);
    if (s->response) cb(s->response);
    if (s->data) cb(s->data);
    if (s->_pending) cb(s->_pending);
}

static const msTypeInfo fetchUser_State_type = {
    .name = "fetchUser_State",
    .size = sizeof(fetchUser_State),
    .is_cyclic = true,
    .trace_fn = fetchUser_State_trace,
};

/* Continue callback - resumes state machine */
void fetchUser_continue(void* ctx) {
    fetchUser_State* s = (fetchUser_State*)ctx;

    switch (s->_state) {
    case 0: {
        /* Initial: call fetch() */
        msString* url = ms_string_format("/users/%lld", s->id);
        s->_pending = ms_fetch(url);
        ms_decref(url);
        s->_state = 1;
        ms_promise_then(s->_pending, fetchUser_continue, s);
        return;  /* YIELD */
    }
    case 1: {
        /* After fetch: read result, call json() */
        s->response = (msResponse*)ms_promise_read(s->_pending);
        ms_incref(s->response);
        ms_decref(s->_pending);

        s->_pending = ms_response_json(s->response);
        s->_state = 2;
        ms_promise_then(s->_pending, fetchUser_continue, s);
        return;  /* YIELD */
    }
    case 2: {
        /* After json: create User, resolve */
        s->data = ms_promise_read(s->_pending);
        ms_decref(s->_pending);
        s->_pending = NULL;

        msUser* user = ms_User_new(s->data);
        ms_promise_resolve(s->_retPromise, user);

        /* Cleanup captures */
        ms_decref(s->response);
        s->_state = -1;  /* Done */
        ms_decref(s);    /* Release self */
        return;
    }
    }
}

/* Entry point */
msPromise* fetchUser(int64_t id) {
    fetchUser_State* s = (fetchUser_State*)ms_alloc(sizeof(fetchUser_State));
    ms_set_type(s, &fetchUser_State_type);

    s->_state = 0;
    s->id = id;
    s->_retPromise = ms_promise_new(&msUser_type);
    s->response = NULL;
    s->data = NULL;
    s->_pending = NULL;

    /* Start state machine (holds ref to self) */
    ms_incref(s);
    fetchUser_continue(s);

    /* Return promise (caller owns it) */
    msPromise* result = s->_retPromise;
    ms_incref(result);
    ms_decref(s);
    return result;
}
```

### Phase 4: Event Loop

```c
/* Global dispatcher (per-thread) */
typedef struct msDispatcher {
    msCallbackNode* pending;      /* Callbacks to run */
    /* Platform-specific: epoll/kqueue/IOCP handle */
    void* platform_handle;
} msDispatcher;

/* Get thread-local dispatcher */
msDispatcher* ms_get_dispatcher(void);

/* Schedule callback for next poll */
void ms_call_soon(msPromiseCallback cb, void* ctx);

/* Poll for events, run callbacks */
bool ms_poll(int timeout_ms);

/* Block until promise resolves */
void* ms_wait_for(msPromise* promise);

/* Run forever */
void ms_run_forever(void);
```

---

## DRC Integration

### Capture Analysis

```
async function example() {
    let a = 1;              // NOT captured (not used after await)
    let b = getUser();      // CAPTURED (used after await)

    await delay(100);       // ─── SUSPENSION POINT ───

    let c = process(b);     // NOT captured (created after await)
    return c;
}
```

**DRC must identify:**
- Variables used AFTER any await → captured in state struct
- Captured variables → shared RC (cannot elide)
- Non-captured → normal DRC optimization applies

### Cycle Detection Hints

```typescript
let promise = doAsync();
promise.then(() => {
    use(promise);  // ⚠️ Captures 'promise' → cycle
});
```

**DRC emits hint (not error):** Callback captures promise, ORC will handle.

---

## Implementation Roadmap

### Priority: Core async/await FIRST, full Promise API LATER

```
PHASE 1 (Weeks 1-8): Core Async/Await
────────────────────────────────────────
✓ async function / await expression
✓ Basic Promise (resolve, reject)
✓ State machine transformation
✓ ORC integration (cycle breaking)
✓ C backend working end-to-end

PHASE 2 (Later): Full Promise API
────────────────────────────────────────
○ .then() / .catch() / .finally() chaining
○ Promise.all() / Promise.race()
○ Promise.allSettled() / Promise.any()
○ new Promise(executor) pattern
```

---

### 🔴 CRITICAL FIXES (From Principal Engineer Review)

These must be implemented for correctness and JS compatibility:

| Fix | Week | Issue | Solution |
|-----|------|-------|----------|
| **Reentrancy Guard** | 1 | Use-after-free if resolve() called during callback | Add `_resolving` flag, panic on reentrant call |
| **Microtask Queue** | 2 | Wrong execution order vs JS | Separate microtask/macrotask queues |
| **Thenable Adoption** | 7 | `Promise.resolve(promise)` must flatten | Check if value is Promise, chain resolution |
| **Unhandled Rejection** | 8 | Silent errors hurt debugging | Warn on GC if rejected + no handler |

---

### Week 1: Promise Runtime - CORE ONLY (src/runtime/promise.h)

- [ ] **TODO 1.1**: Define `msCallbackList` struct (inline first pattern)
- [ ] **TODO 1.2**: Define `msPromise` struct with ORC header
- [ ] **TODO 1.3**: Implement `ms_promise_new()`
- [ ] **TODO 1.4**: Implement `ms_promise_add_callback()` (internal, for state machine)
- [ ] **TODO 1.5**: Implement `ms_promise_resolve()` with callback clearing
- [ ] **TODO 1.6**: Implement `ms_promise_reject()`
- [ ] **TODO 1.7**: Implement `ms_promise_read()`
- [ ] **TODO 1.8**: Implement `ms_promise_trace()` for ORC
- [ ] **TODO 1.9**: Write tests: basic resolve/read
- [ ] **TODO 1.10**: Write tests: callback cycles freed correctly
- [ ] **🔴 TODO 1.11**: Add reentrancy guard to `ms_promise_resolve()`:
  ```c
  typedef struct msPromise {
      // ...
      bool _resolving;  // Reentrancy guard
  } msPromise;

  void ms_promise_resolve(msPromise* p, void* value) {
      if (p->settled) return;  // Idempotent
      if (p->_resolving) ms_panic("Reentrant promise resolution");
      p->_resolving = true;
      // ... resolve logic ...
      p->_resolving = false;
  }
  ```
- [ ] **🔴 TODO 1.12**: Document: state machines are NEVER moved after allocation
- [ ] **🔴 TODO 1.13**: Write leak tests EARLY (10k async ops, memory stable)

### Week 2: Event Loop (src/runtime/dispatcher.h)

- [ ] **TODO 2.1**: Define `msDispatcher` struct with SEPARATE queues:
  ```c
  typedef struct msDispatcher {
      msCallbackNode* microtasks;   // Promise callbacks (drain after EACH task)
      msCallbackNode* macrotasks;   // I/O, timers (one per poll)
      void* platform_handle;
  } msDispatcher;
  ```
- [ ] **🔴 TODO 2.2**: Implement `ms_enqueue_microtask()` (for promise callbacks)
- [ ] **🔴 TODO 2.3**: Implement `ms_enqueue_macrotask()` (for I/O, timers)
- [ ] **🔴 TODO 2.4**: Implement `ms_poll()` with correct drain order:
  ```c
  bool ms_poll(int timeout_ms) {
      // 1. Run ONE macrotask (I/O, timer)
      // 2. Drain ALL microtasks (loop until empty)
      // 3. Microtasks enqueued during drain also run
  }
  ```
- [ ] **TODO 2.5**: Implement `ms_wait_for()` (block until settled)
- [ ] **TODO 2.6**: Platform: macOS kqueue support
- [ ] **TODO 2.7**: Platform: Linux epoll support (optional)
- [ ] **TODO 2.8**: Timer support: `ms_sleep_async()`
- [ ] **🔴 TODO 2.9**: Write test: microtask ordering matches JS:
  ```typescript
  console.log('1');
  Promise.resolve().then(() => console.log('2'));
  console.log('3');
  // MUST output: 1, 3, 2 (NOT 1, 2, 3)
  ```

### Week 3: Parser & AST (src/parser/, src/ast/)

- [ ] **TODO 3.1**: Lexer: `async` and `await` keywords
- [ ] **TODO 3.2**: Parser: async function declaration
- [ ] **TODO 3.3**: Parser: await expression
- [ ] **TODO 3.4**: AST: `AsyncFunctionDecl` node
- [ ] **TODO 3.5**: AST: `AwaitExpr` node
- [ ] **TODO 3.6**: Write tests: parse async/await syntax

### Week 4: Type Checker (src/checker/)

- [ ] **TODO 4.1**: `Promise<T>` generic type
- [ ] **TODO 4.2**: Check: await only in async functions
- [ ] **TODO 4.3**: Check: awaited expr is `Promise<T>`
- [ ] **TODO 4.4**: Check: async function returns `Promise<T>`
- [ ] **TODO 4.5**: Infer return type from async body
- [ ] **TODO 4.6**: Write tests: type errors for async

### Week 5: DRC Capture Analysis (src/analysis/drc.zig)

- [ ] **TODO 5.1**: Identify await suspension points
- [ ] **TODO 5.2**: Track variable liveness across await
- [ ] **TODO 5.3**: Mark captured variables as shared
- [ ] **TODO 5.4**: Emit hint for callback-captures-promise pattern
- [ ] **TODO 5.5**: Write tests: capture detection

### Week 6: Async Transform (src/transform/async.zig)

- [ ] **TODO 6.1**: Create state machine struct from captures
- [ ] **TODO 6.2**: Generate state enum from await points
- [ ] **TODO 6.3**: Transform async body to switch statement
- [ ] **TODO 6.4**: Insert yield/resume logic at await
- [ ] **TODO 6.5**: Generate entry point function
- [ ] **TODO 6.6**: Write tests: transformation correctness

### Week 7: C Codegen (src/codegen/c/)

- [ ] **🔴 TODO 7.0**: Implement Promise Resolution Procedure (Promises/A+ §2.3):
  ```c
  void ms_promise_resolve(msPromise* p, void* value) {
      // Check if value is itself a Promise (thenable adoption)
      if (ms_is_promise(value)) {
          msPromise* inner = (msPromise*)value;
          // Adopt inner's state when it settles
          ms_promise_add_callback(inner, adopt_state_callback, p);
          return;
      }
      // Normal resolution...
  }
  ```
- [ ] **🔴 TODO 7.0.1**: Write test: `Promise.resolve(Promise.resolve(42))` → `42` (flattened)
- [ ] **TODO 7.1**: Generate state struct with ORC header
- [ ] **TODO 7.2**: Generate trace function for state
- [ ] **TODO 7.3**: Generate continue callback
- [ ] **TODO 7.4**: Generate entry point
- [ ] **TODO 7.5**: Handle try/catch in async (error propagation)
- [ ] **TODO 7.6**: Write tests: compile and run async C code

### Week 8: Integration & Polish

- [ ] **🔴 TODO 8.0**: Implement unhandled rejection tracking:
  ```c
  typedef struct msPromise {
      // ...
      bool has_rejection_handler;  // Set by .catch() or await
  } msPromise;

  void ms_promise_destroy(void* obj) {
      msPromise* p = (msPromise*)obj;
      if (p->settled && p->error && !p->has_rejection_handler) {
          ms_warn("Unhandled promise rejection: %s", p->error->message);
      }
      // ... cleanup
  }
  ```
- [ ] **🔴 TODO 8.0.1**: Write test: rejected promise without handler logs warning
- [ ] **TODO 8.1**: LSP: Hover info for async functions
- [ ] **TODO 8.2**: LSP: Diagnostics for async errors
- [ ] **TODO 8.3**: Documentation: async/await guide
- [ ] **TODO 8.4**: Integration tests: real async programs
- [ ] **TODO 8.5**: Memory leak tests with valgrind/ASAN
- [ ] **TODO 8.6**: JS backend: verify passthrough works

---

### PHASE 2: Full Promise API (Post-Core)

- [ ] **TODO P2.1**: `ms_promise_then()` - returns new chained Promise
- [ ] **TODO P2.2**: `ms_promise_catch()` - error handling chain
- [ ] **TODO P2.3**: `ms_promise_finally()` - cleanup chain
- [ ] **TODO P2.4**: `ms_promise_resolved()` - static factory
- [ ] **TODO P2.5**: `ms_promise_rejected()` - static factory
- [ ] **TODO P2.6**: `ms_promise_all()` - parallel wait
- [ ] **TODO P2.7**: `ms_promise_race()` - first wins
- [ ] **TODO P2.8**: `ms_promise_all_settled()` - wait all, return statuses
- [ ] **TODO P2.9**: `ms_promise_any()` - first success wins
- [ ] **TODO P2.10**: `ms_promise_with_executor()` - `new Promise(...)` pattern

---

## Test Cases (Critical)

### Memory Safety Tests

```c
/* Test 1: Simple promise resolves, no leaks */
void test_promise_resolve_no_leak(void) {
    msPromise* p = ms_promise_new(&int_type);
    int value = 42;
    ms_promise_resolve(p, &value);
    ms_decref(p);
    ms_report_leaks();  /* Should report 0 */
}

/* Test 2: Callback captures promise, cycle collected */
void test_callback_cycle(void) {
    msPromise* p = ms_promise_new(&int_type);

    /* Callback captures p (creates cycle) */
    ms_promise_then(p, callback_that_uses_promise, p);
    ms_incref(p);  /* Callback's reference */

    ms_promise_resolve(p, &value);
    ms_decref(p);  /* Our reference */

    /* Cycle should be broken by resolve() clearing callbacks */
    ms_report_leaks();  /* Should report 0 */
}

/* Test 3: Many async calls, stable memory */
void test_async_memory_stable(void) {
    size_t before = ms_get_allocated();

    for (int i = 0; i < 10000; i++) {
        msPromise* p = async_operation();
        ms_wait_for(p);
        ms_decref(p);
    }

    ms_collect_cycles();
    size_t after = ms_get_allocated();

    assert(after <= before + 1024);  /* Allow small variance */
}
```

### Functional Tests

```typescript
/* Test: Sequential await */
async function test_sequential(): Promise<number> {
    const a = await getValue(1);
    const b = await getValue(2);
    return a + b;
}

/* Test: Error propagation */
async function test_error(): Promise<void> {
    try {
        await failingOperation();
    } catch (e) {
        console.log("Caught:", e);
    }
}

/* Test: Nested async */
async function test_nested(): Promise<string> {
    const user = await fetchUser(1);
    const posts = await fetchPosts(user.id);
    return posts[0].title;
}
```

---

## Key Invariants (MUST Maintain)

1. **Callbacks cleared on resolve/reject** - Always, no exceptions
2. **State machine refs self during execution** - incref on start, decref on done
3. **Promise refs cleared on read** - Pending promise decref'd after read
4. **Captured vars have shared RC** - DRC cannot elide
5. **Error propagates through chain** - reject() calls callbacks too
6. **Dispatcher is thread-local** - No global state races
7. **🔴 No reentrant resolution** - `_resolving` guard prevents use-after-free
8. **🔴 State machines never moved** - Heap-allocated, fixed address after init
9. **🔴 Microtasks drain before macrotasks** - JS-compatible execution order
10. **🔴 Promise.resolve(promise) flattens** - Thenable adoption (Promises/A+)

---

## File Structure

```
src/
├── runtime/
│   ├── promise.h         # Promise type, callbacks (Week 1)
│   ├── dispatcher.h      # Event loop (Week 2)
│   └── orc.h            # Existing ORC (unchanged)
├── parser/
│   └── parser.zig       # async/await parsing (Week 3)
├── ast/
│   └── nodes.zig        # AsyncFunctionDecl, AwaitExpr (Week 3)
├── checker/
│   └── typechecker.zig  # Promise<T> type checking (Week 4)
├── analysis/
│   └── drc.zig          # Capture analysis (Week 5)
├── transform/
│   └── async.zig        # State machine transform (Week 6) [NEW]
└── codegen/c/
    └── cgen.zig         # Async C generation (Week 7)

tests/
├── runtime/
│   ├── test_promise.c   # Promise unit tests
│   └── test_async_orc.c # Memory safety tests
└── async/
    ├── test_sequential.ms
    ├── test_error.ms
    └── test_nested.ms

docs/architecture/
└── async.md             # This document
```

---

## Success Criteria

- [ ] All async/await leak tests pass with ASAN
- [ ] Callback cycles collected correctly
- [ ] 10k async operations with stable memory
- [ ] try/catch works across await boundaries
- [ ] LSP shows proper async diagnostics
- [ ] Can compile simple async HTTP client
