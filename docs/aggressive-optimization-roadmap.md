# C Backend Optimization Roadmap

**Status**: Active Development
**Last Updated**: December 5, 2025
**Current Performance**: ~85-90% of C
**Target**: 98% of C (Year 1)

---

## Executive Summary

Path from ~85-90% to 98% of C performance through systematic optimization. Based on Nim ORC, Swift ARC, Lobster, and proven compiler techniques.

---

## Current State

| Component | Status | Notes |
|-----------|--------|-------|
| **ORC Runtime** | ✅ Complete | Bacon-Rajan cycle detection, `orc.h` |
| **DRC Analyzer** | ✅ Complete | RC optimization analysis, `drc.zig` |
| **DRC ↔ Codegen** | ✅ Integrated | `getOpsForLine()` used in cgen |
| **DRC Scope Cleanup** | ✅ Complete | Heap-allocated vars get `ms_decref` at scope exit |
| Type System | ✅ Complete | Full static types |
| C Codegen | ✅ Working | Clean, idiomatic output |
| **String Interning** | ✅ Complete | Compile-time literal pool, `msInternedString` |
| **StringBuilder** | ✅ Complete | Concat-in-loop detection, O(n) allocation |
| **Borrowed Context** | ✅ Complete | Function args don't transfer ownership |
| Ownership Analysis | ⚠️ Exists | `isLastUse()` implemented, partial codegen use |
| Move Semantics | ⚠️ Exists | `.move` op exists, only for ownership-transfer contexts |

### ORC Runtime Details (`src/runtime/orc.h`)

| Feature | Status | Notes |
|---------|--------|-------|
| `ms_alloc()` / `ms_free()` | ✅ | RC=1 on alloc, header-based |
| `ms_incref()` / `ms_decref()` | ✅ | Simple RC ops |
| `ms_decref_typed()` | ✅ | Cycle-aware decref with TypeInfo |
| Bacon-Rajan cycle detection | ✅ | Mark-gray, scan, collect |
| Acyclic type optimization | ✅ | Skip cycle check for `!is_cyclic` |
| Type registry | ✅ | `ms_register_type()`, `ms_get_type_info()` |
| Root buffer management | ✅ | `ms_add_to_roots()`, threshold collection |

### DRC Analyzer Details (`src/analysis/drc.zig`)

| Feature | Status | Notes |
|---------|--------|-------|
| Scope tracking | ✅ | `enterScope()`, `exitScope()` |
| Variable registration | ✅ | `registerVariable()`, `registerAllocation()` |
| Use tracking | ✅ | `trackUse()`, last-use detection |
| RC operation emission | ✅ | `getOpsForLine()` → codegen |
| Move semantics | ✅ | `.move` for ownership-transfer contexts only |
| Borrow inference | ✅ | Borrowed params skip RC ops |
| Scope cleanup | ✅ | `ms_decref` at scope exit for heap vars |
| Interned string handling | ✅ | String literals skip RC (static pool) |

### String Optimization Details (Completed Dec 5, 2025)

| Feature | Status | Test Coverage |
|---------|--------|---------------|
| `ms_string_intern.h` | ✅ | Lazy init, zero-copy access |
| `ms_string_builder.h` | ✅ | Dynamic buffer, batch append |
| Concat-in-loop detection | ✅ | `stringbuilder_loop.ms` |
| Multi-var StringBuilder | ✅ | `stringbuilder_multi_var.ms` |
| Per-function scoping | ✅ | `stringbuilder_multi_func.ms` |
| Early return cleanup | ✅ | `stringbuilder_early_return.ms` |
| Pool exclusion for SB literals | ✅ | Reduces pool bloat |
| 20 codegen tests passing | ✅ | Full coverage |

### DRC Scope Cleanup Details (Completed Dec 5, 2025)

| Feature | Status | Notes |
|---------|--------|-------|
| Heap var detection | ✅ | String concat, `new` expr, call expr |
| Scope exit `ms_decref` | ✅ | Emitted at function/block end |
| Interned string skip | ✅ | `"literal"` vars treated as value type |
| Borrowed param skip | ✅ | Function args don't trigger decref |
| Move optimization guard | ✅ | Only ownership-transfer contexts |
| Test: `scope_cleanup.ms` | ✅ | Verifies only `result` gets decref |

**Key fixes:**
1. Binary expression type inference (`a + b` → string type)
2. Move optimization restricted to ownership-transfer contexts
3. String literal initializers treated as value type (interned)

---

## Performance Gap Analysis

| Overhead Source | Current | Target | Reduction |
|-----------------|---------|--------|-----------|
| RC operations | 6-8% | 0.5-2% | 4-6% |
| Cycle detection | 2-3% | 0.5% | 1.5-2.5% |
| Heap allocation | Variable | Minimal | 1-3% |
| Bounds checking | 1-2% | 0.5% | 0.5-1.5% |
| Indirect calls | 1-2% | ~0% | 1-2% |

**Total recoverable: ~10-15%**

---

## Phase 1: Wire Up Existing Analysis (85% → 92%)

**Goal**: Use the analysis infrastructure that already exists

### 1.1 Scope Cleanup & Last-Use Elision (~2%) ✅ PARTIAL

**Completed:**
- [x] DRC scope cleanup wired to codegen
- [x] Heap-allocated vars (string concat, new, call) get `ms_decref`
- [x] Interned strings skip RC (treated as value types)
- [x] Borrowed function args don't trigger ownership transfer
- [x] Move optimization guards for ownership-transfer contexts only
- [x] Test: `scope_cleanup.ms` verifies correct cleanup

**Example (working):**
```typescript
let a = "hello";      // interned, no RC
let b = " world";     // interned, no RC
let result = a + b;   // heap-allocated, needs RC
consume(result);      // borrowed, result still owned by caller
// scope exit: ms_decref(result) ← only this one!
```

**Remaining for full last-use elision:**
- [ ] Wire `ownership.zig:isLastUse()` for ownership-TRANSFER contexts
- [ ] Benchmark: measure RC operation reduction

### 1.2 Borrow Inference (~1.5%) ✅ COMPLETE

**Status**: Implemented as part of DRC scope cleanup fix.

Borrowed function parameters don't transfer ownership - the caller retains ownership and is responsible for cleanup. This is now correctly handled:

```typescript
function consume(s: string): void {
    console.log(s);  // s is borrowed, no RC in function body
}

function main(): void {
    let result = "a" + "b";  // heap-allocated
    consume(result);          // result borrowed, NOT moved
    // scope exit: ms_decref(result) ← caller cleans up
}
```

**Generated C:**
```c
void consume(msString* s) {
    printf("%s\n", ms_string_cstr(s));
    // NO ms_decref(s) - borrowed!
}
int main() {
    msString* result = ms_string_concat(...);
    consume(result);
    ms_decref(result);  // Caller cleanup
}
```

- [x] Query `drc.zig` for borrowed state in function params
- [x] Skip decref for borrowed parameters in callee
- [x] Test: `scope_cleanup.ms` verifies caller retains ownership

### 1.3 Move Detection (~2%)

Track last-use and transfer ownership instead of copy.

```zig
// Detect: variable used exactly once after definition
fn isLastUse(name: []const u8, line: u32) bool {
    const uses = getUsesAfter(name, line);
    return uses.len == 0;
}

// Codegen impact:
// Before: ms_incref(x); use(x); ms_decref(x);
// After:  use(x);  // move, no RC
```

- [ ] Implement `.move` RcOp type
- [ ] Detect last-use of variables in assignments
- [ ] Emit ownership transfer instead of copy

### 1.4 Escape Analysis → Stack Promotion (~3%)

Prove value doesn't escape → allocate on stack.

```zig
pub const EscapeState = enum {
    local,      // Stack allocatable
    returned,   // Must heap allocate
    stored,     // Must heap allocate
    captured,   // Must heap allocate
};

fn canStackAllocate(expr: *Expr) bool {
    return analyzeEscape(expr) == .local
        and estimateSize(expr.type) <= 4096;
}
```

```c
// Before (heap)
Point* p = ms_alloc(sizeof(Point));
p->x = 1; p->y = 2;
double d = distance(p, origin);
ms_decref(p);

// After (stack)
Point p = {.x = 1, .y = 2};
double d = distance(&p, origin);
// No alloc, no RC, no free
```

- [ ] Implement `EscapeState` enum
- [ ] Add escape analysis pass
- [ ] Emit stack allocation for local-only values

---

## Phase 2: Advanced Analysis (92% → 95%)

### 2.1 Prove Acyclicity (~2%)

Most programs have no cycles. Prove it → skip cycle detector.

```zig
pub const CycleRisk = enum {
    none,       // Proven acyclic, skip detection
    possible,   // May have cycles, need detection
    certain,    // Has cycles (self-ref, mutual ref)
};

fn analyzeCycleRisk(typ: *Type) CycleRisk {
    // Tree structures: acyclic
    // DAGs: acyclic
    // Parent-child with weak refs: acyclic
    // Self-referential: possible
}
```

```c
// Before (cycle-aware decref)
void Node_decref(Node* self) {
    if (--self->rc == 0) {
        orc_add_candidate(self);  // Cycle check
    }
}

// After (proven acyclic)
void Node_decref(Node* self) {
    if (--self->rc == 0) {
        Node_destroy(self);  // Direct free
        ms_free(self);
    }
}
```

- [ ] Implement `CycleRisk` analysis
- [ ] Track type graph for self-references
- [ ] Emit simple decref for acyclic types

### 2.2 Bounds Elision (~1%)

Prove array access in bounds → skip check.

```typescript
// Pattern: for-each (always safe)
for (const x of arr) { ... }

// Pattern: known length
const arr = [1, 2, 3];
arr[0]; arr[1]; arr[2];  // All provably safe

// Pattern: range check already done
if (i < arr.length) { arr[i]; }  // Safe
```

```c
// Before
int get(Array* arr, int i) {
    if (i < 0 || i >= arr->len) panic("bounds");
    return arr->data[i];
}

// After (proven safe)
int get(Array* arr, int i) {
    return arr->data[i];  // Direct access
}
```

- [ ] Track proven bounds from control flow
- [ ] Elide checks for for-each loops
- [ ] Elide checks for literal indices

### 2.3 Devirtualization (~1%)

Static types → resolve virtual calls at compile time.

```typescript
interface Shape { area(): number; }
class Circle implements Shape { ... }

const c: Circle = new Circle(5);
c.area();  // We KNOW it's Circle.area, not virtual
```

```c
// Before (virtual)
double area(Shape* s) {
    return s->vtable->area(s);  // Indirect call
}

// After (devirtualized)
double area(Circle* c) {
    return 3.14159 * c->radius * c->radius;  // Inlined!
}
```

- [ ] Track concrete types through assignments
- [ ] Emit direct calls when type is known
- [ ] Enable inlining for small methods

### 2.4 Inline Small Functions (~0.5%)

```c
// Add always_inline hint for small functions
__attribute__((always_inline))
static inline double square(double x) { return x * x; }
```

- [ ] Detect small pure functions
- [ ] Emit `always_inline` attribute
- [ ] Consider manual inlining in codegen

---

## Phase 3: Codegen Polish (95% → 98%)

### 3.1 Cache-Friendly Layout (~0.5%)

Order struct fields by access pattern and size.

```c
// Before (random order)
typedef struct {
    char* name;      // 8 bytes, rarely accessed
    int id;          // 4 bytes, frequently accessed
    double balance;  // 8 bytes, frequently accessed
    char status;     // 1 byte, rarely accessed
} User;

// After (hot fields together, aligned)
typedef struct {
    int id;          // Hot, 4 bytes
    double balance;  // Hot, 8 bytes
    char* name;      // Cold, 8 bytes
    char status;     // Cold, 1 byte
} User;
```

- [ ] Analyze field access patterns
- [ ] Reorder struct fields by hotness
- [ ] Align hot fields together

### 3.2 Branch Prediction Hints (~0.3%)

```c
// Null checks (usually not null)
if (likely(ptr != NULL)) { use(ptr); }

// Error paths (usually don't error)
if (unlikely(result < 0)) { handle_error(); }
```

- [ ] Add `likely`/`unlikely` macros
- [ ] Emit hints for null checks
- [ ] Emit hints for error paths

### 3.3 SIMD Opportunities (~1%)

```typescript
const sum = arr.reduce((a, b) => a + b, 0);
const doubled = arr.map(x => x * 2);
```

```c
#pragma omp simd
for (int i = 0; i < n; i++) {
    result[i] = arr[i] * 2;
}
```

- [ ] Detect vectorizable array operations
- [ ] Emit SIMD pragmas or intrinsics
- [ ] Benchmark improvement

### 3.4 Small String Optimization (~1%)

```c
// SSO: strings ≤23 bytes inline
typedef struct {
    union {
        struct { char data[23]; uint8_t len; } small;
        struct { char* ptr; size_t len; size_t cap; } heap;
    };
} msString;

// 80% of strings fit in SSO → no heap allocation
```

- [ ] Implement SSO in ms_string.h
- [ ] Benchmark string-heavy programs
- [ ] Measure allocation reduction

---

## Implementation Priority

```
COMPLETED:
├── 1.1 Scope cleanup for heap vars             [~2%] ✅
├── 1.2 Borrow inference (function params)      [~1.5%] ✅
└── String interning + StringBuilder            [~1%] ✅

NEXT (wire up remaining analysis):
├── 1.3 Move detection (return, field store)   [~2%] ← NEXT
└── 1.4 Escape analysis + stack promotion       [~3%]

HIGH IMPACT (new analysis):
└── 2.1 Prove acyclicity                        [~2%]

MEDIUM IMPACT:
├── 2.2 Bounds elision                          [~1%]
├── 2.3 Devirtualization                        [~1%]
└── 2.4 Inline small functions                  [~0.5%]

POLISH:
├── 3.3 SIMD opportunities                      [~1%]
├── 3.1 Cache-friendly layout                   [~0.5%]
├── 3.2 Branch hints                            [~0.3%]
└── 3.4 SSO refinement                          [~1%]
```

---

## Benchmark Targets

| Benchmark | Current | Phase 1 | Phase 2 | Phase 3 |
|-----------|---------|---------|---------|---------|
| Fibonacci (compute) | 95% | 97% | 98% | 99% |
| Binary trees (alloc) | 80% | 88% | 92% | 95% |
| JSON parse (strings) | 75% | 82% | 88% | 92% |
| N-body (numeric) | 90% | 94% | 96% | 98% |
| Game loop (mixed) | 85% | 90% | 94% | 96% |
| **Weighted average** | **85%** | **90%** | **94%** | **96-98%** |

---

## Timeline

| Phase | Effort | Performance |
|-------|--------|-------------|
| Phase 1.1-1.2 (wire up) | 1-2 weeks | 88-89% |
| Phase 1.3-1.4 (analysis) | 4-6 weeks | 92% |
| Phase 2 | 4-6 weeks | 95% |
| Phase 3 | 4-6 weeks | 97-98% |
| **Total** | **13-20 weeks** | **98%** |

---

## Validation Strategy

### Micro-benchmarks
```bash
./bench_rc_overhead        # Target: <2%
./bench_alloc_overhead     # Target: <5%
./bench_bounds_overhead    # Target: <1%
```

### Macro-benchmarks
```bash
./bench_vs_c fibonacci     # Target: >98%
./bench_vs_c binary_trees  # Target: >95%
./bench_vs_c json_parse    # Target: >92%
```

### Real-world
```bash
msc compile --target=c --release game.ms
./game --benchmark         # Target: >95% of equivalent C
```

---

## Why 98% Is Achievable

1. **Static types** - Full type info enables optimizations V8/Hermes can't do
2. **Proven techniques** - Lobster, Nim ORC, Swift ARC all achieve this
3. **Clean C output** - GCC/Clang can optimize idiomatic C effectively
4. **Focused overhead** - Only RC and bounds checks, both reducible
5. **Analysis exists** - `isLastUse()`, `borrowed`, etc. just need wiring

## Why 100% Is Not The Goal

Unavoidable overhead (~2%):
- Some RC operations can't be elided (truly shared data)
- Some bounds checks can't be proven (dynamic indices)
- Some allocations can't be stack-promoted (escaping data)

**98% is the practical ceiling. That's C-competitive.**

---

## Key Files

| File | Purpose | Status |
|------|---------|--------|
| `src/codegen/c/cgen.zig` | C code generation | Active, DRC integrated |
| `src/analysis/ownership.zig` | Last-use, escape | Exists, partial use |
| `src/analysis/drc.zig` | RC optimization | ✅ Complete, wired to codegen |
| `src/analysis/drc_analyzer.zig` | AST → DRC bridge | ✅ Complete, wired to codegen |
| `src/runtime/orc.h` | ORC runtime | ✅ Complete |
| `src/runtime/ms_string.h` | String handling | ✅ Complete |
| `src/runtime/ms_string_intern.h` | String literal pool | ✅ Complete |
| `src/runtime/ms_string_builder.h` | Concat optimization | ✅ Complete |
| `examples/tests/codegen/` | Codegen test suite | 20 tests passing |

---

## References

- Lobster language: Compile-time RC elimination
- Nim ORC: Production-validated cycle detection
- Swift ARC: Ownership and borrowing optimizations
- LLVM: Escape analysis techniques

---

**Completed This Session (Dec 5, 2025):**
- ✅ Phase 1.1: Scope cleanup for heap-allocated values
- ✅ Phase 1.2: Borrow inference for function parameters
- ✅ Fixed 3 bugs: binary expr type inference, move optimization guards, interned string RC

**Next Action**: Phase 1.3 Move Detection - wire `isLastUse()` for ownership-TRANSFER contexts (return, field store).
