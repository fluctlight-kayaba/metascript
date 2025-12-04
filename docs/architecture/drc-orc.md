# DRC (ORC-Inspired) Reference Counting

**Purpose:** Automatic memory management for C backend with cycle detection

**Performance Target:** 6-8% baseline overhead, optimizable to 0.5-2%

---

## Why ORC/DRC

| Alternative | Why Not |
|-------------|---------|
| Manual memory | Error-prone, not TypeScript-like |
| Tracing GC | Unpredictable pauses, complex runtime |
| Pure ARC | Leaks cycles (async/await, closures) |
| **ORC/DRC** | Deterministic + cycle detection |

**Production Validation:** Nim 2.0+ uses ORC as default (August 2023).

---

## RefHeader Layout

All heap-allocated objects start with 8-byte header:

```c
typedef struct {
    uint32_t rc;       // Reference count + flags (low 4 bits)
    int32_t rootIdx;   // Cycle collector root (-1 = not tracked)
} msRefHeader;

// Flag encoding
#define MS_RC_INCREMENT  0b10000  // Uses 4 bits for flags
#define MS_RC_SHIFT      4
#define MS_RC_MOVED      0b0001   // Object was moved
#define MS_RC_PINNED     0b0010   // Don't collect (foreign ref)
#define MS_RC_MARK       0b0100   // GC mark bit
#define MS_RC_OWNED      0b1000   // Single owner (elide RC)

// Actual count = rc >> 4
// Flags = rc & 0b1111
```

---

## Reference Counting Operations

```c
static inline void ms_incref(void* ptr) {
    if (ptr == NULL) return;
    msRefHeader* hdr = (msRefHeader*)ptr - 1;
    hdr->rc += MS_RC_INCREMENT;
}

static inline void ms_decref(void* ptr) {
    if (ptr == NULL) return;
    msRefHeader* hdr = (msRefHeader*)ptr - 1;

    if ((hdr->rc >> MS_RC_SHIFT) == 0) {
        ms_destroy(ptr);
        ms_free(hdr);
    } else {
        hdr->rc -= MS_RC_INCREMENT;
    }
}

// Move operation (no RC update)
static inline void* ms_sink(void* ptr) {
    if (ptr == NULL) return NULL;
    msRefHeader* hdr = (msRefHeader*)ptr - 1;
    hdr->rc |= MS_RC_MOVED;
    return ptr;
}
```

---

## Lifecycle Hooks

Every reference type gets 6 generated hooks:

```c
// Example: User class with reference fields

// 1. Destroy - called when RC reaches 0
void User_destroy(User* self) {
    if (self->name) {
        ms_decref(self->name);
        self->name = NULL;
    }
}

// 2. Copy - RC++ on all reference fields
void User_copy(User* dest, const User* src) {
    if (dest == src) return;
    User_destroy(dest);
    dest->name = src->name;
    if (dest->name) ms_incref(dest->name);
    dest->age = src->age;  // Value type - plain copy
}

// 3. Sink - move ownership (no RC update)
void User_sink(User* dest, User* src) {
    User_destroy(dest);
    dest->name = ms_sink(src->name);
    dest->age = src->age;
    User_wasMoved(src);
}

// 4. WasMoved - mark source as moved
void User_wasMoved(User* self) {
    self->name = NULL;
}

// 5. Trace - ORC cycle collector support
void User_trace(User* self, void (*visit)(void*)) {
    if (self->name) visit(self->name);
}

// 6. Dup - shallow copy with RC++
void User_dup(User* dest, const User* src) {
    *dest = *src;
    if (dest->name) ms_incref(dest->name);
}
```

---

## Cycle Detection (Bacon-Rajan Algorithm)

```
Object States:
  BLACK  - In use, not candidate for collection
  PURPLE - Potential cycle root (RC decremented but > 0)
  GRAY   - Being traced
  WHITE  - Candidate for collection

Algorithm:
1. On RC decrement to non-zero: Mark PURPLE, add to roots
2. Mark phase: Trace from PURPLE roots, mark reachable GRAY
3. Scan phase: Scan GRAY, find objects with RC = 0, mark WHITE
4. Collect phase: Free all WHITE objects
```

---

## Lobster-Style Optimizations

### Goal: Reduce RC operations 50-95%

**Key Insight:** Most values have single owner at compile time.

```zig
pub const OwnershipKind = enum { owned, borrowed, shared };

fn analyzeOwnership(expr: *ast.Node) OwnershipKind {
    return switch (expr.kind) {
        .variable_decl => .owned,
        .parameter => .borrowed,
        .field_access => .borrowed,
        .return_expr => if (owned) .borrowed else .shared,
        else => .shared,
    };
}

// Code generation uses ownership
if (ownership == .owned or ownership == .borrowed) {
    emit("dest = src;");  // No RC
} else {
    emit("dest = src; ms_incref(src);");  // RC++
}
```

### Optimization Phases

| Phase | Technique | Impact |
|-------|-----------|--------|
| 1 | Basic ownership analysis | 50% RC elimination |
| 2 | Escape analysis | 20% more elimination |
| 3 | Inline temporaries | 10% more elimination |
| 4 | Stack allocation | Avoid heap entirely |

---

## Integration with Backends

### C Backend
- Full DRC with cycle detection
- Uses `orc.h` runtime (720 LOC)
- Lifecycle hooks generated per type

### JavaScript Backend
- DRC analysis for optimization hints
- No actual RC (V8 GC handles memory)
- Analysis informs tree-shaking

### Erlang Backend
- No DRC needed (BEAM GC)
- Immutable data, per-process heaps
- Analysis validates immutability

---

## Testing Strategy

### Unit Tests

```zig
test "ORC simple allocation overhead" {
    const baseline = measureMalloc(1000);
    const orc = measureORC(1000);
    const overhead = (orc - baseline) / baseline * 100;
    try testing.expect(overhead < 10);  // <10% overhead
}

test "ORC cycle detection" {
    var a = createNode();
    var b = createNode();
    a.next = b;
    b.next = a;  // Cycle!

    releaseNode(a);
    // Cycle should be detected and freed
    try testing.expect(getAllocatedCount() == 0);
}
```

### Benchmarks

| Workload | Target Overhead |
|----------|-----------------|
| Simple allocation | <10% |
| 100K allocations | <15% |
| Linked list (1000) | <15% |
| Cycle-heavy | <20% |

---

## File Location

```
src/runtime/
  orc.h           # Header-only ORC runtime (720 LOC)
  orc.zig         # Zig bindings and integration

src/codegen/c/
  lifecycle.zig   # Hook generation (destroy, copy, etc.)
  ownership.zig   # Ownership analysis
```

---

## References

- **Nim ORC:** `~/projects/nim/lib/system/orc.nim`
- **Lobster research:** Compile-time RC elimination
- **Memory model:** `../memory-model.md`
- **C backend:** `./backends.md`
