# Lobster-Inspired Optimizations

**Purpose:** Compile-time analysis to minimize runtime overhead

**Goal:** Reduce RC operations 50-95%, enable stack allocation, inline temporaries

---

## Core Insight

Most values have **single owner** at compile time. Lobster proved that compile-time ownership analysis can eliminate the vast majority of reference counting operations.

```
Traditional RC:
  Every assignment: incref + decref
  Result: 5-10% overhead

Lobster-style:
  Analyze ownership at compile time
  Only RC when truly shared
  Result: 0.5-2% overhead
```

---

## Ownership Analysis

### Ownership Kinds

```zig
pub const OwnershipKind = enum {
    owned,      // First assignment, single owner
    borrowed,   // Temporary reference, no ownership change
    shared,     // Multiple owners, needs RC
};
```

### Analysis Rules

| Expression | Ownership | Rationale |
|------------|-----------|-----------|
| Variable declaration | `owned` | First binding owns |
| Function parameter | `borrowed` | Caller owns, callee borrows |
| Field access | `borrowed` | Parent owns |
| Return value | `borrowed` or `shared` | Move if owned, copy if not |
| Closure capture | `shared` | May outlive scope |

### Example Analysis

```typescript
function process(user: User): string {
    let name = user.name;      // borrowed (from user)
    let copy = name;           // shared (two refs)
    return name.toUpperCase(); // borrowed (move to return)
}
```

---

## Escape Analysis

Determine if a value escapes its scope:

```zig
pub const EscapeState = enum {
    local,      // Never escapes, stack allocatable
    returned,   // Escapes via return
    stored,     // Stored in heap object
    captured,   // Captured by closure
};

fn analyzeEscape(expr: *ast.Node, scope: *Scope) EscapeState {
    // Track all uses of the value
    // If any use escapes, the value escapes
}
```

### Stack Allocation Decision

```zig
fn getAllocationSite(typ: *Type, escape: EscapeState) AllocSite {
    if (escape == .local) {
        if (estimateSize(typ) <= 4096) {
            return .stack;  // Stack allocate!
        }
    }
    return .heap;
}
```

---

## Type Inference Enhancements

### Inferred Ownership

```typescript
// Compiler infers ownership from usage patterns
function example() {
    let a = createUser();     // owned (created here)
    let b = a;                // moved (a no longer valid)
    process(b);               // borrowed (still valid after)
    return b;                 // moved (ownership transferred)
}
```

### Inferred Lifetimes

```typescript
// Compiler infers lifetimes without explicit annotations
function getFirst<T>(arr: T[]): T {
    return arr[0];  // Lifetime tied to arr
}

// Equivalent to Rust's:
// fn get_first<'a, T>(arr: &'a [T]) -> &'a T
```

---

## Optimization Passes

### Pass 1: Ownership Assignment

```
For each variable:
  1. Find all assignment sites
  2. Determine if single owner
  3. Mark as owned/borrowed/shared
```

### Pass 2: Move Detection

```
For each assignment:
  1. If source is owned and not used after:
     Mark as MOVE (no RC)
  2. If source still needed:
     Mark as COPY (RC++)
```

### Pass 3: Inline Temporaries

```
For each temporary:
  1. If used exactly once:
     Inline into use site
     Eliminate allocation
```

### Pass 4: Stack Promotion

```
For each heap allocation:
  1. If doesn't escape and size < threshold:
     Promote to stack allocation
     Eliminate heap overhead
```

---

## Code Generation Impact

### Before Optimization

```c
void process(User* user) {
    String* name = user->name;
    ms_incref(name);              // RC++
    String* copy = name;
    ms_incref(copy);              // RC++
    String* upper = toUpperCase(name);
    ms_decref(name);              // RC--
    ms_decref(copy);              // RC--
    return upper;
}
```

### After Optimization

```c
void process(User* user) {
    String* name = user->name;    // borrowed, no RC
    String* copy = name;          // borrowing borrow, no RC
    return toUpperCase(name);     // moved, no RC
}
// 6 RC ops -> 0 RC ops!
```

---

## Integration with DRC

Lobster optimizations **feed into** DRC:

```
Lobster Analysis → Ownership Metadata → DRC Codegen

owned    → no incref on creation
borrowed → no incref/decref
shared   → normal RC operations
```

---

## Performance Expectations

| Optimization | RC Reduction | When |
|--------------|--------------|------|
| Basic ownership | 50% | Phase 1 |
| Move detection | 20% additional | Phase 2 |
| Escape analysis | 15% additional | Phase 2 |
| Stack promotion | 10% additional | Phase 3 |
| **Total** | **80-95%** | Phase 3 |

---

## Testing Strategy

### Unit Tests

```zig
test "ownership analysis: variable declaration is owned" {
    const source = "let x = createUser();";
    const analysis = analyzeOwnership(parse(source));
    try testing.expectEqual(.owned, analysis.get("x").ownership);
}

test "ownership analysis: parameter is borrowed" {
    const source = "function f(x: User) {}";
    const analysis = analyzeOwnership(parse(source));
    try testing.expectEqual(.borrowed, analysis.get("x").ownership);
}

test "escape analysis: local stays on stack" {
    const source = "function f() { let x = Point{1, 2}; return x.x; }";
    const analysis = analyzeEscape(parse(source));
    try testing.expectEqual(.local, analysis.get("x").escape);
}
```

### Benchmarks

Compare RC operation count:
- Baseline (all RC): X operations
- With Lobster: Y operations
- Reduction: (X - Y) / X * 100%

---

## File Location

```
src/analyzer/
  ownership.zig     # Ownership analysis
  escape.zig        # Escape analysis
  lifetime.zig      # Lifetime inference

src/optimizer/
  move_detection.zig
  inline_temps.zig
  stack_promote.zig
```

---

## References

- **Lobster language:** Compile-time RC elimination research
- **Swift ARC:** Similar ownership optimizations
- **Rust borrow checker:** Inspiration for analysis rules
- **DRC integration:** `./drc-orc.md`
