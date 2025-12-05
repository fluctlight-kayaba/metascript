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

 ┌─────────────────────────────────────────────────────────────────────────┐
 │                    METASCRIPT MEMORY MODEL                              │
 │                    ══════════════════════                               │
 ├─────────────────────────────────────────────────────────────────────────┤
 │                                                                         │
 │                        YOUR CODE                                        │
 │                           │                                             │
 │                           ▼                                             │
 │              ┌────────────────────────┐                                 │
 │              │   SHARED BY DEFAULT    │  ← Safe, TypeScript-compatible  │
 │              │   (All refs are RC)    │    Zero learning curve          │
 │              └────────────────────────┘                                 │
 │                           │                                             │
 │                           ▼                                             │
 │              ┌────────────────────────┐                                 │
 │              │DRC LOBSTER OPTIMIZATION│  ← Compiler magic (automatic)   │
 │              │   (Compile-time)       │                                 │
 │              └────────────────────────┘                                 │
 │                      /        \                                         │
 │                     /          \                                        │
 │                    ▼            ▼                                       │
 │         ┌──────────────┐  ┌──────────────┐                              │
 │         │  95% CASES   │  │   5% CASES   │                              │
 │         │  ──────────  │  │  ──────────  │                              │
 │         │              │  │              │                              │
 │         │ Single-owner │  │ Can't prove  │                              │
 │         │ detected     │  │ single-owner │                              │
 │         │              │  │              │                              │
 │         │ → NO RC ops! │  │ → ORC ops    │  ← Still safe! Just slower   │
 │         │   (elided)   │  │   (shared)   │                              │
 │         └──────────────┘  └──────────────┘                              │
 │                                  │                                      │
 │                                  ▼                                      │
 │                     ┌────────────────────────┐                          │
 │                     │  OPTIONAL: move        │  ← User optimization     │
 │                     │  keyword for 5%        │    Only if they care     │
 │                     └────────────────────────┘                          │
 │                                  │                                      │
 │                                  ▼                                      │
 │                           → NO RC ops!                                  │
 │                                                                         │
 └─────────────────────────────────────────────────────────────────────────┘

 0%                        95%            100%
 │                          │               │
 │ ─────────────────────────┼───────────────│
 │                          │               │
 │    AUTOMATIC             │   OPTIONAL    │
 │    (Lobster)             │   (move)      │
 │                          │               │
 │  ✓ Zero effort           │  ✓ Minimal    │
 │  ✓ 95% of C perf         │  ✓ 100% of C  │
 │  ✓ TypeScript-like       │  ✓ Opt-in     │
 │                          │               │
 └──────────────────────────┴───────────────┘
          Most developers       Power users
          stop here             go here

---

## Shared by Default (Critical Design Decision)

### The Core Principle

**Metascript uses "Shared by Default" semantics for all object references.**

This is the most critical design decision in our memory model. Every object reference is **shared** (reference counted) by default, with `move` available as an **optimization** to skip RC overhead.

### Why Shared by Default?

**It matches JavaScript/TypeScript semantics exactly.**

```typescript
// TypeScript behavior - multiple references to same object
const user = { name: "Alice" };
const users = [];
users.push(user);      // user is still valid!
console.log(user.name); // Works fine in TS

// This MUST work identically in Metascript
// If we used "borrow by default", this would break!
```

| Alternative | Why Not |
|-------------|---------|
| **Borrow by Default** | `users.push(user)` would invalidate `user` - BREAKS TypeScript! |
| **Move by Default** | Same problem - ownership transfer breaks TS semantics |
| **Inferred Borrow/Move** | Complex, unpredictable, still risks breaking TS patterns |
| **Shared by Default** ✅ | Matches JS/TS exactly. Use after share is always valid. |

### How It Works

```
┌─────────────────────────────────────────────────────────────────┐
│                    SHARED BY DEFAULT                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Every object reference is SHARED (ref-counted):                │
│                                                                 │
│    let user = new User("Alice");  // RC = 1                     │
│    this.users.push(user);          // RC = 2 (incref)           │
│    console.log(user.name);         // Still valid! (RC = 2)     │
│    // ... end of scope ...         // RC = 1 (decref)           │
│    // user goes out of scope       // RC = 0 → freed            │
│                                                                 │
│  OPTIMIZATION with `move`:                                      │
│                                                                 │
│    let user = new User("Alice");  // RC = 1                     │
│    return move user;               // No incref! Transfer owner │
│    // user is INVALID after move   // Caller owns it now        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### TypeScript Compatibility Guarantee

**Any valid TypeScript pattern involving object references works unchanged:**

```typescript
// Pattern 1: Store in collection, use original
const item = createItem();
collection.add(item);
item.process();  // ✅ Works - item is shared, not moved

// Pattern 2: Return from function, caller uses
function getUser(): User {
    const user = fetchUser();
    cache.store(user);
    return user;  // ✅ Works - user shared with cache AND returned
}

// Pattern 3: Pass to multiple functions
const data = loadData();
validate(data);
transform(data);
save(data);  // ✅ Works - each call shares, none takes ownership
```

### The `move` Keyword: Optimization, Not Requirement

`move` is **strictly optional**. It tells the compiler: "I'm done with this value, transfer ownership without RC overhead."

```typescript
// Without move (default - always correct)
function processAndReturn(user: User): User {
    log(user);           // incref for log, decref after
    return user;         // incref for return, decref at scope exit
}
// Total: 2 incref + 2 decref = 4 RC operations

// With move (optimization - saves RC operations)
function processAndReturn(user: User): User {
    log(user);           // incref for log, decref after
    return move user;    // NO incref! Transfer ownership directly
}
// Total: 1 incref + 1 decref = 2 RC operations (50% reduction)
```

### When to Use `move`

| Context | Use `move`? | Reason |
|---------|-------------|--------|
| Return value (last use) | ✅ Optional optimization | Saves incref/decref pair |
| Pass to sink function | ✅ Optional optimization | Function takes ownership |
| After use in same scope | ❌ Never | Value still needed |
| Multiple references exist | ❌ Never | Other refs would dangle |

### Comparison with Other Languages

| Language | Default Semantics | Metascript Equivalent |
|----------|-------------------|----------------------|
| **JavaScript/TypeScript** | Shared (GC) | **Shared (RC)** ← matches exactly |
| **Rust** | Move by default | Opposite - explicit `clone()` needed |
| **C++** | Copy by default | Similar but with manual memory |
| **Swift** | Shared (ARC) | Same model |

### Why Not Borrow by Default?

**Critical insight from design review:**

```typescript
// This is VALID TypeScript that millions of developers write:
class UserManager {
    private users: User[] = [];

    addUser(user: User): void {
        this.users.push(user);
        // TypeScript: user is still valid here
        // Borrow-by-default: user would be INVALID (ownership transferred)
    }
}

const user = new User("Alice");
manager.addUser(user);
console.log(user.name);  // TypeScript: Works! Borrow-by-default: CRASH!
```

**Borrow by default would break the TypeScript superset guarantee.** Developers copying TypeScript code would face silent bugs or crashes.

### Implementation in DRC

```zig
// src/analysis/drc.zig

pub const UseContext = enum {
    read,                    // Just reading - no ownership change
    function_arg_shared,     // DEFAULT: Shared reference (incref)
    function_arg_owned,      // move: Ownership transfer (no incref)
    returned,                // Return value - may be shared or moved
    field_store,             // Storing in object field (incref)
};

pub fn transfersOwnership(context: UseContext) bool {
    return switch (context) {
        .function_arg_owned, .returned => true,  // move contexts
        .read, .function_arg_shared, .field_store => false,  // shared contexts
    };
}
```

### Generated C Code

```c
// Shared (default)
void addUser(UserManager* self, User* user) {
    ms_incref(user);         // Increment RC - user is SHARED
    array_push(self->users, user);
}

// Move (explicit optimization)
void consumeUser(User* user) {  // Parameter marked with 'move'
    // No ms_incref - ownership transferred to this function
    process(user);
    ms_decref(user);         // We own it, we clean it up
}
```

### Summary

| Aspect | Shared by Default |
|--------|-------------------|
| **Default behavior** | All references are shared (RC managed) |
| **TypeScript compatibility** | 100% - same semantics as JS/TS |
| **Memory safety** | Automatic - RC prevents use-after-free |
| **Optimization** | `move` keyword for explicit ownership transfer |
| **Developer experience** | No new concepts - works like TypeScript |
| **Cognitive load** | Zero for default case, optional optimization |

**This is the right default because it prioritizes correctness and TypeScript compatibility over micro-optimization. Developers who need maximum performance can use `move`, but the 95% common case "just works."**

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

**Key Insight:** While semantics are "Shared by Default", the compiler can often prove that sharing is unnecessary, eliminating RC overhead entirely.

```
┌─────────────────────────────────────────────────────────────────┐
│               OPTIMIZATION STRATEGY                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  SEMANTICS: Shared by Default (safe, TypeScript-compatible)    │
│                 ↓                                               │
│  ANALYSIS: Detect single-owner patterns at compile time        │
│                 ↓                                               │
│  CODEGEN: Elide RC for provably single-owner cases             │
│                                                                 │
│  Result: TypeScript semantics + near-C performance             │
└─────────────────────────────────────────────────────────────────┘
```

```zig
pub const OwnershipKind = enum {
    owned,     // Single owner - can elide RC
    borrowed,  // Temporary access - no RC change
    shared,    // Multiple owners - requires RC
    moved,     // Ownership transferred via `move`
};

fn analyzeOwnership(expr: *ast.Node) OwnershipKind {
    return switch (expr.kind) {
        .variable_decl => .owned,       // Initial allocation = single owner
        .parameter => .borrowed,        // Borrows from caller
        .field_access => .borrowed,     // Borrows from parent
        .function_call => .shared,      // May be stored elsewhere
        .move_expr => .moved,           // Explicit ownership transfer
        else => .shared,                // Default: safe shared semantics
    };
}

// Code generation: Optimize based on analysis
if (ownership == .owned or ownership == .borrowed) {
    emit("dest = src;");                // No RC - provably safe!
} else if (ownership == .moved) {
    emit("dest = src;");                // No RC - explicit transfer
} else {
    emit("dest = src; ms_incref(src);"); // RC++ - shared semantics
}
```

### Optimization Phases

| Phase | Technique | Impact |
|-------|-----------|--------|
| 1 | Basic ownership analysis | 50% RC elimination |
| 2 | `move` keyword recognition | 10% more elimination |
| 3 | Escape analysis | 15% more elimination |
| 4 | Inline temporaries | 10% more elimination |
| 5 | Stack allocation | Avoid heap entirely |

### Automatic vs Explicit Optimization

```typescript
// AUTOMATIC: Compiler detects single-owner pattern
function createUser(name: string): User {
    const user = new User(name);  // owned - single owner
    return user;                  // Compiler auto-elides RC
}
// Generated C: no incref/decref pair

// EXPLICIT: Developer uses `move` for guaranteed optimization
function transferUser(user: User): void {
    process(move user);           // Explicit: skip RC, invalidate user
}
// Generated C: no incref, callee responsible for decref
```

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

## LSP-DRC Integration

### Overview

Surface DRC ownership analysis to developers through LSP features:
- **Diagnostics**: Warnings for unknown ownership, potential leaks, use-after-move
- **Hover**: Show ownership state and cleanup location for variables
- **Related Locations**: Link to move sites, definitions, cleanup points

### Data Flow

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Parser    │────▶│ TypeChecker │────▶│     DRC     │
└─────────────┘     └─────────────┘     └─────────────┘
                                               │
                                               ▼
                                        ┌─────────────┐
                                        │ DrcLspInfo  │
                                        └─────────────┘
                                               │
                    ┌──────────────────────────┼──────────────────────────┐
                    ▼                          ▼                          ▼
             ┌─────────────┐           ┌─────────────┐           ┌─────────────┐
             │ Diagnostics │           │   Hovers    │           │ Code Actions│
             │ (warnings)  │           │ (ownership) │           │  (quick fix)│
             └─────────────┘           └─────────────┘           └─────────────┘
```

### Core Data Structures

```zig
// src/analysis/drc_lsp.zig

pub const DrcLspInfo = struct {
    /// Variable ownership states at each scope point
    variable_info: std.StringHashMap(VariableOwnershipInfo),

    /// Detected issues (leaks, unknown ownership, use-after-move)
    diagnostics: std.ArrayList(DrcDiagnostic),

    /// Cleanup locations (where decref happens)
    cleanup_locations: std.ArrayList(CleanupLocation),
};

pub const VariableOwnershipInfo = struct {
    name: []const u8,
    ownership: OwnershipState,     // owned, borrowed, moved, unknown
    confidence: Confidence,         // high, medium, low
    defined_at: SourceLocation,
    cleanup_at: ?SourceLocation,    // null if leaked or moved
    moved_to: ?SourceLocation,      // if ownership transferred
    type_name: []const u8,
    needs_rc: bool,
};

pub const Confidence = enum { high, medium, low };

pub const DrcDiagnostic = struct {
    kind: DiagnosticKind,
    location: SourceLocation,
    message: []const u8,
    related: []RelatedLocation,
    severity: Severity,  // error, warning, hint
};

pub const DiagnosticKind = enum {
    unknown_ownership,    // Can't determine if owned/borrowed
    potential_leak,       // RC type not cleaned up
    use_after_move,       // Accessing moved value
    double_free,          // Decref on already freed
    escape_to_global,     // Local escapes to module scope
};
```

### Diagnostic Detection

```zig
fn detectDiagnostics(self: *DrcLspInfo, drc: *DrcAnalyzer) void {
    for (drc.variables.values()) |v| {
        // Unknown ownership
        if (v.ownership_state == .unknown and v.needs_rc) {
            self.diagnostics.append(.{
                .kind = .unknown_ownership,
                .location = v.definition_location,
                .message = "Cannot determine ownership - may leak or double-free",
                .severity = .warning,
            });
        }

        // Potential leak (owned but no cleanup)
        if (v.ownership_state == .owned and v.needs_rc and !v.has_cleanup) {
            self.diagnostics.append(.{
                .kind = .potential_leak,
                .location = v.definition_location,
                .message = "Owned value may not be freed",
                .severity = .warning,
            });
        }

        // Use-after-move
        for (v.uses_after_move) |use| {
            self.diagnostics.append(.{
                .kind = .use_after_move,
                .location = use.location,
                .message = "Value was moved, cannot access",
                .severity = .@"error",
                .related = &[_]RelatedLocation{
                    .{ .location = v.moved_at, .message = "moved here" },
                },
            });
        }
    }
}
```

### LSP Hover Format

When hovering over a variable with RC type:

```markdown
**`userName`** `: string`

| Property | Value |
|----------|-------|
| Ownership | `owned` |
| Confidence | `high` |
| Cleanup | Line 42 (scope exit) |
| RC Type | Yes |

---
_DRC: This value will be automatically freed at scope exit._
```

For uncertain ownership:

```markdown
**`result`** `: User`

| Property | Value |
|----------|-------|
| Ownership | `unknown` ⚠️ |
| Confidence | `low` |
| Cleanup | Unknown |
| RC Type | Yes |

---
_DRC: Cannot determine ownership. Value returned from external function._
```

### LSP Server Integration

```zig
// src/lsp/server.zig

const ServerState = struct {
    // ... existing fields ...
    drc_cache: std.StringHashMap(DrcLspInfo),
};

fn handleHover(self: *Server, params: HoverParams) ?Hover {
    const uri = params.textDocument.uri;
    const pos = params.position;

    // Get DRC info for this file
    const drc_info = self.state.drc_cache.get(uri) orelse return null;

    // Find variable at position
    const var_info = drc_info.findVariableAt(pos) orelse return null;

    // Format hover content
    return Hover{
        .contents = .{
            .kind = .markdown,
            .value = formatOwnershipHover(var_info),
        },
    };
}

fn publishDrcDiagnostics(self: *Server, uri: []const u8) void {
    const drc_info = self.state.drc_cache.get(uri) orelse return;

    var diagnostics = std.ArrayList(lsp.Diagnostic).init(self.allocator);
    for (drc_info.diagnostics.items) |diag| {
        diagnostics.append(convertToLspDiagnostic(diag));
    }

    self.notify("textDocument/publishDiagnostics", .{
        .uri = uri,
        .diagnostics = diagnostics.items,
    });
}
```

### Configuration Options

```json
{
  "metascript.drc.enabled": true,
  "metascript.drc.showHoverInfo": true,
  "metascript.drc.diagnosticSeverity": {
    "unknownOwnership": "warning",
    "potentialLeak": "warning",
    "useAfterMove": "error"
  },
  "metascript.drc.showCleanupLocations": true
}
```

### Implementation Phases

| Phase | Scope | Deliverables |
|-------|-------|--------------|
| 1 | Data structures | `DrcLspInfo`, `collectLspInfo()`, cache integration |
| 2 | Diagnostics | Detection logic, LSP diagnostic publishing |
| 3 | Hover | Ownership display, markdown formatting |
| 4 | Polish | Related locations, configuration, code actions |

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
