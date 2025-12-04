# Trans-Am Cache Engine

**Purpose:** Incremental computation for fast LSP and compilation

**Inspiration:** Salsa (rust-analyzer's query framework)

**Name:** Muscle car that "beats Salsa at drag racing"

---

## The Problem

Heavy macro expansion (e.g., `@derive(Eq, Hash)` generating 500+ lines) causes sluggish editor responsiveness. Both Nim and Haxe have this problem.

**Solution:** Salsa-style red-green algorithm + content-addressed macro caching.

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    TRANS-AM QUERY ENGINE                        │
├─────────────────────────────────────────────────────────────────┤
│  INPUT LAYER (never cached)                                     │
│    file_texts, config                                           │
├─────────────────────────────────────────────────────────────────┤
│  QUERY LAYER (red-green algorithm)                              │
│    parse(file_id) → AST                                         │
│    macro_call_sites(file_id) → [MacroCallId]                    │
│    macro_input_hash(call_id) → u64                              │
│    macro_output_hash(call_id) → u64  ← content-addressed!       │
│    macro_expand(call_id) → ExpandedAST                          │
│    symbols(file_id) → SymbolTable                               │
│    type_of(expr_id) → Type                                      │
├─────────────────────────────────────────────────────────────────┤
│  CACHE LAYER                                                    │
│    LRU (bounded): parse, macro_expand                           │
│    Content-addressed: macro_output_hash → output_ast            │
│    Durability-partitioned: LOW/MEDIUM/HIGH                      │
├─────────────────────────────────────────────────────────────────┤
│  INTERNED LAYER (deduplicated, never evicted)                   │
│    symbol_interner, macro_call_interner                         │
└─────────────────────────────────────────────────────────────────┘
```

---

## Core Concepts

### 1. Revision Tracking

Every input change increments a global revision:

```zig
pub const Revision = struct {
    value: u64,

    pub fn increment(self: *Revision) void {
        self.value += 1;
    }
};
```

### 2. Query States (Red-Green Algorithm)

```
       ┌─────────┐
       │  GREEN  │ ← Verified as current
       └────┬────┘
            │ input changed
            ↓
       ┌─────────┐
       │   RED   │ ← May be stale
       └────┬────┘
            │ try_mark_green()
            ↓
       ┌─────────┐
       │ YELLOW  │ ← Being verified (cycle detection)
       └────┬────┘
            │
     ┌──────┴──────┐
     ↓             ↓
  GREEN         recompute
(unchanged)     and mark GREEN
```

**Key Insight:** When dependencies change, don't immediately recompute. First check if the OUTPUT actually changed. If not, mark GREEN without recomputing dependents.

### 3. Durability Classification

```zig
pub const Durability = enum(u2) {
    low = 0,     // User-edited files (src/*.ms)
    medium = 1,  // Config files (*.json)
    high = 2,    // Library files (std/, node_modules/)
};
```

HIGH durability queries survive LOW durability invalidation.

---

## Macro Firewall Pattern

The critical optimization. Split macro expansion into three queries:

```
┌──────────────────┐     ┌──────────────────┐     ┌──────────────────┐
│  macroArg()      │────▶│  macroExpander() │────▶│  macroExpand()   │
│  (extract args)  │     │  (lookup fn)     │     │  (execute)       │
└──────────────────┘     └──────────────────┘     └──────────────────┘
        │                         │                        │
   Only invalidates          Never changes            LRU cached
   when THIS call's          (definition is          (512 entries)
   text changes              immutable)
```

**Without firewall:**
```typescript
@derive(Eq)        // Edit here...
class User { }
@derive(Hash)      // ...re-expands ALL 10 macros
class Item { }
```

**With firewall:**
```typescript
@derive(Eq)        // Edit here...
class User { }
@derive(Hash)      // Only re-expand User's @derive
class Item { }     // Others stay GREEN!
```

---

## Content-Addressed Macro Cache

**Traditional:** `(call_id, revision) → output`

**Problem:** Different input text can produce identical output (whitespace, comments).

**Content-addressed:** `hash(input_ast, args) → hash(output_ast)`

```zig
pub fn macroExpand(self: *Self, call_id: MacroCallId) !MacroExpansion {
    const input_hash = try self.macroInputHash(call_id);

    // Check content cache
    if (self.content_cache.get(input_hash)) |output_hash| {
        if (self.output_store.get(output_hash)) |ast| {
            return ast;
        }
    }

    // Execute macro
    const output = try self.executeAndHash(call_id);

    // Store in content cache
    try self.content_cache.put(input_hash, output.hash);
    try self.output_store.put(output.hash, output.ast);

    return output;
}
```

---

## Cache Key Design

```
cache_key = hash(
    macro_source_hash,     // derive.ms content
    target_ast_hash,       // class User { ... }
    arguments_hash,        // ["Eq", "Hash"]
    dependencies_hash,     // imports in derive.ms
)
```

**Invalidation Strategy:**

| What Changed | What Invalidates |
|--------------|------------------|
| User edits class body | Only THIS class's expansion |
| User edits macro args | Only THIS class's expansion |
| Macro source changes | ALL expansions using this macro |
| Nothing (cursor moved) | NOTHING (instant!) |

---

## LRU Cache Implementation

```zig
pub fn LruCache(comptime V: type, comptime max_size: usize) type {
    return struct {
        entries: std.AutoArrayHashMap(u64, Entry),
        order: std.DoublyLinkedList(u64),

        pub fn get(self: *@This(), key: u64) ?V {
            if (self.entries.get(key)) |entry| {
                // Move to front
                self.order.remove(entry.node);
                self.order.prepend(entry.node);
                return entry.value;
            }
            return null;
        }

        pub fn put(self: *@This(), key: u64, value: V) void {
            if (self.entries.count() >= max_size) {
                // Evict oldest
                if (self.order.popBack()) |oldest| {
                    _ = self.entries.remove(oldest.data);
                }
            }
            // Insert at front
            // ...
        }
    };
}
```

**Defaults (from rust-analyzer):**
- Macro cache: 2000 entries
- Type cache: 2000 entries
- Completion cache: 500 entries

---

## Cooperative Cancellation

```zig
pub const TransAmDatabase = struct {
    cancellation_version: std.atomic.Value(u64),

    pub fn unwindIfCancelled(self: *@This()) !void {
        if (self.current_version != self.cancellation_version.load(.seq_cst)) {
            return error.QueryCancelled;
        }
    }

    pub fn cancelPendingQueries(self: *@This()) void {
        _ = self.cancellation_version.fetchAdd(1, .seq_cst);
    }
};
```

Long queries call `unwindIfCancelled()` periodically (16+ checkpoints in hot paths).

---

## Implementation Status

| Phase | Status |
|-------|--------|
| Core infrastructure | Done |
| Red-green algorithm | Done |
| Macro firewall | Done |
| Parallel highlighting | Done |
| LSP integration | Done |
| Parser integration | Done |
| CLI integration | Done |
| Type checker integration | Done |
| Async threading | Sync only (thread-safe caches TODO) |

---

## Performance Targets

| Operation | L4 Hit | L3 Hit | L2 Hit | Cold |
|-----------|--------|--------|--------|------|
| Completion | <5ms | <20ms | <50ms | <200ms |
| Hover | <5ms | <15ms | <30ms | <100ms |
| Diagnostics | N/A | <50ms | <200ms | <500ms |

**Cache Hit Rates (Expected):**
- L4 (Completion): 80%
- L3 (Types): 95%
- L2 (Macro): 99%
- L1 (Bytecode): 99.9%

---

## File Location

```
src/transam/
  transam.zig       # Main query engine
  cache.zig         # LRU, FreshnessLRU, MacroOutputCache
  network_cache.zig # Disk-based network cache
```

---

## References

- **Salsa book:** https://salsa-rs.github.io/salsa/
- **rust-analyzer architecture:** Three architectures for responsive IDE
- **Full design:** `../trans-am-architecture.md`
- **Gap analysis:** `../trans-am-gaps.md`
