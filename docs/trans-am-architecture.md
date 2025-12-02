# Trans-Am Query Engine Architecture

**Trans-Am** is Metascript's incremental computation engine, inspired by [Salsa](https://github.com/salsa-rs/salsa) (Rust's query framework used in rust-analyzer). The name comes from the muscle car - fast, powerful, and conceptually "beats Salsa at drag racing."

**Last Updated:** December 2024

---

## Design Philosophy

Metascript faces the **Nim/Haxe LSP Problem**: heavy macro expansion (like `@derive(Eq, Hash)` generating 500+ lines) causes sluggish editor responsiveness. Both Nim and Haxe have powerful metaprogramming but suffer from LSP performance issues.

**Our Solution:** Salsa-style red-green incremental computation + content-addressed macro output caching.

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    TRANS-AM QUERY ENGINE                        │
├─────────────────────────────────────────────────────────────────┤
│  INPUT LAYER (set by LSP, never cached)                         │
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
│    check_function(func_id) → Errors                             │
├─────────────────────────────────────────────────────────────────┤
│  CACHE LAYER                                                    │
│    LRU (bounded): parse, macro_expand                           │
│    Content-addressed: macro_output_hash → output_ast            │
│    Durability-partitioned: LOW/MEDIUM/HIGH                      │
├─────────────────────────────────────────────────────────────────┤
│  INTERNED LAYER (deduplicated, never evicted)                   │
│    symbol_interner, macro_call_interner, query_key_interner     │
└─────────────────────────────────────────────────────────────────┘
```

---

## Core Concepts

### 1. Revision Tracking

Every input change increments a global revision counter:

```zig
pub const Revision = struct {
    value: u64,

    pub fn increment(self: *Revision) void {
        self.value += 1;
    }
};
```

When a file changes:
1. Compute content hash
2. If hash unchanged → skip (content-addressed optimization)
3. If hash changed → increment revision, mark queries RED

### 2. Query States (Red-Green Algorithm)

```
       ┌─────────┐
       │  GREEN  │ ← Verified as current in this revision
       └────┬────┘
            │ input changed
            ▼
       ┌─────────┐
       │   RED   │ ← May be stale, needs verification
       └────┬────┘
            │ try_mark_green()
            ▼
       ┌─────────┐
       │ YELLOW  │ ← Currently being verified (cycle detection)
       └────┬────┘
            │
     ┌──────┴──────┐
     ▼             ▼
  GREEN         recompute
(deps unchanged)   │
                   ▼
                GREEN
            (store new value)
```

**Key Insight:** When dependencies change, we don't immediately recompute. Instead, `try_mark_green()` recursively checks if the OUTPUT actually changed. If not, we mark GREEN without recomputing dependents.

### 3. Durability Classification

Not all files change at the same rate:

```zig
pub const Durability = enum(u2) {
    low = 0,     // User-edited files (src/*.ms)
    medium = 1,  // Config files (*.json)
    high = 2,    // Library files (std/, node_modules/)
};
```

HIGH durability queries survive LOW durability invalidation. This prevents recomputing library type info when only user code changes.

### 4. Dependency Tracking

During query execution, we track which other queries were called:

```zig
pub const DependencyStack = struct {
    frames: ArrayList(Frame),

    const Frame = struct {
        query_key: QueryKey,
        dependencies: ArrayList(QueryKey),
        min_durability: Durability,
    };
};
```

This enables:
- Automatic dependency recording
- Cycle detection (YELLOW state)
- Durability propagation (min of all dependencies)

---

## Macro Firewall Pattern

The critical optimization for macro-heavy workloads. We split macro expansion into three queries:

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

**Why This Matters:**

Without firewall:
```typescript
@derive(Eq)        // Edit here...
class User { }
@derive(Hash)      // ...re-expands ALL 10 macros
class Item { }
// ... 8 more @derive calls
```

With firewall:
```typescript
@derive(Eq)        // Edit here...
class User { }
@derive(Hash)      // Only re-expand User's @derive
class Item { }     // Others stay GREEN!
```

---

## Content-Addressed Macro Output

Traditional caching: `(call_id, revision) → output`

**Problem:** Different input text can produce identical output (whitespace changes, comment changes).

Content-addressed caching: `hash(input_ast, args) → hash(output_ast)`

```zig
pub fn macroExpand(self: *Self, call_id: MacroCallId) !MacroExpansion {
    const input_hash = try self.macroInputHash(call_id);

    // Check content cache
    if (self.content_cache.get(input_hash)) |output_hash| {
        // Same input → same output (deterministic macro)
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

**Key Insight:** Even if the macro re-executes, if the OUTPUT hash matches the previous output, dependents (like type checker) stay GREEN.

---

## Two-Phase Type Context

Macros often need type information, but type checking needs macro expansion results. This creates a circular dependency.

**Solution:** Split type context into two phases:

```
PHASE 1: Shallow Context (before macro expansion)
├── Class names
├── Property names
├── Property type names (strings, not resolved)
└── Method signatures (shallow)

PHASE 2: Deep Context (after macro expansion)
├── Fully resolved types
├── Generic instantiations
└── Complete type checking
```

```zig
pub const MacroTypeContext = struct {
    shallow: ShallowContext,  // Available during macro expansion
    deep: ?DeepContext,       // Available after expansion
};
```

Macros like `@derive(Serialize)` only need shallow context (property names and types as strings). Deep context is computed after expansion for full type checking.

---

## Query Dependency Graph

```
file_text (INPUT)
    │
    ├──▶ parse
    │        │
    │        ├──▶ macro_call_sites
    │        │        │
    │        │        └──▶ macro_input_hash
    │        │                   │
    │        │                   └──▶ macro_expand ◀── macroExpander
    │        │                              │
    │        │                              ▼
    │        └─────────────────────▶ expanded_ast
    │                                       │
    └──▶ symbols ◀──────────────────────────┘
              │
              └──▶ type_of
                      │
                      └──▶ check_function
                                │
                                └──▶ ir_generate
```

---

## Performance Targets

| Operation | Target | Why |
|-----------|--------|-----|
| Token highlighting | <5ms | Independent of macros (tree-sitter) |
| Incremental re-expansion | <20ms | Single edit → single macro |
| Full file macro expansion | <200ms | Cold start |
| Type hover | <100ms | Cached type query |
| Semantic tokens | <200ms | After macro completion |

---

## Comparison with Other Approaches

### vs. Salsa (Rust)

| Aspect | Salsa | Trans-Am |
|--------|-------|----------|
| Language | Rust macros | Manual Zig |
| Tracking | Implicit via proc-macro | Explicit via DependencyStack |
| Memory | Arc + refcounting | Manual allocation |
| Content-addressing | Partial | First-class for macros |

**Why not use Salsa directly?** Salsa is Rust-specific. Metascript's compiler is in Zig.

### vs. Zig Sema

| Aspect | Zig Sema | Trans-Am |
|--------|----------|----------|
| Philosophy | Demand-driven monolithic | Query-based incremental |
| Caching | Per-instruction | Per-query |
| LSP focus | Not primary | Primary design goal |

**Why not Zig's approach?** Zig doesn't have macros that generate large AST chunks. We do.

### vs. Nim/Haxe

| Aspect | Nim/Haxe | Trans-Am |
|--------|----------|----------|
| Macro expansion | Per-file, cascading | Per-call, firewall |
| Change detection | Timestamp-based | Content-addressed |
| LSP performance | Poor (known issue) | Target <100ms |

**Why Trans-Am solves this:** The macro firewall prevents cascading invalidation. Content-addressing prevents unnecessary recomputation.

### CLI vs LSP: Shared Engine

**Decision:** Single Trans-Am engine serves both CLI and LSP (Zig-style, not rust-analyzer-style).

| rust-analyzer | Metascript |
|---------------|------------|
| Separate from rustc | CLI + LSP share Trans-Am |
| Duplicate parser/type-checker | Single implementation |
| Historical: RA built after rustc | Building both together |

**Broken code requirement:** Parser returns `(AST, errors)` via `synchronize()` recovery. CLI shows errors; LSP shows errors + provides features on valid parts.

---

## Implementation Status

### Phase 1: Core Infrastructure ✅
- [x] Revision type
- [x] QueryKey, QueryValue structures
- [x] DependencyStack
- [x] Durability classification
- [x] Tests (63 passing)

### Phase 2: Red-Green Algorithm ✅
- [x] try_mark_green() implementation
- [x] Cycle detection via yellow state (RedGreenError.CycleDetected)
- [x] storeQueryResult() for caching
- [x] getCachedQuery() with green verification
- [x] Value hashing (value_hash in QueryValue)
- [x] Tests for red-green algorithm

### Phase 3: Macro Optimization ✅
- [x] hashAstNode() function (content-addressed AST hashing)
- [x] MacroOutputCache (already existed, integrated)
- [x] 5-query decomposition (MacroCallInfo, getMacroCallSites, etc.)
- [x] Shallow type context (ShallowTypeContext struct)

### Phase 4: Parallel Highlighting ✅
- [x] Independent token highlighting (getSyntaxTokens)
- [x] Async macro expansion with cancellation
- [x] Progressive semantic token updates
- [x] MacroExpansionProgress notifications callback
- [x] Semantic tokens status tracking

### Phase 5: LSP Integration ✅
- [x] didChange → Trans-Am revision system
- [x] didOpen → Trans-Am file registration
- [x] Semantic tokens via Trans-Am getSyntaxTokens
- [x] Response time logging (benchmarking)
- [x] Incremental diagnostics via Trans-Am (getDiagnostics with caching)
- [x] Type hover with timing (getTokenAtPosition)

### Phase 6: Parser Integration ✅
- [x] parse(file_id) → AST query with caching
- [x] AST arena per file (memory management)
- [x] getMacroCallSites() extracts arguments from AST (not empty stubs)
- [x] getDiagnostics() includes parse errors (not just lexer)
- [x] Tests (10 new tests, all passing, no memory leaks)

### Phase 6.5: CLI Integration ✅
- [x] Wire `./msc compile` to Trans-Am queries
- [x] Wire `./msc check` to Trans-Am diagnostics
- [x] Wire `./msc pipeline` to show Trans-Am stages
- [x] Validate shared engine works for both CLI and LSP
- [ ] Add `--watch` mode using incremental caching (future)

### Phase 7: Macro Firewall ✅
- [x] `expandMacroCallSite()` with content-addressed caching
- [x] `generateMacroExpansion()` for @derive(Eq), @derive(Hash)
- [x] Firewall test: editing one macro doesn't invalidate others
- [x] 5 new tests (151 total)

### Phase 8: Async Threading Infrastructure ✅
- [x] `runExpansionSync()` - synchronous macro expansion with progress tracking
- [x] `invokeProgressCallback()` - progress notifications during expansion
- [x] `getExpansionProgress()` - query current expansion state
- [x] Thread-safe task management (`expansion_mutex`, `cancel_flag`)
- [x] Proper cleanup: thread joining on deinit, cancel_flag lifecycle
- [x] Fixed use-after-free bug in deinit ordering
- [x] Fixed deadlock in startAsyncMacroExpansion (mutex scope)
- [x] 5 new tests (156 total)
- [ ] **Future:** Enable real async threads (requires thread-safe caches)
  - LRUCache needs mutex protection
  - MacroOutputCache needs mutex protection
  - AST arenas need per-file locking

### Phase 9: Type Checker Integration ✅
- [x] `getSymbols(file_id)` query - returns symbol table for file
- [x] `checkFile(file_id)` query - type checks file, returns errors
- [x] `lookupSymbol(file_id, name)` helper - symbol lookup
- [x] `invalidateSymbolCache(file_id)` - cache invalidation
- [x] Cache integration with `setFileText()` - auto-invalidation
- [x] `TypeCheckResult` struct with errors and symbols
- [x] `CachedSymbolTable` and `CachedFunctionCheck` types
- [x] 8 new tests (164 total)

**Key Design Decisions:**
- Post-expansion type checking (Nim pattern): Parse → Type check
- Type checker caching per file (file_hash → CachedSymbolTable)
- TypeChecker owns SymbolTable (single allocation, clean lifecycle)
- Auto-invalidation when file content changes
- Integration with existing Trans-Am infrastructure

---

## References

### Local Source Code
- `~/projects/rust/src/tools/rust-analyzer/` - Salsa patterns
- `~/projects/zig/src/Sema.zig` - Demand-driven analysis
- `~/projects/typescript-go/internal/checker/` - Type caching

### Documentation
- [Salsa Book](https://salsa-rs.github.io/salsa/)
- [rust-analyzer Architecture](https://rust-analyzer.github.io/blog/2020/07/20/three-architectures-for-responsive-ide.html)
- [Zig Compiler Internals](https://mitchellh.com/zig/sema)

### Design Documents
- `/docs/type-checker.md` - Type checker research
- `/docs/lsp-architecture.md` - LSP design decisions

---

## Decision Log

| Date | Decision | Rationale |
|------|----------|-----------|
| Dec 2024 | Salsa-inspired red-green | Proven incremental algorithm |
| Dec 2024 | Content-addressed macro cache | Prevents cascading invalidation |
| Dec 2024 | Two-phase type context | Solves macro-type cycle |
| Dec 2024 | Manual Zig implementation | Precise control, no Rust dependency |
| Dec 2024 | Shared CLI/LSP engine | Single codebase, already handles broken code |
| Dec 2024 | Synchronous expansion first | Thread-safe caches needed before async; correctness over performance |
| Dec 2024 | Type checker Trans-Am integration | Caching for LSP responsiveness; parse-first pattern from Nim |
