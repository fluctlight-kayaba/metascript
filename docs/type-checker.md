# Type Checker Design

Research and design document for Metascript's type checking system.

**Last Updated:** December 2024

---

## Executive Summary

This document captures research from production compilers (Rust, Zig, TypeScript) to inform Metascript's type checker design. Key insight: **our current batch type checker must integrate with Trans-Am for incremental compilation**.

---

## 1. Production Type Checker Architectures

### 1.1 Salsa Framework (Rust)

**Source:** `~/projects/rust/src/tools/rust-analyzer/crates/`

Salsa is Rust's incremental computation framework. It's what Trans-Am is inspired by.

#### Core Concepts

```
┌─────────────────────────────────────────────────────────────────┐
│                    SALSA QUERY SYSTEM                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  #[salsa::input]     → Never cached, set externally             │
│  #[salsa::tracked]   → Memoized, dependencies tracked           │
│  #[salsa::interned]  → Deduplicated, canonical IDs              │
│                                                                 │
│  Database = Collection of query groups                          │
│  Query = Pure function: (db, inputs) → output                   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### Red-Green Algorithm

The key to incremental re-computation:

```
When input changes:
1. Mark dependent queries as "potentially dirty"
2. For each dirty query:
   a. Check if inputs actually changed (try_mark_green)
   b. If inputs unchanged → GREEN (reuse cached result)
   c. If inputs changed → recompute
   d. If new result == old result → GREEN
   e. If new result != old result → RED (propagate)
```

#### rust-analyzer Example (from source)

```rust
// From ~/projects/rust/src/tools/rust-analyzer/crates/hir-def/src/db.rs

#[salsa::interned]
fn intern_function(&self, loc: FunctionLoc) -> FunctionId;

#[salsa::tracked]
fn function_signature(&self, e: FunctionId) -> Arc<FunctionSignature> {
    self.function_signature_with_source_map(e).0
}

#[salsa::tracked]
fn trait_signature(&self, trait_: TraitId) -> Arc<TraitSignature>;

#[salsa::tracked]
fn impl_signature(&self, impl_: ImplId) -> Arc<ImplSignature>;
```

**Key Pattern:** Every signature lookup is a tracked query that:
- Takes an interned ID (cheap equality)
- Returns Arc (shared ownership, cheap clone)
- Auto-memoized by Salsa

---

### 1.2 Zig's Semantic Analysis (Sema.zig)

**Source:** `~/projects/zig/src/Sema.zig`

Zig's approach: **demand-driven, comptime-integrated type checking**.

#### Core Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    ZIG COMPILATION PIPELINE                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Source → AST → AstGen → ZIR → Sema → AIR → Codegen             │
│                           │      │                              │
│                    Untyped IR   Typed IR                        │
│                                  │                              │
│                           Type checking +                       │
│                           Comptime evaluation                   │
│                           happen together!                      │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### Key Insight: Types ARE Values

```zig
// From Sema.zig header comment:
// "Transforms untyped ZIR instructions into semantically-analyzed AIR instructions.
//  Does type checking, comptime control flow, and safety-check generation.
//  This is the heart of the Zig compiler."

// Sema struct (simplified)
const Sema = @This();
pt: Zcu.PerThread,
gpa: Allocator,
arena: Allocator,
code: Zir,                    // Input: untyped IR
air_instructions: ...,        // Output: typed IR
inst_map: InstMap,            // ZIR → AIR mapping
func_index: InternPool.Index, // Current function
fn_ret_ty: Type,              // Return type (may be inferred)
```

#### Demand-Driven Analysis

```zig
// Only analyze what's referenced
// From InstMap in Sema.zig:

pub const InstMap = struct {
    items: []Air.Inst.Ref = &[_]Air.Inst.Ref{},
    start: Zir.Inst.Index = @enumFromInt(0),

    pub fn get(map: InstMap, key: Zir.Inst.Index) ?Air.Inst.Ref {
        if (!map.contains(key)) return null;
        return map.items[@intFromEnum(key) - @intFromEnum(map.start)];
    }
};
```

**Key Pattern:** Lazy evaluation - only analyze functions that are actually called.

---

### 1.3 TypeScript's Type Checker

**Source:** `~/projects/typescript-go/internal/checker/`

TypeScript uses **structural typing** with **control flow analysis**.

#### Checker Architecture

```go
// From ~/projects/typescript-go/internal/checker/checker.go

type CheckMode uint32

const (
    CheckModeNormal               CheckMode = 0      // Normal type checking
    CheckModeContextual           CheckMode = 1 << 0 // Contextual type assigned
    CheckModeInferential          CheckMode = 1 << 1 // Inferential typing
    CheckModeSkipContextSensitive CheckMode = 1 << 2 // Skip context sensitive
    CheckModeSkipGenericFunctions CheckMode = 1 << 3 // Skip generics
    CheckModeIsForSignatureHelp   CheckMode = 1 << 4 // Signature help mode
)

// Type caching by kind
type CachedTypeKind int32

const (
    CachedTypeKindLiteralUnionBaseType CachedTypeKind = iota
    CachedTypeKindIndexType
    CachedTypeKindApparentType
    CachedTypeKindAwaitedType
    CachedTypeKindEvolvingArrayType
    // ... many more
)
```

#### Type Inference (from source)

```go
// From ~/projects/typescript-go/internal/checker/inference.go

type InferenceState struct {
    inferences        []*InferenceInfo
    originalSource    *Type
    originalTarget    *Type
    priority          InferencePriority
    contravariant     bool
    bivariant         bool
    visited           map[InferenceKey]InferencePriority  // Memoization!
    sourceStack       []*Type  // Cycle detection
    targetStack       []*Type
}

func (c *Checker) inferTypes(inferences []*InferenceInfo,
                             originalSource *Type,
                             originalTarget *Type,
                             priority InferencePriority,
                             contravariant bool) {
    n := c.getInferenceState()  // Pool allocation
    n.inferences = inferences
    n.originalSource = originalSource
    n.originalTarget = originalTarget
    // ... inference logic
    c.putInferenceState(n)  // Return to pool
}
```

**Key Pattern:**
- Memoization via `visited` map
- Object pooling (`getInferenceState`/`putInferenceState`)
- Stack-based cycle detection

---

## 2. Metascript's Current Implementation

### 2.1 What We Have

```
src/checker/
├── symbol.zig      ✅ Symbol table with scopes
├── resolver.zig    ✅ Type reference resolution
├── inference.zig   ✅ Type inference for expressions
└── typechecker.zig ✅ Main checker (batch mode)

src/transam/
└── transam.zig     ✅ Query engine (Salsa-inspired)
                    ❌ Not integrated with type checker!
```

### 2.2 Current Type Checker Flow (Batch)

```
┌─────────────────────────────────────────────────────────────────┐
│                 CURRENT: BATCH TYPE CHECKING                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. collectDeclarations(program)  → Build symbol table          │
│  2. resolver.resolve(program)     → Resolve type references     │
│  3. inference.infer(program)      → Infer expression types      │
│  4. checkTypes(program)           → Validate compatibility      │
│                                                                 │
│  Problem: Recomputes EVERYTHING on any change!                  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 2.3 Trans-Am (Unused Potential)

```zig
// From src/transam/transam.zig

pub const TransAmDatabase = struct {
    // These caches exist but aren't used by type checker!
    type_cache: std.AutoHashMap(u64, TypeInference),  // ← Empty!
    ir_cache: std.AutoHashMap(u64, UnifiedIR),        // ← Empty!

    // These are working
    parse_cache: LRUCache(ParseResult),
    macro_cache: LRUCache(MacroExpansion),
};
```

---

## 3. Target Architecture

### 3.1 Query-Based Type Checking

```
┌─────────────────────────────────────────────────────────────────┐
│               TARGET: INCREMENTAL TYPE CHECKING                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Trans-Am Database                                              │
│  ├── INPUT: file_texts (set by LSP)                             │
│  │                                                              │
│  ├── QUERY: parse(file_id) → AST                                │
│  │   └── depends on: file_texts[file_id]                        │
│  │                                                              │
│  ├── QUERY: macro_expand(call_id) → expanded AST                │
│  │   └── depends on: parse, macro_def                           │
│  │                                                              │
│  ├── QUERY: symbols(file_id) → SymbolTable      ← NEW!          │
│  │   └── depends on: parse, macro_expand                        │
│  │                                                              │
│  ├── QUERY: type_of(expr_id) → Type             ← NEW!          │
│  │   └── depends on: symbols, parent types                      │
│  │                                                              │
│  ├── QUERY: check_function(func_id) → Errors    ← NEW!          │
│  │   └── depends on: type_of for all expressions                │
│  │                                                              │
│  └── QUERY: ir_lower(func_id) → IR                              │
│      └── depends on: check_function                             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 3.2 Integration Plan

**Phase 1: Add type queries to Trans-Am**

```zig
// Add to TransAmDatabase

// QUERY: Get symbols for a file
pub fn symbols(self: *Self, file_id: FileId) !*SymbolTable {
    const hash = hashFileId(file_id);

    if (self.symbol_cache.get(hash)) |cached| {
        return cached;
    }

    // Cache miss: build symbol table
    const ast = try self.parse(file_id);
    const symbols = try self.buildSymbols(ast);
    try self.symbol_cache.put(hash, symbols);

    return symbols;
}

// QUERY: Get type of expression
pub fn typeOf(self: *Self, expr_id: ExprId) !*Type {
    const hash = hashExprId(expr_id);

    if (self.type_cache.get(hash)) |cached| {
        return cached;
    }

    // Cache miss: infer type
    const expr = self.getExpr(expr_id);
    const type_ = try self.inferExprType(expr);
    try self.type_cache.put(hash, type_);

    return type_;
}
```

**Phase 2: Granular invalidation**

```zig
pub fn setFileText(self: *Self, file_id: FileId, text: []const u8) !void {
    // ... existing code ...

    // Invalidate type cache for this file's expressions
    self.invalidateTypesForFile(file_id);

    // Red-green: mark dependent queries, don't recompute yet
    self.markDirty(file_id);
}
```

**Phase 3: LSP integration**

```zig
// On hover: get type without full recompilation
pub fn getHoverType(self: *Self, file_id: FileId, position: Position) !?*Type {
    const expr_id = try self.exprAtPosition(file_id, position);
    return self.typeOf(expr_id);  // Uses cache!
}
```

---

## 4. Key Design Patterns

### 4.1 Interning (from Salsa/rust-analyzer)

```zig
// Intern all identifiers for cheap equality
pub const SymbolId = u64;

pub fn internSymbol(self: *Self, name: []const u8) !SymbolId {
    if (self.symbol_map.get(name)) |id| {
        return id;  // Already interned
    }

    const id = self.next_symbol_id;
    self.next_symbol_id += 1;
    try self.symbol_map.put(name, id);
    return id;
}
```

### 4.2 Demand-Driven Analysis (from Zig)

```zig
// Only analyze referenced functions
pub fn analyzeFunction(self: *Self, func_id: FunctionId) !void {
    if (self.analyzed.contains(func_id)) return;

    // Analyze body
    const body = self.getFunctionBody(func_id);
    for (body.instructions) |inst| {
        try self.analyzeInstruction(inst);
    }

    self.analyzed.put(func_id, {});
}
```

### 4.3 Type Caching (from TypeScript)

```zig
// Cache computed types with composite keys
const TypeCacheKey = struct {
    kind: CachedTypeKind,
    type_id: TypeId,
};

pub fn getApparentType(self: *Self, type_: *Type) !*Type {
    const key = TypeCacheKey{ .kind = .apparent, .type_id = type_.id };

    if (self.type_cache.get(key)) |cached| {
        return cached;
    }

    const apparent = try self.computeApparentType(type_);
    try self.type_cache.put(key, apparent);
    return apparent;
}
```

### 4.4 Inference State Pooling (from TypeScript)

```zig
// Reuse inference state objects to reduce allocation
pub fn getInferenceState(self: *Self) *InferenceState {
    if (self.free_states.popOrNull()) |state| {
        state.reset();
        return state;
    }
    return self.allocator.create(InferenceState);
}

pub fn putInferenceState(self: *Self, state: *InferenceState) void {
    self.free_states.append(state);
}
```

---

## 5. Implementation Roadmap

### Phase 1: Foundation (Current)
- [x] Symbol table with scopes
- [x] Type resolution
- [x] Type inference (batch)
- [x] Type checking (batch)

### Phase 2: Query Integration
- [ ] Define type queries in Trans-Am
- [ ] Move symbol table to query system
- [ ] Move type inference to query system
- [ ] Add cache invalidation

### Phase 3: Incremental
- [ ] Implement red-green algorithm
- [ ] Fine-grained dependency tracking
- [ ] Partial re-checking

### Phase 4: LSP Integration
- [ ] Hover type queries
- [ ] Completion with type info
- [ ] Real-time error checking

---

## 6. References

### Local Source Code
- `~/projects/rust/src/tools/rust-analyzer/` - rust-analyzer implementation
- `~/projects/zig/src/Sema.zig` - Zig semantic analysis
- `~/projects/typescript-go/internal/checker/` - TypeScript checker (Go port)

### Documentation
- [Salsa Book](https://salsa-rs.github.io/salsa/)
- [rust-analyzer Architecture](https://rust-analyzer.github.io/blog/2020/07/20/three-architectures-for-responsive-ide.html)
- [Zig Compiler Internals](https://mitchellh.com/zig/sema)
- [TypeScript Compiler Notes](https://github.com/microsoft/TypeScript/wiki/Architectural-Overview)

### Key Papers
- "A Theory of Type Polymorphism in Programming" (Hindley-Milner)
- "Incremental Computation with Names" (Adapton)

---

## 7. Decision Log

| Date | Decision | Rationale |
|------|----------|-----------|
| Dec 2024 | Batch type checker first | Get something working, iterate |
| Dec 2024 | Keep Trans-Am separate | Clean architecture, can integrate later |
| Dec 2024 | Research before Phase 2 | Learn from production compilers |

---

## 8. New Research Findings (December 2024)

Deep exploration of four production compilers revealed critical patterns for Metascript.

### 8.1 rust-analyzer: Query-Based Type System

**Key Pattern: Immutable InferenceResult**

rust-analyzer returns ALL type information for a function in one immutable struct:

```rust
// From crates/hir-ty/src/infer.rs:479
pub struct InferenceResult<'db> {
    type_of_expr: ArenaMap<ExprId, Ty<'db>>,      // Every expression's type
    type_of_pat: ArenaMap<PatId, Ty<'db>>,        // Every pattern's type
    type_mismatches: FxHashMap<ExprOrPatId, TypeMismatch<'db>>,
    diagnostics: Vec<InferenceDiagnostic<'db>>,
    expr_adjustments: FxHashMap<ExprId, Box<[Adjustment<'db>]>>,  // Coercions
    has_errors: bool,
}
```

**Adoption:** Create `TypeCheckResult` struct holding all type info per function. Query once, index into it.

**Key Pattern: SourceAnalyzer Bridge**

Maps between syntax positions (LSP) and semantic info (types):

```rust
// From crates/hir/src/source_analyzer.rs
pub(crate) fn type_of_expr(&self, db: &'db dyn HirDatabase, expr: &ast::Expr)
    -> Option<(Type<'db>, Option<Type<'db>>)> {
    let expr_id = self.expr_id(expr.clone())?;  // Syntax → HIR
    let infer = self.infer()?;                   // Get cached inference
    let ty = infer[expr_id];                     // O(1) lookup
    Some((mk_ty(ty), coerced.map(mk_ty)))
}
```

**Adoption:** Create similar bridge for Trans-Am → LSP features.

---

### 8.2 Zig Sema: Demand-Driven Analysis

**Key Pattern: Type Interning**

Types are never duplicated - only indices passed around:

```zig
// Type comparison is O(1) - just index equality
pub fn eql(a: Type, b: Type) bool {
    return a.ip_index == b.ip_index;
}
```

**Adoption:** Intern all types in Trans-Am for cheap comparison.

**Key Pattern: InstMap for Dense Lookups**

Array-based mapping (not hash table) for hot paths:

```zig
pub const InstMap = struct {
    items: []Air.Inst.Ref,
    start: Zir.Inst.Index,

    pub fn get(map: InstMap, key: Zir.Inst.Index) ?Air.Inst.Ref {
        return map.items[@intFromEnum(key) - @intFromEnum(map.start)];
    }
};
```

**Adoption:** Use dense arrays for expression → type mapping within a function.

---

### 8.3 Nim: Macro + Type Integration

**Critical Insight: Post-Expansion Type Checking**

Nim does NOT type-check macros before expansion. Instead:

```
WRONG: Type-check macro → expand → done
RIGHT: Expand macro → type-check OUTPUT → done
```

From `sem.nim:457-522` (semAfterMacroCall):
1. Evaluate macro at compile-time
2. Get expanded AST
3. **Re-type-check with full type checker**
4. Validate against expected return type
5. Continue compilation

**Adoption:** Trans-Am flow must be:
```
parse → getMacroCallSites → expandMacro → THEN symbols → THEN type_of
```

**Key Pattern: Three Levels of Type Flexibility**

- `tyUntyped` - Re-infer all types (for AST transformations)
- `tyTyped` - Output must be well-typed (for DSLs)
- Specific types - Must match exactly (for type-constrained macros)

**Adoption:** Support flexible macro return types via metadata.

---

### 8.4 Haxe: Multi-Backend Type System

**Critical Insight: One Type System, Multiple Interpretations**

Haxe proves multi-backend compilation is practical:
- Single unified Type AST
- Metadata annotations guide backend behavior
- "Follow functions" resolve types per-backend context

```ocaml
(* From core/tFunctions.ml - three variants *)
let follow t = ...           (* Basic type following *)
let follow_concrete t = ...  (* Resolve to concrete type *)
let follow_for_backend t = ... (* Backend-specific resolution *)
```

**Adoption for Metascript:**

```zig
const Type = union(enum) {
    number,      // Unified type
    string,
    class: ClassType,
    // ...
};

// Backend interprets:
// C:      number → double
// JS:     number → number (native)
// Erlang: number → float()
```

**Key Pattern: Abstract Types as Bridge**

Haxe's `abstract` types bridge between Haxe types and runtime types:

```haxe
abstract Int(Int32) {
    // Int is Haxe type, Int32 is underlying type
    // Different backends use different underlying types
}
```

**Adoption:** Consider abstract types for FFI and backend-specific optimizations.

---

## 9. Updated Implementation Roadmap

### Phase 1: Trans-Am Type Queries ← **START HERE**

```zig
// Add to QueryType enum
pub const QueryType = enum(u8) {
    // ... existing ...
    symbols,          // NEW: Build symbol table for file
    type_of,          // NEW: Type of expression
    check_function,   // NEW: Validate function
};

// New query implementations
pub fn getSymbols(self: *Self, file_id: []const u8) !*SymbolTable
pub fn typeOf(self: *Self, file_id: []const u8, expr_id: NodeId) !Type
pub fn checkFunction(self: *Self, file_id: []const u8, func_id: NodeId) !TypeCheckResult
```

### Phase 2: Complete Type Inference

Fill TODOs in inference.zig:
- [ ] Member expression types (`obj.prop`)
- [ ] Function types (for call validation)
- [ ] Array element unification
- [ ] Generic instantiation

### Phase 3: Macro-Aware Type Checking

Following Nim's pattern:
- [ ] Expand macros first via Trans-Am
- [ ] Type-check expanded AST
- [ ] Support flexible return types

### Phase 4: Multi-Backend Types

Following Haxe's pattern:
- [ ] Type metadata for backend hints
- [ ] Abstract types for FFI
- [ ] Backend-specific type resolution

---

## 10. Decision Log (Updated)

| Date | Decision | Rationale |
|------|----------|-----------|
| Dec 2024 | Batch type checker first | Get something working, iterate |
| Dec 2024 | Keep Trans-Am separate | Clean architecture, can integrate later |
| Dec 2024 | Research before Phase 2 | Learn from production compilers |
| Dec 2024 | Post-expansion type checking | Nim pattern - expand macros THEN type check |
| Dec 2024 | Immutable TypeCheckResult | rust-analyzer pattern - query once, index into it |
| Dec 2024 | Type interning | Zig pattern - O(1) type comparison |
| Dec 2024 | One type system, multiple backends | Haxe pattern - unified types, backend interprets |

---

**Next Steps:**
1. ~~Review this document~~ ✅
2. **Implement Phase 1: Trans-Am type queries** ← NOW
3. Wire existing type checker into Trans-Am queries
4. Add LSP hover/diagnostics using cached types
