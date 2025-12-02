# Code Generation

Design document for Metascript's backend code emission, informed by **Nim** and **Haxe** - production compilers that achieve excellent multi-backend performance with direct AST-to-target emission.

**Core Insight:** We don't need a separate IR. The Typed AST IS our intermediate representation. Quality of emitted code depends on HOW we emit, not on having an IR layer.

**Last Updated:** December 2024

---

## Executive Summary

**We don't need a separate IR.** The Typed AST IS our intermediate representation.

```
Metascript Pipeline (Simple):

  Source → Parse → Macros → Type Check → Transforms → Backend Emission
                                │              │            │
                                └──────────────┴────────────┘
                                   All work on Typed AST
```

**Why no SSA IR?**

| Target | Who Optimizes? | SSA Needed? |
|--------|---------------|-------------|
| C code | GCC/Clang (converts to SSA internally) | NO |
| JavaScript | V8/SpiderMonkey | NO |
| Erlang | BEAM VM | NO |

**We always emit to targets that have their own optimizers.** Building our own SSA IR would duplicate their work.

**Proof:** Nim achieves 90-95% of hand-written C performance with NO SSA IR.

---

## Reference Architectures

### Nim: AST IS IR (Our Primary Model)

```
Source → Parse → Sema + Macros → Typed AST → Transforms → Backends
                                      │
                    ┌─────────────────┼─────────────────┐
                    ▼                 ▼                 ▼
              cgen.nim          jsgen.nim            nifgen.nim
                 │                  │                    │
                 ▼                  ▼                    ▼
              C code           JavaScript              NIF
                 │
                 ▼
            GCC/Clang (does SSA optimization HERE)
```

**Nim proves:** You don't need SSA IR for multi-backend. Typed AST + shared transforms works.

**Key files to study:**
- `~/projects/nim/compiler/sem.nim` - Semantic analysis + macro expansion
- `~/projects/nim/compiler/transf.nim` - Shared AST transformations (48KB)
- `~/projects/nim/compiler/cgen.nim` - C backend (2.6K lines)
- `~/projects/nim/compiler/jsgen.nim` - JS backend (3.2K lines)

### Haxe: Type Resolution Patterns

```
Source → Parse → Type Check → texpr (Typed AST) → Filters → Backends
                                    │
          ┌────────────────────────┼────────────────────────────┐
          ▼                        ▼                            ▼
    genjs.ml (60KB)         gencpp.ml                    hl2c.ml (64KB)
```

**Haxe proves:** One type system can serve 6+ backends.

**Critical pattern - `follow()`:**
```zig
/// Every backend MUST call follow() before emitting code
/// Resolves type aliases, type variables, lazy types
pub fn follow(t: Type) Type {
    return switch (t) {
        .type_alias => |alias| follow(alias.underlying),
        .type_var => |tv| if (tv.bound) |bound| follow(bound) else t,
        .lazy => |lazy| follow(lazy.force()),
        else => t,
    };
}
```

**Key files to study:**
- `~/projects/haxe/src/core/tType.ml` - Type definitions
- `~/projects/haxe/src/core/tFunctions.ml` - `follow()` pattern
- `~/projects/haxe/src/generators/genjs.ml` - JS backend

---

## Metascript Architecture

### The Pipeline

```
┌─────────────────────────────────────────────────────────────────────────┐
│                      METASCRIPT COMPILATION PIPELINE                     │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  Source (.ms)                                                            │
│      │                                                                   │
│      ▼                                                                   │
│  ┌────────────┐                                                          │
│  │   PARSER   │ ──▶ AST                                                  │
│  └────────────┘                                                          │
│      │                                                                   │
│      ▼                                                                   │
│  ┌────────────┐                                                          │
│  │   MACROS   │ ──▶ Expanded AST (@derive, @comptime)                    │
│  │ (Trans-Am) │     Macros expand BEFORE type checking (Nim pattern)     │
│  └────────────┘                                                          │
│      │                                                                   │
│      ▼                                                                   │
│  ┌────────────┐                                                          │
│  │   SEMA     │ ──▶ TYPED AST  ◀── THIS IS OUR "IR"                      │
│  │ (Trans-Am) │     • Full type info on every node                       │
│  └────────────┘     • Field access fully resolved                        │
│      │              • follow() applied to all types                      │
│      ▼                                                                   │
│  ┌────────────┐                                                          │
│  │ TRANSFORMS │ ──▶ Optimized Typed AST                                  │
│  │  (shared)  │     • Constant inlining                                  │
│  └────────────┘     • Lambda lifting                                     │
│      │              • Dead code elimination                              │
│      │              • Hidden nodes for backends                          │
│      │                                                                   │
│      ├─────────────────────┬─────────────────────┐                       │
│      ▼                     ▼                     ▼                       │
│  ┌────────────┐      ┌────────────┐       ┌────────────┐                 │
│  │ C BACKEND  │      │ JS BACKEND │       │ERL BACKEND │                 │
│  │            │      │            │       │            │                 │
│  │  Direct    │      │  Direct    │       │  Direct    │                 │
│  │  emission  │      │  emission  │       │  emission  │                 │
│  │  from AST  │      │  from AST  │       │  from AST  │                 │
│  └────────────┘      └────────────┘       └────────────┘                 │
│      │                     │                     │                       │
│      ▼                     ▼                     ▼                       │
│   .c file              .js file             .erl file                    │
│      │                                                                   │
│      ▼                                                                   │
│   GCC/Clang                                                              │
│   (SSA optimization happens HERE, not in Metascript)                     │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Typed AST Structure

### Core Node Type

```zig
/// Every AST node after type checking
pub const TypedNode = struct {
    /// Original AST node kind
    kind: NodeKind,

    /// CRITICAL: Fully resolved type (follow() already applied)
    resolved_type: Type,

    /// Source location for error messages
    loc: SourceLoc,

    /// Children (kind-specific)
    children: []const *TypedNode,

    /// For field access: which class/interface? (Haxe pattern)
    field_context: ?FieldContext,

    /// Metadata (@inline, @extern, etc.)
    metadata: ?Metadata,
};

pub const FieldContext = union(enum) {
    /// obj.field where obj is instance of class
    instance: struct { class: *ClassType, field: *FieldDef },
    /// Class.staticField
    static_field: struct { class: *ClassType, field: *FieldDef },
    /// Anonymous object field
    anon: *FieldDef,
    /// Dynamic access (fallback)
    dynamic: []const u8,
};
```

### The `follow()` Pattern (CRITICAL)

Every type must be resolved before code generation:

```zig
pub fn follow(t: Type) Type {
    return switch (t) {
        // Type alias: type UserId = number;
        .type_alias => |alias| follow(alias.underlying),

        // Type variable from generics
        .type_var => |tv| if (tv.bound) |bound| follow(bound) else t,

        // Lazy type (for recursive types)
        .lazy => |lazy| follow(lazy.force()),

        // Nullable wrapper: Null<T> → T
        .nullable => |inner| follow(inner),

        // Already concrete
        else => t,
    };
}

// Usage in backend:
fn emitExpression(node: *TypedNode) void {
    const concrete_type = follow(node.resolved_type);  // ALWAYS call follow()!

    switch (concrete_type) {
        .number => emitNumber(node),
        .string => emitString(node),
        .class => |c| emitClassInstance(node, c),
        // ...
    }
}
```

---

## Shared Transformation Passes

Apply to Typed AST BEFORE backend selection (Nim pattern):

```zig
pub const TransformPipeline = struct {
    pub fn run(ast: *TypedAST) !void {
        // All backends benefit from these transforms
        try inlineConstants(ast);
        try liftLambdas(ast);          // Closures → explicit env parameter
        try insertHiddenNodes(ast);    // Backend-specific helpers
        try eliminateDeadCode(ast);
    }
};
```

### Hidden Nodes (Nim Pattern)

Insert nodes that don't appear in source but help backends:

```zig
pub const HiddenNodeKind = enum {
    hidden_deref,      // C needs explicit pointer dereference
    hidden_addr,       // C needs explicit address-of
    hidden_conv,       // Implicit type conversion made explicit
    hidden_call_conv,  // Calling convention wrapper
};
```

**Example:**
```typescript
// Source (Metascript)
const user: User = getUser();
print(user.name);

// After transform (C backend sees)
const user: User = getUser();
print(HIDDEN_DEREF(user).name);  // C needs explicit deref
```

---

## Code Quality: Nim vs Haxe Lesson

**Critical insight:** Both Nim and Haxe emit C, but with very different quality. This affects debuggability, performance, and interop.

| Factor | Good Output (Nim-style) | Bad Output (avoid) |
|--------|-------------------------|---------------------|
| Field access | `user->name` | `_hx_field(obj, _hx_hash("name"))` |
| Function calls | `greet(user)` | `_hx_call(fn_table, 42, args)` |
| Types | `struct User` | `Dynamic` with runtime checks |
| Debugging | Step through naturally | Jump through trampolines |
| Binary size | Minimal overhead | Runtime reflection bloat |

**Real comparison:**

```c
// Nim-generated C (GOOD - our target)
struct User {
    NimStringV2 name;
    NI age;
};

NimStringV2 getGreeting(struct User* u) {
    return nimAddStrings(3, "Hello ", u->name, "!");
}

// Haxe-generated C++ (complex - supports dynamic features)
::Dynamic User_obj::__Field(const ::String &inName, ::hx::PropertyAccess inCallProp) {
    switch(inName.length) {
        case 3: if (HX_FIELD_EQ(inName,"age")) return age;
        case 4: if (HX_FIELD_EQ(inName,"name")) return name;
    }
    return super::__Field(inName, inCallProp);
}
```

**Why Nim is cleaner:** Static types, no reflection, monomorphized generics.

**Metascript targets Nim-quality output:**

| Design Choice | Impact on Code Quality |
|---------------|----------------------|
| No reflection by default | Direct struct access |
| Monomorphized generics | No runtime type params |
| Static dispatch | Direct function calls |
| Value semantics | Stack allocation |

---

## Backend Code Generation

### Common Pattern (All Backends)

```zig
pub fn generate(typed_ast: *TypedAST, target: Target) ![]const u8 {
    var output = StringBuilder.init(allocator);

    // Walk the typed AST, emit target code
    for (typed_ast.declarations) |decl| {
        try emitDeclaration(&output, decl, target);
    }

    return output.toOwnedSlice();
}

fn emitDeclaration(out: *StringBuilder, decl: *TypedNode, target: Target) !void {
    const resolved = follow(decl.resolved_type);  // ALWAYS follow()!

    switch (decl.kind) {
        .function_decl => try emitFunction(out, decl, target),
        .class_decl => try emitClass(out, decl, target),
        .variable_decl => try emitVariable(out, decl, target),
        // ...
    }
}
```

### C Backend (Nim-style direct emission)

```zig
fn emitFunction(out: *StringBuilder, func: *TypedNode, _: Target) !void {
    const return_type = follow(func.return_type);

    // Emit C function signature
    try out.append(toCType(return_type));
    try out.append(" ");
    try out.append(func.name);
    try out.append("(");
    try emitParams(out, func.params);
    try out.append(") {\n");

    // Emit body
    try emitBlock(out, func.body);

    try out.append("}\n");
}

fn toCType(t: Type) []const u8 {
    const resolved = follow(t);
    return switch (resolved) {
        .number => "double",
        .boolean => "bool",
        .string => "char*",
        .void => "void",
        .class => |c| c.mangled_name,  // struct User*
        // ...
    };
}
```

### JavaScript Backend (even simpler)

```zig
fn emitFunction(out: *StringBuilder, func: *TypedNode, _: Target) !void {
    // JS is close to source - almost direct emission
    if (func.is_async) try out.append("async ");
    try out.append("function ");
    try out.append(func.name);
    try out.append("(");
    try emitParams(out, func.params);
    try out.append(") {\n");
    try emitBlock(out, func.body);
    try out.append("}\n");
}
```

### Erlang Backend

```zig
fn emitFunction(out: *StringBuilder, func: *TypedNode, _: Target) !void {
    // Erlang function
    try out.append(func.name);
    try out.append("(");
    try emitParams(out, func.params);
    try out.append(") ->\n");
    try emitFunctionalBody(out, func.body);  // Convert imperative → functional
    try out.append(".\n");
}
```

---

## Trans-Am Integration

### Queries

```zig
pub const QueryType = enum(u8) {
    parse,              // Source → AST
    macro_expand,       // AST → Expanded AST
    type_check,         // Expanded AST → Typed AST
    transform,          // Typed AST → Transformed AST
    emit_c,             // Transformed AST → C code
    emit_js,            // Transformed AST → JS code
    emit_erlang,        // Transformed AST → Erlang code
};
```

### Incremental Compilation

```
File Edit
    │
    ▼
parse(file) invalidated
    │
    ├──▶ Only changed functions re-parsed
    │
    ▼
type_check(file) invalidated
    │
    ├──▶ Red-green: check if types actually changed
    │
    ▼
emit_*(file) invalidated
    │
    └──▶ Regenerate only affected outputs
```

---

## Implementation Roadmap

### Phase 1: Typed AST Foundation
- [ ] Add `resolved_type` field to AST nodes
- [ ] Implement `follow()` type resolution
- [ ] Add `FieldContext` for resolved field access
- [ ] Typed AST printer (for debugging)

### Phase 2: Shared Transformations
- [ ] Constant inlining pass
- [ ] Lambda lifting (closures → explicit env)
- [ ] Hidden node insertion
- [ ] Dead code elimination

### Phase 3: JavaScript Backend (Easiest)
- [ ] Direct emission from Typed AST
- [ ] Class → ES6 class
- [ ] Source map generation
- [ ] Tests: round-trip execution

### Phase 4: C Backend
- [ ] Direct emission from Typed AST
- [ ] Struct generation for classes
- [ ] Memory management patterns
- [ ] Tests: compile with GCC, run

### Phase 5: Erlang Backend
- [ ] Class → record/map transformation
- [ ] Module/export generation
- [ ] Pattern matching emission
- [ ] Tests: compile with erlc, run

### Phase 6: Trans-Am Integration
- [ ] Wire all stages as queries
- [ ] Incremental regeneration
- [ ] Cache invalidation

---

## Why NOT SSA IR?

| Concern | Answer |
|---------|--------|
| "But Hermes uses SSA!" | Hermes also targets bytecode (HBC). We only target languages with optimizing compilers. |
| "What about optimization?" | GCC/Clang/V8/BEAM do optimization. We'd be duplicating their work. |
| "What about register allocation?" | Only matters for machine code. We emit C text; GCC handles registers. |
| "What about constant propagation?" | We can do this on the Typed AST. No SSA needed. |
| "What if C backend is slow?" | Measure first. Nim achieves 90-95% of C performance without SSA. |

**The rule:** Only build SSA IR if we're targeting something that doesn't have its own optimizer (e.g., our own bytecode VM, direct machine code). We're not.

---

## Backend-Specific Documentation

Each backend has its own detailed implementation guide with architecture patterns, optimization strategies, and reference analyses:

### C Backend
**[codegen-c.md](./codegen-c.md)** - C code generation implementation guide
- Nim C backend analysis (90-95% of C performance)
- Type mapping strategies (primitives, structs, pointers)
- Name mangling for module isolation
- Small struct optimization (pass by value ≤16 bytes)
- Function generation patterns
- Memory model and safety considerations
- Performance benchmarks and expectations

**Status**: ✅ Basic implementation complete (structs, type mapping)
**Next**: Function bodies, expressions, statements

### JavaScript Backend
**[codegen-js.md](./codegen-js.md)** - JavaScript code generation (planned)
- Direct emission patterns
- ES6+ feature mapping
- Source map generation
- Tree-shaking considerations
- Browser/Node.js compatibility

**Status**: 🚧 Planned

### Erlang Backend
**[codegen-erlang.md](./codegen-erlang.md)** - Erlang/BEAM code generation (planned)
- Class → record/map transformation
- Module/export generation
- Pattern matching emission
- OTP integration patterns
- Distributed systems considerations

**Status**: 🚧 Planned

---

## References

### Primary (Study These!)

**Nim:**
- `~/projects/nim/compiler/sem.nim` - Semantic analysis + macros
- `~/projects/nim/compiler/transf.nim` - Shared transforms (48KB)
- `~/projects/nim/compiler/cgen.nim` - C backend
- `~/projects/nim/compiler/jsgen.nim` - JS backend

**Haxe:**
- `~/projects/haxe/src/core/tType.ml` - Type definitions
- `~/projects/haxe/src/core/tFunctions.ml` - `follow()` pattern
- `~/projects/haxe/src/generators/genjs.ml` - JS backend

### Secondary

- `~/projects/hermes/lib/BCGen/SH/SH.cpp` - C emission patterns (useful for idioms, not SSA)

---

## Decision Log

| Date | Decision | Rationale |
|------|----------|-----------|
| Dec 2024 | **No SSA IR** | GCC/Clang/V8/BEAM optimize for us |
| Dec 2024 | **Typed AST as IR** | Nim/Haxe prove it works |
| Dec 2024 | **`follow()` mandatory** | Haxe pattern; all backends need it |
| Dec 2024 | **Macros before typing** | Nim pattern; backends see concrete code |
| Dec 2024 | **Shared transforms** | Write once, benefit all backends |
| Dec 2024 | **JS backend first** | Closest to source; validates pipeline |
| Dec 2024 | **C backend priority** | iOS, embedded, bigger ecosystem than Zig |
| Dec 2024 | **Nim-quality C output** | Direct struct access, no runtime reflection |
| Dec 2024 | **Zig backend optional** | Easy to add later (~3-5 days), same LLVM perf |
| Dec 2024 | **No `struct` keyword** | Just `class`; compiler optimizes for C; JS/Erlang do what's natural |

---

## Summary

```
Metascript Codegen = Nim's "AST as IR" approach
                   + Haxe's type resolution (follow())
                   + Nim-quality C output (direct, debuggable)
                   + Cheap backend addition (~20% unique per target)
                   + Let GCC/V8/BEAM handle optimization
```

**Backend Priority:** C (maximum reach) → JS (browser/npm) → Erlang (distributed) → Zig (optional, easy add)

**We build a good type system and clean code emission. Target compilers do the heavy optimization.**
