# Architecture Overview

This document describes the internal architecture of the Metascript compiler, runtime, and tooling.

## System Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Metascript Toolchain                     │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  Source Code (.mts)                                         │
│       ↓                                                      │
│  [Parser] → AST                                             │
│       ↓                                                      │
│  [Macro Expander] → Expanded AST                            │
│       ↓                                                      │
│  [Type Checker] → Typed AST                                 │
│       ↓                                                      │
│  [Monomorphization] → Specialized AST                       │
│       ↓                                                      │
│  [Zig IR Generator] → Zig IR                                │
│       ↓                                                      │
│  [Backend]                                                   │
│    ├─ LLVM IR → Native Binary                              │
│    └─ C Code → GCC/Clang → Native Binary                   │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## Compiler Pipeline

### 1. Parser

**Language:** Zig
**Input:** Source code (`.mts` files)
**Output:** Abstract Syntax Tree (AST)

**Responsibilities:**
- Lexical analysis (tokenization)
- Syntax parsing (TypeScript grammar)
- AST construction
- Source position tracking (for error messages)

**Key Design Decisions:**
- **TypeScript-compatible syntax**: We reuse TypeScript's grammar with restrictions
- **Incremental parsing**: For LSP responsiveness, parse changed files only
- **Error recovery**: Continue parsing after errors to show multiple issues

**AST Structure:**
```zig
const AST = struct {
    root: *Node,
    source_map: SourceMap,
    errors: []ParseError,
};

const Node = union(enum) {
    class_decl: ClassDeclaration,
    function_decl: FunctionDeclaration,
    interface_decl: InterfaceDeclaration,
    variable_decl: VariableDeclaration,
    // ... more node types
};
```

### 2. Macro Expander

**Input:** AST
**Output:** Expanded AST (with macro-generated code)

**Responsibilities:**
- Execute `@comptime` blocks
- Run `@derive` macros
- Inline macro-generated code
- Track macro expansion for debugging

**Execution Model:**
- Macros run in sandboxed Zig environment
- Access to AST via API
- Type information available
- File system access (read-only)
- Environment variable access

**Example Flow:**
```
@derive(Eq) class User { name: string; }
    ↓ [Macro Expander]
class User {
    name: string;
    equals(other: User): boolean {
        return this.name === other.name;
    }
}
```

### 3. Type Checker

**Input:** Expanded AST
**Output:** Typed AST (with type annotations)

**Responsibilities:**
- Type inference
- Type checking (all nodes must have concrete types)
- Generic resolution
- Error reporting

**Type System:**
```zig
const Type = union(enum) {
    primitive: PrimitiveType,  // number, string, boolean
    struct_type: StructType,   // interfaces compiled to structs
    class_type: ClassType,     // nominal types
    generic: GenericType,      // T, U, etc.
    function: FunctionType,    // (a: number) => string
    union_type: UnionType,     // A | B
    array: ArrayType,          // T[]
};
```

**Key Features:**
- **Nominal typing for classes**: `class A` ≠ `class B` even if shape is the same
- **Structural typing for interfaces**: Shape-based compatibility
- **No `any`**: All types must be statically resolvable
- **Strict null checking**: `T` and `T | null` are distinct

### 4. Monomorphization

**Input:** Typed AST with generics
**Output:** Specialized AST (generics → concrete types)

**Responsibilities:**
- Generate specialized versions of generic functions/classes
- Eliminate type parameters
- Optimize for specific types

**Example:**
```typescript
function identity<T>(x: T): T { return x; }

const a = identity<number>(42);
const b = identity<string>("hello");

// After monomorphization:
function identity_number(x: number): number { return x; }
function identity_string(x: string): string { return x; }

const a = identity_number(42);
const b = identity_string("hello");
```

**Benefits:**
- No runtime type dispatch
- Better optimization opportunities
- Predictable performance

### 5. Zig IR Generator

**Input:** Specialized AST
**Output:** Zig intermediate representation

**Responsibilities:**
- Convert AST to Zig code
- Memory allocation strategy (stack vs heap)
- Call site optimization
- Generate RTTI (if requested)

**Object Model:**
```zig
// Interface → Struct
interface Point { x: number; y: number; }
    ↓
const Point = struct {
    x: f64,
    y: f64,
};

// Class → Struct + VTable
class Animal { speak(): void; }
    ↓
const Animal_VTable = struct {
    speak: *const fn(*Animal) void,
};

const Animal = struct {
    vtable: *const Animal_VTable,
};
```

### 6. Backend

**Option A: LLVM Backend**
- Zig IR → LLVM IR
- LLVM optimizations
- Native code generation

**Option B: C Backend**
- Zig IR → C code
- Compile with GCC/Clang
- Better portability

**Trade-offs:**
| Aspect | LLVM | C |
|--------|------|---|
| Performance | Slightly better | Good |
| Compile time | Slower | Faster |
| Portability | Requires LLVM | Requires C compiler |
| Debugging | DWARF debug info | GDB-friendly |
| Binary size | Smaller | Comparable |

## Runtime System

### Memory Management

**Generational Garbage Collector:**

```
┌──────────────────────────────────────┐
│         Heap Memory Layout           │
├──────────────────────────────────────┤
│  Young Generation (nursery)          │
│  ├─ Eden space (new allocations)     │
│  └─ Survivor spaces (S0, S1)         │
├──────────────────────────────────────┤
│  Old Generation (tenured)            │
│  └─ Long-lived objects               │
└──────────────────────────────────────┘
```

**Allocation Strategy:**
- **Young gen**: Fast bump-pointer allocation
- **Minor GC**: Copy survivors to old gen
- **Major GC**: Full heap collection (rare)

**Alternative: ARC Mode:**
```typescript
// Enable ARC for specific modules
@memory(strategy: "arc")
module MyModule {
    class Node {
        value: number;
        next: Node | null;
    }
}
// Reference counting instead of GC
```

### Stack Allocation

Value types are stack-allocated when possible:

```typescript
interface Point { x: number; y: number; }

function distance(a: Point, b: Point): number {
    // a and b are stack-allocated (no heap, no GC)
    const dx = a.x - b.x;
    const dy = a.y - b.y;
    return Math.sqrt(dx * dx + dy * dy);
}
```

### Panic Handling

```zig
// Runtime panic infrastructure
pub fn panic(msg: []const u8, trace: ?*StackTrace) noreturn {
    stderr.print("Panic: {s}\n", .{msg});
    if (trace) |t| {
        printStackTrace(t);
    }
    os.exit(1);
}
```

## Tooling Architecture

### LSP Server

```
┌─────────────────────────────────────────────┐
│            LSP Server                       │
├─────────────────────────────────────────────┤
│  [JSON-RPC] ← VS Code / Editor             │
│       ↓                                     │
│  [Request Handler]                          │
│    ├─ textDocument/completion              │
│    ├─ textDocument/hover                   │
│    ├─ textDocument/definition              │
│    └─ textDocument/publishDiagnostics      │
│       ↓                                     │
│  [Incremental Compiler]                     │
│    ├─ Parse changed files                  │
│    ├─ Type check incrementally             │
│    ├─ Cache AST/types                      │
│    └─ Report errors in real-time           │
│       ↓                                     │
│  [Response] → VS Code / Editor              │
└─────────────────────────────────────────────┘
```

**Performance Target:** <200ms response time for autocomplete/hover

**Key Optimizations:**
- **Incremental parsing**: Only parse changed files
- **Lazy type checking**: Check visible files first
- **AST caching**: Reuse unchanged AST nodes
- **Parallel processing**: Type check files concurrently

### Build System

```bash
# metascript.json
{
  "name": "my-app",
  "version": "0.1.0",
  "dependencies": {
    "@metascript/core": "0.1.0"
  },
  "build": {
    "target": "native",
    "optimize": "ReleaseFast",
    "strip": true
  }
}
```

**Compilation Modes:**
- **Debug**: Fast compilation, debug symbols, no optimization
- **ReleaseSafe**: Optimized with safety checks
- **ReleaseFast**: Maximum performance, minimal checks
- **ReleaseSmall**: Optimize for binary size

## Standard Library

### Module Organization

```
stdlib/
├── core/
│   ├── primitives.mts    # number, string, boolean
│   ├── option.mts        # Option<T>
│   └── result.mts        # Result<T, E>
├── collections/
│   ├── array.mts         # Array<T>
│   ├── map.mts           # Map<K, V>
│   ├── set.mts           # Set<T>
│   └── list.mts          # LinkedList<T>
├── macros/
│   ├── derive.mts        # @derive macros
│   ├── comptime.mts      # @comptime utilities
│   └── ffi.mts           # @bindC, @cImport
└── ffi/
    ├── libc.mts          # C standard library
    └── posix.mts         # POSIX bindings
```

### API Design Principles

1. **Zero-cost abstractions**: Compile away at zero runtime cost
2. **Explicit over implicit**: No hidden allocations or performance cliffs
3. **Type-safe**: Leverage type system for correctness
4. **Composable**: Traits and generics for reusability

## Performance Characteristics

### Compilation Time

Target times for typical projects:

| Project Size | Files | Lines | Compile Time (Debug) | Compile Time (Release) |
|-------------|-------|-------|---------------------|----------------------|
| Small | 10 | 1K | <1s | <5s |
| Medium | 100 | 10K | <5s | <30s |
| Large | 1000 | 100K | <30s | <5min |

### Runtime Performance

Target performance relative to C:

| Workload | Target | Notes |
|----------|--------|-------|
| Compute-bound | 90-95% | Tight loops, math |
| Mixed | 80-90% | Some allocation |
| Allocation-heavy | 70-85% | GC overhead |

### Memory Usage

| Aspect | Target |
|--------|--------|
| Heap overhead | <10% vs C |
| GC metadata | <5% of heap |
| Stack usage | Same as C |
| Binary size | <2MB for hello world |

## Development Phases

### Phase 1: Foundation (Year 1 Q1-Q2)

**Focus**: Basic compiler working

- [x] TypeScript parser (Zig)
- [x] Type checker (strict mode)
- [x] Simple code generation (Zig → C)
- [x] Basic stdlib (primitives, arrays)
- [x] "Hello World" compiles and runs

### Phase 2: Macros (Year 1 Q3-Q4)

**Focus**: Compile-time metaprogramming

- [ ] `@comptime` execution
- [ ] `@derive` macros (Eq, Hash, Clone, Debug)
- [ ] Macro expansion debugging
- [ ] Standard macro library

### Phase 3: Performance (Year 2 Q1-Q2)

**Focus**: Optimization and benchmarking

- [ ] LLVM backend
- [ ] GC implementation
- [ ] Monomorphization
- [ ] Benchmarks vs C/Rust/Go

### Phase 4: Tooling (Year 2 Q3-Q4)

**Focus**: Developer experience

- [ ] LSP server
- [ ] VS Code extension
- [ ] Debugger integration
- [ ] Source maps

### Phase 5: Ecosystem (Year 3)

**Focus**: Libraries and adoption

- [ ] HTTP/web frameworks
- [ ] Database drivers
- [ ] Cloud integrations
- [ ] Community packages

## Contributing to Architecture

### Adding a New Language Feature

1. Update parser (AST node types)
2. Add type checking rules
3. Implement code generation
4. Add tests
5. Update documentation

### Performance Optimization

1. Profile with `metascript profile`
2. Identify hotspots
3. Optimize IR generation or backend
4. Benchmark improvements
5. Document trade-offs

### See Also

- [Contributing Guide](./contributing.md)
- [Development Setup](./development-setup.md)
- [Compiler Internals](./compiler-internals.md) (detailed implementation guide)

---

**This architecture is a living document.** As we build and learn, it will evolve. Refer to this doc when making design decisions to ensure consistency with the overall vision.
