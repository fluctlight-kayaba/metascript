# Metascript

A universal systems language with TypeScript syntax, compile-time metaprogramming, and three strategic backends: C (native performance), JavaScript (browser/npm ecosystem), and Erlang (fault-tolerant distributed systems).

## Core Vision

**The Magic Formula:** TypeScript syntax + Compile-time macros + Three strategic backends

Metascript is a universal systems language: write once in TypeScript syntax, deploy to three strategic runtimes. Get native performance (C), browser/npm reach (JavaScript), or fault-tolerant distribution (Erlang) from the same codebase.

**What We're Building:**
- TypeScript syntax (familiar, copy-paste friendly)
- Compile-time macro system (bridge dynamic patterns to static code)
- **Three backends:** C (90%+ of C perf), JavaScript (browser/npm), Erlang (OTP runtime)
- Unified IR (one language, three runtimes)
- Strict static typing (no `any`)

**Why This Matters:**
Developers face a false choice: fast development (TypeScript/Node.js) OR native performance (C/Rust) OR fault tolerance (Erlang/Elixir). Metascript eliminates this trade-off with one language targeting three strategic runtimes. Write TypeScript-like code once, deploy for performance (C backend), reach (JavaScript backend), or reliability (Erlang backend).

## The Four Pillars

### 1. Developer Experience (DX)

**Goal:** TS developers adopt Metascript in <30 min with minimal syntax changes

**Key Points:**
- 70% of strict TypeScript code compiles unchanged
- 90% of strict codebases work with <10% modifications
- Quality error messages pointing to user code (not generated)
- LSP responsiveness <200ms
- Progressive adoption path

**See:** [Philosophy](./docs/philosophy.md) for design principles

### 2. Compile-Time Macros & Metaprogramming

**Goal:** Macros bridge dynamic TS patterns to static native code transparently

**Core Capabilities:**
```typescript
// Dynamic property access → static dispatch
const value = obj[key];  // Macro generates type-safe switch

// Boilerplate elimination
@derive(Eq, Hash, Serialize)
class User { name: string; }  // Auto-generates 500+ lines

// FFI bindings
const libc = @comptime bindC('./libc.h');

// DSL support
const route = @route `GET /users/:id => handler`;
```

**Implementation:**
- `@comptime` functions execute during compilation
- AST manipulation API (inspect types, generate code)
- Standard macro library (`@derive`, `@serialize`, `@ffi`, `@route`)
- Hygienic (no variable capture)

**Critical Insight - Macros Are Additive, Not Breaking:**
Metascript's metaprogramming is **perfectly compatible with JavaScript AST** - macros add compile-time superpowers, not runtime overhead:

```typescript
// Metascript with macros
@derive(Eq, Hash)
class User { name: string; }

// Compiles to clean JavaScript (all backends)
// - C backend: Optimized structs
// - JavaScript backend: Clean ES2020 classes (zero macro runtime!)
// - Erlang backend: Erlang records
```

**JavaScript Backend Gets Full Macro Power:**
- Macros expand before JavaScript emission → standard JS output
- Compatible everywhere: Browser, Node.js, Deno, Bun, Hermes
- **Additive optimization**: JavaScript + compile-time manipulation = more powerful JavaScript
- Zero breaking changes - works with existing npm ecosystem

**See:** [Macro System](./docs/macro-system.md) for details

### 3. Multi-Backend Architecture

**Goal:** One language, three strategic runtimes - choose the right backend for each use case

**Architecture:**
```
TypeScript → AST → Macro expansion → Type checking →
Unified IR → Backend Selection
    ├─ C backend → GCC/Clang → Native binary (90%+ of C)
    ├─ JavaScript backend → Modern JS (browser/npm compatible)
    └─ Erlang backend → BEAM bytecode (OTP fault tolerance)
```

**Backend Capabilities:**
- **C backend:** 90%+ of C performance, <50ms Lambda cold starts, native binaries
- **JavaScript backend:** Browser/Node.js compatibility, npm ecosystem access
- **Erlang backend:** OTP supervision, distributed systems, hot code reloading

**Key Techniques:**
- Unified IR validates abstraction across all backends
- Backend-specific optimizations (SIMD for C, tree-shaking for JS, process pooling for Erlang)
- Macros generate IR (backend-agnostic), backends translate idiomatically
- Cross-backend code sharing via common type system

**See:** [Architecture](./docs/architecture.md), [Backends Guide](./docs/backends.md)

### 4. Market Adoption

**Goal:** 50k-200k developers by Year 3

**Target Markets:**
1. Lambda/Edge Computing (Primary) - 100k-500k developers
2. CLI & DevOps Tools (Secondary) - 500k-1M developers
3. Performance-Critical Services (Tertiary) - 50k-100k developers

**Adoption Timeline:**
- Year 1: Lambda/Edge focus, prove performance claims
- Year 2: CLI tooling, expand ecosystem
- Year 3: General systems programming, corporate adoption

**See:** [Market Strategy](./docs/market-strategy.md), [Roadmap](./docs/roadmap.md)

## Quick Reference

### Development Commands

```bash
# Build compiler
zig build

# Compile Metascript file (default: C backend)
metascript compile main.mts

# Compile to specific backend
metascript compile --target=c main.mts          # Native binary
metascript compile --target=js main.mts         # JavaScript
metascript compile --target=erlang main.mts     # Erlang/BEAM

# Run with JIT (development)
metascript run main.mts

# Type check only
metascript check main.mts

# LSP server
metascript lsp

# Macro expansion preview
metascript expand --macro=derive main.mts

# Performance profiling
metascript profile main.mts

# Emit intermediate representations
metascript emit-ir main.mts                     # Unified IR
metascript emit-c main.mts                      # C code
metascript emit-js main.mts                     # JavaScript code
metascript emit-erl main.mts                    # Erlang code
```

### Project Structure

```
metascript/
├── compiler/          # Compiler (Zig)
│   ├── parser/       # TypeScript parser
│   ├── checker/      # Type checker
│   ├── macro/        # Macro expander
│   ├── ir/           # Unified IR
│   └── backends/     # C, JavaScript, Erlang backends
├── runtime/          # Minimal runtime
│   ├── gc/          # Garbage collector
│   ├── allocator/   # Memory allocator
│   └── panic/       # Error handling
├── stdlib/           # Standard library
│   ├── core/        # Core types
│   ├── collections/ # Data structures
│   ├── macros/      # Standard macros
│   └── ffi/         # C bindings
├── lsp/              # Language server
├── tools/            # CLI tools
├── tests/            # Test suite
├── benchmarks/       # Performance tests
└── docs/             # Documentation
```

## Key Design Decisions

### Type System

**Static-first:**
- All types statically resolvable at compile-time
- No `any` (use explicit `unknown` with guards)
- No implicit coercion
- Generics monomorphized

**Object Model:**
- Interfaces → structs (fixed layout)
- No dynamic property addition
- Classes use nominal typing
- Methods use vtables or devirtualization

**Memory Model:**
- Value types: stack allocated
- Reference types: heap with GC
- Optional ARC mode for predictable deallocation
- Explicit `@stack` and `@heap` annotations

### Macro System

**Principles:**
- Bridge dynamic → static transparently
- Type-driven code generation
- Hygienic (no variable capture)
- Clear compile-time/runtime boundary
- Fail-fast with actionable errors

**Execution:**
- Macros run in sandboxed environment
- Access to type info and AST
- Pure functions (no side effects)
- Deterministic output

### Compilation Strategy

**Optimize for:**
- Fast iteration (incremental compilation)
- Clear errors (point to source, not generated code)
- Debuggability (source maps, DWARF symbols)
- Performance (LLVM optimization)

**Trade-offs:**
- Favor runtime over compile-time performance
- Favor performance over binary size
- Favor optimization over compatibility

## Technical Stack

**Compiler:** Zig (self-hosting after bootstrap)
- C interop for LLVM
- Fast compilation
- Memory safety without GC overhead
- Cross-compilation built-in

**Runtime:**
- Minimal runtime (no VM/interpreter)
- Generational GC (optional ARC mode)
- Stack allocation preferred
- RTTI only where requested

**Tooling:**
- LSP server (real-time type checking, macro preview)
- Zig build system integration
- Incremental compilation
- Cross-compilation support

## Proven Reference Models

We synthesize proven approaches rather than inventing new paradigms:

**Haxe (Multi-Backend Pioneer):**
- Validates multi-backend compilation works (6+ backends)
- 85-95% of C performance on C++ backend
- Proven IR abstraction across very different targets
- Multi-backend development is tractable

**Nim (Macros + C Backend):**
- Powerful hygienic macro system with AST manipulation
- C backend achieving 90-95% of C performance
- Compile-time code execution validates our approach
- Zero-cost abstractions via compile-time expansion

**Elixir (Erlang Backend with Modern DX):**
- Proves modern syntax + BEAM runtime works
- TypeScript-like developer experience on OTP
- Fault-tolerant distributed systems accessible
- Validation that familiar syntax drives OTP adoption

**TypeScript (Syntax/Tooling Excellence):**
- Familiar syntax drives massive adoption
- World-class LSP integration sets the bar
- Progressive enhancement philosophy
- Millions of developers already know the syntax

**Local Reference Implementations:**

We have production-grade source code to study locally:

**For C Backend + Metaprogramming:**
- `~/projects/nim` - Nim compiler (C backend, powerful macros, AST manipulation)
- `~/projects/haxe` - Haxe compiler (C++ backend, multi-backend IR, proven at scale)

**For Erlang Backend:**
- `~/projects/elixir` - Elixir compiler (Erlang backend, metaprogramming, modern DX)
- `~/projects/gleam` - Gleam compiler (Erlang backend, type-safe functional)

**For JavaScript Backend:**
- `~/projects/bun` - Bun runtime (Zig implementation, production TypeScript transpilation)
- `~/projects/hermes` - Hermes JS engine (Meta's AOT bytecode compiler, React Native optimized)

**When stuck, study these codebases first** - they've solved the exact problems we're tackling.

**See:** [Design References](./docs/design-references.md) for detailed analysis

## Current Roadmap

### Year 1: Prove Multi-Backend Concept (Q1-Q4 2025)

**Weeks 1-4: Unified IR Design**
- Parser (TypeScript subset)
- Type checker (strict static)
- **Unified IR** (design for all 3 backends)
- Macro system foundation

**Weeks 5-12: All 3 Backends in Parallel**
- C backend (native performance)
- JavaScript backend (browser/npm)
- Erlang backend (OTP runtime)
- "Hello World" compiles to all 3

**Weeks 13-24: Validation & Optimization**
- Performance benchmarks (all backends)
- Standard macros working across backends
- Lambda/Edge deployment (C backend)
- Browser demo (JS backend)
- Distributed demo (Erlang backend)

**Milestones:**
- Same code compiles to all 3 backends
- C backend: 80%+ of C performance
- JS backend: Works in browser/Node.js
- Erlang backend: OTP supervision working

### Year 2: Build Ecosystem (2026)

**Q1-Q2 Tooling:**
- Full LSP integration
- Source maps & debugging
- Documentation & tutorials

**Q3-Q4 Ecosystem:**
- CLI framework
- HTTP/web libraries
- Database drivers
- 10+ community packages

**Milestones:**
- 70% of strict TS compiles unchanged
- 10k+ developers
- 50+ production deployments

### Year 3: Mainstream Adoption (2027)

**Q1-Q2 Maturity:**
- Advanced macros
- Optimization passes
- Self-hosting compiler
- Security audits

**Q3-Q4 Scale:**
- Conference presence
- Corporate sponsorship
- Training materials
- Multi-platform support

**Milestones:**
- 100k-200k developers
- 500+ production apps
- Sustainable funding

**See:** [Roadmap](./docs/roadmap.md), [Project Planning](./docs/project-planning.md) for details

## What We're NOT Building

**Not TypeScript-compatible:**
- Not a drop-in `tsc` replacement
- Subset only (no `any`, no eval, restricted dynamics)
- Breaking changes for multi-backend support

**Not targeting all platforms:**
- C backend: Lambda, CLI, servers (not embedded/IoT)
- JS backend: Modern browsers/Node.js (not IE11)
- Erlang backend: Distributed systems (not single-machine apps)

**Not a general-purpose transpiler:**
- Not competing with esbuild/SWC for pure JS→JS
- Multi-backend requires IR abstraction
- Compile-time macros change architecture

**Not a new syntax:**
- TypeScript syntax is non-negotiable
- Not inventing new paradigms

**Not a quick project:**
- 2-3 year timeline to production-ready
- Quality over speed
- All 3 backends from day 1 = longer initial development

**See:** [Philosophy](./docs/philosophy.md) for complete non-goals

## Documentation

**Writing Style:** Documentation in `docs/` must be laser-focused on facts and critical design decisions only. Strip verbose explanations, redundant examples, and implementation details that don't directly support the core message. Prefer tables, code snippets, and bullet points over prose. Target: Dense, scannable documentation that respects reader's time.

**Getting Started:**
- [Quickstart Guide](./docs/quickstart.md) - 30-minute intro
- [Migration from TypeScript](./docs/migration-from-typescript.md)

**Reference:**
- [Macro System](./docs/macro-system.md) - Compile-time metaprogramming
- [Architecture](./docs/architecture.md) - Compiler internals & unified IR
- [Backends Guide](./docs/backends.md) - C, JavaScript, Erlang backends
- [Performance Guide](./docs/performance-guide.md) - Optimization techniques

**Planning:**
- [Market Strategy](./docs/market-strategy.md) - Target markets, personas, GTM
- [Roadmap](./docs/roadmap.md) - Development timeline
- [Project Planning](./docs/project-planning.md) - Critical success factors
- [Philosophy](./docs/philosophy.md) - Design principles
- [Design References](./docs/design-references.md) - Proven reference models

**Contributing:**
- [Contributing Guide](./docs/contributing.md)
- [Development Setup](./docs/development-setup.md)

**See:** [Documentation Index](./docs/README.md) for complete list

---

**This is our north star.** Every decision—technical, strategic, or tactical—should advance the Four Pillars while staying true to the core vision: TypeScript syntax, compile-time macros, three strategic backends (C/JavaScript/Erlang). One language, three runtimes—choose the right tool for each job.
