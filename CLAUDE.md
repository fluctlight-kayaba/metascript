# Metascript

A systems programming language with TypeScript syntax and compile-time metaprogramming, achieving 85-95% of C performance.

## Core Vision

**The Magic Formula:** TypeScript syntax + Compile-time macros + Native performance

Metascript bridges familiar developer experience with systems-level performance. Write code that looks like TypeScript, compile to native binaries via Zig/LLVM with performance approaching hand-written C.

**What We're Building:**
- TypeScript syntax (familiar, copy-paste friendly)
- Compile-time macro system (bridge dynamic patterns to static code)
- Native compilation (Zig → C/LLVM)
- Strict static typing (no `any`)
- 90%+ of C performance

**Why This Matters:**
JS/TS developers need native performance without learning Rust/C++. Existing solutions sacrifice either performance (Node.js, Bun), require new syntax (Rust, Go), or lack metaprogramming (most compiled languages). Metascript gives TS developers a path to native performance while keeping syntax familiarity and adding compile-time superpowers.

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

**See:** [Macro System](./docs/macro-system.md) for details

### 3. Performance

**Goal:** 90%+ of C on compute-bound code, 80%+ on allocation-heavy workloads

**Architecture:**
```
TypeScript → AST → Macro expansion → Type checking →
Monomorphization → Zig IR → C/LLVM → Native binary
```

**Key Techniques:**
- Static types → concrete native types (no boxing)
- Generics specialized at compile-time (monomorphization)
- Stack allocation preferred, generational GC for heap
- Struct-based object model (no hash table properties)
- Zero-cost abstractions via macros

**Targets:**
- Compute-bound: 90%+ of C
- Mixed workloads: 80%+ of C
- Lambda cold start: <50ms (10x faster than Node.js)
- Memory: Within 20% of equivalent C

**See:** [Performance Guide](./docs/performance-guide.md), [Design References](./docs/design-references.md)

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

# Compile Metascript file
metascript compile main.mts

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

# Generate C code (debugging)
metascript emit-c main.mts
```

### Project Structure

```
metascript/
├── compiler/          # Compiler (Zig)
│   ├── parser/       # TypeScript parser
│   ├── checker/      # Type checker
│   ├── macro/        # Macro expander
│   ├── codegen/      # Code generation
│   └── backend/      # LLVM/C backend
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

**Haxe (Performance):**
- Validated 85-95% of C performance achievable
- Generational GC for native targets
- Struct-based object model

**Nim (Macros):**
- Powerful hygienic macro system
- Compile-time code execution
- Zero-cost abstractions

**TypeScript (Syntax/Tooling):**
- Familiar syntax drives adoption
- World-class LSP integration
- Progressive enhancement

**Deno (Modern Tooling):**
- Zero-config experience
- Integrated development workflow
- Developer-friendly defaults

**See:** [Design References](./docs/design-references.md) for detailed analysis

## Current Roadmap

### Year 1: Prove the Concept (Q1-Q4 2025)

**Q1-Q2 Foundation:**
- Basic compiler (parser, type checker, codegen)
- Core stdlib
- Simple macros
- "Hello World" to native binary

**Q3-Q4 Lambda Focus:**
- Lambda runtime optimization
- Performance benchmarks
- Standard macros (`@derive`, `@serialize`)
- 3-5 production pilots

**Milestones:**
- 50% of strict TS compiles
- 80%+ of C performance
- <100ms Lambda cold starts
- 1k+ developers testing

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

**Not a JavaScript runtime:**
- No browser/Node.js compatibility
- Not targeting dynamic JS code

**Not TypeScript-compatible:**
- Not a drop-in `tsc` replacement
- Subset only (no `any`, no eval)
- Breaking changes for performance

**Not replacing high-level languages:**
- Not for web frontend
- Not competing with Node.js for I/O-heavy apps
- Not prioritizing npm compatibility

**Not a new syntax:**
- TypeScript syntax is non-negotiable
- Not inventing new paradigms

**Not a quick project:**
- 2-3 year timeline to production-ready
- Quality over speed

**See:** [Philosophy](./docs/philosophy.md) for complete non-goals

## Documentation

**Getting Started:**
- [Quickstart Guide](./docs/quickstart.md) - 30-minute intro
- [Migration from TypeScript](./docs/migration-from-typescript.md)

**Reference:**
- [Macro System](./docs/macro-system.md) - Compile-time metaprogramming
- [Architecture](./docs/architecture.md) - Compiler internals
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

**This is our north star.** Every decision—technical, strategic, or tactical—should advance the Four Pillars while staying true to the core vision: TypeScript syntax, compile-time macros, native performance.
