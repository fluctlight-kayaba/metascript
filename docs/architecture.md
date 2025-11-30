# Compiler Architecture

**Goal:** One language, three strategic backends (C, JavaScript, Erlang) via unified IR

---

## Compilation Pipeline

```
Source (.ms) → Parser → AST → Macro Expander → Expanded AST → Type Checker →
Typed AST → Monomorphization → Specialized AST → Unified IR → Backend Selection
```

**Backends:**
- **C**: Native perf (90%+ of C), <50ms Lambda cold start
- **JavaScript**: Browser + npm ecosystem
- **Erlang**: OTP fault tolerance, distributed systems

---

## Core Phases

### 1. Parser (Zig)
- TypeScript-compatible syntax
- Incremental parsing for LSP
- Error recovery for multiple issues

### 2. Macro Expander
- Execute `@comptime` blocks
- Run `@derive` macros
- Sandboxed Zig environment

**Example:**
```typescript
@derive(Eq) class User { name: string; }
// → class User { name: string; equals(other) { return this.name === other.name; } }
```

### 3. Type Checker
- Nominal typing for classes
- Structural typing for interfaces
- No `any`, strict null checking

### 4. Monomorphization
Generics → concrete types:
```typescript
identity<T>(x: T)
// → identity_number(x: number) + identity_string(x: string)
```

### 5. Unified IR
Platform-agnostic intermediate representation mapping cleanly to all 3 backends:

```zig
// Interface → IR Struct
interface Point { x: number; y: number; }
// → IR.Struct { name: "Point", fields: [F64, F64], layout: .stack }

// Class → IR Struct + VTable
class Animal { speak(): void; }
// → IR.Class { vtable: [speak], layout: .heap }
```

---

## Backend Code Generation

### C Backend (Native)

**Target:** 90-95% of hand-written C

```c
// Interface → struct
typedef struct { double x, y; } Point;

// Class → struct + vtable
typedef struct Animal_VTable { void (*speak)(struct Animal*); } Animal_VTable;
typedef struct Animal { Animal_VTable* vtable; } Animal;
```

**Performance:**
- Binary: ~500KB-2MB
- Cold start: <50ms
- Compilation: 2-5s (medium projects)

**Reference:** `~/projects/nim`, `~/projects/haxe`

### JavaScript Backend (Ecosystem)

**Target:** Browser + npm compatible

```javascript
// Macro-generated code compiled away
class Point { constructor(x, y) { this.x = x; this.y = y; } }
Point.prototype.equals = function(other) {
    return this.x === other.x && this.y === other.y;
};

// @comptime → embedded constant
const CONFIG = { apiUrl: "https://api.prod.com", debug: false };
```

**Features:** Source maps, tree-shaking, ESM/CommonJS

**Reference:** `~/projects/bun`

### Erlang Backend (Fault Tolerance)

**Target:** OTP + distributed systems

```erlang
-record(point, {x :: float(), y :: float()}).

-module(animal).
-export([new/0, speak/1]).
-behaviour(gen_server).

new() -> #{vtable => #{speak => fun speak_impl/1}}.
speak(#{vtable := #{speak := SpeakFn}} = Self) -> SpeakFn(Self).
```

**Features:** OTP supervision, hot code reload, immutable data

**Reference:** `~/projects/elixir`, `~/projects/gleam`

---

## Backend Comparison

| Aspect | C | JavaScript | Erlang |
|--------|---|------------|--------|
| Performance | 90-95% of C | V8/JIT | Concurrency-optimized |
| Cold Start | <50ms | ~200ms | ~100ms |
| Binary Size | ~500KB-2MB | ~50KB-500KB | ~1-5MB |
| Best For | Performance, Lambda | Browser, npm | Distributed, fault-tolerant |
| Ecosystem | C libraries (FFI) | npm (100M+) | Hex.pm (OTP) |

---

## Backend Selection

```bash
msc compile --target=c main.ms      # Native binary
msc compile --target=js main.ms     # JavaScript
msc compile --target=erlang main.ms # BEAM bytecode
```

**Auto-detection:**
```typescript
import { fetch } from "@metascript/web";     // → JavaScript
import { ffi } from "@metascript/ffi";       // → C
import { GenServer } from "@metascript/otp"; // → Erlang
```

---

## Memory Management

### C Backend
- **GC (default):** Generational (young/old gen), bump-pointer allocation
- **ARC mode:** Reference counting (optional via `@memory(strategy: "arc")`)

### JavaScript Backend
Managed by V8/SpiderMonkey (generational GC)

### Erlang Backend
Per-process heaps, isolated GC, immutable data

**Stack Allocation:**
```typescript
interface Point { x: number; y: number; }
function distance(a: Point, b: Point) { /* a, b stack-allocated */ }
```

---

## Performance Targets

### Compilation Time

| Project Size | Files | Lines | Debug | Release |
|--------------|-------|-------|-------|---------|
| Small | 10 | 1K | <1s | <5s |
| Medium | 100 | 10K | <5s | <30s |
| Large | 1000 | 100K | <30s | <5min |

### Runtime Performance

| Workload | Target | Notes |
|----------|--------|-------|
| Compute-bound | 90-95% of C | Tight loops |
| Mixed | 80-90% | Some allocation |
| Allocation-heavy | 70-85% | GC overhead |

### Memory

| Aspect | Target |
|--------|--------|
| Heap overhead | <10% vs C |
| GC metadata | <5% of heap |
| Binary size | <2MB hello world |

---

## LSP Architecture

```
Editor → JSON-RPC → Request Handler → Incremental Compiler
         ← Response ←                 (Parse + Type Check + Cache)
```

**Performance:** <200ms autocomplete/hover

**Optimizations:**
- Incremental parsing (changed files only)
- Lazy type checking (visible files first)
- AST caching
- Parallel processing

---

## Standard Library

```
stdlib/
├── core/         # primitives, Option<T>, Result<T,E>
├── collections/  # Array, Map, Set, LinkedList
├── macros/       # @derive, @comptime, @bindC
└── ffi/          # libc, posix
```

**Principles:**
- Zero-cost abstractions
- Explicit (no hidden allocations)
- Type-safe
- Composable (traits + generics)

---

## Development Roadmap

**Weeks 1-4:** Foundation + Unified IR
- [x] Parser, type checker, IR design
- [x] Basic stdlib
- [x] "Hello World" compiles

**Weeks 5-12:** Three Backends (Parallel, 3-5 person team)
- [ ] C backend (2 engineers): IR→C, GC runtime
- [ ] JS backend (1-2 engineers): IR→JS, source maps, npm
- [ ] Erlang backend (1 engineer): IR→Erlang, OTP basics
- [ ] Cross-backend test suite

**Weeks 13-24:** Macros + Validation
- [ ] `@comptime`, `@derive` (all backends)
- [ ] Same semantics across backends
- [ ] Performance benchmarks
- [ ] 3-5 pilot projects

**Year 2:** Tooling (LSP, debugger, VS Code)
**Year 3:** Production adoption, self-hosting

---

## References

- Parser: Study `~/projects/bun` (TS→AST in Zig)
- C Backend: `~/projects/nim`, `~/projects/haxe`
- Erlang Backend: `~/projects/elixir`, `~/projects/gleam`
- LSP: See `lsp-architecture.md`
