# Metaprogramming & Macro System

**Purpose:** Compile-time code generation with zero runtime cost

**Design:** Nim-inspired syntax, TypeScript-familiar usage

---

## Macro Types

Metascript has two types of macros:

### 1. Compiler Intrinsic Macros (`extern macro @name`)

**Declaration:** `extern macro @name(params): ReturnType;`
**Usage:** `@name(args)` - always with `@` prefix

These are built into the compiler - no Metascript body exists.

```typescript
// Declaration (in std/macros/compiler.ms)
extern macro @target(...platforms: string[]): void;
extern macro @emit(code: string): void;
extern macro @emit<T>(code: string): T;
extern macro @comptime<T>(block: Function): T;
extern macro @inline(): void;
extern macro @sizeof<T>(): usize;
extern macro @alignof<T>(): usize;
```

| Macro | Purpose | Example |
|-------|---------|---------|
| `@target` | Conditional compilation by backend | `@target("c") { ... }` |
| `@emit` | Emit raw backend code | `@emit("printf(...)")` |
| `@comptime` | Compile-time evaluation | `@comptime { ... }` |
| `@inline` | Force inlining hint | `@inline` |
| `@sizeof` | Get type size in bytes | `@sizeof<i32>()` |
| `@alignof` | Get type alignment | `@alignof<i64>()` |

### 2. User-Defined Macros (`macro function`)

**Declaration:** `macro function name(params) { ... }`
**Usage:** `name(args)` - called like normal functions (NO `@` prefix)

```typescript
// Declaration
macro function deriveEq(target) {
    const body = ast.createBlock([]);
    const method = ast.createMethod("equals", body);
    target.addMethod(method);
    return target;
}

// Usage - like a normal function call
deriveEq(User);
```

**Key Points:**
- User macros are called like regular functions
- Must be imported before use
- Decorator syntax (`@macroName` before class) is a **future consideration** - to be discussed following Haxe's approach

---

## Compiler Intrinsics

### @target - Conditional Compilation

```typescript
function printValue(msg: string): void {
    @target("c") {
        @emit("printf(\"%s\\n\", $msg);")
    } else @target("js") {
        console.log(msg);
    } else @target("erlang") {
        @emit("io:format(\"~s~n\", [$msg])")
    }
}
```

### @emit - Raw Backend Code

```typescript
@target("c") {
    @emit("printf($format, $args)")
}

// With return value
const now = @emit<number>("Date.now()");
```

### @comptime - Compile-Time Execution

```typescript
const version = @comptime {
    const gitHash = exec("git rev-parse --short HEAD");
    return gitHash;
};
// Result embedded as constant in binary
```

---

## User Macro Definition

### Basic Syntax

```typescript
// Define a macro - uses 'macro function' keyword
macro function myMacro(a: string, b: number) {
    // Compile-time code here
    // Can use @target, @emit inside
    @target("c") {
        @emit("some_c_code($a, $b)")
    }
    return result;
}

// Usage - called like a normal function
myMacro("hello", 42);
```

### Standard Library Macros

```typescript
// std/macros/derive.ms
macro function deriveEq(target) {
    console.log("deriveEq called for:", target.name);
    const body = ast.createBlock([]);
    const method = ast.createMethod("equals", body);
    target.addMethod(method);
    return target;
}

macro function deriveHash(target) {
    const body = ast.createBlock([]);
    const method = ast.createMethod("hash", body);
    target.addMethod(method);
    return target;
}
```

### Using Standard Macros

```typescript
import { deriveEq, deriveHash } from "std/macros";

// Call like normal functions
deriveEq(User);
deriveHash(User);

class User {
    name: string;
    age: number;
}
```

---

## The `quote` Block (Future)

For AST generation without manual `ast.createMethod()` calls:

```typescript
macro function addGetter(target, fieldName: string) {
    quote {
        get${capitalize(fieldName)}() {
            return this.${fieldName};
        }
    }
}
```

- Code inside `quote { }` becomes AST at compile-time
- `${expr}` interpolates values into the quoted AST

---

## Macro Architecture

```
std/macros/
├── compiler.ms   # Compiler intrinsic declarations (extern macro @name)
├── derive.ms     # User macros: deriveEq, deriveHash, etc.
├── serialize.ms  # User macros: serialize/deserialize (TODO)
└── ffi.ms        # User macros: FFI helpers (TODO)
```

**Execution Flow:**
```
deriveEq(User)
      │
      ↓
┌──────────────────┐    ┌──────────────────┐    ┌──────────────────┐
│ Find deriveEq    │ ─▶ │ Execute via      │ ─▶ │ Return modified  │
│ in std/macros/   │    │ Hermes VM        │    │ AST              │
└──────────────────┘    └──────────────────┘    └──────────────────┘
```

---

## Syntax Summary

| Type | Declaration | Usage |
|------|-------------|-------|
| Compiler intrinsic | `extern macro @name(params): Type;` | `@name(args)` |
| User macro | `macro function name(params) { }` | `name(args)` |

**Visual distinction:**
- `@` prefix = compiler magic (always available, no import needed)
- No `@` prefix = user abstraction (requires import, like normal functions)

---

## Future: Decorator Syntax

Decorator-style usage is under consideration for future versions:

```typescript
// FUTURE - not yet implemented
@derive(Eq, Hash)
class User {
    name: string;
}
```

This will follow Haxe's approach with 2-3 different macro invocation styles. Design discussion pending.

---

## Performance

| Metric | Target |
|--------|--------|
| Expansion time | <10% of total build |
| Cache hit rate | >99% |

```bash
# Profile macro overhead
msc compile --profile user.ms

# View expanded code
msc expand --macro=derive user.ms
```

---

## File Locations

```
src/macro/
  vm_expander.zig     # Main expansion engine (Hermes VM)
  source_registry.zig # Source macro registry
  cache.zig           # Expansion caching

std/macros/
  compiler.ms         # extern macro declarations
  derive.ms           # deriveEq, deriveHash, etc.
```

---

## References

- **Macro system design:** `../macro-system.md`
- **Source-defined macros:** `../source-defined-macros.md`
- **Trans-Am caching:** `./trans-am.md`
- **LSP macro support:** `./lsp.md`
