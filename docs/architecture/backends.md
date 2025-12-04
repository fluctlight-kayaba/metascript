# Code Generation Backends

**Purpose:** One language, three strategic runtimes

**Quality Standard:** Hand-written quality output, not transpiler artifacts

---

## Backend Overview

| Backend | Best For | Performance | Ecosystem |
|---------|----------|-------------|-----------|
| **C** | Lambda, CLI, performance | 90%+ of C | C libraries (FFI) |
| **JavaScript** | Browser, npm | V8/JIT | npm (2M+ packages) |
| **Erlang** | Distributed, fault-tolerant | Concurrency-optimized | Hex.pm (OTP) |

---

## Compilation Pipeline

```
TypeScript → AST → Macro Expansion → Type Checking →
Typed AST (IR) → Backend Selection
    ├─ C backend → GCC/Clang → Native binary
    ├─ JavaScript backend → Modern JS
    └─ Erlang backend → BEAM bytecode
```

**Key:** The Typed AST is our IR. No separate IR layer needed.

---

## C Backend

### Code Generation

```typescript
// Input
@derive(Eq, Hash)
class Point {
    x: number;
    y: number;
    distance(other: Point): number {
        return Math.sqrt((this.x - other.x) ** 2 + (this.y - other.y) ** 2);
    }
}
```

```c
// Output
typedef struct Point { double x, y; } Point;

double Point_distance(Point* self, Point* other) {
    double dx = self->x - other->x;
    double dy = self->y - other->y;
    return sqrt(dx * dx + dy * dy);
}

bool Point_equals(Point* self, Point* other) {
    return self->x == other->x && self->y == other->y;
}

uint64_t Point_hash(Point* self) {
    return hash_combine(hash_f64(self->x), hash_f64(self->y));
}
```

### Memory Management

- **DRC/ORC:** Reference counting with cycle detection
- **Runtime:** `orc.h` (720 LOC, header-only)
- **Overhead:** 6-8% baseline, optimizable to 0.5-2%

### Compilation

```bash
msc compile --target=c main.ms    # → main.c
zig cc main.c -o main              # → native binary
```

### Performance Targets

| Workload | Target |
|----------|--------|
| Compute-bound | 90-95% of C |
| Lambda cold start | <50ms |
| Binary size | ~500KB-2MB |

---

## JavaScript Backend

### Code Generation

```typescript
// Input
@derive(Eq)
class User {
    name: string;
    age: number;
}

const config = @comptime {
    return { apiUrl: "https://api.prod.com" };
};
```

```javascript
// Output
class User {
    constructor(name, age) {
        this.name = name;
        this.age = age;
    }
    equals(other) {
        return this.name === other.name && this.age === other.age;
    }
}

const config = { apiUrl: "https://api.prod.com" };
```

### Key Insight: Macros Are Additive

Macros expand **before** JavaScript emission:
- Zero macro runtime in output
- Standard JavaScript everywhere
- Compatible with all JS runtimes

### Features

- Source maps for debugging
- ESM (default), CommonJS, UMD
- Tree-shaking support
- npm compatibility

### Hermes Mode (Mobile)

```bash
msc compile --target=js --runtime=hermes main.ms
# → main.js + main.hbc (Hermes bytecode)
```

---

## Erlang Backend

### Code Generation

```typescript
// Input
import { GenServer } from "@metascript/otp";

class Counter extends GenServer {
    count: number = 0;
    increment(): void { this.count += 1; }
    getCount(): number { return this.count; }
}
```

```erlang
% Output
-module(counter).
-behaviour(gen_server).
-export([start_link/0, increment/1, get_count/1]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

increment(Pid) ->
    gen_server:cast(Pid, increment).

get_count(Pid) ->
    gen_server:call(Pid, get_count).

handle_cast(increment, #{count := Count} = State) ->
    {noreply, State#{count => Count + 1}}.

handle_call(get_count, _From, #{count := Count} = State) ->
    {reply, Count, State}.
```

### OTP Features

- Supervision trees
- Hot code reload
- Distributed computing
- Per-process heaps

### Memory Management

- No DRC needed (BEAM GC)
- Immutable data
- Isolated process heaps

---

## Cross-Backend Considerations

### Same Semantics

```typescript
// This code works identically on all backends
@derive(Eq, Hash)
class User { name: string; }

const u1 = new User("Alice");
const u2 = new User("Alice");
console.log(u1.equals(u2));  // true on all backends
```

### Backend-Specific Code

```typescript
function optimizedSort<T>(items: T[]): T[] {
    @if (target === "c") return nativeQSort(items);
    @if (target === "js") return items.sort();
    @if (target === "erlang") return lists.sort(items);
}
```

### Backend Selection

**Auto-detection by imports:**
```typescript
import { fetch } from "@metascript/web";     // → JavaScript
import { ffi } from "@metascript/ffi";       // → C
import { GenServer } from "@metascript/otp"; // → Erlang
```

**Manual override:**
```bash
msc compile --target=c main.ms
msc compile --target=js main.ms
msc compile --target=erlang main.ms
```

---

## Quality Standards

### Hand-Written Quality

The goal is output that looks **hand-written**, not transpiler garbage:

```c
// Good: Clean, idiomatic C
typedef struct Point { double x, y; } Point;

double Point_distance(Point* self, Point* other) {
    double dx = self->x - other->x;
    double dy = self->y - other->y;
    return sqrt(dx * dx + dy * dy);
}

// Bad: Transpiler artifacts
struct __ms_internal_Point_t {
    double __ms_field_x;
    double __ms_field_y;
};
```

### Testing Strategy

```zig
test "C backend generates clean struct" {
    const output = compileToC("class Point { x: number; y: number; }");
    try testing.expect(std.mem.indexOf(u8, output, "typedef struct Point") != null);
    try testing.expect(std.mem.indexOf(u8, output, "__ms_internal") == null);
}

test "same semantics across backends" {
    const source = "@derive(Eq) class User { name: string; }";

    const c_output = runCompiled(.c, source, "User.equals test");
    const js_output = runCompiled(.js, source, "User.equals test");
    const erl_output = runCompiled(.erlang, source, "User.equals test");

    try testing.expectEqual(c_output, js_output);
    try testing.expectEqual(js_output, erl_output);
}
```

---

## Performance Comparison

| Workload | C | JavaScript | Erlang |
|----------|---|------------|--------|
| Fibonacci(40) | 334ms | 682ms | 1200ms |
| Lambda cold start | <50ms | ~200ms | ~100ms |
| 1M processes | N/A | ~10k async | ~2GB |

**Winner depends on workload:**
- Compute → C
- Concurrency → Erlang
- Ecosystem → JavaScript

---

## File Location

```
src/codegen/
  c/
    cgen.zig        # C code generation
    types.zig       # Type mapping
    lifecycle.zig   # DRC hooks
    mangle.zig      # Name mangling

  js/
    jsgen.zig       # JavaScript code generation
    esm.zig         # ESM output
    sourcemap.zig   # Source maps

  erlang/
    erlgen.zig      # Erlang code generation
    otp.zig         # OTP patterns
```

---

## References

- **C backend details:** `../codegen-c.md`
- **Erlang backend details:** `../codegen-erlang.md`
- **Full backends guide:** `../backends.md`
- **DRC memory model:** `./drc-orc.md`
