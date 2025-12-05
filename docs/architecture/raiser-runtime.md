# Raiser Runtime

Metascript's typed VM for scripting use cases (game engines, plugins, hot-reload).

## Why Not V8/Hermes?

| Engine | Limitation |
|--------|------------|
| V8 | JIT warmup, high memory, deopt cliffs |
| Hermes | Ignores types, still dynamic dispatch |
| JSCore | Same as V8 |

**Core problem**: They discard type information and rediscover it at runtime.

## Raiser Advantage

Types known at compile time → specialized bytecode → no runtime checks.

```
V8:      ADD (check types, maybe string concat, maybe number add...)
Raiser:  ADD_I32 (single CPU instruction, done)
```

## Performance Estimate

| Scenario | vs V8 Interpreter | vs V8 JIT (hot) |
|----------|-------------------|-----------------|
| Cold start | 5-10x faster | 5-10x faster |
| Warm code | 2-3x faster | ~equal |
| Hot loops | 2-3x faster | ~equal or slight edge |
| After deopt | 3x faster | 3x faster (no deopts) |

**Real-world (game scripting): 2-4x faster** - code is warm, not hot.

## Architecture

```
Source → Lexer → Parser → Type Checker → Typed AST
                                              ↓
                                    Bytecode Compiler (uses types!)
                                              ↓
                                    Typed Bytecode
                                              ↓
                                    Raiser VM (no type checks)
```

## Typed Bytecode

```zig
const Opcode = enum(u8) {
    // Typed arithmetic (no dispatch)
    ADD_I32, ADD_I64, ADD_F32, ADD_F64,

    // Direct struct access (offset known at compile time)
    GET_FIELD,  // operand: byte offset

    // Typed arrays (bounds check only)
    ARRAY_GET_I32, ARRAY_GET_F32,

    // Control flow
    CALL, RETURN, JUMP, JUMP_IF_FALSE,

    // ORC integration
    RC_INC, RC_DEC,
};
```

## Why Faster Than JIT?

| V8 JIT | Raiser |
|--------|--------|
| Speculate → Guard → Deopt risk | Know → Emit → Done |
| Hidden class checks | Direct offset access |
| Polymorphic inline caches | Monomorphic calls |
| Type guards in generated code | No guards needed |

## Implementation Effort

| Component | LOC | Time |
|-----------|-----|------|
| Bytecode format | ~300 | 1 week |
| Bytecode compiler | ~2000 | 3 weeks |
| VM interpreter | ~1500 | 3 weeks |
| ORC integration | ~500 | 2 weeks |
| **Total** | **~6K** | **2-3 months** |

## References

- **wren**: Clean embeddable VM (~8K LOC)
- **Lua 5.4**: Register-based, proven design
- **Crafting Interpreters**: Step-by-step guide
- **LuaJIT**: For future JIT implementation

## Key Insight

> V8's interpreter is slow because JavaScript is untyped.
> Raiser is fast because Metascript is typed.
>
> JIT recovers information you threw away.
> If you never throw it away, you start ahead.

---

## Stealing Lua's Magic

### Why Lua Is Fast

| Factor | Effect |
|--------|--------|
| Tiny VM (~20KB) | Fits in L1 instruction cache |
| Few opcodes (~47) | Tight dispatch loop |
| Simple semantics | No prototype chains, no hidden classes |
| NaN-boxing | Values fit in 64-bit, minimal allocation |

**Lua's secret: simplicity at runtime.**

### The Insight

```
Lua:        Simple syntax  → Simple VM → Fast
Raiser:     Rich syntax    → AST transforms → Simple IR → Tiny VM → Faster
```

**Normalize everything at compile time, run a tiny VM.**

### AST Canonicalization

Transform rich syntax to minimal canonical patterns:

```
Source (expressive)              Canonical IR (minimal)
────────────────────────────────────────────────────────
for (x in items) { }       →    LOOP + INDEX + JUMP
x?.foo                     →    LOAD + BRANCH_NULL + LOAD_FIELD
x ?? default               →    LOAD + BRANCH_NULL + LOAD
match expr { }             →    SWITCH or IF chain
class Foo { }              →    STRUCT layout + VTABLE
a + b (where a: i32)       →    ADD_I32 (resolved statically)
```

### Tiny Canonical IR (~25 ops)

```zig
const Op = enum(u8) {
    // Memory (5)
    LOAD, STORE, LOAD_FIELD, STORE_FIELD, ALLOC,

    // Arithmetic (8)
    ADD, SUB, MUL, DIV, MOD, NEG, AND, OR,

    // Comparison (3)
    EQ, LT, LE,

    // Control (5)
    JUMP, BRANCH, CALL, RETURN, SWITCH,

    // Memory management (3)
    RC_INC, RC_DEC, FREE,

    // Special (2)
    NOP, HALT,
};
// ~26 opcodes. Lua has ~47. Even simpler.
```

### Size Target

| VM | Core Size | L1 Cache? |
|----|-----------|-----------|
| V8 | ~MB | No |
| Lua 5.4 | ~20KB | Yes |
| LuaJIT | ~80KB | Mostly |
| **Raiser** | **~10-15KB** | **Yes** |

### Why Raiser Beats Lua

| Aspect | Lua | Raiser |
|--------|-----|--------|
| VM size | ~20KB | ~10-15KB |
| Opcodes | ~47 | ~25 |
| Type checks | Runtime | None |
| Table lookup | Hash | Direct offset |
| Method call | Metatable | Direct call |
| Value boxing | NaN-boxing | Unboxed typed slots |

**Same cache benefits, fewer runtime operations.**

### The Core Loop

```zig
// Entire interpreter hot path - ~200 lines
pub fn run(vm: *VM) void {
    var pc = vm.pc;
    var sp = vm.sp;
    const code = vm.code;
    const stack = vm.stack;

    while (true) {
        switch (@as(Op, @enumFromInt(code[pc]))) {
            .LOAD => {
                stack[sp] = vm.locals[code[pc + 1]];
                sp += 1;
                pc += 2;
            },
            .ADD => {
                sp -= 1;
                stack[sp - 1] +%= stack[sp];
            },
            .BRANCH => {
                const offset: i16 = @bitCast(code[pc + 1 ..][0..2].*);
                sp -= 1;
                pc = if (stack[sp] == 0)
                    @intCast(@as(i32, @intCast(pc)) + offset)
                else
                    pc + 3;
            },
            // ~20 more cases, all trivial
        }
    }
}
```

### Projected Performance

```
                            Speed
C/Zig ──────────────────────┤ 100%
LuaJIT (hot traced) ────────┤ 85%
Raiser (tiny typed VM) ─────┤ 70-80%  ← Target
LuaJIT (cold) ──────────────┤ 40%
Lua 5.4 ────────────────────┤ 30%
V8 (cold) ──────────────────┤ 15%
```

**Match LuaJIT performance WITHOUT a JIT** - type information replaces tracing.

### Summary

```
Rich Metascript syntax
         │
         ▼
    AST + Types
         │
    ┌────┴────┐
    ▼         ▼
Desugar   Monomorphize
    │         │
    └────┬────┘
         ▼
  Canonical IR (~25 ops)
         │
         ▼
  Lua-sized VM + static types
         │
         ▼
  Faster than Lua (no type dispatch)
  Competitive with LuaJIT (no JIT needed)
```

---

## Secret Weapons (Research Findings)

Techniques stolen from the fastest VMs in existence.

### 1. Computed Goto (2x Faster Dispatch)

Switch dispatch has branch prediction overhead. Computed goto eliminates it:

```c
// GCC/Clang extension - direct threading
static void* dispatch[] = { &&op_LOAD, &&op_ADD, &&op_BRANCH, ... };

#define NEXT() goto *dispatch[*pc++]

op_LOAD:
    stack[sp++] = locals[*pc++];
    NEXT();

op_ADD:
    sp--;
    stack[sp-1] += stack[sp];
    NEXT();
```

**Effect**: ~2x faster than switch-case. LuaJIT uses this.

### 2. NaN-Boxing (Zero-Cost Value Tagging)

Pack type + value in 64 bits using IEEE 754 NaN space:

```
64-bit double:  [sign:1][exp:11][mantissa:52]
NaN range:      exp=0x7FF, mantissa!=0

Encoding:
  Double:     Raw IEEE 754 (no tagging needed)
  Pointer:    0x7FFC_xxxx_xxxx_xxxx (48-bit pointer in mantissa)
  Integer:    0x7FFD_xxxx_xxxx_xxxx (32-bit int in mantissa)
  Boolean:    0x7FFE_0000_0000_000x (0 or 1)
  Nil:        0x7FFF_0000_0000_0000
```

**Effect**: No heap allocation for primitives. Lua, SpiderMonkey, LuaJIT use this.

**Raiser advantage**: With static types, we can skip NaN-boxing entirely for typed slots!

### 3. Register-Based VM (20-50% Fewer Dispatches)

Stack VM:
```
LOAD a    ; push a
LOAD b    ; push b
ADD       ; pop 2, push result
STORE c   ; pop to c
// 4 dispatches
```

Register VM:
```
ADD r0, r1, r2   ; c = a + b
// 1 dispatch
```

**Effect**: 15-30% speedup. Lua 5.0+, LuaJIT, mruby use register VMs.

### 4. Inline Caching (10x Faster Method Dispatch)

Cache method lookups at call sites:

```c
// Monomorphic inline cache
struct CallSite {
    Type* cached_type;
    Function* cached_method;
};

// Fast path (common case)
if (obj->type == site->cached_type) {
    site->cached_method(obj);  // Direct call!
} else {
    slow_lookup_and_cache(site, obj);
}
```

**Effect**: 10x faster than hash lookup. V8, JSCore, all fast VMs use this.

**Raiser advantage**: Static types = no inline cache needed. Always direct call.

### 5. Super-Instructions (Fuse Common Patterns)

Combine frequent opcode sequences into single opcodes:

```
// Instead of:
LOAD_FIELD obj, "x"    // dispatch 1
LOAD_CONST 1           // dispatch 2
ADD_I32                // dispatch 3
STORE_FIELD obj, "x"   // dispatch 4

// Single super-instruction:
INC_FIELD_I32 obj, "x", 1   // dispatch 1
```

**Effect**: 30-50% fewer dispatches for common patterns.

### 6. Copy-and-Patch JIT (80% Perf, 20% Complexity)

New technique (2021): Generate machine code by copying templates and patching immediates:

```c
// Pre-compiled template (in .rodata)
uint8_t add_template[] = {
    0x8b, 0x45, 0xXX,  // mov eax, [rbp + PATCH1]
    0x03, 0x45, 0xXX,  // add eax, [rbp + PATCH2]
    0x89, 0x45, 0xXX,  // mov [rbp + PATCH3], eax
};

// JIT = memcpy + patch offsets
memcpy(code, add_template, sizeof(add_template));
code[2] = offset_a;
code[5] = offset_b;
code[8] = offset_c;
```

**Effect**: Microsecond compilation, 80-90% of optimizing JIT speed.

**Future Raiser JIT**: Copy-and-patch is perfect for typed bytecode.

### 7. Arena Allocation (Bulk Alloc/Free)

Allocate AST nodes, bytecode, temporaries in arenas:

```zig
const Arena = struct {
    buffer: []u8,
    offset: usize,

    fn alloc(self: *Arena, size: usize) *anyopaque {
        const ptr = self.buffer[self.offset..];
        self.offset += size;
        return ptr;
    }

    fn reset(self: *Arena) void {
        self.offset = 0;  // Free everything instantly
    }
};
```

**Effect**: No malloc/free overhead. Instant bulk deallocation.

### 8. Small String Optimization (SSO)

Embed short strings inline:

```zig
const String = union {
    small: struct {
        len: u8,
        data: [23]u8,  // 23 bytes inline (64-bit)
    },
    heap: struct {
        len: u64,
        ptr: [*]u8,
        cap: u64,
    },
};
// Strings ≤23 bytes: zero allocation
```

**Effect**: Most strings (~80%) avoid heap allocation entirely.

### 9. Deferred Reference Counting

Batch RC operations to reduce atomic overhead:

```zig
const DeferredRC = struct {
    decrefs: BoundedArray(*Object, 256),

    fn dec(self: *DeferredRC, obj: *Object) void {
        self.decrefs.append(obj);  // Don't decref yet
        if (self.decrefs.len == 256) self.flush();
    }

    fn flush(self: *DeferredRC) void {
        for (self.decrefs.items) |obj| {
            obj.rc -= 1;
            if (obj.rc == 0) free(obj);
        }
        self.decrefs.clear();
    }
};
```

**Effect**: Fewer atomic operations, better cache locality.

### 10. Branch Prediction Hints

Arrange code for predictable branches:

```c
// Tell compiler the common case
#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

if (likely(obj->type == TYPE_INT)) {
    // Fast path - predicted taken
    return obj->int_value + other->int_value;
} else {
    // Slow path - predicted not taken
    return slow_add(obj, other);
}
```

**Effect**: Better branch prediction = fewer pipeline stalls.

---

## VMs to Study

| VM | Highlights | LOC |
|----|------------|-----|
| **wren** | Clean, computed goto, single-pass compiler | ~4K |
| **Gravity** | C99, register VM, embeddable | ~10K |
| **mruby** | Register VM, arena allocators | ~30K |
| **HashLink** | Typed bytecode, PICs, JIT | ~20K |
| **LuaJIT** | Trace JIT, NaN-boxing, DynASM | ~60K |
| **Terra** | Lua + LLVM, near-C speed | ~15K |

### Key Papers

1. **"The Structure and Performance of Efficient Interpreters"** - Ertl & Gregg
2. **"Copy-and-Patch Compilation"** - Xu et al. (2021)
3. **"Trace-based JIT Compilation for Lazy Functional Languages"** - LuaJIT inspiration

---

## Raiser Implementation Priorities

```
Phase 1: Interpreter MVP
  ├── Register-based bytecode
  ├── Computed goto dispatch
  ├── Typed opcodes (no runtime checks)
  └── Arena allocation for compilation

Phase 2: Memory Optimization
  ├── NaN-boxing (optional, for dynamic escape hatch)
  ├── SSO for strings
  ├── Deferred RC
  └── Object pooling

Phase 3: JIT (Future)
  ├── Copy-and-patch baseline JIT
  ├── Type-specialized templates
  └── No deopt guards needed (types proven)
```

**Goal**: Interpreter that matches LuaJIT cold performance, with path to JIT that exceeds it.

---

## The Solid.js Philosophy: No Silver Bullet

Inspired by Ryan Carniato's approach building SolidJS - the fastest JavaScript UI framework.

### The Core Insight

> "It wasn't smart algorithms but understanding what was important and then just a bit of hard work."
> — Ryan Carniato

There's no magic trick. SolidJS became fastest through:
1. **Measure everything** - JS Framework Benchmark as ground truth
2. **Question assumptions** - "Is the Virtual DOM actually necessary?"
3. **Be scrappy** - Take wins where they count, leave other things on the table
4. **Iterate relentlessly** - Refactor, measure, improve, repeat

### The udomdiff Story

Ryan tells how one of the fastest DOM diff algorithms emerged:

> "[@webreflection](https://twitter.com/webreflection) was on twitter asking if anyone knew a faster DOM diffing algorithm after growing tired of tweaking academic algorithms and not making headway. I pointed him to @localvoid's algorithm... he was like 'it looks a bunch of optimizations for a particular benchmark.' To which I replied 'sure, but these are also all the most common ways people manipulate a list.' The next morning he had come back with his new library taking an almost too simple Set lookup combined with these techniques. And guess what - it was smaller and about the same performance."

**Lesson**: Optimize for real patterns, not theoretical elegance.

### Performance Breakthroughs Come From Simplification

SolidJS's biggest performance jump came from a refactor that **simplified** the code:

> "It turns out accessing the DOM less even when that access is reserved to safe properties has significant performance improvements. I'd been spinning my wheels trying to come up with the most optimal system but its complexity had stifled its performance."

**Lesson**: Complexity is the enemy of performance.

### The Solid.js Method

```
1. BENCHMARK
   └── Use established benchmarks (JS Framework Benchmark)
   └── Run locally, measure variance
   └── Compare against the best (not the average)

2. PROFILE
   └── Find actual bottlenecks
   └── Question every assumption
   └── "Is this operation necessary?"

3. SIMPLIFY
   └── Remove unnecessary abstractions
   └── Reduce DOM access
   └── Batch operations

4. MEASURE AGAIN
   └── Did it actually help?
   └── Watch for regressions
   └── Test real-world patterns, not just micro-benchmarks

5. REPEAT
   └── There's always more to find
   └── Community feedback drives discovery
   └── Stay scrappy
```

### Apply to Raiser

| Solid.js Win | Raiser Equivalent |
|--------------|-------------------|
| Fine-grained reactivity | Fine-grained type info |
| No Virtual DOM diffing | No runtime type checking |
| Compile-time JSX transform | Compile-time AST canonicalization |
| DOM Template Node cloning | Bytecode template instantiation |
| Batch DOM updates | Batch RC operations |

### The Mindset

```
❌ "I'll use this clever algorithm"
✅ "What's the actual bottleneck?"

❌ "Academic papers say this is optimal"
✅ "What do real programs actually do?"

❌ "This is architecturally elegant"
✅ "This is measurably faster"

❌ "One breakthrough will solve everything"
✅ "100 small wins compound"
```

### Raiser Performance Methodology

```
1. Establish baseline
   └── Measure against Lua, LuaJIT, wren
   └── Use real game scripting patterns

2. Profile ruthlessly
   └── CPU cache misses
   └── Branch mispredictions
   └── Memory allocations per frame

3. Apply targeted optimizations
   └── One technique at a time
   └── Measure before and after
   └── Keep only what helps

4. Stay scrappy
   └── No sacred cows
   └── Question every abstraction
   └── Simpler is usually faster
```

**Bottom line**: SolidJS proved that disciplined iteration beats clever architecture. Raiser will follow the same path - measure, simplify, optimize, repeat until it's just fucking fast.
