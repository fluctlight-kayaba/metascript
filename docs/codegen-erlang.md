# Erlang Backend: Comprehensive Guide

**TypeScript syntax → Erlang runtime: Transformation strategy, implementation details, and current status**

---

## 1. Philosophy: Transformation Over Restriction

**Say "YES" to TypeScript, Transform to Erlang**

**DON'T** warn users they can't do something because of Erlang's immutability.
**DO** transform familiar TypeScript patterns into optimal functional code.

### The 7 Transformation Patterns

| Pattern | TypeScript | Erlang Output |
|---------|-----------|---------------|
| **1. Variable "Mutation"** | `x = x + 1` | `X$1 = X + 1` |
| **2. Object Spread** | `{...obj, age: 28}` | `Obj#{age := 28}` with `element/2` for unchanged fields |
| **3. Array Prepend** | `[0, ...arr]` | `[0 \| Arr]` (O(1) cons) |
| **4. Control Flow** | `x > 0 ? "pos" : "neg"` | `case true of true when X > 0 -> ... end` |
| **5. Loops** | `for (let i...) sum += i` | Tail-recursive helper function |
| **6. Method Calls** | `counter.increment()` | `counter = counter_increment(Counter)` |
| **7. Array Append** | `[...arr, 4]` | `Arr ++ [4]` with ⚠️ performance warning |

### What We DON'T Restrict

✅ Variable reassignment → shadowing
✅ Object updates → new tuples
✅ Array building → cons cells
✅ Loops → tail recursion

### What We MUST Error On

❌ Direct property mutation: `obj.field = x`
❌ Closure capturing mutated variable
❌ Array element mutation: `arr[0] = x`

---

## 2. Variable Shadowing Implementation

**Core Strategy:** Rename variables with `@N` suffixes on each reassignment.

### Transformation Example

**TypeScript:**
```typescript
let x = 1;
x = x + 1;
x = x * 2;
return x;
```

**Generated Erlang:**
```erlang
X = 1,
X@1 = X + 1,
X@2 = X@1 * 2,
X@2
```

### Implementation (Zig)

```zig
const ErlangEnv = struct {
    var_counters: std.StringHashMap(usize),  // Track generations

    fn nextVarName(self: *Self, name: []const u8) ![]const u8 {
        const count = self.var_counters.get(name) orelse 0;
        try self.var_counters.put(name, count + 1);

        return if (count == 0)
            try capitalizeVar(name)  // "X"
        else
            try std.fmt.allocPrint(self.allocator, "{s}@{d}",
                .{ try capitalizeVar(name), count });  // "X@1", "X@2"
    }
};
```

**Two hash maps needed (from Gleam):**
1. `erl_function_scope_vars`: Function-level counter (never decreases)
2. `current_scope_vars`: Current scope's active generation (resets on scope exit)

### Type Safety

TypeScript types must be preserved across generations:

```typescript
let x: number = 1;  // X: number
x = x + 1;          // X@1: number
x = "hello";        // ❌ Type error
```

**Type checker must:**
- Track type of each variable generation
- Ensure reassignments are type-compatible
- Allow type narrowing after guards

### Closure Semantics ⚠️ Breaking Change

**JavaScript (mutable ref):**
```javascript
let x = 1;
const f = () => x;
x = 2;
f();  // 2
```

**Erlang/Metascript (value capture):**
```erlang
X = 1,
F = fun() -> X end,  % Captures value 1
X@1 = 2,
F().                 % Returns 1
```

**Documentation note:** Closures capture values at capture time, not references.

---

## 3. Typed OTP Abstractions (Gleam-Inspired)

**CRITICAL: This is a killer feature for the Erlang backend!**

Gleam's typed OTP abstractions make BEAM programming type-safe. We must include similar abstractions in Metascript.

### The Problem: Raw Erlang OTP is Unsafe

```erlang
% Erlang - No type safety, runtime errors waiting
handle_call(Request, _From, State) ->
    {reply, State + Request, State + Request}.
    % ❌ What if Request isn't a number? Runtime crash!
```

### Gleam's Solution: Typed Actors

```gleam
pub type Message {
  Increment
  GetCount(reply_with: Subject(Int))
}

pub fn handle(msg: Message, state: Int) -> Int {
  case msg {
    Increment -> state + 1
    GetCount(client) -> {
      actor.send(client, state)
      state
    }
  }  // ✅ Compiler ensures all cases handled
}
```

### Metascript's Typed OTP Library

**Module:** `std/erlang/typed_otp.ms` (Erlang backend only)

```typescript
// Type-safe actor with message protocol
type CounterMessage =
    | { type: "increment" }
    | { type: "decrement" }
    | { type: "get", replyTo: Channel<number> };

class CounterActor extends Actor<CounterMessage, { count: number }> {
    constructor() {
        super(
            { count: 0 },
            (msg, state) => {
                switch (msg.type) {
                    case "increment":
                        return { count: state.count + 1 };
                    case "decrement":
                        return { count: state.count - 1 };
                    case "get":
                        msg.replyTo.send(state.count);
                        return state;
                }  // ✅ TypeScript exhaustiveness checking!
            }
        );
    }
}

// Usage with compile-time type safety
const counter = new CounterActor();
counter.send({ type: "increment" });  // ✅ Type-checked
counter.send({ type: "invalid" });    // ❌ Compile error!
```

### Core Abstractions (Phase 3)

**1. Actor<TMessage, TState>**
```typescript
export class Actor<TMessage, TState> {
    constructor(init: TState, handler: (msg: TMessage, state: TState) => TState);
    send(message: TMessage): void;
    call<TReply>(message: TMessage): Promise<TReply>;
}
```

**2. Supervisor**
```typescript
export class Supervisor {
    child<T>(actor: Actor<any, T>, restart: "permanent" | "temporary"): ChildSpec;
    start(children: ChildSpec[]): SupervisorRef;
}
```

**3. Task<T>**
```typescript
export class Task<T> {
    static async<T>(fn: () => T): Task<T>;
    await(timeout?: number): Promise<T>;
}
```

**4. Channel<T>**
```typescript
export class Channel<T> {
    send(value: T): void;
    receive(): Promise<T>;
}
```

### Benefits

| Feature | Raw Erlang | Metascript (Typed OTP) |
|---------|-----------|----------------------|
| **Message safety** | ❌ Runtime | ✅ Compile-time |
| **Exhaustiveness** | ❌ Manual | ✅ TypeScript ensures |
| **Refactoring** | ❌ Dangerous | ✅ Safe (compiler helps) |
| **IDE support** | ❌ Limited | ✅ Full autocomplete |
| **Documentation** | ❌ External | ✅ Types are docs |

### Implementation Timeline

**Phase 3: Typed OTP Library (2-3 weeks)**
- Implement `Actor<TMessage, TState>`
- Implement `Supervisor` with typed children
- Implement `Task<T>` for parallel operations
- Implement `Channel<T>` for typed message passing
- Generate idiomatic Erlang/OTP code

**Result:**
> **TypeScript syntax + Gleam's type-safe OTP + BEAM fault tolerance**

Better than raw Erlang (type safety!), better than JavaScript (OTP!), familiar to TS devs!

---

## 4. Project Status

### Phase 1: COMPLETE ✅ (Dec 3, 2024)

**Built:**
- 720 LOC `erlgen.zig` + 130 LOC tests
- End-to-end: TypeScript → Erlang source
- Classes → tagged tuples, functions, literals, exports
- Module name from file path

**Critical Bugs Fixed:**
- ✅ Memory leaks (all export names properly freed)
- ✅ Module name derives from file path
- ✅ Unused `var_counter` removed

**Test Results:**
- Build: ✅ Success
- Tests: ✅ 293/293 passing
- Integration: ✅ `examples/hello_erlang.ms` compiles to `.erl`

**Example:**
```bash
$ ./zig-out/bin/msc compile --target=erlang examples/hello_erlang.ms
✓ Compilation complete (4ms)
```

```erlang
-module(hello_erlang).
-export([new_user/2, greet/1]).

new_user(Name, Age) -> {user, Name, Age}.
greet(Name) -> <<<<"Hello ">>/binary, Name/binary>>.
```

### Remaining Work

**Phase 1 Completion (~1 week):**
- Member access: `obj.prop` → `element(N, Tuple)`
- Loops: while → tail recursion, for → list comprehension
- Split `erlgen.zig` into modular files
- Test coverage expansion

**Phase 2: Variable Shadowing (1-2 weeks)**
- Implement `@N` suffix generation
- Scope tracking
- Type safety across generations

**Phase 3: Profile-Driven Optimization (José Valim's Recommendation)**

⚠️ **Do NOT optimize until profiling proves benefit**

1. Profile 10+ real TypeScript codebases compiled to Erlang
2. Use `:observer.start()` and `eprof` to find bottlenecks
3. Only add optimizations if ≥2x speedup measured

**Safe optimizations (if profiling justifies):**
- IOList detection for strings (2-5x potential)
- Map update merging (1.5x potential)

**Avoid (dangerous or redundant):**
- ❌ Automatic ETS/persistent_term (causes production outages)
- ❌ Message passing hints (BEAM already optimizes)
- ❌ Stack allocation hints (JIT already does this)

---

## 4. Architecture: Gleam vs Elixir

| Aspect | Gleam | Elixir | Metascript Choice |
|--------|-------|--------|-------------------|
| **Output** | Erlang source | BEAM bytecode | **Erlang source** (debuggable) |
| **Intermediate** | Document builder | Abstract Format | **Document** (simpler) |
| **Variable Hygiene** | HashMap counters | Scope tracking | **HashMap** (explicit) |
| **LOC** | 3,632 | 1,425 | ~2,700 estimated |
| **Testability** | String snapshots | Bytecode tests | **Snapshots** (easier) |

**Decision:** Gleam's approach - generate Erlang source via Document builder.

---

## 6. Translation Mappings

### Core Constructs

| TypeScript | Erlang | Notes |
|-----------|--------|-------|
| Variables | `VarName@N` | Counter suffix for shadowing |
| Functions | `-spec name(Args) -> Return.\nname(Args) -> Body.` | Spec always generated |
| Strings | `<<\"text\"/utf8>>` | UTF-8 binaries |
| Lists | `[H \| T]` | Cons cells |
| Objects | `#{key => value}` | Maps |
| Classes | `{tag, Field1, Field2}` | Tagged tuples |
| `null` | `undefined` | Atom |

### TypeScript-Specific

| TypeScript | Erlang Strategy |
|-----------|----------------|
| Classes | Tagged tuples: `{class_name, Field1, Field2}` |
| Methods | Functions with `This` param: `method(Obj, A, B)` |
| Property access | `element(Index, Obj)` or `maps:get(field, Obj)` |
| Arrays | Lists `[]` |
| String concat | `<<A/binary, B/binary>>` |
| Destructuring | Pattern matching: `{A, B} = Tuple` |

---

## 7. Pattern Matching Translation

**Gleam uses sophisticated pattern system:**

```rust
struct PatternPrinter {
    variables: Vec<&str>,              // Vars bound in pattern
    guards: Vec<Document>,             // Additional guards needed
    assignments: Vec<Assignment>,      // Post-match assignments
}
```

**Some patterns need guards/assignments:**

```typescript
// Nested pattern with guards
case x {
  Point { x: 0, y: y } -> y
}
```

```erlang
case X of
    {point, X@1, Y} when X@1 =:= 0 -> Y
end
```

**Strategy:** Return tuple of `(erl_pattern, guards, assignments)` from pattern translator.

---

## 8. José Valim's Recommendations ⚠️ CRITICAL

**From:** José Valim (Elixir Creator)
**Message:** "Build simple backend. Prove it works. Profile. Then optimize with data, not theories."

### Phase-by-Phase

**Phase 1: Simple Backend (2-3 weeks)** ← **WE ARE HERE**
- ✅ ~1,200 LOC basic codegen (NO Lobster)
- ✅ Follow Gleam's architecture
- ✅ Within 2x of hand-written Erlang (acceptable)

**Phase 2: Profile Real Code (2-3 weeks)**
- Compile 10+ real codebases
- Profile with BEAM tools
- Identify actual bottlenecks
- **Only proceed to Phase 3 if ≥2x improvement proven**

**Phase 3: Selective Optimization (4-6 weeks, IF justified)**
- IOList detection (safe, useful)
- Map update merging (safe transformation)
- **Avoid:** ETS/persistent_term (dangerous), message hints (redundant)

### Revised Performance Expectations

| Optimization | Initial Claim | José's Reality |
|-------------|---------------|----------------|
| Overall | 10-100x | **1.5-5x on data-heavy** |
| IOList | 10x | **2-5x vs naive** |
| Map merging | 3x | **1.5x limited cases** |
| persistent_term | 100x | **5x, manual only** |

**Verdict:** Temper expectations. Real gain is 1.5-5x on specific workloads, not 10-100x.

---

## 9. Implementation Roadmap

### Week 1-2: Foundation ✅ DONE
- Module generation + basic expressions
- Literals, variables, binary operators
- Snapshot tests

### Week 3-4: Control Flow
- Functions, if/else, case expressions
- Variable scoping and shadowing
- 50+ snapshot tests

### Week 5-6: Data Structures
- Objects → maps, arrays → lists
- Pattern matching
- Property access optimization

### Week 7-8: OOP
- Classes → tagged tuples
- Methods, constructors
- Property access via tuple indexing

### Week 9-10: Optimization Pass
- Constant inlining
- String concatenation
- Dead code elimination
- Benchmark vs hand-written Erlang

---

## 10. Test Strategy

### Snapshot Testing (Gleam-style)

```zig
test "class with methods" {
    const src = \\class User { ... };
    const erl = try compileToErlang(testing.allocator, src);
    try expectSnapshot("class_with_methods.erl", erl);
}
```

### Coverage Goals

| Phase | Tests | Coverage | Focus |
|-------|-------|----------|-------|
| Phase 1 | 50 | 70% | Core expressions |
| Phase 2 | 150 | 85% | Pattern matching, OOP |
| Phase 3 | 300+ | 90% | Edge cases |

### Test Categories

- 40% Unit: Individual expression/statement types
- 30% Integration: Multi-function modules
- 20% Edge cases: Reserved words, Unicode
- 10% Regression: Bug fixes

---

## 11. Code Size Reference (Quick)

| Backend | LOC | Notes |
|---------|-----|-------|
| Gleam | 3,632 | Main codegen |
| Elixir | 1,425 | Core (excludes clauses, comprehensions) |
| **Metascript** | **~2,700** | **Phase 1: 1,200 LOC** |

---

## 12. Reserved Words

**47 Erlang keywords** - must quote on collision:

```
after and andalso band begin bnot bor bsl bsr bxor
case catch cond div end fun if let maybe not of
orelse or receive rem spec try when xor
```

**Strategy:** `'div' = 42` (quote if collision detected)

---

## Key Takeaways

**✅ What Works:**
1. Variable shadowing via `@N` suffixes (proven by Gleam/Elixir)
2. Document builder for clean codegen
3. Snapshot testing for fast feedback
4. Erlang source output (debuggable)

**⚠️ Watch Out For:**
1. Variable hygiene (easy to get wrong)
2. Pattern complexity (need guards/assignments)
3. Reserved words (must escape)
4. Closure semantics (breaking change from JS)

**🎯 Critical Success Factors:**
1. **Follow José's phased approach** (simple → profile → optimize)
2. Environment management (variable scoping)
3. Test coverage (snapshot everything)
4. Readable output (hand-written quality)

---

**Document Version:** 3.0 (Streamlined)
**Last Updated:** 2024-12-03
**Lines:** ~600 (down from 2,167 - 72% reduction)
