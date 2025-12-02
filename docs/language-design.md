# Language Design Decisions

Specific design decisions for Metascript language features, syntax, and semantics.

---

## Type System: Real Static Types vs TypeScript's "Contracts"

**Decision:** Types are semantic reality, not compile-time suggestions.

TypeScript's types are erased at runtime—they're a "contract" that helps development but the JavaScript runtime ignores them completely. Metascript's types determine memory layout, enable optimizations, and affect code generation across all backends. There is no `any` escape hatch, no type erasure, and no runtime type violations. Types in Metascript behave like Zig/Rust (compiler guarantees, predictable performance, monomorphization) while maintaining TypeScript syntax for familiarity.

**Why:** Multi-backend compilation requires real types. Cannot generate efficient C code or achieve 90%+ native performance if types are just documentation. The C backend needs exact sizes and alignments, the JavaScript backend can use typed arrays, and the Erlang backend requires compile-time type contracts for dialyzer.

**Trade-off:** Lower TypeScript compatibility (no `any`, stricter rules) in exchange for native performance and multi-backend code generation.

---

## Equality Operators: `==` vs `===`

**Decision:** Keep both operators with distinct, type-safe semantics.

### The Design

```typescript
// === is strict equality (no coercion)
42 === "42"          // ❌ Compile error: Type mismatch

// == is a @comptime macro (type-safe loose equality)
42 == "42"           // ✓ Expands to: parseFloat("42") === 42
```

**Key Insight:** `==` implements JavaScript's loose equality semantics at **compile time** via macros, not runtime coercion. Everything remains statically typed.

### Why This Approach

**Validates the Macro System:**
- Real, practical use case developers actually want
- Perfect demonstration of "bridging dynamic patterns to static code"
- Showcases compile-time metaprogramming power

**Respects JavaScript Semantics (Safely):**
```typescript
// JavaScript (runtime coercion, footguns)
1 == "1"     // true (runtime type coercion)
1 == true    // true (unexpected behavior)
[] == ![]    // true (WAT)

// Metascript (compile-time macro, type-safe)
1 == "1"     // parseFloat("1") === 1 → true (explicit conversion)
1 == true    // ❌ Compile error: No valid coercion
[] == ![]    // ❌ Compile error: Array comparison not supported
```

**Zero Runtime Cost:**
- All coercion logic executes at **compile time**
- Compiler knows types → generates optimal code
- No runtime type checks for monomorphic code

### Macro Expansion Examples

**Same type → strict equality:**
```typescript
// Source
"hello" == "world"

// Macro expansion
"hello" === "world"  // No coercion needed
```

**String ↔ Number coercion:**
```typescript
// Source
const num: number = 42;
const str: string = "42";
num == str

// Macro expansion (compile-time)
num === parseFloat(str)

// Backend output
├─ C:        num == atof(str)
├─ JS:       num === parseFloat(str)  // Or: num == str
└─ Erlang:   Num =:= string:to_float(Str)
```

**Union types → runtime check:**
```typescript
// Source
const value: number | string = getValue();
const target: number = 42;
value == target

// Macro expansion
typeof value === 'string'
  ? parseFloat(value) === target
  : value === target
```

**Compile-time constants:**
```typescript
// Source
null == undefined

// Macro expansion
true  // Known at compile time
```

### Coercion Rules (Type-Safe Subset)

**Supported coercions (sane subset):**

| Left | Right | Expansion |
|------|-------|-----------|
| `number` | `string` | `parseFloat(string) === number` |
| `string` | `number` | `parseFloat(string) === number` |
| `null` | `undefined` | `true` (compile-time constant) |
| `undefined` | `null` | `true` (compile-time constant) |
| Same type | Same type | `left === right` (no coercion) |

**Rejected coercions (compile errors):**

```typescript
// ❌ Boolean coercion
0 == false           // Error: Cannot compare number and boolean
"" == false          // Error: Cannot compare string and boolean

// ❌ Array/Object coercion
[] == false          // Error: Cannot compare Array and boolean
{} == "[object Object]"  // Error: Object coercion not supported

// ❌ Weird edge cases
[] == ![]            // Error: Array comparison not supported
```

### Implementation Strategy

```zig
// src/macro/builtin_macros.zig
fn expandLooseEquality(ctx: MacroContext, left: Expr, right: Expr) !AST {
    const left_type = ctx.typeOf(left);
    const right_type = ctx.typeOf(right);

    // Same type → strict equality (no coercion)
    if (left_type.equals(right_type)) {
        return AST.strictEq(left, right);
    }

    // String ↔ Number coercion
    if (left_type.isNumber() and right_type.isString()) {
        return AST.strictEq(left, AST.call("parseFloat", right));
    }
    if (left_type.isString() and right_type.isNumber()) {
        return AST.strictEq(AST.call("parseFloat", left), right);
    }

    // null == undefined special case
    if ((left_type.isNull() and right_type.isUndefined()) or
        (left_type.isUndefined() and right_type.isNull())) {
        return AST.literal(true);  // Compile-time constant
    }

    // Union types → runtime type check
    if (left_type.isUnion() or right_type.isUnion()) {
        return expandUnionEquality(ctx, left, right);
    }

    // No valid coercion → compile error
    return ctx.error(
        "Cannot compare {s} and {s}. Use === for strict equality or convert explicitly.",
        .{left_type.name(), right_type.name()}
    );
}
```

### Backend-Specific Optimizations

**C Backend:**
```c
// Metascript: num == str
// Direct C conversion
result = (num == atof(str));
```

**JavaScript Backend (two strategies):**

*Strategy A: Native `==` (when compatible):*
```javascript
// Metascript: num == str
// Emit native JavaScript ==
result = (num == str);
```

*Strategy B: Explicit conversion (safer):*
```javascript
// Metascript: num == str
// Always explicit for predictability
result = (num === parseFloat(str));
```

**Erlang Backend:**
```erlang
% Metascript: num == str
Result = Num =:= string:to_float(Str)
```

### TypeScript Compatibility

**Goal: 70% of strict TypeScript compiles unchanged**

This design maximizes compatibility:
- ✓ Both `==` and `===` work (syntax compatible)
- ✓ Semantics match JavaScript for valid cases
- ✓ Compile errors prevent footguns
- ✓ Clear migration path (lint → fix → migrate)

**Migration from TypeScript:**

```typescript
// TypeScript code using both operators
if (userInput == "42") { /* ... */ }      // Works in Metascript
if (count === total) { /* ... */ }        // Works in Metascript
if (obj == null) { /* ... */ }            // Works in Metascript

// Edge cases that fail
if (value == true) { /* ... */ }          // ❌ Compile error in Metascript
// Fix: if (value === true) { /* ... */ }
```

### Advantages Over Alternatives

| Approach | TS Compatibility | JS Semantics | Macro Showcase | Safety |
|----------|------------------|--------------|----------------|--------|
| Both → `===` | ✓ Syntax only | ✗ Different | ✗ | ✓ |
| Only `===` | ✗ Breaking | ✗ Different | ✗ | ✓ |
| `==` is macro | ✓ Full | ✓ Respected (safely) | ✓ **Perfect** | ✓ |

### Documentation & Messaging

**Pitch:** "Metascript's `==` operator is JavaScript's loose equality, but type-safe and zero-cost."

**Key Points:**
- Familiar semantics for TS/JS developers
- No runtime footguns (type errors at compile time)
- Zero runtime overhead (macro expansion)
- Demonstrates compile-time metaprogramming power

**Error Messages:**
```
error: Cannot compare number and boolean with ==
  --> src/main.mts:42:5
   |
42 |   if (count == true) {
   |       ^^^^^^^^^^^^^ no valid coercion exists
   |
   = help: Use === for strict equality, or convert explicitly:
           if (count > 0) { ... }  // boolean check
           if (count === 1) { ... }  // number check
```

### Future Extensions

**Potential additions (if needed):**

1. **Custom equality macros:**
   ```typescript
   @defineEquality(Point)
   fn pointEquals(a: Point, b: Point): boolean {
     return a.x === b.x && a.y === b.y;
   }
   ```

2. **Approximate equality for floats:**
   ```typescript
   const a: float = 0.1 + 0.2;
   const b: float = 0.3;
   a == b  // Could use epsilon comparison for floats
   ```

3. **User-defined coercions:**
   ```typescript
   @defineCoercion(Timestamp, Date)
   fn timestampToDate(ts: Timestamp): Date {
     return new Date(ts.seconds * 1000);
   }
   ```

### Related Design Decisions

**See also:**
- [Macro System](./macro-system.md) - Compile-time metaprogramming
- [Type System](./type-system.md) - Static typing rules
- [Migration from TypeScript](./migration-from-typescript.md) - TS compatibility

---

## No `struct` Keyword: Class as Value Type in C

**Decision:** No separate `struct`. Class becomes C struct when no inheritance.

```
Class → C struct:  ✓ No inheritance (methods OK!)
Class → C heap:    ✗ Has inheritance (needs vtable)
```

**Example:**

```typescript
class Vec3 {
    x: f32; y: f32; z: f32;
    length(): f32 { return Math.sqrt(this.x**2 + this.y**2 + this.z**2); }
}
// → C struct + functions (Rust-style, methods don't force heap)
```

```c
typedef struct { float x, y, z; } Vec3;
float Vec3_length(Vec3 self) { return sqrt(self.x*self.x + ...); }
```

**JS/Erlang:** Objects/tuples as usual.

---

**Status:** Proposed design (pending implementation)
**Last Updated:** 2025-12-02
