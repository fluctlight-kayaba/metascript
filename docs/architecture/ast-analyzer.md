# AST Analyzer & Type Checker

**Purpose:** Semantic analysis, type checking, symbol resolution, and lifetime analysis

**Performance Target:** <200ms for 10K LOC type checking

---

## Pipeline Overview

```
Parse → Raw AST → Symbol Resolution → Macro Detection →
Macro Execution → Re-analyze → Typed AST (IR)
```

The Typed AST serves as our intermediate representation - no separate IR layer needed.

---

## Type System

### Static-First Design

| Principle | Implementation |
|-----------|----------------|
| All types statically resolvable | No runtime type checks (except guards) |
| No `any` | Use explicit `unknown` with type guards |
| No implicit coercion | Explicit casts required |
| Generics monomorphized | `T` becomes concrete types |

### Object Model

```typescript
// Interfaces → structs (fixed layout)
interface Point { x: number; y: number; }
// → typedef struct { double x, y; } Point;

// Classes use nominal typing
class User { name: string; }
class Admin { name: string; }
// User !== Admin (different types)

// Methods use vtables or devirtualization
```

---

## Semantic Analysis Phases

### 1. Symbol Resolution

```zig
pub const SymbolTable = struct {
    scopes: std.ArrayList(Scope),
    current_scope: usize,

    pub fn lookup(self: *SymbolTable, name: []const u8) ?*Symbol {
        var scope = self.current_scope;
        while (scope >= 0) {
            if (self.scopes.items[scope].get(name)) |sym| {
                return sym;
            }
            scope -= 1;
        }
        return null;
    }
};
```

### 2. Macro Detection & Execution

```zig
pub const MacroResolver = struct {
    ctx: *TypeCheckContext,
    macro_cache: std.AutoHashMap(u64, *Node),

    pub fn resolveDecorators(self: *MacroResolver, decorators: []*Node, target: *Node) ![]*Node {
        var expanded = std.ArrayList(*Node).init(self.ctx.allocator);

        for (decorators) |decorator| {
            if (self.isMacroCall(decorator)) {
                const cache_key = self.computeASTHash(decorator, target);
                if (self.macro_cache.get(cache_key)) |cached| {
                    try expanded.appendSlice(cached);
                    continue;
                }
                const result = try self.executeMacro(decorator, target);
                try self.macro_cache.put(cache_key, result);
                try expanded.appendSlice(result);
            }
        }
        return expanded.toOwnedSlice();
    }
};
```

### 3. Type Checking

- Nominal typing for classes
- Structural typing for interfaces
- Strict null checking
- Type inference with context

---

## Expansion Tracking

Macro-generated code tracks its origin for error messages:

```zig
pub const MacroExpansion = struct {
    macro_name: []const u8,        // "derive", "comptime", etc.
    original_ast: *Node,           // User code (for error messages)
    expanded_at: SourceLocation,   // Where expansion happened
};

pub const Node = struct {
    kind: NodeKind,
    location: SourceLocation,
    data: NodeData,
    type: ?*Type = null,
    expansion_origin: ?*MacroExpansion = null,  // Track macro origin
};
```

**Benefit:** Error messages point to user code, not generated code.

---

## Lifetime Analysis (Lobster-style)

### Purpose

Determine when values "die" to optimize memory management:
- Eliminate unnecessary allocations
- Identify single-use temporaries
- Enable move semantics

### Analysis Output

```zig
pub const OwnershipKind = enum {
    owned,      // First assignment, owns the value
    borrowed,   // Reference, doesn't own
    shared,     // Multiple references, needs RC
};

fn analyzeOwnership(expr: *ast.Node) OwnershipKind {
    return switch (expr.kind) {
        .variable_decl => .owned,
        .parameter => .borrowed,
        .field_access => .borrowed,
        .return_expr => if (expr.value.ownership == .owned)
            .borrowed else .shared,
        else => .shared,
    };
}
```

### Integration with DRC

Lifetime analysis feeds into DRC (reference counting):
- `owned` + `borrowed` = no RC operations needed
- `shared` = requires RC increment/decrement

---

## Two-Phase Type Context

Macros need type info, but type checking needs expanded macros. Solution: split into phases.

```
PHASE 1: Shallow Context (before macro expansion)
  - Class names
  - Property names and type names (strings)
  - Method signatures (shallow)

PHASE 2: Deep Context (after macro expansion)
  - Fully resolved types
  - Generic instantiations
  - Complete type checking
```

---

## Testing Strategy

### Unit Tests (80% coverage)

```zig
test "type checker infers variable type" {
    const source = "let x = 42;";
    const ast = parse(source);
    const typed = typeCheck(ast);
    try testing.expectEqual(.number, typed.variables[0].type.kind);
}

test "type checker errors on type mismatch" {
    const source = "let x: string = 42;";
    const result = typeCheck(parse(source));
    try testing.expect(result.hasError());
}
```

### Integration Tests

```zig
test "macro expansion + type checking" {
    const source = "@derive(Eq) class User { name: string; }";
    const ast = parse(source);
    const expanded = expandMacros(ast);
    const typed = typeCheck(expanded);

    // Check generated equals method has correct type
    const equals = typed.classes[0].getMethod("equals");
    try testing.expectEqual(.boolean, equals.returnType.kind);
}
```

---

## Coverage Expectations

| Component | Target | Critical Paths |
|-----------|--------|----------------|
| Type Checker | 80% | Type inference, error messages |
| Symbol Resolution | 85% | Scope handling, shadowing |
| Macro Resolver | 85% | All standard macros |

---

## File Location

```
src/checker/
  typechecker.zig    # Main type checking
  macro_resolver.zig # Macro detection & execution
  symbols.zig        # Symbol table management
  lifetime.zig       # Lifetime/ownership analysis
```

---

## References

- **Macro system:** `./macros.md`
- **DRC memory management:** `./drc-orc.md`
- **Trans-Am caching:** `./trans-am.md`
- **Type checker research:** `../type-checker.md`
