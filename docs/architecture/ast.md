# Abstract Syntax Tree (AST)

**Purpose:** Unified representation of source code structure

**Role:** Intermediate representation (IR) for all backends

---

## Overview

| Aspect | Implementation |
|--------|----------------|
| **Memory** | Arena-allocated (bulk deallocation) |
| **Nodes** | Tagged union with source locations |
| **Types** | Separate type nodes attached to AST |
| **Traversal** | Visitor pattern |

---

## ASTArena

Central memory manager for all AST allocations:

```zig
pub const ASTArena = struct {
    arena: std.heap.ArenaAllocator,
    file_registry: FileRegistry,

    pub fn createNode(self: *ASTArena, kind: NodeKind, loc: SourceLocation, data: NodeData) !*Node;
    pub fn createType(self: *ASTArena, kind: TypeKind, loc: SourceLocation, data: TypeData) !*Type;
    pub fn addFile(self: *ASTArena, path: []const u8) !FileId;
};
```

### Benefits

| Benefit | Description |
|---------|-------------|
| Fast allocation | O(1) bump allocation |
| Bulk deallocation | Single `deinit()` frees everything |
| Cache-friendly | Nodes allocated contiguously |
| No fragmentation | Arena grows in large chunks |

---

## Node Structure

```zig
pub const Node = struct {
    kind: NodeKind,
    location: SourceLocation,
    data: NodeData,
    type: ?*Type = null,  // Filled by type checker
};
```

### NodeKind Categories

**Literals:**
```zig
.number_literal   // 42, 3.14
.string_literal   // "hello"
.boolean_literal  // true, false
.null_literal     // null
```

**Expressions:**
```zig
.binary_expr      // a + b
.unary_expr       // -a, !a
.call_expr        // foo(x, y)
.member_expr      // obj.prop, obj[key]
.new_expr         // new Foo()
.array_expr       // [1, 2, 3]
.object_expr      // { x: 1 }
.function_expr    // () => {}
.conditional_expr // a ? b : c
.spread_element   // ...expr
```

**Statements:**
```zig
.block_stmt       // { ... }
.expression_stmt  // expr;
.if_stmt          // if (cond) { }
.while_stmt       // while (cond) { }
.for_stmt         // for (;;) { }
.return_stmt      // return x;
.break_stmt       // break;
.continue_stmt    // continue;
.variable_stmt    // const x = 1;
```

**Declarations:**
```zig
.function_decl    // function foo() { }
.class_decl       // class Foo { }
.interface_decl   // interface Foo { }
.type_alias_decl  // type Foo = Bar;
.import_decl      // import { x } from "mod";
.export_decl      // export { x };
```

**Macro Nodes:**
```zig
.macro_decl       // macro derive(ctx) { }
.macro_invocation // @derive(Eq, Hash)
.comptime_block   // @comptime { }
.compile_error    // @compileError("msg")
.quote_expr       // quote { ... }
```

---

## Node Data Types

### Expression Nodes

```zig
pub const BinaryExpr = struct {
    op: BinaryOp,      // +, -, *, /, ===, etc.
    left: *Node,
    right: *Node,
};

pub const CallExpr = struct {
    callee: *Node,
    arguments: []*Node,
    type_args: []*Type,  // foo<T>(x)
};

pub const MemberExpr = struct {
    object: *Node,
    property: *Node,
    computed: bool,  // obj[prop] vs obj.prop
};

pub const ObjectExpr = struct {
    properties: []ObjectProperty,

    pub const ObjectProperty = union(enum) {
        property: struct {
            key: *Node,
            value: *Node,
            shorthand: bool,  // { x } vs { x: x }
        },
        spread: *Node,  // ...other
    };
};
```

### Declaration Nodes

```zig
pub const ClassDecl = struct {
    name: []const u8,
    type_params: []GenericParam,
    extends: ?*Type,
    implements: []*Type,
    members: []*Node,      // Properties, methods
    decorators: []*Node,   // @derive, etc.
};

pub const FunctionDecl = struct {
    name: []const u8,
    type_params: []GenericParam,
    params: []FunctionParam,
    return_type: ?*Type,
    body: ?*Node,
    decorators: []*Node,
};
```

### Macro Nodes

```zig
pub const MacroInvocation = struct {
    name: []const u8,        // "derive"
    arguments: []MacroArgument,
    target: ?*Node,          // The decorated declaration

    pub const MacroArgument = union(enum) {
        identifier: []const u8,  // Eq, Hash
        type: *Type,
        expression: *Node,
    };
};

pub const ComptimeBlock = struct {
    body: *Node,  // Block statement to execute
};

pub const QuoteExpr = struct {
    body: []*Node,              // AST nodes in quote
    interpolations: []*Node,    // ${expr} holes
};
```

---

## Type System

### TypeKind

```zig
pub const TypeKind = enum {
    // Primitives
    number, string, boolean, void, unknown, never,

    // Sized integers
    int8, int16, int32, int64,
    uint8, uint16, uint32, uint64,

    // Floats
    float32, float64,

    // Compound
    object, array, tuple, function,

    // Generics
    generic_param, generic_instance,

    // Memory management (ORC)
    ref,   // ORC-managed heap reference
    lent,  // Borrowed reference (no RC)

    // Special
    @"union", intersection, type_reference,
};
```

### Ownership Tracking

```zig
pub const OwnershipKind = enum {
    owned,    // Single owner, can move
    borrowed, // Temporary borrow, no RC
    shared,   // Multiple owners, RC required
};

pub const Type = struct {
    kind: TypeKind,
    location: SourceLocation,
    data: TypeData,
    ownership: ?OwnershipKind = null,  // For Lobster optimization
};
```

---

## Visitor Pattern

```zig
pub const Visitor = struct {
    pub fn visit(self: *Visitor, node: *Node) anyerror!void {
        switch (node.kind) {
            .program => try self.visitProgram(node),
            .class_decl => try self.visitClassDecl(node),
            .function_decl => try self.visitFunctionDecl(node),
            .binary_expr => try self.visitBinaryExpr(node),
            // ...
        }
    }

    // Override in subclasses
    pub fn visitProgram(self: *Visitor, node: *Node) anyerror!void {
        for (node.data.program.statements) |stmt| {
            try self.visit(stmt);
        }
    }

    pub fn visitBinaryExpr(self: *Visitor, node: *Node) anyerror!void {
        try self.visit(node.data.binary_expr.left);
        try self.visit(node.data.binary_expr.right);
    }
};
```

### Use Cases

| Visitor | Purpose |
|---------|---------|
| TypeChecker | Infer and validate types |
| MacroExpander | Expand @derive, @comptime |
| OwnershipAnalyzer | Determine RC requirements |
| CodeGenerator | Emit C/JS/Erlang |
| ASTPrinter | Debug output |

---

## Source Locations

```zig
pub const SourceLocation = struct {
    file: FileId,
    start: Position,
    end: Position,

    pub const Position = struct {
        line: u32,
        column: u32,
        offset: u32,  // Byte offset in file
    };
};
```

### FileRegistry

```zig
pub const FileRegistry = struct {
    files: std.ArrayList([]const u8),

    pub fn addFile(self: *FileRegistry, path: []const u8) !FileId;
    pub fn getPath(self: *const FileRegistry, id: FileId) ?[]const u8;
};
```

---

## AST as IR

The typed AST serves as the intermediate representation:

```
Source → Parser → AST → Macro Expansion → Type Checking →
                                                         ↓
                                                   Typed AST (IR)
                                                         ↓
                              ┌──────────────────────────┼──────────────────────────┐
                              ↓                          ↓                          ↓
                         C Backend                 JS Backend               Erlang Backend
```

### Why Typed AST as IR?

| Advantage | Description |
|-----------|-------------|
| No separate IR | Less complexity, fewer transformations |
| Type info preserved | Backends can make type-based decisions |
| Source locations | Error messages point to user code |
| Macro-expanded | Backends see final code structure |

---

## Node Helpers

```zig
impl Node {
    pub fn isExpression(self: *const Node) bool {
        return switch (self.kind) {
            .number_literal, .string_literal, .binary_expr,
            .call_expr, .member_expr, .array_expr, ... => true,
            else => false,
        };
    }

    pub fn isStatement(self: *const Node) bool {
        return switch (self.kind) {
            .block_stmt, .if_stmt, .while_stmt,
            .return_stmt, .variable_stmt, ... => true,
            else => false,
        };
    }
}

impl Type {
    pub fn isPrimitive(self: Type) bool;
    pub fn isRefType(self: Type) bool;  // Requires ORC
}
```

---

## File Structure

```
src/ast/
  ast.zig           # ASTArena, Visitor, re-exports
  node.zig          # NodeKind, Node, expression/statement types
  types.zig         # TypeKind, Type, ownership
  location.zig      # SourceLocation, FileRegistry
```

---

## Usage Example

```zig
// Create arena
var arena = ASTArena.init(allocator);
defer arena.deinit();

// Register file
const file_id = try arena.addFile("example.ms");

// Create nodes
const num = try arena.createNode(.number_literal, loc, .{ .number_literal = 42 });
const str = try arena.createNode(.string_literal, loc, .{ .string_literal = "hello" });

// Create binary expression
const add = try arena.createNode(.binary_expr, loc, .{
    .binary_expr = .{
        .op = .add,
        .left = num,
        .right = num,
    },
});

// Visit tree
var visitor = MyVisitor{};
try visitor.visit(add);
```

---

## Testing

```bash
zig build test -- --test-filter="ast"
```

```zig
test "AST: create class node" {
    var arena = ASTArena.init(testing.allocator);
    defer arena.deinit();

    const class_node = try arena.createNode(.class_decl, loc, .{
        .class_decl = .{
            .name = "User",
            .members = &.{},
            .decorators = &.{},
            ...
        },
    });

    try testing.expectEqual(.class_decl, class_node.kind);
    try testing.expectEqualStrings("User", class_node.data.class_decl.name);
}
```

---

## References

- **Parser output:** `./parser.md`
- **Type checking:** `./ast-analyzer.md`
- **Macro expansion:** `./macros.md`
- **Ownership analysis:** `./lobster.md`
- **Code generation:** `./backends.md`
