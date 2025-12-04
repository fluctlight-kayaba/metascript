# Parser

**Purpose:** Transform token stream into Abstract Syntax Tree

**Technique:** Recursive descent with Pratt precedence for expressions

---

## Overview

| Aspect | Implementation |
|--------|----------------|
| **Algorithm** | Recursive descent + Pratt precedence |
| **Error Recovery** | Panic mode with synchronization |
| **Memory** | Arena allocation via ASTArena |
| **Output** | Typed AST nodes with source locations |

---

## Parser Architecture

```zig
pub const Parser = struct {
    allocator: std.mem.Allocator,
    arena: *ast.ASTArena,
    lexer: *Lexer,
    file_id: ast.FileId,

    current: Token,      // Current token
    previous: Token,     // Previous token (for locations)

    errors: std.ArrayList(ParseError),
    panic_mode: bool,    // Error recovery state
};
```

### Token Management

```zig
// Advance to next token
fn advance(self: *Parser) void;

// Check current token type (no consume)
fn check(self: *Parser, kind: TokenKind) bool;

// Consume if match, else error
fn consume(self: *Parser, kind: TokenKind, msg: []const u8) !void;

// Check and consume if match
fn match(self: *Parser, kind: TokenKind) bool;
```

---

## Parse Entry Point

```zig
pub fn parse(self: *Parser) !*ast.Node {
    self.advance();  // Prime with first token

    var statements = std.ArrayList(*ast.Node).init(self.allocator);

    while (!self.check(.end_of_file)) {
        if (self.parseTopLevelDeclaration()) |stmt| {
            try statements.append(stmt);
        } else |err| {
            switch (err) {
                error.ParseError => self.synchronize(),
                else => return err,
            }
        }
    }

    return try self.arena.createNode(.program, ...);
}
```

---

## Grammar Rules

### Top-Level Declarations

| Declaration | Keyword | Example |
|-------------|---------|---------|
| Class | `class` | `class User { ... }` |
| Function | `function` | `function add(a: number) { }` |
| Interface | `interface` | `interface Shape { }` |
| Type Alias | `type` | `type ID = number` |
| Enum | `enum` | `enum Color { Red, Blue }` |
| Variable | `const/let/var` | `const x = 1;` |
| Import | `import` | `import { x } from "mod"` |
| Export | `export` | `export { x }` |
| Macro | `macro` | `macro derive(ctx) { }` |

### Decorator Parsing

```zig
fn parseDecorator(self: *Parser) !*ast.Node {
    // @name or @name(args)
    self.advance(); // consume @
    const macro_name = self.current.text;
    self.advance();

    // Parse arguments: @derive(Eq, Hash)
    if (self.match(.left_paren)) {
        // Parse comma-separated arguments
    }

    return try self.arena.createNode(.macro_invocation, ...);
}
```

Decorator flow:
```
@derive(Eq, Hash)    →    MacroInvocation
class User { }              ↓
                     ClassDecl.decorators = [MacroInvocation]
```

### Class Declaration

```zig
fn parseClassDeclaration(self: *Parser, decorators: *ArrayList(*Node)) !*Node {
    // class Name<T> extends Base implements Interface { ... }

    // 1. Parse name
    const name = self.current.text;

    // 2. Type parameters <T, U>
    var type_params = ...;
    if (self.match(.less_than)) {
        // Parse generic parameters
    }

    // 3. Extends clause
    if (self.match(.keyword_extends)) {
        extends_clause = try self.parseType();
    }

    // 4. Implements clause
    if (self.match(.keyword_implements)) {
        // Parse interface list
    }

    // 5. Class body { ... }
    // Parse properties, methods, constructor
}
```

### Expression Parsing (Pratt Precedence)

```zig
fn parseExpression(self: *Parser) !*Node {
    return self.parsePrecedence(.assignment);
}

fn parsePrecedence(self: *Parser, min_prec: Precedence) !*Node {
    // Prefix: literals, unary, grouping
    var left = try self.parsePrefix();

    // Infix: binary operators
    while (precedenceOf(self.current.kind) > min_prec) {
        left = try self.parseInfix(left);
    }

    return left;
}
```

Precedence levels (low to high):

| Level | Operators |
|-------|-----------|
| Assignment | `=` |
| Or | `\|\|` |
| And | `&&` |
| Equality | `===`, `!==` |
| Comparison | `<`, `>`, `<=`, `>=` |
| Term | `+`, `-` |
| Factor | `*`, `/`, `%` |
| Unary | `-`, `!`, `typeof` |
| Call | `()`, `.`, `[]` |
| Primary | literals, identifiers |

---

## Macro Syntax Support

### Nim-Style Macro Declaration

```typescript
macro derive(ctx) {
    const targetClass = ctx.target;
    // Generate methods...
    return quote { ... };
}
```

Parser creates:
```zig
.macro_decl = .{
    .name = "derive",
    .params = [ctx],
    .body = BlockStmt,
}
```

### Comptime Blocks

```typescript
@comptime {
    const config = loadConfig();
    // Compile-time code
}
```

Parser creates:
```zig
.comptime_block = .{
    .body = BlockStmt,
}
```

### Quote Expressions

```typescript
macro addMethod(ctx) {
    return quote {
        equals(other: ${ctx.target.name}): boolean {
            return this.id === other.id;
        }
    };
}
```

Parser creates:
```zig
.quote_expr = .{
    .body = [...nodes...],
    .interpolations = [${ctx.target.name}],
}
```

---

## Error Recovery

### Panic Mode

When error occurs:
1. Set `panic_mode = true`
2. Report error with location
3. Call `synchronize()` to find safe point
4. Resume parsing

```zig
fn synchronize(self: *Parser) void {
    self.panic_mode = false;

    while (!self.check(.end_of_file)) {
        // Synchronize at statement boundaries
        if (self.previous.kind == .semicolon) return;

        switch (self.current.kind) {
            .keyword_class,
            .keyword_function,
            .keyword_const,
            .keyword_let,
            .keyword_if,
            .keyword_for,
            .keyword_while,
            .keyword_return => return,
            else => {},
        }
        self.advance();
    }
}
```

### Error Reporting

```zig
pub const ParseError = struct {
    message: []const u8,
    loc: ast.SourceLocation,
    severity: Severity,

    pub const Severity = enum { @"error", warning };
};
```

---

## Integration with Pipeline

```
Lexer → Parser → AST → Macro Expansion → Type Checking → Codegen
              ↑
         This component
```

### Usage

```zig
const lexer = Lexer.init(allocator, source);
var arena = ASTArena.init(allocator);
var parser = Parser.init(allocator, &arena, &lexer, file_id);
defer parser.deinit();

const ast = try parser.parse();

if (parser.errors.items.len > 0) {
    for (parser.errors.items) |err| {
        reportError(err);
    }
}
```

---

## Supported Syntax

### TypeScript Subset

| Feature | Supported | Notes |
|---------|-----------|-------|
| Classes | Yes | Full ES6 classes |
| Interfaces | Yes | For type checking |
| Generics | Yes | `<T, U>` syntax |
| Type annotations | Yes | `: Type` syntax |
| Arrow functions | Yes | `() => {}` |
| Destructuring | Partial | Basic patterns |
| Spread operator | Yes | `...obj` |
| Optional chaining | Planned | `?.` |
| Nullish coalescing | Planned | `??` |

### Metascript Extensions

| Feature | Syntax | Purpose |
|---------|--------|---------|
| Decorators | `@derive(...)` | Macro invocation |
| Comptime | `@comptime { }` | Compile-time execution |
| Quote | `quote { }` | AST quotation |
| Macro decl | `macro name(ctx) { }` | Define macros |

---

## File Location

```
src/parser/
  parser.zig         # Main parser implementation

src/ast/
  ast.zig            # ASTArena, createNode
  node.zig           # NodeKind, Node, NodeData
  types.zig          # Type representations
  location.zig       # SourceLocation
```

---

## Testing

```bash
# Run parser tests
zig build test -- --test-filter="parser"

# Test specific syntax
zig test src/parser/parser_test.zig --test-filter="class"
```

### Test Patterns

```zig
test "parser: class with generics" {
    const source = "class Box<T> { value: T; }";
    var parser = setupParser(source);

    const ast = try parser.parse();

    try testing.expectEqual(.class_decl, ast.statements[0].kind);
    try testing.expectEqual(1, ast.statements[0].data.class_decl.type_params.len);
}

test "parser: decorator with arguments" {
    const source = "@derive(Eq, Hash) class User { }";
    var parser = setupParser(source);

    const ast = try parser.parse();
    const class_node = ast.statements[0];

    try testing.expectEqual(1, class_node.data.class_decl.decorators.len);
    try testing.expectEqualStrings("derive", class_node.data.class_decl.decorators[0].data.macro_invocation.name);
}
```

---

## References

- **Lexer input:** `./lexer.md`
- **AST output:** `./ast.md`
- **Macro expansion:** `./macros.md`
- **Type checking:** `./ast-analyzer.md`
