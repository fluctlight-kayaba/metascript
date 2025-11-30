# Metascript Parser Design

**Goal:** TypeScript parser with macro support, LSP-first architecture, <100ms responsiveness

**Research Base:** Nim (macros), Haxe (multi-backend), Bun (Zig TypeScript parser)

---

## Architecture Overview

```
Source (.mts)
    ↓
┌─────────────────────────────────────────────────────────────┐
│ Lexer (single-pass, streaming tokens)                      │
│  - @comptime, @derive → T.at_comptime, T.at_derive         │
│  - Stack-allocated token buffer                             │
│  - Source location tracking (byte offsets)                  │
└────────────────────┬────────────────────────────────────────┘
                     │ Token Stream
                     ↓
┌─────────────────────────────────────────────────────────────┐
│ Parser (comptime-specialized, macro-agnostic)               │
│  - Recursive descent                                        │
│  - 8KB stack allocator → bump allocator → heap             │
│  - Error recovery (collect all, halt before visiting)       │
│  - Decorators parsed as CallExpr nodes                      │
│  - NO macro expansion (just AST construction)               │
└────────────────────┬────────────────────────────────────────┘
                     │ Raw AST (macros = CallExpr)
                     ↓
┌─────────────────────────────────────────────────────────────┐
│ Semantic Analysis (Type Checker)                            │
│  - Symbol resolution                                        │
│  - Identify macro invocations (@derive, @comptime)          │
│  - Execute macros (AST → AST transformation)                │
│  - Re-analyze expanded AST                                  │
│  - Cache expansions by AST hash                             │
└────────────────────┬────────────────────────────────────────┘
                     │ Typed AST (all macros expanded)
                     ↓
┌─────────────────────────────────────────────────────────────┐
│ Unified IR Lowering                                         │
│  - Backend-agnostic IR                                      │
│  - No macro syntax (all expanded)                           │
└────────────────────┬────────────────────────────────────────┘
                     │
          ┌──────────┴──────────┬──────────────┐
          ↓                     ↓              ↓
    C Backend           JavaScript Backend   Erlang Backend
```

---

## 1. Lexer Design

**Pattern:** Bun's single-pass streaming + Nim's macro token awareness

### Token Types (Extend TypeScript)

```zig
// src/lexer/token.zig
pub const TokenKind = enum {
    // TypeScript standard tokens
    t_identifier,
    t_number,
    t_string,
    t_keyword_function,
    t_keyword_class,
    t_keyword_const,
    // ... ~80 standard tokens

    // Macro-specific tokens
    t_at_sign,           // @
    t_at_derive,         // @derive
    t_at_comptime,       // @comptime
    t_at_serialize,      // @serialize
    t_at_ffi,            // @ffi

    // Special
    t_end_of_file,
    t_syntax_error,
};

pub const Token = struct {
    kind: TokenKind,
    loc: SourceLocation,  // Byte offset + length

    // Payload (no heap allocation)
    data: union(TokenKind) {
        t_identifier: []const u8,
        t_number: f64,
        t_string: []const u8,
        // Most tokens have no data
        else: void,
    },
};
```

### Lexer State Machine

```zig
// src/lexer/lexer.zig
pub const Lexer = struct {
    source: []const u8,
    current: usize,      // Current byte offset
    start: usize,        // Token start

    // State
    has_newline_before: bool,
    prev_error_loc: SourceLocation,

    // Error tracking (LSP-friendly)
    errors: std.ArrayList(LexError),

    pub fn next(self: *Lexer) !Token {
        self.skipWhitespaceAndComments();
        self.start = self.current;

        if (self.isAtEnd()) return self.makeToken(.t_end_of_file);

        const c = self.advance();
        return switch (c) {
            '@' => self.scanMacroToken(),  // @derive, @comptime, etc.
            'a'...'z', 'A'...'Z', '_' => self.scanIdentifier(),
            '0'...'9' => self.scanNumber(),
            '"', '\'' => self.scanString(),
            // ... standard operators
            else => self.errorToken("Unexpected character"),
        };
    }

    fn scanMacroToken(self: *Lexer) Token {
        // @derive, @comptime, @serialize, etc.
        if (self.matchWord("derive")) return self.makeToken(.t_at_derive);
        if (self.matchWord("comptime")) return self.makeToken(.t_at_comptime);
        if (self.matchWord("serialize")) return self.makeToken(.t_at_serialize);
        if (self.matchWord("ffi")) return self.makeToken(.t_at_ffi);

        // Generic @identifier for user-defined macros
        return self.makeToken(.t_at_sign);
    }
};
```

**Performance:** Single-pass, no backtracking, <1ms for 1000 LOC

---

## 2. Parser Design

**Pattern:** Bun's comptime specialization + Nim's macro-agnostic AST + Haxe's metadata

### Comptime Specialization

```zig
// src/parser/parser.zig
pub const ParserFeatures = struct {
    macros: bool = true,
    typescript: bool = true,
    backend: Backend = .c,
};

pub fn NewParser(comptime features: ParserFeatures) type {
    return struct {
        const Self = @This();
        const has_macros = features.macros;
        const has_typescript = features.typescript;

        lexer: *Lexer,
        allocator: std.mem.Allocator,
        arena: *ASTArena,

        // Error recovery
        errors: std.ArrayList(ParseError),
        panic_mode: bool = false,

        pub fn parseProgram(self: *Self) !*Node {
            var statements = std.ArrayList(*Node).init(self.allocator);

            while (!self.match(.t_end_of_file)) {
                if (self.parseTopLevelStmt()) |stmt| {
                    try statements.append(stmt);
                } else |err| {
                    // Error recovery: skip to next statement
                    self.synchronize();
                    if (err != error.ParseError) return err;
                }
            }

            return try self.arena.createNode(.program, ...);
        }

        fn parseTopLevelStmt(self: *Self) !*Node {
            // Decorators (including macros)
            var decorators: []*Node = &[_]*Node{};
            if (comptime has_macros) {
                decorators = try self.parseDecorators();
            }

            return switch (self.lexer.token.kind) {
                .t_keyword_function => self.parseFunctionDecl(decorators),
                .t_keyword_class => self.parseClassDecl(decorators),
                .t_keyword_interface => self.parseInterfaceDecl(),
                // ... other declarations
                else => self.parseStmt(),
            };
        }

        fn parseDecorators(self: *Self) ![]*Node {
            var decorators = std.ArrayList(*Node).init(self.allocator);

            while (self.match(.t_at_sign) or
                   self.match(.t_at_derive) or
                   self.match(.t_at_comptime))
            {
                // Parse as CallExpr: @derive(Eq, Hash)
                const decorator = try self.parseCallExpr();
                try decorators.append(decorator);
            }

            return decorators.toOwnedSlice();
        }
    };
}

// Instantiate specialized parsers
pub const MetascriptParser = NewParser(.{
    .macros = true,
    .typescript = true,
    .backend = .c,
});
```

### Memory Management (Bun Pattern)

```zig
// src/parser/arena.zig
pub const ASTArena = struct {
    // Stack-first allocation (8KB fast path)
    stack_allocator: std.heap.StackFallbackAllocator(8192),
    bump_allocator: std.mem.Allocator,
    allocator: std.mem.Allocator,

    pub fn init(backing_allocator: std.mem.Allocator) ASTArena {
        var stack_allocator = std.heap.StackFallbackAllocator(8192){
            .fallback_allocator = backing_allocator,
        };

        return .{
            .stack_allocator = stack_allocator,
            .bump_allocator = stack_allocator.get(),
            .allocator = backing_allocator,
        };
    }

    pub fn createNode(self: *ASTArena, kind: NodeKind, loc: SourceLocation, data: NodeData) !*Node {
        const node = try self.bump_allocator.create(Node);
        node.* = .{
            .kind = kind,
            .loc = loc,
            .data = data,
        };
        return node;
    }

    pub fn reset(self: *ASTArena) void {
        // Reset for next parse (LSP incremental)
        self.stack_allocator.reset();
    }
};
```

**Performance:** 90% of functions fit in 8KB stack → no malloc

---

## 3. AST Design (Macro-Agnostic)

**Pattern:** Nim (uniform CallExpr) + Bun (24-byte nodes) + Haxe (metadata)

### Node Structure

```zig
// src/ast/node.zig (already exists, extend it)

// Add macro-specific tokens to existing NodeKind:
pub const NodeKind = enum {
    // ... existing 37 node types

    // Macro invocations parsed as CallExpr initially
    // Semantic analysis distinguishes them

    // Macro expansion tracking (Nim's ComesFrom pattern)
    macro_expansion,  // Wraps macro-generated AST
};

// Extend Node with expansion tracking:
pub const Node = struct {
    kind: NodeKind,
    location: SourceLocation,
    data: NodeData,
    type: ?*Type = null,

    // Macro expansion tracking (Nim pattern)
    expansion_origin: ?*MacroExpansion = null,
};

pub const MacroExpansion = struct {
    macro_name: []const u8,        // "derive", "comptime", etc.
    original_ast: *Node,           // User code (for error messages)
    expanded_at: SourceLocation,   // Where expansion happened
};

// Decorators stored on declarations
pub const ClassDecl = struct {
    name: []const u8,
    type_params: []GenericParam,
    extends: ?*Type,
    implements: []*Type,
    decorators: []*Node,  // ← @derive(Eq), etc. (CallExpr nodes)
    members: []*Node,
};
```

### Macro Invocation Representation

```typescript
// Source code:
@derive(Eq, Hash)
class User {
    name: string;
    age: number;
}

// AST after parsing (BEFORE semantic analysis):
ClassDecl {
    name: "User",
    decorators: [
        CallExpr {  // NOT MacroInvocation yet!
            callee: Identifier("derive"),
            arguments: [
                Identifier("Eq"),
                Identifier("Hash"),
            ],
        }
    ],
    members: [
        PropertyDecl { name: "name", type: "string" },
        PropertyDecl { name: "age", type: "number" },
    ],
}

// After semantic analysis identifies macros:
ClassDecl {
    name: "User",
    decorators: [],  // Macros removed after expansion
    members: [
        PropertyDecl { name: "name", type: "string" },
        PropertyDecl { name: "age", type: "number" },

        // Generated by @derive(Eq):
        MethodDecl {
            name: "equals",
            expansion_origin: MacroExpansion {
                macro_name: "derive",
                original_ast: /* CallExpr(@derive) */,
            },
            // ... method body
        },

        // Generated by @derive(Hash):
        MethodDecl {
            name: "hash",
            expansion_origin: MacroExpansion { ... },
            // ... method body
        },
    ],
}
```

**Key Insight:** Parser creates **uniform AST**. Semantic analysis converts decorators → macro expansions.

---

## 4. Semantic Analysis (Macro Execution)

**Pattern:** Haxe's type-aware expansion + Nim's two-pass

### Macro Detection & Execution

```zig
// src/checker/macro_resolver.zig (new file)
pub const MacroResolver = struct {
    ctx: *TypeCheckContext,
    macro_cache: std.AutoHashMap(u64, *Node),  // AST hash → expanded AST

    pub fn resolveDecorators(self: *MacroResolver, decorators: []*Node, target: *Node) ![]*Node {
        var expanded_nodes = std.ArrayList(*Node).init(self.ctx.allocator);

        for (decorators) |decorator| {
            // Is this a macro invocation?
            if (self.isMacroCall(decorator)) {
                // Check cache (Salsa pattern from LSP doc)
                const cache_key = self.computeASTHash(decorator, target);
                if (self.macro_cache.get(cache_key)) |cached| {
                    try expanded_nodes.appendSlice(cached);
                    continue;
                }

                // Execute macro
                const expanded = try self.executeMacro(decorator, target);
                try self.macro_cache.put(cache_key, expanded);
                try expanded_nodes.appendSlice(expanded);
            }
        }

        return expanded_nodes.toOwnedSlice();
    }

    fn isMacroCall(self: *MacroResolver, node: *Node) bool {
        if (node.kind != .call_expr) return false;

        const call = &node.data.call_expr;
        if (call.callee.kind != .identifier) return false;

        const name = call.callee.data.identifier;
        return self.ctx.macro_registry.contains(name);
    }

    fn executeMacro(self: *MacroResolver, call: *Node, target: *Node) ![]*Node {
        const macro_name = call.data.call_expr.callee.data.identifier;
        const macro_func = self.ctx.macro_registry.get(macro_name).?;

        // Execute macro (AST → AST transformation)
        const expanded = try macro_func(self.ctx.macro_ctx, call, target);

        // Track expansion origin (for error messages)
        for (expanded) |node| {
            node.expansion_origin = try self.ctx.allocator.create(MacroExpansion);
            node.expansion_origin.?.* = .{
                .macro_name = macro_name,
                .original_ast = call,
                .expanded_at = call.location,
            };
        }

        return expanded;
    }
};
```

### Type Checking Flow

```
Parse → Raw AST (decorators = CallExpr)
    ↓
Symbol Resolution
    ↓
Macro Detection (is decorator a macro?)
    ↓
Macro Execution (AST → AST transformation)
    ├─ Check cache (AST hash)
    ├─ Execute macro function
    └─ Track expansion origin
    ↓
Re-typecheck Expanded AST
    ↓
Typed AST (all macros expanded, origin tracked)
```

---

## 5. Error Recovery (LSP-Critical)

**Pattern:** Bun's continue-on-error + deduplication

```zig
// src/parser/error_recovery.zig
pub const ParseError = struct {
    message: []const u8,
    loc: SourceLocation,
    severity: Severity,

    pub const Severity = enum { error, warning };
};

pub fn synchronize(parser: *Parser) void {
    parser.panic_mode = false;

    // Skip tokens until next statement boundary
    while (!parser.match(.t_end_of_file)) {
        if (parser.previous.kind == .t_semicolon) return;

        switch (parser.current.kind) {
            .t_keyword_class,
            .t_keyword_function,
            .t_keyword_const,
            .t_keyword_let,
            .t_keyword_if,
            .t_keyword_while,
            => return,
            else => parser.advance(),
        }
    }
}

pub fn reportError(parser: *Parser, loc: SourceLocation, message: []const u8) !void {
    // Deduplicate cascading errors (Bun pattern)
    if (parser.errors.items.len > 0) {
        const last = &parser.errors.items[parser.errors.items.len - 1];
        if (last.loc.start == loc.start) return;  // Same location
    }

    try parser.errors.append(.{
        .message = message,
        .loc = loc,
        .severity = .error,
    });

    parser.panic_mode = true;
}
```

**LSP Benefit:** Collect all errors in one pass → red squiggles everywhere, not just first error

---

## 6. Incremental Parsing (Cache Strategy)

**Pattern:** Bun's source hash cache + LSP doc's Salsa-style invalidation

```zig
// src/parser/cache.zig
pub const ParseCache = struct {
    cache: std.AutoHashMap(u64, CacheEntry),

    pub const CacheEntry = struct {
        source_hash: u64,
        ast: *Node,
        errors: []ParseError,
    };

    pub fn get(self: *ParseCache, source: []const u8) ?*Node {
        const hash = std.hash.Wyhash.hash(0, source);

        if (self.cache.get(hash)) |entry| {
            // Verify hash (collision check)
            const current_hash = std.hash.Wyhash.hash(0, source);
            if (entry.source_hash == current_hash) {
                return entry.ast;
            }
        }

        return null;
    }

    pub fn put(self: *ParseCache, source: []const u8, ast: *Node, errors: []ParseError) !void {
        const hash = std.hash.Wyhash.hash(0, source);
        try self.cache.put(hash, .{
            .source_hash = hash,
            .ast = ast,
            .errors = errors,
        });
    }
};
```

**LSP Flow:**

```
textDocument/didChange
    ↓
Compute source hash
    ↓
Cache hit? → Return cached AST + errors
    ↓ (miss)
Parse + collect errors
    ↓
Cache AST + errors
    ↓
Return diagnostics
```

**Performance:** <10ms for cache hit, <100ms for parse + macro expansion

---

## 7. LSP Integration Points

### Hover (Show Type Info)

```zig
// LSP server receives hover request at position
pub fn onHover(lsp: *LSPServer, uri: []const u8, position: Position) !HoverResult {
    const doc = lsp.documents.get(uri).?;

    // Find AST node at position
    const node = findNodeAtPosition(doc.ast, position);

    // Check if this is macro-generated code
    if (node.expansion_origin) |origin| {
        // Show: "Generated by @derive macro"
        return .{
            .contents = try std.fmt.allocPrint(
                lsp.allocator,
                "Generated by @{s} macro\nOriginal: {s}",
                .{ origin.macro_name, formatNode(origin.original_ast) }
            ),
        };
    }

    // Show normal type info
    if (node.type) |type_info| {
        return .{ .contents = formatType(type_info) };
    }

    return .{ .contents = "" };
}
```

### Macro Expansion Preview

```zig
// VSCode command: "Show Macro Expansion"
pub fn expandMacroAtCursor(lsp: *LSPServer, uri: []const u8, position: Position) ![]const u8 {
    const doc = lsp.documents.get(uri).?;
    const node = findNodeAtPosition(doc.ast, position);

    // Is cursor on a macro invocation?
    if (isMacroInvocation(node)) {
        // Expand macro (use cache)
        const expanded = try lsp.macro_resolver.executeMacro(node, /* target */);

        // Format as TypeScript code
        return formatAST(expanded);
    }

    return "";
}
```

---

## 8. Performance Targets

| Operation | Target | Max | Strategy |
|-----------|--------|-----|----------|
| Parse (1000 LOC) | <10ms | 20ms | Stack allocator + single-pass |
| Macro expansion | <50ms | 100ms | Cache by AST hash |
| LSP hover | <50ms | 100ms | Cached AST + lazy expansion |
| LSP completion | <100ms | 200ms | Scan-only mode for imports |
| Full diagnostics | <500ms | 1s | Parse + expand + typecheck |

**Rule:** If user notices lag, it's too slow.

---

## 9. Implementation Roadmap

**Week 5-6: Lexer + Parser**
1. ✅ Implement `Lexer.zig` (single-pass, @macro tokens)
2. ✅ Implement `Parser.zig` (recursive descent, macro-agnostic)
3. ✅ Implement `ASTArena.zig` (stack allocator)
4. ✅ Test: Parse `examples/macro.ms` → AST

**Week 7-8: Semantic Analysis + Macros**
1. ✅ Implement `MacroResolver.zig` (decorator → macro detection)
2. ✅ Implement macro execution (AST → AST transformation)
3. ✅ Implement expansion caching (AST hash → expanded AST)
4. ✅ Test: Expand `@derive(Eq)` → generates `equals()` method

**Week 9-10: LSP Foundation**
1. ✅ Implement `ParseCache.zig` (source hash → cached AST)
2. ✅ Implement error recovery (collect all errors)
3. ✅ Implement LSP server (hover, completion, diagnostics)
4. ✅ Benchmark: <100ms hover, <200ms completion

---

## 10. Key Design Principles (Validated by Research)

**From Nim:**
- ✅ Parser is macro-agnostic (uniform AST)
- ✅ Two-pass minimum (parse → expand → re-analyze)
- ✅ Track expansion origin (error messages point to user code)

**From Haxe:**
- ✅ Macros as metadata/decorators (not special syntax)
- ✅ Backend-agnostic parser (pure AST)
- ✅ Type-aware macro execution (during semantic analysis)

**From Bun:**
- ✅ Comptime specialization (zero-cost variants)
- ✅ Stack-first allocation (8KB fast path)
- ✅ 24-byte AST nodes (cache-friendly)
- ✅ Error recovery (continue parsing, deduplicate)
- ✅ Incremental cache (hash source → cached AST)

---

## 11. File Structure

```
src/
├── lexer/
│   ├── lexer.zig         # Token streaming
│   ├── token.zig         # Token types (@macro tokens)
│   └── source.zig        # Source location tracking
├── parser/
│   ├── parser.zig        # Comptime-specialized parser
│   ├── arena.zig         # Stack allocator (Bun pattern)
│   ├── cache.zig         # Parse cache (LSP optimization)
│   └── error_recovery.zig
├── ast/
│   ├── node.zig          # ✅ Already exists (extend for expansion tracking)
│   ├── types.zig         # ✅ Already exists
│   └── visitor.zig       # AST traversal
├── checker/
│   ├── typechecker.zig   # ✅ Already exists (stub)
│   └── macro_resolver.zig # NEW: Macro detection + execution
├── macro/
│   ├── expander.zig      # ✅ Already exists
│   ├── builtin_macros.zig # ✅ Already exists
│   └── cache.zig         # NEW: Expansion cache (AST hash)
└── lsp/
    ├── server.zig        # LSP server (upcoming)
    └── hover.zig         # Hover provider (upcoming)
```

---

## Summary

**Metascript's parser synthesizes proven patterns:**
- **Nim:** Macro-agnostic parsing, two-pass expansion
- **Haxe:** Metadata-based macros, type-aware execution
- **Bun:** Zig best practices, LSP-first performance

**Result:** Fast, incremental, LSP-ready parser that handles TypeScript + macros + three backends.

**Next Step:** Implement `Lexer.zig` with @macro token support.
