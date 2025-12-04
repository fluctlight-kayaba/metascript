# Source-Defined Macros (Nim-Style)

**Goal:** Macros as normal functions in `.ms` source code, executed at compile-time to produce AST

---

## Syntax Design

**The Rule:**
| Action | Syntax | Example |
|--------|--------|---------|
| **Define** | `macro` keyword (no @) | `macro derive(ctx) { }` |
| **Use** | Always `@` prefix | `@derive(Eq)`, `@comptime { }` |

**Rationale:** Nim's clean `macro` keyword for definitions + TypeScript's `@` familiarity for usage.

---

## Overview

Like Nim, macros use the `macro` keyword. They:
1. Live in normal `.ms` source files
2. Get parsed/cached by Trans-Am engine
3. Execute at compile-time to generate AST
4. Integrate with LSP for real-time feedback

```typescript
// macros.ms - macro definitions using `macro` keyword
macro derive(ctx: MacroContext) {
    for trait in ctx.traits {
        if trait == "Eq" {
            // quote block generates AST at compile-time
            quote {
                equals(other: ${ctx.target.name}): boolean {
                    return ${generateFieldComparison(ctx.target.fields)};
                }
            }
        }
    }
}

// user.ms - macro usage (always @ prefix)
@derive(Eq, Hash)
class User {
    name: string;
    age: number;
}
```

---

## Architecture: The Complete Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        PHASE 1: DISCOVERY & PARSING                         │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  Source Files (.ms)                                                         │
│  ┌──────────────┐     ┌──────────────┐     ┌──────────────┐                │
│  │ macros.ms    │     │ utils.ms     │     │ user.ms      │                │
│  │              │     │              │     │              │                │
│  │ macro        │     │ function     │     │ @derive(Eq)  │                │
│  │ derive(ctx)  │     │ helper()     │     │ class User   │                │
│  │ { ... }      │     │ { ... }      │     │ { ... }      │                │
│  └──────┬───────┘     └──────────────┘     └──────┬───────┘                │
│         │                                         │                         │
│         ▼                                         ▼                         │
│  ┌──────────────────────────────────────────────────────────────┐          │
│  │                      LEXER + PARSER                          │          │
│  │  - Tokenize all source files                                 │          │
│  │  - Build AST for each file                                   │          │
│  │  - Identify `macro` keyword declarations                     │          │
│  │  - Identify @macroName usage sites                           │          │
│  └──────────────────────────────────────────────────────────────┘          │
│                                │                                            │
│                                ▼                                            │
│  ┌──────────────────────────────────────────────────────────────┐          │
│  │                   MACRO REGISTRY (Trans-Am Cache)            │          │
│  │                                                              │          │
│  │  MacroDefinitions:                                           │          │
│  │  ┌─────────────────────────────────────────────────────┐    │          │
│  │  │ "derive" => {                                       │    │          │
│  │  │   file_id: 1,                                       │    │          │
│  │  │   node: *FunctionDecl,                              │    │          │
│  │  │   bytecode: ?[]u8,     // Cached HBC                │    │          │
│  │  │   js_source: ?[]u8,    // Transpiled JS             │    │          │
│  │  │   hash: u64,           // For invalidation          │    │          │
│  │  │ }                                                   │    │          │
│  │  └─────────────────────────────────────────────────────┘    │          │
│  │                                                              │          │
│  │  MacroUsages:                                                │          │
│  │  ┌─────────────────────────────────────────────────────┐    │          │
│  │  │ file:2, line:1 => { macro: "derive", args: [Eq,Hash]}│    │          │
│  │  └─────────────────────────────────────────────────────┘    │          │
│  └──────────────────────────────────────────────────────────────┘          │
└─────────────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                    PHASE 2: MACRO COMPILATION (LAZY)                        │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  When macro is first used (or definition changes):                          │
│                                                                             │
│  ┌────────────────┐    ┌────────────────┐    ┌────────────────┐            │
│  │ Macro AST      │───▶│ Metascript     │───▶│ JavaScript     │            │
│  │ (Zig *Node)    │    │ Printer        │    │ Source         │            │
│  │                │    │                │    │                │            │
│  │ macro          │    │ macro          │    │ function       │            │
│  │ derive(ctx)    │    │ derive(ctx)    │    │ derive(ctx)    │            │
│  │ { ... }        │    │ { ... }        │    │ { ... }        │            │
│  └────────────────┘    └────────────────┘    └────────────────┘            │
│                                                     │                       │
│                                                     ▼                       │
│                        ┌────────────────┐    ┌────────────────┐            │
│                        │ Hermes         │◀───│ esbuild        │            │
│                        │ Bytecode       │    │ (TS→JS)        │            │
│                        │ (.hbc)         │    │                │            │
│                        │                │    │ Optional:      │            │
│                        │ Cached in      │    │ Macro body     │            │
│                        │ Trans-Am       │    │ may use TS     │            │
│                        └────────────────┘    └────────────────┘            │
│                                                                             │
│  Cache Key: hash(macro_source + dependencies)                               │
│  Cache Location: .msc-cache/macros/<hash>.hbc                               │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                      PHASE 3: MACRO EXECUTION                               │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  For each @macroName usage site:                                            │
│                                                                             │
│  ┌──────────────────────────────────────────────────────────────┐          │
│  │                    HERMES VM SANDBOX                         │          │
│  │                                                              │          │
│  │  Globals Injected:                                           │          │
│  │  ┌──────────────────────────────────────────────────────┐   │          │
│  │  │ target: {                                            │   │          │
│  │  │   name: "User",                                      │   │          │
│  │  │   kind: "class",                                     │   │          │
│  │  │   properties: ["name", "age"],                       │   │          │
│  │  │   methods: [],                                       │   │          │
│  │  │   addMethod(node): void,                             │   │          │
│  │  │   addProperty(node): void,                           │   │          │
│  │  │ }                                                    │   │          │
│  │  │                                                      │   │          │
│  │  │ ast: {                                               │   │          │
│  │  │   createMethod(name, body): Node,                    │   │          │
│  │  │   createBinaryExpr(op, left, right): Node,           │   │          │
│  │  │   createIdentifier(name): Node,                      │   │          │
│  │  │   createMemberExpr(obj, prop): Node,                 │   │          │
│  │  │   createBlock(statements): Node,                     │   │          │
│  │  │   createReturnStmt(expr): Node,                      │   │          │
│  │  │   // ... more AST builders                           │   │          │
│  │  │ }                                                    │   │          │
│  │  │                                                      │   │          │
│  │  │ traits: ["Eq", "Hash"]   // From @derive(Eq, Hash)   │   │          │
│  │  │ console: { log: ... }    // For debugging            │   │          │
│  │  └──────────────────────────────────────────────────────┘   │          │
│  │                                                              │          │
│  │  Execute: bytecode or source                                 │          │
│  │  Time: ~0.003ms (bytecode) or ~0.05ms (source)              │          │
│  │                                                              │          │
│  └──────────────────────────────────────────────────────────────┘          │
│                                │                                            │
│                                ▼                                            │
│  ┌──────────────────────────────────────────────────────────────┐          │
│  │                  MODIFIED AST                                │          │
│  │                                                              │          │
│  │  class User {                                                │          │
│  │      name: string;                                           │          │
│  │      age: number;                                            │          │
│  │      equals(other: User): boolean { ... }  // Generated!     │          │
│  │      hashCode(): number { ... }            // Generated!     │          │
│  │  }                                                           │          │
│  └──────────────────────────────────────────────────────────────┘          │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Phase 1: Macro Discovery

### 1.1 Syntax: `macro` Keyword (FINAL DESIGN)

```typescript
// FINAL: Nim-style keyword for definition, @ for usage
macro derive(ctx: MacroContext) {
    for trait in ctx.traits {
        if trait == "Eq" {
            quote {
                equals(other: ${ctx.target.name}): boolean {
                    return ${generateFieldComparison(ctx.target.fields)};
                }
            }
        }
    }
}

// Usage always uses @ prefix
@derive(Eq, Hash)
class User {
    name: string;
    age: number;
}

// @comptime for inline compile-time blocks
const version = @comptime {
    exec("git rev-parse --short HEAD")
};
```

**Design Rationale:**
- `macro` keyword (no @) for definitions - clean, Nim-inspired
- `@name(...)` for usage - familiar TypeScript decorator syntax
- `@comptime { }` for inline compile-time blocks - consistent with usage pattern

### 1.2 Parser Changes

```zig
// src/parser/parser.zig

fn parseDeclaration(self: *Parser) !*Node {
    // Check for `macro` keyword (Nim-style, no @ prefix)
    if (self.check(.keyword_macro)) {
        return self.parseMacroDefinition();
    }

    // ... existing declaration parsing
}

fn parseMacroDefinition(self: *Parser) !*Node {
    _ = try self.consume(.keyword_macro, "Expected 'macro' keyword");

    // Parse macro name
    const name = try self.consume(.identifier, "Expected macro name");

    // Parse parameters
    _ = try self.consume(.left_paren, "Expected '(' after macro name");
    const params = try self.parseParameterList();
    _ = try self.consume(.right_paren, "Expected ')' after parameters");

    // Parse body (may contain `quote { }` blocks)
    const body = try self.parseBlock();

    return self.arena.createNode(.macro_decl, name.loc, .{
        .macro_decl = .{
            .name = name.lexeme,
            .params = params,
            .body = body,
        },
    });
}

fn parseQuoteBlock(self: *Parser) !*Node {
    // Parse `quote { ... }` - AST quotation
    _ = try self.consume(.keyword_quote, "Expected 'quote' keyword");
    _ = try self.consume(.left_brace, "Expected '{' after quote");

    // Parse quoted code (becomes AST at compile-time)
    const quoted_stmts = try self.parseStatementList();

    _ = try self.consume(.right_brace, "Expected '}' to close quote block");

    return self.arena.createNode(.quote_expr, self.previous().loc, .{
        .quote_expr = .{ .body = quoted_stmts },
    });
}
```

### 1.3 Macro Registry

```zig
// src/macro/registry.zig

pub const MacroRegistry = struct {
    allocator: std.mem.Allocator,
    definitions: std.StringHashMap(MacroDefinition),
    usages: std.ArrayList(MacroUsage),

    pub const MacroDefinition = struct {
        name: []const u8,
        file_id: FileId,
        node: *ast.Node,           // The @macro function AST
        source_hash: u64,          // For cache invalidation
        bytecode: ?[]const u8,     // Compiled HBC (cached)
        js_source: ?[]const u8,    // Transpiled JS (cached)
        last_compiled: i64,        // Timestamp
    };

    pub const MacroUsage = struct {
        file_id: FileId,
        location: SourceLocation,
        macro_name: []const u8,
        arguments: []*ast.Node,
        target_node: *ast.Node,    // The decorated class/function
    };

    /// Scan AST for macro definitions and usages
    pub fn scan(self: *MacroRegistry, file_id: FileId, program: *ast.Node) !void {
        for (program.data.program.statements) |stmt| {
            switch (stmt.kind) {
                .macro_decl => {
                    try self.registerDefinition(file_id, stmt);
                },
                .class_decl, .function_decl => {
                    // Check for decorator usages
                    const decorators = self.getDecorators(stmt);
                    for (decorators) |dec| {
                        if (self.definitions.contains(dec.name)) {
                            try self.registerUsage(file_id, dec, stmt);
                        }
                    }
                },
                else => {},
            }
        }
    }

    /// Compile macro to bytecode (lazy, cached)
    pub fn compile(self: *MacroRegistry, name: []const u8) ![]const u8 {
        const def = self.definitions.get(name) orelse return error.UnknownMacro;

        // Check cache
        if (def.bytecode) |bc| {
            return bc;
        }

        // Compile: AST → Metascript → JS → HBC
        const ms_source = try self.astToSource(def.node);
        const js_source = try transpileToJS(ms_source);
        const bytecode = try compileToHBC(js_source);

        // Cache
        def.bytecode = bytecode;
        def.js_source = js_source;

        return bytecode;
    }
};
```

---

## Phase 2: LSP Integration

### 2.1 Macro-Aware Query Database

```zig
// src/lsp/database.zig - Extension for macros

pub const QueryDatabase = struct {
    // ... existing fields ...

    // NEW: Macro-specific caches
    macro_registry: MacroRegistry,
    macro_expansion_cache: LRUCache(MacroCallId, *ast.Node, 512),
    macro_bytecode_cache: LRUCache(MacroDefId, []const u8, 64),

    /// Query: Get expanded AST for a macro call site
    pub fn macroExpand(self: *QueryDatabase, call_id: MacroCallId) !*ast.Node {
        // Check cache
        if (self.macro_expansion_cache.get(call_id)) |cached| {
            return cached;
        }

        // Get macro definition
        const usage = self.macro_registry.getUsage(call_id);
        const bytecode = try self.macroBytecode(usage.macro_name);

        // Execute in VM
        const expanded = try self.vm.executeMacro(bytecode, usage.target_node, usage.arguments);

        // Cache
        try self.macro_expansion_cache.put(call_id, expanded);

        return expanded;
    }

    /// Query: Get compiled bytecode for macro definition (cached)
    pub fn macroBytecode(self: *QueryDatabase, name: []const u8) ![]const u8 {
        const def_id = try self.macro_registry.getDefId(name);

        if (self.macro_bytecode_cache.get(def_id)) |cached| {
            return cached;
        }

        const bytecode = try self.macro_registry.compile(name);
        try self.macro_bytecode_cache.put(def_id, bytecode);

        return bytecode;
    }
};
```

### 2.2 LSP Features for Macros

```zig
// src/lsp/handlers.zig

/// textDocument/hover - Show macro expansion preview
fn handleHover(db: *QueryDatabase, params: HoverParams) !?Hover {
    const file_id = db.getFileId(params.textDocument.uri);
    const pos = params.position;

    // Check if hovering over a macro usage
    if (db.macro_registry.getUsageAt(file_id, pos)) |usage| {
        // Get expanded AST
        const expanded = try db.macroExpand(usage.call_id);

        // Pretty-print to show preview
        const preview = try db.printer.print(expanded);

        return Hover{
            .contents = .{
                .kind = .markdown,
                .value = std.fmt.allocPrint(db.allocator,
                    \\**@{s}** macro expansion:
                    \\```typescript
                    \\{s}
                    \\```
                , .{ usage.macro_name, preview }),
            },
        };
    }

    // Check if hovering over a macro definition
    if (db.macro_registry.getDefinitionAt(file_id, pos)) |def| {
        return Hover{
            .contents = .{
                .kind = .markdown,
                .value = std.fmt.allocPrint(db.allocator,
                    \\**Macro definition:** @{s}
                    \\
                    \\Used in {d} places.
                , .{ def.name, def.usage_count }),
            },
        };
    }

    // ... existing hover logic
}

/// textDocument/definition - Go to macro definition
fn handleGotoDefinition(db: *QueryDatabase, params: DefinitionParams) !?Location {
    const file_id = db.getFileId(params.textDocument.uri);
    const pos = params.position;

    // If on @macroName, go to macro definition
    if (db.macro_registry.getUsageAt(file_id, pos)) |usage| {
        const def = db.macro_registry.getDefinition(usage.macro_name);
        return Location{
            .uri = db.getUri(def.file_id),
            .range = def.node.loc.toRange(),
        };
    }

    // ... existing definition logic
}

/// textDocument/completion - Suggest available macros
fn handleCompletion(db: *QueryDatabase, params: CompletionParams) ![]CompletionItem {
    const file_id = db.getFileId(params.textDocument.uri);
    const pos = params.position;

    // If typing @, suggest available macros
    if (db.isTypingDecorator(file_id, pos)) {
        var items = std.ArrayList(CompletionItem).init(db.allocator);

        var it = db.macro_registry.definitions.iterator();
        while (it.next()) |entry| {
            try items.append(.{
                .label = entry.key_ptr.*,
                .kind = .Function,
                .detail = "macro",
                .insertText = entry.key_ptr.*,
            });
        }

        return items.toOwnedSlice();
    }

    // ... existing completion logic
}
```

### 2.3 Semantic Highlighting for Macros

```zig
// src/lsp/semantic_tokens.zig

pub const SemanticTokenType = enum {
    // ... existing types ...
    macro,           // `macro` keyword definitions
    quote,           // `quote` blocks
    macroUsage,      // @derive, @comptime, etc.
    macroParameter,  // Parameters inside macro body
    comptimeCode,    // Code inside macro/quote body (different color)
};

fn computeSemanticTokens(db: *QueryDatabase, file_id: FileId) ![]SemanticToken {
    var tokens = std.ArrayList(SemanticToken).init(db.allocator);

    const ast = try db.parse(file_id);

    // Walk AST
    var walker = ASTWalker.init(ast);
    while (walker.next()) |node| {
        switch (node.kind) {
            .macro_decl => {
                // Highlight function name as macro
                try tokens.append(.{
                    .line = node.loc.start.line,
                    .col = node.data.macro_decl.name_loc.col,
                    .length = node.data.macro_decl.name.len,
                    .type = .macro,
                    .modifiers = .{ .declaration = true },
                });

                // Highlight macro body differently (compile-time context)
                try self.highlightMacroBody(&tokens, node.data.macro_decl.body);
            },
            .decorator => {
                // Check if it's a macro usage
                if (db.macro_registry.definitions.contains(node.data.decorator.name)) {
                    try tokens.append(.{
                        .line = node.loc.start.line,
                        .col = node.loc.start.col,
                        .length = node.data.decorator.name.len + 1, // +1 for @
                        .type = .macroUsage,
                        .modifiers = .{},
                    });
                }
            },
            else => {},
        }
    }

    return tokens.toOwnedSlice();
}

/// Highlight code inside macro body with different style
fn highlightMacroBody(self: *SemanticTokenizer, tokens: *std.ArrayList(SemanticToken), body: *ast.Node) !void {
    // All code inside macro body gets a "comptime" modifier
    // This allows different coloring in the theme

    var walker = ASTWalker.init(body);
    while (walker.next()) |node| {
        switch (node.kind) {
            .identifier => {
                try tokens.append(.{
                    .line = node.loc.start.line,
                    .col = node.loc.start.col,
                    .length = node.data.identifier.len,
                    .type = .variable,
                    .modifiers = .{ .comptime = true },  // Special modifier!
                });
            },
            // ... etc
        }
    }
}
```

---

## Phase 3: Cache Invalidation

### 3.1 Dependency Graph

```
┌──────────────────────────────────────────────────────────────────┐
│                    DEPENDENCY GRAPH                              │
│                                                                  │
│  macros.ms                                                       │
│  ┌─────────────────┐                                            │
│  │ macro derive()  │─────────────────┐                          │
│  └─────────────────┘                 │                          │
│           │                          │                          │
│           │ defines                  │ defines                  │
│           ▼                          ▼                          │
│  ┌─────────────────┐      ┌─────────────────┐                   │
│  │ "derive" macro  │      │ "serialize"     │                   │
│  │ bytecode cache  │      │ macro bytecode  │                   │
│  └────────┬────────┘      └────────┬────────┘                   │
│           │                        │                            │
│           │ used by                │ used by                    │
│           ▼                        ▼                            │
│  ┌─────────────────┐      ┌─────────────────┐                   │
│  │ user.ms:1       │      │ product.ms:1    │                   │
│  │ @derive(Eq)     │      │ @serialize()    │                   │
│  │ class User      │      │ class Product   │                   │
│  └─────────────────┘      └─────────────────┘                   │
│                                                                  │
│  INVALIDATION RULES:                                             │
│  1. Change macros.ms → invalidate "derive" bytecode              │
│  2. Invalidate bytecode → invalidate all usages                  │
│  3. Change @derive args → invalidate only that usage             │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

### 3.2 Smart Invalidation

```zig
// src/macro/invalidation.zig

pub fn invalidateMacroDefinition(db: *QueryDatabase, macro_name: []const u8) !void {
    // 1. Clear bytecode cache
    const def_id = db.macro_registry.getDefId(macro_name);
    db.macro_bytecode_cache.remove(def_id);

    // 2. Clear all expansion caches that use this macro
    const usages = db.macro_registry.getUsagesOf(macro_name);
    for (usages) |usage| {
        db.macro_expansion_cache.remove(usage.call_id);
    }

    // 3. Mark dependent files dirty (for diagnostics)
    for (usages) |usage| {
        try db.dirty_files.put(usage.file_id, {});
    }
}

pub fn invalidateMacroUsage(db: *QueryDatabase, call_id: MacroCallId) !void {
    // Only invalidate this specific expansion
    db.macro_expansion_cache.remove(call_id);
}
```

---

## Phase 4: Tree-sitter Integration (Level 1 Highlighting)

### 4.1 Grammar Extension

```javascript
// tree-sitter-metascript/grammar.js

module.exports = grammar({
  name: 'metascript',

  rules: {
    // ... existing rules ...

    // Macro definition (keyword-based, no @)
    macro_definition: $ => seq(
      'macro',
      $.identifier,
      $.parameter_list,
      $.block
    ),

    // Quote block for AST generation
    quote_block: $ => seq(
      'quote',
      $.block
    ),

    // Decorator (including macro usage)
    decorator: $ => seq(
      '@',
      $.identifier,
      optional($.arguments)
    ),

    // Decorated declaration
    decorated_declaration: $ => seq(
      repeat1($.decorator),
      choice(
        $.class_declaration,
        $.function_declaration
      )
    ),
  }
});
```

### 4.2 Highlight Queries

```scm
; tree-sitter-metascript/queries/highlights.scm

; Macro definition (keyword-based)
(macro_definition
  "macro" @keyword.macro
  (identifier) @function.macro)

; Quote block
(quote_block
  "quote" @keyword.quote)

; Macro usage (decorator)
(decorator
  "@" @punctuation.special
  (identifier) @attribute)

; Special highlighting for known macros
((decorator
  "@" @punctuation.special
  (identifier) @attribute.builtin)
 (#match? @attribute.builtin "^(derive|comptime|inline|serialize)$"))
```

---

## Phase 5: Dynamic Highlighting (Level 2 - LSP Semantic Tokens)

Tree-sitter provides static highlighting. LSP semantic tokens provide **dynamic** highlighting based on:

1. **Macro expansion state** - Is this macro successfully expanded?
2. **Macro body context** - Code inside `macro` functions and `quote` blocks is compile-time
3. **Generated code preview** - Inline hints showing what's generated

### 5.1 VSCode Theme Extension

```json
// metascript-theme/package.json
{
  "contributes": {
    "semanticTokenTypes": [
      { "id": "macro", "description": "Macro function definition" },
      { "id": "macroUsage", "description": "Macro invocation" },
      { "id": "comptimeCode", "description": "Compile-time executed code" }
    ],
    "semanticTokenModifiers": [
      { "id": "comptime", "description": "Compile-time context" }
    ],
    "semanticTokenScopes": {
      "macro": ["entity.name.function.macro.metascript"],
      "macroUsage": ["entity.name.tag.macro.metascript"],
      "comptimeCode": ["meta.comptime.metascript"]
    }
  }
}
```

```json
// Theme colors
{
  "semanticTokenColors": {
    "macro": { "foreground": "#C586C0", "fontStyle": "bold" },
    "macroUsage": { "foreground": "#DCDCAA", "fontStyle": "italic" },
    "*.comptime": { "foreground": "#9CDCFE", "fontStyle": "italic" }
  }
}
```

---

## CRITICAL: LSP on Expanded AST (The Holy Grail)

**The core insight:** LSP must query the EXPANDED AST, not the source AST. Otherwise, macro-generated members are invisible to autocomplete/hover/go-to-definition.

### The Problem

```typescript
// User writes this:
@derive(Eq)
class User {
    name: string;
    age: number;
}

const user = new User();
user.█  // ← Cursor here, what completions appear?
```

**Without expanded AST:** Only `name`, `age` appear.
**With expanded AST:** `name`, `age`, AND `equals()` appear! (generated by `@derive(Eq)`)

### The Haxe Inspiration: Network-Fetched Completions

```typescript
// Macro fetches OpenAPI spec at COMPILE-TIME and generates typed client
const api = @comptime {
    const schema = fetch("https://api.example.com/openapi.json");
    return generateTypedClient(schema);
};

api.█  // ← LSP shows: getUsers(), getUserById(), createUser()
       //   ALL FROM NETWORK DATA FETCHED AT COMPILE-TIME!
```

**This is the holy grail of metaprogramming DX.** Compile-time code can do I/O, and the results flow into LSP completions.

### Architecture Required

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        LSP QUERIES EXPANDED AST                              │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  1. User types: user.█                                                      │
│                    │                                                        │
│                    ▼                                                        │
│  2. LSP sends completion request                                            │
│                    │                                                        │
│                    ▼                                                        │
│  3. Trans-Am: "Do I have expanded AST for User class?"                      │
│                    │                                                        │
│         ┌─────────┴─────────┐                                               │
│         │ Cache HIT         │ Cache MISS                                    │
│         │ (instant!)        │    │                                          │
│         │                   │    ▼                                          │
│         │              4. Execute @derive(Eq) via Hermes VM                 │
│         │                   │    │                                          │
│         │                   │    ▼ (can do network I/O!)                    │
│         │              5. Produces expanded AST with equals()               │
│         │                   │    │                                          │
│         │                   │    ▼                                          │
│         │              6. Cache in Trans-Am                                 │
│         │                   │                                               │
│         └─────────┬─────────┘                                               │
│                   │                                                         │
│                   ▼                                                         │
│  7. TypeChecker runs on EXPANDED AST                                        │
│                   │                                                         │
│                   ▼                                                         │
│  8. Return completions: [name, age, equals]                                 │
│                                                                             │
│  CRITICAL: Steps 4-6 are cached! Only re-run when:                          │
│  - Macro source (derive.ms) changes                                         │
│  - Target class (User) changes                                              │
│  - Macro arguments change                                                   │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Implementation Checklist

| Feature | Status | Notes |
|---------|--------|-------|
| Trans-Am macro cache (L2) | ✅ Done | `macro_output_cache: MacroOutputCache` |
| Hermes VM execution | ✅ Done | `macro_vm.zig` |
| `getMacroExpansionHover` | 🚧 Partial | Shows preview but doesn't feed TypeChecker |
| TypeChecker on expanded AST | ❌ Missing | **CRITICAL** |
| Completions from expanded | ❌ Missing | **CRITICAL** |
| Go-to-definition for generated | ❌ Missing | Should jump to macro source |
| Network I/O in @comptime | ❌ Missing | `fetch()` in Hermes sandbox |
| File I/O in @comptime | ❌ Missing | `readFile()` in Hermes sandbox |

### Cache & Safety (Automatic, No Configuration)

**5-Level Hierarchy (Always On):**
| Level | Name | Status | Timeout |
|-------|------|--------|---------|
| L1 | Bytecode | ✅ Done | N/A (disk) |
| L2 | Macro Output | ✅ Done | 100ms |
| L3 | Types | 🚧 Partial | 200ms |
| L4 | Completions | ❌ Planned | 50ms |
| L5 | Network | ❌ Planned | 5s (first only) |

**Self-Optimizing System:**
- Slow macro (>timeout)? Warned once, excluded from LSP cache thereafter
- User fixes macro or lives without LSP support for that macro
- Full compile still runs all macros (correctness > speed)
- LRU eviction (2000 entries) prevents OOM - no config needed

See: `docs/lsp-architecture.md` "Timeout Bailout" section.

### Shift+K (Hover) Behavior

When user hovers over macro-decorated code:

```
┌─────────────────────────────────────────────────────────────────┐
│  @derive(Eq, Hash)  ← HOVER HERE                                │
│  class User {                                                   │
│      name: string;                                              │
│  }                                                              │
└─────────────────────────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────────────────────┐
│  **@derive(Eq, Hash)** - Macro Expansion Preview                │
│                                                                 │
│  ```typescript                                                  │
│  class User {                                                   │
│      name: string;                                              │
│      equals(other: User): boolean {                             │
│          return this.name === other.name;                       │
│      }                                                          │
│      hash(): number {                                           │
│          return hashString(this.name);                          │
│      }                                                          │
│  }                                                              │
│  ```                                                            │
│                                                                 │
│  *Macro source: std/macros/derive.ms*                           │
└─────────────────────────────────────────────────────────────────┘
```

### Performance Targets

| Operation | Target | Max | Notes |
|-----------|--------|-----|-------|
| Completion (cached) | <50ms | 100ms | Trans-Am cache hit |
| Completion (expand) | <200ms | 500ms | Cache miss, run macro |
| Hover (cached) | <30ms | 50ms | Already expanded |
| Hover (expand) | <100ms | 200ms | Run macro for preview |
| Go-to-definition | <50ms | 100ms | Jump to macro source |

### Why This Is Non-Negotiable

**Metascript's value proposition:** TypeScript syntax + compile-time macros + multiple backends.

If LSP doesn't understand macros, users will:
1. Not discover macro-generated methods
2. Get type errors for valid code
3. Not be able to refactor macro-affected code
4. Feel like the IDE is broken

**The bar:** Haxe already does this (network-fetched completions). Metascript MUST match or exceed.

---

## Implementation Roadmap

### Week 1-2: Parser & AST
- [ ] Add `macro` keyword support in lexer
- [ ] Add `quote` keyword support in lexer
- [ ] Add `macro_decl` and `quote_expr` node kinds
- [ ] Create `MacroRegistry` for tracking definitions/usages
- [ ] Test: Parse macro definitions and usages

### Week 3-4: Macro Compilation
- [ ] AST → Metascript source (extend Printer for macro bodies)
- [ ] Metascript → JavaScript transpilation
- [ ] JavaScript → HBC compilation
- [ ] Cache compiled bytecode
- [ ] Test: Round-trip macro definition → execution

### Week 5-6: VM Integration
- [ ] Extend Hermes VM to load macro bytecode dynamically
- [ ] Inject `target`, `ast`, `traits` globals
- [ ] Execute macro and capture AST modifications
- [ ] Test: @derive(Eq) from source-defined macro

### Week 7-8: LSP Integration
- [ ] Add macro-aware queries to QueryDatabase
- [ ] Implement hover (expansion preview)
- [ ] Implement go-to-definition
- [ ] Implement completion for @macroName
- [ ] Test: LSP features in VSCode

### Week 9-10: Highlighting
- [ ] Update tree-sitter grammar
- [ ] Implement semantic tokens for macros
- [ ] Add VSCode theme extension
- [ ] Test: Visual distinction of macro code

### Week 11-12: Polish
- [ ] Cache invalidation correctness
- [ ] Performance benchmarks (<100ms expansion)
- [ ] Error messages for macro failures
- [ ] Documentation

---

## Summary

**Syntax Design:**
```
DEFINE:  macro name(ctx) { quote { ... } }
USE:     @name(...)
INLINE:  @comptime { ... }
```

**Source-defined macros combine:**

1. **Nim-style definition** - `macro` keyword (no @) in regular `.ms` files
2. **Quote blocks** - `quote { }` for AST generation (no manual ast.createX calls)
3. **@ for usage** - `@derive(Eq)`, `@comptime { }` - familiar TypeScript style
4. **Trans-Am caching** - Bytecode cached, smart invalidation
5. **LSP integration** - Hover preview, go-to-definition, completion
6. **Semantic highlighting** - Different colors for compile-time code

**Key insight:** The macro body is regular Metascript code that happens to run at compile-time. This means:
- Same parser, same AST
- Same type checking (if we add it later)
- Same LSP features (with compile-time context awareness)

**See:** `macro-system.md` (macro API), `lsp-implementation.md` (Trans-Am architecture)
