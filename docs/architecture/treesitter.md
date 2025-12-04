# Tree-sitter Static Syntax Highlighting

**Purpose:** Instant syntax highlighting independent of macro expansion

**Performance Target:** <5ms for any file, <16ms worst case

---

## Why Tree-sitter

| Approach | Latency | Macro Aware | Independence |
|----------|---------|-------------|--------------|
| Regex-based | <1ms | No | Yes |
| LSP semantic tokens | 50-200ms | Yes | No |
| **Tree-sitter** | <5ms | No* | Yes |

*Tree-sitter doesn't know macro expansions, but that's by design.

**Key Insight:** Syntax highlighting should NEVER wait for macro expansion.

---

## Architecture

```
Editor
   │
   ├─ Tree-sitter (instant)     ← Syntax highlighting
   │   └─ <5ms any file
   │
   └─ LSP Server (async)         ← Semantic tokens
       └─ 50-200ms, stale-while-revalidate
```

Tree-sitter provides **immediate feedback** while LSP provides **semantic enhancement**.

---

## Grammar Definition

### Macro Tokens

Tree-sitter must recognize macro invocations as distinct syntax:

```javascript
// grammar.js
const metascript = grammar({
    name: 'metascript',

    rules: {
        // Macro invocation: @derive(Eq, Hash)
        decorator: $ => seq(
            '@',
            $.identifier,
            optional($.arguments)
        ),

        // @comptime block
        comptime_block: $ => seq(
            '@comptime',
            $.block
        ),

        // Standard TypeScript rules...
        class_declaration: $ => seq(
            repeat($.decorator),  // Decorators before class
            'class',
            $.identifier,
            optional($.type_parameters),
            optional($.heritage),
            $.class_body
        ),
    }
});
```

### Query Patterns (Highlights)

```scheme
; highlights.scm

; Macro decorators - distinct color
(decorator "@" @keyword.macro)
(decorator (identifier) @function.macro)

; @comptime - special highlighting
(comptime_block "@comptime" @keyword.comptime)

; Standard TypeScript highlighting
(class_declaration "class" @keyword)
(class_declaration (identifier) @type)
(function_declaration "function" @keyword)
(function_declaration (identifier) @function)

; Keywords
["const" "let" "var" "if" "else" "while" "for" "return"] @keyword

; Types
(type_annotation (identifier) @type)

; Strings
(string) @string

; Numbers
(number) @number

; Comments
(comment) @comment
```

---

## Visual Highlighting

```typescript
@derive(Eq, Hash)          // @derive = macro keyword (purple)
class User {               // class = keyword (blue), User = type (green)
    name: string;          // name = property, string = type
    age: number;           // age = property, number = type

    greet(): string {      // greet = method, string = return type
        return "Hello";    // return = keyword, "Hello" = string
    }
}
```

**Before Tree-sitter:** Everything white until LSP responds (200ms+)
**With Tree-sitter:** Colored instantly (<5ms)

---

## Editor Integration

### VS Code

```json
// package.json
{
    "contributes": {
        "grammars": [{
            "language": "metascript",
            "scopeName": "source.metascript",
            "path": "./syntaxes/metascript.tmGrammar.json"
        }]
    }
}
```

### Neovim

```lua
-- init.lua
require('nvim-treesitter.configs').setup {
    ensure_installed = { "metascript" },
    highlight = { enable = true },
}
```

### Helix

```toml
# languages.toml
[[language]]
name = "metascript"
scope = "source.metascript"
injection-regex = "metascript"
file-types = ["ms", "mts"]
roots = []
auto-format = false
comment-token = "//"
indent = { tab-width = 4, unit = "    " }

[[grammar]]
name = "metascript"
source = { git = "https://github.com/metascript/tree-sitter-metascript" }
```

---

## Testing Strategy

### Unit Tests

```javascript
// test/highlight.test.js
test("decorator highlighted as macro", () => {
    const tree = parser.parse("@derive(Eq) class User {}");
    const captures = query.captures(tree.rootNode);

    const derive = captures.find(c => c.node.text === "derive");
    expect(derive.name).toBe("function.macro");
});

test("class keyword highlighted", () => {
    const tree = parser.parse("class User {}");
    const captures = query.captures(tree.rootNode);

    const classKw = captures.find(c => c.node.text === "class");
    expect(classKw.name).toBe("keyword");
});
```

### Performance Tests

```javascript
test("10K LOC file highlights in <50ms", () => {
    const largeFile = generateLargeFile(10000);
    const start = performance.now();
    parser.parse(largeFile);
    const duration = performance.now() - start;
    expect(duration).toBeLessThan(50);
});
```

---

## Semantic Token Enhancement

Tree-sitter provides base highlighting. LSP **enhances** with semantic info:

```
Tree-sitter (instant):
  @derive     → keyword.macro (purple)
  Eq          → identifier (default)

LSP semantic tokens (async):
  Eq          → type.trait (special color)
```

**Flow:**
1. Tree-sitter: Instant base colors
2. LSP: Async enhancement when ready
3. Result: Fast + accurate

---

## File Location

```
tree-sitter-metascript/
  grammar.js           # Grammar definition
  src/
    parser.c           # Generated parser
    scanner.c          # Custom scanner (if needed)
  queries/
    highlights.scm     # Highlighting queries
    injections.scm     # Language injection
    locals.scm         # Local scope queries
  bindings/
    node/              # Node.js bindings
    rust/              # Rust bindings
```

---

## Development Workflow

```bash
# Generate parser
tree-sitter generate

# Test parsing
tree-sitter parse examples/macro.ms

# Test highlighting
tree-sitter highlight examples/macro.ms

# Run tests
tree-sitter test
```

---

## References

- **Tree-sitter docs:** https://tree-sitter.github.io/tree-sitter/
- **TypeScript grammar:** Reference implementation
- **LSP integration:** `./lsp.md`
