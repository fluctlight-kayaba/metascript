# High-Performance LSP for Macro-Based Languages

**Goal:** <100ms responsiveness for syntax highlighting and LSP with dynamic macro expansion

**Key Insight:** Rust-analyzer (<100ms) and ZLS (300ms) prove macros + fast LSP are compatible. Nim (5-6s) and Haxe (800ms) prove wrong architecture creates unfixable ceiling.

---

## Performance Benchmarks

| Language | Latency | Memory | Architecture |
|----------|---------|--------|--------------|
| Rust-analyzer | <100ms | Moderate | Query engine (Salsa) |
| ZLS | <50ms syntax, 300ms semantic | Low | AST-first + build-on-save |
| Nim | 5-6s | 24GB leaks | Full compiler per request |
| Haxe | 800ms | Multi-GB | Fresh AST + macro re-expansion |

**Root Cause:**

```
✅ Rust/Zig: LSP Server (Query Engine) → Selective Compiler Queries → Cached
❌ Nim/Haxe: LSP Wrapper → Full Compiler Invocation → No Caching
```

**Critical Insight:** Invert dependency - compiler serves LSP, not vice versa.

---

## Architectural Decisions

### 1. Salsa Query Engine (Rust-Analyzer)

**How it works:**
- Parse, expand macro, infer type = separate queries
- Cache results by input fingerprint
- Invalidate only affected queries

**Example:**
```rust
#[derive(Serialize)]  // Macro cached
struct User {
    name: String,  // Edit here: only type inference re-runs (25ms)
}
```

**vs Nim:**
```nim
type User = object
  name: string  # Edit here: re-parse all imports + re-expand all macros (5s)
```

### 2. Build-on-Save Hybrid (ZLS)

**How it works:**
- AST parsing for syntax (instant)
- Full compiler for semantics (on save only)
- Cancellation model (discard stale analysis)

**Key:** Users accept 300ms delay on save, interactive features stay instant.

**vs Haxe:** Runs macros every keystroke (800ms overhead)

### 3. Macro Expansion Comparison

| Strategy | Frequency | Caching | Result |
|----------|-----------|---------|--------|
| Rust | On-demand | Yes | <100ms ✓ |
| Zig | On-save | Partial | 300ms ✓ |
| Nim | Every request | None | 5-6s ✗ |
| Haxe | Every completion | None | 800ms ✗ |

---

## Critical Design Principles

**1. LSP-First Architecture**
- Design LSP as primary interface, not afterthought
- Compiler serves LSP queries
- Incremental compilation from day one

**2. Caching is Non-Negotiable**
- Cache macro expansion by AST fingerprint
- Invalidate only when macro source/inputs change
- Query-based system (Salsa or equivalent)

**3. Lazy Macro Expansion**
- Do NOT expand on every keystroke
- Expand on-demand or on-save only
- Never block user input

**4. Separate Static from Dynamic**
- Static: TypeScript + `@derive/@comptime` (Tree-sitter)
- Dynamic: Macro expansion + type checking (LSP server)

**5. Incremental Everything**
- Re-parse only changed functions
- Re-expand only affected macros
- Re-check only modified scopes

---

## Performance Targets

| Feature | Target | Max |
|---------|--------|-----|
| Syntax highlighting | <16ms | 50ms |
| Hover (static) | <50ms | 100ms |
| Completion (no macros) | <100ms | 200ms |
| Completion (with macros) | <200ms | 500ms |
| Macro expansion preview | <500ms | 1s |
| Full diagnostics | <1s | 2s |

**Rule:** If users notice lag, it's too slow.

---

## Three-Layer Architecture

```
┌────────────────────────────────────────┐
│ Editor (VSCode/Neovim)                 │
├────────────────────────────────────────┤
│ Tree-sitter    │  Metascript LSP       │
│ (Static)       │  (Semantic)           │
│                │                        │
│ Highlighting   │  Salsa Query Cache    │
│ (<16ms)        │  ├─ Macro (Cached)    │
│                │  ├─ Type (Incremental)│
│                │  └─ Diagnostics       │
└────────────────────────────────────────┘
                 │
                 ▼
          msc compiler
      (explicit request only)
```

---

## Implementation Core

### LSP Server (Zig)

```zig
pub const LSPServer = struct {
    macro_cache: std.AutoHashMap(u64, *ast.Node),

    pub fn onDidChange(self: *LSPServer, params: DidChangeParams) !void {
        doc.ast = try parser.parse(self.arena, doc.text);
        doc.expanded_ast = try self.expandWithCache(doc.ast.?);
        const errors = try checker.checkIncremental(doc.expanded_ast.?);
        try self.publishDiagnostics(params.uri);
    }

    fn expandWithCache(self: *LSPServer, node: *ast.Node) !*ast.Node {
        const hash = computeASTHash(node);
        if (self.macro_cache.get(hash)) |cached| return cached;

        const expanded = try macro.expand(self.arena, node);
        try self.macro_cache.put(hash, expanded);
        return expanded;
    }
};
```

### Incremental Expansion

```zig
pub fn incrementalExpand(self: *LSPServer, doc: *Document, change: TextChange) !void {
    const affected = try findMacrosInRange(doc.ast.?, change.range);

    for (affected) |m| {
        _ = self.macro_cache.remove(computeASTHash(m));
        const expanded = try macro.expandSingle(self.arena, m);
        try self.macro_cache.put(computeASTHash(m), expanded);
    }

    try checker.checkIncremental(doc.expanded_ast.?, affected);
}
```

### Macro Expansion Preview

```typescript
// VSCode: Show expansion side-by-side
vscode.commands.registerCommand('metascript.showMacroExpansion', async (code: string) => {
    const doc = await vscode.workspace.openTextDocument({ content: code });
    await vscode.window.showTextDocument(doc, vscode.ViewColumn.Beside);
});
```

---

## Critical Anti-Patterns

**❌ Full compiler per LSP request** (Nim: 5-6s)
✅ Separate LSP analysis engine

**❌ Re-expand macros every keystroke** (Haxe: 800ms)
✅ Expand on-save or on-demand

**❌ Tight LSP-compiler coupling** (Nim/Haxe)
✅ Custom query engine

**❌ Ignore memory leaks** (Nim: 24GB)
✅ Stateless expansion + GC

**❌ Block for semantic analysis** (Nim: 100% CPU)
✅ Async background + serve stale

---

## Roadmap

**Phase 1 (Week 1-2):** Tree-sitter grammar, <16ms highlighting
**Phase 2 (Week 3-4):** LSP server, <100ms completions
**Phase 3 (Week 5-6):** Macro expansion cache, <200ms with macros
**Phase 4 (Week 7-8):** Type checker, <500ms diagnostics
**Phase 5 (Week 9-12):** Incremental everything, benchmark suite

---

## Files to Create

```
tooling/
├── tree-sitter-metascript/grammar.js
├── lsp/
│   ├── server.zig
│   └── macro_cache.zig
└── extensions/
    ├── metascript-vscode/
    └── metascript-nvim/
```

---

## Conclusion

**Success factors:**
1. LSP-first (not afterthought)
2. Incremental (cache + invalidate minimally)
3. Lazy (expand on-demand)
4. Separate (static syntax ≠ dynamic semantics)

**For Metascript:** Follow Rust/Zig playbook. Build LSP with same care as compiler.

---

## References

- [Rust-Analyzer: IDEs and Macros](https://rust-analyzer.github.io/blog/2021/11/21/ides-and-macros.html)
- [Matklad: ZLS and Cancellation](https://matklad.github.io/2023/05/06/zig-language-server-and-cancellation.html)
- [Nim RFC #300 - Tooling Crisis](https://github.com/nim-lang/RFCs/issues/300)
- [Haxe vshaxe #199 - Macro Overhead](https://github.com/vshaxe/vshaxe/issues/199)
