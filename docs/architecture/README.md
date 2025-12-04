# Architecture Documentation

Domain-specific technical documentation for Metascript compiler components.

---

## Component Documents

### Frontend (Parsing & Analysis)

| Document | Description |
|----------|-------------|
| [lexer.md](./lexer.md) | Token types, streaming lexer, error recovery |
| [parser.md](./parser.md) | Recursive descent parser, Pratt precedence |
| [ast.md](./ast.md) | AST nodes, arena allocation, visitor pattern |
| [ast-analyzer.md](./ast-analyzer.md) | Type checking, semantic analysis, lifetime analysis |

### Memory Management

| Document | Description |
|----------|-------------|
| [drc-orc.md](./drc-orc.md) | Reference counting with cycle detection |
| [lobster.md](./lobster.md) | Compile-time optimizations to reduce RC overhead |
| [runtime.md](./runtime.md) | ORC runtime, msString, C runtime library |

### Developer Experience

| Document | Description |
|----------|-------------|
| [lsp.md](./lsp.md) | Language server for IDE features |
| [trans-am.md](./trans-am.md) | Incremental computation cache engine |
| [treesitter.md](./treesitter.md) | Static syntax highlighting |
| [cli.md](./cli.md) | Command line interface and commands |

### Metaprogramming

| Document | Description |
|----------|-------------|
| [macros.md](./macros.md) | @derive, @comptime, custom macros |
| [vm.md](./vm.md) | Hermes-based compile-time VM for macros |

### Module System

| Document | Description |
|----------|-------------|
| [modules.md](./modules.md) | Import/export resolution, module loading |

### Code Generation

| Document | Description |
|----------|-------------|
| [backends.md](./backends.md) | C, JavaScript, Erlang backends |

---

## Navigation

### By Development Phase

**Phase 1: Core Pipeline**
1. [Lexer](./lexer.md) - Tokenization
2. [Parser](./parser.md) - Parsing
3. [AST](./ast.md) - AST structure
4. [AST Analyzer](./ast-analyzer.md) - Type checking
5. [Macros](./macros.md) - Metaprogramming
6. [VM](./vm.md) - Compile-time execution
7. [Backends](./backends.md) - Code generation

**Phase 2: Performance**
1. [DRC/ORC](./drc-orc.md) - Memory management
2. [Lobster](./lobster.md) - RC optimizations
3. [Runtime](./runtime.md) - Runtime library
4. [Trans-Am](./trans-am.md) - Caching

**Phase 3: Tooling**
1. [CLI](./cli.md) - Command line interface
2. [LSP](./lsp.md) - IDE integration
3. [Tree-sitter](./treesitter.md) - Syntax highlighting
4. [Modules](./modules.md) - Module system

---

## Cross-References

| If working on... | Also see... |
|------------------|-------------|
| Parsing | Parser, AST, Lexer |
| Macro expansion | VM (execution), Trans-Am (caching), LSP (hover support) |
| Memory management | DRC/ORC, Lobster (optimizations), Runtime, Backends (C generation) |
| LSP performance | Trans-Am (query engine), Tree-sitter (instant highlighting) |
| Type checking | AST Analyzer, AST, Macros (expansion before checking) |
| Module imports | Modules, Parser (import syntax) |
| CLI commands | CLI, Backends (compile targets) |

---

## Quick Reference

### Compilation Pipeline

```
Source → Lexer → Parser → AST → Macro Expansion (VM)
                           ↓
                    Type Checking (AST Analyzer)
                           ↓
                    DRC/Lobster Analysis
                           ↓
                    Backend Selection
        ┌──────────────────┼──────────────────┐
        ↓                  ↓                  ↓
    C Backend        JS Backend       Erlang Backend
```

### Key Files

| Component | Location |
|-----------|----------|
| Lexer | `src/lexer/lexer.zig` |
| Parser | `src/parser/parser.zig` |
| AST | `src/ast/ast.zig`, `node.zig`, `types.zig` |
| Type Checker | `src/checker/typechecker.zig` |
| Macro VM | `src/vm/macro_vm.zig` |
| ORC Runtime | `src/runtime/orc.zig` |
| Module Loader | `src/module/loader.zig` |
| C Backend | `src/codegen/c/cgen.zig` |
| JS Backend | `src/codegen/js/jsgen.zig` |
| Erlang Backend | `src/codegen/erlang/erlgen.zig` |
| CLI | `src/main.zig`, `src/cli/*.zig` |
| LSP | `src/lsp/*.zig` |
| Trans-Am | `src/transam/transam.zig` |

---

## Related Documentation

- **Main CLAUDE.md:** Development philosophy, quick reference
- **Parser design:** `../parser-design.md`
- **Full LSP architecture:** `../lsp-architecture.md`
- **Trans-Am details:** `../trans-am-architecture.md`
- **C backend details:** `../codegen-c.md`
- **Testing infrastructure:** `../testing-infrastructure.md`
