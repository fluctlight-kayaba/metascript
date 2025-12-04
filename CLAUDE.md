# Metascript

A universal systems language: TypeScript syntax + compile-time macros + three strategic backends (C, JavaScript, Erlang).

---

## Development Philosophy

### See It, Feel It, TEST It, Fix It

```
SEE IT   → Make behavior visible immediately (even if broken)
FEEL IT  → Use it, experience the workflow
TEST IT  → Prove it works with tests (TDD: test BEFORE implementation)
FIX IT   → Iterate with confidence (tests protect you)
```

**Core Rules:**
1. **Visibility First:** LSP, syntax highlighting, compiler output must be touchable ASAP
2. **Test First (Non-Negotiable):** Write test → See it fail (RED) → Implement → Pass (GREEN) → Refactor
3. **Correctness Over Speed:** Find root causes, no band-aid fixes

---

## Quick Reference

### Development Commands

```bash
# Build
zig build
zig build test --summary all              # Run all tests
zig build test -- --test-filter="lexer"   # Run specific tests

# Compile
msc compile main.ms                       # Default: C backend
msc compile --target=c main.ms            # Native binary
msc compile --target=js main.ms           # JavaScript
msc compile --target=erlang main.ms       # Erlang/BEAM

# Tools
msc check main.ms                         # Type check only
msc lsp                                   # Start LSP server
msc expand --macro=derive main.ms         # Preview macro expansion
```

### Project Structure

```
metascript/
├── src/
│   ├── lexer/          # Tokenization
│   ├── parser/         # AST construction
│   ├── checker/        # Type checking
│   ├── macro/          # Macro expansion
│   ├── codegen/        # C, JS, Erlang backends
│   ├── transam/        # Incremental cache engine
│   ├── lsp/            # Language server
│   └── runtime/        # ORC memory management
├── std/macros/         # @derive, @comptime implementations
├── tests/              # Test suite
├── examples/           # Example programs
└── docs/               # Documentation
    └── architecture/   # Domain-specific technical docs
```

---

## Architecture Documentation

Detailed technical documentation lives in `docs/architecture/`:

| Document | Purpose |
|----------|---------|
| [lexer.md](docs/architecture/lexer.md) | Token types, streaming, error recovery |
| [ast-analyzer.md](docs/architecture/ast-analyzer.md) | Type checking, semantic analysis |
| [drc-orc.md](docs/architecture/drc-orc.md) | Reference counting, cycle detection |
| [lobster.md](docs/architecture/lobster.md) | Compile-time RC optimizations |
| [lsp.md](docs/architecture/lsp.md) | IDE features, macro-aware completion |
| [trans-am.md](docs/architecture/trans-am.md) | Incremental caching (Salsa-style) |
| [macros.md](docs/architecture/macros.md) | @derive, @comptime, custom macros |
| [backends.md](docs/architecture/backends.md) | C, JavaScript, Erlang code generation |
| [treesitter.md](docs/architecture/treesitter.md) | Static syntax highlighting |

---

## Testing Guidelines

### Test Categories

| Type | Ratio | Speed | Purpose |
|------|-------|-------|---------|
| Unit | 40% | <10ms | Single function/module |
| Integration | 30% | <100ms | Multi-component |
| E2E | 20% | <1s | Full compilation |
| Property/Fuzz | 10% | Varies | Edge cases |

### Coverage Targets

| Component | Target |
|-----------|--------|
| Lexer | 90% |
| Parser | 85% |
| Type Checker | 80% |
| Macro Expander | 85% |
| Backends | 75% |

### Test Structure

```zig
test "component action expected_result" {
    // Arrange
    const input = "...";

    // Act
    const result = component.process(input);

    // Assert
    try testing.expectEqual(expected, result);
}
```

---

## Reference Models

Local codebases to study when stuck:

| Problem | Study |
|---------|-------|
| C backend + macros | `~/projects/nim`, `~/projects/haxe` |
| Erlang backend | `~/projects/elixir`, `~/projects/gleam` |
| JavaScript backend | `~/projects/bun`, `~/projects/hermes` |

---

## Performance Targets

### Compilation

| Size | Debug | Release |
|------|-------|---------|
| 1K LOC | <1s | <5s |
| 10K LOC | <5s | <30s |
| 100K LOC | <30s | <5min |

### LSP

| Operation | Target |
|-----------|--------|
| Syntax highlighting | <5ms |
| Hover | <50ms |
| Completion | <100ms |
| Diagnostics | <500ms |

### C Backend Runtime

| Metric | Target |
|--------|--------|
| Performance vs C | 90%+ |
| Lambda cold start | <50ms |
| RC overhead | 0.5-2% (with Lobster optimizations) |

---

## Documentation Links

### Getting Started
- [Quickstart](docs/quickstart.md)
- [Migration from TypeScript](docs/migration-from-typescript.md)

### Technical Reference
- [Macro System](docs/macro-system.md)
- [Architecture Overview](docs/architecture.md)
- [Backends Guide](docs/backends.md)
- [Memory Model](docs/memory-model.md)

### Development
- [Parser Design](docs/parser-design.md)
- [Trans-Am Architecture](docs/trans-am-architecture.md)
- [LSP Architecture](docs/lsp-architecture.md)
- [Testing Infrastructure](docs/testing-infrastructure.md)

### Planning
- [Roadmap](docs/roadmap.md)
- [Philosophy](docs/philosophy.md)
- [Design References](docs/design-references.md)

---

## Anti-Patterns

| Avoid | Instead |
|-------|---------|
| Building without visibility | Show broken version NOW |
| Tests after implementation | TDD: test first |
| Masking symptoms | Find root cause |
| Waiting for "complete" | Iterate incrementally |
| "Trust me, it works" | Prove with tests |

---

**North Star:** TypeScript syntax + compile-time macros + three backends. One language, three runtimes.
