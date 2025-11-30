# Contributing to Metascript

Thank you for contributing! This guide helps you get started.

---

## Code of Conduct

We are committed to a welcoming environment. Read and follow our [Code of Conduct](./CODE_OF_CONDUCT.md).

---

## How to Contribute

### Reporting Bugs

**Include:**
- Clear, descriptive title
- Metascript version (`msc --version`)
- Operating system and version
- Minimal code example
- Steps to reproduce
- Expected vs actual behavior

**Example:**
```markdown
## Bug: Type checker crashes on recursive generic types

**Metascript version:** 0.1.0
**OS:** macOS 14.0

**Code:**
```typescript
interface Node<T> { value: T; next: Node<T> | null; }
```

**Expected:** Type checker accepts valid recursive generic
**Actual:** Crashes with stack overflow

**Steps:** Create file with code above → Run `msc check node.ms` → Crashes
```

### Suggesting Features

**Include:**
- Clear use case and motivation
- Proposed syntax/API
- Alternative approaches considered
- Impact on existing code

**Check:** Aligns with [Metascript's vision](../CLAUDE.md), backward compatibility

### Contributing Code

**Workflow:**
```bash
# 1. Fork and clone
git clone https://github.com/YOUR_USERNAME/metascript.git
cd metascript

# 2. Set up (requires Zig 0.15.1+)
zig build
zig build test

# 3. Create feature branch
git checkout -b feature/my-feature

# 4. Make changes, test, format
zig fmt .
zig build test

# 5. Commit with clear message
git commit -m "Add pattern matching for discriminated unions

- Implement match expression parsing
- Add exhaustiveness checking
- Generate efficient switch statements
- Add tests and documentation

Closes #123"

# 6. Push and create PR
git push origin feature/my-feature
```

**Tips for successful PRs:**
- Keep PRs focused and small (<500 lines)
- Write clear commit messages
- Add tests for new features
- Update documentation
- Respond to feedback promptly

---

## Development Setup

### Prerequisites
- Zig 0.15.1+ ([Download](https://ziglang.org/download/))
- Git
- VS Code (recommended) with Zig extension

### Building from Source
```bash
git clone https://github.com/metascript/metascript.git
cd metascript
zig build
zig build test
zig build install --prefix ~/.local
```

### Project Structure
```
metascript/
├── compiler/          # Compiler (Zig): parser, checker, macro, codegen, backend
├── runtime/          # Minimal runtime: gc, allocator
├── stdlib/           # Standard library: core, collections, macros
├── lsp/              # Language server
├── tests/            # Test suite
├── benchmarks/       # Performance tests
└── docs/             # Documentation
```

### Running Tests
```bash
zig build test                  # All tests
zig build test-parser           # Specific suite
zig build test -- --verbose     # Verbose output
zig build bench                 # Benchmarks
```

---

## Code Style

### Zig Code
- Use `zig fmt` before committing
- Follow [Zig Style Guide](https://ziglang.org/documentation/master/#Style-Guide)
- Clear, descriptive names
- Comment complex logic

**Example:**
```zig
const TypeChecker = struct {
    ast: *AST,
    types: TypeMap,

    /// Check types for all nodes in the AST
    pub fn check(self: *TypeChecker) !void {
        try self.checkNode(self.ast.root);
    }

    fn checkNode(self: *TypeChecker, node: *Node) TypeError!void {
        switch (node.*) {
            .class_decl => try self.checkClass(node.class_decl),
            .function_decl => try self.checkFunction(node.function_decl),
        }
    }
};
```

### Metascript Code (stdlib)
- Follow TypeScript conventions
- Use `@derive` macros where appropriate
- Document public APIs with examples

**Example:**
```typescript
/// A growable array type
///
/// # Examples
/// ```
/// const arr = new Array<number>();
/// arr.push(1);
/// console.log(arr.length);  // 1
/// ```
@derive(Clone, Debug)
export class Array<T> {
    private data: T[];
    private len: number;

    constructor() { this.data = []; this.len = 0; }
    push(value: T): void { this.data[this.len++] = value; }
}
```

---

## Areas Needing Help

### High Priority
**Compiler:** Parser error recovery, type checker edge cases, macro system, LLVM optimization
**Stdlib:** Core collections (Map, Set, List), string utilities, file I/O, networking
**Tooling:** LSP server, VS Code extension, debugger integration, package manager
**Docs:** Tutorial improvements, API docs, examples, migration guides

### Good First Issues
Look for `good-first-issue` tag:
- Documentation improvements
- Test coverage
- Simple bug fixes
- Example programs

---

## Testing Guidelines

### Unit Tests
```zig
test "parse class declaration" {
    const source = "class Foo { x: number; }";
    const parser = Parser.init(source);
    const ast = try parser.parse();

    try std.testing.expectEqual(.class_decl, ast.root);
    try std.testing.expectEqualStrings("Foo", ast.root.class_decl.name);
}
```

### Integration Tests
```typescript
// tests/integration/hello_world.ms
function main(): void { console.log("Hello, World!"); }
```
```bash
msc compile tests/integration/hello_world.ms
./hello_world  # Expected: "Hello, World!"
```

---

## Documentation Guidelines

### Code Comments
```zig
/// Parse a TypeScript source file into an AST
///
/// Returns an error if parsing fails
///
/// # Examples
/// ```zig
/// const parser = Parser.init(source);
/// const ast = try parser.parse();
/// ```
pub fn parse(self: *Parser) !AST { /* implementation */ }
```

### User Documentation
- Write clearly for TypeScript developers
- Include code examples
- Explain "why" not just "how"
- Link to related docs

---

## Getting Help

- **Discord:** [#contributors channel](https://discord.gg/metascript)
- **GitHub Discussions:** [metascript/discussions](https://github.com/metascript/metascript/discussions)
- **Office Hours:** Weekly video calls (see Discord)

---

## Recognition

Contributors are recognized in:
- `CONTRIBUTORS.md` file
- Release notes
- Project website

---

## License

By contributing, you agree your contributions will be licensed under the MIT License.

---

**Thank you for contributing!** Your work brings native performance to TypeScript developers worldwide.
