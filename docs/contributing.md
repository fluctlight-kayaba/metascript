# Contributing to Metascript

Thank you for your interest in contributing to Metascript! This guide will help you get started.

## Code of Conduct

We are committed to providing a welcoming and inclusive environment. Please read and follow our [Code of Conduct](./CODE_OF_CONDUCT.md).

## How Can I Contribute?

### Reporting Bugs

**Before submitting a bug report:**
- Check the [issue tracker](https://github.com/metascript/metascript/issues) for existing reports
- Try the latest version - the bug may already be fixed
- Collect information about your environment

**What to include in a bug report:**
- Clear, descriptive title
- Steps to reproduce
- Expected behavior vs actual behavior
- Metascript version (`metascript --version`)
- Operating system and version
- Minimal code example demonstrating the issue

**Example:**
```markdown
## Bug: Type checker crashes on recursive generic types

**Metascript version:** 0.1.0
**OS:** macOS 14.0

**Code:**
```typescript
interface Node<T> {
    value: T;
    next: Node<T> | null;
}
```

**Expected:** Type checker accepts valid recursive generic
**Actual:** Crashes with stack overflow

**Steps to reproduce:**
1. Create file with code above
2. Run `metascript check node.mts`
3. Crashes
```

### Suggesting Features

We welcome feature suggestions! Before suggesting:
- Check existing feature requests
- Consider if it aligns with [Metascript's vision](../CLAUDE.md)
- Think about backward compatibility

**Good feature suggestions include:**
- Clear use case and motivation
- Proposed syntax/API
- Alternative approaches considered
- Impact on existing code

**Example:**
```markdown
## Feature: Pattern matching for discriminated unions

**Motivation:**
TypeScript's discriminated unions are verbose. Pattern matching would improve ergonomics.

**Proposed syntax:**
```typescript
type Result<T, E> = { ok: true; value: T } | { ok: false; error: E };

const result = getResult();
match (result) {
    { ok: true, value } => console.log(value),
    { ok: false, error } => console.error(error)
}
```

**Alternatives:**
- Use existing if/else (verbose)
- Macro-based solution (possible with @comptime)

**Impact:**
- New syntax (requires parser changes)
- Compile-time exhaustiveness checking
- Zero runtime cost (compiles to switch)
```

### Contributing Code

**Before writing code:**
1. Open an issue to discuss the change
2. Get feedback from maintainers
3. Fork the repository
4. Create a feature branch

**Development workflow:**

```bash
# 1. Fork and clone
git clone https://github.com/YOUR_USERNAME/metascript.git
cd metascript

# 2. Set up development environment
zig version  # Ensure Zig 0.15.1+
zig build

# 3. Create feature branch
git checkout -b feature/my-feature

# 4. Make changes and test
zig build test
metascript test tests/

# 5. Format and lint
zig fmt .
metascript lint src/

# 6. Commit with clear message
git commit -m "Add pattern matching for discriminated unions

- Implement match expression parsing
- Add exhaustiveness checking
- Generate efficient switch statements
- Add tests and documentation

Closes #123"

# 7. Push and create PR
git push origin feature/my-feature
```

### Code Review Process

**What reviewers look for:**
- Code quality and clarity
- Test coverage
- Documentation
- Performance impact
- Backward compatibility

**Tips for successful reviews:**
- Keep PRs focused and small (<500 lines)
- Write clear commit messages
- Add tests for new features
- Update documentation
- Respond to feedback promptly

## Development Setup

### Prerequisites

- **Zig 0.15.1 or later**: [Download Zig](https://ziglang.org/download/)
- **Git**: For version control
- **VS Code** (recommended): With Zig extension

### Building from Source

```bash
# Clone repository
git clone https://github.com/metascript/metascript.git
cd metascript

# Build compiler
zig build

# Run tests
zig build test

# Install locally
zig build install --prefix ~/.local
```

### Project Structure

```
metascript/
├── compiler/          # Compiler implementation (Zig)
│   ├── parser/       # TypeScript parser
│   ├── checker/      # Type checker
│   ├── macro/        # Macro expander
│   ├── codegen/      # Code generation
│   └── backend/      # LLVM/C backend
├── runtime/          # Minimal runtime library
│   ├── gc/          # Garbage collector
│   └── allocator/   # Memory allocator
├── stdlib/           # Standard library
│   ├── core/        # Core types
│   ├── collections/ # Data structures
│   └── macros/      # Standard macros
├── lsp/              # Language server
├── tests/            # Test suite
├── benchmarks/       # Performance tests
└── docs/             # Documentation
```

### Running Tests

```bash
# Run all tests
zig build test

# Run specific test suite
zig build test-parser
zig build test-checker
zig build test-codegen

# Run with verbose output
zig build test -- --verbose

# Run benchmarks
zig build bench
```

### Code Style

**Zig code:**
- Use `zig fmt` before committing
- Follow [Zig Style Guide](https://ziglang.org/documentation/master/#Style-Guide)
- Clear, descriptive names
- Comment complex logic

**Example:**
```zig
// ✅ Good
const TypeChecker = struct {
    ast: *AST,
    types: TypeMap,
    errors: ArrayList(TypeError),

    /// Check types for all nodes in the AST
    pub fn check(self: *TypeChecker) !void {
        try self.checkNode(self.ast.root);
    }

    fn checkNode(self: *TypeChecker, node: *Node) TypeError!void {
        switch (node.*) {
            .class_decl => try self.checkClass(node.class_decl),
            .function_decl => try self.checkFunction(node.function_decl),
            // ...
        }
    }
};
```

**Metascript code (stdlib):**
- Follow TypeScript conventions
- Use `@derive` macros where appropriate
- Document public APIs
- Include examples

**Example:**
```typescript
/// A growable array type
///
/// # Examples
/// ```
/// const arr = new Array<number>();
/// arr.push(1);
/// arr.push(2);
/// console.log(arr.length);  // 2
/// ```
@derive(Clone, Debug)
export class Array<T> {
    private data: T[];
    private len: number;

    /// Create a new empty array
    constructor() {
        this.data = [];
        this.len = 0;
    }

    /// Add element to end of array
    push(value: T): void {
        this.data[this.len] = value;
        this.len += 1;
    }
}
```

## Areas Needing Help

### High Priority

**Compiler:**
- [ ] Parser improvements (better error recovery)
- [ ] Type checker edge cases
- [ ] Macro system implementation
- [ ] LLVM backend optimization

**Standard Library:**
- [ ] Core collections (Map, Set, List)
- [ ] String utilities
- [ ] File I/O
- [ ] Networking

**Tooling:**
- [ ] LSP server implementation
- [ ] VS Code extension
- [ ] Debugger integration
- [ ] Package manager

**Documentation:**
- [ ] Tutorial improvements
- [ ] API documentation
- [ ] Example projects
- [ ] Migration guides

### Good First Issues

Look for issues tagged `good-first-issue`:
- Documentation improvements
- Test coverage
- Simple bug fixes
- Example programs

## Testing Guidelines

### Unit Tests

```zig
// compiler/parser/parser_test.zig
test "parse class declaration" {
    const source = "class Foo { x: number; }";
    const parser = Parser.init(source);
    const ast = try parser.parse();

    try std.testing.expectEqual(.class_decl, ast.root);
    try std.testing.expectEqualStrings("Foo", ast.root.class_decl.name);
}
```

### Integration Tests

```bash
# tests/integration/hello_world.mts
function main(): void {
    console.log("Hello, World!");
}
```

```bash
# Run integration test
metascript compile tests/integration/hello_world.mts
./hello_world
# Expected output: "Hello, World!"
```

### Benchmarks

```zig
// benchmarks/fibonacci.zig
pub fn benchFibonacci(b: *Bench) void {
    b.run("fibonacci(30)", struct {
        pub fn run() void {
            _ = fibonacci(30);
        }
    });
}
```

## Documentation

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
pub fn parse(self: *Parser) !AST {
    // Implementation
}
```

### User Documentation

- Write clearly for TypeScript developers
- Include code examples
- Explain "why" not just "how"
- Link to related docs

## Getting Help

- **Discord**: [#contributors channel](https://discord.gg/metascript)
- **GitHub Discussions**: [github.com/metascript/metascript/discussions](https://github.com/metascript/metascript/discussions)
- **Office Hours**: Weekly video calls (see Discord for schedule)

## Recognition

Contributors are recognized in:
- `CONTRIBUTORS.md` file
- Release notes
- Project website

## License

By contributing, you agree that your contributions will be licensed under the same license as the project (MIT License).

---

**Thank you for contributing to Metascript!** Your work helps bring native performance to TypeScript developers worldwide.
