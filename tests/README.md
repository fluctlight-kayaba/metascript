# Metascript Test Suite

Comprehensive testing infrastructure following TDD principles from CLAUDE.md.

## Quick Start

```bash
# Run all tests with progress (recommended)
zig build test-all --summary all

# Run inline tests only (fastest)
zig build test --summary all

# Run specific test categories
zig build test-unit        # Unit tests
zig build test-integration # Integration tests
zig build test-property    # Property/fuzz tests
zig build test-quick       # Quick smoke tests
zig build test-e2e         # E2E tests (slow first run - compiles Hermes C++)

# Run with verbose output
zig build test -- --verbose

# Filter by test name
zig build test -- --test-filter="lexer"
```

**Note:** First run of `test-all` or `test-e2e` takes ~2 minutes (compiling Hermes VM C++).
Subsequent runs are fast (cached). Use `--summary all` to see progress.

## Directory Structure

```
tests/
├── README.md                 # This file
├── all_tests.zig             # Test entry point and documentation
├── unit/                     # Unit tests (40%)
│   ├── lexer_test.zig        # Lexer tokenization tests
│   └── parser_test.zig       # Parser AST tests
├── integration/              # Integration tests (30%)
│   └── macro_expansion_test.zig  # Lexer → Parser → Macro
├── e2e/                      # End-to-end tests (20%)
│   └── compilation_test.zig  # Full compilation pipeline
├── property/                 # Property/fuzz tests (10%)
│   └── lexer_fuzz_test.zig   # Lexer robustness
├── fixtures/                 # Test data
│   └── sources.zig           # Metascript source samples
├── snapshots/                # Snapshot test data
│   └── *.snap                # Saved snapshots
└── helpers/                  # Test utilities
    └── testing.zig           # Assertions, TestContext, Timer
```

## Test Categories

### Unit Tests (40% of tests)
- Test single function/module in isolation
- Fast (<10ms each)
- No I/O, no external dependencies
- Location: `tests/unit/`

### Integration Tests (30% of tests)
- Test multiple components together
- Lexer + Parser, Parser + Macro Expander, etc.
- Location: `tests/integration/`

### End-to-End Tests (20% of tests)
- Test full compilation pipeline
- Real .ms files as input
- Verify actual output
- Location: `tests/e2e/`

### Property Tests (10% of tests)
- Fuzz testing with random inputs
- Edge case discovery
- Verifies invariants (e.g., lexer never crashes)
- Location: `tests/property/`

## Writing Tests

### TDD Workflow

```
1. Write failing test (RED)
2. Run test → verify it fails
3. Implement feature
4. Run test → verify it passes (GREEN)
5. Refactor (REFACTOR)
6. Repeat
```

### Test Naming Convention

```zig
// Pattern: "component action expected_result"
test "lexer tokenizes class keyword" { }
test "parser parses empty class declaration" { }
test "parser errors on missing class name" { }
test "macro expander generates equals method" { }
```

### Using Test Fixtures

```zig
const fixtures = @import("../fixtures/sources.zig");

test "parser parses class with properties" {
    const program = try parse(fixtures.CLASS_WITH_PROPERTIES);
    // assertions...
}
```

### Using Test Helpers

```zig
const t = @import("../helpers/testing.zig");

test "string assertions" {
    try t.expectContains("hello world", "world");
    try t.expectStartsWith("hello", "hel");
    try t.expectEndsWith("world", "rld");
}

test "numeric assertions" {
    try t.expectInRange(i32, 5, 0, 10);
}

test "performance timing" {
    const timer = t.Timer.start();
    // ... do work ...
    try timer.expectUnder(100); // must complete in <100ms
}
```

### Using TestContext for Temp Files

```zig
const t = @import("../helpers/testing.zig");

test "test with temp files" {
    var ctx = t.TestContext.init();
    defer ctx.deinit(); // Auto-cleans temp files

    const path = try ctx.createTempFile("test.ms", "const x = 42;");
    const content = try ctx.readTempFile("test.ms");
    try t.expectEqualStrings("const x = 42;", content);
}
```

### Snapshot Testing

```zig
const t = @import("../helpers/testing.zig");

test "AST snapshot" {
    const ast = try parse("class User {}");
    const output = try formatAST(ast);

    // Compares against tests/snapshots/user_class_ast.snap
    try t.expectSnapshot("user_class_ast", output);
}
```

Update snapshots:
```bash
METASCRIPT_UPDATE_SNAPSHOTS=1 zig build test
```

## Running Specific Tests

```bash
# Filter by test name
zig build test -- --test-filter="lexer"

# Run single test file
zig test tests/unit/lexer_test.zig

# Verbose output
zig build test -- --verbose
```

## Test Coverage Goals

| Component | Target Coverage |
|-----------|-----------------|
| Lexer     | 90%             |
| Parser    | 85%             |
| AST       | 80%             |
| Macro     | 85%             |
| IR        | 75%             |
| LSP       | 70%             |

## Adding New Tests

1. **Decide category**: unit, integration, or e2e?
2. **Create test file**: `tests/<category>/<component>_test.zig`
3. **Import fixtures**: Use `fixtures/sources.zig` for test data
4. **Use helpers**: Import from `helpers/testing.zig`
5. **Update build.zig**: Add new test file to appropriate step
6. **Run tests**: `zig build test-all`

## CI Integration

Tests run automatically on:
- Every commit
- Every PR
- Nightly builds

Required for merge:
- All tests pass
- No memory leaks (via test allocator)
- Performance tests under threshold
