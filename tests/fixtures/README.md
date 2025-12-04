# MetaScript Test Fixtures

Real MetaScript (.ms) files for testing, development, and examples.

## Directory Structure

```
fixtures/
├── basic/              # Basic language features
│   ├── simple_function.ms
│   ├── factorial_recursive.ms
│   └── factorial_iterative.ms
├── control_flow/       # Loops, conditionals, flow control
│   ├── while_loop_counter.ms
│   └── for_loop_sum.ms
├── classes/            # OOP features
│   ├── simple_class.ms
│   ├── class_with_methods.ms
│   ├── inheritance_simple.ms
│   ├── method_override.ms
│   └── polymorphism.ms
├── algorithms/         # Data structures and algorithms
│   ├── quicksort.ms
│   ├── merge_sort.ms
│   ├── linked_list.ms
│   └── tree.ms          (Binary Search Tree)
├── patterns/           # Design patterns
│   ├── builder.ms
│   ├── factory.ms
│   ├── singleton.ms
│   └── observer.ms
├── executable/         # Self-testing fixtures with main()
│   ├── simple_function_test.ms
│   └── factorial_test.ms
└── bugs/               # Known backend bugs (regression tests)

## Usage

### Manual Compilation
```bash
# Compile individual fixture
msc compile tests/fixtures/basic/simple_function.ms

# Compile to specific backend
msc compile --target=c tests/fixtures/algorithms/quicksort.ms
msc compile --target=js tests/fixtures/classes/polymorphism.ms
msc compile --target=erlang tests/fixtures/patterns/observer.ms
```

### LSP Support
Open any .ms file in your editor with Metascript LSP configured for:
- Syntax highlighting
- Hover information
- Type checking
- Diagnostics

### Automated Testing
Fixtures are used by the test infrastructure:
```bash
# Run backend analytics (Phase 0: Code generation)
zig build test-analytics

# Run enhanced execution tests (Phase 0-2: Generate, compile, execute)
zig build test-execution
```

## Fixture Categories

### Basic (3 fixtures)
Simple language features to validate core functionality.

### Control Flow (2 fixtures)
Loops, conditionals, and flow control constructs.

### Classes (5 fixtures)
Object-oriented programming features:
- Simple class definitions
- Methods and constructors
- Inheritance (`extends`)
- Method overriding
- Polymorphism

### Algorithms (4 fixtures)
Real-world data structures and algorithms:
- Quicksort (recursive, array operations)
- Merge sort (divide-and-conquer)
- Linked list (dynamic data structure)
- Binary search tree (recursive tree operations)

### Patterns (4 fixtures)
Common design patterns:
- Builder pattern (fluent interface)
- Factory pattern (object creation)
- Singleton pattern (static fields, getInstance)
- Observer pattern (arrays, iteration)

### Executable (2+ fixtures)
Fixtures with embedded test assertions and main() function.
These compile and execute to verify correctness.

## Implementation Notes

**Current State:**
Fixtures exist as .ms files for manual use (LSP, compilation, examples).
Test infrastructure uses inline strings due to Zig @embedFile() constraints.

**Future:**
Convert to @embedFile() when build system supports accessing ../fixtures/ from tests/backends/.

## Coverage

Current test coverage: **31 fixtures** across all categories.

Analytics results:
- C Backend: 31/31 (100% code generation)
- JavaScript Backend: 31/31 (100% code generation)
- Erlang Backend: 31/31 (100% code generation)

**Note:** 100% code generation ≠ 100% compilation/execution success.
Use `zig build test-execution` for full pipeline validation.
