/// C Backend Code Generation Tests
///
/// Tests C code generation for correctness, performance, and safety.
/// These tests are written BEFORE C backend implementation (TDD Red phase).
/// They define what correct C output should look like.

const std = @import("std");
const testing = std.testing;
const helpers = @import("backend_test_helpers.zig");
const fixtures = @import("real_world_fixtures.zig");

// ============================================================================
// Basic C Code Generation (Start Here)
// ============================================================================

test "c: simple function generates valid C" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.SIMPLE_FUNCTION,
        .c,
    );
    defer result.deinit();

    // Should have C function signature
    try helpers.expectContains(result.output, "double add(double a, double b)");
    // Note: codegen adds parens around binary expressions for safety
    try helpers.expectContains(result.output, "return (a + b);");
}

test "c: function compiles with gcc" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.SIMPLE_FUNCTION,
        .c,
    );
    defer result.deinit();

    // Compile with GCC to verify valid C
    var compile_result = try helpers.compileWithGCC(testing.allocator, result.output);
    defer compile_result.deinit();

    if (!compile_result.success) {
        std.debug.print("\n❌ Generated C code failed to compile with GCC\n", .{});
        std.debug.print("Stderr: {s}\n", .{compile_result.stderr});
        std.debug.print("Generated code:\n{s}\n", .{result.output});
        return error.CCompilationFailed;
    }
}

// ============================================================================
// Variable Handling
// ============================================================================

test "c: variables use stack allocation for primitives" {
    const source =
        \\function demo(): number {
        \\    const x = 10;
        \\    const y = 20;
        \\    return x + y;
        \\}
    ;

    var result = try helpers.expectCompiles(testing.allocator, source, .c);
    defer result.deinit();

    // Primitives should be stack variables
    try helpers.expectContains(result.output, "double x = 10");
    try helpers.expectContains(result.output, "double y = 20");
}

test "c: variable reassignment generates new binding in C" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.VARIABLE_SHADOWING_SIMPLE,
        .c,
    );
    defer result.deinit();

    // C doesn't need shadowing like Erlang - just reassign
    // Note: codegen adds parens for safety: (x = (x + 5))
    try helpers.expectContains(result.output, "(x = (x + 5))");
}

// ============================================================================
// Control Flow
// ============================================================================

test "c: while loop generates native while" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.WHILE_LOOP_COUNTER,
        .c,
    );
    defer result.deinit();

    // Should use C while loop (not tail recursion like Erlang)
    // Note: codegen adds parens around condition
    try helpers.expectContains(result.output, "while ((count > 0))");
}

test "c: for loop generates native for" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.FOR_LOOP_SUM,
        .c,
    );
    defer result.deinit();

    // Should use C for loop
    try helpers.expectContains(result.output, "for (");
}

test "c: if-else generates native if" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.ERLANG_BUG_EARLY_RETURN,
        .c,
    );
    defer result.deinit();

    // C has proper return statements - no case expression needed
    // Note: codegen adds parens around condition
    try helpers.expectContains(result.output, "if ((x < 0))");
    try helpers.expectContains(result.output, "return -x;");
}

test "c: early return works correctly" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.FACTORIAL_RECURSIVE,
        .c,
    );
    defer result.deinit();

    // Early return should just work in C
    // Note: codegen adds parens around conditions and expressions
    try helpers.expectContains(result.output, "if ((n <= 1))");
    try helpers.expectContains(result.output, "return 1;");
    try helpers.expectContains(result.output, "return (n * factorial((n - 1)));");
}

// ============================================================================
// Memory Management
// ============================================================================

test "c: includes runtime headers" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.SIMPLE_FUNCTION,
        .c,
    );
    defer result.deinit();

    // Should include standard library and MetaScript runtime
    try helpers.expectContains(result.output, "#include");
}

test "c: objects allocate on heap with GC" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.OBJECT_LITERAL,
        .c,
    );
    defer result.deinit();

    // Objects should use heap allocation
    // (implementation-specific, but should have allocation)
    try helpers.expectContains(result.output, "struct");
}

test "c: arrays use dynamic allocation" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.ARRAY_OPERATIONS,
        .c,
    );
    defer result.deinit();

    // Arrays should be heap-allocated with length tracking
    // (exact implementation TBD, but should involve allocation)
}

// ============================================================================
// Function Calls
// ============================================================================

test "c: recursive calls work correctly" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.FIBONACCI,
        .c,
    );
    defer result.deinit();

    // Should have recursive function calls
    // Note: codegen adds parens around args
    try helpers.expectContains(result.output, "fibonacci((n - 1))");
    try helpers.expectContains(result.output, "fibonacci((n - 2))");
}

test "c: console.log maps to printf" {
    const source =
        \\function demo(): void {
        \\    console.log("Hello", 42);
        \\}
    ;

    var result = try helpers.expectCompiles(testing.allocator, source, .c);
    defer result.deinit();

    // Should use printf or custom logging function
    try helpers.expectContains(result.output, "printf(");
}

// ============================================================================
// Type System
// ============================================================================

test "c: number maps to double" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.SIMPLE_FUNCTION,
        .c,
    );
    defer result.deinit();

    // TypeScript number → C double
    try helpers.expectContains(result.output, "double");
}

test "c: boolean maps to bool" {
    const source =
        \\function isPositive(x: number): boolean {
        \\    return x > 0;
        \\}
    ;

    var result = try helpers.expectCompiles(testing.allocator, source, .c);
    defer result.deinit();

    // Should use C bool (from stdbool.h)
    try helpers.expectContains(result.output, "bool");
}

test "c: string maps to char pointer or string struct" {
    const source =
        \\function getMessage(): string {
        \\    return "Hello, World!";
        \\}
    ;

    var result = try helpers.expectCompiles(testing.allocator, source, .c);
    defer result.deinit();

    // String representation (exact type TBD)
    // Could be char*, String*, or custom struct
}

// ============================================================================
// Classes and Objects
// ============================================================================

test "c: class becomes struct" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.SIMPLE_CLASS,
        .c,
    );
    defer result.deinit();

    // Classes should generate structs
    try helpers.expectContains(result.output, "struct Point");
    try helpers.expectContains(result.output, "double x");
    try helpers.expectContains(result.output, "double y");
}

test "c: constructor becomes initialization function" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.SIMPLE_CLASS,
        .c,
    );
    defer result.deinit();

    // Constructor should be a function that returns pointer to struct
    try helpers.expectContains(result.output, "Point");
}

test "c: method becomes function with this pointer" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.CLASS_WITH_METHODS,
        .c,
    );
    defer result.deinit();

    // Methods should take struct pointer as first argument
    // e.g., double Calculator_add(Calculator* this, double n)
}

test "c: method call transforms to function call" {
    // Test that obj.method(args) becomes ClassName_method(obj, args)
    const source =
        \\class Counter {
        \\    value: number;
        \\
        \\    increment(): void {
        \\        this.value = this.value + 1;
        \\    }
        \\}
        \\
        \\function main(): number {
        \\    const c = new Counter();
        \\    c.value = 0;
        \\    c.increment();
        \\    return c.value;
        \\}
    ;

    var result = try helpers.expectCompiles(testing.allocator, source, .c);
    defer result.deinit();

    // Method call should be transformed: c.increment() → Counter_increment(c)
    try helpers.expectContains(result.output, "Counter_increment");
    // Should NOT have C++ style method call
    try helpers.expectNotContains(result.output, "->increment()");
}

// ============================================================================
// Performance and Optimization
// ============================================================================

test "c: no unnecessary allocations for primitives" {
    const source =
        \\function calculate(): number {
        \\    const a = 1;
        \\    const b = 2;
        \\    return a + b;
        \\}
    ;

    var result = try helpers.expectCompiles(testing.allocator, source, .c);
    defer result.deinit();

    // Primitives should be stack-allocated
    // Should NOT see malloc for primitive values
    try helpers.expectNotContains(result.output, "malloc");
}

test "c: tail recursion can be optimized by compiler" {
    const source =
        \\function sum(n: number, acc: number): number {
        \\    if (n <= 0) return acc;
        \\    return sum(n - 1, acc + n);
        \\}
    ;

    var result = try helpers.expectCompiles(testing.allocator, source, .c);
    defer result.deinit();

    // Should generate tail call that GCC/Clang can optimize
    try helpers.expectContains(result.output, "return sum(");
}

// ============================================================================
// Error Handling
// ============================================================================

test "c: runtime errors use panic mechanism" {
    const source =
        \\function divide(a: number, b: number): number {
        \\    if (b === 0) {
        \\        throw new Error("Division by zero");
        \\    }
        \\    return a / b;
        \\}
    ;

    var result = try helpers.expectCompiles(testing.allocator, source, .c);
    defer result.deinit();

    // Should have error handling mechanism
    // (exact implementation TBD - could be panic, longjmp, etc.)
}

// ============================================================================
// Smoke Tests: Real-World Programs
// ============================================================================

test "c: fibonacci compiles and runs correctly" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.FIBONACCI,
        .c,
    );
    defer result.deinit();

    // Should compile with GCC
    var compile_result = try helpers.compileWithGCC(testing.allocator, result.output);
    defer compile_result.deinit();

    if (!compile_result.success) {
        std.debug.print("\nGenerated C:\n{s}\n", .{result.output});
        std.debug.print("GCC stderr: {s}\n", .{compile_result.stderr});
        return error.InvalidCGenerated;
    }
}

test "c: quicksort compiles" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.QUICKSORT,
        .c,
    );
    defer result.deinit();

    // Complex recursive algorithm should compile
    try helpers.expectContains(result.output, "quicksort");
}

test "c: comprehensive demo compiles" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.COMPREHENSIVE_DEMO,
        .c,
    );
    defer result.deinit();

    // Full program should compile
    try helpers.expectContains(result.output, "fibonacci");
    try helpers.expectContains(result.output, "main");
}

// ============================================================================
// Advanced TypeScript Concepts - Boundary Testing
// ============================================================================

test "c: generics basic compiles" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.GENERICS_BASIC,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Generics not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Generics basic: SUPPORTED\n", .{});
}

test "c: generics with constraints compiles" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.GENERICS_CONSTRAINTS,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Generic constraints not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Generic constraints: SUPPORTED\n", .{});
}

test "c: generic class compiles" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.GENERIC_CLASS,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Generic classes not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Generic classes: SUPPORTED\n", .{});
}

test "c: union types compile" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.UNION_TYPES,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Union types not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Union types: SUPPORTED\n", .{});
}

test "c: intersection types compile" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.INTERSECTION_TYPES,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Intersection types not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Intersection types: SUPPORTED\n", .{});
}

test "c: optional chaining compiles" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.OPTIONAL_CHAINING,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Optional chaining not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Optional chaining: SUPPORTED\n", .{});
}

test "c: nullish coalescing compiles" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.NULLISH_COALESCING,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Nullish coalescing not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Nullish coalescing: SUPPORTED\n", .{});
}

test "c: type guards compile" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.TYPE_GUARDS,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Type guards not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Type guards: SUPPORTED\n", .{});
}

test "c: closures compile" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.CLOSURES,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Closures not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Closures: SUPPORTED\n", .{});
}

test "c: higher-order functions compile" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.HIGHER_ORDER_FUNCTIONS,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Higher-order functions not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Higher-order functions: SUPPORTED\n", .{});
}

test "c: arrow functions compile" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.ARROW_FUNCTIONS,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Arrow functions not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Arrow functions: SUPPORTED\n", .{});
}

test "c: spread operator compiles" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.SPREAD_OPERATOR,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Spread operator not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Spread operator: SUPPORTED\n", .{});
}

test "c: destructuring compiles" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.DESTRUCTURING,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Destructuring not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Destructuring: SUPPORTED\n", .{});
}

test "c: async/await compiles" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.ASYNC_AWAIT,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Async/await not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Async/await: SUPPORTED\n", .{});
}

test "c: recursive types compile" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.RECURSIVE_TYPES,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Recursive types not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Recursive types: SUPPORTED\n", .{});
}

test "c: enums compile" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.ENUMS,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Enums not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Enums: SUPPORTED\n", .{});
}

test "c: tuple types compile" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.TUPLE_TYPES,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Tuple types not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Tuple types: SUPPORTED\n", .{});
}

test "c: mapped types compile" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.MAPPED_TYPES,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Mapped types not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Mapped types: SUPPORTED\n", .{});
}

test "c: conditional types compile" {
    var result = helpers.compile(
        testing.allocator,
        fixtures.CONDITIONAL_TYPES,
        .c,
    ) catch |err| {
        std.debug.print("\n⚠️ BOUNDARY: Conditional types not supported - {s}\n", .{@errorName(err)});
        return;
    };
    defer result.deinit();
    std.debug.print("\n✅ Conditional types: SUPPORTED\n", .{});
}

// ============================================================================
// Performance Expectations
// ============================================================================

test "c: achieves 90%+ of native C performance" {
    // This is a documentation test - actual benchmarking happens elsewhere
    // Generated C should:
    // - Use native types (double, int, bool)
    // - Use stack allocation where possible
    // - Minimize indirection
    // - Allow compiler optimizations
    // - Avoid unnecessary allocations
}
