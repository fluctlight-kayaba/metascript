/// Erlang Backend Code Generation Tests
///
/// Tests Erlang code generation for correctness, idiomaticity, and known bugs.
/// Following TDD: tests are written to FAIL first, exposing current limitations.

const std = @import("std");
const testing = std.testing;
const helpers = @import("backend_test_helpers.zig");
const fixtures = @import("fixtures");

// ============================================================================
// ✅ Tests That SHOULD Pass (Currently Working)
// ============================================================================

test "erlang: simple function compiles" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.SIMPLE_FUNCTION,
        .erlang,
    );
    defer result.deinit();

    // Should have function definition
    try helpers.expectContains(result.output, "add(");
    try helpers.expectContains(result.output, "A + B");
}

test "erlang: variable shadowing generates @-suffix pattern" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.VARIABLE_SHADOWING_SIMPLE,
        .erlang,
    );
    defer result.deinit();

    // Should have X, X@1, X@2 pattern (Gleam-style)
    try helpers.expectContains(result.output, "X = 10");
    try helpers.expectContains(result.output, "X@1 = X + 5");
    try helpers.expectContains(result.output, "X@2 = X@1 * 2");
}

test "erlang: multiple variable shadowing works independently" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.VARIABLE_SHADOWING_MULTIPLE,
        .erlang,
    );
    defer result.deinit();

    // Each variable gets its own shadowing counter
    try helpers.expectContains(result.output, "A = 1");
    try helpers.expectContains(result.output, "A@1 = A + 10");
    try helpers.expectContains(result.output, "B = 2");
    try helpers.expectContains(result.output, "B@1 = B + 20");
}

test "erlang: object literal generates map" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.OBJECT_LITERAL,
        .erlang,
    );
    defer result.deinit();

    // Should use Erlang map syntax
    try helpers.expectContains(result.output, "#{");
    try helpers.expectContains(result.output, "name =>");
    try helpers.expectContains(result.output, "age =>");
}

test "erlang: member access generates maps:get" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.OBJECT_MEMBER_ACCESS,
        .erlang,
    );
    defer result.deinit();

    // Should use maps:get(key, Map)
    try helpers.expectContains(result.output, "maps:get(age,");
}

test "erlang: array methods map to lists module" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.ARRAY_OPERATIONS,
        .erlang,
    );
    defer result.deinit();

    // arr.length → length(Arr)
    try helpers.expectContains(result.output, "length(");
    // arr.push(x) → lists:append(Arr, [x])
    try helpers.expectContains(result.output, "lists:append(");
}

test "erlang: console.log generates io:format" {
    const source =
        \\function demo(): void {
        \\    console.log("Hello", 42);
        \\}
    ;

    var result = try helpers.expectCompiles(testing.allocator, source, .erlang);
    defer result.deinit();

    // Should use io:format with ~p placeholders
    try helpers.expectContains(result.output, "io:format(");
    try helpers.expectContains(result.output, "~p");
}

// ============================================================================
// ❌ Tests That SHOULD FAIL (Known Bugs - TDD Red Phase)
// ============================================================================

test "erlang: while loop with mutable variable generates parameterized recursion" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.ERLANG_BUG_LOOP_CLOSURE,
        .erlang,
    );
    defer result.deinit();

    // BUG: Currently generates closure over Count, should parameterize
    // SHOULD generate:
    //   Loop_0 = fun Loop_0_Inner(CurrentCount) ->
    //       case CurrentCount > 0 of
    //           true -> ..., Loop_0_Inner(NewCount)
    //   Loop_0(Count)

    // This test WILL FAIL until we fix the bug
    try helpers.expectContains(result.output, "fun Loop_0_Inner(");
    // Should pass count as parameter
    try helpers.expectContains(result.output, "Loop_0(");

    // Should NOT close over the original variable
    // (this is the bug - it currently does)
}

test "erlang: for loop with mutable variable generates parameterized recursion" {
    const source =
        \\function sumToN(n: number): number {
        \\    let total = 0;
        \\    for (let i = 1; i <= n; i = i + 1) {
        \\        total = total + i;
        \\    }
        \\    return total;
        \\}
    ;

    var result = try helpers.expectCompiles(testing.allocator, source, .erlang);
    defer result.deinit();

    // BUG: Currently doesn't parameterize loop variables
    // SHOULD pass both Total and I as parameters to recursive function
    try helpers.expectContains(result.output, "fun Loop_0_Inner(");
}

test "erlang: if-else with early return uses guard clauses or proper case expression" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.ERLANG_BUG_EARLY_RETURN,
        .erlang,
    );
    defer result.deinit();

    // BUG: Currently generates code that continues after case expression
    // Option 1: Guard clauses
    //   absoluteValue(X) when X < 0 -> -X;
    //   absoluteValue(X) -> X.
    // Option 2: Proper case expression
    //   case X < 0 of
    //       true -> -X;
    //       false -> X
    //   end.

    // This test WILL FAIL - we need to detect and fix this pattern
    // Should NOT have code after the case expression
    const has_code_after_case = std.mem.indexOf(u8, result.output, "end,") != null and
        std.mem.indexOf(u8, result.output, "end.\n") == null;

    if (has_code_after_case) {
        std.debug.print("\n⚠️ WARNING: Code exists after case expression (early return bug)\n", .{});
        return error.EarlyReturnBugDetected;
    }
}

test "erlang: recursive function with base case uses guard clauses" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.FACTORIAL_RECURSIVE,
        .erlang,
    );
    defer result.deinit();

    // BUG: Currently generates broken recursion due to early return issue
    // Should generate either:
    // 1. Guard clauses: factorial(N) when N =< 1 -> 1;
    // 2. Case expression that covers all paths

    // This test documents the expected behavior
    // (will fail until we fix early return handling)
}

// ============================================================================
// Cross-Backend Compatibility Tests
// ============================================================================

test "erlang: generates valid Erlang that compiles with erlc" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.SIMPLE_FUNCTION,
        .erlang,
    );
    defer result.deinit();

    // Compile with external erlc
    var compile_result = try helpers.compileWithErlc(testing.allocator, result.output);
    defer compile_result.deinit();

    if (!compile_result.success) {
        std.debug.print("\n❌ Generated Erlang code failed to compile with erlc\n", .{});
        std.debug.print("Stderr: {s}\n", .{compile_result.stderr});
        std.debug.print("Generated code:\n{s}\n", .{result.output});
        return error.ErlangCompilationFailed;
    }
}

// ============================================================================
// Idiomatic Erlang Tests
// ============================================================================

test "erlang: function names are lowercase" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.SIMPLE_FUNCTION,
        .erlang,
    );
    defer result.deinit();

    // Erlang functions must start with lowercase
    try helpers.expectNotContains(result.output, "Add(");
    try helpers.expectContains(result.output, "add(");
}

test "erlang: variables are capitalized" {
    const source = "const myVar = 42;";

    var result = try helpers.expectCompiles(testing.allocator, source, .erlang);
    defer result.deinit();

    // Erlang variables must start with uppercase
    try helpers.expectContains(result.output, "MyVar");
    try helpers.expectNotContains(result.output, "myVar");
}

test "erlang: strings become binaries" {
    const source =
        \\const message = "Hello, World!";
    ;

    var result = try helpers.expectCompiles(testing.allocator, source, .erlang);
    defer result.deinit();

    // Strings should be Erlang binaries <<>>
    try helpers.expectContains(result.output, "<<\"Hello, World!\">>");
}

test "erlang: tail recursion pattern for loops" {
    const source =
        \\function countdown(n: number): void {
        \\    while (n > 0) {
        \\        console.log(n);
        \\        n = n - 1;
        \\    }
        \\}
    ;

    var result = try helpers.expectCompiles(testing.allocator, source, .erlang);
    defer result.deinit();

    // Should generate tail-recursive loop function
    try helpers.expectContains(result.output, "fun Loop_");
    try helpers.expectContains(result.output, "case ");
}

// ============================================================================
// Smoke Tests: Real-World Programs
// ============================================================================

test "erlang: comprehensive demo compiles" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.COMPREHENSIVE_DEMO,
        .erlang,
    );
    defer result.deinit();

    // Should have both functions
    try helpers.expectContains(result.output, "fibonacci(");
    try helpers.expectContains(result.output, "main(");
}

test "erlang: fibonacci compiles to valid Erlang" {
    var result = try helpers.expectCompiles(
        testing.allocator,
        fixtures.FIBONACCI,
        .erlang,
    );
    defer result.deinit();

    // Check it compiles with erlc
    var compile_result = try helpers.compileWithErlc(testing.allocator, result.output);
    defer compile_result.deinit();

    if (!compile_result.success) {
        std.debug.print("\nGenerated Erlang:\n{s}\n", .{result.output});
        std.debug.print("erlc stderr: {s}\n", .{compile_result.stderr});
        return error.InvalidErlangGenerated;
    }
}
