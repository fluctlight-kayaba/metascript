/// C Backend Execution Tests
///
/// These tests verify that generated C code not only compiles but produces
/// CORRECT OUTPUT when executed. This is the ultimate validation.
///
/// Each test:
/// 1. Compiles MetaScript source to C
/// 2. Compiles C with zig cc
/// 3. Runs the binary
/// 4. Verifies the output matches expected value

const std = @import("std");
const testing = std.testing;
const helpers = @import("backend_test_helpers.zig");

// ============================================================================
// Test Fixtures with Expected Outputs
// ============================================================================

const TestCase = struct {
    name: []const u8,
    source: []const u8,
    expected_output: []const u8,
};

// Basic arithmetic
const BASIC_ARITHMETIC = TestCase{
    .name = "basic_arithmetic",
    .source =
    \\function add(a: number, b: number): number {
    \\    return a + b;
    \\}
    \\
    \\function main(): number {
    \\    const result = add(10, 32);
    \\    console.log(result);
    \\    return 0;
    \\}
    ,
    .expected_output = "42",
};

// Fibonacci
const FIBONACCI_TEST = TestCase{
    .name = "fibonacci",
    .source =
    \\function fibonacci(n: number): number {
    \\    if (n <= 1) {
    \\        return n;
    \\    }
    \\    return fibonacci(n - 1) + fibonacci(n - 2);
    \\}
    \\
    \\function main(): number {
    \\    console.log(fibonacci(10));
    \\    return 0;
    \\}
    ,
    .expected_output = "55",
};

// Factorial recursive
const FACTORIAL_RECURSIVE_TEST = TestCase{
    .name = "factorial_recursive",
    .source =
    \\function factorial(n: number): number {
    \\    if (n <= 1) {
    \\        return 1;
    \\    }
    \\    return n * factorial(n - 1);
    \\}
    \\
    \\function main(): number {
    \\    console.log(factorial(5));
    \\    return 0;
    \\}
    ,
    .expected_output = "120",
};

// While loop
const WHILE_LOOP_TEST = TestCase{
    .name = "while_loop",
    .source =
    \\function sumToN(n: number): number {
    \\    let sum = 0;
    \\    let i = 1;
    \\    while (i <= n) {
    \\        sum = sum + i;
    \\        i = i + 1;
    \\    }
    \\    return sum;
    \\}
    \\
    \\function main(): number {
    \\    console.log(sumToN(10));
    \\    return 0;
    \\}
    ,
    .expected_output = "55",
};

// For loop
const FOR_LOOP_TEST = TestCase{
    .name = "for_loop",
    .source =
    \\function sumRange(n: number): number {
    \\    let sum = 0;
    \\    for (let i = 1; i <= n; i = i + 1) {
    \\        sum = sum + i;
    \\    }
    \\    return sum;
    \\}
    \\
    \\function main(): number {
    \\    console.log(sumRange(100));
    \\    return 0;
    \\}
    ,
    .expected_output = "5050",
};

// Nested conditionals
const NESTED_IF_TEST = TestCase{
    .name = "nested_if",
    .source =
    \\function classify(n: number): number {
    \\    if (n < 0) {
    \\        return -1;
    \\    } else if (n === 0) {
    \\        return 0;
    \\    } else if (n < 10) {
    \\        return 1;
    \\    } else {
    \\        return 2;
    \\    }
    \\}
    \\
    \\function main(): number {
    \\    console.log(classify(-5));
    \\    console.log(classify(0));
    \\    console.log(classify(5));
    \\    console.log(classify(100));
    \\    return 0;
    \\}
    ,
    .expected_output = "-1\n0\n1\n2",
};

// Class with methods
const CLASS_METHOD_TEST = TestCase{
    .name = "class_method",
    .source =
    \\class Calculator {
    \\    value: number;
    \\
    \\    add(n: number): number {
    \\        this.value = this.value + n;
    \\        return this.value;
    \\    }
    \\
    \\    multiply(n: number): number {
    \\        this.value = this.value * n;
    \\        return this.value;
    \\    }
    \\}
    \\
    \\function main(): number {
    \\    const calc = new Calculator();
    \\    calc.value = 10;
    \\    calc.add(5);
    \\    calc.multiply(2);
    \\    console.log(calc.value);
    \\    return 0;
    \\}
    ,
    .expected_output = "30",
};

// Multiple functions calling each other
const MULTI_FUNCTION_TEST = TestCase{
    .name = "multi_function",
    .source =
    \\function doubleNum(x: number): number {
    \\    return x * 2;
    \\}
    \\
    \\function tripleNum(x: number): number {
    \\    return x * 3;
    \\}
    \\
    \\function compute(x: number): number {
    \\    return doubleNum(x) + tripleNum(x);
    \\}
    \\
    \\function main(): number {
    \\    console.log(compute(10));
    \\    return 0;
    \\}
    ,
    .expected_output = "50",
};

// isPrime algorithm
const IS_PRIME_TEST = TestCase{
    .name = "is_prime",
    .source =
    \\function isPrime(n: number): boolean {
    \\    if (n <= 1) return false;
    \\    if (n <= 3) return true;
    \\    if (n % 2 === 0) return false;
    \\    let i = 3;
    \\    while (i * i <= n) {
    \\        if (n % i === 0) {
    \\            return false;
    \\        }
    \\        i = i + 2;
    \\    }
    \\    return true;
    \\}
    \\
    \\function main(): number {
    \\    if (isPrime(17)) {
    \\        console.log(1);
    \\    } else {
    \\        console.log(0);
    \\    }
    \\    if (isPrime(18)) {
    \\        console.log(1);
    \\    } else {
    \\        console.log(0);
    \\    }
    \\    return 0;
    \\}
    ,
    .expected_output = "1\n0",
};

// GCD algorithm
const GCD_TEST = TestCase{
    .name = "gcd",
    .source =
    \\function gcd(a: number, b: number): number {
    \\    while (b !== 0) {
    \\        const temp = b;
    \\        b = a % b;
    \\        a = temp;
    \\    }
    \\    return a;
    \\}
    \\
    \\function main(): number {
    \\    console.log(gcd(48, 18));
    \\    return 0;
    \\}
    ,
    .expected_output = "6",
};

// ============================================================================
// Test Runner
// ============================================================================

fn runExecutionTest(allocator: std.mem.Allocator, test_case: TestCase) !void {
    const temp_dir = "/tmp/metascript_exec_test";
    std.fs.makeDirAbsolute(temp_dir) catch {};

    // 1. Compile MetaScript to C
    var result = helpers.compile(allocator, test_case.source, .c) catch |err| {
        std.debug.print("\n❌ {s}: Failed to compile to C - {s}\n", .{ test_case.name, @errorName(err) });
        return err;
    };
    defer result.deinit();

    if (!result.success) {
        std.debug.print("\n❌ {s}: Compilation failed - {s}\n", .{ test_case.name, result.error_message orelse "unknown" });
        return error.CompilationFailed;
    }

    // 2. Write C code to file
    const c_file = try std.fmt.allocPrint(allocator, "{s}/{s}.c", .{ temp_dir, test_case.name });
    defer allocator.free(c_file);

    const bin_file = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ temp_dir, test_case.name });
    defer allocator.free(bin_file);

    {
        const file = try std.fs.createFileAbsolute(c_file, .{});
        defer file.close();
        try file.writeAll(result.output);
    }

    // 3. Compile C with zig cc
    const compile_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "zig", "cc", "-I", "src/runtime", c_file, "-o", bin_file, "-lm" },
    });
    defer allocator.free(compile_result.stdout);
    defer allocator.free(compile_result.stderr);

    if (compile_result.term.Exited != 0) {
        std.debug.print("\n❌ {s}: C compilation failed\n", .{test_case.name});
        std.debug.print("stderr: {s}\n", .{compile_result.stderr});
        std.debug.print("Generated C:\n{s}\n", .{result.output});
        return error.CCompilationFailed;
    }

    // 4. Run the binary
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{bin_file},
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // 5. Verify output
    const actual_output = std.mem.trim(u8, run_result.stdout, &[_]u8{ ' ', '\n', '\r', '\t' });
    const expected = std.mem.trim(u8, test_case.expected_output, &[_]u8{ ' ', '\n', '\r', '\t' });

    if (!std.mem.eql(u8, actual_output, expected)) {
        std.debug.print("\n❌ {s}: Output mismatch\n", .{test_case.name});
        std.debug.print("   Expected: '{s}'\n", .{expected});
        std.debug.print("   Actual:   '{s}'\n", .{actual_output});
        std.debug.print("Generated C:\n{s}\n", .{result.output});
        return error.OutputMismatch;
    }

    std.debug.print("✅ {s}: PASS (output: {s})\n", .{ test_case.name, actual_output });
}

// ============================================================================
// Tests
// ============================================================================

test "exec: basic arithmetic" {
    try runExecutionTest(testing.allocator, BASIC_ARITHMETIC);
}

test "exec: fibonacci" {
    try runExecutionTest(testing.allocator, FIBONACCI_TEST);
}

test "exec: factorial recursive" {
    try runExecutionTest(testing.allocator, FACTORIAL_RECURSIVE_TEST);
}

test "exec: while loop sum" {
    try runExecutionTest(testing.allocator, WHILE_LOOP_TEST);
}

test "exec: for loop sum" {
    try runExecutionTest(testing.allocator, FOR_LOOP_TEST);
}

test "exec: nested conditionals" {
    try runExecutionTest(testing.allocator, NESTED_IF_TEST);
}

test "exec: class with methods" {
    try runExecutionTest(testing.allocator, CLASS_METHOD_TEST);
}

test "exec: multiple functions" {
    try runExecutionTest(testing.allocator, MULTI_FUNCTION_TEST);
}

test "exec: isPrime algorithm" {
    try runExecutionTest(testing.allocator, IS_PRIME_TEST);
}

test "exec: GCD algorithm" {
    try runExecutionTest(testing.allocator, GCD_TEST);
}
