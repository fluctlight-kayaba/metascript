/// Compilation Performance Benchmarks
///
/// Measures compilation time overhead of normalization phase.
/// Target: <5% overhead (Phase 2 from roadmap)
///
/// These benchmarks use the existing framework (tests/framework/benchmark.zig)
/// to compare compilation WITH and WITHOUT normalization.

const std = @import("std");
const framework = @import("../framework/benchmark.zig");
const compile = @import("src").cli_compile;

// Test fixtures
const fixtures = struct {
    // Simple function - baseline test
    pub const SIMPLE_FUNCTION =
        \\function add(a: number, b: number): number {
        \\    return a + b;
        \\}
    ;

    // Class with properties
    pub const CLASS_WITH_PROPERTIES =
        \\class User {
        \\    name: string;
        \\    age: number;
        \\    email: string;
        \\}
    ;

    // Multiple classes
    pub const MULTIPLE_CLASSES =
        \\class User {
        \\    id: number;
        \\    name: string;
        \\}
        \\
        \\class Product {
        \\    id: number;
        \\    title: string;
        \\    price: number;
        \\}
        \\
        \\class Order {
        \\    id: number;
        \\    userId: number;
        \\    products: Product[];
        \\}
    ;

    // Complex module with multiple constructs
    pub const COMPLEX_MODULE =
        \\interface Identifiable {
        \\    id: number;
        \\}
        \\
        \\class User implements Identifiable {
        \\    id: number;
        \\    name: string;
        \\    email: string;
        \\
        \\    constructor(id: number, name: string, email: string) {
        \\        this.id = id;
        \\        this.name = name;
        \\        this.email = email;
        \\    }
        \\
        \\    greet(): string {
        \\        return "Hello, " + this.name;
        \\    }
        \\}
        \\
        \\function processUser(user: User): string {
        \\    return user.greet();
        \\}
    ;
};

/// Compile source WITHOUT normalization (baseline)
fn compileWithoutNormalize(allocator: std.mem.Allocator, source: []const u8) !void {
    // Write temp file
    const tmp_dir = std.testing.tmpDir(.{});
    var dir = tmp_dir.dir;
    defer tmp_dir.cleanup();

    const filename = "benchmark_test.ms";
    const file = try dir.createFile(filename, .{});
    defer file.close();
    try file.writeAll(source);

    // Get absolute path
    var path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const abs_path = try dir.realpath(filename, &path_buf);

    // Compile without normalization
    try compile.runWithArgs(allocator, abs_path, .js, null, false);
}

/// Compile source WITH normalization (current)
fn compileWithNormalize(allocator: std.mem.Allocator, source: []const u8) !void {
    // Write temp file
    const tmp_dir = std.testing.tmpDir(.{});
    var dir = tmp_dir.dir;
    defer tmp_dir.cleanup();

    const filename = "benchmark_test.ms";
    const file = try dir.createFile(filename, .{});
    defer file.close();
    try file.writeAll(source);

    // Get absolute path
    var path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const abs_path = try dir.realpath(filename, &path_buf);

    // Compile with normalization (default)
    try compile.runWithArgs(allocator, abs_path, .js, null, true);
}

// ============================================================================
// Benchmark Registration
// ============================================================================

pub fn registerBenchmarks(suite: *framework.BenchmarkSuite) !void {
    const allocator = suite.allocator;

    // Benchmark 1: Simple function compilation
    {
        const baseline_fn = struct {
            fn call() void {
                compileWithoutNormalize(
                    std.testing.allocator,
                    fixtures.SIMPLE_FUNCTION,
                ) catch unreachable;
            }
        }.call;

        const current_fn = struct {
            fn call() void {
                compileWithNormalize(
                    std.testing.allocator,
                    fixtures.SIMPLE_FUNCTION,
                ) catch unreachable;
            }
        }.call;

        const bench = try framework.runBenchmark(
            allocator,
            "compile_simple_function",
            "Compile simple function (baseline overhead test)",
            .e2e_compile,
            baseline_fn,
            current_fn,
            1, // Run once per sample (compilation is slow)
            5.0, // Target: <5% overhead
            "Tests minimal normalization overhead on simple function. " ++
                "Since transformations are NOPs, overhead should be <5%.",
        );
        try suite.add(bench);
    }

    // Benchmark 2: Class with properties
    {
        const baseline_fn = struct {
            fn call() void {
                compileWithoutNormalize(
                    std.testing.allocator,
                    fixtures.CLASS_WITH_PROPERTIES,
                ) catch unreachable;
            }
        }.call;

        const current_fn = struct {
            fn call() void {
                compileWithNormalize(
                    std.testing.allocator,
                    fixtures.CLASS_WITH_PROPERTIES,
                ) catch unreachable;
            }
        }.call;

        const bench = try framework.runBenchmark(
            allocator,
            "compile_class_with_properties",
            "Compile class with properties",
            .e2e_compile,
            baseline_fn,
            current_fn,
            1,
            5.0,
            "Tests normalization overhead on class declaration. " ++
                "Validates AST traversal cost.",
        );
        try suite.add(bench);
    }

    // Benchmark 3: Multiple classes
    {
        const baseline_fn = struct {
            fn call() void {
                compileWithoutNormalize(
                    std.testing.allocator,
                    fixtures.MULTIPLE_CLASSES,
                ) catch unreachable;
            }
        }.call;

        const current_fn = struct {
            fn call() void {
                compileWithNormalize(
                    std.testing.allocator,
                    fixtures.MULTIPLE_CLASSES,
                ) catch unreachable;
            }
        }.call;

        const bench = try framework.runBenchmark(
            allocator,
            "compile_multiple_classes",
            "Compile multiple classes",
            .e2e_compile,
            baseline_fn,
            current_fn,
            1,
            5.0,
            "Tests normalization overhead on medium-sized program. " ++
                "Validates that overhead doesn't scale linearly with AST size.",
        );
        try suite.add(bench);
    }

    // Benchmark 4: Complex module
    {
        const baseline_fn = struct {
            fn call() void {
                compileWithoutNormalize(
                    std.testing.allocator,
                    fixtures.COMPLEX_MODULE,
                ) catch unreachable;
            }
        }.call;

        const current_fn = struct {
            fn call() void {
                compileWithNormalize(
                    std.testing.allocator,
                    fixtures.COMPLEX_MODULE,
                ) catch unreachable;
            }
        }.call;

        const bench = try framework.runBenchmark(
            allocator,
            "compile_complex_module",
            "Compile complex module with interface, class, methods, constructor",
            .e2e_compile,
            baseline_fn,
            current_fn,
            1,
            5.0,
            "Tests normalization overhead on realistic code. " ++
                "Most comprehensive benchmark - validates real-world impact.",
        );
        try suite.add(bench);
    }
}

// ============================================================================
// Tests
// ============================================================================

test "compileWithoutNormalize: simple function" {
    const allocator = std.testing.allocator;
    try compileWithoutNormalize(allocator, fixtures.SIMPLE_FUNCTION);
}

test "compileWithNormalize: simple function" {
    const allocator = std.testing.allocator;
    try compileWithNormalize(allocator, fixtures.SIMPLE_FUNCTION);
}
