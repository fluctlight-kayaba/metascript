const std = @import("std");
const benchmark_framework = @import("benchmark");
const reporter = @import("reporter");
const msString = @import("string").msString;

// =============================================================================
// Baseline C implementations (pure allocation, no RC tracking)
// All functions return values to prevent compiler optimization
// =============================================================================

/// Baseline: Create string with malloc
/// Returns pointer address to prevent optimization
fn baselineStringCreate() usize {
    const data = "hello world";
    const len = data.len;

    // Pure C allocation: malloc + memcpy
    const ptr = std.c.malloc(len + 1) orelse unreachable;
    @memcpy(@as([*]u8, @ptrCast(ptr))[0..len], data);
    @as([*]u8, @ptrCast(ptr))[len] = 0;
    const addr = @intFromPtr(ptr);
    std.c.free(ptr);
    return addr;
}

/// Current: Create msString (with ORC header)
/// Returns pointer address to prevent optimization
fn msStringCreate() usize {
    const allocator = std.heap.c_allocator;
    const str = msString.new(allocator, "hello world") catch unreachable;
    const addr = @intFromPtr(str);
    str.decref(allocator);
    return addr;
}

/// Baseline: String concatenation with malloc
/// Returns pointer address to prevent optimization
fn baselineConcat() usize {
    const a = "hello ";
    const b = "world";
    const total_len = a.len + b.len;

    // Allocate and concat
    const ptr = std.c.malloc(total_len + 1) orelse unreachable;
    const dest = @as([*]u8, @ptrCast(ptr));
    @memcpy(dest[0..a.len], a);
    @memcpy(dest[a.len .. a.len + b.len], b);
    dest[total_len] = 0;
    const addr = @intFromPtr(ptr);
    std.c.free(ptr);
    return addr;
}

/// Current: msString concatenation
/// Returns pointer address to prevent optimization
fn msStringConcat() usize {
    const allocator = std.heap.c_allocator;
    const a = msString.new(allocator, "hello ") catch unreachable;
    const b = msString.new(allocator, "world") catch unreachable;
    const result = msString.concat(allocator, a, b) catch unreachable;

    const addr = @intFromPtr(result);
    a.decref(allocator);
    b.decref(allocator);
    result.decref(allocator);
    return addr;
}

/// Baseline: Substring with malloc
/// Returns pointer address to prevent optimization
fn baselineSubstring() usize {
    const data = "hello world";
    const start: usize = 0;
    const end: usize = 5;
    const sub_len = end - start;

    const ptr = std.c.malloc(sub_len + 1) orelse unreachable;
    @memcpy(@as([*]u8, @ptrCast(ptr))[0..sub_len], data[start..end]);
    @as([*]u8, @ptrCast(ptr))[sub_len] = 0;
    const addr = @intFromPtr(ptr);
    std.c.free(ptr);
    return addr;
}

/// Current: msString substring
/// Returns pointer address to prevent optimization
fn msStringSubstring() usize {
    const allocator = std.heap.c_allocator;
    const str = msString.new(allocator, "hello world") catch unreachable;
    const sub = str.substring(allocator, 0, 5) catch unreachable;

    const addr = @intFromPtr(sub);
    str.decref(allocator);
    sub.decref(allocator);
    return addr;
}

/// Baseline: 1000 string creations
/// Returns sum of addresses to prevent optimization
fn baseline1000Creates() usize {
    var sum: usize = 0;
    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        const ptr = std.c.malloc(12) orelse unreachable;
        @memcpy(@as([*]u8, @ptrCast(ptr))[0..11], "hello world");
        @as([*]u8, @ptrCast(ptr))[11] = 0;
        sum +%= @intFromPtr(ptr);
        std.c.free(ptr);
    }
    return sum;
}

/// Current: 1000 msString creations
/// Returns sum of addresses to prevent optimization
fn msString1000Creates() usize {
    const allocator = std.heap.c_allocator;
    var sum: usize = 0;
    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        const str = msString.new(allocator, "hello world") catch unreachable;
        sum +%= @intFromPtr(str);
        str.decref(allocator);
    }
    return sum;
}

// =============================================================================
// Benchmark Registration
// =============================================================================

pub fn registerBenchmarks(suite: *benchmark_framework.BenchmarkSuite) !void {
    // Benchmark 1: Simple string creation
    const bench1 = try benchmark_framework.runBenchmark(
        suite.allocator,
        "String: Creation",
        "Create and free a single string",
        .runtime_string,
        baselineStringCreate,
        msStringCreate,
        1_000_000,
        10.0, // Max 10% overhead
        \\Measures msString creation performance for a single "hello world" string.
        \\Includes ORC RefHeader allocation and UTF-8 validation.
        \\Baseline is pure C malloc/memcpy/free with no RC tracking.
        \\Expected: <10% overhead. If > 15%, investigate UTF-8 validation cost
        \\or memory layout alignment.
    ,
    );
    try suite.add(bench1);

    // Benchmark 2: String concatenation
    const bench2 = try benchmark_framework.runBenchmark(
        suite.allocator,
        "String: Concatenation",
        "Concatenate two strings",
        .runtime_string,
        baselineConcat,
        msStringConcat,
        100_000,
        60.0, // Allow higher overhead (known issue per design doc)
        \\Measures msString concatenation performance.
        \\Creates two strings, concatenates, and frees all three.
        \\Current overhead target is <60% (optimization planned Week 5-6).
        \\Baseline is pure C malloc with memcpy. Future optimizations:
        \\(1) String interning (30-40% improvement), (2) SSO for small strings,
        \\(3) Rope data structure for large strings.
    ,
    );
    try suite.add(bench2);

    // Benchmark 3: Substring extraction
    const bench3 = try benchmark_framework.runBenchmark(
        suite.allocator,
        "String: Substring",
        "Extract substring from string",
        .runtime_string,
        baselineSubstring,
        msStringSubstring,
        500_000,
        15.0, // Max 15% overhead
        \\Measures substring extraction performance.
        \\Creates a string, extracts "hello" (first 5 chars), frees both.
        \\Expected: <15% overhead. Substring creates new allocation.
        \\Future optimization: Copy-on-write substrings (share underlying buffer).
    ,
    );
    try suite.add(bench3);

    // Benchmark 4: String equality (comparison only, no allocation in loop)
    // Note: Skipping this benchmark as it compares static vs allocated strings
    // which is an unfair comparison. The allocation benchmarks above already
    // capture allocation overhead. Pure comparison is identical to baseline.

    // Benchmark 5: Bulk string operations (1000 creates)
    const bench5 = try benchmark_framework.runBenchmark(
        suite.allocator,
        "String: 1000 Creates",
        "Create and free 1000 strings",
        .runtime_string,
        baseline1000Creates,
        msString1000Creates,
        100, // Run 100 iterations of 1000 creates each
        20.0, // Max 20% overhead
        \\Measures msString performance under bulk allocation pressure.
        \\Creates and frees 1000 "hello world" strings.
        \\Expected: <20% overhead. This tests allocator efficiency
        \\under repeated small allocations with ORC headers.
    ,
    );
    try suite.add(bench5);
}

// =============================================================================
// Main Entry Point
// =============================================================================

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Create benchmark suite
    var suite = benchmark_framework.BenchmarkSuite.init(allocator, "String Runtime Benchmarks");
    defer suite.deinit();

    // Register benchmarks
    try registerBenchmarks(&suite);

    // Create reporter
    var rep = reporter.Reporter.init(allocator);

    // Print summary to console
    try rep.printSummary(&suite);

    // Generate JSON report
    const json = try rep.generateJSON(&suite);
    defer allocator.free(json);

    // Ensure output directory exists
    std.fs.cwd().makeDir("benchmarks") catch |err| {
        if (err != error.PathAlreadyExists) {
            std.debug.print("\nWarning: Could not create benchmarks directory: {}\n", .{err});
            std.debug.print("Reports not written to disk.\n\n", .{});
            return;
        }
    };

    // Write JSON to file
    const json_file = std.fs.cwd().createFile("benchmarks/string_benchmarks.json", .{}) catch |err| {
        std.debug.print("\nWarning: Could not write JSON report: {}\n", .{err});
        return;
    };
    defer json_file.close();
    try json_file.writeAll(json);

    // Generate Markdown report
    const markdown = try rep.generateMarkdown(&suite);
    defer allocator.free(markdown);

    // Write Markdown to file
    const md_file = std.fs.cwd().createFile("benchmarks/string_benchmarks.md", .{}) catch |err| {
        std.debug.print("\nWarning: Could not write Markdown report: {}\n", .{err});
        return;
    };
    defer md_file.close();
    try md_file.writeAll(markdown);

    std.debug.print("\nReports generated:\n", .{});
    std.debug.print("  - benchmarks/string_benchmarks.json (for LLM)\n", .{});
    std.debug.print("  - benchmarks/string_benchmarks.md (for humans)\n\n", .{});

    // Exit with error code if any benchmarks failed
    if (suite.totalFailed() > 0) {
        std.process.exit(1);
    }
}
