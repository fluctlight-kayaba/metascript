const std = @import("std");
const benchmark_framework = @import("benchmark");
const reporter = @import("reporter");
const orc = @import("orc");

// Baseline: Pure C malloc/free (no RC tracking)
// Returns pointer value to prevent optimization
fn baselineSimpleAlloc() usize {
    const ptr = std.c.malloc(@sizeOf(i32)) orelse unreachable;
    const addr = @intFromPtr(ptr);
    std.c.free(ptr);
    return addr;
}

// Current: ORC allocation with RC tracking
// Returns pointer value to prevent optimization
fn orcSimpleAlloc() usize {
    const allocator = std.heap.c_allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    const ptr = runtime.alloc(i32) catch unreachable;
    ptr.* = 42;
    const addr = @intFromPtr(ptr);
    runtime.decref(ptr);
    return addr;
}

// Baseline: 100K malloc/free
// Returns sum of addresses to prevent optimization
fn baseline100KAllocs() usize {
    var sum: usize = 0;
    var i: usize = 0;
    while (i < 100_000) : (i += 1) {
        const ptr = std.c.malloc(@sizeOf(i32)) orelse unreachable;
        sum +%= @intFromPtr(ptr);
        std.c.free(ptr);
    }
    return sum;
}

// Current: 100K ORC allocations
// Returns sum of addresses to prevent optimization
fn orc100KAllocs() usize {
    const allocator = std.heap.c_allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    var sum: usize = 0;
    var i: usize = 0;
    while (i < 100_000) : (i += 1) {
        const ptr = runtime.alloc(i32) catch unreachable;
        ptr.* = @intCast(i);
        sum +%= @intFromPtr(ptr);
        runtime.decref(ptr);
    }
    return sum;
}

pub fn registerBenchmarks(suite: *benchmark_framework.BenchmarkSuite) !void {
    // Benchmark 1: Simple allocation
    const bench1 = try benchmark_framework.runBenchmark(
        suite.allocator,
        "ORC: Simple allocation",
        "Single i32 allocation with RC tracking",
        .runtime_orc,
        baselineSimpleAlloc,
        orcSimpleAlloc,
        1_000_000,
        10.0, // Max 10% overhead
        \\Measures ORC allocation performance for a single i32 object.
        \\This validates our 6-16% overhead target for memory management.
        \\Baseline is pure C malloc/free with no RC tracking.
        \\Expected: <10% overhead. If > 10%, investigate RefHeader size
        \\(currently 8 bytes) or allocation alignment overhead.
    ,
    );
    try suite.add(bench1);

    // Benchmark 2: 100K allocations
    const bench2 = try benchmark_framework.runBenchmark(
        suite.allocator,
        "ORC: 100K allocations",
        "Rapid allocation/deallocation of 100K objects",
        .runtime_orc,
        baseline100KAllocs,
        orc100KAllocs,
        10, // Run 10 times
        15.0, // Max 15% overhead
        \\Measures ORC allocation performance for 100K small objects (i32).
        \\This validates our 6-16% overhead target under memory pressure.
        \\Current overhead should be within 15%. If overhead exceeds 20%,
        \\investigate: (1) RefHeader size (currently 8 bytes),
        \\(2) Allocation alignment overhead, (3) Memory fragmentation.
        \\Baseline is pure C malloc/free with no RC tracking.
    ,
    );
    try suite.add(bench2);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Create benchmark suite
    var suite = benchmark_framework.BenchmarkSuite.init(allocator, "ORC Runtime Benchmarks");
    defer suite.deinit();

    // Register benchmarks
    try registerBenchmarks(&suite);

    // Create reporter
    var rep = reporter.Reporter.init(allocator);

    // Print summary to console
    try rep.printSummary(&suite);

    // Ensure output directory exists
    std.fs.cwd().makeDir("benchmarks") catch |err| {
        if (err != error.PathAlreadyExists) {
            std.debug.print("\nWarning: Could not create benchmarks directory: {}\n", .{err});
            std.debug.print("Reports not written to disk.\n\n", .{});
            return;
        }
    };

    // Generate JSON report
    const json = try rep.generateJSON(&suite);
    defer allocator.free(json);

    // Write JSON to file
    const json_file = std.fs.cwd().createFile("benchmarks/orc_benchmarks.json", .{}) catch |err| {
        std.debug.print("\nWarning: Could not write JSON report: {}\n", .{err});
        return;
    };
    defer json_file.close();
    try json_file.writeAll(json);

    // Generate Markdown report
    const markdown = try rep.generateMarkdown(&suite);
    defer allocator.free(markdown);

    // Write Markdown to file
    const md_file = std.fs.cwd().createFile("benchmarks/orc_benchmarks.md", .{}) catch |err| {
        std.debug.print("\nWarning: Could not write Markdown report: {}\n", .{err});
        return;
    };
    defer md_file.close();
    try md_file.writeAll(markdown);

    std.debug.print("\nReports generated:\n", .{});
    std.debug.print("  - benchmarks/orc_benchmarks.json (for LLM)\n", .{});
    std.debug.print("  - benchmarks/orc_benchmarks.md (for humans)\n\n", .{});

    // Exit with error code if any benchmarks failed
    if (suite.totalFailed() > 0) {
        std.process.exit(1);
    }
}
