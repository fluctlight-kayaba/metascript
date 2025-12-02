// Metascript Benchmark Runner
// Unified entry point for all benchmarks
//
// Usage:
//   zig build benchmark              # Run all benchmarks
//   zig build benchmark-orc          # Run ORC benchmarks only
//   zig build benchmark-string       # Run string benchmarks only
//
// Or run directly:
//   zig run tests/benchmarks/main.zig -- [options]
//
// Options:
//   --category=<cat>   Filter by category (runtime_orc, runtime_string, etc.)
//   --json             Output JSON only
//   --markdown         Output Markdown only
//   --verbose          Verbose output
//   --help             Show help

const std = @import("std");
const framework = @import("benchmark");
const runner = @import("runner");
const reporter = @import("reporter");

// Import benchmark suites
const orc_benchmarks = @import("orc_benchmarks");
const string_benchmarks = @import("string_benchmarks");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Parse command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var config = runner.RunnerConfig{
        .output_format = .all,
        .verbose = false,
    };

    var show_help = false;

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            show_help = true;
        } else if (std.mem.eql(u8, arg, "--json")) {
            config.output_format = .json;
        } else if (std.mem.eql(u8, arg, "--markdown")) {
            config.output_format = .markdown;
        } else if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            config.verbose = true;
        } else if (std.mem.startsWith(u8, arg, "--category=")) {
            const cat_str = arg["--category=".len..];
            config.category_filter = parseCategory(cat_str);
        }
    }

    if (show_help) {
        printHelp();
        return;
    }

    // Print header
    const stdout = std.io.getStdOut().writer();
    try stdout.print("\n", .{});
    try stdout.print("╔═══════════════════════════════════════════════════════════╗\n", .{});
    try stdout.print("║         Metascript Benchmark Suite                        ║\n", .{});
    try stdout.print("║                                                           ║\n", .{});
    try stdout.print("║  Target: <15% overall overhead vs C                       ║\n", .{});
    try stdout.print("╚═══════════════════════════════════════════════════════════╝\n", .{});
    try stdout.print("\n", .{});

    // Create master suite
    var master_suite = framework.BenchmarkSuite.init(allocator, "Metascript Runtime Benchmarks");
    defer master_suite.deinit();

    // Register all benchmarks
    if (config.verbose) {
        try stdout.print("Registering ORC benchmarks...\n", .{});
    }
    try orc_benchmarks.registerBenchmarks(&master_suite);

    if (config.verbose) {
        try stdout.print("Registering String benchmarks...\n", .{});
    }
    try string_benchmarks.registerBenchmarks(&master_suite);

    // Create reporter
    var rep = reporter.Reporter.init(allocator);

    // Generate output based on format
    switch (config.output_format) {
        .console => {
            try rep.printSummary(&master_suite);
        },
        .json => {
            const json = try rep.generateJSON(&master_suite);
            defer allocator.free(json);
            try stdout.writeAll(json);
        },
        .markdown => {
            const md = try rep.generateMarkdown(&master_suite);
            defer allocator.free(md);
            try stdout.writeAll(md);
        },
        .all => {
            try rep.printSummary(&master_suite);

            // Ensure output directory exists
            std.fs.cwd().makeDir("benchmarks") catch |err| {
                if (err != error.PathAlreadyExists) return err;
            };

            // Write JSON report
            const json = try rep.generateJSON(&master_suite);
            defer allocator.free(json);
            const json_file = try std.fs.cwd().createFile("benchmarks/all_benchmarks.json", .{});
            defer json_file.close();
            try json_file.writeAll(json);

            // Write Markdown report
            const md = try rep.generateMarkdown(&master_suite);
            defer allocator.free(md);
            const md_file = try std.fs.cwd().createFile("benchmarks/all_benchmarks.md", .{});
            defer md_file.close();
            try md_file.writeAll(md);

            try stdout.print("\nReports written to:\n", .{});
            try stdout.print("  - benchmarks/all_benchmarks.json (for LLM consumption)\n", .{});
            try stdout.print("  - benchmarks/all_benchmarks.md (for humans)\n", .{});
        },
    }

    // Summary
    try stdout.print("\n", .{});

    const overall = master_suite.overallOverhead();
    const passed = master_suite.totalPassed();
    const failed = master_suite.totalFailed();
    const total = passed + failed;

    if (failed == 0 and overall <= 15.0) {
        try stdout.print("Result: ALL BENCHMARKS PASSED\n", .{});
        try stdout.print("Overall overhead: {d:.2}% (target: <15%)\n", .{overall});
    } else if (failed > 0) {
        try stdout.print("Result: {d}/{d} BENCHMARKS FAILED\n", .{ failed, total });
        try stdout.print("Overall overhead: {d:.2}%\n", .{overall});
        std.process.exit(1);
    } else {
        try stdout.print("Result: WARNING - High overhead\n", .{});
        try stdout.print("Overall overhead: {d:.2}% (target: <15%)\n", .{overall});
        std.process.exit(1);
    }

    try stdout.print("\n", .{});
}

fn parseCategory(cat_str: []const u8) ?framework.Category {
    inline for (@typeInfo(framework.Category).@"enum".fields) |field| {
        if (std.mem.eql(u8, cat_str, field.name)) {
            return @enumFromInt(field.value);
        }
    }
    return null;
}

fn printHelp() void {
    const help =
        \\Metascript Benchmark Runner
        \\
        \\Usage: zig run tests/benchmarks/main.zig -- [options]
        \\
        \\Options:
        \\  --help, -h            Show this help message
        \\  --verbose, -v         Verbose output
        \\  --json                Output JSON only (to stdout)
        \\  --markdown            Output Markdown only (to stdout)
        \\  --category=<cat>      Filter by category
        \\
        \\Categories:
        \\  runtime_orc           ORC memory management benchmarks
        \\  runtime_string        String operation benchmarks
        \\  runtime_collection    Collection benchmarks (future)
        \\  lexer                 Lexer performance (future)
        \\  parser                Parser performance (future)
        \\  backend_c             C backend benchmarks (future)
        \\  backend_js            JS backend benchmarks (future)
        \\  backend_erlang        Erlang backend benchmarks (future)
        \\
        \\Build commands:
        \\  zig build benchmark          Run all benchmarks
        \\  zig build benchmark-orc      Run ORC benchmarks only
        \\  zig build benchmark-string   Run string benchmarks only
        \\
        \\Output files (when using --all or default):
        \\  benchmarks/all_benchmarks.json   JSON report for LLM consumption
        \\  benchmarks/all_benchmarks.md     Markdown report for humans
        \\
        \\Performance targets:
        \\  - Overall: <15% overhead vs C
        \\  - ORC: <10% overhead for simple allocation
        \\  - String: <10% overhead for creation, <60% for concat (optimization planned)
        \\
    ;
    std.debug.print("{s}\n", .{help});
}

// =============================================================================
// Tests
// =============================================================================

test "parseCategory: valid categories" {
    try std.testing.expectEqual(framework.Category.runtime_orc, parseCategory("runtime_orc").?);
    try std.testing.expectEqual(framework.Category.runtime_string, parseCategory("runtime_string").?);
    try std.testing.expectEqual(framework.Category.lexer, parseCategory("lexer").?);
}

test "parseCategory: invalid category returns null" {
    try std.testing.expect(parseCategory("invalid") == null);
    try std.testing.expect(parseCategory("") == null);
}
