const std = @import("std");
const benchmark_mod = @import("benchmark");
const Benchmark = benchmark_mod.Benchmark;
const BenchmarkSuite = benchmark_mod.BenchmarkSuite;

pub const Reporter = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Reporter {
        return .{ .allocator = allocator };
    }

    /// Generate JSON report (for LLM consumption)
    pub fn generateJSON(self: *Reporter, suite: *const BenchmarkSuite) ![]u8 {
        var output = std.ArrayList(u8).init(self.allocator);
        const writer = output.writer();

        try writer.writeAll("{\n");
        try writer.print("  \"name\": \"{s}\",\n", .{suite.name});
        try writer.print("  \"timestamp\": {d},\n", .{std.time.timestamp()});
        try writer.print("  \"overall_overhead_percent\": {d:.2},\n", .{suite.overallOverhead()});
        try writer.print("  \"total_passed\": {d},\n", .{suite.totalPassed()});
        try writer.print("  \"total_failed\": {d},\n", .{suite.totalFailed()});
        try writer.writeAll("  \"benchmarks\": [\n");

        for (suite.benchmarks.items, 0..) |bench, idx| {
            try writer.writeAll("    {\n");
            try writer.print("      \"name\": \"{s}\",\n", .{bench.name});
            try writer.print("      \"description\": \"{s}\",\n", .{bench.description});
            try writer.print("      \"category\": \"{s}\",\n", .{bench.category.toString()});
            try writer.print("      \"timestamp\": {d},\n", .{bench.timestamp});
            try writer.print("      \"baseline_c_ns\": {d},\n", .{bench.baseline_c_ns});
            try writer.print("      \"current_ns\": {d},\n", .{bench.current_ns});
            try writer.print("      \"overhead_percent\": {d:.2},\n", .{bench.overhead_percent});
            try writer.print("      \"memory_overhead_percent\": {d:.2},\n", .{bench.memory_overhead_percent});
            try writer.print("      \"max_overhead_percent\": {d:.2},\n", .{bench.max_overhead_percent});
            try writer.print("      \"passed\": {s},\n", .{if (bench.passed) "true" else "false"});
            try writer.print("      \"iterations_used\": {d},\n", .{bench.iterations_used});
            try writer.print("      \"samples_collected\": {d},\n", .{bench.samples_collected});
            try writer.print("      \"llm_context\": \"{s}\"\n", .{bench.llm_context});
            try writer.writeAll("    }");
            if (idx < suite.benchmarks.items.len - 1) {
                try writer.writeAll(",\n");
            } else {
                try writer.writeAll("\n");
            }
        }

        try writer.writeAll("  ]\n");
        try writer.writeAll("}\n");

        return output.toOwnedSlice();
    }

    /// Generate Markdown report (for human consumption)
    pub fn generateMarkdown(self: *Reporter, suite: *const BenchmarkSuite) ![]u8 {
        var output = std.ArrayList(u8).init(self.allocator);
        const writer = output.writer();

        try writer.print("# Benchmark Report: {s}\n\n", .{suite.name});
        try writer.print("**Date:** {d}\n\n", .{std.time.timestamp()});
        try writer.print("**Overall Overhead:** {d:.2}%\n", .{suite.overallOverhead()});
        try writer.print("**Status:** {d} passed, {d} failed\n\n", .{ suite.totalPassed(), suite.totalFailed() });

        const overall_status = if (suite.totalFailed() == 0) "✅" else "❌";
        try writer.print("**Overall:** {s}\n\n", .{overall_status});

        try writer.writeAll("---\n\n");
        try writer.writeAll("## Results by Category\n\n");
        try writer.writeAll("| Benchmark | Category | Overhead | Status |\n");
        try writer.writeAll("|-----------|----------|----------|--------|\n");

        for (suite.benchmarks.items) |bench| {
            const status = if (bench.passed) "✅" else "❌";
            try writer.print("| {s} | {s} | {d:.2}% | {s} |\n", .{
                bench.name,
                bench.category.toString(),
                bench.overhead_percent,
                status,
            });
        }

        try writer.writeAll("\n---\n\n");
        try writer.writeAll("## Detailed Results\n\n");

        for (suite.benchmarks.items) |bench| {
            const status = if (bench.passed) "✅ PASS" else "❌ FAIL";
            try writer.print("### {s} - {s}\n\n", .{ bench.name, status });
            try writer.print("**Category:** {s}\n\n", .{bench.category.toString()});
            try writer.print("**Description:** {s}\n\n", .{bench.description});
            try writer.print("**Performance:**\n", .{});
            try writer.print("- Baseline (C): {d} ns\n", .{bench.baseline_c_ns});
            try writer.print("- Current: {d} ns\n", .{bench.current_ns});
            try writer.print("- Overhead: {d:.2}% (max: {d:.2}%)\n\n", .{ bench.overhead_percent, bench.max_overhead_percent });

            try writer.print("**LLM Context:**\n\n{s}\n\n", .{bench.llm_context});
            try writer.writeAll("---\n\n");
        }

        return output.toOwnedSlice();
    }

    /// Generate console summary (for terminal output)
    pub fn printSummary(_: *Reporter, suite: *const BenchmarkSuite) !void {
        const stdout = std.io.getStdOut().writer();

        try stdout.print("\n", .{});
        try stdout.print("╔═══════════════════════════════════════════════════════════╗\n", .{});
        try stdout.print("║ Benchmark Report: {s: <40} ║\n", .{suite.name});
        try stdout.print("╠═══════════════════════════════════════════════════════════╣\n", .{});
        try stdout.print("║ Overall Overhead: {d: >6.2}% {s: <31} ║\n", .{
            suite.overallOverhead(),
            if (suite.overallOverhead() <= 15.0) "✅" else "⚠️ ",
        });
        try stdout.print("║ Tests Passed: {d: >3}/{d: <3} {s: <35} ║\n", .{
            suite.totalPassed(),
            suite.benchmarks.items.len,
            if (suite.totalFailed() == 0) "✅" else "❌",
        });
        try stdout.print("╚═══════════════════════════════════════════════════════════╝\n", .{});
        try stdout.print("\n", .{});

        // Print results by category
        try stdout.print("Results:\n", .{});
        for (suite.benchmarks.items) |bench| {
            const status = if (bench.passed) "✅" else "❌";
            try stdout.print("  {s: <40} {d: >6.2}%  {s}\n", .{
                bench.name,
                bench.overhead_percent,
                status,
            });
        }
        try stdout.print("\n", .{});

        // Print failures
        if (suite.totalFailed() > 0) {
            try stdout.print("Failures:\n", .{});
            for (suite.benchmarks.items) |bench| {
                if (!bench.passed) {
                    try stdout.print("  ❌ {s}\n", .{bench.name});
                    try stdout.print("     Overhead: {d:.2}% (max: {d:.2}%)\n", .{
                        bench.overhead_percent,
                        bench.max_overhead_percent,
                    });
                    try stdout.print("     {s}\n\n", .{bench.llm_context});
                }
            }
        }
    }
};
