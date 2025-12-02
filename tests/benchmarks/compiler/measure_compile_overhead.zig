/// Standalone Compilation Overhead Measurement
///
/// Simple script to measure normalization overhead without complex build configuration.
/// Run with: zig run tests/benchmarks/compiler/measure_compile_overhead.zig
///
/// Target: <5% overhead for normalization phase (Phase 2 from roadmap)

const std = @import("std");

// Sample source code to compile
const test_source =
    \\class User {
    \\    id: number;
    \\    name: string;
    \\    email: string;
    \\}
    \\
    \\class Product {
    \\    id: number;
    \\    title: string;
    \\    price: number;
    \\}
    \\
    \\function processUser(user: User): string {
    \\    return "User: " + user.name;
    \\}
;

fn measureCompilation(allocator: std.mem.Allocator, with_normalize: bool, iterations: usize) !u64 {
    // Create temp file
    var tmp_dir = std.testing.tmpDir(.{});
    var dir = tmp_dir.dir;
    defer tmp_dir.cleanup();

    const filename = "benchmark_test.ms";
    const file = try dir.createFile(filename, .{});
    try file.writeAll(test_source);
    file.close();

    // Get absolute path
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_path = try dir.realpath(filename, &path_buf);

    var total_time: u64 = 0;
    var i: usize = 0;
    while (i < iterations) : (i += 1) {
        const start = std.time.nanoTimestamp();

        // Run msc compile
        var argv = std.ArrayList([]const u8).init(allocator);
        defer argv.deinit();

        try argv.append("./zig-out/bin/msc");
        try argv.append("compile");
        try argv.append(abs_path);
        if (!with_normalize) {
            try argv.append("--no-normalize");
        }

        var child = std.process.Child.init(argv.items, allocator);
        child.stdout_behavior = .Ignore;
        child.stderr_behavior = .Ignore;

        const term = try child.spawnAndWait();
        if (term != .Exited or term.Exited != 0) {
            std.debug.print("Compilation failed!\n", .{});
            return error.CompilationFailed;
        }

        const elapsed = @as(u64, @intCast(std.time.nanoTimestamp() - start));
        total_time += elapsed;
    }

    return total_time / iterations; // Return average
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const stdout = std.io.getStdOut().writer();

    try stdout.print("\n", .{});
    try stdout.print("╔═══════════════════════════════════════════════════════════╗\n", .{});
    try stdout.print("║     Compilation Overhead Measurement                      ║\n", .{});
    try stdout.print("║                                                           ║\n", .{});
    try stdout.print("║  Target: <5% overhead for normalization phase            ║\n", .{});
    try stdout.print("╚═══════════════════════════════════════════════════════════╝\n", .{});
    try stdout.print("\n", .{});

    const iterations: usize = 10;
    try stdout.print("Running {d} iterations per configuration...\n\n", .{iterations});

    // Warmup
    try stdout.print("Warming up...\n", .{});
    _ = try measureCompilation(allocator, false, 2);
    _ = try measureCompilation(allocator, true, 2);

    // Measure baseline (without normalization)
    try stdout.print("Measuring baseline (--no-normalize)...\n", .{});
    const baseline_ns = try measureCompilation(allocator, false, iterations);
    const baseline_ms = @as(f64, @floatFromInt(baseline_ns)) / 1_000_000.0;

    // Measure with normalization
    try stdout.print("Measuring with normalization...\n\n", .{});
    const current_ns = try measureCompilation(allocator, true, iterations);
    const current_ms = @as(f64, @floatFromInt(current_ns)) / 1_000_000.0;

    // Calculate overhead
    const overhead_percent = if (baseline_ns > 0)
        (((@as(f64, @floatFromInt(current_ns)) - @as(f64, @floatFromInt(baseline_ns))) /
            @as(f64, @floatFromInt(baseline_ns))) * 100.0)
    else
        0.0;

    // Results
    try stdout.print("╔═══════════════════════════════════════════════════════════╗\n", .{});
    try stdout.print("║                         RESULTS                           ║\n", .{});
    try stdout.print("╠═══════════════════════════════════════════════════════════╣\n", .{});
    try stdout.print("║  Baseline (--no-normalize):  {d:>8.2} ms                 ║\n", .{baseline_ms});
    try stdout.print("║  With normalization:         {d:>8.2} ms                 ║\n", .{current_ms});
    try stdout.print("║  Overhead:                   {d:>8.2}%                   ║\n", .{overhead_percent});
    try stdout.print("╠═══════════════════════════════════════════════════════════╣\n", .{});

    if (overhead_percent < 5.0) {
        try stdout.print("║  Status: ✓ PASS (under 5% target)                        ║\n", .{});
        try stdout.print("╚═══════════════════════════════════════════════════════════╝\n", .{});
        try stdout.print("\n", .{});
        std.process.exit(0);
    } else {
        try stdout.print("║  Status: ✗ FAIL (exceeds 5% target)                      ║\n", .{});
        try stdout.print("╚═══════════════════════════════════════════════════════════╝\n", .{});
        try stdout.print("\n", .{});
        std.process.exit(1);
    }
}
