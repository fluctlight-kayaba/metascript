// Test orchestration framework
// Coordinates benchmark execution, aggregates results, supports filtering

const std = @import("std");
const benchmark = @import("benchmark");
const reporter = @import("reporter");

/// Benchmark registration function type
pub const RegisterFn = *const fn (*benchmark.BenchmarkSuite) anyerror!void;

/// Benchmark runner configuration
pub const RunnerConfig = struct {
    /// Output format
    output_format: OutputFormat = .console,
    /// Category filter (null = all)
    category_filter: ?benchmark.Category = null,
    /// Name pattern filter (null = all)
    name_filter: ?[]const u8 = null,
    /// Output directory for reports
    output_dir: []const u8 = "benchmarks",
    /// Fail on any benchmark failure
    fail_on_regression: bool = true,
    /// Maximum acceptable overhead (default 20%)
    max_overall_overhead: f64 = 20.0,
    /// Verbose output
    verbose: bool = false,
};

pub const OutputFormat = enum {
    console,
    json,
    markdown,
    all,
};

/// Benchmark runner - orchestrates benchmark execution
pub const Runner = struct {
    allocator: std.mem.Allocator,
    config: RunnerConfig,
    suites: std.ArrayList(benchmark.BenchmarkSuite),
    register_fns: std.ArrayList(RegisterFn),

    pub fn init(allocator: std.mem.Allocator, config: RunnerConfig) Runner {
        return .{
            .allocator = allocator,
            .config = config,
            .suites = std.ArrayList(benchmark.BenchmarkSuite).init(allocator),
            .register_fns = std.ArrayList(RegisterFn).init(allocator),
        };
    }

    pub fn deinit(self: *Runner) void {
        for (self.suites.items) |*suite| {
            suite.deinit();
        }
        self.suites.deinit();
        self.register_fns.deinit();
    }

    /// Register a benchmark suite
    pub fn registerSuite(self: *Runner, name: []const u8, register_fn: RegisterFn) !void {
        var suite = benchmark.BenchmarkSuite.init(self.allocator, name);
        try register_fn(&suite);
        try self.suites.append(suite);
    }

    /// Run all registered benchmarks
    pub fn run(self: *Runner) !RunResult {
        var total_passed: usize = 0;
        var total_failed: usize = 0;
        var total_overhead: f64 = 0.0;
        var count: usize = 0;

        // Filter and aggregate results
        for (self.suites.items) |*suite| {
            for (suite.benchmarks.items) |bench| {
                // Apply category filter
                if (self.config.category_filter) |cat| {
                    if (bench.category != cat) continue;
                }

                // Apply name filter
                if (self.config.name_filter) |filter| {
                    if (std.mem.indexOf(u8, bench.name, filter) == null) continue;
                }

                if (bench.passed) {
                    total_passed += 1;
                } else {
                    total_failed += 1;
                }
                total_overhead += bench.overhead_percent;
                count += 1;
            }
        }

        const avg_overhead = if (count > 0) total_overhead / @as(f64, @floatFromInt(count)) else 0.0;

        return RunResult{
            .total_passed = total_passed,
            .total_failed = total_failed,
            .overall_overhead = avg_overhead,
            .passed = total_failed == 0 and avg_overhead <= self.config.max_overall_overhead,
        };
    }

    /// Generate reports based on config
    pub fn generateReports(self: *Runner) !void {
        var rep = reporter.Reporter.init(self.allocator);

        for (self.suites.items) |*suite| {
            switch (self.config.output_format) {
                .console => {
                    try rep.printSummary(suite);
                },
                .json => {
                    try self.writeJsonReport(suite, &rep);
                },
                .markdown => {
                    try self.writeMarkdownReport(suite, &rep);
                },
                .all => {
                    try rep.printSummary(suite);
                    try self.writeJsonReport(suite, &rep);
                    try self.writeMarkdownReport(suite, &rep);
                },
            }
        }
    }

    fn writeJsonReport(self: *Runner, suite: *benchmark.BenchmarkSuite, rep: *reporter.Reporter) !void {
        const json = try rep.generateJSON(suite);
        defer self.allocator.free(json);

        // Create output directory if needed
        std.fs.cwd().makeDir(self.config.output_dir) catch |err| {
            if (err != error.PathAlreadyExists) return err;
        };

        // Generate filename from suite name
        var filename_buf: [256]u8 = undefined;
        const filename = try std.fmt.bufPrint(&filename_buf, "{s}/{s}.json", .{
            self.config.output_dir,
            sanitizeFilename(suite.name),
        });

        const file = try std.fs.cwd().createFile(filename, .{});
        defer file.close();
        try file.writeAll(json);

        if (self.config.verbose) {
            std.debug.print("  Written: {s}\n", .{filename});
        }
    }

    fn writeMarkdownReport(self: *Runner, suite: *benchmark.BenchmarkSuite, rep: *reporter.Reporter) !void {
        const markdown = try rep.generateMarkdown(suite);
        defer self.allocator.free(markdown);

        // Create output directory if needed
        std.fs.cwd().makeDir(self.config.output_dir) catch |err| {
            if (err != error.PathAlreadyExists) return err;
        };

        // Generate filename from suite name
        var filename_buf: [256]u8 = undefined;
        const filename = try std.fmt.bufPrint(&filename_buf, "{s}/{s}.md", .{
            self.config.output_dir,
            sanitizeFilename(suite.name),
        });

        const file = try std.fs.cwd().createFile(filename, .{});
        defer file.close();
        try file.writeAll(markdown);

        if (self.config.verbose) {
            std.debug.print("  Written: {s}\n", .{filename});
        }
    }
};

pub const RunResult = struct {
    total_passed: usize,
    total_failed: usize,
    overall_overhead: f64,
    passed: bool,
};

/// Convert suite name to safe filename
fn sanitizeFilename(name: []const u8) []const u8 {
    // For simplicity, just use the name as-is for now
    // In production, replace spaces/special chars
    return name;
}

/// Diff reporter for comparing benchmark runs
pub const DiffReporter = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) DiffReporter {
        return .{ .allocator = allocator };
    }

    /// Compare two benchmark suites and generate diff report
    pub fn compare(self: *DiffReporter, baseline: *const benchmark.BenchmarkSuite, current: *const benchmark.BenchmarkSuite) !DiffReport {
        var report = DiffReport{
            .new_failures = std.ArrayList(Regression).init(self.allocator),
            .improvements = std.ArrayList(Improvement).init(self.allocator),
            .no_change = std.ArrayList([]const u8).init(self.allocator),
        };

        // Build map of baseline benchmarks by name
        var baseline_map = std.StringHashMap(benchmark.Benchmark).init(self.allocator);
        defer baseline_map.deinit();

        for (baseline.benchmarks.items) |bench| {
            try baseline_map.put(bench.name, bench);
        }

        // Compare each current benchmark with baseline
        for (current.benchmarks.items) |curr| {
            if (baseline_map.get(curr.name)) |base| {
                const delta = curr.overhead_percent - base.overhead_percent;

                if (!curr.passed and base.passed) {
                    // Regression: was passing, now failing
                    try report.new_failures.append(.{
                        .name = curr.name,
                        .previous_overhead = base.overhead_percent,
                        .current_overhead = curr.overhead_percent,
                        .delta = delta,
                    });
                } else if (delta < -5.0) {
                    // Improvement: >5% reduction in overhead
                    try report.improvements.append(.{
                        .name = curr.name,
                        .previous_overhead = base.overhead_percent,
                        .current_overhead = curr.overhead_percent,
                        .delta = delta,
                    });
                } else {
                    try report.no_change.append(curr.name);
                }
            }
        }

        return report;
    }
};

pub const DiffReport = struct {
    new_failures: std.ArrayList(Regression),
    improvements: std.ArrayList(Improvement),
    no_change: std.ArrayList([]const u8),

    pub fn deinit(self: *DiffReport) void {
        self.new_failures.deinit();
        self.improvements.deinit();
        self.no_change.deinit();
    }

    pub fn hasRegressions(self: *const DiffReport) bool {
        return self.new_failures.items.len > 0;
    }
};

pub const Regression = struct {
    name: []const u8,
    previous_overhead: f64,
    current_overhead: f64,
    delta: f64,
};

pub const Improvement = struct {
    name: []const u8,
    previous_overhead: f64,
    current_overhead: f64,
    delta: f64,
};

// =============================================================================
// Tests
// =============================================================================

test "Runner: initializes correctly" {
    const allocator = std.testing.allocator;
    var runner = Runner.init(allocator, .{});
    defer runner.deinit();

    try std.testing.expectEqual(@as(usize, 0), runner.suites.items.len);
}

test "RunResult: passed when no failures and low overhead" {
    const result = RunResult{
        .total_passed = 10,
        .total_failed = 0,
        .overall_overhead = 8.5,
        .passed = true,
    };

    try std.testing.expect(result.passed);
    try std.testing.expectEqual(@as(usize, 10), result.total_passed);
    try std.testing.expectEqual(@as(usize, 0), result.total_failed);
}

test "sanitizeFilename: returns name" {
    const result = sanitizeFilename("ORC Benchmarks");
    try std.testing.expectEqualStrings("ORC Benchmarks", result);
}
