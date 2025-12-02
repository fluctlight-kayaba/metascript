const std = @import("std");

/// Benchmark category for organization
pub const Category = enum {
    lexer,
    parser,
    typechecker,
    macro_expansion,
    codegen,
    runtime_orc,
    runtime_string,
    runtime_collection,
    backend_c,
    backend_js,
    backend_erlang,
    e2e_compile,

    pub fn toString(self: Category) []const u8 {
        return @tagName(self);
    }
};

/// Benchmark configuration
pub const BenchmarkConfig = struct {
    /// Minimum samples to collect for statistical analysis
    min_samples: usize = 5,
    /// Minimum warmup time in nanoseconds (default 50ms)
    warmup_time_ns: u64 = 50_000_000,
    /// Minimum time per benchmark measurement (default 10ms)
    min_measurement_time_ns: u64 = 10_000_000,
    /// Maximum iterations cap
    max_iterations: usize = 100_000_000,
};

/// Default benchmark configuration
pub const default_config = BenchmarkConfig{};

/// Core benchmark result structure
pub const Benchmark = struct {
    // Metadata
    name: []const u8,
    description: []const u8,
    category: Category,
    timestamp: i64,

    // Performance metrics
    baseline_c_ns: u64, // Baseline C implementation time (nanoseconds)
    current_ns: u64, // Our implementation time (nanoseconds)
    overhead_percent: f64, // (current - baseline) / baseline * 100

    // Memory metrics
    baseline_memory_bytes: usize, // Baseline C memory usage
    current_memory_bytes: usize, // Our memory usage
    memory_overhead_percent: f64, // Memory overhead %

    // Success criteria
    max_overhead_percent: f64, // Fail if overhead > this
    max_memory_overhead_percent: f64, // Fail if memory overhead > this
    passed: bool,

    // LLM-friendly context
    llm_context: []const u8, // Human-readable explanation

    // Statistical info
    iterations_used: usize, // Actual iterations used
    samples_collected: usize, // Number of samples

    pub fn calculateOverhead(baseline_ns: u64, current_ns: u64) f64 {
        if (baseline_ns == 0) {
            // Baseline too fast to measure - compare absolute times
            if (current_ns == 0) return 0.0; // Both unmeasurably fast
            // Current is measurable but baseline isn't - significant overhead
            return 100.0;
        }
        const baseline = @as(f64, @floatFromInt(baseline_ns));
        const current = @as(f64, @floatFromInt(current_ns));
        return ((current - baseline) / baseline) * 100.0;
    }

    pub fn calculateMemoryOverhead(baseline_bytes: usize, current_bytes: usize) f64 {
        if (baseline_bytes == 0) return 0.0;
        const baseline = @as(f64, @floatFromInt(baseline_bytes));
        const current = @as(f64, @floatFromInt(current_bytes));
        return ((current - baseline) / baseline) * 100.0;
    }
};

/// Collection of benchmarks
pub const BenchmarkSuite = struct {
    name: []const u8,
    benchmarks: std.ArrayList(Benchmark),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, name: []const u8) BenchmarkSuite {
        return .{
            .name = name,
            .benchmarks = std.ArrayList(Benchmark).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *BenchmarkSuite) void {
        self.benchmarks.deinit();
    }

    pub fn add(self: *BenchmarkSuite, benchmark: Benchmark) !void {
        try self.benchmarks.append(benchmark);
    }

    pub fn overallOverhead(self: *const BenchmarkSuite) f64 {
        if (self.benchmarks.items.len == 0) return 0.0;

        var total: f64 = 0.0;
        for (self.benchmarks.items) |bench| {
            total += bench.overhead_percent;
        }
        return total / @as(f64, @floatFromInt(self.benchmarks.items.len));
    }

    pub fn totalPassed(self: *const BenchmarkSuite) usize {
        var count: usize = 0;
        for (self.benchmarks.items) |bench| {
            if (bench.passed) count += 1;
        }
        return count;
    }

    pub fn totalFailed(self: *const BenchmarkSuite) usize {
        return self.benchmarks.items.len - self.totalPassed();
    }
};

/// Prevent compiler from optimizing away benchmark results
/// This is critical for accurate measurements
pub inline fn doNotOptimize(val: anytype) void {
    // Use Zig's built-in mechanism to prevent optimization
    // Store to a volatile pointer to create a side effect
    const T = @TypeOf(val);
    if (@sizeOf(T) > 0) {
        const ptr = @as(*volatile T, @constCast(@ptrCast(&val)));
        _ = ptr.*;
    }
}

/// Calibrate the number of iterations needed for accurate measurement
fn calibrateIterations(
    comptime func: anytype,
    min_time_ns: u64,
    max_iterations: usize,
) usize {
    var iterations: usize = 1;

    while (iterations < max_iterations) {
        const start = std.time.nanoTimestamp();
        var i: usize = 0;
        while (i < iterations) : (i += 1) {
            const result = func();
            doNotOptimize(result);
        }
        const elapsed = @as(u64, @intCast(std.time.nanoTimestamp() - start));

        if (elapsed >= min_time_ns) {
            return iterations;
        }

        // Increase iterations: try to reach target time
        if (elapsed == 0) {
            iterations *= 100;
        } else {
            const scale = @max(2, min_time_ns / elapsed);
            iterations = @min(iterations * scale, max_iterations);
        }
    }

    return max_iterations;
}

/// Run warmup phase to stabilize CPU state
fn runWarmup(
    comptime baseline_fn: anytype,
    comptime current_fn: anytype,
    warmup_time_ns: u64,
) void {
    const start = std.time.nanoTimestamp();
    var count: usize = 0;

    while (true) {
        const result1 = baseline_fn();
        doNotOptimize(result1);
        const result2 = current_fn();
        doNotOptimize(result2);
        count += 1;

        const elapsed = @as(u64, @intCast(std.time.nanoTimestamp() - start));
        if (elapsed >= warmup_time_ns) break;
        if (count >= 10000) break; // Safety cap
    }
}

/// Measure execution time for a function
fn measureTime(
    comptime func: anytype,
    iterations: usize,
) u64 {
    const start = std.time.nanoTimestamp();
    var i: usize = 0;
    while (i < iterations) : (i += 1) {
        const result = func();
        doNotOptimize(result);
    }
    const end = std.time.nanoTimestamp();
    return @as(u64, @intCast(end - start));
}

/// Collect multiple samples and return median
fn collectSamples(
    comptime func: anytype,
    iterations: usize,
    num_samples: usize,
    samples_buf: []u64,
) void {
    const count = @min(num_samples, samples_buf.len);
    var i: usize = 0;
    while (i < count) : (i += 1) {
        samples_buf[i] = measureTime(func, iterations);
    }
}

/// Calculate median of samples (sorts in place)
fn median(samples: []u64) u64 {
    if (samples.len == 0) return 0;
    std.mem.sort(u64, samples, {}, std.sort.asc(u64));
    return samples[samples.len / 2];
}

/// Enhanced benchmark runner with statistical analysis
pub fn runBenchmark(
    _: std.mem.Allocator, // Reserved for future memory tracking
    comptime name: []const u8,
    comptime description: []const u8,
    category: Category,
    comptime baseline_fn: anytype,
    comptime current_fn: anytype,
    hint_iterations: usize,
    max_overhead: f64,
    llm_context: []const u8,
) !Benchmark {
    return runBenchmarkWithConfig(
        name,
        description,
        category,
        baseline_fn,
        current_fn,
        hint_iterations,
        max_overhead,
        llm_context,
        default_config,
    );
}

/// Benchmark runner with configurable parameters
pub fn runBenchmarkWithConfig(
    comptime name: []const u8,
    comptime description: []const u8,
    category: Category,
    comptime baseline_fn: anytype,
    comptime current_fn: anytype,
    hint_iterations: usize,
    max_overhead: f64,
    llm_context: []const u8,
    config: BenchmarkConfig,
) !Benchmark {
    // 1. Warmup phase - stabilize CPU caches, branch predictors, frequency scaling
    runWarmup(baseline_fn, current_fn, config.warmup_time_ns);

    // 2. Calibrate iterations - ensure we measure for long enough
    const baseline_iters = @max(
        hint_iterations,
        calibrateIterations(baseline_fn, config.min_measurement_time_ns, config.max_iterations),
    );
    const current_iters = @max(
        hint_iterations,
        calibrateIterations(current_fn, config.min_measurement_time_ns, config.max_iterations),
    );
    // Use same iterations for fair comparison
    const iterations = @min(baseline_iters, current_iters);

    // 3. Collect multiple samples for statistical robustness
    var baseline_samples: [10]u64 = undefined;
    var current_samples: [10]u64 = undefined;
    const num_samples = @min(config.min_samples, 10);

    collectSamples(baseline_fn, iterations, num_samples, &baseline_samples);
    collectSamples(current_fn, iterations, num_samples, &current_samples);

    // 4. Calculate median (more robust than mean against outliers)
    const baseline_median = median(baseline_samples[0..num_samples]);
    const current_median = median(current_samples[0..num_samples]);

    // 5. Calculate overhead
    const overhead = Benchmark.calculateOverhead(baseline_median, current_median);

    return Benchmark{
        .name = name,
        .description = description,
        .category = category,
        .timestamp = std.time.timestamp(),
        .baseline_c_ns = baseline_median,
        .current_ns = current_median,
        .overhead_percent = overhead,
        .baseline_memory_bytes = 0, // TODO: Measure memory usage
        .current_memory_bytes = 0,
        .memory_overhead_percent = 0.0,
        .max_overhead_percent = max_overhead,
        .max_memory_overhead_percent = 100.0,
        .passed = overhead <= max_overhead,
        .llm_context = llm_context,
        .iterations_used = iterations,
        .samples_collected = num_samples,
    };
}

// =============================================================================
// Tests
// =============================================================================

test "Benchmark.calculateOverhead: normal case" {
    const overhead = Benchmark.calculateOverhead(100, 110);
    try std.testing.expectApproxEqAbs(@as(f64, 10.0), overhead, 0.01);
}

test "Benchmark.calculateOverhead: zero baseline returns 100" {
    const overhead = Benchmark.calculateOverhead(0, 1000);
    try std.testing.expectEqual(@as(f64, 100.0), overhead);
}

test "Benchmark.calculateOverhead: both zero returns 0" {
    const overhead = Benchmark.calculateOverhead(0, 0);
    try std.testing.expectEqual(@as(f64, 0.0), overhead);
}

test "Benchmark.calculateOverhead: negative overhead" {
    const overhead = Benchmark.calculateOverhead(100, 90);
    try std.testing.expectApproxEqAbs(@as(f64, -10.0), overhead, 0.01);
}

test "median: odd number of samples" {
    var samples = [_]u64{ 5, 1, 9, 3, 7 };
    const result = median(&samples);
    try std.testing.expectEqual(@as(u64, 5), result);
}

test "median: even number of samples" {
    var samples = [_]u64{ 1, 2, 3, 4 };
    const result = median(&samples);
    // Returns middle element (index 2)
    try std.testing.expectEqual(@as(u64, 3), result);
}

test "median: single sample" {
    var samples = [_]u64{42};
    const result = median(&samples);
    try std.testing.expectEqual(@as(u64, 42), result);
}

test "median: empty returns 0" {
    var samples: [0]u64 = undefined;
    const result = median(&samples);
    try std.testing.expectEqual(@as(u64, 0), result);
}

test "BenchmarkSuite: init and deinit" {
    const allocator = std.testing.allocator;
    var suite = BenchmarkSuite.init(allocator, "Test Suite");
    defer suite.deinit();

    try std.testing.expectEqualStrings("Test Suite", suite.name);
    try std.testing.expectEqual(@as(usize, 0), suite.benchmarks.items.len);
}

test "doNotOptimize: compiles without error" {
    var x: i32 = 42;
    doNotOptimize(x);
    doNotOptimize(&x);
    doNotOptimize(@as(u64, 100));
}
