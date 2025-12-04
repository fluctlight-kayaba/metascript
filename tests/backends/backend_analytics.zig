/// Backend Analytics - Real-World Program Success Metrics
///
/// Compiles all fixtures to all backends and reports success rates.
/// Provides concrete data on backend maturity.

const std = @import("std");
const testing = std.testing;
const helpers = @import("backend_test_helpers.zig");
const fixtures = @import("fixtures");

const Backend = helpers.Backend;

/// Test result for a single fixture
const FixtureResult = struct {
    name: []const u8,
    c_success: bool,
    js_success: bool,
    erlang_success: bool,
    c_error: ?[]const u8 = null,
    js_error: ?[]const u8 = null,
    erlang_error: ?[]const u8 = null,
};

/// Analytics summary across all fixtures
const AnalyticsSummary = struct {
    total_fixtures: usize,
    c_success_count: usize,
    js_success_count: usize,
    erlang_success_count: usize,

    pub fn calculatePercentage(self: AnalyticsSummary, backend: Backend) f64 {
        if (self.total_fixtures == 0) return 0.0;
        const count = switch (backend) {
            .c => self.c_success_count,
            .javascript => self.js_success_count,
            .erlang => self.erlang_success_count,
        };
        return @as(f64, @floatFromInt(count)) / @as(f64, @floatFromInt(self.total_fixtures)) * 100.0;
    }

    pub fn print(self: AnalyticsSummary) void {
        const sep = "======================================================================";
        std.debug.print("\n{s}\n", .{sep});
        std.debug.print("MetaScript Backend Analytics - Real-World Program Success Rates\n", .{});
        std.debug.print("{s}\n\n", .{sep});

        std.debug.print("Total Fixtures Tested: {d}\n\n", .{self.total_fixtures});

        // C Backend
        std.debug.print("C Backend:        {d}/{d} ({d:.1}%)\n", .{
            self.c_success_count,
            self.total_fixtures,
            self.calculatePercentage(.c),
        });

        // JavaScript Backend
        std.debug.print("JavaScript Backend: {d}/{d} ({d:.1}%)\n", .{
            self.js_success_count,
            self.total_fixtures,
            self.calculatePercentage(.javascript),
        });

        // Erlang Backend
        std.debug.print("Erlang Backend:   {d}/{d} ({d:.1}%)\n\n", .{
            self.erlang_success_count,
            self.total_fixtures,
            self.calculatePercentage(.erlang),
        });

        std.debug.print("{s}\n\n", .{sep});
    }
};

/// Compile a single fixture to all backends and record results
fn testFixture(
    allocator: std.mem.Allocator,
    name: []const u8,
    source: []const u8,
) FixtureResult {
    var result = FixtureResult{
        .name = name,
        .c_success = false,
        .js_success = false,
        .erlang_success = false,
    };

    // Test C backend
    var c_result_opt: ?helpers.CompilationResult = helpers.compile(allocator, source, .c) catch |err| blk: {
        result.c_error = @errorName(err);
        break :blk null;
    };
    if (c_result_opt) |*c_result| {
        result.c_success = true;
        c_result.deinit();
    }

    // Test JavaScript backend
    var js_result_opt: ?helpers.CompilationResult = helpers.compile(allocator, source, .javascript) catch |err| blk: {
        result.js_error = @errorName(err);
        break :blk null;
    };
    if (js_result_opt) |*js_result| {
        result.js_success = true;
        js_result.deinit();
    }

    // Test Erlang backend
    var erl_result_opt: ?helpers.CompilationResult = helpers.compile(allocator, source, .erlang) catch |err| blk: {
        result.erlang_error = @errorName(err);
        break :blk null;
    };
    if (erl_result_opt) |*erl_result| {
        result.erlang_success = true;
        erl_result.deinit();
    }

    return result;
}

/// Run analytics on all fixtures
fn runAnalytics(allocator: std.mem.Allocator) !AnalyticsSummary {
    var results = std.ArrayList(FixtureResult).init(allocator);
    defer results.deinit();

    // Use unified fixture system - single source of truth
    const all_fixtures = fixtures.all();
    for (all_fixtures) |fixture| {
        try results.append(testFixture(allocator, fixture.name, fixture.source));
    }

    // Calculate summary
    var summary = AnalyticsSummary{
        .total_fixtures = results.items.len,
        .c_success_count = 0,
        .js_success_count = 0,
        .erlang_success_count = 0,
    };

    for (results.items) |r| {
        if (r.c_success) summary.c_success_count += 1;
        if (r.js_success) summary.js_success_count += 1;
        if (r.erlang_success) summary.erlang_success_count += 1;
    }

    // Print detailed results
    const sep = "======================================================================";
    std.debug.print("\n{s}\n", .{sep});
    std.debug.print("Detailed Results by Fixture\n", .{});
    std.debug.print("{s}\n\n", .{sep});

    for (results.items) |r| {
        std.debug.print("{s}:\n", .{r.name});
        std.debug.print("  C:        {s}\n", .{if (r.c_success) "✅ PASS" else "❌ FAIL"});
        if (r.c_error) |err| {
            std.debug.print("            Error: {s}\n", .{err});
        }
        std.debug.print("  JS:       {s}\n", .{if (r.js_success) "✅ PASS" else "❌ FAIL"});
        if (r.js_error) |err| {
            std.debug.print("            Error: {s}\n", .{err});
        }
        std.debug.print("  Erlang:   {s}\n", .{if (r.erlang_success) "✅ PASS" else "❌ FAIL"});
        if (r.erlang_error) |err| {
            std.debug.print("            Error: {s}\n", .{err});
        }
        std.debug.print("\n", .{});
    }

    return summary;
}

test "analytics: real-world program success rates across all backends" {
    const summary = try runAnalytics(testing.allocator);
    summary.print();

    // Print interpretation
    const sep = "======================================================================";
    std.debug.print("{s}\n", .{sep});
    std.debug.print("Interpretation\n", .{});
    std.debug.print("{s}\n\n", .{sep});

    const c_pct = summary.calculatePercentage(.c);
    const js_pct = summary.calculatePercentage(.javascript);
    const erl_pct = summary.calculatePercentage(.erlang);

    if (c_pct >= 80.0) {
        std.debug.print("✅ C Backend: PRODUCTION-READY (>80% success)\n", .{});
    } else if (c_pct >= 50.0) {
        std.debug.print("⚠️  C Backend: BETA (50-80% success)\n", .{});
    } else {
        std.debug.print("🚧 C Backend: ALPHA (<50% success)\n", .{});
    }

    if (js_pct >= 80.0) {
        std.debug.print("✅ JavaScript Backend: PRODUCTION-READY (>80% success)\n", .{});
    } else if (js_pct >= 50.0) {
        std.debug.print("⚠️  JavaScript Backend: BETA (50-80% success)\n", .{});
    } else {
        std.debug.print("🚧 JavaScript Backend: ALPHA (<50% success)\n", .{});
    }

    if (erl_pct >= 80.0) {
        std.debug.print("✅ Erlang Backend: PRODUCTION-READY (>80% success)\n", .{});
    } else if (erl_pct >= 50.0) {
        std.debug.print("⚠️  Erlang Backend: BETA (50-80% success)\n", .{});
    } else {
        std.debug.print("🚧 Erlang Backend: ALPHA (<50% success)\n", .{});
    }

    std.debug.print("\n{s}\n\n", .{sep});

    // This test always passes - it's for metrics only
}
