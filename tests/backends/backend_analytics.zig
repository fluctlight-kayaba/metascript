/// Backend Analytics - Real-World Program Success Metrics
///
/// Compiles all fixtures to all backends and reports success rates.
/// Provides concrete data on backend maturity.

const std = @import("std");
const testing = std.testing;
const helpers = @import("backend_test_helpers.zig");
const fixtures = @import("real_world_fixtures.zig");

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

    // Test all basic language features
    try results.append(testFixture(allocator, "SIMPLE_FUNCTION", fixtures.SIMPLE_FUNCTION));
    try results.append(testFixture(allocator, "FACTORIAL_RECURSIVE", fixtures.FACTORIAL_RECURSIVE));
    try results.append(testFixture(allocator, "FACTORIAL_ITERATIVE", fixtures.FACTORIAL_ITERATIVE));
    try results.append(testFixture(allocator, "FIBONACCI", fixtures.FIBONACCI));

    // Test loops and control flow
    try results.append(testFixture(allocator, "WHILE_LOOP_COUNTER", fixtures.WHILE_LOOP_COUNTER));
    try results.append(testFixture(allocator, "FOR_LOOP_SUM", fixtures.FOR_LOOP_SUM));
    try results.append(testFixture(allocator, "NESTED_LOOPS", fixtures.NESTED_LOOPS));
    try results.append(testFixture(allocator, "EARLY_RETURN", fixtures.EARLY_RETURN));

    // Test variable shadowing
    try results.append(testFixture(allocator, "VARIABLE_SHADOWING_SIMPLE", fixtures.VARIABLE_SHADOWING_SIMPLE));
    try results.append(testFixture(allocator, "VARIABLE_SHADOWING_MULTIPLE", fixtures.VARIABLE_SHADOWING_MULTIPLE));

    // Test objects and arrays
    try results.append(testFixture(allocator, "OBJECT_LITERAL", fixtures.OBJECT_LITERAL));
    try results.append(testFixture(allocator, "OBJECT_MEMBER_ACCESS", fixtures.OBJECT_MEMBER_ACCESS));
    try results.append(testFixture(allocator, "ARRAY_OPERATIONS", fixtures.ARRAY_OPERATIONS));

    // Test classes
    try results.append(testFixture(allocator, "SIMPLE_CLASS", fixtures.SIMPLE_CLASS));
    try results.append(testFixture(allocator, "CLASS_WITH_METHODS", fixtures.CLASS_WITH_METHODS));
    try results.append(testFixture(allocator, "INHERITANCE_SIMPLE", fixtures.INHERITANCE_SIMPLE));
    try results.append(testFixture(allocator, "METHOD_OVERRIDE", fixtures.METHOD_OVERRIDE));
    try results.append(testFixture(allocator, "POLYMORPHISM", fixtures.POLYMORPHISM));

    // Test algorithms
    try results.append(testFixture(allocator, "QUICKSORT", fixtures.QUICKSORT));
    try results.append(testFixture(allocator, "MERGE_SORT", fixtures.MERGE_SORT));
    try results.append(testFixture(allocator, "LINKED_LIST", fixtures.LINKED_LIST));
    try results.append(testFixture(allocator, "BINARY_SEARCH_TREE", fixtures.BINARY_SEARCH_TREE));
    try results.append(testFixture(allocator, "BINARY_SEARCH", fixtures.BINARY_SEARCH));
    try results.append(testFixture(allocator, "IS_PRIME", fixtures.IS_PRIME));

    // Test design patterns
    try results.append(testFixture(allocator, "BUILDER_PATTERN", fixtures.BUILDER_PATTERN));
    try results.append(testFixture(allocator, "FACTORY_PATTERN", fixtures.FACTORY_PATTERN));
    try results.append(testFixture(allocator, "SINGLETON_PATTERN", fixtures.SINGLETON_PATTERN));
    try results.append(testFixture(allocator, "OBSERVER_PATTERN", fixtures.OBSERVER_PATTERN));

    // Test comprehensive demo
    try results.append(testFixture(allocator, "COMPREHENSIVE_DEMO", fixtures.COMPREHENSIVE_DEMO));

    // Test known bugs (these SHOULD fail)
    try results.append(testFixture(allocator, "ERLANG_BUG_LOOP_CLOSURE", fixtures.ERLANG_BUG_LOOP_CLOSURE));
    try results.append(testFixture(allocator, "ERLANG_BUG_EARLY_RETURN", fixtures.ERLANG_BUG_EARLY_RETURN));

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
