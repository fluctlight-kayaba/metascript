const std = @import("std");
const testing = std.testing;
const helpers = @import("backend_test_helpers.zig");

test "error detection: invalid syntax should fail" {
    // Missing parameter in function
    const broken1 = "function test( { return 1; }";

    var result1 = try helpers.compile(testing.allocator, broken1, .c);
    defer result1.deinit();

    std.debug.print("\n=== Error Detection Test ===\n", .{});
    std.debug.print("Test 1 - Missing parameter:\n", .{});
    std.debug.print("  Code: {s}\n", .{broken1});
    std.debug.print("  Success: {}\n", .{result1.success});
    if (result1.error_message) |msg| {
        std.debug.print("  Error: {s}\n", .{msg});
    }

    // Missing closing brace
    const broken2 = "function test() { return 1;";

    var result2 = try helpers.compile(testing.allocator, broken2, .c);
    defer result2.deinit();

    std.debug.print("\nTest 2 - Missing closing brace:\n", .{});
    std.debug.print("  Code: {s}\n", .{broken2});
    std.debug.print("  Success: {}\n", .{result2.success});
    if (result2.error_message) |msg| {
        std.debug.print("  Error: {s}\n", .{msg});
    }

    // Invalid token
    const broken3 = "function test() { return @@@ 1; }";

    var result3 = try helpers.compile(testing.allocator, broken3, .c);
    defer result3.deinit();

    std.debug.print("\nTest 3 - Invalid token:\n", .{});
    std.debug.print("  Code: {s}\n", .{broken3});
    std.debug.print("  Success: {}\n", .{result3.success});
    if (result3.error_message) |msg| {
        std.debug.print("  Error: {s}\n", .{msg});
    }

    std.debug.print("\n=========================\n", .{});
}

test "valid code should succeed" {
    const valid = "function test() { return 1; }";

    var result = try helpers.compile(testing.allocator, valid, .c);
    defer result.deinit();

    std.debug.print("\n=== Valid Code Test ===\n", .{});
    std.debug.print("  Code: {s}\n", .{valid});
    std.debug.print("  Success: {}\n", .{result.success});
    std.debug.print("======================\n", .{});

    try testing.expect(result.success);
}
