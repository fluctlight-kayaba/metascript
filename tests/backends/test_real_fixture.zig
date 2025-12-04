const std = @import("std");
const testing = std.testing;
const helpers = @import("backend_test_helpers.zig");
const fixtures = @import("real_world_fixtures.zig");

test "test actual fixture: INHERITANCE_SIMPLE" {
    std.debug.print("\n=== Testing INHERITANCE_SIMPLE ===\n", .{});
    std.debug.print("Source:\n{s}\n\n", .{fixtures.INHERITANCE_SIMPLE});

    var c_result = try helpers.compile(testing.allocator, fixtures.INHERITANCE_SIMPLE, .c);
    defer c_result.deinit();

    std.debug.print("C Backend:\n", .{});
    std.debug.print("  Success: {}\n", .{c_result.success});
    if (c_result.error_message) |msg| {
        std.debug.print("  Error: {s}\n", .{msg});
    } else {
        std.debug.print("  Generated {} bytes of C code\n", .{c_result.output.len});
    }

    var js_result = try helpers.compile(testing.allocator, fixtures.INHERITANCE_SIMPLE, .javascript);
    defer js_result.deinit();

    std.debug.print("\nJavaScript Backend:\n", .{});
    std.debug.print("  Success: {}\n", .{js_result.success});
    if (js_result.error_message) |msg| {
        std.debug.print("  Error: {s}\n", .{msg});
    }

    var erl_result = try helpers.compile(testing.allocator, fixtures.INHERITANCE_SIMPLE, .erlang);
    defer erl_result.deinit();

    std.debug.print("\nErlang Backend:\n", .{});
    std.debug.print("  Success: {}\n", .{erl_result.success});
    if (erl_result.error_message) |msg| {
        std.debug.print("  Error: {s}\n", .{msg});
    }

    std.debug.print("\n=================================\n", .{});
}

test "test actual fixture: POLYMORPHISM" {
    std.debug.print("\n=== Testing POLYMORPHISM ===\n", .{});
    std.debug.print("Source:\n{s}\n\n", .{fixtures.POLYMORPHISM});

    var c_result = try helpers.compile(testing.allocator, fixtures.POLYMORPHISM, .c);
    defer c_result.deinit();

    std.debug.print("C Backend:\n", .{});
    std.debug.print("  Success: {}\n", .{c_result.success});
    if (c_result.error_message) |msg| {
        std.debug.print("  Error: {s}\n", .{msg});
    }

    std.debug.print("\n=========================\n", .{});
}

test "test actual fixture: BUILDER_PATTERN" {
    std.debug.print("\n=== Testing BUILDER_PATTERN ===\n", .{});

    var c_result = try helpers.compile(testing.allocator, fixtures.BUILDER_PATTERN, .c);
    defer c_result.deinit();

    std.debug.print("C Backend:\n", .{});
    std.debug.print("  Success: {}\n", .{c_result.success});
    if (c_result.error_message) |msg| {
        std.debug.print("  Error: {s}\n", .{msg});
    }

    std.debug.print("\n============================\n", .{});
}
