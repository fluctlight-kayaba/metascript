const std = @import("std");
const testing = std.testing;
const helpers = @import("backend_test_helpers.zig");

test "these SHOULD fail - testing if parser is too permissive" {
    const allocator = testing.allocator;

    // Test 1: TypeScript features we DON'T support yet
    const tests = [_]struct {
        name: []const u8,
        code: []const u8,
        should_fail: bool,
    }{
        .{
            .name = "Generic class (not supported?)",
            .code = "class Box<T> { value: T; }",
            .should_fail = true,  // Assuming generics not implemented
        },
        .{
            .name = "Interface (not supported?)",
            .code = "interface IFoo { x: number; }",
            .should_fail = true,  // Assuming interfaces not implemented
        },
        .{
            .name = "Type alias (not supported?)",
            .code = "type MyType = string | number;",
            .should_fail = true,
        },
        .{
            .name = "Async/await (not supported?)",
            .code = "async function test() { await foo(); }",
            .should_fail = true,
        },
        .{
            .name = "Destructuring (not supported?)",
            .code = "const { x, y } = point;",
            .should_fail = true,
        },
        .{
            .name = "Spread operator (not supported?)",
            .code = "const arr2 = [...arr1, 4, 5];",
            .should_fail = true,
        },
        .{
            .name = "Arrow function (might work?)",
            .code = "const fn = (x: number) => x + 1;",
            .should_fail = false,  // Might be supported
        },
        .{
            .name = "Template literals (not supported?)",
            .code = "const str = `Hello ${name}`;",
            .should_fail = true,
        },
    };

    std.debug.print("\n=== Testing Parser Permissiveness ===\n\n", .{});

    for (tests) |t| {
        var result = try helpers.compile(allocator, t.code, .c);
        defer result.deinit();

        const actual_failed = !result.success;
        const expectation_met = actual_failed == t.should_fail;

        std.debug.print("{s}: {s}\n", .{
            t.name,
            if (expectation_met) "✅ As expected" else "⚠️  UNEXPECTED",
        });
        std.debug.print("  Code: {s}\n", .{t.code});
        std.debug.print("  Should fail: {} | Actually failed: {}\n", .{ t.should_fail, actual_failed });
        if (result.error_message) |msg| {
            std.debug.print("  Error: {s}\n", .{msg});
        } else {
            std.debug.print("  Result: Generated code successfully\n", .{});
        }
        std.debug.print("\n", .{});
    }

    std.debug.print("=================================\n\n", .{});
}
