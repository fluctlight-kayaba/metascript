const std = @import("std");
const hooks = @import("hooks.zig");
const testing = std.testing;

// End-to-end test: Generate hooks and compile with C
test "E2E: Generate User class hooks and compile with C" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    // Define User class: { name: string, age: number }
    const user_class = hooks.ClassDef{
        .name = "User",
        .fields = &[_]hooks.Field{
            .{ .name = "name", .type_name = "msString", .kind = .string },
            .{ .name = "age", .type_name = "int32_t", .kind = .value },
        },
    };

    // Generate hooks
    const generated_hooks = try generator.generateHooks(user_class);
    defer allocator.free(generated_hooks);

    // Write complete C file
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    const writer = output.writer();

    // Header
    try writer.writeAll(
        \\#include "runtime/orc.h"
        \\#include "runtime/string.h"
        \\#include <stdio.h>
        \\#include <assert.h>
        \\
        \\// User struct
        \\typedef struct {
        \\  msString* name;
        \\  int32_t age;
        \\} User;
        \\
        \\
    );

    // Generated hooks
    try writer.writeAll(generated_hooks);
    try writer.writeAll("\n\n");

    // Test main
    try writer.writeAll(
        \\int main() {
        \\  printf("Testing User lifecycle hooks...\n");
        \\
        \\  // Test 1: Create and destroy
        \\  printf("  Test 1: Create and destroy...");
        \\  User* user = (User*)ms_alloc(sizeof(User));
        \\  user->name = ms_string_new("Alice", 5);
        \\  user->age = 30;
        \\
        \\  assert(ms_string_getrc(user->name) == 1);
        \\
        \\  User_destroy(user);
        \\  ms_decref(user);
        \\  printf("OK\n");
        \\
        \\  // Test 2: Copy
        \\  printf("  Test 2: Copy...");
        \\  User* user1 = (User*)ms_alloc(sizeof(User));
        \\  user1->name = ms_string_new("Bob", 3);
        \\  user1->age = 25;
        \\
        \\  User* user2 = User_copy(user1);
        \\  assert(user2 != NULL);
        \\  assert(user2 != user1);  // Different pointers
        \\  assert(ms_string_equals(user1->name, user2->name));  // Same content
        \\  assert(user2->age == 25);
        \\  assert(ms_string_getrc(user1->name) == 2);  // Shared string
        \\
        \\  User_destroy(user1);
        \\  ms_decref(user1);
        \\  User_destroy(user2);
        \\  ms_decref(user2);
        \\  printf("OK\n");
        \\
        \\  // Test 3: Sink (move)
        \\  printf("  Test 3: Sink (move)...");
        \\  User* user3 = (User*)ms_alloc(sizeof(User));
        \\  user3->name = ms_string_new("Charlie", 7);
        \\  user3->age = 40;
        \\
        \\  User* user4 = User_sink(user3);
        \\  assert(user4 == user3);  // Same pointer (move)
        \\  assert(ms_string_getrc(user4->name) == 1);  // No RC change
        \\
        \\  User_destroy(user4);
        \\  ms_decref(user4);
        \\  printf("OK\n");
        \\
        \\  // Test 4: NULL safety
        \\  printf("  Test 4: NULL safety...");
        \\  User_destroy(NULL);  // Should not crash
        \\  User* null_copy = User_copy(NULL);
        \\  assert(null_copy == NULL);
        \\  User_sink(NULL);  // Should not crash
        \\  User_trace(NULL);  // Should not crash
        \\  printf("OK\n");
        \\
        \\  printf("\nAll User hook tests passed!\n");
        \\  return 0;
        \\}
        \\
    );

    // Write to file
    const c_file_path = "/tmp/user_hooks_test.c";
    const file = try std.fs.createFileAbsolute(c_file_path, .{});
    defer file.close();
    try file.writeAll(output.items);

    std.debug.print("\nGenerated C file:\n{s}\n", .{output.items});
    std.debug.print("\nCompiling {s}...\n", .{c_file_path});

    // Compile with clang
    const compile_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "clang",
            "-I/Users/le/projects/metascript/src",
            "-o",
            "/tmp/user_hooks_test",
            c_file_path,
        },
    });
    defer allocator.free(compile_result.stdout);
    defer allocator.free(compile_result.stderr);

    if (compile_result.term.Exited != 0) {
        std.debug.print("Compilation failed!\nstderr: {s}\n", .{compile_result.stderr});
        return error.CompilationFailed;
    }

    std.debug.print("Compilation successful!\n", .{});
    std.debug.print("Running test...\n\n", .{});

    // Run the compiled binary
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{"/tmp/user_hooks_test"},
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    std.debug.print("{s}", .{run_result.stdout});

    if (run_result.term.Exited != 0) {
        std.debug.print("Test execution failed!\nstderr: {s}\n", .{run_result.stderr});
        return error.TestFailed;
    }

    // Verify output contains success message
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "All User hook tests passed!") != null);
}
