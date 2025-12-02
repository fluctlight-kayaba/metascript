const std = @import("std");
const hooks = @import("hooks.zig");
const testing = std.testing;

// Test: Multiple ref fields
test "HookGenerator: class with multiple ref fields" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    const class = hooks.ClassDef{
        .name = "Person",
        .fields = &[_]hooks.Field{
            .{ .name = "first_name", .type_name = "msString", .kind = .string },
            .{ .name = "last_name", .type_name = "msString", .kind = .string },
            .{ .name = "email", .type_name = "msString", .kind = .string },
            .{ .name = "age", .type_name = "int32_t", .kind = .value },
        },
    };

    const generated = try generator.generateHooks(class);
    defer allocator.free(generated);

    // All 3 strings should be decreffed in destroy
    try testing.expect(std.mem.indexOf(u8, generated, "ms_string_decref(self->first_name)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "ms_string_decref(self->last_name)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "ms_string_decref(self->email)") != null);

    // All 3 strings should be cloned in copy
    try testing.expect(std.mem.indexOf(u8, generated, "ms_string_clone(self->first_name)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "ms_string_clone(self->last_name)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "ms_string_clone(self->email)") != null);
}

// Test: Deeply nested objects
test "HookGenerator: deeply nested objects (3 levels)" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    // Address { city: string }
    // User { name: string, address: Address }
    // Post { title: string, author: User }

    const post_class = hooks.ClassDef{
        .name = "Post",
        .fields = &[_]hooks.Field{
            .{ .name = "title", .type_name = "msString", .kind = .string },
            .{ .name = "author", .type_name = "User", .kind = .object },
        },
    };

    const generated = try generator.generateHooks(post_class);
    defer allocator.free(generated);

    // Should call User_destroy recursively
    try testing.expect(std.mem.indexOf(u8, generated, "User_destroy(self->author)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "User_copy(self->author)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "User_trace(self->author)") != null);
}

// Test: Mix of all field types
test "HookGenerator: class with all field types" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    const class = hooks.ClassDef{
        .name = "MixedClass",
        .fields = &[_]hooks.Field{
            .{ .name = "id", .type_name = "int64_t", .kind = .value },
            .{ .name = "name", .type_name = "msString", .kind = .string },
            .{ .name = "owner", .type_name = "User", .kind = .object },
            .{ .name = "tags", .type_name = "msString[]", .kind = .array },
            .{ .name = "score", .type_name = "double", .kind = .value },
        },
    };

    const generated = try generator.generateHooks(class);
    defer allocator.free(generated);

    // Verify each field type is handled correctly
    try testing.expect(std.mem.indexOf(u8, generated, "// id: value type, no cleanup") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "ms_string_decref(self->name)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "User_destroy(self->owner)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "// TODO: Clean up array tags") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "// score: value type, no cleanup") != null);
}

// Test: Class with only strings (no value types)
test "HookGenerator: class with only ref fields" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    const class = hooks.ClassDef{
        .name = "StringBag",
        .fields = &[_]hooks.Field{
            .{ .name = "a", .type_name = "msString", .kind = .string },
            .{ .name = "b", .type_name = "msString", .kind = .string },
            .{ .name = "c", .type_name = "msString", .kind = .string },
        },
    };

    const generated = try generator.generateHooks(class);
    defer allocator.free(generated);

    // Should generate hooks (has ref fields)
    try testing.expect(generated.len > 0);

    // All fields should be handled
    const decref_count = std.mem.count(u8, generated, "ms_string_decref");
    try testing.expectEqual(@as(usize, 3), decref_count);
}

// Test: Large class (10+ fields)
test "HookGenerator: large class with many fields" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    const class = hooks.ClassDef{
        .name = "LargeClass",
        .fields = &[_]hooks.Field{
            .{ .name = "field1", .type_name = "msString", .kind = .string },
            .{ .name = "field2", .type_name = "int32_t", .kind = .value },
            .{ .name = "field3", .type_name = "msString", .kind = .string },
            .{ .name = "field4", .type_name = "int32_t", .kind = .value },
            .{ .name = "field5", .type_name = "msString", .kind = .string },
            .{ .name = "field6", .type_name = "int32_t", .kind = .value },
            .{ .name = "field7", .type_name = "User", .kind = .object },
            .{ .name = "field8", .type_name = "int32_t", .kind = .value },
            .{ .name = "field9", .type_name = "msString", .kind = .string },
            .{ .name = "field10", .type_name = "int32_t", .kind = .value },
        },
    };

    const generated = try generator.generateHooks(class);
    defer allocator.free(generated);

    // Should handle all 10 fields
    try testing.expect(std.mem.indexOf(u8, generated, "field1") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "field10") != null);

    // Count string decrefs (should be 4)
    const decref_count = std.mem.count(u8, generated, "ms_string_decref");
    try testing.expectEqual(@as(usize, 4), decref_count);

    // Count object destroys (should be 1)
    const destroy_count = std.mem.count(u8, generated, "User_destroy");
    try testing.expectEqual(@as(usize, 1), destroy_count);
}

// Test: Empty class (edge case)
test "HookGenerator: empty class generates no hooks" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    const class = hooks.ClassDef{
        .name = "Empty",
        .fields = &[_]hooks.Field{},
    };

    const generated = try generator.generateHooks(class);
    defer allocator.free(generated);

    // Empty class → no hooks
    try testing.expectEqual(@as(usize, 0), generated.len);
}

// Test: Class name with underscores and numbers
test "HookGenerator: class name with special characters" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    const class = hooks.ClassDef{
        .name = "User_Account_v2",
        .fields = &[_]hooks.Field{
            .{ .name = "name", .type_name = "msString", .kind = .string },
        },
    };

    const generated = try generator.generateHooks(class);
    defer allocator.free(generated);

    // Should generate hooks with correct naming
    try testing.expect(std.mem.indexOf(u8, generated, "User_Account_v2_destroy") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "User_Account_v2_copy") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "User_Account_v2_sink") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "User_Account_v2_trace") != null);
}

// Test: Field name with underscores
test "HookGenerator: field names with underscores" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    const class = hooks.ClassDef{
        .name = "Config",
        .fields = &[_]hooks.Field{
            .{ .name = "api_key", .type_name = "msString", .kind = .string },
            .{ .name = "user_name", .type_name = "msString", .kind = .string },
            .{ .name = "max_retries", .type_name = "int32_t", .kind = .value },
        },
    };

    const generated = try generator.generateHooks(class);
    defer allocator.free(generated);

    // Should handle underscored names correctly
    try testing.expect(std.mem.indexOf(u8, generated, "self->api_key") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "self->user_name") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "self->max_retries") != null);
}

// Test: Verify proper indentation in generated code
test "HookGenerator: generated code has proper indentation" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    const class = hooks.ClassDef{
        .name = "User",
        .fields = &[_]hooks.Field{
            .{ .name = "name", .type_name = "msString", .kind = .string },
        },
    };

    const generated = try generator.generateHooks(class);
    defer allocator.free(generated);

    // Check for proper indentation (2 spaces)
    try testing.expect(std.mem.indexOf(u8, generated, "  if (self == NULL) return;") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "  ms_string_decref") != null);
}

// Test: Verify all hooks are present
test "HookGenerator: all 4 hooks are generated" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    const class = hooks.ClassDef{
        .name = "User",
        .fields = &[_]hooks.Field{
            .{ .name = "name", .type_name = "msString", .kind = .string },
        },
    };

    const generated = try generator.generateHooks(class);
    defer allocator.free(generated);

    // Count occurrences of each hook
    const destroy_idx = std.mem.indexOf(u8, generated, "User_destroy");
    const copy_idx = std.mem.indexOf(u8, generated, "User_copy");
    const sink_idx = std.mem.indexOf(u8, generated, "User_sink");
    const trace_idx = std.mem.indexOf(u8, generated, "User_trace");

    // All hooks should be present
    try testing.expect(destroy_idx != null);
    try testing.expect(copy_idx != null);
    try testing.expect(sink_idx != null);
    try testing.expect(trace_idx != null);

    // Hooks should appear in order
    try testing.expect(destroy_idx.? < copy_idx.?);
    try testing.expect(copy_idx.? < sink_idx.?);
    try testing.expect(sink_idx.? < trace_idx.?);
}
