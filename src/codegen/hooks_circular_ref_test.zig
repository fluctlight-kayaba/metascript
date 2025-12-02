const std = @import("std");
const hooks = @import("hooks.zig");
const testing = std.testing;

// =============================================================================
// Circular Reference Tests
// =============================================================================

// Test: User → Post → User (simple cycle)
test "HookGenerator: circular reference User→Post→User" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    // User has posts: Post[]
    // Post has author: User
    // This creates a cycle: User → Post → User

    const user_class = hooks.ClassDef{
        .name = "User",
        .fields = &[_]hooks.Field{
            .{ .name = "name", .type_name = "msString", .kind = .string },
            .{ .name = "posts", .type_name = "Post[]", .kind = .array },
        },
    };

    const generated_user = try generator.generateHooks(user_class);
    defer allocator.free(generated_user);

    // User_destroy should handle posts array
    try testing.expect(std.mem.indexOf(u8, generated_user, "User_destroy") != null);
    try testing.expect(std.mem.indexOf(u8, generated_user, "ms_string_decref(self->name)") != null);
    try testing.expect(std.mem.indexOf(u8, generated_user, "// TODO: Clean up array posts") != null);

    const post_class = hooks.ClassDef{
        .name = "Post",
        .fields = &[_]hooks.Field{
            .{ .name = "title", .type_name = "msString", .kind = .string },
            .{ .name = "author", .type_name = "User", .kind = .object },
        },
    };

    const generated_post = try generator.generateHooks(post_class);
    defer allocator.free(generated_post);

    // Post_destroy should call User_destroy recursively
    try testing.expect(std.mem.indexOf(u8, generated_post, "Post_destroy") != null);
    try testing.expect(std.mem.indexOf(u8, generated_post, "User_destroy(self->author)") != null);
}

// Test: Self-referential type (LinkedList node)
test "HookGenerator: self-referential type (Node→Node)" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    // LinkedList node points to itself
    const node_class = hooks.ClassDef{
        .name = "Node",
        .fields = &[_]hooks.Field{
            .{ .name = "value", .type_name = "int32_t", .kind = .value },
            .{ .name = "next", .type_name = "Node", .kind = .object },
        },
    };

    const generated = try generator.generateHooks(node_class);
    defer allocator.free(generated);

    // Node_destroy should call itself recursively
    try testing.expect(std.mem.indexOf(u8, generated, "Node_destroy") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "Node_destroy(self->next)") != null);

    // Node_copy should clone recursively
    try testing.expect(std.mem.indexOf(u8, generated, "Node_copy") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "Node_copy(self->next)") != null);

    // Node_trace should trace recursively
    try testing.expect(std.mem.indexOf(u8, generated, "Node_trace") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "Node_trace(self->next)") != null);
}

// Test: Tree node (self-referential with multiple children)
test "HookGenerator: tree node (Node→left,right)" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    const tree_node_class = hooks.ClassDef{
        .name = "TreeNode",
        .fields = &[_]hooks.Field{
            .{ .name = "value", .type_name = "int32_t", .kind = .value },
            .{ .name = "left", .type_name = "TreeNode", .kind = .object },
            .{ .name = "right", .type_name = "TreeNode", .kind = .object },
        },
    };

    const generated = try generator.generateHooks(tree_node_class);
    defer allocator.free(generated);

    // TreeNode_destroy should destroy both children
    try testing.expect(std.mem.indexOf(u8, generated, "TreeNode_destroy(self->left)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "TreeNode_destroy(self->right)") != null);

    // Count destroy calls (should be 2: left and right)
    const destroy_count = std.mem.count(u8, generated, "TreeNode_destroy(self->");
    try testing.expectEqual(@as(usize, 2), destroy_count);
}

// Test: Complex cycle (User → Profile → Settings → User)
test "HookGenerator: complex cycle User→Profile→Settings→User" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    const user_class = hooks.ClassDef{
        .name = "User",
        .fields = &[_]hooks.Field{
            .{ .name = "name", .type_name = "msString", .kind = .string },
            .{ .name = "profile", .type_name = "Profile", .kind = .object },
        },
    };

    const profile_class = hooks.ClassDef{
        .name = "Profile",
        .fields = &[_]hooks.Field{
            .{ .name = "bio", .type_name = "msString", .kind = .string },
            .{ .name = "settings", .type_name = "Settings", .kind = .object },
        },
    };

    const settings_class = hooks.ClassDef{
        .name = "Settings",
        .fields = &[_]hooks.Field{
            .{ .name = "theme", .type_name = "msString", .kind = .string },
            .{ .name = "owner", .type_name = "User", .kind = .object },
        },
    };

    // Generate hooks for all three
    const gen_user = try generator.generateHooks(user_class);
    defer allocator.free(gen_user);

    const gen_profile = try generator.generateHooks(profile_class);
    defer allocator.free(gen_profile);

    const gen_settings = try generator.generateHooks(settings_class);
    defer allocator.free(gen_settings);

    // Verify User calls Profile_destroy
    try testing.expect(std.mem.indexOf(u8, gen_user, "Profile_destroy(self->profile)") != null);

    // Verify Profile calls Settings_destroy
    try testing.expect(std.mem.indexOf(u8, gen_profile, "Settings_destroy(self->settings)") != null);

    // Verify Settings calls User_destroy (completing the cycle)
    try testing.expect(std.mem.indexOf(u8, gen_settings, "User_destroy(self->owner)") != null);
}

// Test: Graph node (multiple parents, multiple children)
test "HookGenerator: graph node with multiple edges" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    const graph_node_class = hooks.ClassDef{
        .name = "GraphNode",
        .fields = &[_]hooks.Field{
            .{ .name = "id", .type_name = "int32_t", .kind = .value },
            .{ .name = "neighbors", .type_name = "GraphNode[]", .kind = .array },
        },
    };

    const generated = try generator.generateHooks(graph_node_class);
    defer allocator.free(generated);

    // GraphNode_destroy should handle neighbors array
    try testing.expect(std.mem.indexOf(u8, generated, "GraphNode_destroy") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "// TODO: Clean up array neighbors") != null);

    // GraphNode_trace should trace neighbors
    try testing.expect(std.mem.indexOf(u8, generated, "GraphNode_trace") != null);
}

// =============================================================================
// Optional/Nullable Fields Tests
// =============================================================================

// Test: Optional field (nullable pointer)
test "HookGenerator: optional field handling" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    // In C, optional is represented as nullable pointer
    // We'll use naming convention: optional fields end with "_opt"
    const user_class = hooks.ClassDef{
        .name = "User",
        .fields = &[_]hooks.Field{
            .{ .name = "name", .type_name = "msString", .kind = .string },
            .{ .name = "email", .type_name = "msString", .kind = .string }, // Optional field
            .{ .name = "profile", .type_name = "Profile", .kind = .object }, // Optional object
        },
    };

    const generated = try generator.generateHooks(user_class);
    defer allocator.free(generated);

    // All hooks should check for NULL before accessing
    // (Current implementation already does this via "if (self == NULL) return;")
    try testing.expect(std.mem.indexOf(u8, generated, "if (self == NULL) return;") != null);

    // Verify hooks handle both required and optional fields
    try testing.expect(std.mem.indexOf(u8, generated, "ms_string_decref(self->name)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "ms_string_decref(self->email)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "Profile_destroy(self->profile)") != null);
}

// Test: Mixed optional and required fields
test "HookGenerator: mixed optional and required fields" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    const post_class = hooks.ClassDef{
        .name = "Post",
        .fields = &[_]hooks.Field{
            .{ .name = "title", .type_name = "msString", .kind = .string }, // Required
            .{ .name = "subtitle", .type_name = "msString", .kind = .string }, // Optional
            .{ .name = "author", .type_name = "User", .kind = .object }, // Required
            .{ .name = "editor", .type_name = "User", .kind = .object }, // Optional
        },
    };

    const generated = try generator.generateHooks(post_class);
    defer allocator.free(generated);

    // All fields should be handled in destroy
    try testing.expect(std.mem.indexOf(u8, generated, "ms_string_decref(self->title)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "ms_string_decref(self->subtitle)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "User_destroy(self->author)") != null);
    try testing.expect(std.mem.indexOf(u8, generated, "User_destroy(self->editor)") != null);

    // Count field cleanups (4 total)
    const string_count = std.mem.count(u8, generated, "ms_string_decref");
    const object_count = std.mem.count(u8, generated, "User_destroy");
    try testing.expectEqual(@as(usize, 2), string_count);
    try testing.expectEqual(@as(usize, 2), object_count);
}

// =============================================================================
// Integration: Circular Refs + Optional Fields
// =============================================================================

// Test: Circular references with optional fields
test "HookGenerator: circular refs with optional fields" {
    const allocator = testing.allocator;
    var generator = hooks.HookGenerator.init(allocator);
    defer generator.deinit();

    // Parent → Child (optional) → Parent (optional)
    const parent_class = hooks.ClassDef{
        .name = "Parent",
        .fields = &[_]hooks.Field{
            .{ .name = "name", .type_name = "msString", .kind = .string },
            .{ .name = "child", .type_name = "Child", .kind = .object }, // Optional
        },
    };

    const child_class = hooks.ClassDef{
        .name = "Child",
        .fields = &[_]hooks.Field{
            .{ .name = "name", .type_name = "msString", .kind = .string },
            .{ .name = "parent", .type_name = "Parent", .kind = .object }, // Optional
        },
    };

    const gen_parent = try generator.generateHooks(parent_class);
    defer allocator.free(gen_parent);

    const gen_child = try generator.generateHooks(child_class);
    defer allocator.free(gen_child);

    // Parent_destroy should handle optional child
    try testing.expect(std.mem.indexOf(u8, gen_parent, "Child_destroy(self->child)") != null);

    // Child_destroy should handle optional parent
    try testing.expect(std.mem.indexOf(u8, gen_child, "Parent_destroy(self->parent)") != null);

    // Both should have NULL checks
    try testing.expect(std.mem.indexOf(u8, gen_parent, "if (self == NULL) return;") != null);
    try testing.expect(std.mem.indexOf(u8, gen_child, "if (self == NULL) return;") != null);
}
