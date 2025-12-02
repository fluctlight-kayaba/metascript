const std = @import("std");
const testing = std.testing;

/// Field type classification for hook generation
pub const FieldKind = enum {
    value, // number, boolean (no RC needed)
    string, // msString (needs RC)
    object, // User-defined class (needs RC + recursive hooks)
    array, // Array of objects (needs RC + iteration)
};

/// Field definition in a class
pub const Field = struct {
    name: []const u8,
    type_name: []const u8,
    kind: FieldKind,
};

/// Class definition
pub const ClassDef = struct {
    name: []const u8,
    fields: []const Field,
};

/// Hook generator - generates lifecycle hooks for classes
pub const HookGenerator = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) HookGenerator {
        return HookGenerator{
            .allocator = allocator,
            .output = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *HookGenerator) void {
        self.output.deinit();
    }

    /// Generate all hooks for a class
    pub fn generateHooks(self: *HookGenerator, class: ClassDef) ![]const u8 {
        self.output.clearRetainingCapacity();

        // Only generate hooks if class has ref-counted fields
        if (!hasRefFields(class)) {
            return try self.output.toOwnedSlice();
        }

        try self.generateDestroyHook(class);
        try self.output.appendSlice("\n");
        try self.generateCopyHook(class);
        try self.output.appendSlice("\n");
        try self.generateSinkHook(class);
        try self.output.appendSlice("\n");
        try self.generateTraceHook(class);

        return try self.output.toOwnedSlice();
    }

    /// Generate =destroy hook
    fn generateDestroyHook(self: *HookGenerator, class: ClassDef) !void {
        const writer = self.output.writer();

        // Function signature
        try writer.print("// =destroy: Clean up {s} (decref fields)\n", .{class.name});
        try writer.print("void {s}_destroy({s}* self) {{\n", .{ class.name, class.name });
        try writer.writeAll("  if (self == NULL) return;\n\n");

        // Generate decref for each ref-counted field
        for (class.fields) |field| {
            switch (field.kind) {
                .value => {
                    try writer.print("  // {s}: value type, no cleanup\n", .{field.name});
                },
                .string => {
                    try writer.print("  ms_string_decref(self->{s});\n", .{field.name});
                },
                .object => {
                    try writer.print("  if (self->{s} != NULL) {{\n", .{field.name});
                    try writer.print("    {s}_destroy(self->{s});\n", .{ field.type_name, field.name });
                    try writer.print("    ms_decref(self->{s});\n", .{field.name});
                    try writer.writeAll("  }\n");
                },
                .array => {
                    // TODO: Array cleanup (Week 5-6)
                    try writer.print("  // TODO: Clean up array {s}\n", .{field.name});
                },
            }
        }

        try writer.writeAll("}\n");
    }

    /// Generate =copy hook
    fn generateCopyHook(self: *HookGenerator, class: ClassDef) !void {
        const writer = self.output.writer();

        try writer.print("// =copy: Copy {s} (incref fields)\n", .{class.name});
        try writer.print("{s}* {s}_copy({s}* self) {{\n", .{ class.name, class.name, class.name });
        try writer.writeAll("  if (self == NULL) return NULL;\n\n");

        try writer.print("  // Allocate new {s} with ORC\n", .{class.name});
        try writer.print("  {s}* copy = ({s}*)ms_alloc(sizeof({s}));\n\n", .{ class.name, class.name, class.name });

        // Copy each field
        for (class.fields) |field| {
            switch (field.kind) {
                .value => {
                    try writer.print("  copy->{s} = self->{s};  // Value type\n", .{ field.name, field.name });
                },
                .string => {
                    try writer.print("  copy->{s} = ms_string_clone(self->{s});\n", .{ field.name, field.name });
                },
                .object => {
                    try writer.print("  copy->{s} = {s}_copy(self->{s});\n", .{ field.name, field.type_name, field.name });
                },
                .array => {
                    try writer.print("  // TODO: Copy array {s}\n", .{field.name});
                },
            }
        }

        try writer.writeAll("\n  return copy;\n}\n");
    }

    /// Generate =sink hook (move semantics)
    fn generateSinkHook(self: *HookGenerator, class: ClassDef) !void {
        const writer = self.output.writer();

        try writer.print("// =sink: Move {s} (no RC change)\n", .{class.name});
        try writer.print("{s}* {s}_sink({s}* self) {{\n", .{ class.name, class.name, class.name });
        try writer.writeAll("  return self;  // Move semantics: just return pointer\n");
        try writer.writeAll("}\n");
    }

    /// Generate =trace hook (for cycle collector)
    fn generateTraceHook(self: *HookGenerator, class: ClassDef) !void {
        const writer = self.output.writer();

        try writer.writeAll("// =trace: Mark reachable objects (cycle collector)\n");
        try writer.print("void {s}_trace({s}* self) {{\n", .{ class.name, class.name });
        try writer.writeAll("  if (self == NULL) return;\n\n");

        // Trace ref-counted fields
        var has_traced = false;
        for (class.fields) |field| {
            switch (field.kind) {
                .value => {}, // No tracing needed
                .string => {
                    try writer.print("  // TODO: Mark string {s}\n", .{field.name});
                    has_traced = true;
                },
                .object => {
                    try writer.print("  if (self->{s} != NULL) {{\n", .{field.name});
                    try writer.print("    {s}_trace(self->{s});\n", .{ field.type_name, field.name });
                    try writer.writeAll("  }\n");
                    has_traced = true;
                },
                .array => {
                    try writer.print("  // TODO: Trace array {s}\n", .{field.name});
                    has_traced = true;
                },
            }
        }

        if (!has_traced) {
            try writer.writeAll("  // No ref fields to trace\n");
        }

        try writer.writeAll("}\n");
    }

    /// Check if class has any ref-counted fields
    fn hasRefFields(class: ClassDef) bool {
        for (class.fields) |field| {
            if (field.kind != .value) return true;
        }
        return false;
    }
};

// =============================================================================
// Tests (TDD: Tests define "done")
// =============================================================================

test "HookGenerator: class with only value types generates no hooks" {
    const allocator = testing.allocator;
    var generator = HookGenerator.init(allocator);
    defer generator.deinit();

    const class = ClassDef{
        .name = "Point",
        .fields = &[_]Field{
            .{ .name = "x", .type_name = "int32_t", .kind = .value },
            .{ .name = "y", .type_name = "int32_t", .kind = .value },
        },
    };

    const hooks = try generator.generateHooks(class);
    defer allocator.free(hooks);

    // No ref fields → no hooks generated
    try testing.expectEqual(@as(usize, 0), hooks.len);
}

test "HookGenerator: class with string field generates all hooks" {
    const allocator = testing.allocator;
    var generator = HookGenerator.init(allocator);
    defer generator.deinit();

    const class = ClassDef{
        .name = "User",
        .fields = &[_]Field{
            .{ .name = "name", .type_name = "msString", .kind = .string },
            .{ .name = "age", .type_name = "int32_t", .kind = .value },
        },
    };

    const hooks = try generator.generateHooks(class);
    defer allocator.free(hooks);

    // Should generate destroy, copy, sink, trace
    try testing.expect(hooks.len > 0);
    try testing.expect(std.mem.indexOf(u8, hooks, "User_destroy") != null);
    try testing.expect(std.mem.indexOf(u8, hooks, "User_copy") != null);
    try testing.expect(std.mem.indexOf(u8, hooks, "User_sink") != null);
    try testing.expect(std.mem.indexOf(u8, hooks, "User_trace") != null);

    // Destroy should decref string
    try testing.expect(std.mem.indexOf(u8, hooks, "ms_string_decref(self->name)") != null);

    // Copy should clone string
    try testing.expect(std.mem.indexOf(u8, hooks, "ms_string_clone(self->name)") != null);
}

test "HookGenerator: class with nested object generates recursive hooks" {
    const allocator = testing.allocator;
    var generator = HookGenerator.init(allocator);
    defer generator.deinit();

    const class = ClassDef{
        .name = "Post",
        .fields = &[_]Field{
            .{ .name = "title", .type_name = "msString", .kind = .string },
            .{ .name = "author", .type_name = "User", .kind = .object },
        },
    };

    const hooks = try generator.generateHooks(class);
    defer allocator.free(hooks);

    // Destroy should call User_destroy
    try testing.expect(std.mem.indexOf(u8, hooks, "User_destroy(self->author)") != null);

    // Copy should call User_copy
    try testing.expect(std.mem.indexOf(u8, hooks, "User_copy(self->author)") != null);

    // Trace should call User_trace
    try testing.expect(std.mem.indexOf(u8, hooks, "User_trace(self->author)") != null);
}

test "HookGenerator: output is valid C syntax" {
    const allocator = testing.allocator;
    var generator = HookGenerator.init(allocator);
    defer generator.deinit();

    const class = ClassDef{
        .name = "User",
        .fields = &[_]Field{
            .{ .name = "name", .type_name = "msString", .kind = .string },
            .{ .name = "age", .type_name = "int32_t", .kind = .value },
        },
    };

    const hooks = try generator.generateHooks(class);
    defer allocator.free(hooks);

    // Basic syntax checks
    try testing.expect(std.mem.indexOf(u8, hooks, "void User_destroy(User* self) {") != null);
    try testing.expect(std.mem.indexOf(u8, hooks, "User* User_copy(User* self) {") != null);
    try testing.expect(std.mem.indexOf(u8, hooks, "User* User_sink(User* self) {") != null);
    try testing.expect(std.mem.indexOf(u8, hooks, "void User_trace(User* self) {") != null);

    // Check for proper NULL checks
    try testing.expect(std.mem.indexOf(u8, hooks, "if (self == NULL) return") != null);
}
