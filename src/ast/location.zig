const std = @import("std");

/// Source file identifier
pub const FileId = u32;

/// Position in source file (line and column, 0-indexed)
pub const Position = struct {
    line: u32,
    column: u32,
};

/// Source location (file + start + end positions)
pub const SourceLocation = struct {
    file_id: FileId,
    start: Position,
    end: Position,

    pub fn init(file_id: FileId, start: Position, end: Position) SourceLocation {
        return .{
            .file_id = file_id,
            .start = start,
            .end = end,
        };
    }

    pub fn dummy() SourceLocation {
        return .{
            .file_id = 0,
            .start = .{ .line = 0, .column = 0 },
            .end = .{ .line = 0, .column = 0 },
        };
    }

    pub fn format(
        self: SourceLocation,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{}:{}:{}", .{
            self.file_id,
            self.start.line + 1, // Convert to 1-indexed for display
            self.start.column + 1,
        });
    }
};

/// File registry for mapping file IDs to paths
pub const FileRegistry = struct {
    files: std.StringArrayHashMap(FileId),
    paths: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,
    next_id: FileId,

    pub fn init(allocator: std.mem.Allocator) FileRegistry {
        return .{
            .files = std.StringArrayHashMap(FileId).init(allocator),
            .paths = std.ArrayList([]const u8).init(allocator),
            .allocator = allocator,
            .next_id = 1, // Start at 1, 0 reserved for dummy/unknown
        };
    }

    pub fn deinit(self: *FileRegistry) void {
        for (self.paths.items) |path| {
            self.allocator.free(path);
        }
        self.paths.deinit();
        self.files.deinit();
    }

    pub fn addFile(self: *FileRegistry, path: []const u8) !FileId {
        if (self.files.get(path)) |id| {
            return id;
        }

        const id = self.next_id;
        self.next_id += 1;

        const owned_path = try self.allocator.dupe(u8, path);
        try self.files.put(owned_path, id);
        try self.paths.append(owned_path);

        return id;
    }

    pub fn getPath(self: *const FileRegistry, file_id: FileId) ?[]const u8 {
        if (file_id == 0 or file_id >= self.next_id) return null;
        return self.paths.items[file_id - 1];
    }
};

test "SourceLocation formatting" {
    const loc = SourceLocation.init(1, .{ .line = 10, .column = 5 }, .{ .line = 10, .column = 15 });
    const str = try std.fmt.allocPrint(std.testing.allocator, "{}", .{loc});
    defer std.testing.allocator.free(str);
    try std.testing.expectEqualStrings("1:11:6", str);
}

test "FileRegistry basic operations" {
    var registry = FileRegistry.init(std.testing.allocator);
    defer registry.deinit();

    const id1 = try registry.addFile("hello.ms");
    const id2 = try registry.addFile("world.ms");
    const id1_again = try registry.addFile("hello.ms");

    try std.testing.expectEqual(id1, id1_again);
    try std.testing.expect(id1 != id2);

    try std.testing.expectEqualStrings("hello.ms", registry.getPath(id1).?);
    try std.testing.expectEqualStrings("world.ms", registry.getPath(id2).?);
}
