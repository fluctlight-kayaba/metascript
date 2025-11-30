// FileStore - Build-on-save pattern with stat tracking
// Based on Zig compiler's incremental compilation strategy
//
// Key Pattern: Lazy invalidation
// - Track file mtime + size (cheap stat check)
// - Only reparse when diagnostics requested (not on every save)
// - Keep prev_ir for incremental mapping

const std = @import("std");

/// Unique identifier for a source file (0 reserved for unknown/dummy)
pub const FileId = u32;

/// File status in the compilation pipeline
pub const FileStatus = enum {
    never_loaded,      // File never parsed
    success,           // Last parse succeeded
    parse_failure,     // Parse failed (syntax errors)
    io_error,          // File read failed

    pub fn isError(self: FileStatus) bool {
        return self == .parse_failure or self == .io_error;
    }
};

/// File metadata from filesystem
pub const FileStat = struct {
    mtime: i128,       // Modification time (nanoseconds since epoch)
    size: u64,         // File size in bytes

    /// Check if file has changed on disk
    pub fn hasChanged(self: FileStat, path: []const u8) !bool {
        const stat = std.fs.cwd().statFile(path) catch |err| {
            // File deleted or inaccessible
            return if (err == error.FileNotFound) true else err;
        };

        return self.mtime != stat.mtime or self.size != stat.size;
    }

    /// Create FileStat from path
    pub fn fromPath(path: []const u8) !FileStat {
        const stat = try std.fs.cwd().statFile(path);
        return .{
            .mtime = stat.mtime,
            .size = stat.size,
        };
    }
};

/// File entry in the store
pub const FileEntry = struct {
    /// File path (owned)
    path: []const u8,

    /// Current source text (owned, null if not loaded)
    text: ?[]u8,

    /// Filesystem metadata
    stat: FileStat,

    /// Compilation status
    status: FileStatus,

    /// Marked dirty (needs reparse)
    dirty: bool,

    /// LSP document version (for open editor documents)
    version: i32,

    /// Allocator for this entry (needed for text updates)
    allocator: std.mem.Allocator,

    pub fn deinit(self: *FileEntry, allocator: std.mem.Allocator) void {
        allocator.free(self.path);
        if (self.text) |txt| {
            allocator.free(txt);
        }
    }

    /// Apply incremental text change (for LSP didChange)
    pub fn applyChange(self: *FileEntry, range: ?Range, new_text: []const u8) !void {
        if (range) |r| {
            // Incremental update
            const start_offset = self.positionToOffset(r.start) orelse return error.InvalidRange;
            const end_offset = self.positionToOffset(r.end) orelse return error.InvalidRange;

            const old_text = self.text orelse return error.NoContent;
            const new_len = old_text.len - (end_offset - start_offset) + new_text.len;
            const new_content = try self.allocator.alloc(u8, new_len);

            // Copy prefix + new text + suffix
            @memcpy(new_content[0..start_offset], old_text[0..start_offset]);
            @memcpy(new_content[start_offset .. start_offset + new_text.len], new_text);
            @memcpy(new_content[start_offset + new_text.len ..], old_text[end_offset..]);

            self.allocator.free(old_text);
            self.text = new_content;
        } else {
            // Full document update
            if (self.text) |old| {
                self.allocator.free(old);
            }
            self.text = try self.allocator.dupe(u8, new_text);
        }
        self.dirty = true;
    }

    fn positionToOffset(self: *const FileEntry, pos: Position) ?usize {
        const content = self.text orelse return null;
        var line: u32 = 0;
        var col: u32 = 0;

        for (content, 0..) |c, i| {
            if (line == pos.line and col == pos.character) {
                return i;
            }
            if (c == '\n') {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        // Handle position at end of document
        if (line == pos.line and col == pos.character) {
            return content.len;
        }
        return null;
    }
};

/// LSP Position (line/character)
pub const Position = struct {
    line: u32,
    character: u32,
};

/// LSP Range (start/end positions)
pub const Range = struct {
    start: Position,
    end: Position,
};

/// FileStore - Manages source files with lazy invalidation
pub const FileStore = struct {
    allocator: std.mem.Allocator,
    files: std.AutoHashMap(FileId, FileEntry),
    path_to_id: std.StringHashMap(FileId),
    next_id: FileId,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .files = std.AutoHashMap(FileId, FileEntry).init(allocator),
            .path_to_id = std.StringHashMap(FileId).init(allocator),
            .next_id = 1, // Start at 1 (0 reserved for dummy/unknown)
        };
    }

    pub fn deinit(self: *Self) void {
        // Free file entries
        var it = self.files.valueIterator();
        while (it.next()) |entry| {
            var e = entry.*;
            e.deinit(self.allocator);
        }
        self.files.deinit();

        // Free path_to_id keys (owned strings)
        var key_it = self.path_to_id.keyIterator();
        while (key_it.next()) |key| {
            self.allocator.free(key.*);
        }
        self.path_to_id.deinit();
    }

    /// Add or update file in store (in-memory content, e.g., from LSP didOpen)
    /// Does NOT require file to exist on disk - for editor-provided content
    pub fn set(self: *Self, path: []const u8, text: []const u8, version: i32) !FileId {
        // Get or create file ID
        const file_id = try self.getOrCreateFileId(path);

        // Try to get stat (optional - file may not exist on disk for unsaved buffers)
        const stat = FileStat.fromPath(path) catch FileStat{
            .mtime = 0,
            .size = text.len,
        };

        // Duplicate text
        const owned_text = try self.allocator.dupe(u8, text);

        // Update or create entry
        if (self.files.getPtr(file_id)) |entry| {
            // Free old text
            if (entry.text) |old_text| {
                self.allocator.free(old_text);
            }

            entry.text = owned_text;
            entry.stat = stat;
            entry.status = .success;
            entry.dirty = true;
            entry.version = version;
        } else {
            const owned_path = try self.allocator.dupe(u8, path);
            try self.files.put(file_id, .{
                .path = owned_path,
                .text = owned_text,
                .stat = stat,
                .status = .success,
                .dirty = true,
                .version = version,
                .allocator = self.allocator,
            });
        }

        return file_id;
    }

    /// Get file text (returns null if not loaded)
    pub fn get(self: *Self, file_id: FileId) ?[]const u8 {
        const entry = self.files.get(file_id) orelse return null;
        return entry.text;
    }

    /// Get file by path (returns null if not loaded)
    pub fn getByPath(self: *Self, path: []const u8) ?[]const u8 {
        const file_id = self.path_to_id.get(path) orelse return null;
        return self.get(file_id);
    }

    /// Get mutable entry for applying changes (for LSP didChange)
    pub fn getEntry(self: *Self, path: []const u8) ?*FileEntry {
        const file_id = self.path_to_id.get(path) orelse return null;
        return self.files.getPtr(file_id);
    }

    /// Close/remove a file from the store (for LSP didClose)
    pub fn close(self: *Self, path: []const u8) void {
        const file_id = self.path_to_id.get(path) orelse return;

        // Remove and free file entry
        if (self.files.fetchRemove(file_id)) |kv| {
            var entry = kv.value;
            entry.deinit(self.allocator);
        }

        // Remove from path_to_id (key is owned, need to free)
        if (self.path_to_id.fetchRemove(path)) |kv| {
            self.allocator.free(kv.key);
        }
    }

    /// Load file from disk
    pub fn load(self: *Self, path: []const u8) !FileId {
        // Get or create file ID
        const file_id = try self.getOrCreateFileId(path);

        // Check if already loaded and not stale
        if (self.files.get(file_id)) |entry| {
            if (entry.text != null and !try entry.stat.hasChanged(path)) {
                // Already loaded and up to date
                return file_id;
            }
        }

        // Read file
        const text = std.fs.cwd().readFileAlloc(
            self.allocator,
            path,
            10 * 1024 * 1024, // Max 10MB
        ) catch |err| {
            // IO error
            if (self.files.getPtr(file_id)) |entry| {
                entry.status = .io_error;
            }
            return err;
        };

        // Get stat
        const stat = try FileStat.fromPath(path);

        // Update entry
        if (self.files.getPtr(file_id)) |entry| {
            if (entry.text) |old_text| {
                self.allocator.free(old_text);
            }
            entry.text = text;
            entry.stat = stat;
            entry.status = .success;
            entry.dirty = true;
        } else {
            const owned_path = try self.allocator.dupe(u8, path);
            try self.files.put(file_id, .{
                .path = owned_path,
                .text = text,
                .stat = stat,
                .status = .success,
                .dirty = true,
                .version = 0, // Disk files don't have LSP version
                .allocator = self.allocator,
            });
        }

        return file_id;
    }

    /// Check if file is stale (changed on disk)
    /// This is a CHEAP operation (just stat check, no file read)
    pub fn checkStale(self: *Self, file_id: FileId) !bool {
        const entry = self.files.get(file_id) orelse return true;
        return try entry.stat.hasChanged(entry.path);
    }

    /// Mark file dirty (needs reparse)
    pub fn markDirty(self: *Self, file_id: FileId) void {
        if (self.files.getPtr(file_id)) |entry| {
            entry.dirty = true;
        }
    }

    /// Clear dirty flag (after successful parse)
    pub fn markClean(self: *Self, file_id: FileId) void {
        if (self.files.getPtr(file_id)) |entry| {
            entry.dirty = false;
        }
    }

    /// Check if file needs reparse
    pub fn isDirty(self: *Self, file_id: FileId) bool {
        const entry = self.files.get(file_id) orelse return true;
        return entry.dirty;
    }

    /// Update file status (after parse attempt)
    pub fn setStatus(self: *Self, file_id: FileId, status: FileStatus) void {
        if (self.files.getPtr(file_id)) |entry| {
            entry.status = status;
        }
    }

    /// Get file status
    pub fn getStatus(self: *Self, file_id: FileId) FileStatus {
        const entry = self.files.get(file_id) orelse return .never_loaded;
        return entry.status;
    }

    /// Get file path from ID
    pub fn getPath(self: *Self, file_id: FileId) ?[]const u8 {
        const entry = self.files.get(file_id) orelse return null;
        return entry.path;
    }

    /// Reload file from disk if stale
    /// Returns true if file was reloaded
    pub fn reloadIfStale(self: *Self, file_id: FileId) !bool {
        const entry = self.files.get(file_id) orelse return false;

        if (!try entry.stat.hasChanged(entry.path)) {
            return false;  // Not stale
        }

        // Reload
        _ = try self.load(entry.path);
        return true;
    }

    // ===== INTERNAL HELPERS =====

    fn getOrCreateFileId(self: *Self, path: []const u8) !FileId {
        if (self.path_to_id.get(path)) |id| {
            return id;
        }

        const id = self.next_id;
        self.next_id += 1;

        const owned_path = try self.allocator.dupe(u8, path);
        try self.path_to_id.put(owned_path, id);

        return id;
    }
};

// ===== TESTS =====

test "FileStore: basic set and get" {
    var store = FileStore.init(std.testing.allocator);
    defer store.deinit();

    const file_id = try store.set("test.ms", "const x = 1;", 1);

    const text = store.get(file_id).?;
    try std.testing.expectEqualStrings("const x = 1;", text);

    const text2 = store.getByPath("test.ms").?;
    try std.testing.expectEqualStrings("const x = 1;", text2);
}

test "FileStore: dirty tracking" {
    var store = FileStore.init(std.testing.allocator);
    defer store.deinit();

    const file_id = try store.set("test.ms", "const x = 1;", 1);

    // Initially dirty
    try std.testing.expect(store.isDirty(file_id));

    // Mark clean
    store.markClean(file_id);
    try std.testing.expect(!store.isDirty(file_id));

    // Mark dirty again
    store.markDirty(file_id);
    try std.testing.expect(store.isDirty(file_id));
}

test "FileStore: status tracking" {
    var store = FileStore.init(std.testing.allocator);
    defer store.deinit();

    const file_id = try store.set("test.ms", "const x = 1;", 1);

    // Initially success
    try std.testing.expect(store.getStatus(file_id) == .success);

    // Set parse failure
    store.setStatus(file_id, .parse_failure);
    try std.testing.expect(store.getStatus(file_id) == .parse_failure);
    try std.testing.expect(store.getStatus(file_id).isError());
}

test "FileStore: path lookup" {
    var store = FileStore.init(std.testing.allocator);
    defer store.deinit();

    const file_id = try store.set("hello.ms", "const x = 1;", 1);

    const path = store.getPath(file_id).?;
    try std.testing.expectEqualStrings("hello.ms", path);
}

test "FileStore: update existing file" {
    var store = FileStore.init(std.testing.allocator);
    defer store.deinit();

    const id1 = try store.set("test.ms", "const x = 1;", 1);
    const id2 = try store.set("test.ms", "const y = 2;", 2);

    // Same ID
    try std.testing.expectEqual(id1, id2);

    // Updated text
    const text = store.get(id1).?;
    try std.testing.expectEqualStrings("const y = 2;", text);
}

test "FileStore: close removes file" {
    var store = FileStore.init(std.testing.allocator);
    defer store.deinit();

    _ = try store.set("test.ms", "const x = 1;", 1);
    try std.testing.expect(store.getByPath("test.ms") != null);

    store.close("test.ms");
    try std.testing.expect(store.getByPath("test.ms") == null);
}

test "FileEntry: applyChange incremental" {
    var store = FileStore.init(std.testing.allocator);
    defer store.deinit();

    _ = try store.set("test.ms", "hello world", 1);
    const entry = store.getEntry("test.ms").?;

    // Replace "world" with "universe"
    try entry.applyChange(.{
        .start = .{ .line = 0, .character = 6 },
        .end = .{ .line = 0, .character = 11 },
    }, "universe");

    try std.testing.expectEqualStrings("hello universe", entry.text.?);
}

test "FileEntry: applyChange full replace" {
    var store = FileStore.init(std.testing.allocator);
    defer store.deinit();

    _ = try store.set("test.ms", "old content", 1);
    const entry = store.getEntry("test.ms").?;

    // Full replace (no range)
    try entry.applyChange(null, "new content");

    try std.testing.expectEqualStrings("new content", entry.text.?);
}

test "FileStat: fromPath" {
    // Create temp file
    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    var temp_file = try temp_dir.dir.createFile("test.txt", .{});
    defer temp_file.close();

    try temp_file.writeAll("hello");

    // Get path
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = try temp_dir.dir.realpath("test.txt", &path_buf);

    // Get stat
    const stat = try FileStat.fromPath(path);

    try std.testing.expect(stat.size == 5);
    try std.testing.expect(stat.mtime > 0);

    // Check unchanged
    try std.testing.expect(!try stat.hasChanged(path));

    // Modify file
    std.time.sleep(1_000_000_000); // 1 second (ensure mtime changes)
    try temp_file.seekTo(0);
    try temp_file.writeAll("world!");

    // Check changed
    try std.testing.expect(try stat.hasChanged(path));
}
