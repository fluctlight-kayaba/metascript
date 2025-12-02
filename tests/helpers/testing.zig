/// Metascript Test Utilities
///
/// Provides common testing helpers, assertions, and utilities for all test types.
/// Import this module in any test file for consistent testing patterns.

const std = @import("std");
const testing = std.testing;

// ============================================================================
// Test Allocator with Leak Detection
// ============================================================================

/// Get the test allocator - automatically checks for memory leaks
pub fn getAllocator() std.mem.Allocator {
    return std.testing.allocator;
}

/// Arena allocator for tests that don't need leak detection
pub fn getArenaAllocator(backing: std.mem.Allocator) std.heap.ArenaAllocator {
    return std.heap.ArenaAllocator.init(backing);
}

// ============================================================================
// String Assertions
// ============================================================================

/// Assert two strings are equal with nice diff output
pub fn expectEqualStrings(expected: []const u8, actual: []const u8) !void {
    try testing.expectEqualStrings(expected, actual);
}

/// Assert string contains substring
pub fn expectContains(haystack: []const u8, needle: []const u8) !void {
    if (std.mem.indexOf(u8, haystack, needle) == null) {
        std.debug.print("\n❌ Expected string to contain: \"{s}\"\n", .{needle});
        std.debug.print("   Actual: \"{s}\"\n", .{haystack});
        return error.TestExpectedContains;
    }
}

/// Assert string starts with prefix
pub fn expectStartsWith(str: []const u8, prefix: []const u8) !void {
    if (!std.mem.startsWith(u8, str, prefix)) {
        std.debug.print("\n❌ Expected string to start with: \"{s}\"\n", .{prefix});
        std.debug.print("   Actual: \"{s}\"\n", .{str});
        return error.TestExpectedStartsWith;
    }
}

/// Assert string ends with suffix
pub fn expectEndsWith(str: []const u8, suffix: []const u8) !void {
    if (!std.mem.endsWith(u8, str, suffix)) {
        std.debug.print("\n❌ Expected string to end with: \"{s}\"\n", .{suffix});
        std.debug.print("   Actual: \"{s}\"\n", .{str});
        return error.TestExpectedEndsWith;
    }
}

// ============================================================================
// Numeric Assertions
// ============================================================================

/// Assert value is within range [min, max]
pub fn expectInRange(comptime T: type, value: T, min: T, max: T) !void {
    if (value < min or value > max) {
        std.debug.print("\n❌ Expected value in range [{}, {}], got: {}\n", .{ min, max, value });
        return error.TestExpectedInRange;
    }
}

/// Assert value is approximately equal (for floats)
pub fn expectApproxEqual(expected: f64, actual: f64, tolerance: f64) !void {
    if (@abs(expected - actual) > tolerance) {
        std.debug.print("\n❌ Expected approximately {d}, got {d} (tolerance: {d})\n", .{ expected, actual, tolerance });
        return error.TestExpectedApproxEqual;
    }
}

// ============================================================================
// Collection Assertions
// ============================================================================

/// Assert slice has expected length
pub fn expectLength(comptime T: type, slice: []const T, expected_len: usize) !void {
    try testing.expectEqual(expected_len, slice.len);
}

/// Assert slice is empty
pub fn expectEmpty(comptime T: type, slice: []const T) !void {
    if (slice.len != 0) {
        std.debug.print("\n❌ Expected empty slice, got {} elements\n", .{slice.len});
        return error.TestExpectedEmpty;
    }
}

/// Assert slice is not empty
pub fn expectNotEmpty(comptime T: type, slice: []const T) !void {
    if (slice.len == 0) {
        std.debug.print("\n❌ Expected non-empty slice\n", .{});
        return error.TestExpectedNotEmpty;
    }
}

// ============================================================================
// Error Assertions
// ============================================================================

/// Assert that a function returns an error
pub fn expectError(comptime expected_error: anytype, result: anytype) !void {
    if (result) |_| {
        std.debug.print("\n❌ Expected error {}, got success\n", .{expected_error});
        return error.TestExpectedError;
    } else |actual_error| {
        try testing.expectEqual(expected_error, actual_error);
    }
}

/// Assert that a function succeeds (doesn't return error)
pub fn expectSuccess(result: anytype) !@TypeOf(result catch unreachable) {
    if (result) |value| {
        return value;
    } else |err| {
        std.debug.print("\n❌ Expected success, got error: {}\n", .{err});
        return error.TestExpectedSuccess;
    }
}

// ============================================================================
// Test Context / Setup & Teardown
// ============================================================================

/// Test context for managing test resources
pub const TestContext = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    temp_dir_path: ?[]const u8 = null,
    temp_files: std.ArrayList([]const u8),

    pub fn init() TestContext {
        const allocator = getAllocator();
        return .{
            .allocator = allocator,
            .arena = getArenaAllocator(allocator),
            .temp_files = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *TestContext) void {
        // Clean up temp files
        for (self.temp_files.items) |path| {
            std.fs.deleteFileAbsolute(path) catch {};
            self.allocator.free(path);
        }
        self.temp_files.deinit();

        // Clean up temp directory if created
        if (self.temp_dir_path) |dir_path| {
            std.fs.deleteTreeAbsolute(dir_path) catch {};
            self.allocator.free(dir_path);
        }

        self.arena.deinit();
    }

    /// Get arena allocator for test (no leak tracking, auto-freed)
    pub fn arenaAlloc(self: *TestContext) std.mem.Allocator {
        return self.arena.allocator();
    }

    /// Get or create temp directory
    fn getTempDir(self: *TestContext) ![]const u8 {
        if (self.temp_dir_path) |path| {
            return path;
        }

        // Create a unique temp directory
        const tmp_base = "/tmp/metascript_test_";
        const timestamp = @as(u64, @intCast(std.time.milliTimestamp()));
        const dir_name = try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ tmp_base, timestamp });
        errdefer self.allocator.free(dir_name);

        std.fs.makeDirAbsolute(dir_name) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        self.temp_dir_path = dir_name;
        return dir_name;
    }

    /// Create a temporary file with content
    pub fn createTempFile(self: *TestContext, name: []const u8, content: []const u8) ![]const u8 {
        const dir_path = try self.getTempDir();
        const file_path = try std.fmt.allocPrint(self.allocator, "{s}/{s}", .{ dir_path, name });
        errdefer self.allocator.free(file_path);

        // Write content to file
        const file = try std.fs.createFileAbsolute(file_path, .{});
        defer file.close();
        try file.writeAll(content);

        try self.temp_files.append(file_path);
        return file_path;
    }

    /// Read a temp file's content
    pub fn readTempFile(self: *TestContext, name: []const u8) ![]const u8 {
        const dir_path = self.temp_dir_path orelse return error.NoTempDir;
        const file_path = try std.fmt.allocPrint(self.arena.allocator(), "{s}/{s}", .{ dir_path, name });

        const file = try std.fs.openFileAbsolute(file_path, .{});
        defer file.close();

        return try file.readToEndAlloc(self.arena.allocator(), 1024 * 1024);
    }
};

// ============================================================================
// Snapshot Testing
// ============================================================================

/// Snapshot directory (relative to tests/)
const SNAPSHOT_DIR = "tests/snapshots";

/// Compare output against a saved snapshot.
/// If METASCRIPT_UPDATE_SNAPSHOTS=1, updates the snapshot instead of comparing.
pub fn expectSnapshot(name: []const u8, actual: []const u8) !void {
    const allocator = getAllocator();

    // Build snapshot path
    const snapshot_path = try std.fmt.allocPrint(allocator, "{s}/{s}.snap", .{ SNAPSHOT_DIR, name });
    defer allocator.free(snapshot_path);

    // Check if we should update snapshots
    const update_mode = std.posix.getenv("METASCRIPT_UPDATE_SNAPSHOTS") != null;

    if (update_mode) {
        // Update mode: write actual as new snapshot
        std.fs.makeDirAbsolute(SNAPSHOT_DIR) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        const file = try std.fs.createFileAbsolute(snapshot_path, .{});
        defer file.close();
        try file.writeAll(actual);
        std.debug.print("\n[SNAPSHOT] Updated: {s}\n", .{name});
        return;
    }

    // Compare mode: read snapshot and compare
    const file = std.fs.openFileAbsolute(snapshot_path, .{}) catch |err| {
        if (err == error.FileNotFound) {
            std.debug.print("\n[SNAPSHOT] Missing: {s}\n", .{name});
            std.debug.print("  Run with METASCRIPT_UPDATE_SNAPSHOTS=1 to create\n", .{});
            std.debug.print("  Actual value:\n{s}\n", .{actual});
            return error.SnapshotMissing;
        }
        return err;
    };
    defer file.close();

    const expected = try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(expected);

    if (!std.mem.eql(u8, expected, actual)) {
        std.debug.print("\n[SNAPSHOT] Mismatch: {s}\n", .{name});
        std.debug.print("  Run with METASCRIPT_UPDATE_SNAPSHOTS=1 to update\n", .{});
        std.debug.print("\n--- Expected ---\n{s}\n", .{expected});
        std.debug.print("\n--- Actual ---\n{s}\n", .{actual});
        return error.SnapshotMismatch;
    }
}

/// Snapshot for structured data (pretty-printed JSON-like format)
pub fn expectSnapshotJson(name: []const u8, value: anytype) !void {
    const allocator = getAllocator();

    // Format value as string
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    try std.json.stringify(value, .{ .whitespace = .indent_2 }, buffer.writer());

    try expectSnapshot(name, buffer.items);
}

// ============================================================================
// Benchmark Helpers
// ============================================================================

/// Simple timer for performance assertions
pub const Timer = struct {
    start_time: i64,

    pub fn start() Timer {
        return .{ .start_time = std.time.milliTimestamp() };
    }

    pub fn elapsed(self: Timer) i64 {
        return std.time.milliTimestamp() - self.start_time;
    }

    pub fn expectUnder(self: Timer, max_ms: i64) !void {
        const elapsed_ms = self.elapsed();
        if (elapsed_ms > max_ms) {
            std.debug.print("\n❌ Expected under {}ms, took {}ms\n", .{ max_ms, elapsed_ms });
            return error.TestTooSlow;
        }
    }
};

// ============================================================================
// Debug Helpers
// ============================================================================

/// Print a separator line for visual debugging
pub fn printSeparator(label: []const u8) void {
    std.debug.print("\n{'='repeated 60}\n", .{});
    std.debug.print("  {s}\n", .{label});
    std.debug.print("{'='repeated 60}\n", .{});
}

/// Pretty print a value for debugging
pub fn debugPrint(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("[DEBUG] " ++ fmt ++ "\n", args);
}

// ============================================================================
// Tests for Test Utilities (meta!)
// ============================================================================

test "expectContains finds substring" {
    try expectContains("hello world", "world");
    try expectContains("hello world", "hello");
    try expectContains("hello world", "o w");
}

test "expectStartsWith works" {
    try expectStartsWith("hello world", "hello");
    try expectStartsWith("hello world", "");
}

test "expectEndsWith works" {
    try expectEndsWith("hello world", "world");
    try expectEndsWith("hello world", "");
}

test "Timer measures time" {
    const timer = Timer.start();
    std.time.sleep(10 * std.time.ns_per_ms);
    const elapsed = timer.elapsed();
    try testing.expect(elapsed >= 10);
}

test "TestContext creates and cleans up temp files" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const path = try ctx.createTempFile("test_file.txt", "hello world");
    try testing.expect(path.len > 0);

    // File should exist
    const file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    const content = try file.readToEndAlloc(testing.allocator, 1024);
    defer testing.allocator.free(content);

    try testing.expectEqualStrings("hello world", content);
}

test "TestContext readTempFile works" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    _ = try ctx.createTempFile("read_test.txt", "test content");
    const content = try ctx.readTempFile("read_test.txt");

    try testing.expectEqualStrings("test content", content);
}

test "expectInRange works correctly" {
    try expectInRange(i32, 5, 0, 10);
    try expectInRange(i32, 0, 0, 10);
    try expectInRange(i32, 10, 0, 10);
}

test "expectEmpty works correctly" {
    const empty: []const u8 = "";
    try expectEmpty(u8, empty);
}

test "expectNotEmpty works correctly" {
    const non_empty = "hello";
    try expectNotEmpty(u8, non_empty);
}
