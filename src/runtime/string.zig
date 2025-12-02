const std = @import("std");
const testing = std.testing;
// Use module import when available, fall back to relative for standalone tests
const orc = @import("orc");

/// Metascript String - UTF-8 encoded with ORC
/// Memory layout: [RefHeader][msString struct][UTF-8 bytes]
pub const msString = struct {
    len: usize, // Length in bytes (NOT code points)
    capacity: usize, // Allocated capacity
    data: [*]u8, // Pointer to UTF-8 bytes (null-terminated)

    /// Create new string from UTF-8 bytes
    pub fn new(allocator: std.mem.Allocator, data: []const u8) !*msString {
        // Validate UTF-8
        if (!std.unicode.utf8ValidateSlice(data)) {
            return error.InvalidUtf8;
        }

        // Allocate: [RefHeader][msString][data + null terminator]
        const total_size = @sizeOf(orc.RefHeader) + @sizeOf(msString) + data.len + 1;
        const mem = try allocator.alignedAlloc(u8, @alignOf(orc.RefHeader), total_size);

        // Initialize RefHeader
        const header = @as(*orc.RefHeader, @ptrCast(@alignCast(mem.ptr)));
        header.* = orc.RefHeader.init();

        // Initialize msString
        const str = @as(*msString, @ptrCast(@alignCast(mem.ptr + @sizeOf(orc.RefHeader))));
        str.len = data.len;
        str.capacity = data.len;

        // Initialize data buffer (after msString struct)
        const data_ptr = mem.ptr + @sizeOf(orc.RefHeader) + @sizeOf(msString);
        str.data = @ptrCast(data_ptr);

        // Copy data and add null terminator
        @memcpy(str.data[0..data.len], data);
        str.data[data.len] = 0;

        return str;
    }

    /// Create empty string
    pub fn empty(allocator: std.mem.Allocator) !*msString {
        return new(allocator, "");
    }

    /// Get bytes as slice
    pub fn bytes(self: *const msString) []const u8 {
        return self.data[0..self.len];
    }

    /// Get RefHeader from string pointer
    fn getHeader(self: *msString) *orc.RefHeader {
        const str_addr = @intFromPtr(self);
        const header_addr = str_addr - @sizeOf(orc.RefHeader);
        return @ptrFromInt(header_addr);
    }

    /// Increment reference count
    pub fn incref(self: *msString) void {
        const header = self.getHeader();
        header.rc += 1;
    }

    /// Decrement reference count, free if rc=0
    pub fn decref(self: *msString, allocator: std.mem.Allocator) void {
        const header = self.getHeader();

        std.debug.assert(header.rc > 0); // Catch double-free

        header.rc -= 1;

        if (header.rc == 0) {
            // Free entire allocation (header + string + data)
            // Must free with the same alignment we allocated with
            const header_addr = @intFromPtr(header);
            const total_size = @sizeOf(orc.RefHeader) + @sizeOf(msString) + self.capacity + 1;
            const mem_ptr = @as([*]align(@alignOf(orc.RefHeader)) u8, @ptrFromInt(header_addr));
            const mem_slice = mem_ptr[0..total_size];
            allocator.free(mem_slice);
        }
    }

    /// Get current reference count (for testing/debugging)
    pub fn getRC(self: *msString) u32 {
        return self.getHeader().rc;
    }

    /// Concatenate two strings
    pub fn concat(allocator: std.mem.Allocator, a: *msString, b: *msString) !*msString {
        const new_len = a.len + b.len;

        // Allocate new string
        const total_size = @sizeOf(orc.RefHeader) + @sizeOf(msString) + new_len + 1;
        const mem = try allocator.alignedAlloc(u8, @alignOf(orc.RefHeader), total_size);

        // Initialize RefHeader
        const header = @as(*orc.RefHeader, @ptrCast(@alignCast(mem.ptr)));
        header.* = orc.RefHeader.init();

        // Initialize msString
        const result = @as(*msString, @ptrCast(@alignCast(mem.ptr + @sizeOf(orc.RefHeader))));
        result.len = new_len;
        result.capacity = new_len;

        // Initialize data buffer
        const data_ptr = mem.ptr + @sizeOf(orc.RefHeader) + @sizeOf(msString);
        result.data = @ptrCast(data_ptr);

        // Copy both strings
        @memcpy(result.data[0..a.len], a.bytes());
        @memcpy(result.data[a.len .. a.len + b.len], b.bytes());
        result.data[new_len] = 0;

        return result;
    }

    /// Extract substring [start..end)
    pub fn substring(self: *msString, allocator: std.mem.Allocator, start: usize, end: usize) !*msString {
        if (start > end or end > self.len) {
            return error.InvalidRange;
        }

        const sub_data = self.data[start..end];
        return new(allocator, sub_data);
    }

    /// Check equality
    pub fn equals(self: *const msString, other: *const msString) bool {
        if (self.len != other.len) return false;
        return std.mem.eql(u8, self.bytes(), other.bytes());
    }

    /// Compare strings (for sorting)
    pub fn compare(self: *const msString, other: *const msString) std.math.Order {
        return std.mem.order(u8, self.bytes(), other.bytes());
    }

    /// Check if string starts with prefix
    pub fn startsWith(self: *const msString, prefix: []const u8) bool {
        if (prefix.len > self.len) return false;
        return std.mem.eql(u8, self.data[0..prefix.len], prefix);
    }

    /// Check if string ends with suffix
    pub fn endsWith(self: *const msString, suffix: []const u8) bool {
        if (suffix.len > self.len) return false;
        const start = self.len - suffix.len;
        return std.mem.eql(u8, self.data[start..self.len], suffix);
    }

    /// Count UTF-8 code points (not bytes)
    pub fn countCodePoints(self: *const msString) usize {
        var count: usize = 0;
        var i: usize = 0;
        while (i < self.len) {
            const cp_len = std.unicode.utf8ByteSequenceLength(self.data[i]) catch 1;
            i += cp_len;
            count += 1;
        }
        return count;
    }
};

// =============================================================================
// Tests (TDD: Tests define "done")
// =============================================================================

test "msString: create from UTF-8 bytes" {
    const allocator = testing.allocator;

    const str = try msString.new(allocator, "hello");
    defer str.decref(allocator);

    try testing.expectEqual(@as(usize, 5), str.len);
    try testing.expectEqualStrings("hello", str.bytes());
    try testing.expectEqual(@as(u32, 1), str.getRC());
}

test "msString: create empty string" {
    const allocator = testing.allocator;

    const str = try msString.empty(allocator);
    defer str.decref(allocator);

    try testing.expectEqual(@as(usize, 0), str.len);
    try testing.expectEqualStrings("", str.bytes());
}

test "msString: UTF-8 validation rejects invalid bytes" {
    const allocator = testing.allocator;

    // Invalid UTF-8 sequence
    const invalid = [_]u8{ 0xFF, 0xFE };
    const result = msString.new(allocator, &invalid);

    try testing.expectError(error.InvalidUtf8, result);
}

test "msString: UTF-8 validation accepts valid multi-byte chars" {
    const allocator = testing.allocator;

    // Valid UTF-8: "Hello 世界" (Chinese characters)
    const str = try msString.new(allocator, "Hello 世界");
    defer str.decref(allocator);

    try testing.expectEqual(@as(usize, 12), str.len); // Bytes
    try testing.expectEqual(@as(usize, 8), str.countCodePoints()); // Code points
    try testing.expectEqualStrings("Hello 世界", str.bytes());
}

test "msString: incref increments reference count" {
    const allocator = testing.allocator;

    const str = try msString.new(allocator, "test");
    defer str.decref(allocator);

    try testing.expectEqual(@as(u32, 1), str.getRC());

    str.incref();
    try testing.expectEqual(@as(u32, 2), str.getRC());

    str.incref();
    try testing.expectEqual(@as(u32, 3), str.getRC());

    // Decref back to 1 for cleanup
    str.decref(allocator);
    str.decref(allocator);
}

test "msString: decref decrements and frees at rc=0" {
    const allocator = testing.allocator;

    const str = try msString.new(allocator, "test");

    str.incref(); // rc=2
    str.incref(); // rc=3

    str.decref(allocator); // rc=2
    try testing.expectEqual(@as(u32, 2), str.getRC());

    str.decref(allocator); // rc=1
    try testing.expectEqual(@as(u32, 1), str.getRC());

    str.decref(allocator); // rc=0, freed
}

test "msString: concat two strings" {
    const allocator = testing.allocator;

    const a = try msString.new(allocator, "hello");
    defer a.decref(allocator);

    const b = try msString.new(allocator, " world");
    defer b.decref(allocator);

    const result = try msString.concat(allocator, a, b);
    defer result.decref(allocator);

    try testing.expectEqual(@as(usize, 11), result.len);
    try testing.expectEqualStrings("hello world", result.bytes());
    try testing.expectEqual(@as(u32, 1), result.getRC());
}

test "msString: concat with empty string" {
    const allocator = testing.allocator;

    const a = try msString.new(allocator, "hello");
    defer a.decref(allocator);

    const b = try msString.empty(allocator);
    defer b.decref(allocator);

    const result = try msString.concat(allocator, a, b);
    defer result.decref(allocator);

    try testing.expectEqualStrings("hello", result.bytes());
}

test "msString: substring extraction" {
    const allocator = testing.allocator;

    const str = try msString.new(allocator, "hello world");
    defer str.decref(allocator);

    const sub = try str.substring(allocator, 0, 5);
    defer sub.decref(allocator);

    try testing.expectEqualStrings("hello", sub.bytes());
    try testing.expectEqual(@as(u32, 1), sub.getRC());
}

test "msString: substring with invalid range" {
    const allocator = testing.allocator;

    const str = try msString.new(allocator, "hello");
    defer str.decref(allocator);

    // start > end
    try testing.expectError(error.InvalidRange, str.substring(allocator, 5, 2));

    // end > len
    try testing.expectError(error.InvalidRange, str.substring(allocator, 0, 10));
}

test "msString: equality comparison" {
    const allocator = testing.allocator;

    const a = try msString.new(allocator, "hello");
    defer a.decref(allocator);

    const b = try msString.new(allocator, "hello");
    defer b.decref(allocator);

    const c = try msString.new(allocator, "world");
    defer c.decref(allocator);

    try testing.expect(a.equals(b));
    try testing.expect(!a.equals(c));
}

test "msString: comparison for sorting" {
    const allocator = testing.allocator;

    const a = try msString.new(allocator, "apple");
    defer a.decref(allocator);

    const b = try msString.new(allocator, "banana");
    defer b.decref(allocator);

    try testing.expectEqual(std.math.Order.lt, a.compare(b));
    try testing.expectEqual(std.math.Order.gt, b.compare(a));
    try testing.expectEqual(std.math.Order.eq, a.compare(a));
}

test "msString: startsWith check" {
    const allocator = testing.allocator;

    const str = try msString.new(allocator, "hello world");
    defer str.decref(allocator);

    try testing.expect(str.startsWith("hello"));
    try testing.expect(!str.startsWith("world"));
    try testing.expect(str.startsWith(""));
}

test "msString: endsWith check" {
    const allocator = testing.allocator;

    const str = try msString.new(allocator, "hello world");
    defer str.decref(allocator);

    try testing.expect(str.endsWith("world"));
    try testing.expect(!str.endsWith("hello"));
    try testing.expect(str.endsWith(""));
}

test "msString: count UTF-8 code points" {
    const allocator = testing.allocator;

    const ascii = try msString.new(allocator, "hello");
    defer ascii.decref(allocator);
    try testing.expectEqual(@as(usize, 5), ascii.countCodePoints());

    const unicode = try msString.new(allocator, "Hello 世界");
    defer unicode.decref(allocator);
    try testing.expectEqual(@as(usize, 12), unicode.len); // Bytes
    try testing.expectEqual(@as(usize, 8), unicode.countCodePoints()); // Code points
}

test "msString: no memory leaks with testing allocator" {
    const allocator = testing.allocator;

    // Create and properly free
    const str1 = try msString.new(allocator, "test1");
    str1.decref(allocator);

    // Create with extra refs, then free all
    const str2 = try msString.new(allocator, "test2");
    str2.incref();
    str2.incref();
    str2.decref(allocator);
    str2.decref(allocator);
    str2.decref(allocator);

    // Concat test
    const a = try msString.new(allocator, "a");
    const b = try msString.new(allocator, "b");
    const c = try msString.concat(allocator, a, b);
    a.decref(allocator);
    b.decref(allocator);
    c.decref(allocator);

    // Substring test
    const str3 = try msString.new(allocator, "hello world");
    const sub = try str3.substring(allocator, 0, 5);
    str3.decref(allocator);
    sub.decref(allocator);

    // If there are any leaks, testing.allocator will fail the test
}
