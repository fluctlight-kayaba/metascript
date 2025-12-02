const std = @import("std");
const string = @import("string.zig");
const testing = std.testing;

// =============================================================================
// Large String Tests
// =============================================================================

// Test: Very long string (1MB)
test "String edge: 1MB string allocation" {
    const allocator = testing.allocator;

    // Allocate 1MB string (1 million 'a' characters)
    const SIZE = 1_000_000;
    const large_data = try allocator.alloc(u8, SIZE);
    defer allocator.free(large_data);
    @memset(large_data, 'a');

    const str = try string.msString.new(allocator, large_data);
    defer str.decref(allocator);

    try testing.expectEqual(@as(usize, SIZE), str.len);
    try testing.expectEqual(@as(u8, 'a'), str.data[0]);
    try testing.expectEqual(@as(u8, 'a'), str.data[SIZE - 1]);
    try testing.expectEqual(@as(u32, 1), str.getRC());
}

// Test: Multiple large strings (10x 100KB)
test "String edge: multiple large strings" {
    const allocator = testing.allocator;

    const SIZE = 100_000;
    var strings = std.ArrayList(*string.msString).init(allocator);
    defer strings.deinit();

    // Allocate 10 large strings
    var i: usize = 0;
    while (i < 10) : (i += 1) {
        const data = try allocator.alloc(u8, SIZE);
        defer allocator.free(data);
        @memset(data, @intCast('A' + i));

        const str = try string.msString.new(allocator, data);
        try strings.append(str);
    }

    // Verify all strings
    for (strings.items, 0..) |str, idx| {
        try testing.expectEqual(@as(usize, SIZE), str.len);
        try testing.expectEqual(@as(u8, @intCast('A' + idx)), str.data[0]);
    }

    // Free all
    for (strings.items) |str| {
        str.decref(allocator);
    }
}

// Test: Concatenate to very long string
test "String edge: concatenate to 1MB string" {
    const allocator = testing.allocator;

    // Start with small string
    var result = try string.msString.new(allocator, "start");

    // Concatenate 10KB chunks 100 times (1MB total)
    const CHUNK_SIZE = 10_000;
    const chunk_data = try allocator.alloc(u8, CHUNK_SIZE);
    defer allocator.free(chunk_data);
    @memset(chunk_data, 'x');

    var i: usize = 0;
    while (i < 100) : (i += 1) {
        const chunk = try string.msString.new(allocator, chunk_data);
        const new_result = try string.msString.concat(allocator, result, chunk);

        result.decref(allocator);
        chunk.decref(allocator);
        result = new_result;
    }
    defer result.decref(allocator);

    // Verify size (5 + 100 * 10000)
    try testing.expectEqual(@as(usize, 5 + 100 * CHUNK_SIZE), result.len);
}

// =============================================================================
// UTF-8 Edge Cases
// =============================================================================

// Test: All valid 1-byte UTF-8 (ASCII)
test "String edge: all ASCII characters" {
    const allocator = testing.allocator;

    // Test all ASCII (0x00-0x7F)
    var i: u8 = 0;
    while (i < 128) : (i += 1) {
        const data = [_]u8{i};
        const str = try string.msString.new(allocator, &data);
        defer str.decref(allocator);

        try testing.expectEqual(@as(usize, 1), str.len);
        try testing.expectEqual(i, str.data[0]);
    }
}

// Test: 2-byte UTF-8 boundary (0xC2 0x80 = U+0080)
test "String edge: 2-byte UTF-8 boundary" {
    const allocator = testing.allocator;

    // U+0080 (first 2-byte character)
    const str1 = try string.msString.new(allocator, "\xC2\x80");
    defer str1.decref(allocator);
    try testing.expectEqual(@as(usize, 2), str1.len);

    // U+07FF (last 2-byte character)
    const str2 = try string.msString.new(allocator, "\xDF\xBF");
    defer str2.decref(allocator);
    try testing.expectEqual(@as(usize, 2), str2.len);
}

// Test: 3-byte UTF-8 boundary (U+0800, U+FFFF)
test "String edge: 3-byte UTF-8 boundary" {
    const allocator = testing.allocator;

    // U+0800 (first 3-byte character)
    const str1 = try string.msString.new(allocator, "\xE0\xA0\x80");
    defer str1.decref(allocator);
    try testing.expectEqual(@as(usize, 3), str1.len);

    // U+FFFF (last 3-byte character, excluding surrogates)
    const str2 = try string.msString.new(allocator, "\xEF\xBF\xBF");
    defer str2.decref(allocator);
    try testing.expectEqual(@as(usize, 3), str2.len);
}

// Test: 4-byte UTF-8 boundary (U+10000, U+10FFFF)
test "String edge: 4-byte UTF-8 boundary" {
    const allocator = testing.allocator;

    // U+10000 (first 4-byte character)
    const str1 = try string.msString.new(allocator, "\xF0\x90\x80\x80");
    defer str1.decref(allocator);
    try testing.expectEqual(@as(usize, 4), str1.len);

    // U+10FFFF (last valid Unicode)
    const str2 = try string.msString.new(allocator, "\xF4\x8F\xBF\xBF");
    defer str2.decref(allocator);
    try testing.expectEqual(@as(usize, 4), str2.len);
}

// Test: Invalid UTF-8 sequences
test "String edge: invalid UTF-8 sequences" {
    const allocator = testing.allocator;

    // Invalid start byte (0xC0, 0xC1)
    try testing.expectError(error.InvalidUtf8, string.msString.new(allocator, "\xC0\x80"));
    try testing.expectError(error.InvalidUtf8, string.msString.new(allocator, "\xC1\xBF"));

    // Invalid continuation byte
    try testing.expectError(error.InvalidUtf8, string.msString.new(allocator, "\xC2\x00"));

    // Truncated sequence (2-byte with only 1 byte)
    try testing.expectError(error.InvalidUtf8, string.msString.new(allocator, "\xC2"));

    // Surrogate pairs (U+D800-U+DFFF, invalid in UTF-8)
    try testing.expectError(error.InvalidUtf8, string.msString.new(allocator, "\xED\xA0\x80")); // U+D800

    // Beyond U+10FFFF
    try testing.expectError(error.InvalidUtf8, string.msString.new(allocator, "\xF4\x90\x80\x80")); // U+110000
}

// Test: String with NULL bytes
test "String edge: NULL bytes in middle" {
    const allocator = testing.allocator;

    const data = "Hello\x00World";
    const str = try string.msString.new(allocator, data);
    defer str.decref(allocator);

    try testing.expectEqual(@as(usize, 11), str.len);
    try testing.expectEqual(@as(u8, 'H'), str.data[0]);
    try testing.expectEqual(@as(u8, 0), str.data[5]);
    try testing.expectEqual(@as(u8, 'W'), str.data[6]);
}

// Test: String with all emoji (4-byte UTF-8)
test "String edge: emoji string" {
    const allocator = testing.allocator;

    const emoji = "😀😃😄😁😆😅🤣😂";
    const str = try string.msString.new(allocator, emoji);
    defer str.decref(allocator);

    // 8 emoji * 4 bytes each = 32 bytes
    try testing.expectEqual(@as(usize, 32), str.len);

    // Count code points (should be 8)
    const count = str.countCodePoints();
    try testing.expectEqual(@as(usize, 8), count);
}

// =============================================================================
// Reference Counting Edge Cases
// =============================================================================

// Test: 1000 references to same string
test "String edge: 1000 references to same string" {
    const allocator = testing.allocator;

    const str = try string.msString.new(allocator, "shared");
    defer str.decref(allocator);

    // Add 1000 references
    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        str.incref();
    }

    try testing.expectEqual(@as(u32, 1001), str.getRC());

    // Remove 1000 references
    i = 0;
    while (i < 1000) : (i += 1) {
        str.decref(allocator);
    }

    try testing.expectEqual(@as(u32, 1), str.getRC());
}

// Test: Interleaved incref/decref on string
test "String edge: interleaved incref/decref" {
    const allocator = testing.allocator;

    const str = try string.msString.new(allocator, "test");
    defer str.decref(allocator);

    try testing.expectEqual(@as(u32, 1), str.getRC());

    str.incref(); // 2
    str.incref(); // 3
    str.decref(allocator); // 2
    str.incref(); // 3
    str.decref(allocator); // 2
    str.decref(allocator); // 1

    try testing.expectEqual(@as(u32, 1), str.getRC());
}

// =============================================================================
// Substring Edge Cases
// =============================================================================

// Test: Substring at boundaries
test "String edge: substring boundaries" {
    const allocator = testing.allocator;

    const str = try string.msString.new(allocator, "Hello, World!");
    defer str.decref(allocator);

    // Empty substring (start == end)
    const empty = try str.substring(allocator, 0, 0);
    defer empty.decref(allocator);
    try testing.expectEqual(@as(usize, 0), empty.len);

    // Full string (0 to len)
    const full = try str.substring(allocator, 0, str.len);
    defer full.decref(allocator);
    try testing.expectEqual(str.len, full.len);

    // Last character only
    const last = try str.substring(allocator, str.len - 1, str.len);
    defer last.decref(allocator);
    try testing.expectEqual(@as(usize, 1), last.len);
    try testing.expectEqual(@as(u8, '!'), last.data[0]);

    // First character only
    const first = try str.substring(allocator, 0, 1);
    defer first.decref(allocator);
    try testing.expectEqual(@as(usize, 1), first.len);
    try testing.expectEqual(@as(u8, 'H'), first.data[0]);
}

// Test: Invalid substring ranges
test "String edge: invalid substring ranges" {
    const allocator = testing.allocator;

    const str = try string.msString.new(allocator, "Hello");
    defer str.decref(allocator);

    // start > end
    try testing.expectError(error.InvalidRange, str.substring(allocator, 3, 1));

    // end > length
    try testing.expectError(error.InvalidRange, str.substring(allocator, 0, 100));

    // start >= length
    try testing.expectError(error.InvalidRange, str.substring(allocator, 10, 20));
}

// =============================================================================
// Comparison Edge Cases
// =============================================================================

// Test: Compare identical long strings
test "String edge: compare identical 100KB strings" {
    const allocator = testing.allocator;

    const SIZE = 100_000;
    const data = try allocator.alloc(u8, SIZE);
    defer allocator.free(data);
    @memset(data, 'x');

    const str1 = try string.msString.new(allocator, data);
    defer str1.decref(allocator);

    const str2 = try string.msString.new(allocator, data);
    defer str2.decref(allocator);

    try testing.expect(str1.equals(str2));
    try testing.expectEqual(std.math.Order.eq, str1.compare(str2));
}

// Test: Compare strings differing only in last byte
test "String edge: compare strings with last byte different" {
    const allocator = testing.allocator;

    const SIZE = 10_000;
    var data1 = try allocator.alloc(u8, SIZE);
    defer allocator.free(data1);
    @memset(data1, 'a');
    data1[SIZE - 1] = 'b';

    var data2 = try allocator.alloc(u8, SIZE);
    defer allocator.free(data2);
    @memset(data2, 'a');
    data2[SIZE - 1] = 'c';

    const str1 = try string.msString.new(allocator, data1);
    defer str1.decref(allocator);

    const str2 = try string.msString.new(allocator, data2);
    defer str2.decref(allocator);

    try testing.expect(!str1.equals(str2));
    try testing.expectEqual(std.math.Order.lt, str1.compare(str2)); // 'b' < 'c'
}

// =============================================================================
// Stress: Rapid Allocation/Deallocation
// =============================================================================

// Test: Allocate and free 100K strings
test "String stress: 100K rapid alloc/free" {
    const allocator = testing.allocator;

    var i: usize = 0;
    while (i < 100_000) : (i += 1) {
        const str = try string.msString.new(allocator, "test");
        str.decref(allocator);
    }
}

// Test: Build and destroy 1000 concatenated strings
test "String stress: 1000 concatenations" {
    const allocator = testing.allocator;

    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        const str1 = try string.msString.new(allocator, "Hello");
        const str2 = try string.msString.new(allocator, "World");
        const result = try string.msString.concat(allocator, str1, str2);

        str1.decref(allocator);
        str2.decref(allocator);
        result.decref(allocator);
    }
}
