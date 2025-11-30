// JSON-RPC 2.0 implementation for LSP
// Handles reading/writing messages over stdin/stdout with proper protocol compliance

const std = @import("std");

/// LSP JSON-RPC error codes (from LSP spec)
pub const ErrorCode = struct {
    pub const ParseError: i32 = -32700;
    pub const InvalidRequest: i32 = -32600;
    pub const MethodNotFound: i32 = -32601;
    pub const InvalidParams: i32 = -32602;
    pub const InternalError: i32 = -32603;
    // LSP-specific
    pub const ServerNotInitialized: i32 = -32002;
    pub const UnknownErrorCode: i32 = -32001;
    pub const RequestCancelled: i32 = -32800;
    pub const ContentModified: i32 = -32801;
};

pub const ReadError = error{
    MissingContentLength,
    InvalidContentLength,
    HeaderTooLong,
    EndOfStream,
    OutOfMemory,
    InputOutput,
    Unexpected,
};

/// Maximum header line length (generous limit for complex Content-Type headers)
const MAX_HEADER_LINE: usize = 8192;

/// Read a JSON-RPC message from reader
/// LSP uses Content-Length header before JSON payload
pub fn readMessage(allocator: std.mem.Allocator, reader: anytype) ReadError!?[]const u8 {
    // Use dynamic buffer for headers (fixes 1024-byte limit issue)
    var header_buf = std.ArrayList(u8).init(allocator);
    defer header_buf.deinit();

    var content_length: ?usize = null;

    // Read headers until blank line
    while (true) {
        header_buf.clearRetainingCapacity();

        // Read until newline
        reader.streamUntilDelimiter(header_buf.writer(), '\n', MAX_HEADER_LINE) catch |err| {
            return switch (err) {
                error.EndOfStream => null,
                error.StreamTooLong => ReadError.HeaderTooLong,
                else => ReadError.InputOutput,
            };
        };

        const line = header_buf.items;

        // Trim \r if present (CRLF line endings per LSP spec)
        const trimmed = std.mem.trimRight(u8, line, "\r");

        // Empty line means headers are done
        if (trimmed.len == 0) break;

        // Parse Content-Length header (case-insensitive per HTTP spec)
        if (parseHeader(trimmed, "Content-Length")) |length_str| {
            content_length = std.fmt.parseInt(usize, length_str, 10) catch {
                return ReadError.InvalidContentLength;
            };
        }
        // Ignore other headers (Content-Type, etc.)
    }

    if (content_length == null) {
        return ReadError.MissingContentLength;
    }

    // Sanity check: reject absurdly large content
    const max_content: usize = 100 * 1024 * 1024; // 100MB max
    if (content_length.? > max_content) {
        return ReadError.InvalidContentLength;
    }

    // Read exact number of bytes for JSON content
    const content = allocator.alloc(u8, content_length.?) catch {
        return ReadError.OutOfMemory;
    };
    errdefer allocator.free(content);

    reader.readNoEof(content) catch {
        return ReadError.EndOfStream;
    };

    return content;
}

/// Parse a header line, returning value if header name matches (case-insensitive)
fn parseHeader(line: []const u8, name: []const u8) ?[]const u8 {
    // Header format: "Name: Value"
    const colon_pos = std.mem.indexOf(u8, line, ": ") orelse return null;
    const header_name = line[0..colon_pos];

    // Case-insensitive comparison
    if (std.ascii.eqlIgnoreCase(header_name, name)) {
        return line[colon_pos + 2 ..];
    }
    return null;
}

/// Write a JSON-RPC message to writer
pub fn writeMessage(writer: anytype, content: []const u8) !void {
    // Write Content-Length header with CRLF per LSP spec
    try writer.print("Content-Length: {d}\r\n\r\n", .{content.len});

    // Write JSON content
    try writer.writeAll(content);
}

// ============================================================================
// Unit Tests
// ============================================================================

const testing = std.testing;

/// Helper to create a FixedBufferStream from a string for testing
fn testReader(data: []const u8) std.io.FixedBufferStream([]const u8) {
    return std.io.fixedBufferStream(data);
}

test "readMessage: basic valid message" {
    const allocator = testing.allocator;
    const input = "Content-Length: 13\r\n\r\n{\"test\":true}";
    var stream = testReader(input);

    const result = try readMessage(allocator, stream.reader());
    defer if (result) |r| allocator.free(r);

    try testing.expect(result != null);
    try testing.expectEqualStrings("{\"test\":true}", result.?);
}

test "readMessage: handles LF-only line endings" {
    const allocator = testing.allocator;
    const input = "Content-Length: 13\n\n{\"test\":true}";
    var stream = testReader(input);

    const result = try readMessage(allocator, stream.reader());
    defer if (result) |r| allocator.free(r);

    try testing.expect(result != null);
    try testing.expectEqualStrings("{\"test\":true}", result.?);
}

test "readMessage: ignores Content-Type header" {
    const allocator = testing.allocator;
    const input = "Content-Length: 13\r\nContent-Type: application/json\r\n\r\n{\"test\":true}";
    var stream = testReader(input);

    const result = try readMessage(allocator, stream.reader());
    defer if (result) |r| allocator.free(r);

    try testing.expect(result != null);
    try testing.expectEqualStrings("{\"test\":true}", result.?);
}

test "readMessage: case-insensitive header name" {
    const allocator = testing.allocator;
    const input = "content-length: 13\r\n\r\n{\"test\":true}";
    var stream = testReader(input);

    const result = try readMessage(allocator, stream.reader());
    defer if (result) |r| allocator.free(r);

    try testing.expect(result != null);
    try testing.expectEqualStrings("{\"test\":true}", result.?);
}

test "readMessage: returns null on empty stream" {
    const allocator = testing.allocator;
    const input = "";
    var stream = testReader(input);

    const result = try readMessage(allocator, stream.reader());
    try testing.expect(result == null);
}

test "readMessage: missing Content-Length returns error" {
    const allocator = testing.allocator;
    const input = "Content-Type: application/json\r\n\r\n{\"test\":true}";
    var stream = testReader(input);

    const result = readMessage(allocator, stream.reader());
    try testing.expectError(ReadError.MissingContentLength, result);
}

test "readMessage: invalid Content-Length returns error" {
    const allocator = testing.allocator;
    const input = "Content-Length: abc\r\n\r\n{\"test\":true}";
    var stream = testReader(input);

    const result = readMessage(allocator, stream.reader());
    try testing.expectError(ReadError.InvalidContentLength, result);
}

test "readMessage: negative Content-Length returns error" {
    const allocator = testing.allocator;
    // Note: parseInt for usize will fail on negative numbers
    const input = "Content-Length: -50\r\n\r\n{\"test\":true}";
    var stream = testReader(input);

    const result = readMessage(allocator, stream.reader());
    try testing.expectError(ReadError.InvalidContentLength, result);
}

test "readMessage: multiple messages in sequence" {
    const allocator = testing.allocator;
    const input = "Content-Length: 13\r\n\r\n{\"test\":true}Content-Length: 14\r\n\r\n{\"test\":false}";
    var stream = testReader(input);
    const reader = stream.reader();

    // First message
    const result1 = try readMessage(allocator, reader);
    defer if (result1) |r| allocator.free(r);
    try testing.expect(result1 != null);
    try testing.expectEqualStrings("{\"test\":true}", result1.?);

    // Second message
    const result2 = try readMessage(allocator, reader);
    defer if (result2) |r| allocator.free(r);
    try testing.expect(result2 != null);
    try testing.expectEqualStrings("{\"test\":false}", result2.?);
}

test "readMessage: handles leading zeros in Content-Length" {
    const allocator = testing.allocator;
    const input = "Content-Length: 00013\r\n\r\n{\"test\":true}";
    var stream = testReader(input);

    const result = try readMessage(allocator, stream.reader());
    defer if (result) |r| allocator.free(r);

    try testing.expect(result != null);
    try testing.expectEqualStrings("{\"test\":true}", result.?);
}

test "readMessage: handles double space in header" {
    const allocator = testing.allocator;
    // "Content-Length:  13" has double space - value becomes " 13" which parseInt fails on
    const input = "Content-Length:  13\r\n\r\n{\"test\":true}";
    var stream = testReader(input);

    // parseInt fails on " 13" (leading space)
    const result = readMessage(allocator, stream.reader());
    try testing.expectError(ReadError.InvalidContentLength, result);
}

test "readMessage: Content-Length with extra whitespace" {
    const allocator = testing.allocator;
    // Trailing whitespace in value
    const input = "Content-Length: 13 \r\n\r\n{\"test\":true}";
    var stream = testReader(input);

    // parseInt handles trailing whitespace
    const result = readMessage(allocator, stream.reader());
    try testing.expectError(ReadError.InvalidContentLength, result);
}

test "writeMessage: formats correctly" {
    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    try writeMessage(writer, "{\"test\":true}");

    const expected = "Content-Length: 13\r\n\r\n{\"test\":true}";
    try testing.expectEqualStrings(expected, stream.getWritten());
}

test "writeMessage: empty content" {
    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    try writeMessage(writer, "");

    const expected = "Content-Length: 0\r\n\r\n";
    try testing.expectEqualStrings(expected, stream.getWritten());
}

test "parseHeader: extracts value correctly" {
    const result = parseHeader("Content-Length: 42", "Content-Length");
    try testing.expect(result != null);
    try testing.expectEqualStrings("42", result.?);
}

test "parseHeader: case insensitive match" {
    const result = parseHeader("content-length: 42", "Content-Length");
    try testing.expect(result != null);
    try testing.expectEqualStrings("42", result.?);
}

test "parseHeader: returns null for non-match" {
    const result = parseHeader("Content-Type: application/json", "Content-Length");
    try testing.expect(result == null);
}

test "parseHeader: handles complex values" {
    const result = parseHeader("Content-Type: application/vscode-jsonrpc; charset=utf-8", "Content-Type");
    try testing.expect(result != null);
    try testing.expectEqualStrings("application/vscode-jsonrpc; charset=utf-8", result.?);
}
