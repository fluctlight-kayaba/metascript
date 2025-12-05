/// Low-level emission helpers for JavaScript code generation
/// Handles indentation, output buffer management, and operator mapping.

const std = @import("std");
const node_mod = @import("../../ast/node.zig");

// Forward declare the generator type to avoid circular imports
const JSGenerator = @import("jsgen.zig").JSGenerator;

/// Append string to output buffer
pub fn emit(gen: *JSGenerator, str: []const u8) !void {
    try gen.output.appendSlice(str);
}

/// Emit current indentation level
pub fn emitIndent(gen: *JSGenerator) !void {
    for (0..gen.indent_level) |_| {
        try emit(gen, "  ");
    }
}

/// Emit a simple statement (break, continue)
pub fn emitSimpleStmt(gen: *JSGenerator, keyword: []const u8) !void {
    try emitIndent(gen);
    try emit(gen, keyword);
    try emit(gen, ";\n");
}

/// Emit a newline
pub fn emitNewline(gen: *JSGenerator) !void {
    try emit(gen, "\n");
}

/// Emit a semicolon and newline
pub fn emitSemicolon(gen: *JSGenerator) !void {
    try emit(gen, ";\n");
}

/// Convert binary operator to JavaScript operator string
pub fn binaryOpToJS(op: node_mod.BinaryOp) []const u8 {
    return switch (op) {
        .assign => "=",
        .add => "+",
        .sub => "-",
        .mul => "*",
        .div => "/",
        .mod => "%",
        .eq => "===",
        .ne => "!==",
        .lt => "<",
        .le => "<=",
        .gt => ">",
        .ge => ">=",
        .@"and" => "&&",
        .@"or" => "||",
        .nullish_coalesce => "??",
        .bit_and => "&",
        .bit_or => "|",
        .bit_xor => "^",
        .shl => "<<",
        .shr => ">>",
    };
}

/// Convert unary operator to JavaScript operator string
pub fn unaryOpToJS(op: node_mod.UnaryOp) []const u8 {
    return switch (op) {
        .neg => "-",
        .not => "!",
        .bit_not => "~",
        .typeof => "typeof ",
        .void => "void ",
    };
}

/// Escape a string for JavaScript output
pub fn escapeString(allocator: std.mem.Allocator, str: []const u8) ![]const u8 {
    var escaped = std.ArrayList(u8).init(allocator);
    errdefer escaped.deinit();

    for (str) |c| {
        switch (c) {
            '\\' => try escaped.appendSlice("\\\\"),
            '"' => try escaped.appendSlice("\\\""),
            '\n' => try escaped.appendSlice("\\n"),
            '\r' => try escaped.appendSlice("\\r"),
            '\t' => try escaped.appendSlice("\\t"),
            else => try escaped.append(c),
        }
    }

    return try escaped.toOwnedSlice();
}

// =============================================================================
// Tests
// =============================================================================

test "emit: binaryOpToJS" {
    try std.testing.expectEqualStrings("+", binaryOpToJS(.add));
    try std.testing.expectEqualStrings("===", binaryOpToJS(.eq));
    try std.testing.expectEqualStrings("&&", binaryOpToJS(.@"and"));
}

test "emit: unaryOpToJS" {
    try std.testing.expectEqualStrings("-", unaryOpToJS(.neg));
    try std.testing.expectEqualStrings("!", unaryOpToJS(.not));
    try std.testing.expectEqualStrings("typeof ", unaryOpToJS(.typeof));
}

test "emit: escapeString" {
    const allocator = std.testing.allocator;

    const result = try escapeString(allocator, "hello\nworld");
    defer allocator.free(result);

    try std.testing.expectEqualStrings("hello\\nworld", result);
}

test "emit: escapeString with quotes" {
    const allocator = std.testing.allocator;

    const result = try escapeString(allocator, "say \"hello\"");
    defer allocator.free(result);

    try std.testing.expectEqualStrings("say \\\"hello\\\"", result);
}
