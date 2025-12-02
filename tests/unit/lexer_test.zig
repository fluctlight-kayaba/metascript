/// Lexer Unit Tests
///
/// Tests for the lexer component in isolation.
/// Follows TDD: write test first, then implement feature.

const std = @import("std");
const testing = std.testing;

// Import test utilities
const test_utils = @import("../helpers/testing.zig");
const fixtures = @import("../fixtures/sources.zig");

// Import the lexer
const lexer_mod = @import("../../src/lexer/lexer.zig");
const token_mod = @import("../../src/lexer/token.zig");

const Lexer = lexer_mod.Lexer;
const TokenKind = token_mod.TokenKind;

// ============================================================================
// Helper Functions
// ============================================================================

fn tokenize(source: []const u8) !Lexer {
    return try Lexer.init(testing.allocator, source, 1);
}

fn expectNextToken(lexer: *Lexer, expected_kind: TokenKind) !void {
    const token = try lexer.next();
    try testing.expectEqual(expected_kind, token.kind);
}

fn expectTokenText(lexer: *Lexer, expected_text: []const u8) !void {
    const token = try lexer.next();
    try testing.expectEqualStrings(expected_text, token.text);
}

// ============================================================================
// Basic Token Tests
// ============================================================================

test "lexer: empty input produces EOF" {
    var lexer = try tokenize(fixtures.EMPTY);
    defer lexer.deinit();

    try expectNextToken(&lexer, .end_of_file);
}

test "lexer: whitespace only produces EOF" {
    var lexer = try tokenize(fixtures.WHITESPACE_ONLY);
    defer lexer.deinit();

    try expectNextToken(&lexer, .end_of_file);
}

// ============================================================================
// Keyword Tests
// ============================================================================

test "lexer: tokenizes class keyword" {
    var lexer = try tokenize("class");
    defer lexer.deinit();

    try expectNextToken(&lexer, .keyword_class);
}

test "lexer: tokenizes function keyword" {
    var lexer = try tokenize("function");
    defer lexer.deinit();

    try expectNextToken(&lexer, .keyword_function);
}

test "lexer: tokenizes const keyword" {
    var lexer = try tokenize("const");
    defer lexer.deinit();

    try expectNextToken(&lexer, .keyword_const);
}

test "lexer: tokenizes let keyword" {
    var lexer = try tokenize("let");
    defer lexer.deinit();

    try expectNextToken(&lexer, .keyword_let);
}

test "lexer: tokenizes if keyword" {
    var lexer = try tokenize("if");
    defer lexer.deinit();

    try expectNextToken(&lexer, .keyword_if);
}

test "lexer: tokenizes else keyword" {
    var lexer = try tokenize("else");
    defer lexer.deinit();

    try expectNextToken(&lexer, .keyword_else);
}

test "lexer: tokenizes return keyword" {
    var lexer = try tokenize("return");
    defer lexer.deinit();

    try expectNextToken(&lexer, .keyword_return);
}

test "lexer: tokenizes import keyword" {
    var lexer = try tokenize("import");
    defer lexer.deinit();

    try expectNextToken(&lexer, .keyword_import);
}

test "lexer: tokenizes export keyword" {
    var lexer = try tokenize("export");
    defer lexer.deinit();

    try expectNextToken(&lexer, .keyword_export);
}

// ============================================================================
// Identifier Tests
// ============================================================================

test "lexer: tokenizes simple identifier" {
    var lexer = try tokenize("myVariable");
    defer lexer.deinit();

    const token = try lexer.next();
    try testing.expectEqual(TokenKind.identifier, token.kind);
    try testing.expectEqualStrings("myVariable", token.text);
}

test "lexer: tokenizes identifier with underscore" {
    var lexer = try tokenize("my_variable");
    defer lexer.deinit();

    const token = try lexer.next();
    try testing.expectEqual(TokenKind.identifier, token.kind);
    try testing.expectEqualStrings("my_variable", token.text);
}

test "lexer: tokenizes identifier with numbers" {
    var lexer = try tokenize("var123");
    defer lexer.deinit();

    const token = try lexer.next();
    try testing.expectEqual(TokenKind.identifier, token.kind);
    try testing.expectEqualStrings("var123", token.text);
}

// ============================================================================
// Number Literal Tests
// ============================================================================

test "lexer: tokenizes integer" {
    var lexer = try tokenize("42");
    defer lexer.deinit();

    const token = try lexer.next();
    try testing.expectEqual(TokenKind.number, token.kind);
    try testing.expectEqualStrings("42", token.text);
}

test "lexer: tokenizes float" {
    var lexer = try tokenize("3.14");
    defer lexer.deinit();

    const token = try lexer.next();
    try testing.expectEqual(TokenKind.number, token.kind);
    try testing.expectEqualStrings("3.14", token.text);
}

// ============================================================================
// String Literal Tests
// ============================================================================

test "lexer: tokenizes double-quoted string" {
    var lexer = try tokenize("\"hello\"");
    defer lexer.deinit();

    const token = try lexer.next();
    try testing.expectEqual(TokenKind.string, token.kind);
}

test "lexer: tokenizes single-quoted string" {
    var lexer = try tokenize("'hello'");
    defer lexer.deinit();

    const token = try lexer.next();
    try testing.expectEqual(TokenKind.string, token.kind);
}

// ============================================================================
// Operator Tests
// ============================================================================

test "lexer: tokenizes assignment operator" {
    var lexer = try tokenize("=");
    defer lexer.deinit();

    try expectNextToken(&lexer, .equals);
}

test "lexer: tokenizes equality operator" {
    var lexer = try tokenize("===");
    defer lexer.deinit();

    try expectNextToken(&lexer, .equals_equals_equals);
}

test "lexer: tokenizes plus operator" {
    var lexer = try tokenize("+");
    defer lexer.deinit();

    try expectNextToken(&lexer, .plus);
}

test "lexer: tokenizes arrow operator" {
    var lexer = try tokenize("=>");
    defer lexer.deinit();

    try expectNextToken(&lexer, .arrow);
}

// ============================================================================
// Punctuation Tests
// ============================================================================

test "lexer: tokenizes braces" {
    var lexer = try tokenize("{}");
    defer lexer.deinit();

    try expectNextToken(&lexer, .left_brace);
    try expectNextToken(&lexer, .right_brace);
}

test "lexer: tokenizes parentheses" {
    var lexer = try tokenize("()");
    defer lexer.deinit();

    try expectNextToken(&lexer, .left_paren);
    try expectNextToken(&lexer, .right_paren);
}

test "lexer: tokenizes brackets" {
    var lexer = try tokenize("[]");
    defer lexer.deinit();

    try expectNextToken(&lexer, .left_bracket);
    try expectNextToken(&lexer, .right_bracket);
}

test "lexer: tokenizes semicolon" {
    var lexer = try tokenize(";");
    defer lexer.deinit();

    try expectNextToken(&lexer, .semicolon);
}

test "lexer: tokenizes colon" {
    var lexer = try tokenize(":");
    defer lexer.deinit();

    try expectNextToken(&lexer, .colon);
}

test "lexer: tokenizes comma" {
    var lexer = try tokenize(",");
    defer lexer.deinit();

    try expectNextToken(&lexer, .comma);
}

// ============================================================================
// Macro Token Tests
// ============================================================================

test "lexer: tokenizes @derive as @ + identifier" {
    // All macros are now @ + identifier
    var lexer = try tokenize("@derive");
    defer lexer.deinit();

    try expectNextToken(&lexer, .at_sign);
    const token = try lexer.next();
    try testing.expectEqual(TokenKind.identifier, token.kind);
    try testing.expectEqualStrings("derive", token.text);
}

test "lexer: tokenizes @macro as @ + identifier" {
    var lexer = try tokenize("@macro");
    defer lexer.deinit();

    try expectNextToken(&lexer, .at_sign);
    const token = try lexer.next();
    try testing.expectEqual(TokenKind.identifier, token.kind);
    try testing.expectEqualStrings("macro", token.text);
}

test "lexer: tokenizes @comptime as @ + identifier" {
    var lexer = try tokenize("@comptime");
    defer lexer.deinit();

    try expectNextToken(&lexer, .at_sign);
    const token = try lexer.next();
    try testing.expectEqual(TokenKind.identifier, token.kind);
    try testing.expectEqualStrings("comptime", token.text);
}

test "lexer: tokenizes @ followed by custom identifier" {
    var lexer = try tokenize("@customMacro");
    defer lexer.deinit();

    try expectNextToken(&lexer, .at_sign);
    const token = try lexer.next();
    try testing.expectEqual(TokenKind.identifier, token.kind);
    try testing.expectEqualStrings("customMacro", token.text);
}

// ============================================================================
// Complex Expression Tests
// ============================================================================

test "lexer: tokenizes variable declaration" {
    var lexer = try tokenize("const x = 42;");
    defer lexer.deinit();

    try expectNextToken(&lexer, .keyword_const);
    const id = try lexer.next();
    try testing.expectEqual(TokenKind.identifier, id.kind);
    try testing.expectEqualStrings("x", id.text);
    try expectNextToken(&lexer, .equals);
    const num = try lexer.next();
    try testing.expectEqual(TokenKind.number, num.kind);
    try expectNextToken(&lexer, .semicolon);
}

test "lexer: tokenizes function call" {
    var lexer = try tokenize("foo(1, 2)");
    defer lexer.deinit();

    const id = try lexer.next();
    try testing.expectEqual(TokenKind.identifier, id.kind);
    try testing.expectEqualStrings("foo", id.text);
    try expectNextToken(&lexer, .left_paren);
    _ = try lexer.next(); // 1
    try expectNextToken(&lexer, .comma);
    _ = try lexer.next(); // 2
    try expectNextToken(&lexer, .right_paren);
}

// ============================================================================
// Comment Tests
// ============================================================================

test "lexer: skips single-line comment" {
    var lexer = try tokenize("// comment\n42");
    defer lexer.deinit();

    const token = try lexer.next();
    try testing.expectEqual(TokenKind.number, token.kind);
}

test "lexer: skips multi-line comment" {
    var lexer = try tokenize("/* comment */ 42");
    defer lexer.deinit();

    const token = try lexer.next();
    try testing.expectEqual(TokenKind.number, token.kind);
}

// ============================================================================
// Location Tracking Tests
// ============================================================================

test "lexer: tracks line and column" {
    var lexer = try tokenize("const\nx");
    defer lexer.deinit();

    const const_token = try lexer.next();
    try testing.expectEqual(@as(u32, 1), const_token.loc.start.line);
    try testing.expectEqual(@as(u32, 0), const_token.loc.start.column);

    const x_token = try lexer.next();
    try testing.expectEqual(@as(u32, 2), x_token.loc.start.line);
}
