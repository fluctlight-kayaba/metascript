// Lexer for Metascript
// Single-pass tokenization with @macro support
//
// Performance target: <1ms for 1000 LOC
// Pattern: Bun's single-pass state machine (no regex, optimized branching)

const std = @import("std");
const token = @import("token.zig");
const ast = @import("../ast/ast.zig");

const Token = token.Token;
const TokenKind = token.TokenKind;

/// Lexer state machine
pub const Lexer = struct {
    source: []const u8,
    file_id: ast.location.FileId,

    // Position tracking
    current: usize = 0,
    start: usize = 0,
    line: u32 = 1,
    column: u32 = 0,  // 0-indexed column (current position)
    start_column: u32 = 0,  // Column at token start
    start_line: u32 = 1,  // Line at token start

    // State
    had_newline_before: bool = false,

    // Keyword lookup
    keywords: token.KeywordMap,

    // Error tracking
    errors: std.ArrayList(LexError),
    allocator: std.mem.Allocator,

    const Self = @This();

    pub const LexError = struct {
        message: []const u8,
        loc: ast.SourceLocation,
    };

    pub fn init(allocator: std.mem.Allocator, source: []const u8, file_id: ast.location.FileId) !Self {
        return .{
            .source = source,
            .file_id = file_id,
            .keywords = try token.initKeywordMap(allocator),
            .errors = std.ArrayList(LexError).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.keywords.deinit();
        self.errors.deinit();
    }

    /// Get next token
    pub fn next(self: *Self) !Token {
        // Skip whitespace and regular comments, but capture JSDoc comments
        if (self.skipWhitespaceAndComments()) |doc_token| {
            return doc_token;
        }
        self.start = self.current;
        self.start_column = self.column;
        self.start_line = self.line;
        self.had_newline_before = false;

        if (self.isAtEnd()) {
            return self.makeToken(.end_of_file);
        }

        const c = self.advance();

        return switch (c) {
            // Macro tokens
            '@' => self.scanMacroToken(),

            // Identifiers and keywords
            'a'...'z', 'A'...'Z', '_', '$' => self.scanIdentifier(),

            // Numbers
            '0'...'9' => self.scanNumber(),

            // Strings
            '"', '\'' => self.scanString(c),
            '`' => self.scanTemplateString(),

            // Operators (single char)
            '(' => self.makeToken(.left_paren),
            ')' => self.makeToken(.right_paren),
            '{' => self.makeToken(.left_brace),
            '}' => self.makeToken(.right_brace),
            '[' => self.makeToken(.left_bracket),
            ']' => self.makeToken(.right_bracket),
            ';' => self.makeToken(.semicolon),
            ',' => self.makeToken(.comma),
            '~' => self.makeToken(.tilde),
            '?' => self.makeToken(.question),
            ':' => self.makeToken(.colon),

            // Operators (multi-char)
            '+' => if (self.match('=')) self.makeToken(.plus_equals)
                   else if (self.match('+')) self.makeToken(.plus_plus)
                   else self.makeToken(.plus),

            '-' => if (self.match('=')) self.makeToken(.minus_equals)
                   else if (self.match('-')) self.makeToken(.minus_minus)
                   else self.makeToken(.minus),

            '*' => if (self.match('=')) self.makeToken(.star_equals)
                   else if (self.match('*')) self.makeToken(.star_star)
                   else self.makeToken(.star),

            '/' => if (self.match('=')) self.makeToken(.slash_equals)
                   else self.makeToken(.slash),

            '%' => if (self.match('=')) self.makeToken(.percent_equals)
                   else self.makeToken(.percent),

            '=' => blk: {
                if (self.match('=')) {
                    if (self.match('=')) {
                        break :blk self.makeToken(.equals_equals_equals);
                    } else {
                        break :blk self.makeToken(.equals_equals);
                    }
                } else if (self.match('>')) {
                    break :blk self.makeToken(.arrow);
                } else {
                    break :blk self.makeToken(.equals);
                }
            },

            '!' => blk: {
                if (self.match('=')) {
                    if (self.match('=')) {
                        break :blk self.makeToken(.bang_equals_equals);
                    } else {
                        break :blk self.makeToken(.bang_equals);
                    }
                } else {
                    break :blk self.makeToken(.bang);
                }
            },

            '<' => if (self.match('=')) self.makeToken(.less_equals)
                   else if (self.match('<')) self.makeToken(.less_less)
                   else self.makeToken(.less_than),

            '>' => blk: {
                if (self.match('=')) {
                    break :blk self.makeToken(.greater_equals);
                } else if (self.match('>')) {
                    if (self.match('>')) {
                        break :blk self.makeToken(.greater_greater_greater);
                    } else {
                        break :blk self.makeToken(.greater_greater);
                    }
                } else {
                    break :blk self.makeToken(.greater_than);
                }
            },

            '&' => if (self.match('&')) self.makeToken(.ampersand_ampersand)
                   else self.makeToken(.ampersand),

            '|' => if (self.match('|')) self.makeToken(.pipe_pipe)
                   else self.makeToken(.pipe),

            '^' => self.makeToken(.caret),

            '.' => blk: {
                if (self.match('.')) {
                    if (self.match('.')) {
                        break :blk self.makeToken(.dot_dot_dot);
                    } else {
                        break :blk self.errorToken("Unexpected '..'");
                    }
                } else {
                    break :blk self.makeToken(.dot);
                }
            },

            else => self.errorToken("Unexpected character"),
        };
    }

    /// Scan macro token - just returns @ sign
    /// All macros are user-defined in std/macros/*.ms
    /// Parser combines @ with following identifier (e.g., @derive = @ + identifier("derive"))
    fn scanMacroToken(self: *Self) Token {
        return self.makeToken(.at_sign);
    }

    /// Scan identifier or keyword
    fn scanIdentifier(self: *Self) Token {
        while (self.isAlphaNumeric(self.peek())) {
            _ = self.advance();
        }

        const text = self.source[self.start..self.current];

        // Check if it's a keyword
        if (self.keywords.get(text)) |kind| {
            return self.makeToken(kind);
        }

        return self.makeToken(.identifier);
    }

    /// Scan number (integer or float)
    fn scanNumber(self: *Self) Token {
        // Handle hex (0x), binary (0b), octal (0o)
        if (self.source[self.start] == '0' and self.current < self.source.len) {
            const next_char = self.source[self.current];
            if (next_char == 'x' or next_char == 'X') {
                _ = self.advance();
                while (self.isHexDigit(self.peek())) {
                    _ = self.advance();
                }
                return self.makeToken(.number);
            } else if (next_char == 'b' or next_char == 'B') {
                _ = self.advance();
                while (self.peek() == '0' or self.peek() == '1') {
                    _ = self.advance();
                }
                return self.makeToken(.number);
            } else if (next_char == 'o' or next_char == 'O') {
                _ = self.advance();
                while (self.isOctalDigit(self.peek())) {
                    _ = self.advance();
                }
                return self.makeToken(.number);
            }
        }

        // Integer part
        while (self.isDigit(self.peek())) {
            _ = self.advance();
        }

        // Fractional part
        if (self.peek() == '.' and self.isDigit(self.peekNext())) {
            _ = self.advance();  // Consume '.'
            while (self.isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        // Exponent
        if (self.peek() == 'e' or self.peek() == 'E') {
            _ = self.advance();
            if (self.peek() == '+' or self.peek() == '-') {
                _ = self.advance();
            }
            while (self.isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(.number);
    }

    /// Scan string literal
    fn scanString(self: *Self, quote: u8) Token {
        while (!self.isAtEnd() and self.peek() != quote) {
            if (self.peek() == '\n') {
                _ = self.advance();
                self.line += 1;
                self.column = 0;
            } else if (self.peek() == '\\') {
                _ = self.advance();  // Escape char
                if (!self.isAtEnd()) {
                    _ = self.advance();  // Escaped char
                }
            } else {
                _ = self.advance();
            }
        }

        if (self.isAtEnd()) {
            return self.errorToken("Unterminated string");
        }

        _ = self.advance();  // Closing quote
        return self.makeToken(.string);
    }

    /// Scan template string (backticks)
    fn scanTemplateString(self: *Self) Token {
        while (!self.isAtEnd() and self.peek() != '`') {
            if (self.peek() == '\n') {
                _ = self.advance();
                self.line += 1;
                self.column = 0;
            } else if (self.peek() == '\\') {
                _ = self.advance();  // Escape char
                if (!self.isAtEnd()) {
                    _ = self.advance();  // Escaped char
                }
            } else {
                _ = self.advance();
            }
        }

        if (self.isAtEnd()) {
            return self.errorToken("Unterminated template string");
        }

        _ = self.advance();  // Closing backtick
        return self.makeToken(.template_string);
    }

    /// Skip whitespace and comments, returning JSDoc comment token if found
    fn skipWhitespaceAndComments(self: *Self) ?Token {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.had_newline_before = true;
                    _ = self.advance();
                    self.line += 1;
                    self.column = 0;  // Reset to 0 (0-indexed), advance() already incremented
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // Line comment - skip
                        while (!self.isAtEnd() and self.peek() != '\n') {
                            _ = self.advance();
                        }
                    } else if (self.peekNext() == '*') {
                        // Check if this is a JSDoc comment (/**)
                        const is_jsdoc = self.current + 2 < self.source.len and
                            self.source[self.current + 2] == '*' and
                            (self.current + 3 >= self.source.len or self.source[self.current + 3] != '/');

                        if (is_jsdoc) {
                            // JSDoc comment - capture it as a token
                            self.start = self.current;
                            self.start_column = self.column;
                            self.start_line = self.line;

                            _ = self.advance();  // /
                            _ = self.advance();  // *
                            _ = self.advance();  // * (second asterisk for JSDoc)

                            while (!self.isAtEnd()) {
                                if (self.peek() == '*' and self.peekNext() == '/') {
                                    _ = self.advance();  // *
                                    _ = self.advance();  // /
                                    break;
                                }
                                if (self.peek() == '\n') {
                                    _ = self.advance();
                                    self.line += 1;
                                    self.column = 0;
                                } else {
                                    _ = self.advance();
                                }
                            }

                            return self.makeToken(.doc_comment);
                        } else {
                            // Regular block comment - skip
                            _ = self.advance();  // /
                            _ = self.advance();  // *
                            while (!self.isAtEnd()) {
                                if (self.peek() == '*' and self.peekNext() == '/') {
                                    _ = self.advance();  // *
                                    _ = self.advance();  // /
                                    break;
                                }
                                if (self.peek() == '\n') {
                                    _ = self.advance();
                                    self.line += 1;
                                    self.column = 0;
                                } else {
                                    _ = self.advance();
                                }
                            }
                        }
                    } else {
                        return null;
                    }
                },
                else => return null,
            }
        }
    }

    // ===== HELPER FUNCTIONS =====

    fn advance(self: *Self) u8 {
        const c = self.source[self.current];
        self.current += 1;
        self.column += 1;
        return c;
    }

    fn peek(self: *Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn peekNext(self: *Self) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        self.column += 1;
        return true;
    }

    fn matchWord(self: *Self, word: []const u8) bool {
        const end = self.current + word.len;
        if (end > self.source.len) return false;

        const slice = self.source[self.current..end];
        if (std.mem.eql(u8, slice, word)) {
            // Ensure it's not part of a larger identifier
            if (end < self.source.len and self.isAlphaNumeric(self.source[end])) {
                return false;
            }
            self.current = end;
            self.column += @intCast(word.len);
            return true;
        }
        return false;
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }

    fn isDigit(self: *Self, c: u8) bool {
        _ = self;
        return c >= '0' and c <= '9';
    }

    fn isHexDigit(self: *Self, c: u8) bool {
        _ = self;
        return (c >= '0' and c <= '9') or
               (c >= 'a' and c <= 'f') or
               (c >= 'A' and c <= 'F');
    }

    fn isOctalDigit(self: *Self, c: u8) bool {
        _ = self;
        return c >= '0' and c <= '7';
    }

    fn isAlpha(self: *Self, c: u8) bool {
        _ = self;
        return (c >= 'a' and c <= 'z') or
               (c >= 'A' and c <= 'Z') or
               c == '_' or c == '$';
    }

    fn isAlphaNumeric(self: *Self, c: u8) bool {
        return self.isAlpha(c) or self.isDigit(c);
    }

    fn makeToken(self: *Self, kind: TokenKind) Token {
        const text = self.source[self.start..self.current];
        const loc = ast.SourceLocation{
            .file_id = self.file_id,
            .start = ast.location.Position{
                .line = self.start_line,
                .column = self.start_column,
            },
            .end = ast.location.Position{
                .line = self.line,
                .column = self.column,
            },
        };
        return Token.init(kind, loc, text);
    }

    fn errorToken(self: *Self, message: []const u8) Token {
        const loc = ast.SourceLocation{
            .file_id = self.file_id,
            .start = ast.location.Position{
                .line = self.start_line,
                .column = self.start_column,
            },
            .end = ast.location.Position{
                .line = self.line,
                .column = if (self.column > self.start_column) self.column else self.start_column + 1,
            },
        };

        // Record error
        self.errors.append(.{
            .message = message,
            .loc = loc,
        }) catch {};

        return Token.init(.syntax_error, loc, "");
    }
};

// ===== TESTS =====

test "lexer: basic tokens" {
    const source = "const x = 42;";
    var lexer = try Lexer.init(std.testing.allocator, source, 1);
    defer lexer.deinit();

    const tok1 = try lexer.next();
    try std.testing.expect(tok1.kind == .keyword_const);

    const tok2 = try lexer.next();
    try std.testing.expect(tok2.kind == .identifier);
    try std.testing.expectEqualStrings("x", tok2.text);

    const tok3 = try lexer.next();
    try std.testing.expect(tok3.kind == .equals);

    const tok4 = try lexer.next();
    try std.testing.expect(tok4.kind == .number);
    try std.testing.expectEqualStrings("42", tok4.text);

    const tok5 = try lexer.next();
    try std.testing.expect(tok5.kind == .semicolon);

    const tok6 = try lexer.next();
    try std.testing.expect(tok6.kind == .end_of_file);
}

test "lexer: macro tokens" {
    // @derive is now tokenized as @ + identifier("derive")
    const source = "@derive(Eq, Hash)";
    var lexer = try Lexer.init(std.testing.allocator, source, 1);
    defer lexer.deinit();

    const tok1 = try lexer.next();
    try std.testing.expect(tok1.kind == .at_sign);
    try std.testing.expectEqualStrings("@", tok1.text);

    const tok2 = try lexer.next();
    try std.testing.expect(tok2.kind == .identifier);
    try std.testing.expectEqualStrings("derive", tok2.text);

    const tok3 = try lexer.next();
    try std.testing.expect(tok3.kind == .left_paren);

    const tok4 = try lexer.next();
    try std.testing.expect(tok4.kind == .identifier);
    try std.testing.expectEqualStrings("Eq", tok4.text);

    const tok5 = try lexer.next();
    try std.testing.expect(tok5.kind == .comma);

    const tok6 = try lexer.next();
    try std.testing.expect(tok6.kind == .identifier);
    try std.testing.expectEqualStrings("Hash", tok6.text);
}

test "lexer: class with @derive" {
    const source =
        \\@derive(Eq, Hash)
        \\class User {
        \\  name: string;
        \\  age: number;
        \\}
    ;
    var lexer = try Lexer.init(std.testing.allocator, source, 1);
    defer lexer.deinit();

    const tok1 = try lexer.next();
    try std.testing.expect(tok1.kind == .at_sign);

    const tok2 = try lexer.next();
    try std.testing.expect(tok2.kind == .identifier);
    try std.testing.expectEqualStrings("derive", tok2.text);

    const tok3 = try lexer.next();
    try std.testing.expect(tok3.kind == .left_paren);

    // Skip to class keyword
    _ = try lexer.next();  // Eq
    _ = try lexer.next();  // ,
    _ = try lexer.next();  // Hash
    _ = try lexer.next();  // )

    const tok_class = try lexer.next();
    try std.testing.expect(tok_class.kind == .keyword_class);
}

test "lexer: numbers" {
    const source = "42 3.14 0xFF 0b1010 0o777";
    var lexer = try Lexer.init(std.testing.allocator, source, 1);
    defer lexer.deinit();

    const tok1 = try lexer.next();
    try std.testing.expect(tok1.kind == .number);
    try std.testing.expectEqualStrings("42", tok1.text);

    const tok2 = try lexer.next();
    try std.testing.expect(tok2.kind == .number);
    try std.testing.expectEqualStrings("3.14", tok2.text);

    const tok3 = try lexer.next();
    try std.testing.expect(tok3.kind == .number);
    try std.testing.expectEqualStrings("0xFF", tok3.text);

    const tok4 = try lexer.next();
    try std.testing.expect(tok4.kind == .number);
    try std.testing.expectEqualStrings("0b1010", tok4.text);

    const tok5 = try lexer.next();
    try std.testing.expect(tok5.kind == .number);
    try std.testing.expectEqualStrings("0o777", tok5.text);
}

test "lexer: strings" {
    const source = "\"hello\" 'world' `template`";
    var lexer = try Lexer.init(std.testing.allocator, source, 1);
    defer lexer.deinit();

    const tok1 = try lexer.next();
    try std.testing.expect(tok1.kind == .string);
    try std.testing.expectEqualStrings("\"hello\"", tok1.text);

    const tok2 = try lexer.next();
    try std.testing.expect(tok2.kind == .string);
    try std.testing.expectEqualStrings("'world'", tok2.text);

    const tok3 = try lexer.next();
    try std.testing.expect(tok3.kind == .template_string);
    try std.testing.expectEqualStrings("`template`", tok3.text);
}

test "lexer: operators" {
    const source = "+ - * / % == === != !== <= >= && || => ...";
    var lexer = try Lexer.init(std.testing.allocator, source, 1);
    defer lexer.deinit();

    const expected = [_]TokenKind{
        .plus, .minus, .star, .slash, .percent,
        .equals_equals, .equals_equals_equals,
        .bang_equals, .bang_equals_equals,
        .less_equals, .greater_equals,
        .ampersand_ampersand, .pipe_pipe,
        .arrow, .dot_dot_dot,
    };

    for (expected) |expected_kind| {
        const tok = try lexer.next();
        try std.testing.expect(tok.kind == expected_kind);
    }
}

test "lexer: comments" {
    const source =
        \\// Line comment
        \\const x = 42; /* block comment */ const y = 10;
    ;
    var lexer = try Lexer.init(std.testing.allocator, source, 1);
    defer lexer.deinit();

    const tok1 = try lexer.next();
    try std.testing.expect(tok1.kind == .keyword_const);

    const tok2 = try lexer.next();
    try std.testing.expect(tok2.kind == .identifier);
    try std.testing.expectEqualStrings("x", tok2.text);

    _ = try lexer.next();  // =
    _ = try lexer.next();  // 42
    _ = try lexer.next();  // ;

    const tok6 = try lexer.next();
    try std.testing.expect(tok6.kind == .keyword_const);

    const tok7 = try lexer.next();
    try std.testing.expect(tok7.kind == .identifier);
    try std.testing.expectEqualStrings("y", tok7.text);
}

test "lexer: JSDoc comments" {
    const source =
        \\/** This is a JSDoc comment */
        \\function foo() {}
    ;
    var lexer = try Lexer.init(std.testing.allocator, source, 1);
    defer lexer.deinit();

    // JSDoc comment should be captured as a token
    const doc_tok = try lexer.next();
    try std.testing.expect(doc_tok.kind == .doc_comment);
    try std.testing.expectEqualStrings("/** This is a JSDoc comment */", doc_tok.text);

    // Then the function declaration
    const func_tok = try lexer.next();
    try std.testing.expect(func_tok.kind == .keyword_function);
}

test "lexer: regular block comment vs JSDoc" {
    const source =
        \\/* Regular comment */
        \\/** JSDoc comment */
        \\const x = 1;
    ;
    var lexer = try Lexer.init(std.testing.allocator, source, 1);
    defer lexer.deinit();

    // JSDoc should be captured, regular comment skipped
    const doc_tok = try lexer.next();
    try std.testing.expect(doc_tok.kind == .doc_comment);
    try std.testing.expectEqualStrings("/** JSDoc comment */", doc_tok.text);

    // Then const
    const const_tok = try lexer.next();
    try std.testing.expect(const_tok.kind == .keyword_const);
}
