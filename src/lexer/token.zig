// Token definitions for Metascript
// Extends TypeScript syntax with macro tokens (@derive, @comptime, etc.)

const std = @import("std");
const ast = @import("../ast/ast.zig");

/// Token kinds - all TypeScript tokens + macro extensions
pub const TokenKind = enum {
    // ===== Literals =====
    number,           // 42, 3.14, 0x1A
    string,           // "hello", 'world'
    template_string,  // `template ${x}`
    regex,            // /pattern/flags

    // ===== Comments =====
    doc_comment,      // /** JSDoc comment */

    // ===== Identifiers =====
    identifier,       // foo, myVar

    // ===== Keywords (TypeScript) =====
    keyword_break,
    keyword_case,
    keyword_catch,
    keyword_class,
    keyword_const,
    keyword_continue,
    keyword_debugger,
    keyword_default,
    keyword_delete,
    keyword_do,
    keyword_else,
    keyword_enum,
    keyword_export,
    keyword_extends,
    keyword_false,
    keyword_finally,
    keyword_for,
    keyword_function,
    keyword_if,
    keyword_import,
    keyword_in,
    keyword_instanceof,
    keyword_interface,
    keyword_let,
    keyword_new,
    keyword_null,
    keyword_return,
    keyword_super,
    keyword_switch,
    keyword_this,
    keyword_throw,
    keyword_true,
    keyword_try,
    keyword_typeof,
    keyword_var,
    keyword_void,
    keyword_while,
    keyword_with,

    // TypeScript-specific keywords
    keyword_abstract,
    keyword_as,
    keyword_async,
    keyword_await,
    keyword_constructor,
    keyword_declare,
    keyword_from,
    keyword_get,
    keyword_implements,
    keyword_is,
    keyword_keyof,
    keyword_namespace,
    keyword_never,
    keyword_of,
    keyword_private,
    keyword_protected,
    keyword_public,
    keyword_readonly,
    keyword_require,
    keyword_set,
    keyword_static,
    keyword_type,
    keyword_unknown,

    // Metascript sized types (C backend)
    keyword_int8,
    keyword_int16,
    keyword_int32,
    keyword_int64,
    keyword_uint8,
    keyword_uint16,
    keyword_uint32,
    keyword_uint64,
    keyword_float32,
    keyword_float64,

    // Metascript type aliases
    keyword_int,      // → int32
    keyword_float,    // → float32
    keyword_double,   // → float64

    // Metascript keywords
    keyword_defer,    // defer cleanup
    keyword_distinct, // distinct type

    // Metascript macro keywords (Nim-style)
    keyword_macro,    // macro definition: macro derive(ctx) { }
    keyword_quote,    // AST quotation: quote { ... }
    keyword_extern,   // extern declaration: extern function, extern class

    // ===== Macro Tokens (Metascript extensions) =====
    // All macros are now user-defined in std/macros/*.ms
    // The lexer only produces @ as a token, parser combines with identifier
    at_sign,          // @ (all macros start with this)

    // ===== Operators =====
    // Arithmetic
    plus,             // +
    minus,            // -
    star,             // *
    slash,            // /
    percent,          // %
    star_star,        // **

    // Assignment
    equals,           // =
    plus_equals,      // +=
    minus_equals,     // -=
    star_equals,      // *=
    slash_equals,     // /=
    percent_equals,   // %=

    // Comparison
    equals_equals,    // ==
    equals_equals_equals,  // ===
    bang_equals,      // !=
    bang_equals_equals,    // !==
    less_than,        // <
    less_equals,      // <=
    greater_than,     // >
    greater_equals,   // >=

    // Logical
    ampersand_ampersand,  // &&
    pipe_pipe,            // ||
    bang,                 // !

    // Bitwise
    ampersand,        // &
    pipe,             // |
    caret,            // ^
    tilde,            // ~
    less_less,        // <<
    greater_greater,  // >>
    greater_greater_greater,  // >>>

    // Increment/Decrement
    plus_plus,        // ++
    minus_minus,      // --

    // Other
    question,         // ?
    dot,              // .
    dot_dot_dot,      // ...
    arrow,            // =>

    // ===== Punctuation =====
    left_paren,       // (
    right_paren,      // )
    left_brace,       // {
    right_brace,      // }
    left_bracket,     // [
    right_bracket,    // ]
    semicolon,        // ;
    colon,            // :
    comma,            // ,

    // ===== Special =====
    newline,          // \n (significant in some contexts)
    end_of_file,
    syntax_error,

    /// Get string representation for debugging
    pub fn toString(self: TokenKind) []const u8 {
        return switch (self) {
            .number => "number",
            .string => "string",
            .identifier => "identifier",
            .keyword_function => "function",
            .keyword_class => "class",
            .at_sign => "@",
            .equals => "=",
            .semicolon => ";",
            .end_of_file => "EOF",
            else => @tagName(self),
        };
    }
};

/// Token with location information
pub const Token = struct {
    kind: TokenKind,
    loc: ast.SourceLocation,

    // Payload for literals/identifiers (points into source text)
    text: []const u8 = "",

    /// Create a new token
    pub fn init(kind: TokenKind, loc: ast.SourceLocation, text: []const u8) Token {
        return .{
            .kind = kind,
            .loc = loc,
            .text = text,
        };
    }

    /// Check if token is a keyword
    pub fn isKeyword(self: Token) bool {
        return switch (self.kind) {
            .keyword_break,
            .keyword_case,
            .keyword_catch,
            .keyword_class,
            .keyword_const,
            .keyword_continue,
            .keyword_debugger,
            .keyword_default,
            .keyword_delete,
            .keyword_do,
            .keyword_else,
            .keyword_enum,
            .keyword_export,
            .keyword_extends,
            .keyword_false,
            .keyword_finally,
            .keyword_for,
            .keyword_function,
            .keyword_if,
            .keyword_import,
            .keyword_in,
            .keyword_instanceof,
            .keyword_interface,
            .keyword_let,
            .keyword_new,
            .keyword_null,
            .keyword_return,
            .keyword_super,
            .keyword_switch,
            .keyword_this,
            .keyword_throw,
            .keyword_true,
            .keyword_try,
            .keyword_typeof,
            .keyword_var,
            .keyword_void,
            .keyword_while,
            .keyword_with,
            .keyword_abstract,
            .keyword_as,
            .keyword_async,
            .keyword_await,
            .keyword_constructor,
            .keyword_declare,
            .keyword_from,
            .keyword_get,
            .keyword_implements,
            .keyword_is,
            .keyword_keyof,
            .keyword_namespace,
            .keyword_never,
            .keyword_of,
            .keyword_private,
            .keyword_protected,
            .keyword_public,
            .keyword_readonly,
            .keyword_require,
            .keyword_set,
            .keyword_static,
            .keyword_type,
            .keyword_unknown,
            .keyword_int8,
            .keyword_int16,
            .keyword_int32,
            .keyword_int64,
            .keyword_uint8,
            .keyword_uint16,
            .keyword_uint32,
            .keyword_uint64,
            .keyword_float32,
            .keyword_float64,
            .keyword_int,
            .keyword_float,
            .keyword_double,
            .keyword_defer,
            .keyword_distinct,
            .keyword_macro,
            .keyword_quote,
            .keyword_extern,
            => true,
            else => false,
        };
    }

    /// Check if token is a macro token (just @ sign now)
    pub fn isMacro(self: Token) bool {
        return self.kind == .at_sign;
    }
};

/// Keyword lookup table
pub const KeywordMap = std.StringHashMap(TokenKind);

/// Initialize keyword map
pub fn initKeywordMap(allocator: std.mem.Allocator) !KeywordMap {
    var map = KeywordMap.init(allocator);

    // JavaScript/TypeScript keywords
    try map.put("break", .keyword_break);
    try map.put("case", .keyword_case);
    try map.put("catch", .keyword_catch);
    try map.put("class", .keyword_class);
    try map.put("const", .keyword_const);
    try map.put("continue", .keyword_continue);
    try map.put("debugger", .keyword_debugger);
    try map.put("default", .keyword_default);
    try map.put("delete", .keyword_delete);
    try map.put("do", .keyword_do);
    try map.put("else", .keyword_else);
    try map.put("enum", .keyword_enum);
    try map.put("export", .keyword_export);
    try map.put("extends", .keyword_extends);
    try map.put("false", .keyword_false);
    try map.put("finally", .keyword_finally);
    try map.put("for", .keyword_for);
    try map.put("function", .keyword_function);
    try map.put("if", .keyword_if);
    try map.put("import", .keyword_import);
    try map.put("in", .keyword_in);
    try map.put("instanceof", .keyword_instanceof);
    try map.put("interface", .keyword_interface);
    try map.put("let", .keyword_let);
    try map.put("new", .keyword_new);
    try map.put("null", .keyword_null);
    try map.put("return", .keyword_return);
    try map.put("super", .keyword_super);
    try map.put("switch", .keyword_switch);
    try map.put("this", .keyword_this);
    try map.put("throw", .keyword_throw);
    try map.put("true", .keyword_true);
    try map.put("try", .keyword_try);
    try map.put("typeof", .keyword_typeof);
    try map.put("var", .keyword_var);
    try map.put("void", .keyword_void);
    try map.put("while", .keyword_while);
    try map.put("with", .keyword_with);

    // TypeScript-specific
    try map.put("abstract", .keyword_abstract);
    try map.put("as", .keyword_as);
    try map.put("async", .keyword_async);
    try map.put("await", .keyword_await);
    try map.put("constructor", .keyword_constructor);
    try map.put("declare", .keyword_declare);
    try map.put("from", .keyword_from);
    try map.put("get", .keyword_get);
    try map.put("implements", .keyword_implements);
    try map.put("is", .keyword_is);
    try map.put("keyof", .keyword_keyof);
    try map.put("namespace", .keyword_namespace);
    try map.put("never", .keyword_never);
    try map.put("of", .keyword_of);
    try map.put("private", .keyword_private);
    try map.put("protected", .keyword_protected);
    try map.put("public", .keyword_public);
    try map.put("readonly", .keyword_readonly);
    try map.put("require", .keyword_require);
    try map.put("set", .keyword_set);
    try map.put("static", .keyword_static);
    try map.put("type", .keyword_type);
    try map.put("unknown", .keyword_unknown);

    // Metascript sized types
    try map.put("int8", .keyword_int8);
    try map.put("int16", .keyword_int16);
    try map.put("int32", .keyword_int32);
    try map.put("int64", .keyword_int64);
    try map.put("uint8", .keyword_uint8);
    try map.put("uint16", .keyword_uint16);
    try map.put("uint32", .keyword_uint32);
    try map.put("uint64", .keyword_uint64);
    try map.put("float32", .keyword_float32);
    try map.put("float64", .keyword_float64);

    // Metascript type aliases
    try map.put("int", .keyword_int);
    try map.put("float", .keyword_float);
    try map.put("double", .keyword_double);

    // Metascript keywords
    try map.put("defer", .keyword_defer);
    try map.put("distinct", .keyword_distinct);

    // Metascript macro keywords (Nim-style)
    try map.put("macro", .keyword_macro);
    try map.put("quote", .keyword_quote);
    try map.put("extern", .keyword_extern);

    return map;
}

test "token kind toString" {
    try std.testing.expectEqualStrings("number", TokenKind.number.toString());
    try std.testing.expectEqualStrings("@", TokenKind.at_sign.toString());
    try std.testing.expectEqualStrings("EOF", TokenKind.end_of_file.toString());
}

test "token keyword detection" {
    const loc = ast.SourceLocation.dummy();

    const keyword_token = Token.init(.keyword_class, loc, "class");
    try std.testing.expect(keyword_token.isKeyword());

    const identifier_token = Token.init(.identifier, loc, "myVar");
    try std.testing.expect(!identifier_token.isKeyword());
}

test "token macro detection" {
    const loc = ast.SourceLocation.dummy();

    const macro_token = Token.init(.at_sign, loc, "@");
    try std.testing.expect(macro_token.isMacro());

    const normal_token = Token.init(.identifier, loc, "foo");
    try std.testing.expect(!normal_token.isMacro());
}

test "keyword map lookup" {
    var map = try initKeywordMap(std.testing.allocator);
    defer map.deinit();

    try std.testing.expect(map.get("class").? == .keyword_class);
    try std.testing.expect(map.get("function").? == .keyword_function);
    try std.testing.expect(map.get("async").? == .keyword_async);
    try std.testing.expect(map.get("notakeyword") == null);
}
