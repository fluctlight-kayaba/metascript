/// Property/Fuzz Tests for Lexer Robustness
///
/// These tests verify that the lexer handles arbitrary input gracefully:
/// - Never crashes on random input
/// - Always terminates
/// - Produces valid output or error
///
/// Based on CLAUDE.md guidelines: "Random inputs, find edge cases"

const std = @import("std");
const testing = std.testing;

// Import compiler modules via build.zig configured imports
const src = @import("src");
const lexer_mod = src.lexer_mod;
const token_mod = src.token_mod;

const Lexer = lexer_mod.Lexer;
const TokenKind = token_mod.TokenKind;

// ============================================================================
// Deterministic Random Generator for Reproducible Tests
// ============================================================================

const TestRng = struct {
    state: u64,

    fn init(seed: u64) TestRng {
        return .{ .state = seed };
    }

    fn next(self: *TestRng) u64 {
        // Simple xorshift64
        self.state ^= self.state << 13;
        self.state ^= self.state >> 7;
        self.state ^= self.state << 17;
        return self.state;
    }

    fn nextInRange(self: *TestRng, min: usize, max: usize) usize {
        if (max <= min) return min;
        return min + @as(usize, @intCast(self.next() % @as(u64, @intCast(max - min))));
    }

    fn nextByte(self: *TestRng) u8 {
        return @truncate(self.next());
    }
};

// ============================================================================
// Fuzz Helpers
// ============================================================================

fn generateRandomAscii(rng: *TestRng, buffer: []u8) []u8 {
    const len = rng.nextInRange(0, buffer.len);
    for (0..len) |i| {
        // Generate printable ASCII + some whitespace
        const byte = rng.nextByte();
        buffer[i] = if (byte < 32) ' ' else if (byte > 126) 'z' else byte;
    }
    return buffer[0..len];
}

fn generateRandomBytes(rng: *TestRng, buffer: []u8) []u8 {
    const len = rng.nextInRange(0, buffer.len);
    for (0..len) |i| {
        buffer[i] = rng.nextByte();
    }
    return buffer[0..len];
}

fn generateValidishCode(rng: *TestRng, buffer: []u8) []u8 {
    const templates = [_][]const u8{
        "const x = 42;",
        "let y = \"hello\";",
        "function foo() { return 1; }",
        "class Bar { x: number; }",
        "@derive(Eq) class Point { x: number; y: number; }",
        "if (true) { } else { }",
        "while (x > 0) { x = x - 1; }",
        "import { a, b } from \"module\";",
        "export { foo };",
        "const arr = [1, 2, 3];",
        "const obj = { a: 1, b: 2 };",
        "// comment\n42",
        "/* block */ 42",
        "const f = (x) => x + 1;",
    };

    const choice = rng.nextInRange(0, templates.len);
    const template = templates[choice];

    const copy_len = @min(template.len, buffer.len);
    @memcpy(buffer[0..copy_len], template[0..copy_len]);
    return buffer[0..copy_len];
}

fn generateEdgeCaseString(rng: *TestRng, buffer: []u8) []u8 {
    const edge_cases = [_][]const u8{
        // Empty and whitespace
        "",
        " ",
        "\t",
        "\n",
        "\r\n",
        "   \t\n   ",
        // Unclosed strings
        "\"unclosed",
        "'unclosed",
        "`unclosed",
        // Unclosed comments
        "/* unclosed comment",
        "// comment without newline",
        // Nested structures
        "(((((",
        ")))))",
        "{{{{{",
        "}}}}}",
        "[[[[[",
        "]]]]]",
        // Many operators
        "++++++",
        "------",
        "======",
        "<<<<<<",
        ">>>>>>",
        // Mixed
        "@@@@@",
        "$$$$$",
        "#####",
        // Long identifiers
        "abcdefghijklmnopqrstuvwxyz",
        // Numbers
        "123456789012345678901234567890",
        "0x",
        "0b",
        "0.",
        ".0",
        "1e",
        "1e+",
        "1e-",
        // Escape sequences
        "\"\\n\\t\\r\"",
        "\"\\x00\"",
        "\"\\u0000\"",
        // Unicode (if supported)
        "const x = 42",
        // Keywords partially
        "clas",
        "functio",
        "retur",
        "expor",
        "impor",
    };

    const choice = rng.nextInRange(0, edge_cases.len);
    const edge_case = edge_cases[choice];

    const copy_len = @min(edge_case.len, buffer.len);
    @memcpy(buffer[0..copy_len], edge_case[0..copy_len]);
    return buffer[0..copy_len];
}

// ============================================================================
// Property: Lexer Never Crashes
// ============================================================================

test "property: lexer never crashes on random ASCII (1000 iterations)" {
    var buffer: [256]u8 = undefined;
    var rng = TestRng.init(0x12345678);

    for (0..1000) |_| {
        const input = generateRandomAscii(&rng, &buffer);

        // Should not crash - may produce errors or illegal tokens
        var lexer = Lexer.init(testing.allocator, input, 1) catch continue;
        defer lexer.deinit();

        // Consume all tokens
        while (true) {
            const token = lexer.next() catch break;
            if (token.kind == .end_of_file) break;
        }
    }
}

test "property: lexer never crashes on random bytes (1000 iterations)" {
    var buffer: [256]u8 = undefined;
    var rng = TestRng.init(0xDEADBEEF);

    for (0..1000) |_| {
        const input = generateRandomBytes(&rng, &buffer);

        // Should not crash even on invalid UTF-8
        var lexer = Lexer.init(testing.allocator, input, 1) catch continue;
        defer lexer.deinit();

        // Consume all tokens
        while (true) {
            const token = lexer.next() catch break;
            if (token.kind == .end_of_file) break;
        }
    }
}

test "property: lexer never crashes on edge cases (all edge cases)" {
    var buffer: [256]u8 = undefined;
    var rng = TestRng.init(0xCAFEBABE);

    for (0..100) |_| {
        const input = generateEdgeCaseString(&rng, &buffer);

        // Should not crash on edge cases
        var lexer = Lexer.init(testing.allocator, input, 1) catch continue;
        defer lexer.deinit();

        // Consume all tokens
        while (true) {
            const token = lexer.next() catch break;
            if (token.kind == .end_of_file) break;
        }
    }
}

// ============================================================================
// Property: Lexer Always Terminates
// ============================================================================

test "property: lexer terminates on valid-ish code (500 iterations)" {
    var buffer: [512]u8 = undefined;
    var rng = TestRng.init(0xBEEFCAFE);

    for (0..500) |_| {
        const input = generateValidishCode(&rng, &buffer);

        var lexer = Lexer.init(testing.allocator, input, 1) catch continue;
        defer lexer.deinit();

        var token_count: usize = 0;
        const max_tokens = 10000; // Safety limit

        while (token_count < max_tokens) : (token_count += 1) {
            const token = lexer.next() catch break;
            if (token.kind == .end_of_file) break;
        }

        // Should terminate well before max_tokens for small inputs
        try testing.expect(token_count < max_tokens);
    }
}

// ============================================================================
// Property: Empty Input Produces EOF
// ============================================================================

test "property: empty input always produces single EOF" {
    var lexer = try Lexer.init(testing.allocator, "", 1);
    defer lexer.deinit();

    const token = try lexer.next();
    try testing.expectEqual(TokenKind.end_of_file, token.kind);
}

// ============================================================================
// Property: Token Positions Are Valid
// ============================================================================

test "property: token positions are valid (1000 iterations)" {
    var buffer: [256]u8 = undefined;
    var rng = TestRng.init(0xABCDEF01);

    for (0..1000) |_| {
        const input = generateValidishCode(&rng, &buffer);

        var lexer = Lexer.init(testing.allocator, input, 1) catch continue;
        defer lexer.deinit();

        var prev_line: u32 = 0;
        var prev_col: u32 = 0;

        while (true) {
            const token = lexer.next() catch break;
            if (token.kind == .end_of_file) break;

            // Token positions should be valid (non-negative is guaranteed by u32)
            // Line should be >= previous token's line (tokens go forward)
            try testing.expect(token.loc.start.line >= prev_line or
                (token.loc.start.line == prev_line and token.loc.start.column >= prev_col));

            prev_line = token.loc.start.line;
            prev_col = token.loc.start.column;
        }
    }
}

// ============================================================================
// Property: Known Keywords Are Recognized
// ============================================================================

test "property: all keywords are recognized correctly" {
    const keywords = [_]struct { text: []const u8, kind: TokenKind }{
        .{ .text = "class", .kind = .keyword_class },
        .{ .text = "function", .kind = .keyword_function },
        .{ .text = "const", .kind = .keyword_const },
        .{ .text = "let", .kind = .keyword_let },
        .{ .text = "if", .kind = .keyword_if },
        .{ .text = "else", .kind = .keyword_else },
        .{ .text = "return", .kind = .keyword_return },
        .{ .text = "import", .kind = .keyword_import },
        .{ .text = "export", .kind = .keyword_export },
        .{ .text = "while", .kind = .keyword_while },
        .{ .text = "for", .kind = .keyword_for },
        .{ .text = "true", .kind = .keyword_true },
        .{ .text = "false", .kind = .keyword_false },
        .{ .text = "null", .kind = .keyword_null },
    };

    for (keywords) |kw| {
        var lexer = try Lexer.init(testing.allocator, kw.text, 1);
        defer lexer.deinit();

        const token = try lexer.next();
        try testing.expectEqual(kw.kind, token.kind);
    }
}

// ============================================================================
// Property: Operators Are Recognized
// ============================================================================

test "property: all operators are recognized correctly" {
    const operators = [_]struct { text: []const u8, kind: TokenKind }{
        .{ .text = "+", .kind = .plus },
        .{ .text = "-", .kind = .minus },
        .{ .text = "*", .kind = .star },
        .{ .text = "/", .kind = .slash },
        .{ .text = "=", .kind = .equals },
        .{ .text = "==", .kind = .equals_equals },
        .{ .text = "===", .kind = .equals_equals_equals },
        .{ .text = "!=", .kind = .bang_equals },
        .{ .text = "<", .kind = .less_than },
        .{ .text = ">", .kind = .greater_than },
        .{ .text = "<=", .kind = .less_equals },
        .{ .text = ">=", .kind = .greater_equals },
        .{ .text = "=>", .kind = .arrow },
        .{ .text = "&&", .kind = .ampersand_ampersand },
        .{ .text = "||", .kind = .pipe_pipe },
    };

    for (operators) |op| {
        var lexer = try Lexer.init(testing.allocator, op.text, 1);
        defer lexer.deinit();

        const token = try lexer.next();
        try testing.expectEqual(op.kind, token.kind);
    }
}

// ============================================================================
// Property: Idempotent Re-lexing
// ============================================================================

test "property: lexing same input twice produces same tokens" {
    const inputs = [_][]const u8{
        "const x = 42;",
        "function foo() {}",
        "@derive(Eq) class Point { x: number; }",
        "if (true) { return 1; } else { return 0; }",
    };

    for (inputs) |input| {
        // First pass
        var tokens1 = std.ArrayList(TokenKind).init(testing.allocator);
        defer tokens1.deinit();

        {
            var lexer = try Lexer.init(testing.allocator, input, 1);
            defer lexer.deinit();

            while (true) {
                const token = try lexer.next();
                try tokens1.append(token.kind);
                if (token.kind == .end_of_file) break;
            }
        }

        // Second pass
        var tokens2 = std.ArrayList(TokenKind).init(testing.allocator);
        defer tokens2.deinit();

        {
            var lexer = try Lexer.init(testing.allocator, input, 1);
            defer lexer.deinit();

            while (true) {
                const token = try lexer.next();
                try tokens2.append(token.kind);
                if (token.kind == .end_of_file) break;
            }
        }

        // Compare
        try testing.expectEqual(tokens1.items.len, tokens2.items.len);
        for (tokens1.items, tokens2.items) |t1, t2| {
            try testing.expectEqual(t1, t2);
        }
    }
}

// ============================================================================
// Performance Property: Linear Time Complexity
// ============================================================================

test "property: lexing time scales linearly with input size" {
    const allocator = testing.allocator;

    // Generate inputs of increasing size
    const sizes = [_]usize{ 100, 500, 1000, 2000 };
    var times: [sizes.len]i64 = undefined;

    for (sizes, 0..) |size, i| {
        var source = try allocator.alloc(u8, size);
        defer allocator.free(source);

        // Fill with valid-ish code pattern
        var j: usize = 0;
        while (j < size) {
            const pattern = "const x = 42; ";
            const copy_len = @min(pattern.len, size - j);
            @memcpy(source[j .. j + copy_len], pattern[0..copy_len]);
            j += copy_len;
        }

        const start = std.time.milliTimestamp();

        // Lex 10 times for more accurate measurement
        for (0..10) |_| {
            var lexer = try Lexer.init(allocator, source, 1);
            defer lexer.deinit();

            while (true) {
                const token = try lexer.next();
                if (token.kind == .end_of_file) break;
            }
        }

        times[i] = std.time.milliTimestamp() - start;
    }

    // Check that time roughly scales with size (allow 4x for 2x input)
    // This is a weak check but catches O(n^2) or worse behavior
    if (times[3] > 0 and times[0] > 0) {
        const ratio = @as(f64, @floatFromInt(times[3])) / @as(f64, @floatFromInt(@max(times[0], 1)));
        const size_ratio = @as(f64, @floatFromInt(sizes[3])) / @as(f64, @floatFromInt(sizes[0]));

        // Allow up to 4x the linear expectation for overhead
        try testing.expect(ratio < size_ratio * 4.0);
    }
}

// ============================================================================
// Memory Property: No Leaks
// ============================================================================

test "property: no memory leaks on repeated lexing" {
    // The testing.allocator automatically checks for leaks
    // If this test completes without error, no leaks occurred

    for (0..100) |_| {
        var lexer = try Lexer.init(testing.allocator, "const x = 42; function foo() { return x; }", 1);
        defer lexer.deinit();

        while (true) {
            const token = try lexer.next();
            if (token.kind == .end_of_file) break;
        }
    }
}
