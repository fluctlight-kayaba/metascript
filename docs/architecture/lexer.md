# Lexer Architecture

**Purpose:** Single-pass streaming tokenizer for TypeScript syntax with macro support

**Performance Target:** <1ms for 1000 LOC, <16ms for syntax highlighting response

---

## Token Types

The lexer extends standard TypeScript tokens with macro-specific tokens:

```zig
pub const TokenKind = enum {
    // TypeScript standard tokens (~80 types)
    t_identifier,
    t_number,
    t_string,
    t_keyword_function,
    t_keyword_class,
    t_keyword_const,
    // ...

    // Macro-specific tokens
    t_at_sign,           // @
    t_at_derive,         // @derive
    t_at_comptime,       // @comptime
    t_at_serialize,      // @serialize
    t_at_ffi,            // @ffi

    // Special
    t_end_of_file,
    t_syntax_error,
};
```

---

## Token Structure

```zig
pub const Token = struct {
    kind: TokenKind,
    loc: SourceLocation,  // Byte offset + length

    // Payload (no heap allocation)
    data: union(TokenKind) {
        t_identifier: []const u8,
        t_number: f64,
        t_string: []const u8,
        else: void,
    },
};
```

---

## Lexer State Machine

```zig
pub const Lexer = struct {
    source: []const u8,
    current: usize,           // Current byte offset
    start: usize,             // Token start
    has_newline_before: bool,
    prev_error_loc: SourceLocation,
    errors: std.ArrayList(LexError),

    pub fn next(self: *Lexer) !Token {
        self.skipWhitespaceAndComments();
        self.start = self.current;

        if (self.isAtEnd()) return self.makeToken(.t_end_of_file);

        const c = self.advance();
        return switch (c) {
            '@' => self.scanMacroToken(),
            'a'...'z', 'A'...'Z', '_' => self.scanIdentifier(),
            '0'...'9' => self.scanNumber(),
            '"', '\'' => self.scanString(),
            // ... standard operators
            else => self.errorToken("Unexpected character"),
        };
    }

    fn scanMacroToken(self: *Lexer) Token {
        if (self.matchWord("derive")) return self.makeToken(.t_at_derive);
        if (self.matchWord("comptime")) return self.makeToken(.t_at_comptime);
        if (self.matchWord("serialize")) return self.makeToken(.t_at_serialize);
        if (self.matchWord("ffi")) return self.makeToken(.t_at_ffi);
        return self.makeToken(.t_at_sign);
    }
};
```

---

## Error Recovery

The lexer collects all errors in a single pass for LSP-friendly diagnostics:

```zig
pub const LexError = struct {
    message: []const u8,
    loc: SourceLocation,
    severity: enum { error, warning },
};
```

**Key Principles:**
- Continue lexing after errors (don't stop at first error)
- Deduplicate cascading errors at same location
- Track error locations for red squiggles in IDE

---

## Performance Patterns

| Pattern | Strategy |
|---------|----------|
| Single-pass | No backtracking, O(n) complexity |
| No heap allocation | Token data references source directly |
| Streaming | Tokens produced on-demand |
| Stack buffer | 8KB fast path before heap fallback |

---

## Testing Strategy

**Unit Tests (90% coverage):**
```zig
test "lexer tokenizes class keyword" {
    var l = Lexer.init("class User {}");
    const token = l.nextToken();
    try testing.expectEqual(.t_keyword_class, token.kind);
}

test "lexer handles @derive macro" {
    var l = Lexer.init("@derive(Eq)");
    try testing.expectEqual(.t_at_derive, l.nextToken().kind);
}

test "lexer reports error on invalid character" {
    var l = Lexer.init("class $ User");
    _ = l.nextToken();
    try testing.expect(l.hasError());
}
```

**Fuzz Tests:**
- Random input should never crash
- All input produces valid token stream (may include error tokens)

---

## File Location

```
src/lexer/
  lexer.zig      # Main lexer implementation
  token.zig      # Token types and structures
  source.zig     # Source location tracking
```

---

## References

- **Bun's lexer:** `~/projects/bun` (single-pass, streaming)
- **Parser design:** `../parser-design.md`
- **LSP integration:** `./lsp.md`
