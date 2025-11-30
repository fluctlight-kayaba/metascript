// LSP Server Implementation
// Handles initialize, shutdown, and document synchronization with proper protocol compliance

const std = @import("std");
const jsonrpc = @import("jsonrpc.zig");
const file_store = @import("file_store.zig");

// Lexer for diagnostics
const lexer_mod = @import("../lexer/lexer.zig");
const token_mod = @import("../lexer/token.zig");
const ast_loc = @import("../ast/location.zig");

// ============================================================================
// LSP Types - Structured JSON serialization
// ============================================================================

/// Server capabilities as defined by LSP spec
pub const ServerCapabilities = struct {
    /// How documents are synced: 0=None, 1=Full, 2=Incremental
    textDocumentSync: ?TextDocumentSyncOptions = null,
    /// Whether server provides hover support
    hoverProvider: bool = false,
    /// Whether server provides completion support
    completionProvider: ?CompletionOptions = null,
    /// Whether server provides definition support
    definitionProvider: bool = false,
    /// Whether server provides references support
    referencesProvider: bool = false,
    /// Whether server provides document symbol support
    documentSymbolProvider: bool = false,
    /// Semantic tokens provider
    semanticTokensProvider: ?SemanticTokensOptions = null,
};

/// Semantic tokens configuration
pub const SemanticTokensOptions = struct {
    legend: SemanticTokensLegend,
    full: bool = true,
    range: bool = false,
};

/// Semantic tokens legend - defines token types and modifiers
pub const SemanticTokensLegend = struct {
    tokenTypes: []const []const u8,
    tokenModifiers: []const []const u8,
};

pub const TextDocumentSyncOptions = struct {
    /// Open and close notifications are sent to server
    openClose: bool = true,
    /// Change notifications: 0=None, 1=Full, 2=Incremental
    change: u8 = 2, // Incremental by default
    /// Save notifications
    save: ?SaveOptions = null,
};

pub const SaveOptions = struct {
    includeText: bool = false,
};

pub const CompletionOptions = struct {
    triggerCharacters: ?[]const []const u8 = null,
    resolveProvider: bool = false,
};

/// Server information
pub const ServerInfo = struct {
    name: []const u8,
    version: ?[]const u8 = null,
};

/// Initialize result
pub const InitializeResult = struct {
    capabilities: ServerCapabilities,
    serverInfo: ?ServerInfo = null,
};

/// JSON-RPC response structure
pub const Response = struct {
    jsonrpc: []const u8 = "2.0",
    id: std.json.Value,
    result: ?std.json.Value = null,
    @"error": ?ResponseError = null,

    pub fn err(id: std.json.Value, code: i32, message: []const u8) Response {
        return Response{
            .id = id,
            .@"error" = .{
                .code = code,
                .message = message,
            },
        };
    }

    pub fn nullResult(id: std.json.Value) Response {
        return Response{
            .id = id,
            .result = .null,
        };
    }
};

pub const ResponseError = struct {
    code: i32,
    message: []const u8,
    data: ?std.json.Value = null,
};

// Re-export types from file_store for convenience
pub const Position = file_store.Position;
pub const Range = file_store.Range;

// ============================================================================
// LSP Diagnostic Types
// ============================================================================

/// Diagnostic severity levels
pub const DiagnosticSeverity = enum(u8) {
    @"error" = 1,
    warning = 2,
    information = 3,
    hint = 4,
};

/// LSP Diagnostic
pub const Diagnostic = struct {
    range: Range,
    severity: ?DiagnosticSeverity = null,
    code: ?[]const u8 = null,
    source: ?[]const u8 = null,
    message: []const u8,
};

/// PublishDiagnostics params
pub const PublishDiagnosticsParams = struct {
    uri: []const u8,
    diagnostics: []const Diagnostic,
};

// ============================================================================
// Semantic Tokens
// ============================================================================

/// Standard LSP semantic token types (order matters - index into legend)
pub const SemanticTokenType = enum(u8) {
    namespace = 0,
    type = 1,
    class = 2,
    @"enum" = 3,
    interface = 4,
    @"struct" = 5,
    typeParameter = 6,
    parameter = 7,
    variable = 8,
    property = 9,
    enumMember = 10,
    event = 11,
    function = 12,
    method = 13,
    macro = 14,
    keyword = 15,
    modifier = 16,
    comment = 17,
    string = 18,
    number = 19,
    regexp = 20,
    operator = 21,
};

/// Token type names for the legend (must match SemanticTokenType order)
pub const SEMANTIC_TOKEN_TYPES: []const []const u8 = &.{
    "namespace",
    "type",
    "class",
    "enum",
    "interface",
    "struct",
    "typeParameter",
    "parameter",
    "variable",
    "property",
    "enumMember",
    "event",
    "function",
    "method",
    "macro",
    "keyword",
    "modifier",
    "comment",
    "string",
    "number",
    "regexp",
    "operator",
};

/// Token modifier names for the legend
pub const SEMANTIC_TOKEN_MODIFIERS: []const []const u8 = &.{
    "declaration",
    "definition",
    "readonly",
    "static",
    "deprecated",
    "abstract",
    "async",
    "modification",
    "documentation",
    "defaultLibrary",
};

/// Map a lexer token kind to a semantic token type
fn tokenKindToSemanticType(kind: token_mod.TokenKind) ?SemanticTokenType {
    return switch (kind) {
        // Keywords
        .keyword_break,
        .keyword_case,
        .keyword_catch,
        .keyword_continue,
        .keyword_debugger,
        .keyword_default,
        .keyword_delete,
        .keyword_do,
        .keyword_else,
        .keyword_finally,
        .keyword_for,
        .keyword_if,
        .keyword_in,
        .keyword_instanceof,
        .keyword_new,
        .keyword_return,
        .keyword_switch,
        .keyword_throw,
        .keyword_try,
        .keyword_typeof,
        .keyword_void,
        .keyword_while,
        .keyword_with,
        .keyword_as,
        .keyword_from,
        .keyword_of,
        .keyword_is,
        .keyword_keyof,
        => .keyword,

        // Type-related keywords
        .keyword_class => .class,
        .keyword_interface => .interface,
        .keyword_enum => .@"enum",
        .keyword_type => .type,
        .keyword_namespace => .namespace,

        // Function-related
        .keyword_function => .function,
        .keyword_async,
        .keyword_await,
        => .keyword,

        // Variable declarations
        .keyword_const,
        .keyword_let,
        .keyword_var,
        => .keyword,

        // Modifiers
        .keyword_abstract,
        .keyword_declare,
        .keyword_export,
        .keyword_extends,
        .keyword_implements,
        .keyword_import,
        .keyword_private,
        .keyword_protected,
        .keyword_public,
        .keyword_readonly,
        .keyword_static,
        .keyword_constructor,
        .keyword_get,
        .keyword_set,
        .keyword_require,
        => .modifier,

        // Literals
        .keyword_true,
        .keyword_false,
        .keyword_null,
        .keyword_this,
        .keyword_super,
        .keyword_never,
        .keyword_unknown,
        => .keyword,

        // Numbers and strings
        .number => .number,
        .string, .template_string => .string,
        .regex => .regexp,

        // Macros
        .at_sign,
        .at_derive,
        .at_comptime,
        .at_serialize,
        .at_ffi,
        => .macro,

        // Operators
        .plus,
        .minus,
        .star,
        .slash,
        .percent,
        .star_star,
        .equals,
        .plus_equals,
        .minus_equals,
        .star_equals,
        .slash_equals,
        .percent_equals,
        .equals_equals,
        .equals_equals_equals,
        .bang_equals,
        .bang_equals_equals,
        .less_than,
        .less_equals,
        .greater_than,
        .greater_equals,
        .ampersand_ampersand,
        .pipe_pipe,
        .bang,
        .ampersand,
        .pipe,
        .caret,
        .tilde,
        .less_less,
        .greater_greater,
        .greater_greater_greater,
        .plus_plus,
        .minus_minus,
        .question,
        .dot,
        .dot_dot_dot,
        .arrow,
        => .operator,

        // Identifiers are handled separately (could be variable, function, etc.)
        .identifier => .variable,

        // Skip punctuation and special tokens
        .left_paren,
        .right_paren,
        .left_brace,
        .right_brace,
        .left_bracket,
        .right_bracket,
        .semicolon,
        .colon,
        .comma,
        .newline,
        .end_of_file,
        .syntax_error,
        => null,
    };
}

// Document store now provided by file_store.FileStore

/// Get hover content for a token
fn getHoverContent(kind: token_mod.TokenKind, text: []const u8) ?[]const u8 {
    _ = text;
    return switch (kind) {
        // Macros - provide detailed descriptions
        .at_derive => "**@derive** - Compile-time macro\n\nAutomatically generates trait implementations.\n\n```typescript\n@derive(Eq, Hash, Serialize)\nclass User { name: string; }\n```\n\nGenerates `equals()`, `hashCode()`, and serialization methods.",

        .at_comptime => "**@comptime** - Compile-time execution\n\nExecutes code at compile time for metaprogramming.\n\n```typescript\nconst config = @comptime loadConfig();\n```",

        .at_serialize => "**@serialize** - Serialization macro\n\nGenerates JSON/binary serialization code.\n\n```typescript\n@serialize\nclass Message { data: string; }\n```",

        .at_ffi => "**@ffi** - Foreign Function Interface\n\nBinds to native C libraries.\n\n```typescript\nconst libc = @ffi bindC('./libc.h');\n```",

        .at_sign => "**@** - Macro prefix\n\nUsed to invoke compile-time macros.",

        // Keywords with descriptions
        .keyword_class => "**class** - Class declaration\n\nDefines a class with properties and methods.",
        .keyword_interface => "**interface** - Interface declaration\n\nDefines a contract for class implementations.",
        .keyword_enum => "**enum** - Enumeration\n\nDefines a set of named constants.",
        .keyword_type => "**type** - Type alias\n\nCreates a new name for an existing type.",
        .keyword_function => "**function** - Function declaration\n\nDefines a named function.",
        .keyword_const => "**const** - Constant declaration\n\nDeclares an immutable binding.",
        .keyword_let => "**let** - Variable declaration\n\nDeclares a mutable binding.",
        .keyword_var => "**var** - Variable declaration (legacy)\n\nDeclares a function-scoped variable.",
        .keyword_async => "**async** - Async function\n\nMarks a function as asynchronous.",
        .keyword_await => "**await** - Await expression\n\nWaits for a Promise to resolve.",
        .keyword_import => "**import** - Module import\n\nImports exports from another module.",
        .keyword_export => "**export** - Module export\n\nExports declarations for use in other modules.",
        .keyword_return => "**return** - Return statement\n\nReturns a value from a function.",
        .keyword_if => "**if** - Conditional\n\nExecutes code based on a condition.",
        .keyword_else => "**else** - Else branch\n\nAlternative branch for if statement.",
        .keyword_for => "**for** - For loop\n\nIterates over a sequence.",
        .keyword_while => "**while** - While loop\n\nRepeats while condition is true.",
        .keyword_break => "**break** - Break statement\n\nExits the current loop.",
        .keyword_continue => "**continue** - Continue statement\n\nSkips to next iteration.",
        .keyword_switch => "**switch** - Switch statement\n\nMulti-way branch based on value.",
        .keyword_case => "**case** - Case clause\n\nDefines a branch in switch statement.",
        .keyword_default => "**default** - Default clause\n\nFallback branch in switch statement.",
        .keyword_try => "**try** - Try block\n\nWraps code that may throw.",
        .keyword_catch => "**catch** - Catch clause\n\nHandles exceptions from try block.",
        .keyword_finally => "**finally** - Finally clause\n\nAlways executes after try/catch.",
        .keyword_throw => "**throw** - Throw statement\n\nThrows an exception.",
        .keyword_new => "**new** - Constructor call\n\nCreates a new instance.",
        .keyword_this => "**this** - This reference\n\nRefers to current instance.",
        .keyword_super => "**super** - Super reference\n\nRefers to parent class.",
        .keyword_extends => "**extends** - Class inheritance\n\nInherits from a base class.",
        .keyword_implements => "**implements** - Interface implementation\n\nImplements an interface.",
        .keyword_public => "**public** - Public modifier\n\nAccessible from anywhere.",
        .keyword_private => "**private** - Private modifier\n\nOnly accessible within the class.",
        .keyword_protected => "**protected** - Protected modifier\n\nAccessible in class and subclasses.",
        .keyword_static => "**static** - Static modifier\n\nBelongs to class, not instance.",
        .keyword_readonly => "**readonly** - Readonly modifier\n\nCannot be reassigned after initialization.",
        .keyword_abstract => "**abstract** - Abstract modifier\n\nMust be implemented by subclasses.",
        .keyword_true => "**true** - Boolean literal\n\nThe boolean value true.",
        .keyword_false => "**false** - Boolean literal\n\nThe boolean value false.",
        .keyword_null => "**null** - Null literal\n\nRepresents absence of value.",
        .keyword_typeof => "**typeof** - Type operator\n\nReturns the type of an expression.",
        .keyword_instanceof => "**instanceof** - Instance check\n\nChecks if object is instance of class.",
        .keyword_keyof => "**keyof** - Key type operator\n\nGets union of property keys.",
        .keyword_namespace => "**namespace** - Namespace declaration\n\nGroups related declarations.",
        .keyword_never => "**never** - Never type\n\nType that never occurs.",
        .keyword_unknown => "**unknown** - Unknown type\n\nType-safe alternative to any.",

        // Types
        .number => "**number** - Numeric literal",
        .string => "**string** - String literal",
        .template_string => "**template string** - Template literal\n\nSupports interpolation with `${expr}`.",
        .regex => "**regex** - Regular expression literal",

        // Identifiers don't have hover info without type analysis
        .identifier => null,

        // No hover for operators and punctuation
        else => null,
    };
}

// ============================================================================
// Server
// ============================================================================

pub const Server = struct {
    allocator: std.mem.Allocator,
    initialized: bool = false,
    shutdown_requested: bool = false,
    documents: file_store.FileStore,

    pub fn init(allocator: std.mem.Allocator) Server {
        return .{
            .allocator = allocator,
            .documents = file_store.FileStore.init(allocator),
        };
    }

    pub fn deinit(self: *Server) void {
        self.documents.deinit();
    }

    /// Main server loop - read messages from stdin, process, write to stdout
    pub fn run(self: *Server) !void {
        const stdin = std.io.getStdIn().reader();
        const stdout = std.io.getStdOut().writer();
        const stderr = std.io.getStdErr().writer();

        try stderr.writeAll("[mls] Metascript Language Server starting...\n");

        while (true) {
            const content = jsonrpc.readMessage(self.allocator, stdin) catch |err| {
                switch (err) {
                    jsonrpc.ReadError.MissingContentLength => {
                        try stderr.writeAll("[mls] Error: Missing Content-Length header\n");
                        continue;
                    },
                    jsonrpc.ReadError.InvalidContentLength => {
                        try stderr.writeAll("[mls] Error: Invalid Content-Length\n");
                        continue;
                    },
                    jsonrpc.ReadError.HeaderTooLong => {
                        try stderr.writeAll("[mls] Error: Header too long\n");
                        continue;
                    },
                    else => {
                        try stderr.writeAll("[mls] Client disconnected\n");
                        break;
                    },
                }
            };

            if (content == null) break;
            defer self.allocator.free(content.?);

            try stderr.print("[mls] Received: {s}\n", .{content.?});

            // Parse JSON
            const parsed = std.json.parseFromSlice(
                std.json.Value,
                self.allocator,
                content.?,
                .{},
            ) catch |err| {
                try stderr.print("[mls] JSON parse error: {}\n", .{err});
                // Send parse error response
                try self.sendParseError(stdout);
                continue;
            };
            defer parsed.deinit();

            // Process message
            try self.handleMessage(parsed.value, stdout, stderr);
        }
    }

    fn handleMessage(
        self: *Server,
        value: std.json.Value,
        stdout: anytype,
        stderr: anytype,
    ) !void {
        const obj = value.object;
        const method = if (obj.get("method")) |m| m.string else null;
        const id = obj.get("id");

        if (method) |m| {
            try stderr.print("[mls] Method: {s}\n", .{m});

            // Check if server is initialized
            // Allow: initialize request, initialized notification, exit notification
            const allowed_before_init = std.mem.eql(u8, m, "initialize") or
                std.mem.eql(u8, m, "initialized") or
                std.mem.eql(u8, m, "exit");

            if (!self.initialized and !allowed_before_init) {
                if (id != null) {
                    try self.sendErrorResponse(
                        id.?,
                        jsonrpc.ErrorCode.ServerNotInitialized,
                        "Server not initialized",
                        stdout,
                    );
                }
                return;
            }

            if (id != null) {
                try self.handleRequest(m, id.?, obj, stdout, stderr);
            } else {
                try self.handleNotification(m, obj, stdout, stderr);
            }
        }
    }

    fn handleRequest(
        self: *Server,
        method: []const u8,
        id: std.json.Value,
        msg: std.json.ObjectMap,
        stdout: anytype,
        stderr: anytype,
    ) !void {
        if (std.mem.eql(u8, method, "initialize")) {
            try self.handleInitialize(id, stdout, stderr);
        } else if (std.mem.eql(u8, method, "shutdown")) {
            self.shutdown_requested = true;
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
        } else if (std.mem.eql(u8, method, "textDocument/hover")) {
            try self.handleHover(id, msg, stdout, stderr);
        } else if (std.mem.eql(u8, method, "textDocument/completion")) {
            try self.handleCompletion(id, msg, stdout, stderr);
        } else if (std.mem.eql(u8, method, "textDocument/definition")) {
            try self.handleDefinition(id, msg, stdout, stderr);
        } else if (std.mem.eql(u8, method, "textDocument/documentSymbol")) {
            try self.handleDocumentSymbol(id, msg, stdout, stderr);
        } else if (std.mem.eql(u8, method, "textDocument/semanticTokens/full")) {
            try self.handleSemanticTokens(id, msg, stdout, stderr);
        } else {
            try stderr.print("[mls] Unknown method: {s}\n", .{method});
            try self.sendErrorResponse(
                id,
                jsonrpc.ErrorCode.MethodNotFound,
                "Method not found",
                stdout,
            );
        }
    }

    fn handleNotification(
        self: *Server,
        method: []const u8,
        msg: std.json.ObjectMap,
        stdout: anytype,
        stderr: anytype,
    ) !void {
        if (std.mem.eql(u8, method, "initialized")) {
            self.initialized = true;
            try stderr.writeAll("[mls] Client initialized\n");
        } else if (std.mem.eql(u8, method, "exit")) {
            try stderr.writeAll("[mls] Exit notification received\n");
            const exit_code: u8 = if (self.shutdown_requested) 0 else 1;
            std.process.exit(exit_code);
        } else if (std.mem.eql(u8, method, "textDocument/didOpen")) {
            try self.handleDidOpen(msg, stdout, stderr);
        } else if (std.mem.eql(u8, method, "textDocument/didChange")) {
            try self.handleDidChange(msg, stdout, stderr);
        } else if (std.mem.eql(u8, method, "textDocument/didClose")) {
            try self.handleDidClose(msg, stderr);
        } else if (std.mem.eql(u8, method, "textDocument/didSave")) {
            try stderr.writeAll("[mls] textDocument/didSave\n");
        }
    }

    fn handleInitialize(
        self: *Server,
        id: std.json.Value,
        stdout: anytype,
        stderr: anytype,
    ) !void {
        try stderr.writeAll("[mls] Handling initialize request\n");

        const result = InitializeResult{
            .capabilities = .{
                .textDocumentSync = .{
                    .openClose = true,
                    .change = 2, // Incremental sync
                    .save = .{ .includeText = false },
                },
                .hoverProvider = true,
                .completionProvider = .{
                    .triggerCharacters = &.{ ".", "@" },
                    .resolveProvider = false,
                },
                .definitionProvider = true,
                .referencesProvider = false,
                .documentSymbolProvider = true,
                .semanticTokensProvider = .{
                    .legend = .{
                        .tokenTypes = SEMANTIC_TOKEN_TYPES,
                        .tokenModifiers = SEMANTIC_TOKEN_MODIFIERS,
                    },
                    .full = true,
                    .range = false,
                },
            },
            .serverInfo = .{
                .name = "mls",
                .version = "0.1.0",
            },
        };

        // Serialize using Zig's JSON serializer
        var response_buf = std.ArrayList(u8).init(self.allocator);
        defer response_buf.deinit();

        try response_buf.writer().writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try std.json.stringify(id, .{}, response_buf.writer());
        try response_buf.writer().writeAll(",\"result\":");
        try std.json.stringify(result, .{ .emit_null_optional_fields = false }, response_buf.writer());
        try response_buf.writer().writeAll("}");

        try jsonrpc.writeMessage(stdout, response_buf.items);
        try stderr.print("[mls] Sent: {s}\n", .{response_buf.items});
    }

    fn handleDidOpen(
        self: *Server,
        msg: std.json.ObjectMap,
        stdout: anytype,
        stderr: anytype,
    ) !void {
        const params = msg.get("params") orelse return;
        const text_document = params.object.get("textDocument") orelse return;
        const uri = text_document.object.get("uri") orelse return;
        const text = text_document.object.get("text") orelse return;
        const version = text_document.object.get("version") orelse return;

        _ = try self.documents.set(
            uri.string,
            text.string,
            @intCast(version.integer),
        );

        try stderr.print("[mls] Opened: {s} (version {d}, {d} bytes)\n", .{
            uri.string,
            version.integer,
            text.string.len,
        });

        // Run diagnostics immediately
        try self.runDiagnostics(uri.string, stdout, stderr);
    }

    fn handleDidChange(
        self: *Server,
        msg: std.json.ObjectMap,
        stdout: anytype,
        stderr: anytype,
    ) !void {
        const params = msg.get("params") orelse return;
        const text_document = params.object.get("textDocument") orelse return;
        const uri = text_document.object.get("uri") orelse return;
        const changes = params.object.get("contentChanges") orelse return;

        const entry = self.documents.getEntry(uri.string) orelse {
            try stderr.print("[mls] Warning: didChange for unknown document: {s}\n", .{uri.string});
            return;
        };

        for (changes.array.items) |change_value| {
            const change_obj = change_value.object;
            const text = change_obj.get("text") orelse continue;

            // Check if this is an incremental change (has range)
            var range: ?file_store.Range = null;
            if (change_obj.get("range")) |range_value| {
                const range_obj = range_value.object;
                const start = range_obj.get("start").?.object;
                const end = range_obj.get("end").?.object;

                range = .{
                    .start = .{
                        .line = @intCast(start.get("line").?.integer),
                        .character = @intCast(start.get("character").?.integer),
                    },
                    .end = .{
                        .line = @intCast(end.get("line").?.integer),
                        .character = @intCast(end.get("character").?.integer),
                    },
                };
            }

            entry.applyChange(range, text.string) catch |err| {
                try stderr.print("[mls] Error applying change: {}\n", .{err});
            };
        }

        const content_len = if (entry.text) |t| t.len else 0;
        try stderr.print("[mls] Changed: {s} (now {d} bytes)\n", .{
            uri.string,
            content_len,
        });

        // Run diagnostics after changes
        try self.runDiagnostics(uri.string, stdout, stderr);
    }

    fn handleDidClose(
        self: *Server,
        msg: std.json.ObjectMap,
        stderr: anytype,
    ) !void {
        const params = msg.get("params") orelse return;
        const text_document = params.object.get("textDocument") orelse return;
        const uri = text_document.object.get("uri") orelse return;

        self.documents.close(uri.string);
        try stderr.print("[mls] Closed: {s}\n", .{uri.string});
    }

    // =========================================================================
    // Hover
    // =========================================================================

    fn handleHover(
        self: *Server,
        id: std.json.Value,
        msg: std.json.ObjectMap,
        stdout: anytype,
        stderr: anytype,
    ) !void {
        const params = msg.get("params") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const text_document = params.object.get("textDocument") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const uri = text_document.object.get("uri") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const position = params.object.get("position") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };

        const line: u32 = @intCast(position.object.get("line").?.integer);
        const character: u32 = @intCast(position.object.get("character").?.integer);

        const entry = self.documents.getEntry(uri.string) orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const text = entry.text orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };

        // Run lexer and find token at position
        var lexer = lexer_mod.Lexer.init(self.allocator, text, 1) catch {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        defer lexer.deinit();

        var found_token: ?token_mod.Token = null;

        while (true) {
            const tok = lexer.next() catch break;
            if (tok.kind == .end_of_file) break;

            // Check if position is within this token
            // Token lines are 1-indexed, LSP is 0-indexed
            const tok_line = if (tok.loc.start.line > 0) tok.loc.start.line - 1 else 0;
            const tok_start_col = tok.loc.start.column;
            const tok_end_col = tok.loc.end.column;

            if (tok_line == line and character >= tok_start_col and character < tok_end_col) {
                found_token = tok;
                break;
            }
        }

        if (found_token) |tok| {
            const hover_content = getHoverContent(tok.kind, tok.text);
            if (hover_content) |content| {
                try self.sendHoverResponse(id, content, tok, stdout, stderr);
                return;
            }
        }

        // No hover info available
        try self.sendResponse(Response.nullResult(id), stdout, stderr);
    }

    fn sendHoverResponse(
        self: *Server,
        id: std.json.Value,
        content: []const u8,
        tok: token_mod.Token,
        stdout: anytype,
        stderr: anytype,
    ) !void {
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        const writer = buf.writer();
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try std.json.stringify(id, .{}, writer);

        // Hover result with markdown content and range
        try writer.writeAll(",\"result\":{\"contents\":{\"kind\":\"markdown\",\"value\":");
        try std.json.stringify(content, .{}, writer);
        try writer.writeAll("},\"range\":{\"start\":{\"line\":");

        const start_line = if (tok.loc.start.line > 0) tok.loc.start.line - 1 else 0;
        const end_line = if (tok.loc.end.line > 0) tok.loc.end.line - 1 else 0;

        try writer.print("{d}", .{start_line});
        try writer.writeAll(",\"character\":");
        try writer.print("{d}", .{tok.loc.start.column});
        try writer.writeAll("},\"end\":{\"line\":");
        try writer.print("{d}", .{end_line});
        try writer.writeAll(",\"character\":");
        try writer.print("{d}", .{tok.loc.end.column});
        try writer.writeAll("}}}}");

        try jsonrpc.writeMessage(stdout, buf.items);
        try stderr.print("[mls] Hover: {s}\n", .{tok.text});
    }

    // =========================================================================
    // Document Symbols (Outline)
    // =========================================================================

    /// Symbol kinds as defined by LSP
    const SymbolKind = enum(u8) {
        file = 1,
        module = 2,
        namespace = 3,
        package = 4,
        class = 5,
        method = 6,
        property = 7,
        field = 8,
        constructor = 9,
        @"enum" = 10,
        interface = 11,
        function = 12,
        variable = 13,
        constant = 14,
        string = 15,
        number = 16,
        boolean = 17,
        array = 18,
        object = 19,
        key = 20,
        null = 21,
        enumMember = 22,
        @"struct" = 23,
        event = 24,
        operator = 25,
        typeParameter = 26,
    };

    fn handleDocumentSymbol(
        self: *Server,
        id: std.json.Value,
        msg: std.json.ObjectMap,
        stdout: anytype,
        stderr: anytype,
    ) !void {
        const params = msg.get("params") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const text_document = params.object.get("textDocument") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const uri = text_document.object.get("uri") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };

        const entry = self.documents.getEntry(uri.string) orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const text = entry.text orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };

        // Run lexer to find declarations
        var lexer = lexer_mod.Lexer.init(self.allocator, text, 1) catch {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        defer lexer.deinit();

        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        const writer = buf.writer();
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try std.json.stringify(id, .{}, writer);
        try writer.writeAll(",\"result\":[");

        var symbol_count: usize = 0;
        var prev_token: ?token_mod.Token = null;

        while (true) {
            const tok = lexer.next() catch break;
            if (tok.kind == .end_of_file) break;

            // Check for declaration patterns: class/function/const/let/var/interface/enum/type followed by identifier
            if (prev_token) |prev| {
                if (tok.kind == .identifier) {
                    const symbol_kind: ?SymbolKind = switch (prev.kind) {
                        .keyword_class => .class,
                        .keyword_function => .function,
                        .keyword_const => .constant,
                        .keyword_let => .variable,
                        .keyword_var => .variable,
                        .keyword_interface => .interface,
                        .keyword_enum => .@"enum",
                        .keyword_type => .typeParameter,
                        .keyword_namespace => .namespace,
                        else => null,
                    };

                    if (symbol_kind) |kind| {
                        if (symbol_count > 0) try writer.writeAll(",");

                        const start_line = if (prev.loc.start.line > 0) prev.loc.start.line - 1 else 0;
                        const end_line = if (tok.loc.end.line > 0) tok.loc.end.line - 1 else 0;

                        try writer.writeAll("{\"name\":");
                        try std.json.stringify(tok.text, .{}, writer);
                        try writer.print(",\"kind\":{d}", .{@intFromEnum(kind)});
                        try writer.writeAll(",\"range\":{\"start\":{\"line\":");
                        try writer.print("{d}", .{start_line});
                        try writer.writeAll(",\"character\":");
                        try writer.print("{d}", .{prev.loc.start.column});
                        try writer.writeAll("},\"end\":{\"line\":");
                        try writer.print("{d}", .{end_line});
                        try writer.writeAll(",\"character\":");
                        try writer.print("{d}", .{tok.loc.end.column});
                        try writer.writeAll("}},\"selectionRange\":{\"start\":{\"line\":");
                        try writer.print("{d}", .{if (tok.loc.start.line > 0) tok.loc.start.line - 1 else 0});
                        try writer.writeAll(",\"character\":");
                        try writer.print("{d}", .{tok.loc.start.column});
                        try writer.writeAll("},\"end\":{\"line\":");
                        try writer.print("{d}", .{end_line});
                        try writer.writeAll(",\"character\":");
                        try writer.print("{d}", .{tok.loc.end.column});
                        try writer.writeAll("}}}");

                        symbol_count += 1;
                    }
                }
            }
            prev_token = tok;
        }

        try writer.writeAll("]}");
        try jsonrpc.writeMessage(stdout, buf.items);
        try stderr.print("[mls] Document symbols: {d} symbols\n", .{symbol_count});
    }

    // =========================================================================
    // Go to Definition
    // =========================================================================

    fn handleDefinition(
        self: *Server,
        id: std.json.Value,
        msg: std.json.ObjectMap,
        stdout: anytype,
        stderr: anytype,
    ) !void {
        const params = msg.get("params") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const text_document = params.object.get("textDocument") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const uri = text_document.object.get("uri") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const position = params.object.get("position") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };

        const line: u32 = @intCast(position.object.get("line").?.integer);
        const character: u32 = @intCast(position.object.get("character").?.integer);

        const entry = self.documents.getEntry(uri.string) orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const text = entry.text orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };

        // First pass: find the identifier at cursor position
        var lexer = lexer_mod.Lexer.init(self.allocator, text, 1) catch {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        defer lexer.deinit();

        var target_name: ?[]const u8 = null;

        while (true) {
            const tok = lexer.next() catch break;
            if (tok.kind == .end_of_file) break;

            if (tok.kind == .identifier) {
                const tok_line = if (tok.loc.start.line > 0) tok.loc.start.line - 1 else 0;
                if (tok_line == line and character >= tok.loc.start.column and character < tok.loc.end.column) {
                    target_name = tok.text;
                    break;
                }
            }
        }

        if (target_name == null) {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        }

        // Second pass: find the definition of this name
        var lexer2 = lexer_mod.Lexer.init(self.allocator, text, 1) catch {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        defer lexer2.deinit();

        var prev_token: ?token_mod.Token = null;

        while (true) {
            const tok = lexer2.next() catch break;
            if (tok.kind == .end_of_file) break;

            if (prev_token) |prev| {
                if (tok.kind == .identifier and std.mem.eql(u8, tok.text, target_name.?)) {
                    // Check if this is a definition (preceded by keyword)
                    const is_definition = switch (prev.kind) {
                        .keyword_class,
                        .keyword_function,
                        .keyword_const,
                        .keyword_let,
                        .keyword_var,
                        .keyword_interface,
                        .keyword_enum,
                        .keyword_type,
                        .keyword_namespace,
                        => true,
                        else => false,
                    };

                    if (is_definition) {
                        // Found definition - send location
                        var buf = std.ArrayList(u8).init(self.allocator);
                        defer buf.deinit();

                        const writer = buf.writer();
                        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
                        try std.json.stringify(id, .{}, writer);
                        try writer.writeAll(",\"result\":{\"uri\":");
                        try std.json.stringify(uri.string, .{}, writer);

                        const def_line = if (tok.loc.start.line > 0) tok.loc.start.line - 1 else 0;
                        try writer.writeAll(",\"range\":{\"start\":{\"line\":");
                        try writer.print("{d}", .{def_line});
                        try writer.writeAll(",\"character\":");
                        try writer.print("{d}", .{tok.loc.start.column});
                        try writer.writeAll("},\"end\":{\"line\":");
                        try writer.print("{d}", .{def_line});
                        try writer.writeAll(",\"character\":");
                        try writer.print("{d}", .{tok.loc.end.column});
                        try writer.writeAll("}}}}");

                        try jsonrpc.writeMessage(stdout, buf.items);
                        try stderr.print("[mls] Definition: {s}\n", .{tok.text});
                        return;
                    }
                }
            }
            prev_token = tok;
        }

        // No definition found
        try self.sendResponse(Response.nullResult(id), stdout, stderr);
    }

    // =========================================================================
    // Completion
    // =========================================================================

    fn handleCompletion(
        self: *Server,
        id: std.json.Value,
        msg: std.json.ObjectMap,
        stdout: anytype,
        stderr: anytype,
    ) !void {
        const params = msg.get("params") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const text_document = params.object.get("textDocument") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const uri = text_document.object.get("uri") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };

        const entry = self.documents.getEntry(uri.string) orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const text = entry.text orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };

        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        const writer = buf.writer();
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try std.json.stringify(id, .{}, writer);
        try writer.writeAll(",\"result\":{\"isIncomplete\":false,\"items\":[");

        var item_count: usize = 0;

        // Add keywords
        const keywords = [_][]const u8{
            "async",     "await",       "break",    "case",     "catch",
            "class",     "const",       "continue", "debugger", "default",
            "delete",    "do",          "else",     "enum",     "export",
            "extends",   "false",       "finally",  "for",      "function",
            "if",        "implements",  "import",   "in",       "instanceof",
            "interface", "let",         "new",      "null",     "private",
            "protected", "public",      "return",   "static",   "super",
            "switch",    "this",        "throw",    "true",     "try",
            "typeof",    "var",         "void",     "while",    "with",
        };

        for (keywords) |kw| {
            if (item_count > 0) try writer.writeAll(",");
            try writer.writeAll("{\"label\":");
            try std.json.stringify(kw, .{}, writer);
            try writer.writeAll(",\"kind\":14}"); // 14 = Keyword
            item_count += 1;
        }

        // Add macros
        const macros = [_][]const u8{ "@derive", "@comptime", "@serialize", "@ffi" };
        for (macros) |m| {
            if (item_count > 0) try writer.writeAll(",");
            try writer.writeAll("{\"label\":");
            try std.json.stringify(m, .{}, writer);
            try writer.writeAll(",\"kind\":15}"); // 15 = Snippet
            item_count += 1;
        }

        // Add symbols from current document
        var lexer = lexer_mod.Lexer.init(self.allocator, text, 1) catch {
            try writer.writeAll("]}}");
            try jsonrpc.writeMessage(stdout, buf.items);
            return;
        };
        defer lexer.deinit();

        var seen = std.StringHashMap(void).init(self.allocator);
        defer seen.deinit();

        var prev_token: ?token_mod.Token = null;

        while (true) {
            const tok = lexer.next() catch break;
            if (tok.kind == .end_of_file) break;

            if (prev_token) |prev| {
                if (tok.kind == .identifier) {
                    const kind: ?u8 = switch (prev.kind) {
                        .keyword_class => 7, // Class
                        .keyword_function => 3, // Function
                        .keyword_const, .keyword_let, .keyword_var => 6, // Variable
                        .keyword_interface => 8, // Interface
                        .keyword_enum => 13, // Enum
                        else => null,
                    };

                    if (kind) |k| {
                        if (!seen.contains(tok.text)) {
                            try seen.put(tok.text, {});
                            if (item_count > 0) try writer.writeAll(",");
                            try writer.writeAll("{\"label\":");
                            try std.json.stringify(tok.text, .{}, writer);
                            try writer.print(",\"kind\":{d}}}", .{k});
                            item_count += 1;
                        }
                    }
                }
            }
            prev_token = tok;
        }

        try writer.writeAll("]}}");
        try jsonrpc.writeMessage(stdout, buf.items);
        try stderr.print("[mls] Completion: {d} items\n", .{item_count});
    }

    // =========================================================================
    // Semantic Tokens
    // =========================================================================

    fn handleSemanticTokens(
        self: *Server,
        id: std.json.Value,
        msg: std.json.ObjectMap,
        stdout: anytype,
        stderr: anytype,
    ) !void {
        const params = msg.get("params") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const text_document = params.object.get("textDocument") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const uri = text_document.object.get("uri") orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };

        const entry = self.documents.getEntry(uri.string) orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        const text = entry.text orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };

        // Run lexer to collect all tokens
        var lexer = lexer_mod.Lexer.init(self.allocator, text, 1) catch {
            try stderr.writeAll("[mls] Failed to initialize lexer for semantic tokens\n");
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        defer lexer.deinit();

        // Collect tokens
        var tokens = std.ArrayList(token_mod.Token).init(self.allocator);
        defer tokens.deinit();

        while (true) {
            const tok = lexer.next() catch break;
            if (tok.kind == .end_of_file) break;
            try tokens.append(tok);
        }

        // Convert to semantic token data
        // Each token is 5 integers: deltaLine, deltaStart, length, tokenType, tokenModifiers
        var data = std.ArrayList(u32).init(self.allocator);
        defer data.deinit();

        var prev_line: u32 = 0;
        var prev_char: u32 = 0;

        for (tokens.items) |tok| {
            const sem_type = tokenKindToSemanticType(tok.kind) orelse continue;

            // LSP uses 0-indexed lines, lexer uses 1-indexed
            const line: u32 = if (tok.loc.start.line > 0) tok.loc.start.line - 1 else 0;
            const char: u32 = tok.loc.start.column;
            const length: u32 = @intCast(tok.text.len);

            // Skip zero-length tokens
            if (length == 0) continue;

            // Calculate deltas
            const delta_line = line - prev_line;
            const delta_start = if (delta_line == 0) char - prev_char else char;

            try data.append(delta_line);
            try data.append(delta_start);
            try data.append(length);
            try data.append(@intFromEnum(sem_type));
            try data.append(0); // No modifiers for now

            prev_line = line;
            prev_char = char;
        }

        try stderr.print("[mls] Returning {d} semantic tokens for {s}\n", .{ data.items.len / 5, uri.string });

        // Build response
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        const writer = buf.writer();
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try std.json.stringify(id, .{}, writer);
        try writer.writeAll(",\"result\":{\"data\":[");

        for (data.items, 0..) |val, i| {
            if (i > 0) try writer.writeAll(",");
            try writer.print("{d}", .{val});
        }

        try writer.writeAll("]}}");

        try jsonrpc.writeMessage(stdout, buf.items);
        try stderr.print("[mls] Sent semantic tokens response\n", .{});
    }

    fn sendResponse(
        self: *Server,
        response: Response,
        stdout: anytype,
        stderr: anytype,
    ) !void {
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        try buf.writer().writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try std.json.stringify(response.id, .{}, buf.writer());

        if (response.@"error") |e| {
            try buf.writer().print(",\"error\":{{\"code\":{d},\"message\":", .{e.code});
            try std.json.stringify(e.message, .{}, buf.writer());
            try buf.writer().writeAll("}}");
        } else if (response.result) |r| {
            try buf.writer().writeAll(",\"result\":");
            try std.json.stringify(r, .{}, buf.writer());
        } else {
            try buf.writer().writeAll(",\"result\":null");
        }

        try buf.writer().writeAll("}");
        try jsonrpc.writeMessage(stdout, buf.items);
        try stderr.print("[mls] Sent: {s}\n", .{buf.items});
    }

    fn sendErrorResponse(
        self: *Server,
        id: std.json.Value,
        code: i32,
        message: []const u8,
        stdout: anytype,
    ) !void {
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        try buf.writer().writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try std.json.stringify(id, .{}, buf.writer());
        try buf.writer().print(",\"error\":{{\"code\":{d},\"message\":", .{code});
        try std.json.stringify(message, .{}, buf.writer());
        try buf.writer().writeAll("}}");

        try jsonrpc.writeMessage(stdout, buf.items);
    }

    fn sendParseError(self: *Server, stdout: anytype) !void {
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        try buf.writer().print(
            "{{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{{\"code\":{d},\"message\":\"Parse error\"}}}}",
            .{jsonrpc.ErrorCode.ParseError},
        );

        try jsonrpc.writeMessage(stdout, buf.items);
    }

    // =========================================================================
    // Diagnostics
    // =========================================================================

    /// Run lexer on document and publish diagnostics
    pub fn runDiagnostics(self: *Server, uri: []const u8, stdout: anytype, stderr: anytype) !void {
        const entry = self.documents.getEntry(uri) orelse return;
        const text = entry.text orelse return;

        // Run lexer
        var lexer = lexer_mod.Lexer.init(self.allocator, text, 1) catch {
            try stderr.writeAll("[mls] Failed to initialize lexer\n");
            return;
        };
        defer lexer.deinit();

        // Consume all tokens, collecting errors
        while (true) {
            const tok = lexer.next() catch break;
            if (tok.kind == .end_of_file) break;
        }

        // Convert lexer errors to LSP diagnostics
        var diagnostics = std.ArrayList(Diagnostic).init(self.allocator);
        defer diagnostics.deinit();

        for (lexer.errors.items) |err| {
            // Convert ast.SourceLocation to LSP Range
            // Note: Lexer uses 1-indexed lines, LSP uses 0-indexed
            const diag = Diagnostic{
                .range = .{
                    .start = .{
                        .line = if (err.loc.start.line > 0) err.loc.start.line - 1 else 0,
                        .character = err.loc.start.column,
                    },
                    .end = .{
                        .line = if (err.loc.end.line > 0) err.loc.end.line - 1 else 0,
                        .character = err.loc.end.column,
                    },
                },
                .severity = .@"error",
                .source = "mls",
                .message = err.message,
            };
            try diagnostics.append(diag);
        }

        try stderr.print("[mls] Publishing {d} diagnostics for {s}\n", .{ diagnostics.items.len, uri });

        // Send publishDiagnostics notification
        try self.publishDiagnostics(uri, diagnostics.items, stdout);
    }

    /// Send textDocument/publishDiagnostics notification
    fn publishDiagnostics(self: *Server, uri: []const u8, diagnostics: []const Diagnostic, stdout: anytype) !void {
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        const writer = buf.writer();

        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":");
        try std.json.stringify(uri, .{}, writer);
        try writer.writeAll(",\"diagnostics\":[");

        for (diagnostics, 0..) |diag, i| {
            if (i > 0) try writer.writeAll(",");
            try writer.writeAll("{\"range\":{\"start\":{\"line\":");
            try writer.print("{d}", .{diag.range.start.line});
            try writer.writeAll(",\"character\":");
            try writer.print("{d}", .{diag.range.start.character});
            try writer.writeAll("},\"end\":{\"line\":");
            try writer.print("{d}", .{diag.range.end.line});
            try writer.writeAll(",\"character\":");
            try writer.print("{d}", .{diag.range.end.character});
            try writer.writeAll("}}");

            if (diag.severity) |sev| {
                try writer.print(",\"severity\":{d}", .{@intFromEnum(sev)});
            }
            if (diag.source) |src| {
                try writer.writeAll(",\"source\":");
                try std.json.stringify(src, .{}, writer);
            }
            try writer.writeAll(",\"message\":");
            try std.json.stringify(diag.message, .{}, writer);
            try writer.writeAll("}");
        }

        try writer.writeAll("]}}");

        try jsonrpc.writeMessage(stdout, buf.items);
    }
};

// ============================================================================
// Unit Tests
// ============================================================================

const testing = std.testing;

// Note: Document/DocumentStore tests moved to file_store.zig
// The FileStore module now handles all document storage with additional
// features like dirty tracking, status, and incremental compilation support.

test "ServerCapabilities: JSON serialization" {
    const caps = ServerCapabilities{
        .textDocumentSync = .{
            .openClose = true,
            .change = 2,
            .save = .{ .includeText = false },
        },
        .hoverProvider = true,
    };

    var buf = std.ArrayList(u8).init(testing.allocator);
    defer buf.deinit();

    try std.json.stringify(caps, .{ .emit_null_optional_fields = false }, buf.writer());

    // Verify it's valid JSON by parsing it back
    const parsed = try std.json.parseFromSlice(
        std.json.Value,
        testing.allocator,
        buf.items,
        .{},
    );
    defer parsed.deinit();

    try testing.expect(parsed.value == .object);
}

test "InitializeResult: JSON serialization" {
    const result = InitializeResult{
        .capabilities = .{
            .textDocumentSync = .{
                .openClose = true,
                .change = 2,
            },
        },
        .serverInfo = .{
            .name = "mls",
            .version = "0.1.0",
        },
    };

    var buf = std.ArrayList(u8).init(testing.allocator);
    defer buf.deinit();

    try std.json.stringify(result, .{ .emit_null_optional_fields = false }, buf.writer());

    // Should contain expected fields
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"name\":\"mls\"") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"version\":\"0.1.0\"") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "\"change\":2") != null);
}
