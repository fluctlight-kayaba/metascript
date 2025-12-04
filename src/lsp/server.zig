// LSP Server Implementation
// Handles initialize, shutdown, and document synchronization with proper protocol compliance

const std = @import("std");
const jsonrpc = @import("jsonrpc.zig");
const file_store = @import("file_store.zig");

// Lexer for diagnostics
const lexer_mod = @import("../lexer/lexer.zig");
const token_mod = @import("../lexer/token.zig");
const ast_loc = @import("../ast/location.zig");

// Trans-Am query engine for incremental computation
const transam = @import("../transam/transam.zig");

// Background worker for stale-while-revalidate pattern
const BackgroundWorker = @import("background_worker.zig").BackgroundWorker;
const JobType = @import("background_worker.zig").JobType;
const Priority = @import("background_worker.zig").Priority;

// Type checker for symbol lookup
const checker = @import("../checker/typechecker.zig");
const symbol_mod = @import("../checker/symbol.zig");
const types_mod = @import("../ast/types.zig");

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
        .keyword_defer,    // Metascript: defer cleanup
        .keyword_distinct, // Metascript: distinct types
        => .keyword,

        // Type-related keywords
        .keyword_class => .class,
        .keyword_interface => .interface,
        .keyword_enum => .@"enum",
        .keyword_type => .type,
        .keyword_namespace => .namespace,

        // Sized type keywords
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
        => .type,

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

        // Macros - only @ sign now, macro name is separate identifier
        .at_sign => .macro,

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
        // @ sign - macro prefix (actual macro name is in following identifier)
        .at_sign => "**@** - Macro prefix\n\nUsed to invoke compile-time macros from `std/macros/*.ms`.\n\nCommon macros: `@derive`, `@comptime`, `@serialize`, `@ffi`",

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
// Type Formatting for Hover
// ============================================================================

/// Format a type for hover display
fn formatType(allocator: std.mem.Allocator, typ: ?*types_mod.Type) ![]const u8 {
    if (typ == null) return try allocator.dupe(u8, "unknown");

    const t = typ.?;
    return switch (t.kind) {
        .number => try allocator.dupe(u8, "number"),
        .string => try allocator.dupe(u8, "string"),
        .boolean => try allocator.dupe(u8, "boolean"),
        .void => try allocator.dupe(u8, "void"),
        .unknown => try allocator.dupe(u8, "unknown"),
        .never => try allocator.dupe(u8, "never"),
        .int8 => try allocator.dupe(u8, "int8"),
        .int16 => try allocator.dupe(u8, "int16"),
        .int32 => try allocator.dupe(u8, "int32"),
        .int64 => try allocator.dupe(u8, "int64"),
        .uint8 => try allocator.dupe(u8, "uint8"),
        .uint16 => try allocator.dupe(u8, "uint16"),
        .uint32 => try allocator.dupe(u8, "uint32"),
        .uint64 => try allocator.dupe(u8, "uint64"),
        .float32 => try allocator.dupe(u8, "float32"),
        .float64 => try allocator.dupe(u8, "float64"),
        .ref => blk: {
            const inner_type = try formatType(allocator, t.data.ref);
            defer allocator.free(inner_type);
            break :blk try std.fmt.allocPrint(allocator, "ref {s}", .{inner_type});
        },
        .lent => blk: {
            const inner_type = try formatType(allocator, t.data.lent);
            defer allocator.free(inner_type);
            break :blk try std.fmt.allocPrint(allocator, "lent {s}", .{inner_type});
        },
        .array => blk: {
            const elem_type = try formatType(allocator, t.data.array);
            defer allocator.free(elem_type);
            break :blk try std.fmt.allocPrint(allocator, "{s}[]", .{elem_type});
        },
        .function => blk: {
            const func = t.data.function;
            var params_buf = std.ArrayList(u8).init(allocator);
            defer params_buf.deinit();

            for (func.params, 0..) |param, i| {
                if (i > 0) try params_buf.appendSlice(", ");
                try params_buf.appendSlice(param.name);
                try params_buf.appendSlice(": ");
                const param_type = try formatType(allocator, param.type);
                defer allocator.free(param_type);
                try params_buf.appendSlice(param_type);
            }

            const ret_type = try formatType(allocator, func.return_type);
            defer allocator.free(ret_type);

            break :blk try std.fmt.allocPrint(allocator, "({s}) => {s}", .{ params_buf.items, ret_type });
        },
        .type_reference => blk: {
            const ref = t.data.type_reference;
            if (ref.type_args.len == 0) {
                break :blk try allocator.dupe(u8, ref.name);
            }
            // Format generic type args
            var args_buf = std.ArrayList(u8).init(allocator);
            defer args_buf.deinit();
            try args_buf.appendSlice(ref.name);
            try args_buf.append('<');
            for (ref.type_args, 0..) |arg, i| {
                if (i > 0) try args_buf.appendSlice(", ");
                const arg_type = try formatType(allocator, arg);
                defer allocator.free(arg_type);
                try args_buf.appendSlice(arg_type);
            }
            try args_buf.append('>');
            break :blk try args_buf.toOwnedSlice();
        },
        .@"union" => blk: {
            const u = t.data.@"union";
            var buf = std.ArrayList(u8).init(allocator);
            defer buf.deinit();
            for (u.types, 0..) |member, i| {
                if (i > 0) try buf.appendSlice(" | ");
                const member_type = try formatType(allocator, member);
                defer allocator.free(member_type);
                try buf.appendSlice(member_type);
            }
            break :blk try buf.toOwnedSlice();
        },
        .object => try allocator.dupe(u8, "object"),
        .tuple => try allocator.dupe(u8, "tuple"),
        .generic_param => blk: {
            break :blk try allocator.dupe(u8, t.data.generic_param.name);
        },
        .generic_instance => try allocator.dupe(u8, "generic"),
        .intersection => try allocator.dupe(u8, "intersection"),
    };
}

/// Format a symbol for hover display (returns allocated markdown string)
fn formatSymbolHover(allocator: std.mem.Allocator, symbol: symbol_mod.Symbol) ![]const u8 {
    var buf = std.ArrayList(u8).init(allocator);
    errdefer buf.deinit();

    // Format based on symbol kind
    switch (symbol.kind) {
        .variable => {
            const modifier = if (symbol.mutable) "let" else "const";
            const type_str = try formatType(allocator, symbol.type);
            defer allocator.free(type_str);
            try buf.writer().print("```typescript\n{s} {s}: {s}\n```", .{ modifier, symbol.name, type_str });
        },
        .function => {
            if (symbol.type) |t| {
                if (t.kind == .function) {
                    const func = t.data.function;
                    try buf.appendSlice("```typescript\nfunction ");
                    try buf.appendSlice(symbol.name);
                    try buf.append('(');
                    for (func.params, 0..) |param, i| {
                        if (i > 0) try buf.appendSlice(", ");
                        try buf.appendSlice(param.name);
                        try buf.appendSlice(": ");
                        const param_type = try formatType(allocator, param.type);
                        defer allocator.free(param_type);
                        try buf.appendSlice(param_type);
                    }
                    try buf.appendSlice("): ");
                    const ret_type = try formatType(allocator, func.return_type);
                    defer allocator.free(ret_type);
                    try buf.appendSlice(ret_type);
                    try buf.appendSlice("\n```");
                } else {
                    try buf.writer().print("```typescript\nfunction {s}\n```", .{symbol.name});
                }
            } else {
                try buf.writer().print("```typescript\nfunction {s}\n```", .{symbol.name});
            }
        },
        .class => {
            try buf.writer().print("```typescript\nclass {s}\n```", .{symbol.name});
        },
        .interface => {
            try buf.writer().print("```typescript\ninterface {s}\n```", .{symbol.name});
        },
        .type_alias => {
            const type_str = try formatType(allocator, symbol.type);
            defer allocator.free(type_str);
            try buf.writer().print("```typescript\ntype {s} = {s}\n```", .{ symbol.name, type_str });
        },
        .parameter => {
            const type_str = try formatType(allocator, symbol.type);
            defer allocator.free(type_str);
            try buf.writer().print("```typescript\n(parameter) {s}: {s}\n```", .{ symbol.name, type_str });
        },
        .property => {
            const type_str = try formatType(allocator, symbol.type);
            defer allocator.free(type_str);
            const readonly = if (!symbol.mutable) "readonly " else "";
            try buf.writer().print("```typescript\n{s}(property) {s}: {s}\n```", .{ readonly, symbol.name, type_str });
        },
        .method => {
            if (symbol.type) |t| {
                if (t.kind == .function) {
                    const func = t.data.function;
                    try buf.appendSlice("```typescript\n(method) ");
                    try buf.appendSlice(symbol.name);
                    try buf.append('(');
                    for (func.params, 0..) |param, i| {
                        if (i > 0) try buf.appendSlice(", ");
                        try buf.appendSlice(param.name);
                        try buf.appendSlice(": ");
                        const param_type = try formatType(allocator, param.type);
                        defer allocator.free(param_type);
                        try buf.appendSlice(param_type);
                    }
                    try buf.appendSlice("): ");
                    const ret_type = try formatType(allocator, func.return_type);
                    defer allocator.free(ret_type);
                    try buf.appendSlice(ret_type);
                    try buf.appendSlice("\n```");
                } else {
                    try buf.writer().print("```typescript\n(method) {s}\n```", .{symbol.name});
                }
            } else {
                try buf.writer().print("```typescript\n(method) {s}\n```", .{symbol.name});
            }
        },
    }

    return buf.toOwnedSlice();
}

/// Format generated AST nodes for macro expansion preview
fn formatMacroExpansion(allocator: std.mem.Allocator, nodes: []*ast.Node, macro_name: []const u8, arguments: []const []const u8) ![]const u8 {
    var buf = std.ArrayList(u8).init(allocator);
    errdefer buf.deinit();

    const writer = buf.writer();

    // Header with macro info
    try writer.print("**@{s}(", .{macro_name});
    for (arguments, 0..) |arg, i| {
        if (i > 0) try writer.writeAll(", ");
        try writer.writeAll(arg);
    }
    try writer.writeAll(")** - Macro expansion preview\n\n");

    if (nodes.len == 0) {
        try writer.writeAll("*No code generated*");
        return buf.toOwnedSlice();
    }

    try writer.writeAll("```typescript\n");
    try writer.writeAll("// Generated methods:\n");

    for (nodes) |node| {
        try formatAstNode(writer, node);
        try writer.writeAll("\n");
    }

    try writer.writeAll("```");
    return buf.toOwnedSlice();
}

/// Format a single AST node as code
fn formatAstNode(writer: anytype, node: *ast.Node) !void {
    switch (node.kind) {
        .function_decl => {
            const func = node.data.function_decl;
            try writer.print("{s}(", .{func.name});

            // Format parameters
            for (func.params, 0..) |param, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.writeAll(param.name);
                if (param.type) |param_type| {
                    try writer.writeAll(": ");
                    try formatTypePtr(writer, param_type);
                }
            }

            try writer.writeAll(")");

            // Return type
            if (func.return_type) |ret_type| {
                try writer.writeAll(": ");
                try formatTypePtr(writer, ret_type);
            }

            try writer.writeAll(" { ... }");
        },
        .method_decl => {
            const method = node.data.method_decl;
            try writer.print("{s}(", .{method.name});

            for (method.params, 0..) |param, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.writeAll(param.name);
                if (param.type) |param_type| {
                    try writer.writeAll(": ");
                    try formatTypePtr(writer, param_type);
                }
            }

            try writer.writeAll(")");

            if (method.return_type) |ret_type| {
                try writer.writeAll(": ");
                try formatTypePtr(writer, ret_type);
            }

            // Format actual body if present
            if (method.body) |body| {
                try writer.writeAll(" {\n");
                try formatMethodBody(writer, body, 1);
                try writer.writeAll("}");
            } else {
                try writer.writeAll(" { }");
            }
        },
        .property_decl => {
            const prop = node.data.property_decl;
            if (prop.readonly) try writer.writeAll("readonly ");
            try writer.print("{s}", .{prop.name});
            if (prop.type) |prop_type| {
                try writer.writeAll(": ");
                try formatTypePtr(writer, prop_type);
            }
            try writer.writeAll(";");
        },
        else => {
            try writer.writeAll("// (generated code)");
        },
    }
}

/// Format method body (block statement) recursively
fn formatMethodBody(writer: anytype, body: *ast.Node, indent: usize) !void {
    if (body.kind != .block_stmt) {
        try formatExpr(writer, body);
        return;
    }

    const block = body.data.block_stmt;
    for (block.statements) |stmt| {
        // Write indent
        for (0..indent) |_| {
            try writer.writeAll("  ");
        }
        try formatStatement(writer, stmt, indent);
        try writer.writeAll("\n");
    }
}

/// Format a statement
fn formatStatement(writer: anytype, stmt: *ast.Node, indent: usize) !void {
    _ = indent;
    switch (stmt.kind) {
        .return_stmt => {
            try writer.writeAll("return ");
            if (stmt.data.return_stmt.argument) |arg| {
                try formatExpr(writer, arg);
            }
            try writer.writeAll(";");
        },
        else => {
            try formatExpr(writer, stmt);
            try writer.writeAll(";");
        },
    }
}

/// Format an expression
fn formatExpr(writer: anytype, expr: *ast.Node) !void {
    switch (expr.kind) {
        .identifier => {
            try writer.writeAll(expr.data.identifier);
        },
        .binary_expr => {
            const bin = expr.data.binary_expr;
            try formatExpr(writer, bin.left);
            const op_str = switch (bin.op) {
                .eq => " === ",
                .@"and" => " && ",
                .@"or" => " || ",
                .add => " + ",
                .sub => " - ",
                .mul => " * ",
                .div => " / ",
                else => " ? ",
            };
            try writer.writeAll(op_str);
            try formatExpr(writer, bin.right);
        },
        .member_expr => {
            const mem = expr.data.member_expr;
            try formatExpr(writer, mem.object);
            try writer.writeAll(".");
            try formatExpr(writer, mem.property);
        },
        else => {
            try writer.writeAll("...");
        },
    }
}

/// Format a types.Type pointer as code
fn formatTypePtr(writer: anytype, typ: *types_mod.Type) !void {
    switch (typ.kind) {
        .number => try writer.writeAll("number"),
        .string => try writer.writeAll("string"),
        .boolean => try writer.writeAll("boolean"),
        .void => try writer.writeAll("void"),
        .unknown => try writer.writeAll("unknown"),
        .never => try writer.writeAll("never"),
        .array => {
            try formatTypePtr(writer, typ.data.array);
            try writer.writeAll("[]");
        },
        .type_reference => {
            const ref = typ.data.type_reference;
            try writer.writeAll(ref.name);
            if (ref.type_args.len > 0) {
                try writer.writeAll("<");
                for (ref.type_args, 0..) |arg, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try formatTypePtr(writer, arg);
                }
                try writer.writeAll(">");
            }
        },
        else => try writer.writeAll("any"),
    }
}

// Import AST for formatting
const ast = @import("../ast/ast.zig");

// ============================================================================
// Server
// ============================================================================

pub const Server = struct {
    allocator: std.mem.Allocator,
    initialized: bool = false,
    shutdown_requested: bool = false,
    documents: file_store.FileStore,

    // Trans-Am query engine for incremental computation
    transam_db: ?transam.TransAmDatabase = null,

    // Background worker for stale-while-revalidate pattern
    background_worker: ?BackgroundWorker = null,

    pub fn init(allocator: std.mem.Allocator) Server {
        return .{
            .allocator = allocator,
            .documents = file_store.FileStore.init(allocator),
            .transam_db = null,
            .background_worker = null,
        };
    }

    /// Initialize Trans-Am database (called after LSP initialize)
    pub fn initTransAm(self: *Server) !void {
        if (self.transam_db == null) {
            self.transam_db = try transam.TransAmDatabase.init(self.allocator);
            // Initialize background worker with Trans-Am's cancellation version
            if (self.transam_db) |*db| {
                self.background_worker = BackgroundWorker.init(self.allocator, db.cancellation_version);
            }
        }
    }

    pub fn deinit(self: *Server) void {
        // Clean up background worker first (before Trans-Am)
        if (self.background_worker) |*bw| {
            bw.deinit();
        }
        if (self.transam_db) |*db| {
            db.deinit();
        }
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
            // Initialize Trans-Am query engine
            self.initTransAm() catch |err| {
                try stderr.print("[mls] Warning: Failed to init Trans-Am: {}\n", .{err});
            };
            try stderr.writeAll("[mls] Client initialized (Trans-Am ready)\n");
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

        // Feed into Trans-Am for incremental computation
        if (self.transam_db) |*db| {
            const changed = db.setFileText(uri.string, text.string) catch false;
            if (changed) {
                try stderr.print("[mls] Trans-Am: file registered (revision {d})\n", .{db.getRevision().value});
            }
        }

        // Queue background pre-warming for faster subsequent requests
        if (self.background_worker) |*bw| {
            const file_hash = transam.hashString(uri.string);
            bw.queue(.prewarm, .open, file_hash, uri.string) catch {};
        }

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
        // Cancel all pending queries immediately (rust-analyzer pattern)
        // When user types, all in-flight macro expansions, type checks, etc. should abort
        if (self.transam_db) |*db| {
            db.cancelPendingQueries();
        }

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

        // Feed updated content into Trans-Am for incremental recomputation
        if (self.transam_db) |*db| {
            if (entry.text) |current_text| {
                const changed = db.setFileText(uri.string, current_text) catch false;
                if (changed) {
                    try stderr.print("[mls] Trans-Am: revision {d}\n", .{db.getRevision().value});
                }
            }
        }

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
        const start_time = std.time.milliTimestamp();

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
            // Check for user-defined macro: @ sign followed by identifier
            // Re-lex to find if we're on @macroName pattern
            var is_user_macro = false;
            var macro_name: ?[]const u8 = null;

            if (tok.kind == .at_sign or tok.kind == .identifier) {
                var lexer2 = lexer_mod.Lexer.init(self.allocator, text, 1) catch {
                    try self.sendResponse(Response.nullResult(id), stdout, stderr);
                    return;
                };
                defer lexer2.deinit();

                var prev_tok2: ?token_mod.Token = null;
                while (true) {
                    const tok2 = lexer2.next() catch break;
                    if (tok2.kind == .end_of_file) break;

                    if (tok2.kind == .identifier and prev_tok2 != null and prev_tok2.?.kind == .at_sign) {
                        // Check if either @ or identifier is at our position
                        const at_line = if (prev_tok2.?.loc.start.line > 0) prev_tok2.?.loc.start.line - 1 else 0;
                        const id_line = if (tok2.loc.start.line > 0) tok2.loc.start.line - 1 else 0;

                        if ((at_line == line and character >= prev_tok2.?.loc.start.column and character < prev_tok2.?.loc.end.column) or
                            (id_line == line and character >= tok2.loc.start.column and character < tok2.loc.end.column))
                        {
                            is_user_macro = true;
                            macro_name = tok2.text;
                            break;
                        }
                    }
                    prev_tok2 = tok2;
                }
            }

            // If it's a user-defined macro, try to expand and show preview
            if (is_user_macro and macro_name != null) {
                // Try Trans-Am macro expansion first
                if (self.transam_db) |*db| {
                    const expansion_hover = self.getMacroExpansionHover(db, uri.string, macro_name.?, line) catch null;
                    if (expansion_hover) |content| {
                        defer self.allocator.free(content);
                        const elapsed_ms = std.time.milliTimestamp() - start_time;
                        try stderr.print("[mls] Hover: macro expansion '{s}' ({d}ms)\n", .{ macro_name.?, elapsed_ms });
                        try self.sendHoverResponse(id, content, tok, stdout, stderr);
                        return;
                    }
                }
                // Fallback to docs from source file
                if (try self.getMacroHoverDocs(macro_name.?)) |docs| {
                    const elapsed_ms = std.time.milliTimestamp() - start_time;
                    try stderr.print("[mls] Hover: macro '{s}' ({d}ms)\n", .{ macro_name.?, elapsed_ms });
                    try self.sendHoverResponse(id, docs, tok, stdout, stderr);
                    self.allocator.free(docs);
                    return;
                }
            }

            // All macros are now @ + identifier, handled above via is_user_macro path

            const hover_content = getHoverContent(tok.kind, tok.text);
            if (hover_content) |content| {
                const elapsed_ms = std.time.milliTimestamp() - start_time;
                try stderr.print("[mls] Hover: '{s}' ({d}ms)\n", .{ tok.text, elapsed_ms });
                try self.sendHoverResponse(id, content, tok, stdout, stderr);
                return;
            }

            // For identifiers, try to look up type information via Trans-Am
            if (tok.kind == .identifier) {
                if (self.transam_db) |*db| {
                    // Look up the symbol in the type checker's symbol table
                    const symbol_result = db.lookupSymbol(uri.string, tok.text) catch null;
                    if (symbol_result) |symbol| {
                        const hover_text = formatSymbolHover(self.allocator, symbol) catch null;
                        if (hover_text) |content| {
                            defer self.allocator.free(content);
                            const elapsed_ms = std.time.milliTimestamp() - start_time;
                            try stderr.print("[mls] Hover: type info for '{s}' ({d}ms)\n", .{ tok.text, elapsed_ms });
                            try self.sendHoverResponse(id, content, tok, stdout, stderr);
                            return;
                        }
                    }
                }
            }
        }

        // No hover info available
        const elapsed_ms = std.time.milliTimestamp() - start_time;
        try stderr.print("[mls] Hover: no info at {d}:{d} ({d}ms)\n", .{ line, character, elapsed_ms });
        try self.sendResponse(Response.nullResult(id), stdout, stderr);
    }

    /// Get hover documentation for a macro from its source file
    fn getMacroHoverDocs(self: *Server, macro_name: []const u8) !?[]const u8 {
        // Map macro names to potential source files
        var target_file: ?[]const u8 = null;

        if (std.mem.startsWith(u8, macro_name, "derive")) {
            target_file = "std/macros/derive.ms";
        }

        if (target_file) |file_path| {
            if (std.fs.cwd().access(file_path, .{})) |_| {
                const file = std.fs.cwd().openFile(file_path, .{}) catch return null;
                defer file.close();

                const content = file.readToEndAlloc(self.allocator, 1024 * 1024) catch return null;
                defer self.allocator.free(content);

                // Search for the macro definition and preceding comments
                const search_pattern = try std.fmt.allocPrint(
                    self.allocator,
                    "@macro function {s}",
                    .{macro_name},
                );
                defer self.allocator.free(search_pattern);

                if (std.mem.indexOf(u8, content, search_pattern)) |def_pos| {
                    // Look backwards for comments
                    var comment_start: usize = def_pos;
                    var i = def_pos;
                    while (i > 0) {
                        i -= 1;
                        if (content[i] == '\n') {
                            // Check if previous line is a comment
                            var line_start = i + 1;
                            while (line_start < def_pos and (content[line_start] == ' ' or content[line_start] == '\t')) {
                                line_start += 1;
                            }
                            if (line_start + 2 < def_pos and content[line_start] == '/' and content[line_start + 1] == '/') {
                                comment_start = line_start;
                            } else {
                                break;
                            }
                        }
                    }

                    // Extract comments
                    var docs = std.ArrayList(u8).init(self.allocator);
                    errdefer docs.deinit();

                    try docs.appendSlice("**@");
                    try docs.appendSlice(macro_name);
                    try docs.appendSlice("** - Source-defined macro\n\n");

                    // Parse comment lines
                    var line_start: usize = comment_start;
                    for (content[comment_start..def_pos], 0..) |c, idx| {
                        if (c == '\n') {
                            const line = content[line_start .. comment_start + idx];
                            // Strip leading whitespace and //
                            var trimmed = line;
                            while (trimmed.len > 0 and (trimmed[0] == ' ' or trimmed[0] == '\t')) {
                                trimmed = trimmed[1..];
                            }
                            if (trimmed.len >= 2 and trimmed[0] == '/' and trimmed[1] == '/') {
                                trimmed = trimmed[2..];
                                if (trimmed.len > 0 and trimmed[0] == ' ') {
                                    trimmed = trimmed[1..];
                                }
                                try docs.appendSlice(trimmed);
                                try docs.append('\n');
                            }
                            line_start = comment_start + idx + 1;
                        }
                    }

                    try docs.appendSlice("\n```typescript\n@");
                    try docs.appendSlice(macro_name);
                    try docs.appendSlice("\nclass YourClass { ... }\n```\n\n");
                    try docs.appendSlice("*Defined in: ");
                    try docs.appendSlice(file_path);
                    try docs.appendSlice("*");

                    return try docs.toOwnedSlice();
                }
            } else |_| {}
        }

        return null;
    }

    /// Get macro expansion hover content using Trans-Am
    fn getMacroExpansionHover(
        self: *Server,
        db: *transam.TransAmDatabase,
        file_uri: []const u8,
        macro_name: []const u8,
        hover_line: u32,
    ) !?[]const u8 {
        // Get all macro call sites in the file
        const call_sites = db.getMacroCallSites(file_uri) catch return null;
        defer {
            for (call_sites) |site| {
                self.allocator.free(site.arguments);
            }
            self.allocator.free(call_sites);
        }

        // Find the macro call site at the hover line
        var target_site: ?transam.MacroCallSite = null;
        for (call_sites) |site| {
            // Check if macro name matches and line is close
            if (std.mem.eql(u8, site.macro_name, macro_name)) {
                // Line comparison: site.line is from AST (1-indexed, points to class)
                // hover_line is from LSP (0-indexed, points to @derive decorator)
                // Decorators are typically on lines before the class
                // So check if hover_line+1 is at or before site.line (within a few lines)
                const hover_line_1indexed = hover_line + 1;
                if (hover_line_1indexed <= site.line and site.line - hover_line_1indexed <= 3) {
                    target_site = site;
                    break;
                }
            }
        }

        if (target_site == null) return null;
        const site = target_site.?;

        // Debug: Check if target_node is available for MacroVM
        std.debug.print("[mls] Macro site: {s}, target_node: {?}\n", .{ site.macro_name, site.target_node });

        // Expand the macro
        const expansion_result = db.expandMacroCallSite(file_uri, site) catch |err| {
            std.debug.print("[mls] expandMacroCallSite failed: {}\n", .{err});
            return null;
        };

        // Format the expansion for display
        const hover_content = formatMacroExpansion(
            self.allocator,
            expansion_result.generated_nodes,
            site.macro_name,
            site.arguments,
        ) catch return null;

        return hover_content;
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

        // First pass: find the identifier or macro at cursor position
        var lexer = lexer_mod.Lexer.init(self.allocator, text, 1) catch {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        defer lexer.deinit();

        var target_name: ?[]const u8 = null;
        var is_macro: bool = false;
        var prev_tok: ?token_mod.Token = null;

        while (true) {
            const tok = lexer.next() catch break;
            if (tok.kind == .end_of_file) break;

            const tok_line = if (tok.loc.start.line > 0) tok.loc.start.line - 1 else 0;

            // All macros are now @ followed by identifier
            if (tok.kind == .identifier) {
                // Check if previous token was @
                if (prev_tok) |prev| {
                    if (prev.kind == .at_sign) {
                        // This is @macroName - check if cursor is on either @ or the name
                        const at_line = if (prev.loc.start.line > 0) prev.loc.start.line - 1 else 0;
                        if ((at_line == line and character >= prev.loc.start.column and character < prev.loc.end.column) or
                            (tok_line == line and character >= tok.loc.start.column and character < tok.loc.end.column))
                        {
                            target_name = tok.text;
                            is_macro = true;
                            break;
                        }
                    }
                }

                // Check regular identifier
                if (tok_line == line and character >= tok.loc.start.column and character < tok.loc.end.column) {
                    target_name = tok.text;
                    break;
                }
            }

            prev_tok = tok;
        }

        if (target_name == null) {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        }

        // If it's a macro, try to find it in std/macros
        if (is_macro) {
            if (try self.findMacroDefinition(target_name.?, id, stdout, stderr)) {
                return;
            }
        }

        // Try Trans-Am symbol table lookup first (type-aware, scope-aware)
        if (self.transam_db) |*db| {
            const symbol_opt = db.lookupSymbol(uri.string, target_name.?) catch null;
            if (symbol_opt) |symbol| {
                // Found symbol in type checker's symbol table
                var buf = std.ArrayList(u8).init(self.allocator);
                defer buf.deinit();

                const writer = buf.writer();
                try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
                try std.json.stringify(id, .{}, writer);
                try writer.writeAll(",\"result\":{\"uri\":");
                try std.json.stringify(uri.string, .{}, writer);

                // SourceLocation uses 1-based lines, LSP uses 0-based
                const def_line = if (symbol.location.start.line > 0) symbol.location.start.line - 1 else 0;
                try writer.writeAll(",\"range\":{\"start\":{\"line\":");
                try writer.print("{d}", .{def_line});
                try writer.writeAll(",\"character\":");
                try writer.print("{d}", .{symbol.location.start.column});
                try writer.writeAll("},\"end\":{\"line\":");
                const end_line = if (symbol.location.end.line > 0) symbol.location.end.line - 1 else def_line;
                try writer.print("{d}", .{end_line});
                try writer.writeAll(",\"character\":");
                try writer.print("{d}", .{symbol.location.end.column});
                try writer.writeAll("}}}}");

                try jsonrpc.writeMessage(stdout, buf.items);
                try stderr.print("[mls] Definition (Trans-Am): {s} at line {d}\n", .{ target_name.?, def_line + 1 });
                return;
            }
        }

        // Fallback: lexer-based search for definition of this name
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

    /// Find macro definition in std/macros or source files
    fn findMacroDefinition(
        self: *Server,
        macro_name: []const u8,
        id: std.json.Value,
        stdout: anytype,
        stderr: anytype,
    ) !bool {
        try stderr.print("[mls] Looking for macro definition: @{s}\n", .{macro_name});

        // Map macro names to file locations
        // Built-in derive macros are in std/macros/derive.ms
        const macro_files = [_]struct { prefix: []const u8, file: []const u8 }{
            .{ .prefix = "derive", .file = "std/macros/derive.ms" },
            .{ .prefix = "Eq", .file = "std/macros/derive.ms" },
            .{ .prefix = "Hash", .file = "std/macros/derive.ms" },
            .{ .prefix = "Clone", .file = "std/macros/derive.ms" },
            .{ .prefix = "Debug", .file = "std/macros/derive.ms" },
        };

        var target_file: ?[]const u8 = null;

        // Check if macro name matches known macros
        for (macro_files) |mapping| {
            if (std.mem.eql(u8, macro_name, mapping.prefix)) {
                target_file = mapping.file;
                break;
            }
        }

        // Also check for deriveX pattern
        if (target_file == null and std.mem.startsWith(u8, macro_name, "derive")) {
            target_file = "std/macros/derive.ms";
        }

        if (target_file) |file_path| {
            // Try to access the file
            if (std.fs.cwd().access(file_path, .{})) |_| {
                // Read and find the exact function definition
                const file = std.fs.cwd().openFile(file_path, .{}) catch {
                    return false;
                };
                defer file.close();

                const content = file.readToEndAlloc(self.allocator, 1024 * 1024) catch {
                    return false;
                };
                defer self.allocator.free(content);

                // Search for @macro function macroName or function macroName
                var line_num: u32 = 0;
                var line_start: usize = 0;

                for (content, 0..) |c, i| {
                    if (c == '\n') {
                        const line = content[line_start..i];

                        // Look for @macro function macroName
                        const search_pattern = try std.fmt.allocPrint(
                            self.allocator,
                            "@macro function {s}",
                            .{macro_name},
                        );
                        defer self.allocator.free(search_pattern);

                        if (std.mem.indexOf(u8, line, search_pattern)) |col| {
                            // Found it! Calculate the column where the name starts
                            const name_col = col + "@macro function ".len;

                            // Get absolute path
                            var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
                            const abs_path = std.fs.cwd().realpath(file_path, &abs_path_buf) catch {
                                return false;
                            };

                            // Build file URI
                            const file_uri = try std.fmt.allocPrint(
                                self.allocator,
                                "file://{s}",
                                .{abs_path},
                            );
                            defer self.allocator.free(file_uri);

                            // Send location response
                            var buf = std.ArrayList(u8).init(self.allocator);
                            defer buf.deinit();

                            const writer = buf.writer();
                            try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
                            try std.json.stringify(id, .{}, writer);
                            try writer.writeAll(",\"result\":{\"uri\":");
                            try std.json.stringify(file_uri, .{}, writer);
                            try writer.writeAll(",\"range\":{\"start\":{\"line\":");
                            try writer.print("{d}", .{line_num});
                            try writer.writeAll(",\"character\":");
                            try writer.print("{d}", .{name_col});
                            try writer.writeAll("},\"end\":{\"line\":");
                            try writer.print("{d}", .{line_num});
                            try writer.writeAll(",\"character\":");
                            try writer.print("{d}", .{name_col + macro_name.len});
                            try writer.writeAll("}}}}");

                            try jsonrpc.writeMessage(stdout, buf.items);
                            try stderr.print("[mls] Found macro @{s} in {s}:{d}\n", .{
                                macro_name,
                                file_path,
                                line_num + 1,
                            });
                            return true;
                        }

                        line_num += 1;
                        line_start = i + 1;
                    }
                }
            } else |_| {}
        }

        return false;
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
        _ = entry; // We use uri.string as file_id for Trans-Am

        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        const writer = buf.writer();
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try std.json.stringify(id, .{}, writer);
        try writer.writeAll(",\"result\":{\"isIncomplete\":false,\"items\":[");

        var item_count: usize = 0;

        // Add keywords (static list)
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

        // Add macros (static list)
        const macros = [_][]const u8{ "@derive", "@comptime", "@serialize", "@ffi" };
        for (macros) |m| {
            if (item_count > 0) try writer.writeAll(",");
            try writer.writeAll("{\"label\":");
            try std.json.stringify(m, .{}, writer);
            try writer.writeAll(",\"kind\":15}"); // 15 = Snippet
            item_count += 1;
        }

        // Add symbols from document using cached completions (L4)
        if (self.transam_db) |*db| {
            const completions = db.getCompletions(uri.string) catch null;
            if (completions) |result| {
                defer self.allocator.free(result.items);
                for (result.items) |item| {
                    defer self.allocator.free(item.label);
                    if (item.detail) |d| self.allocator.free(d);

                    if (item_count > 0) try writer.writeAll(",");
                    try writer.writeAll("{\"label\":");
                    try std.json.stringify(item.label, .{}, writer);
                    try writer.print(",\"kind\":{d}}}", .{item.kind});
                    item_count += 1;
                }
                if (result.from_cache) {
                    try stderr.print("[mls] Completion: {d} items (cached{s})\n", .{
                        item_count,
                        if (result.is_stale) ", stale" else "",
                    });
                    // Queue background refresh if stale
                    if (result.is_stale) {
                        if (self.background_worker) |*bw| {
                            const file_hash = transam.hashString(uri.string);
                            bw.queue(.completion_refresh, .visible, file_hash, uri.string) catch {};
                        }
                    }
                } else {
                    try stderr.print("[mls] Completion: {d} items (generated)\n", .{item_count});
                }
            } else {
                try stderr.print("[mls] Completion: {d} items (no db)\n", .{item_count});
            }
        } else {
            try stderr.print("[mls] Completion: {d} items (no transam)\n", .{item_count});
        }

        try writer.writeAll("]}}");
        try jsonrpc.writeMessage(stdout, buf.items);
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
        const start_time = std.time.milliTimestamp();

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

        // Convert to semantic token data
        // Each token is 5 integers: deltaLine, deltaStart, length, tokenType, tokenModifiers
        var data = std.ArrayList(u32).init(self.allocator);
        defer data.deinit();

        // Try Trans-Am first (fast path with caching)
        const used_transam = if (self.transam_db) |*db| blk: {
            const syntax_tokens = db.getSyntaxTokens(uri.string) catch {
                break :blk false;
            };
            defer self.allocator.free(syntax_tokens);

            var prev_line: u32 = 0;
            var prev_char: u32 = 0;

            for (syntax_tokens) |tok| {
                const sem_type = syntaxTokenTypeToSemanticType(tok.token_type);

                // Skip zero-length tokens
                if (tok.length == 0) continue;

                // Calculate deltas
                const delta_line = tok.line -| prev_line;
                const delta_start = if (delta_line == 0) tok.column -| prev_char else tok.column;

                try data.append(delta_line);
                try data.append(delta_start);
                try data.append(tok.length);
                try data.append(@intFromEnum(sem_type));
                try data.append(tok.modifiers);

                prev_line = tok.line;
                prev_char = tok.column;
            }
            break :blk true;
        } else false;

        // Fallback to direct lexer if Trans-Am not available
        if (!used_transam) {
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

            var prev_line: u32 = 0;
            var prev_char: u32 = 0;

            while (true) {
                const tok = lexer.next() catch break;
                if (tok.kind == .end_of_file) break;

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
        }

        const elapsed_ms = std.time.milliTimestamp() - start_time;
        const source = if (used_transam) "Trans-Am" else "lexer";
        try stderr.print("[mls] Returning {d} semantic tokens for {s} ({s}, {d}ms)\n", .{
            data.items.len / 5,
            uri.string,
            source,
            elapsed_ms,
        });

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

    /// Convert Trans-Am SyntaxTokenType to LSP SemanticTokenType
    fn syntaxTokenTypeToSemanticType(syntax_type: transam.SyntaxTokenType) SemanticTokenType {
        return switch (syntax_type) {
            .keyword => .keyword,
            .identifier => .variable,
            .string => .string,
            .number => .number,
            .comment => .comment,
            .operator => .operator,
            .punctuation => .operator, // LSP doesn't have punctuation
            .macro => .macro,
            .type_name => .type,
            .function_name => .function,
            .parameter => .parameter,
            .property => .property,
        };
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
        const start_time = std.time.milliTimestamp();

        var diagnostics = std.ArrayList(Diagnostic).init(self.allocator);
        defer diagnostics.deinit();

        // Try Trans-Am first (fast path with caching)
        const used_transam = if (self.transam_db) |*db| blk: {
            const transam_diags = db.getDiagnostics(uri) catch {
                break :blk false;
            };
            defer db.freeDiagnostics(transam_diags);

            for (transam_diags) |td| {
                try diagnostics.append(.{
                    .range = .{
                        .start = .{ .line = td.start_line, .character = td.start_col },
                        .end = .{ .line = td.end_line, .character = td.end_col },
                    },
                    .severity = switch (td.severity) {
                        .@"error" => .@"error",
                        .warning => .warning,
                        .information => .information,
                        .hint => .hint,
                    },
                    .source = "mls",
                    .message = td.message,
                });
            }

            // Run type checker using cached Trans-Am infrastructure
            // (avoids creating new TypeChecker each time)
            if (transam_diags.len == 0) {
                const check_result = db.checkFile(uri) catch {
                    break :blk true;
                };

                // Convert type errors to LSP diagnostics
                for (check_result.errors) |type_err| {
                    try diagnostics.append(.{
                        .range = .{
                            .start = .{
                                .line = if (type_err.location.start.line > 0) type_err.location.start.line - 1 else 0,
                                .character = type_err.location.start.column,
                            },
                            .end = .{
                                .line = if (type_err.location.end.line > 0) type_err.location.end.line - 1 else 0,
                                .character = type_err.location.end.column,
                            },
                        },
                        .severity = .@"error",
                        .source = "mls-typecheck",
                        .message = type_err.message,
                    });
                }
            }

            break :blk true;
        } else false;

        // Fallback to direct lexer if Trans-Am not available
        if (!used_transam) {
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
            for (lexer.errors.items) |err| {
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
        }

        const elapsed_ms = std.time.milliTimestamp() - start_time;
        const source = if (used_transam) "Trans-Am" else "lexer";
        try stderr.print("[mls] Publishing {d} diagnostics for {s} ({s}, {d}ms)\n", .{
            diagnostics.items.len,
            uri,
            source,
            elapsed_ms,
        });

        // Reset cancellation flag after diagnostics complete
        if (self.transam_db) |*db| {
            db.resetAfterCancellation();
        }

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
