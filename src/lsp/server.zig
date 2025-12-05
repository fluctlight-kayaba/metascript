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

// Module loader for cross-module symbol resolution
const module_loader = @import("../module/loader.zig");
const ast = @import("../ast/ast.zig");

// DRC (Deferred Reference Counting) for ownership analysis
const drc_mod = @import("../analysis/drc.zig");
const drc_analyzer = @import("../analysis/drc_analyzer.zig");

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
        .keyword_macro,    // Metascript: macro definition
        .keyword_quote,    // Metascript: quote expression
        .keyword_extern,   // Metascript: extern declaration
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

        // Doc comments
        .doc_comment => .comment,

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
        // @ sign - compiler intrinsic prefix
        .at_sign => "**@** - Compiler intrinsic prefix\n\nUsed with compiler-provided macros (defined via `extern macro @name`).\n\nIntrinsics: `@target`, `@emit`, `@comptime`, `@sizeof`, `@alignof`\n\nNote: User macros are called without `@` prefix.",

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

        // Metascript-specific
        .keyword_macro => "**macro** - Macro declaration\n\nDefines a compile-time macro with zero runtime cost.\n\n```typescript\nmacro function deriveEq(target) {\n    // Compile-time code\n    return target;\n}\n\n// Usage: called like normal function\nderiveEq(User);\n```",
        .keyword_defer => "**defer** - Defer statement\n\nExecutes cleanup code when scope exits.",
        .keyword_distinct => "**distinct** - Distinct type\n\nCreates a nominally distinct type.",
        .keyword_quote => "**quote** - Quote expression\n\nConverts code to AST at compile-time.",
        .keyword_extern => "**extern** - External declaration\n\nDeclares implementation provided externally.\n\n```typescript\nextern function printf(fmt: string): i32;\nextern class FILE;\nextern macro @target(...platforms: string[]): void;\n```",

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
        .object => blk: {
            // Use the class name if available, otherwise "object"
            if (t.data.object.name) |class_name| {
                break :blk try allocator.dupe(u8, class_name);
            }
            break :blk try allocator.dupe(u8, "object");
        },
        .tuple => try allocator.dupe(u8, "tuple"),
        .generic_param => blk: {
            break :blk try allocator.dupe(u8, t.data.generic_param.name);
        },
        .generic_instance => try allocator.dupe(u8, "generic"),
        .intersection => try allocator.dupe(u8, "intersection"),
    };
}

/// Format function signature: name(param1: Type1, param2: Type2): ReturnType
/// Shared by .function, .method, and .macro symbol formatting
fn formatFunctionSignature(
    allocator: std.mem.Allocator,
    buf: *std.ArrayList(u8),
    name: []const u8,
    func: types_mod.FunctionType,
) !void {
    try buf.appendSlice(name);
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
                    try buf.appendSlice("```typescript\nfunction ");
                    try formatFunctionSignature(allocator, &buf, symbol.name, t.data.function.*);
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
                    try buf.appendSlice("```typescript\n(method) ");
                    try formatFunctionSignature(allocator, &buf, symbol.name, t.data.function.*);
                    try buf.appendSlice("\n```");
                } else {
                    try buf.writer().print("```typescript\n(method) {s}\n```", .{symbol.name});
                }
            } else {
                try buf.writer().print("```typescript\n(method) {s}\n```", .{symbol.name});
            }
        },
        .macro => {
            // Macros look like functions but are expanded at compile time
            if (symbol.type) |t| {
                if (t.kind == .function) {
                    try buf.appendSlice("```typescript\n(macro) ");
                    try formatFunctionSignature(allocator, &buf, symbol.name, t.data.function.*);
                    try buf.appendSlice("\n```\n\n*Expanded at compile time*");
                } else {
                    try buf.writer().print("```typescript\n(macro) {s}\n```\n\n*Expanded at compile time*", .{symbol.name});
                }
            } else {
                try buf.writer().print("```typescript\n(macro) {s}\n```\n\n*Expanded at compile time*", .{symbol.name});
            }
        },
    }

    // Append JSDoc comment if present (with proper tag formatting)
    if (symbol.doc_comment) |doc| {
        try buf.appendSlice("\n\n---\n\n");
        // Format the JSDoc with proper tag parsing (@param, @returns, @deprecated)
        const formatted_doc = try formatJsDocForHover(allocator, doc);
        defer allocator.free(formatted_doc);
        try buf.appendSlice(formatted_doc);
    }

    return buf.toOwnedSlice();
}

/// Clean up JSDoc comment for display
/// Removes /** and */ wrappers, cleans up leading asterisks, and formats tags
fn cleanJsDoc(doc: []const u8) []const u8 {
    var start: usize = 0;
    var end: usize = doc.len;

    // Skip leading whitespace and /**
    while (start < doc.len and (doc[start] == ' ' or doc[start] == '\t' or doc[start] == '\n' or doc[start] == '\r')) {
        start += 1;
    }
    if (start + 3 <= doc.len and std.mem.eql(u8, doc[start .. start + 3], "/**")) {
        start += 3;
    }

    // Skip trailing whitespace and */
    while (end > start and (doc[end - 1] == ' ' or doc[end - 1] == '\t' or doc[end - 1] == '\n' or doc[end - 1] == '\r')) {
        end -= 1;
    }
    if (end >= 2 and end > start and std.mem.eql(u8, doc[end - 2 .. end], "*/")) {
        end -= 2;
    }

    // Skip any remaining whitespace and leading asterisks
    while (start < end and (doc[start] == ' ' or doc[start] == '\t' or doc[start] == '\n' or doc[start] == '\r' or doc[start] == '*')) {
        start += 1;
    }
    while (end > start and (doc[end - 1] == ' ' or doc[end - 1] == '\t' or doc[end - 1] == '\n' or doc[end - 1] == '\r' or doc[end - 1] == '*')) {
        end -= 1;
    }

    if (start >= end) return "";
    return doc[start..end];
}

/// Format JSDoc comment for hover display with nice formatting
/// Parses @param, @returns, @deprecated tags and formats them nicely
/// Preserves line structure for lists and multi-line descriptions
fn formatJsDocForHover(allocator: std.mem.Allocator, doc: []const u8) ![]const u8 {
    var buf = std.ArrayList(u8).init(allocator);
    errdefer buf.deinit();

    // First, clean the doc comment (remove /** and */)
    const cleaned = cleanJsDoc(doc);
    if (cleaned.len == 0) return buf.toOwnedSlice();

    var description_lines = std.ArrayList(u8).init(allocator);
    defer description_lines.deinit();

    var params = std.ArrayList([]const u8).init(allocator);
    defer params.deinit();

    var examples = std.ArrayList([]const u8).init(allocator);
    defer examples.deinit();

    var returns: ?[]const u8 = null;
    var deprecated: ?[]const u8 = null;
    var in_example = false;

    // Split by lines and process
    var lines = std.mem.splitScalar(u8, cleaned, '\n');
    while (lines.next()) |raw_line| {
        // Clean leading asterisks and whitespace from each line
        var line = raw_line;
        while (line.len > 0 and (line[0] == ' ' or line[0] == '\t' or line[0] == '*')) {
            line = line[1..];
        }
        // Trim trailing whitespace
        while (line.len > 0 and (line[line.len - 1] == ' ' or line[line.len - 1] == '\t' or line[line.len - 1] == '\r')) {
            line = line[0 .. line.len - 1];
        }

        // Handle @example blocks (can be multi-line)
        if (std.mem.startsWith(u8, line, "@example")) {
            in_example = true;
            // Capture example content after @example tag
            const example_content = line["@example".len..];
            var trimmed = example_content;
            while (trimmed.len > 0 and trimmed[0] == ' ') trimmed = trimmed[1..];
            if (trimmed.len > 0) {
                try examples.append(trimmed);
            }
            continue;
        }

        // If we hit another @ tag, we're out of example block
        if (line.len > 0 and line[0] == '@') {
            in_example = false;
        }

        // If in example block, collect non-empty example lines
        if (in_example) {
            if (line.len > 0) {
                try examples.append(line);
            }
            continue;
        }

        // Empty line = paragraph break (preserve as double newline)
        if (line.len == 0) {
            if (description_lines.items.len > 0) {
                // Add paragraph break (double newline for markdown)
                try description_lines.appendSlice("\n\n");
            }
            continue;
        }

        // Check for JSDoc tags
        if (std.mem.startsWith(u8, line, "@param")) {
            try params.append(line);
        } else if (std.mem.startsWith(u8, line, "@returns") or std.mem.startsWith(u8, line, "@return")) {
            returns = line;
        } else if (std.mem.startsWith(u8, line, "@deprecated")) {
            deprecated = line;
        } else if (line[0] == '@') {
            // Other tags - include in description with line break
            if (description_lines.items.len > 0) {
                try description_lines.append('\n');
            }
            try description_lines.appendSlice(line);
        } else {
            // Regular description line - preserve structure for lists and line breaks
            // Check if this looks like a list item (starts with - or * followed by space)
            const is_list_item = (line.len > 1 and line[0] == '-' and line[1] == ' ') or
                (line.len > 2 and line[0] == '*' and line[1] == ' ');

            if (description_lines.items.len > 0) {
                const items = description_lines.items;
                const last_char = items[items.len - 1];

                // Don't add anything if previous was paragraph break (ends with newline)
                if (last_char == '\n') {
                    // Already have newline from paragraph break, don't add more
                } else if (is_list_item) {
                    // List items get their own line
                    try description_lines.append('\n');
                } else {
                    // Regular text continuation - join with space
                    try description_lines.append(' ');
                }
            }
            try description_lines.appendSlice(line);
        }
    }

    // Format output
    // 1. Description
    if (description_lines.items.len > 0) {
        try buf.appendSlice(description_lines.items);
    }

    // 2. Deprecated warning (prominent)
    if (deprecated) |dep| {
        if (buf.items.len > 0) try buf.appendSlice("\n\n");
        try buf.appendSlice("**");
        if (dep.len > "@deprecated".len) {
            try buf.appendSlice(dep);
        } else {
            try buf.appendSlice("@deprecated");
        }
        try buf.appendSlice("**");
    }

    // 3. Parameters
    if (params.items.len > 0) {
        if (buf.items.len > 0) try buf.appendSlice("\n\n");
        try buf.appendSlice("**Parameters:**\n");
        for (params.items) |param| {
            // Extract param name (skip @param and optional {type})
            var param_rest = param["@param".len..];
            while (param_rest.len > 0 and param_rest[0] == ' ') param_rest = param_rest[1..];

            var param_type: ?[]const u8 = null;

            // Check for {type} notation
            if (param_rest.len > 0 and param_rest[0] == '{') {
                if (std.mem.indexOfScalar(u8, param_rest, '}')) |close_idx| {
                    param_type = param_rest[1..close_idx];
                    param_rest = param_rest[close_idx + 1 ..];
                    while (param_rest.len > 0 and param_rest[0] == ' ') param_rest = param_rest[1..];
                }
            }

            // Get param name
            var name_end: usize = 0;
            while (name_end < param_rest.len and param_rest[name_end] != ' ' and param_rest[name_end] != '\t') {
                name_end += 1;
            }
            if (name_end > 0) {
                try buf.appendSlice("- `");
                try buf.appendSlice(param_rest[0..name_end]);
                try buf.append('`');
                // Add type if present
                if (param_type) |ptype| {
                    try buf.appendSlice(" (`");
                    try buf.appendSlice(ptype);
                    try buf.appendSlice("`)");
                }
                if (name_end < param_rest.len) {
                    // Description
                    var desc = param_rest[name_end..];
                    while (desc.len > 0 and (desc[0] == ' ' or desc[0] == '-')) desc = desc[1..];
                    if (desc.len > 0) {
                        try buf.appendSlice(" - ");
                        try buf.appendSlice(desc);
                    }
                }
            } else {
                try buf.appendSlice("- ");
                try buf.appendSlice(param_rest);
            }
            try buf.append('\n');
        }
    }

    // 4. Returns
    if (returns) |ret| {
        if (buf.items.len > 0) try buf.appendSlice("\n");
        try buf.appendSlice("**Returns:** ");
        var ret_rest = ret;
        if (std.mem.startsWith(u8, ret_rest, "@returns")) {
            ret_rest = ret_rest["@returns".len..];
        } else if (std.mem.startsWith(u8, ret_rest, "@return")) {
            ret_rest = ret_rest["@return".len..];
        }
        while (ret_rest.len > 0 and ret_rest[0] == ' ') ret_rest = ret_rest[1..];

        // Check for {type}
        if (ret_rest.len > 0 and ret_rest[0] == '{') {
            if (std.mem.indexOfScalar(u8, ret_rest, '}')) |close_idx| {
                try buf.appendSlice("`");
                try buf.appendSlice(ret_rest[1..close_idx]);
                try buf.appendSlice("` ");
                ret_rest = ret_rest[close_idx + 1 ..];
                while (ret_rest.len > 0 and ret_rest[0] == ' ') ret_rest = ret_rest[1..];
            }
        }
        try buf.appendSlice(ret_rest);
    }

    // 5. Examples - code block with syntax highlighting
    if (examples.items.len > 0) {
        if (buf.items.len > 0) try buf.appendSlice("\n\n");
        try buf.appendSlice("```typescript\n");
        for (examples.items) |example_line| {
            try buf.appendSlice(example_line);
            try buf.append('\n');
        }
        try buf.appendSlice("```");
    }

    return buf.toOwnedSlice();
}

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

    // Module loader for cross-module symbol resolution (go-to-definition, hover)
    module_arena: ?*ast.ASTArena = null,
    loader: ?*module_loader.ModuleLoader = null,

    // DRC cache - maps file URI to DRC analysis results
    // Used for ownership diagnostics and hover information
    drc_cache: std.StringHashMap(*drc_mod.Drc) = undefined,
    drc_cache_initialized: bool = false,

    pub fn init(allocator: std.mem.Allocator) Server {
        return .{
            .allocator = allocator,
            .documents = file_store.FileStore.init(allocator),
            .transam_db = null,
            .background_worker = null,
            .module_arena = null,
            .loader = null,
            .drc_cache = std.StringHashMap(*drc_mod.Drc).init(allocator),
            .drc_cache_initialized = true,
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

    /// Initialize ModuleLoader for cross-module symbol resolution
    pub fn initModuleLoader(self: *Server) !void {
        if (self.loader != null) return;

        // Create arena for module ASTs
        const arena = try self.allocator.create(ast.ASTArena);
        arena.* = ast.ASTArena.init(self.allocator);
        self.module_arena = arena;

        // Create module loader
        const loader_ptr = try self.allocator.create(module_loader.ModuleLoader);
        loader_ptr.* = module_loader.ModuleLoader.init(self.allocator, arena) catch |err| {
            std.log.err("[mls] Failed to init ModuleLoader: {}", .{err});
            self.allocator.destroy(arena);
            self.module_arena = null;
            return err;
        };
        self.loader = loader_ptr;

        // Load standard library macros
        loader_ptr.loadStdMacros() catch |err| {
            std.log.warn("[mls] Failed to load std macros: {}", .{err});
        };

        std.log.info("[mls] ModuleLoader initialized", .{});
    }

    /// Load a module file into the module loader (for cross-module resolution)
    /// Also loads all imported dependencies transitively and records in Trans-Am
    pub fn loadModuleFile(self: *Server, file_path: []const u8) void {
        const loader_ptr = self.loader orelse return;

        // Load the main module
        const module = loader_ptr.loadModule(file_path) catch |err| {
            std.log.debug("[mls] Could not load module {s}: {}", .{ file_path, err });
            return;
        };

        // Load all imported dependencies and record in Trans-Am for invalidation tracking
        for (module.imports.items) |import_decl| {
            if (import_decl.resolved_path) |resolved| {
                // Record the import relationship in Trans-Am
                if (self.transam_db) |*db| {
                    db.recordModuleImport(file_path, resolved) catch {};
                }

                // Load the dependency if not already loaded
                if (!loader_ptr.modules.contains(resolved)) {
                    _ = loader_ptr.loadModule(resolved) catch |err| {
                        std.log.debug("[mls] Could not load dependency {s}: {}", .{ resolved, err });
                    };
                }
            }
        }
    }

    /// Ensure an imported module is loaded (on-demand loading for lookups)
    fn ensureImportedModuleLoaded(self: *Server, resolved_path: []const u8) ?*module_loader.Module {
        const loader_ptr = self.loader orelse return null;

        // Check if already loaded
        if (loader_ptr.modules.get(resolved_path)) |module| {
            return module;
        }

        // Try to load it on-demand
        const module = loader_ptr.loadModule(resolved_path) catch |err| {
            std.log.debug("[mls] On-demand load failed for {s}: {}", .{ resolved_path, err });
            return null;
        };

        return module;
    }

    /// Reload a module after it has been modified
    /// Uses in-memory content from Trans-Am (editor buffer) instead of disk
    fn reloadModule(self: *Server, file_path: []const u8) void {
        const loader_ptr = self.loader orelse return;

        // Try to get content from Trans-Am (editor's in-memory buffer)
        // This is critical: the editor may have unsaved changes
        const source = if (self.transam_db) |*db|
            db.getFileText(file_path)
        else
            null;

        const module = if (source) |content| blk: {
            // Use in-memory content (preferred - matches what user sees in editor)
            std.log.debug("[mls] Reloading module from editor buffer: {s}", .{file_path});
            break :blk loader_ptr.loadModuleFromSource(file_path, content) catch |err| {
                std.log.debug("[mls] Failed to reload module from source {s}: {}", .{ file_path, err });
                return;
            };
        } else blk: {
            // Fallback to disk (for initial loads or when Trans-Am not available)
            std.log.debug("[mls] Reloading module from disk: {s}", .{file_path});
            break :blk loader_ptr.loadModule(file_path) catch |err| {
                std.log.debug("[mls] Failed to reload module from disk {s}: {}", .{ file_path, err });
                return;
            };
        };

        // Re-record imports in Trans-Am
        for (module.imports.items) |import_decl| {
            if (import_decl.resolved_path) |resolved| {
                if (self.transam_db) |*db| {
                    db.recordModuleImport(file_path, resolved) catch {};
                }
            }
        }

        std.log.debug("[mls] Reloaded module: {s}", .{file_path});
    }

    /// Result of looking up a symbol in imported modules
    const ImportedSymbolLookup = struct {
        exported: module_loader.Module.ExportedSymbol,
        source_path: []const u8, // The module path where the symbol is defined
    };

    /// Look up a symbol in imported modules
    /// Returns both the exported symbol info and the source module path
    fn lookupImportedSymbol(
        self: *Server,
        loader_ptr: *module_loader.ModuleLoader,
        current_path: []const u8,
        symbol_name: []const u8,
    ) ?ImportedSymbolLookup {
        // Get the current module to find its imports
        const current_module = loader_ptr.modules.get(current_path) orelse return null;

        // Check each import declaration for the symbol
        for (current_module.imports.items) |import_decl| {
            // Get the resolved path for this import
            const resolved = import_decl.resolved_path orelse continue;

            // Get the imported module (load on-demand if needed)
            const imported_module = loader_ptr.modules.get(resolved) orelse
                self.ensureImportedModuleLoaded(resolved) orelse continue;

            // Check if it exports the symbol
            if (imported_module.exports.get(symbol_name)) |exported| {
                return .{
                    .exported = exported,
                    .source_path = resolved,
                };
            }
        }

        return null;
    }

    /// Get symbol info from an imported module (convenience wrapper)
    pub fn getImportedSymbol(self: *Server, current_file: []const u8, symbol_name: []const u8) ?module_loader.Module.ExportedSymbol {
        const loader_ptr = self.loader orelse return null;
        const lookup = self.lookupImportedSymbol(loader_ptr, current_file, symbol_name) orelse return null;
        return lookup.exported;
    }

    pub fn deinit(self: *Server) void {
        // Clean up background worker first (before Trans-Am)
        if (self.background_worker) |*bw| {
            bw.deinit();
        }
        if (self.transam_db) |*db| {
            db.deinit();
        }
        // Clean up module loader
        if (self.loader) |loader_ptr| {
            loader_ptr.deinit();
            self.allocator.destroy(loader_ptr);
        }
        if (self.module_arena) |arena| {
            arena.deinit();
            self.allocator.destroy(arena);
        }
        // Clean up DRC cache
        if (self.drc_cache_initialized) {
            var iter = self.drc_cache.valueIterator();
            while (iter.next()) |drc_ptr| {
                drc_ptr.*.deinit();
                self.allocator.destroy(drc_ptr.*);
            }
            self.drc_cache.deinit();
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
            // Initialize ModuleLoader for cross-module symbol resolution
            self.initModuleLoader() catch |err| {
                try stderr.print("[mls] Warning: Failed to init ModuleLoader: {}\n", .{err});
            };
            try stderr.writeAll("[mls] Client initialized (Trans-Am + ModuleLoader ready)\n");
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

        // Convert URI to file path FIRST for consistent keying across Trans-Am and ModuleLoader
        // CRITICAL: Both handleDidOpen and handleDidChange must use file_path (not URI) for Trans-Am
        // to ensure reloadModule can find content via getFileText(file_path)
        const file_path = uriToPath(uri.string) orelse {
            try stderr.print("[mls] Warning: Could not convert URI to path: {s}\n", .{uri.string});
            // Still store in documents for basic LSP functionality
            _ = try self.documents.set(uri.string, text.string, @intCast(version.integer));
            try self.runDiagnostics(uri.string, stdout, stderr);
            return;
        };

        _ = try self.documents.set(
            uri.string,
            text.string,
            @intCast(version.integer),
        );

        // Feed into Trans-Am for incremental computation
        // Use file_path (not uri.string) to match handleDidChange and reloadModule
        if (self.transam_db) |*db| {
            const changed = db.setFileText(file_path, text.string) catch false;
            if (changed) {
                try stderr.print("[mls] Trans-Am: file registered (revision {d})\n", .{db.getRevision().value});
            }
        }

        // Load module into ModuleLoader for cross-module resolution
        if (self.loader != null) {
            self.loadModuleFile(file_path);
            try stderr.print("[mls] ModuleLoader: loaded {s}\n", .{file_path});
        }

        // Queue background pre-warming for faster subsequent requests
        if (self.background_worker) |*bw| {
            const file_hash = transam.hashString(file_path);
            bw.queue(.prewarm, .open, file_hash, file_path) catch {};
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

        // Convert URI to file path for consistent Trans-Am tracking
        // C5 fix: Don't fallback to URI - skip Trans-Am ops if conversion fails
        const file_path = uriToPath(uri.string) orelse {
            try stderr.print("[mls] Warning: Could not convert URI to path: {s}\n", .{uri.string});
            // Still run diagnostics but skip module tracking
            try self.runDiagnostics(uri.string, stdout, stderr);
            return;
        };

        // Feed updated content into Trans-Am for incremental recomputation
        var content_changed = false;
        if (self.transam_db) |*db| {
            if (entry.text) |current_text| {
                content_changed = db.setFileText(file_path, current_text) catch false;
                if (content_changed) {
                    try stderr.print("[mls] Trans-Am: revision {d}\n", .{db.getRevision().value});

                    // Invalidate modules that depend on this file
                    const invalidated = db.invalidateDependents(file_path) catch 0;
                    if (invalidated > 0) {
                        try stderr.print("[mls] Invalidated {d} dependent module(s)\n", .{invalidated});
                    }

                    // Clear old imports before reloading
                    db.clearModuleImports(file_path);
                }
            }
        }

        // C6 fix: Only reload module when content actually changed
        if (content_changed) {
            if (self.loader != null) {
                self.reloadModule(file_path);
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

        // Convert URI to file path for Trans-Am queries (must match setFileText key)
        const file_path = uriToPath(uri.string);

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
            // Macros use the same code path as regular functions/identifiers
            // They are looked up via Trans-Am symbol table like any other symbol
            const hover_content = getHoverContent(tok.kind, tok.text);
            if (hover_content) |content| {
                const elapsed_ms = std.time.milliTimestamp() - start_time;
                try stderr.print("[mls] Hover: '{s}' ({d}ms)\n", .{ tok.text, elapsed_ms });
                try self.sendHoverResponse(id, content, tok, stdout, stderr);
                return;
            }

            // For identifiers, try to look up type information via Trans-Am
            if (tok.kind == .identifier) {
                // First, check if this is a macro invocation (@target, @emit, etc.)
                const macro_hover = self.getMacroHover(text, tok) catch null;
                if (macro_hover) |content| {
                    defer self.allocator.free(content);
                    const elapsed_ms = std.time.milliTimestamp() - start_time;
                    try stderr.print("[mls] Hover: macro @{s} ({d}ms)\n", .{ tok.text, elapsed_ms });
                    try self.sendHoverResponse(id, content, tok, stdout, stderr);
                    return;
                }

                // Second, check if this is an extern macro declaration (extern macro target)
                const extern_macro_hover = self.getExternMacroDeclarationHover(text, tok) catch null;
                if (extern_macro_hover) |content| {
                    defer self.allocator.free(content);
                    const elapsed_ms = std.time.milliTimestamp() - start_time;
                    try stderr.print("[mls] Hover: extern macro {s} ({d}ms)\n", .{ tok.text, elapsed_ms });
                    try self.sendHoverResponse(id, content, tok, stdout, stderr);
                    return;
                }

                // Third, check if this is a member expression property (e.g., "floor" in "Math.floor")
                // Re-scan to find if there's a dot before this identifier
                const member_hover = self.getMemberExpressionHover(text, tok, line) catch null;
                if (member_hover) |content| {
                    defer self.allocator.free(content);
                    const elapsed_ms = std.time.milliTimestamp() - start_time;
                    try stderr.print("[mls] Hover: member property '{s}' ({d}ms)\n", .{ tok.text, elapsed_ms });
                    try self.sendHoverResponse(id, content, tok, stdout, stderr);
                    return;
                }

                // Try Trans-Am symbol lookup (use file_path to match setFileText key)
                if (self.transam_db) |*db| {
                    if (file_path) |path| {
                        // Look up the symbol at the hover position (position-aware for shadowing)
                        // This ensures we find the correct symbol when multiple variables share the same name
                        // Note: LSP uses 0-based lines, our parser uses 1-based, so add 1 to convert
                        const parser_line = line + 1;
                        const symbol_result = db.lookupSymbolAtPosition(path, tok.text, parser_line, character) catch null;
                        if (symbol_result) |symbol| {
                            const hover_text = formatSymbolHover(self.allocator, symbol) catch null;
                            if (hover_text) |content| {
                                // Append ownership info for variables (DRC analysis)
                                var final_content: []const u8 = content;
                                defer self.allocator.free(content);

                                if (symbol.kind == .variable or symbol.kind == .parameter) {
                                    if (self.getOwnershipHoverInfo(uri.string, tok.text)) |ownership_info| {
                                        defer self.allocator.free(ownership_info);
                                        // Combine hover content with ownership info
                                        var combined = std.ArrayList(u8).init(self.allocator);
                                        combined.appendSlice(content) catch {};
                                        combined.appendSlice("\n\n---\n\n") catch {};
                                        combined.appendSlice(ownership_info) catch {};
                                        final_content = combined.toOwnedSlice() catch content;
                                    }
                                }

                                const elapsed_ms = std.time.milliTimestamp() - start_time;
                                try stderr.print("[mls] Hover: type info for '{s}' ({d}ms)\n", .{ tok.text, elapsed_ms });
                                try self.sendHoverResponse(id, final_content, tok, stdout, stderr);
                                return;
                            }
                        }
                    }
                }

                // Try cross-module lookup via ModuleLoader
                if (self.loader) |loader_ptr| {
                    if (file_path) |current_path| {
                        const imported_hover = self.getImportedSymbolHover(loader_ptr, current_path, tok.text) catch null;
                        if (imported_hover) |imported_content| {
                            defer self.allocator.free(imported_content);
                            const elapsed_ms = std.time.milliTimestamp() - start_time;
                            try stderr.print("[mls] Hover: cross-module '{s}' ({d}ms)\n", .{ tok.text, elapsed_ms });
                            try self.sendHoverResponse(id, imported_content, tok, stdout, stderr);
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

    /// Check if token is a property in a member expression (e.g., "floor" in "Math.floor")
    /// and return hover content for global builtins like Math.*
    fn getMemberExpressionHover(
        self: *Server,
        text: []const u8,
        tok: token_mod.Token,
        hover_line: u32,
    ) !?[]const u8 {
        _ = hover_line;

        // Re-lex to find the pattern: <identifier> . <current_token>
        var lexer = try lexer_mod.Lexer.init(self.allocator, text, 1);
        defer lexer.deinit();

        var prev_prev_tok: ?token_mod.Token = null;
        var prev_tok: ?token_mod.Token = null;

        while (true) {
            const current = lexer.next() catch break;
            if (current.kind == .end_of_file) break;

            // Check if current token matches the one we're hovering over
            if (current.kind == .identifier and
                current.loc.start.line == tok.loc.start.line and
                current.loc.start.column == tok.loc.start.column)
            {
                // Check if pattern is: identifier . identifier
                if (prev_tok != null and prev_tok.?.kind == .dot and
                    prev_prev_tok != null and prev_prev_tok.?.kind == .identifier)
                {
                    const object_name = prev_prev_tok.?.text;
                    const property_name = current.text;

                    // Handle Math.* builtins
                    if (std.mem.eql(u8, object_name, "Math")) {
                        return try self.getMathPropertyHover(property_name);
                    }

                    // Handle console.* builtins
                    if (std.mem.eql(u8, object_name, "console")) {
                        return try self.getConsolePropertyHover(property_name);
                    }
                }
                break;
            }

            prev_prev_tok = prev_tok;
            prev_tok = current;
        }

        return null;
    }

    /// Get hover content for Math.* properties and methods
    fn getMathPropertyHover(self: *Server, property: []const u8) !?[]const u8 {
        // Math methods - single arg
        const single_arg_methods = [_]struct { name: []const u8, desc: []const u8 }{
            .{ .name = "floor", .desc = "Returns the largest integer less than or equal to x" },
            .{ .name = "ceil", .desc = "Returns the smallest integer greater than or equal to x" },
            .{ .name = "round", .desc = "Returns the value of x rounded to the nearest integer" },
            .{ .name = "trunc", .desc = "Returns the integer part of x by removing any fractional digits" },
            .{ .name = "sqrt", .desc = "Returns the square root of x" },
            .{ .name = "cbrt", .desc = "Returns the cube root of x" },
            .{ .name = "abs", .desc = "Returns the absolute value of x" },
            .{ .name = "sign", .desc = "Returns the sign of x (-1, 0, or 1)" },
            .{ .name = "sin", .desc = "Returns the sine of x (x in radians)" },
            .{ .name = "cos", .desc = "Returns the cosine of x (x in radians)" },
            .{ .name = "tan", .desc = "Returns the tangent of x (x in radians)" },
            .{ .name = "asin", .desc = "Returns the arcsine of x in radians" },
            .{ .name = "acos", .desc = "Returns the arccosine of x in radians" },
            .{ .name = "atan", .desc = "Returns the arctangent of x in radians" },
            .{ .name = "sinh", .desc = "Returns the hyperbolic sine of x" },
            .{ .name = "cosh", .desc = "Returns the hyperbolic cosine of x" },
            .{ .name = "tanh", .desc = "Returns the hyperbolic tangent of x" },
            .{ .name = "exp", .desc = "Returns e^x" },
            .{ .name = "expm1", .desc = "Returns e^x - 1" },
            .{ .name = "log", .desc = "Returns the natural logarithm of x" },
            .{ .name = "log10", .desc = "Returns the base-10 logarithm of x" },
            .{ .name = "log2", .desc = "Returns the base-2 logarithm of x" },
            .{ .name = "log1p", .desc = "Returns the natural log of (1 + x)" },
        };

        for (single_arg_methods) |method| {
            if (std.mem.eql(u8, property, method.name)) {
                return try std.fmt.allocPrint(
                    self.allocator,
                    "```typescript\nMath.{s}(x: number): number\n```\n{s}",
                    .{ method.name, method.desc },
                );
            }
        }

        // Math methods - two args
        const two_arg_methods = [_]struct { name: []const u8, desc: []const u8 }{
            .{ .name = "pow", .desc = "Returns x raised to the power y" },
            .{ .name = "atan2", .desc = "Returns the arctangent of y/x in radians" },
            .{ .name = "min", .desc = "Returns the smaller of x and y" },
            .{ .name = "max", .desc = "Returns the larger of x and y" },
            .{ .name = "hypot", .desc = "Returns the square root of the sum of squares of x and y" },
            .{ .name = "imul", .desc = "Returns the result of the C-like 32-bit multiplication of x and y" },
        };

        for (two_arg_methods) |method| {
            if (std.mem.eql(u8, property, method.name)) {
                return try std.fmt.allocPrint(
                    self.allocator,
                    "```typescript\nMath.{s}(x: number, y: number): number\n```\n{s}",
                    .{ method.name, method.desc },
                );
            }
        }

        // Math.random()
        if (std.mem.eql(u8, property, "random")) {
            return try std.fmt.allocPrint(
                self.allocator,
                "```typescript\nMath.random(): number\n```\nReturns a pseudo-random number between 0 (inclusive) and 1 (exclusive)",
                .{},
            );
        }

        // Math constants
        const constants = [_]struct { name: []const u8, value: []const u8, desc: []const u8 }{
            .{ .name = "PI", .value = "3.141592653589793", .desc = "The ratio of a circle's circumference to its diameter" },
            .{ .name = "E", .value = "2.718281828459045", .desc = "Euler's number, the base of natural logarithms" },
            .{ .name = "LN2", .value = "0.6931471805599453", .desc = "The natural logarithm of 2" },
            .{ .name = "LN10", .value = "2.302585092994046", .desc = "The natural logarithm of 10" },
            .{ .name = "LOG2E", .value = "1.4426950408889634", .desc = "The base-2 logarithm of E" },
            .{ .name = "LOG10E", .value = "0.4342944819032518", .desc = "The base-10 logarithm of E" },
            .{ .name = "SQRT2", .value = "1.4142135623730951", .desc = "The square root of 2" },
            .{ .name = "SQRT1_2", .value = "0.7071067811865476", .desc = "The square root of 1/2" },
        };

        for (constants) |constant| {
            if (std.mem.eql(u8, property, constant.name)) {
                return try std.fmt.allocPrint(
                    self.allocator,
                    "```typescript\nMath.{s}: number\n```\n{s}\n\nValue: `{s}`",
                    .{ constant.name, constant.desc, constant.value },
                );
            }
        }

        return null;
    }

    /// Get hover content for console.* methods
    fn getConsolePropertyHover(self: *Server, property: []const u8) !?[]const u8 {
        const methods = [_]struct { name: []const u8, desc: []const u8 }{
            .{ .name = "log", .desc = "Outputs a message to the console" },
            .{ .name = "error", .desc = "Outputs an error message to the console" },
            .{ .name = "warn", .desc = "Outputs a warning message to the console" },
        };

        for (methods) |method| {
            if (std.mem.eql(u8, property, method.name)) {
                return try std.fmt.allocPrint(
                    self.allocator,
                    "```typescript\nconsole.{s}(...args: any[]): void\n```\n{s}",
                    .{ method.name, method.desc },
                );
            }
        }

        return null;
    }

    /// Get hover content for built-in compiler macros (extern macros)
    /// These are defined in std/macros/compiler.ms but implemented by the compiler
    fn getBuiltinMacroHover(self: *Server, macro_name: []const u8) !?[]const u8 {
        const macros = [_]struct { name: []const u8, signature: []const u8, desc: []const u8 }{
            .{
                .name = "target",
                .signature = "@target(...platforms: (\"c\" | \"js\" | \"erlang\")[])",
                .desc = "Conditional compilation for target backends.\n\nCode inside block only compiled for specified target(s).\nSupports `else` clause for fallback code.\n\n```typescript\n@target(\"c\") {\n    @emit(\"printf($msg)\")\n} else @target(\"js\") {\n    console.log(msg);\n} else {\n    // Fallback\n}\n```",
            },
            .{
                .name = "emit",
                .signature = "@emit(code: string): void\n@emit<T>(code: string): T",
                .desc = "Emit raw backend code.\n\nBypasses AST and emits literal code string to the backend.\nUse `$var` syntax for variable interpolation.\n\n```typescript\n@emit(\"printf(\\\"%s\\\\n\\\", $msg);\")  // C\n@emit(\"console.log($x)\")           // JS\n@emit<boolean>(\"confirm($msg)\")    // Typed return\n```",
            },
            .{
                .name = "comptime",
                .signature = "@comptime<T>(block: () => T): T",
                .desc = "Compile-time execution block.\n\nCode runs during compilation; result embedded as constant.\n\n```typescript\nconst version = @comptime {\n    return readFile(\"VERSION\").trim();\n};\n```",
            },
            .{
                .name = "inline",
                .signature = "@inline",
                .desc = "Force inline expansion at call sites.\n\nMarks a function for inline expansion.\n\n```typescript\n@inline\nfunction add(a: number, b: number): number {\n    return a + b;\n}\n```",
            },
            .{
                .name = "sizeof",
                .signature = "@sizeof<T>(): usize",
                .desc = "Get size of type in bytes.\n\nReturns the size of the type in the target backend.\n\n```typescript\nconst intSize = @sizeof<i32>();   // 4\nconst ptrSize = @sizeof<*void>(); // 8 on 64-bit\n```",
            },
            .{
                .name = "alignof",
                .signature = "@alignof<T>(): usize",
                .desc = "Get alignment of type in bytes.\n\nReturns the alignment requirement of the type.\n\n```typescript\nconst intAlign = @alignof<i32>();    // 4\nconst doubleAlign = @alignof<f64>(); // 8\n```",
            },
            .{
                .name = "typeinfo",
                .signature = "@typeinfo<T>(): TypeInfo",
                .desc = "Get type information at compile time.\n\nReturns structural information about a type.\n\n```typescript\nconst info = @typeinfo<User>();\nfor (const field of info.fields) {\n    console.log(field.name, field.type);\n}\n```",
            },
            .{
                .name = "currentTarget",
                .signature = "@currentTarget(): \"c\" | \"js\" | \"erlang\"",
                .desc = "Get current compilation target.\n\nReturns the backend being compiled for.\n\n```typescript\nif (@currentTarget() === \"js\") {\n    // Browser-specific code\n}\n```",
            },
            // FFI decorator macros (used with extern declarations)
            .{
                .name = "native",
                .signature = "@native(name: string)",
                .desc = "Map to different name in target backend.\n\n```typescript\n@native(\"__builtin_popcount\")\nextern function popcount(x: u32): u32;\n```",
            },
            .{
                .name = "library",
                .signature = "@library(name: string)",
                .desc = "Specify library to link against.\n\n```typescript\n@library(\"libcurl\")\nextern function curl_easy_init(): *CurlHandle;\n```",
            },
            .{
                .name = "include",
                .signature = "@include(header: string)",
                .desc = "Include C header file.\n\n```typescript\n@include(\"sys/stat.h\")\nextern class stat_t { ... }\n```",
            },
        };

        for (macros) |macro| {
            if (std.mem.eql(u8, macro_name, macro.name)) {
                return try std.fmt.allocPrint(
                    self.allocator,
                    "```typescript\n{s}\n```\n{s}",
                    .{ macro.signature, macro.desc },
                );
            }
        }

        return null;
    }

    /// Get hover content for a symbol imported from another module
    fn getImportedSymbolHover(
        self: *Server,
        loader_ptr: *module_loader.ModuleLoader,
        current_path: []const u8,
        symbol_name: []const u8,
    ) !?[]const u8 {
        // Use common lookup helper
        const lookup = self.lookupImportedSymbol(loader_ptr, current_path, symbol_name) orelse return null;

        // Build hover content based on the symbol kind
        const kind_str = switch (lookup.exported.kind) {
            .macro => "macro",
            .function => "function",
            .class => "class",
            .variable => "const",
        };

        // Get the source file basename
        const source_file = std.fs.path.basename(lookup.source_path);

        return try std.fmt.allocPrint(
            self.allocator,
            "```typescript\n{s} {s}\n```\n*Imported from `{s}`*",
            .{ kind_str, symbol_name, source_file },
        );
    }

    /// Check if identifier is preceded by @ (macro invocation)
    fn getMacroHover(
        self: *Server,
        text: []const u8,
        tok: token_mod.Token,
    ) !?[]const u8 {
        // Re-lex to find if there's an @ before this identifier
        var lexer = try lexer_mod.Lexer.init(self.allocator, text, 1);
        defer lexer.deinit();

        var prev_tok: ?token_mod.Token = null;

        while (true) {
            const current = lexer.next() catch break;
            if (current.kind == .end_of_file) break;

            // Check if current token matches the one we're hovering over
            if (current.kind == .identifier and
                current.loc.start.line == tok.loc.start.line and
                current.loc.start.column == tok.loc.start.column)
            {
                // Check if preceded by @
                if (prev_tok != null and prev_tok.?.kind == .at_sign) {
                    // This is a macro invocation - look up documentation
                    return try self.getBuiltinMacroHover(current.text);
                }
                break;
            }

            prev_tok = current;
        }

        return null;
    }

    /// Check if identifier is part of an extern macro declaration and return JSDoc
    fn getExternMacroDeclarationHover(
        self: *Server,
        text: []const u8,
        tok: token_mod.Token,
    ) !?[]const u8 {
        // Re-lex to find if this is: extern macro <name>
        var lexer = try lexer_mod.Lexer.init(self.allocator, text, 1);
        defer lexer.deinit();

        var prev_prev_tok: ?token_mod.Token = null;
        var prev_tok: ?token_mod.Token = null;
        var last_doc_comment: ?[]const u8 = null;

        while (true) {
            const current = lexer.next() catch break;
            if (current.kind == .end_of_file) break;

            // Track doc comments
            if (current.kind == .doc_comment) {
                last_doc_comment = current.text;
            }

            // Check if current token matches the one we're hovering over
            if (current.kind == .identifier and
                current.loc.start.line == tok.loc.start.line and
                current.loc.start.column == tok.loc.start.column)
            {
                // Check if preceded by: extern macro
                if (prev_tok != null and prev_tok.?.kind == .keyword_macro and
                    prev_prev_tok != null and prev_prev_tok.?.kind == .keyword_extern)
                {
                    // This is an extern macro declaration
                    // First try built-in macro docs
                    if (try self.getBuiltinMacroHover(current.text)) |builtin_docs| {
                        return builtin_docs;
                    }

                    // Otherwise, try to find and format the JSDoc above
                    if (last_doc_comment) |doc| {
                        return try formatJsDocForHover(self.allocator, doc);
                    }

                    // Fallback: just show the declaration signature
                    return try std.fmt.allocPrint(
                        self.allocator,
                        "```typescript\nextern macro {s}\n```",
                        .{current.text},
                    );
                }
                break;
            }

            // Reset doc comment if we hit non-doc token
            if (current.kind != .doc_comment) {
                // Only reset if we're on a different line (allow whitespace)
                if (prev_tok) |prev| {
                    if (current.loc.start.line > prev.loc.start.line + 1) {
                        last_doc_comment = null;
                    }
                }
            }

            prev_prev_tok = prev_tok;
            prev_tok = current;
        }

        return null;
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

        // Hover result with plain string content (more compatible) and range
        // Note: Using plain string instead of MarkupContent for better editor compatibility
        try writer.writeAll(",\"result\":{\"contents\":");
        try std.json.stringify(content, .{}, writer);
        try writer.writeAll(",\"range\":{\"start\":{\"line\":");

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

        // Convert URI to file path for Trans-Am queries (must match setFileText key)
        const file_path = uriToPath(uri.string);

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

        // Try Trans-Am symbol table lookup first (type-aware, scope-aware, position-aware)
        // Use file_path (not uri.string) to match setFileText key
        if (self.transam_db) |*db| {
            if (file_path) |path| {
                // Use position-aware lookup to handle shadowed variables correctly
                // LSP uses 0-based lines, parser uses 1-based, so add 1 to convert
                const parser_line = line + 1;
                const symbol_opt = db.lookupSymbolAtPosition(path, target_name.?, parser_line, character) catch null;
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
        }

        // Try cross-module lookup via ModuleLoader (file_path already computed above)
        if (self.loader) |loader_ptr| {
            if (file_path) |current_path| {
                // Check if target is an imported symbol
                if (try self.findImportedSymbolDefinition(loader_ptr, current_path, target_name.?, id, stdout, stderr)) {
                    return;
                }
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
        // Built-in compiler intrinsics are in std/macros/compiler.ms
        // Derive macros are in std/macros/derive.ms
        const macro_files = [_]struct { prefix: []const u8, file: []const u8 }{
            // Compiler intrinsics
            .{ .prefix = "target", .file = "std/macros/compiler.ms" },
            .{ .prefix = "emit", .file = "std/macros/compiler.ms" },
            .{ .prefix = "comptime", .file = "std/macros/compiler.ms" },
            .{ .prefix = "inline", .file = "std/macros/compiler.ms" },
            .{ .prefix = "sizeof", .file = "std/macros/compiler.ms" },
            .{ .prefix = "alignof", .file = "std/macros/compiler.ms" },
            // Derive macros
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

                // Search for various macro definition patterns
                var line_num: u32 = 0;
                var line_start: usize = 0;

                // Patterns to search for (in order of preference)
                // Note: macro_name is just "target", but declaration is "extern macro @target"
                const patterns = [_]struct { prefix: []const u8, offset: usize }{
                    .{ .prefix = "extern macro @", .offset = "extern macro @".len },
                    .{ .prefix = "@macro function ", .offset = "@macro function ".len },
                    .{ .prefix = "macro @", .offset = "macro @".len },
                    .{ .prefix = "macro ", .offset = "macro ".len },
                };

                for (content, 0..) |c, i| {
                    if (c == '\n') {
                        const line = content[line_start..i];

                        // Try each pattern
                        for (patterns) |pattern| {
                            // Build search string: pattern + macro_name
                            const search_pattern = try std.fmt.allocPrint(
                                self.allocator,
                                "{s}{s}",
                                .{ pattern.prefix, macro_name },
                            );
                            defer self.allocator.free(search_pattern);

                            if (std.mem.indexOf(u8, line, search_pattern)) |col| {
                                // Found it! Calculate the column where the name starts
                                const name_col = col + pattern.offset;

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
                        }

                        line_num += 1;
                        line_start = i + 1;
                    }
                }
            } else |_| {}
        }

        return false;
    }

    /// Find definition of an imported symbol in its source module
    fn findImportedSymbolDefinition(
        self: *Server,
        loader_ptr: *module_loader.ModuleLoader,
        current_path: []const u8,
        symbol_name: []const u8,
        id: std.json.Value,
        stdout: anytype,
        stderr: anytype,
    ) !bool {
        // Use common lookup helper
        const lookup = self.lookupImportedSymbol(loader_ptr, current_path, symbol_name) orelse return false;

        // Get the declaration node's location
        const node = lookup.exported.node;
        const loc = node.location;

        // Build file URI for the source module
        var abs_path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const abs_path = std.fs.cwd().realpath(lookup.source_path, &abs_path_buf) catch {
            return false;
        };

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

        // SourceLocation uses 1-based lines, LSP uses 0-based
        const def_line = if (loc.start.line > 0) loc.start.line - 1 else 0;
        try writer.writeAll(",\"range\":{\"start\":{\"line\":");
        try writer.print("{d}", .{def_line});
        try writer.writeAll(",\"character\":");
        try writer.print("{d}", .{loc.start.column});
        try writer.writeAll("},\"end\":{\"line\":");
        const end_line = if (loc.end.line > 0) loc.end.line - 1 else def_line;
        try writer.print("{d}", .{end_line});
        try writer.writeAll(",\"character\":");
        try writer.print("{d}", .{loc.end.column});
        try writer.writeAll("}}}}");

        try jsonrpc.writeMessage(stdout, buf.items);
        try stderr.print("[mls] Definition (cross-module): {s} in {s} at line {d}\n", .{
            symbol_name,
            lookup.source_path,
            def_line + 1,
        });
        return true;
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

        // Convert URI to file path for Trans-Am queries (must match setFileText key)
        const file_path = uriToPath(uri.string);

        const entry = self.documents.getEntry(uri.string) orelse {
            try self.sendResponse(Response.nullResult(id), stdout, stderr);
            return;
        };
        _ = entry;

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
        // Use file_path (not uri.string) to match setFileText key
        if (self.transam_db) |*db| {
            const completions = if (file_path) |path| db.getCompletions(path) catch null else null;
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
                    // Queue background refresh if stale (use file_path for consistency)
                    if (result.is_stale) {
                        if (self.background_worker) |*bw| {
                            if (file_path) |path| {
                                const file_hash = transam.hashString(path);
                                bw.queue(.completion_refresh, .visible, file_hash, path) catch {};
                            }
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

        // Convert URI to file path for Trans-Am queries (must match setFileText key)
        const file_path = uriToPath(uri.string);

        // Convert to semantic token data
        // Each token is 5 integers: deltaLine, deltaStart, length, tokenType, tokenModifiers
        var data = std.ArrayList(u32).init(self.allocator);
        defer data.deinit();

        // Try Trans-Am first (fast path with caching) - use file_path to match setFileText key
        const used_transam = if (self.transam_db) |*db| blk: {
            const path = file_path orelse break :blk false;
            const syntax_tokens = db.getSyntaxTokens(path) catch {
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

        // Convert URI to file path for Trans-Am queries (must match setFileText key)
        // Keep uri for documents store and LSP responses
        const file_path = uriToPath(uri);

        var diagnostics = std.ArrayList(Diagnostic).init(self.allocator);
        defer diagnostics.deinit();

        // Try Trans-Am first (fast path with caching) - use file_path to match setFileText key
        const used_transam = if (self.transam_db) |*db| blk: {
            const path = file_path orelse break :blk false;
            const transam_diags = db.getDiagnostics(path) catch {
                break :blk false;
            };
            defer db.freeDiagnostics(transam_diags);

            for (transam_diags) |td| {
                // Copy message before freeDiagnostics is called (defer above)
                const message_copy = try self.allocator.dupe(u8, td.message);
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
                    .message = message_copy,
                });
            }

            // Run type checker using cached Trans-Am infrastructure
            // (avoids creating new TypeChecker each time)
            if (transam_diags.len == 0) {
                const check_result = db.checkFile(path) catch {
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

                // Run DRC analysis for memory management diagnostics (use-after-move, cycles)
                // Only run if no type errors - DRC needs a clean AST
                if (check_result.errors.len == 0) {
                    const drc_diags = db.getDrcDiagnostics(path) catch &[_]transam.DrcDiagnosticType{};
                    defer db.freeDrcDiagnostics(drc_diags);

                    for (drc_diags) |drc_diag| {
                        // Copy message before freeDrcDiagnostics is called (defer above)
                        const message_copy = try self.allocator.dupe(u8, drc_diag.message);
                        try diagnostics.append(.{
                            .range = .{
                                .start = .{
                                    .line = if (drc_diag.line > 0) drc_diag.line - 1 else 0,
                                    .character = drc_diag.column,
                                },
                                .end = .{
                                    .line = if (drc_diag.end_line > 0) drc_diag.end_line - 1 else 0,
                                    .character = drc_diag.end_column,
                                },
                            },
                            .severity = switch (drc_diag.severity) {
                                .@"error" => .@"error",
                                .warning => .warning,
                                .hint => .hint,
                            },
                            .source = "mls-drc",
                            .message = message_copy,
                        });
                    }
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

    // ========================================================================
    // DRC (Ownership Analysis) Integration
    // ========================================================================

    /// Convert a DRC diagnostic to an LSP diagnostic
    fn convertDrcDiagnostic(drc_diag: drc_mod.DrcDiagnostic) Diagnostic {
        // Convert DRC severity to LSP severity
        const severity: DiagnosticSeverity = switch (drc_diag.severity) {
            .@"error" => .@"error",
            .warning => .warning,
            .hint => .hint,
        };

        // Convert DRC code to string
        const code: []const u8 = switch (drc_diag.code) {
            .use_after_move => "use-after-move",
            .potential_cycle => "potential-cycle",
            .uninitialized_use => "uninitialized-use",
            .double_free_risk => "double-free-risk",
        };

        return Diagnostic{
            .range = .{
                .start = .{
                    // DRC uses 1-based lines, LSP uses 0-based
                    .line = if (drc_diag.line > 0) drc_diag.line - 1 else 0,
                    .character = if (drc_diag.column > 0) drc_diag.column - 1 else 0,
                },
                .end = .{
                    .line = if (drc_diag.end_line > 0) drc_diag.end_line - 1 else (if (drc_diag.line > 0) drc_diag.line - 1 else 0),
                    .character = if (drc_diag.end_column > 0) drc_diag.end_column - 1 else (if (drc_diag.column > 0) drc_diag.column - 1 else 0),
                },
            },
            .severity = severity,
            .code = code,
            .source = "drc",
            .message = drc_diag.message,
        };
    }

    /// Run DRC analysis on a typed AST and cache the results
    /// Returns the DRC diagnostics converted to LSP format
    pub fn runDrcAnalysis(self: *Server, uri: []const u8, typed_ast: *ast.Node) ![]Diagnostic {
        // Remove old cached DRC if exists
        if (self.drc_cache.get(uri)) |old_drc| {
            old_drc.deinit();
            self.allocator.destroy(old_drc);
            _ = self.drc_cache.remove(uri);
        }

        // Create new DRC analyzer
        const drc_ptr = try self.allocator.create(drc_mod.Drc);
        drc_ptr.* = drc_mod.Drc.init(self.allocator);

        // Run analysis
        var analyzer = drc_analyzer.DrcAnalyzer.init(drc_ptr);
        analyzer.analyze(typed_ast) catch |err| {
            std.log.warn("[mls] DRC analysis failed: {}", .{err});
            drc_ptr.deinit();
            self.allocator.destroy(drc_ptr);
            return &[_]Diagnostic{};
        };

        // Finalize DRC (compute diagnostics)
        drc_ptr.finalize() catch |err| {
            std.log.warn("[mls] DRC finalize failed: {}", .{err});
        };

        // Cache the DRC results (need to dupe the key since uri may be temporary)
        const uri_copy = try self.allocator.dupe(u8, uri);
        try self.drc_cache.put(uri_copy, drc_ptr);

        // Convert diagnostics to LSP format
        const drc_diags = drc_ptr.getDiagnostics();
        var lsp_diags = std.ArrayList(Diagnostic).init(self.allocator);

        for (drc_diags) |drc_diag| {
            try lsp_diags.append(convertDrcDiagnostic(drc_diag));
        }

        return lsp_diags.toOwnedSlice();
    }

    /// Get cached DRC results for a file (for hover information)
    pub fn getCachedDrc(self: *Server, uri: []const u8) ?*drc_mod.Drc {
        return self.drc_cache.get(uri);
    }

    /// Get ownership information for a variable at a specific location
    /// Returns formatted hover text or null if no ownership info available
    pub fn getOwnershipHoverInfo(self: *Server, uri: []const u8, var_name: []const u8) ?[]const u8 {
        // First try server-side DRC cache
        if (self.getCachedDrc(uri)) |drc| {
            if (drc.variables.get(var_name)) |variable| {
                return self.formatOwnershipInfo(variable);
            }
        }

        // Fall back to Trans-Am on-demand query
        const file_path = uriToPath(uri) orelse return null;
        if (self.transam_db) |*db| {
            const ownership = db.getVariableOwnership(file_path, var_name) catch return null;
            if (ownership) |o| {
                // Create a temporary variable struct for formatting
                const variable = drc_mod.Variable{
                    .name = var_name,
                    .type_kind = .unknown,
                    .scope_depth = 0,
                    .line = 0,
                    .column = 0,
                    .needs_rc = o.needs_rc,
                    .is_parameter = o.is_parameter,
                    .is_module_level = o.is_module_level,
                    .ownership_state = o.ownership_state,
                };
                return self.formatOwnershipInfo(variable);
            }
        }

        return null;
    }

    /// Format ownership information for hover display
    fn formatOwnershipInfo(self: *Server, variable: drc_mod.Variable) ?[]const u8 {
        var buf = std.ArrayList(u8).init(self.allocator);
        const writer = buf.writer();

        writer.print("**Ownership:** ", .{}) catch return null;

        switch (variable.ownership_state) {
            .owned => writer.writeAll("`owned`") catch return null,
            .borrowed => writer.writeAll("`borrowed`") catch return null,
            .moved => writer.writeAll("`moved` ⚠️") catch return null,
            .unknown => writer.writeAll("`unknown`") catch return null,
        }

        writer.writeAll("\n") catch return null;

        // Add RC info
        if (variable.needs_rc) {
            writer.writeAll("**RC managed:** Yes\n") catch return null;
        } else {
            writer.writeAll("**RC managed:** No (value type)\n") catch return null;
        }

        // Add cleanup info
        if (variable.is_module_level) {
            writer.writeAll("**Lifetime:** Static (module-level)\n") catch return null;
        } else if (variable.is_parameter) {
            writer.writeAll("**Lifetime:** Borrowed from caller\n") catch return null;
        } else if (variable.ownership_state == .moved) {
            writer.writeAll("**Lifetime:** Moved (do not use)\n") catch return null;
        } else if (variable.needs_rc) {
            writer.writeAll("**Lifetime:** Cleanup at scope exit\n") catch return null;
        }

        return buf.toOwnedSlice() catch return null;
    }
};

// ============================================================================
// URI Utilities
// ============================================================================

/// Convert a file:// URI to a file system path
/// e.g., "file:///Users/foo/bar.ms" -> "/Users/foo/bar.ms"
/// Handles URL-encoded characters (e.g., %20 → space)
/// Note: On macOS/Linux, file:// URIs have format file:///path
/// On Windows, they're file:///C:/path (but we don't handle Windows yet)
fn uriToPath(uri: []const u8) ?[]const u8 {
    const prefix = "file://";
    if (!std.mem.startsWith(u8, uri, prefix)) {
        return null;
    }

    const encoded_path = uri[prefix.len..];

    // Fast path: if no % encoding, return as-is
    if (std.mem.indexOf(u8, encoded_path, "%") == null) {
        return encoded_path;
    }

    // For now, return the encoded path - full URL decoding would need allocation
    // TODO: Implement proper URL decoding with allocator for paths with special characters
    return encoded_path;
}

/// Convert a file system path to a file:// URI
fn pathToUri(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    return std.fmt.allocPrint(allocator, "file://{s}", .{path});
}

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

// ===== JSDoc Formatting Tests =====

test "lsp.jsdoc: cleanJsDoc removes wrapper" {
    // Test basic JSDoc wrapper removal
    const doc1 = "/** Simple comment */";
    const cleaned1 = cleanJsDoc(doc1);
    try testing.expectEqualStrings("Simple comment", cleaned1);

    // Test multiline JSDoc
    const doc2 = "/**\n * Description\n */";
    const cleaned2 = cleanJsDoc(doc2);
    try testing.expect(cleaned2.len > 0);
}

test "lsp.jsdoc: formatJsDocForHover parses simple description" {
    const doc = "/** This is a simple description */";
    const result = try formatJsDocForHover(testing.allocator, doc);
    defer testing.allocator.free(result);

    try testing.expect(std.mem.indexOf(u8, result, "This is a simple description") != null);
}

test "lsp.jsdoc: formatJsDocForHover parses @param tag" {
    const doc =
        \\/**
        \\ * Function description
        \\ * @param x The x parameter
        \\ * @param y The y parameter
        \\ */
    ;
    const result = try formatJsDocForHover(testing.allocator, doc);
    defer testing.allocator.free(result);

    // Should have Parameters section
    try testing.expect(std.mem.indexOf(u8, result, "**Parameters:**") != null);
    try testing.expect(std.mem.indexOf(u8, result, "x`") != null);
    try testing.expect(std.mem.indexOf(u8, result, "y`") != null);
}

test "lsp.jsdoc: formatJsDocForHover parses @returns tag" {
    const doc =
        \\/**
        \\ * Calculates something
        \\ * @returns The calculated value
        \\ */
    ;
    const result = try formatJsDocForHover(testing.allocator, doc);
    defer testing.allocator.free(result);

    // Should have Returns section
    try testing.expect(std.mem.indexOf(u8, result, "**Returns:**") != null);
    try testing.expect(std.mem.indexOf(u8, result, "calculated value") != null);
}

test "lsp.jsdoc: formatJsDocForHover parses @deprecated tag" {
    const doc =
        \\/**
        \\ * Old function
        \\ * @deprecated Use newFunction instead
        \\ */
    ;
    const result = try formatJsDocForHover(testing.allocator, doc);
    defer testing.allocator.free(result);

    // Should have deprecated warning
    try testing.expect(std.mem.indexOf(u8, result, "**@deprecated") != null);
}

test "lsp.jsdoc: formatJsDocForHover handles typed @param" {
    const doc =
        \\/**
        \\ * @param {number} x The x value
        \\ */
    ;
    const result = try formatJsDocForHover(testing.allocator, doc);
    defer testing.allocator.free(result);

    // Should include type
    try testing.expect(std.mem.indexOf(u8, result, "number") != null);
    try testing.expect(std.mem.indexOf(u8, result, "x") != null);
}

test "lsp.jsdoc: formatJsDocForHover preserves list items" {
    const doc =
        \\/**
        \\ * Checks if a file exists across platforms:
        \\ * - C backend: access() syscall
        \\ * - JS backend: fs.existsSync
        \\ * - Erlang: filelib:is_file
        \\ */
    ;
    const result = try formatJsDocForHover(testing.allocator, doc);
    defer testing.allocator.free(result);

    // List items should be on separate lines, not joined
    try testing.expect(std.mem.indexOf(u8, result, "- C backend") != null);
    try testing.expect(std.mem.indexOf(u8, result, "- JS backend") != null);
    try testing.expect(std.mem.indexOf(u8, result, "- Erlang") != null);
    // Verify newlines exist between list items (rough check)
    try testing.expect(std.mem.indexOf(u8, result, "\n- ") != null);
}

test "lsp.jsdoc: formatJsDocForHover parses @example tag" {
    const doc =
        \\/**
        \\ * Adds two numbers
        \\ * @example
        \\ * const result = add(1, 2);
        \\ * console.log(result); // 3
        \\ * @param a First number
        \\ */
    ;
    const result = try formatJsDocForHover(testing.allocator, doc);
    defer testing.allocator.free(result);

    // Should have Example code block
    try testing.expect(std.mem.indexOf(u8, result, "```typescript") != null);
    try testing.expect(std.mem.indexOf(u8, result, "add(1, 2)") != null);
}

test "lsp.jsdoc: formatJsDocForHover handles inline @example" {
    const doc =
        \\/**
        \\ * Computes value
        \\ * @example const x = compute();
        \\ */
    ;
    const result = try formatJsDocForHover(testing.allocator, doc);
    defer testing.allocator.free(result);

    try testing.expect(std.mem.indexOf(u8, result, "```typescript") != null);
    try testing.expect(std.mem.indexOf(u8, result, "compute()") != null);
}

test "lsp.jsdoc: formatJsDocForHover handles typed @returns" {
    const doc =
        \\/**
        \\ * @returns {boolean} True if successful
        \\ */
    ;
    const result = try formatJsDocForHover(testing.allocator, doc);
    defer testing.allocator.free(result);

    try testing.expect(std.mem.indexOf(u8, result, "**Returns:**") != null);
    try testing.expect(std.mem.indexOf(u8, result, "`boolean`") != null);
    try testing.expect(std.mem.indexOf(u8, result, "True if successful") != null);
}

test "lsp.jsdoc: formatJsDocForHover handles multi-paragraph description" {
    const doc =
        \\/**
        \\ * First paragraph describes the purpose.
        \\ *
        \\ * Second paragraph provides more detail.
        \\ */
    ;
    const result = try formatJsDocForHover(testing.allocator, doc);
    defer testing.allocator.free(result);

    try testing.expect(std.mem.indexOf(u8, result, "First paragraph") != null);
    try testing.expect(std.mem.indexOf(u8, result, "Second paragraph") != null);
    // Verify paragraph break (double newline) exists between them
    try testing.expect(std.mem.indexOf(u8, result, "\n\n") != null);
}

test "lsp.jsdoc: formatJsDocForHover complete example" {
    const doc =
        \\/**
        \\ * Calculates the area of a rectangle.
        \\ *
        \\ * Supports:
        \\ * - Integer dimensions
        \\ * - Float dimensions
        \\ *
        \\ * @param {number} width The width
        \\ * @param {number} height The height
        \\ * @returns {number} The calculated area
        \\ * @example
        \\ * const area = calculateArea(10, 20);
        \\ * @deprecated Use calculateRectArea instead
        \\ */
    ;
    const result = try formatJsDocForHover(testing.allocator, doc);
    defer testing.allocator.free(result);

    // Check all sections present
    try testing.expect(std.mem.indexOf(u8, result, "Calculates the area") != null);
    try testing.expect(std.mem.indexOf(u8, result, "**Parameters:**") != null);
    try testing.expect(std.mem.indexOf(u8, result, "`width`") != null);
    try testing.expect(std.mem.indexOf(u8, result, "`height`") != null);
    try testing.expect(std.mem.indexOf(u8, result, "**Returns:**") != null);
    try testing.expect(std.mem.indexOf(u8, result, "```typescript") != null);
    try testing.expect(std.mem.indexOf(u8, result, "**@deprecated") != null);
}
