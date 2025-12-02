// Trans-Am Type Definitions
// Core types for the incremental computation engine

const std = @import("std");
const ast = @import("../ast/ast.zig");
const checker = @import("../checker/typechecker.zig");

// ===== REVISION TRACKING =====

/// Global revision counter - incremented on any input change
pub const Revision = struct {
    value: u64,

    pub fn init() Revision {
        return .{ .value = 0 };
    }

    pub fn increment(self: *Revision) void {
        self.value += 1;
    }

    pub fn eq(self: Revision, other: Revision) bool {
        return self.value == other.value;
    }
};

// ===== QUERY TYPES =====

/// Query type identifier
pub const QueryType = enum(u8) {
    file_text, // Input query
    parse, // Lexer + Parser
    macro_call_sites, // Find macro invocations
    macro_input_hash, // Hash of macro inputs
    macro_output_hash, // Hash of macro outputs (content-addressed)
    macro_expand, // Execute macro expansion
    symbols, // Symbol table
    type_of, // Type inference
    check_function, // Type checking
};

/// Unique identifier for a query invocation
pub const QueryKey = struct {
    query_type: QueryType,
    input_hash: u64,

    pub fn hash(self: QueryKey) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, @intFromEnum(self.query_type));
        std.hash.autoHash(&hasher, self.input_hash);
        return hasher.final();
    }

    pub fn eql(self: QueryKey, other: QueryKey) bool {
        return self.query_type == other.query_type and self.input_hash == other.input_hash;
    }
};

/// Query state in red-green algorithm
pub const QueryState = enum(u2) {
    green, // Verified as current
    red, // May be stale
    yellow, // Currently being verified (cycle detection)
};

/// Durability levels for queries
pub const Durability = enum(u2) {
    low = 0, // User-edited files (src/*.ms)
    medium = 1, // Config files (*.json)
    high = 2, // Library files (std/, node_modules/)

    /// Get minimum durability from two values
    pub fn min(a: Durability, b: Durability) Durability {
        return @enumFromInt(@min(@intFromEnum(a), @intFromEnum(b)));
    }
};

/// Cached query result with dependency tracking
pub const QueryValue = struct {
    /// The actual cached value (type-erased pointer)
    value: *anyopaque,
    /// Type ID for runtime type checking
    type_id: u64,
    /// Hash of the computed value (for content-addressed comparison)
    value_hash: u64,
    /// Query state (green/red/yellow)
    state: QueryState,
    /// Revision when this value was computed
    computed_at: Revision,
    /// Revision when this was last verified
    verified_at: Revision,
    /// Dependencies: queries we called during computation
    dependencies: []QueryKey,
    /// Durability level of this query (min of all dependencies)
    durability: Durability,
};

// ===== DEPENDENCY TRACKING =====

/// Stack frame for tracking dependencies during query execution
pub const DependencyFrame = struct {
    query_key: QueryKey,
    dependencies: std.ArrayList(QueryKey),
    min_durability: Durability,
};

/// Stack for tracking query dependencies during execution
pub const DependencyStack = struct {
    allocator: std.mem.Allocator,
    frames: std.ArrayList(DependencyFrame),

    pub fn init(allocator: std.mem.Allocator) DependencyStack {
        return .{
            .allocator = allocator,
            .frames = std.ArrayList(DependencyFrame).init(allocator),
        };
    }

    pub fn deinit(self: *DependencyStack) void {
        for (self.frames.items) |*frame| {
            frame.dependencies.deinit();
        }
        self.frames.deinit();
    }

    /// Push new frame when starting a query
    pub fn push(self: *DependencyStack, key: QueryKey) !void {
        try self.frames.append(.{
            .query_key = key,
            .dependencies = std.ArrayList(QueryKey).init(self.allocator),
            .min_durability = .high,
        });
    }

    /// Record a dependency on another query
    pub fn recordDependency(self: *DependencyStack, dep_key: QueryKey, dep_durability: Durability) !void {
        if (self.frames.items.len > 0) {
            const frame = &self.frames.items[self.frames.items.len - 1];
            try frame.dependencies.append(dep_key);
            // Durability propagates: min of all dependencies
            if (@intFromEnum(dep_durability) < @intFromEnum(frame.min_durability)) {
                frame.min_durability = dep_durability;
            }
        }
    }

    /// Pop frame and return collected dependencies
    pub fn pop(self: *DependencyStack) ?DependencyFrame {
        if (self.frames.items.len == 0) return null;
        return self.frames.pop();
    }

    /// Check if a query is currently being executed (cycle detection)
    pub fn isInProgress(self: *DependencyStack, key: QueryKey) bool {
        for (self.frames.items) |frame| {
            if (frame.query_key.eql(key)) return true;
        }
        return false;
    }
};

// ===== SYNTAX HIGHLIGHTING TYPES =====

/// Syntax token type for lexer-based highlighting (independent of macros)
pub const SyntaxTokenType = enum(u8) {
    keyword,
    identifier,
    string,
    number,
    comment,
    operator,
    punctuation,
    macro,
    type_name,
    function_name,
    parameter,
    property,
};

/// Syntax token for immediate highlighting (no macro dependency)
pub const SyntaxToken = struct {
    line: u32, // 0-indexed
    column: u32,
    length: u32,
    token_type: SyntaxTokenType,
    modifiers: u16 = 0,
};

/// Status of semantic tokens availability
pub const SemanticTokensStatus = enum {
    available, // Full semantic tokens ready
    pending_expansion, // Waiting for macro expansion
    syntax_only, // Only syntax tokens available
};

/// Semantic tokens with version for delta requests
pub const SemanticTokensResult = struct {
    tokens: []SyntaxToken,
    version: u64,
};

/// Delta edit for semantic tokens
pub const SemanticTokenEdit = struct {
    start: u32,
    delete_count: u32,
    data: []const u32, // New tokens in LSP format
};

/// Delta result for semantic tokens
pub const SemanticTokensDelta = struct {
    edits: []SemanticTokenEdit,
    full_tokens: ?[]SyntaxToken, // If delta not possible, full tokens
    result_version: u64,
};

// ===== ASYNC EXPANSION TYPES =====

/// Handle for tracking async macro expansion
pub const ExpansionHandle = struct {
    id: u64,
    file_id_hash: u64,
    started_at: i64,
};

/// Status of async macro expansion
pub const ExpansionStatus = enum {
    pending, // Queued but not started
    running, // Currently expanding
    completed, // Finished successfully
    cancelled, // Cancelled by user or new edit
    failed, // Failed with error
};

/// Progress info for macro expansion
pub const ExpansionProgress = struct {
    file_id: []const u8,
    total_macros: u32,
    current_index: u32,
    current_macro_name: ?[]const u8,
    elapsed_ms: u64,
    status: ExpansionStatus,
};

/// Progress callback type (ctx may be null)
pub const ExpansionProgressCallback = *const fn (progress: ExpansionProgress, ctx: ?*anyopaque) void;

// ===== DIAGNOSTICS TYPES =====

/// Diagnostic severity levels
pub const DiagnosticSeverity = enum(u8) {
    @"error" = 1,
    warning = 2,
    information = 3,
    hint = 4,
};

/// A diagnostic message with location
pub const Diagnostic = struct {
    start_line: u32, // 0-indexed
    start_col: u32,
    end_line: u32,
    end_col: u32,
    severity: DiagnosticSeverity,
    message: []const u8,
    source: []const u8,
};

// ===== PARSE RESULT TYPES =====

pub const ParseResult = struct {
    tree: *ast.Node,
    errors: []ParseError,
    arena: *ast.ASTArena, // Arena owning the AST memory
};

pub const ParseError = struct {
    message: []const u8,
    location: ast.SourceLocation,
};

// ===== MACRO TYPES =====

pub const MacroExpansion = struct {
    ast: *ast.Node,
    origin: MacroCallLoc,
};

pub const MacroArgs = struct {
    args: [][]const u8 = &.{},
};

pub const MacroCallLoc = struct {
    file_id: []const u8,
    def_id: MacroDefId,
    range: SourceRange,
};

pub const SourceRange = struct {
    start: u32,
    end: u32,
};

pub const MacroCallId = u64;
pub const MacroDefId = u64;
pub const SymbolId = u64;

// ===== TYPE CHECKER INTEGRATION TYPES =====

/// Result of type checking a file or function
pub const TypeCheckResult = struct {
    success: bool,
    errors: []const checker.TypeError,
    /// Cached reference to symbol table (owned by symbols_cache)
    symbols: ?*checker.symbol.SymbolTable,

    pub fn ok(symbols: *checker.symbol.SymbolTable) TypeCheckResult {
        return .{
            .success = true,
            .errors = &.{},
            .symbols = symbols,
        };
    }

    pub fn withErrors(errors: []const checker.TypeError) TypeCheckResult {
        return .{
            .success = false,
            .errors = errors,
            .symbols = null,
        };
    }
};

// ===== CONFIGURATION =====

/// Compiler configuration
pub const CompilerConfig = struct {
    target: Target = .c,
    optimization: Optimization = .none,
    debug_info: bool = true,

    pub const Target = enum { c, javascript, erlang };
    pub const Optimization = enum { none, speed, size };

    pub fn default() CompilerConfig {
        return .{};
    }
};

// ===== INTERNAL CACHE TYPES =====

/// Cached diagnostics for a file
pub const CachedDiagnostics = struct {
    diagnostics: []Diagnostic,
    file_hash: u64,
    computed_at: i64,
};

/// Cached syntax tokens for a file
pub const CachedSyntaxTokens = struct {
    tokens: []SyntaxToken,
    file_hash: u64,
    computed_at: i64,
};

/// Async expansion task state
pub const AsyncExpansionTask = struct {
    handle: ExpansionHandle,
    file_id: []const u8,
    file_id_hash: u64,
    status: ExpansionStatus,
    total_macros: u32,
    completed_macros: u32,
    error_message: ?[]const u8,
    thread: ?std.Thread,
};

/// Context passed to async expansion thread
pub const ThreadContext = struct {
    db: *anyopaque, // Actually *TransAmDatabase, but forward reference
    task_id: u64,
    file_id: []const u8,
};

/// Cached symbol table for a file
pub const CachedSymbolTable = struct {
    table: *checker.symbol.SymbolTable,
    type_checker: *checker.TypeChecker,
    file_hash: u64,
    computed_at: i64,
};

/// Cached type check result for a function
pub const CachedFunctionCheck = struct {
    result: TypeCheckResult,
    file_hash: u64,
    func_name: []const u8,
    computed_at: i64,
};

// ===== MACRO CALL INFO =====

/// Unique identifier for a macro call site (firewall pattern)
pub const MacroCallInfo = struct {
    file_id_hash: u64,
    call_index: u32, // Index within the file
    input_hash: u64, // Content hash of the call

    pub fn hash(self: MacroCallInfo) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, self.file_id_hash);
        std.hash.autoHash(&hasher, self.call_index);
        std.hash.autoHash(&hasher, self.input_hash);
        return hasher.final();
    }
};

/// Information about a macro call site
pub const MacroCallSite = struct {
    call_id: MacroCallInfo,
    macro_name: []const u8,
    line: u32,
    column: u32,
    arguments: []const []const u8, // Argument identifiers (Eq, Hash, etc.)
    /// Target AST node that the macro operates on (e.g., class declaration)
    /// Required for MacroVM execution
    target_node: ?*ast.Node = null,
};

/// Cached macro expansion result
pub const CachedMacroExpansion = struct {
    call_id: MacroCallInfo,
    input_hash: u64,
    output_hash: u64,
    expanded_ast: ?*ast.Node,
    computed_at: i64,
};

// ===== TYPE INFERENCE (PLACEHOLDER) =====

pub const TypeInference = struct {
    // Placeholder for type inference results
};

pub const UnifiedIR = struct {
    // Placeholder for unified IR
};

pub const AstIdMap = struct {
    // Placeholder for AST ID mapping
};

// ===== UTILITY FUNCTIONS =====

/// Classify input durability based on file path
pub fn classifyDurability(file_id: []const u8) Durability {
    // Library files rarely change
    if (std.mem.indexOf(u8, file_id, "node_modules") != null) {
        return .high;
    }
    if (std.mem.startsWith(u8, file_id, "std/")) {
        return .high;
    }
    // Config files change occasionally
    if (std.mem.endsWith(u8, file_id, ".json")) {
        return .medium;
    }
    if (std.mem.endsWith(u8, file_id, "metascript.config")) {
        return .medium;
    }
    // User source files change frequently
    return .low;
}

// ===== TESTS =====

test "Revision: init and increment" {
    var rev = Revision.init();
    try std.testing.expectEqual(@as(u64, 0), rev.value);

    rev.increment();
    try std.testing.expectEqual(@as(u64, 1), rev.value);
}

test "QueryKey: hash and equality" {
    const key1 = QueryKey{ .query_type = .parse, .input_hash = 12345 };
    const key2 = QueryKey{ .query_type = .parse, .input_hash = 12345 };
    const key3 = QueryKey{ .query_type = .parse, .input_hash = 67890 };
    const key4 = QueryKey{ .query_type = .macro_expand, .input_hash = 12345 };

    try std.testing.expect(key1.eql(key2));
    try std.testing.expect(!key1.eql(key3));
    try std.testing.expect(!key1.eql(key4));
    try std.testing.expectEqual(key1.hash(), key2.hash());
}

test "DependencyStack: push and pop" {
    var stack = DependencyStack.init(std.testing.allocator);
    defer stack.deinit();

    const key1 = QueryKey{ .query_type = .parse, .input_hash = 1 };
    const key2 = QueryKey{ .query_type = .macro_expand, .input_hash = 2 };

    try stack.push(key1);
    try stack.recordDependency(key2, .low);

    var frame = stack.pop();
    try std.testing.expect(frame != null);
    try std.testing.expect(frame.?.query_key.eql(key1));
    try std.testing.expectEqual(@as(usize, 1), frame.?.dependencies.items.len);
    // Clean up the popped frame's dependencies
    frame.?.dependencies.deinit();
}

test "DependencyStack: cycle detection" {
    var stack = DependencyStack.init(std.testing.allocator);
    defer stack.deinit();

    const key1 = QueryKey{ .query_type = .parse, .input_hash = 1 };

    try stack.push(key1);
    try std.testing.expect(stack.isInProgress(key1));

    _ = stack.pop();
    try std.testing.expect(!stack.isInProgress(key1));
}

test "classifyDurability" {
    try std.testing.expectEqual(Durability.high, classifyDurability("node_modules/foo/bar.js"));
    try std.testing.expectEqual(Durability.high, classifyDurability("std/io.ms"));
    try std.testing.expectEqual(Durability.medium, classifyDurability("config.json"));
    try std.testing.expectEqual(Durability.low, classifyDurability("src/main.ms"));
}
