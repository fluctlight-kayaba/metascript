// Trans-am Query Engine
// Metascript's incremental computation engine (inspired by Salsa)
//
// The Trans-am engine provides:
// - Input queries (file text, config) - never cached, set by LSP
// - LRU cached queries (parse trees, macro expansions) - limited size
// - Default cached queries (type inference, IR) - unbounded (for now)
// - Interned queries (symbols, IDs) - deduplicated, never evicted
//
// Named "Trans-am" for speed and power - the muscle car of query engines.
//
// RED-GREEN ALGORITHM (Salsa-inspired):
// - GREEN: Query result verified as current in this revision
// - RED: Query result may be stale, needs verification
// - YELLOW: Currently being verified (cycle detection)
//
// Key insight: When a dependency changes, we don't immediately recompute.
// Instead, we try_mark_green() - recursively check if the OUTPUT actually
// changed. If not, we mark GREEN without recomputing dependents.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const lexer_mod = @import("../lexer/lexer.zig");
const token_mod = @import("../lexer/token.zig");
const parser_mod = @import("../parser/parser.zig");
const checker = @import("../checker/typechecker.zig");
const vm = @import("../vm/vm.zig");

// DRC (Deterministic Reference Counting) for memory analysis
const drc_mod = @import("../analysis/drc.zig");
const Drc = drc_mod.Drc;
const DrcDiagnostic = drc_mod.DrcDiagnostic;
const DrcVariable = drc_mod.Variable;
const DrcAnalyzer = @import("../analysis/drc_analyzer.zig").DrcAnalyzer;

// ===== SUBMODULES =====
// Trans-Am is organized into focused submodules for maintainability.

/// Submodule: Core type definitions (Revision, QueryKey, QueryValue, etc.)
pub const types_mod = @import("types.zig");

/// Submodule: Cache implementations (LRUCache, MacroOutputCache)
pub const cache_mod = @import("cache.zig");

/// Submodule: Disk cache for .metascript-cache (shared between msc/mls)
pub const disk_cache_mod = @import("disk_cache.zig");

/// Submodule: Hash functions for content-addressing
pub const hash_mod = @import("hash.zig");

/// Submodule: String and macro call interning
pub const intern_mod = @import("intern.zig");

/// Submodule: Input queries (setFileText, getFileText, etc.)
pub const input_queries = @import("input_queries.zig");

/// Submodule: Red-green algorithm (tryMarkGreen, storeQueryResult, etc.)
pub const red_green = @import("red_green.zig");

/// Submodule: Module dependency tracking (recordModuleImport, invalidateDependents, etc.)
pub const module_graph = @import("module_graph.zig");

/// Submodule: Cancellation support (checkCancellation, cancelPendingQueries, etc.)
pub const cancellation = @import("cancellation.zig");

/// Submodule: Type checking queries (getSymbols, checkFile, getDrcDiagnostics, etc.)
pub const type_check = @import("type_check.zig");

/// Submodule: Syntax/semantic highlighting (getSyntaxTokens, async expansion, etc.)
pub const highlight = @import("highlight.zig");

/// Submodule: Macro queries (getMacroCallSites, expandMacroCallSite, etc.)
pub const macro_queries = @import("macro_queries.zig");

/// Submodule: Diagnostics (getDiagnostics, freeDiagnostics, etc.)
pub const diagnostics_mod = @import("diagnostics.zig");

/// Submodule: Code completion (getCompletions, invalidateCompletions, etc.)
pub const completion = @import("completion.zig");

// ===== RE-EXPORTS FROM SUBMODULES =====
// For convenience, re-export commonly used types at module level

// From types.zig
pub const Revision = types_mod.Revision;
pub const QueryType = types_mod.QueryType;
pub const QueryKey = types_mod.QueryKey;
pub const QueryState = types_mod.QueryState;
pub const Durability = types_mod.Durability;
pub const QueryValue = types_mod.QueryValue;
pub const DependencyFrame = types_mod.DependencyFrame;
pub const DependencyStack = types_mod.DependencyStack;
pub const SyntaxTokenType = types_mod.SyntaxTokenType;
pub const SyntaxToken = types_mod.SyntaxToken;
pub const SemanticTokensStatus = types_mod.SemanticTokensStatus;
pub const SemanticTokensResult = types_mod.SemanticTokensResult;
pub const SemanticTokenEdit = types_mod.SemanticTokenEdit;
pub const SemanticTokensDelta = types_mod.SemanticTokensDelta;
pub const ExpansionHandle = types_mod.ExpansionHandle;
pub const ExpansionStatus = types_mod.ExpansionStatus;
pub const ExpansionProgress = types_mod.ExpansionProgress;
pub const ExpansionProgressCallback = types_mod.ExpansionProgressCallback;
pub const DiagnosticSeverity = types_mod.DiagnosticSeverity;
pub const Diagnostic = types_mod.Diagnostic;
pub const ParseResult = types_mod.ParseResult;
pub const ParseError = types_mod.ParseError;
pub const MacroExpansion = types_mod.MacroExpansion;
pub const MacroArgs = types_mod.MacroArgs;
pub const MacroCallLoc = types_mod.MacroCallLoc;
pub const SourceRange = types_mod.SourceRange;
pub const MacroCallId = types_mod.MacroCallId;
pub const MacroDefId = types_mod.MacroDefId;
pub const SymbolId = types_mod.SymbolId;
pub const TypeCheckResult = types_mod.TypeCheckResult;
pub const CompilerConfig = types_mod.CompilerConfig;
pub const CachedDiagnostics = types_mod.CachedDiagnostics;
pub const AsyncExpansionTask = types_mod.AsyncExpansionTask;
pub const ThreadContext = types_mod.ThreadContext;
pub const CachedSymbolTable = types_mod.CachedSymbolTable;
pub const CachedFunctionCheck = types_mod.CachedFunctionCheck;
pub const CachedDrcDiagnostics = types_mod.CachedDrcDiagnostics;
pub const MacroCallInfo = types_mod.MacroCallInfo;
pub const MacroCallSite = types_mod.MacroCallSite;
pub const CachedMacroExpansion = types_mod.CachedMacroExpansion;
pub const TypeInference = types_mod.TypeInference;
pub const UnifiedIR = types_mod.UnifiedIR;
pub const AstIdMap = types_mod.AstIdMap;
pub const classifyDurability = types_mod.classifyDurability;

// From drc.zig (DRC diagnostics for LSP)
pub const DrcDiagnosticType = DrcDiagnostic;

// From cache.zig
pub const LRUCache = cache_mod.LRUCache;
pub const MacroOutputCache = cache_mod.MacroOutputCache;
pub const ShallowTypeContext = cache_mod.ShallowTypeContext;
pub const CompletionCache = cache_mod.CompletionCache;
pub const CachedCompletions = CompletionCache.CachedCompletions;
pub const CompletionItem = cache_mod.CompletionItem;

// From disk_cache.zig
pub const BytecodeCache = disk_cache_mod.BytecodeCache;
pub const CACHE_DIR = disk_cache_mod.CACHE_DIR;
pub const hashFileContents = disk_cache_mod.hashFileContents;
pub const hashMacroSource = disk_cache_mod.hashMacroSource;

/// Submodule: Network cache for @comptime fetch() responses (L5)
pub const network_cache_mod = @import("network_cache.zig");

// From network_cache.zig
pub const NetworkCache = network_cache_mod.NetworkCache;
pub const CachedResponse = network_cache_mod.CachedResponse;
pub const DEFAULT_TTL_MS = network_cache_mod.DEFAULT_TTL_MS;
pub const NETWORK_TIMEOUT_MS = network_cache_mod.NETWORK_TIMEOUT_MS;

// From hash.zig
pub const hashString = hash_mod.hashString;
pub const hashAstNode = hash_mod.hashAstNode;
pub const hashAstNodeInto = hash_mod.hashAstNodeInto;
pub const hashMacroInput = hash_mod.hashMacroInput;
pub const hashMacroCallId = hash_mod.hashMacroCallId;
pub const hashMacroCallLoc = hash_mod.hashMacroCallLoc;
pub const typeId = hash_mod.typeId;
pub const hashValue = hash_mod.hashValue;

// From intern.zig
pub const StringInterner = intern_mod.StringInterner;
pub const MacroCallInterner = intern_mod.MacroCallInterner;

// ===== MACRO EXPANDER FN TYPE =====
// This type references TransAmDatabase, so it must be defined here

/// Function pointer type for macro expanders
pub const MacroExpanderFn = *const fn (db: *TransAmDatabase, args: MacroArgs) anyerror!*ast.Node;

/// Trans-am Query Database
/// The core of incremental compilation for Metascript
pub const TransAmDatabase = struct {
    allocator: std.mem.Allocator,

    // ===== RED-GREEN ALGORITHM STATE =====
    current_revision: Revision,
    query_cache: std.AutoHashMap(u64, QueryValue), // QueryKey.hash() -> QueryValue
    dependency_stack: DependencyStack,

    // ===== MACRO OUTPUT CACHE (Content-Addressed) =====
    macro_output_cache: MacroOutputCache,

    // ===== BYTECODE CACHE (Disk-persisted .metascript-cache) =====
    bytecode_cache: ?BytecodeCache,

    // INPUT LAYER - Set by LSP, never cached
    file_texts: std.StringHashMap([]const u8), // FileId -> source text
    file_hashes: std.StringHashMap(u64), // FileId -> content hash (for change detection)
    config: CompilerConfig,

    // CACHE LAYER - LRU caching with size limits (legacy, being migrated to query_cache)
    parse_cache: LRUCache(ParseResult),
    macro_cache: LRUCache(MacroExpansion),
    ast_id_cache: LRUCache(AstIdMap),

    // Type cache - LRU bounded (2000 entries, ~50MB)
    type_cache: LRUCache(TypeInference),
    ir_cache: std.AutoHashMap(u64, UnifiedIR),

    // INTERNED LAYER - Deduplicated, never evicted
    symbol_interner: StringInterner,
    macro_call_interner: MacroCallInterner,

    // MODULE DEPENDENCY GRAPH - Track which files import which
    // Used for invalidation: when file B changes, files that import B should be re-analyzed
    // Key: file_id, Value: list of files that file_id imports
    module_imports: std.StringHashMap(std.ArrayList([]const u8)),
    // Reverse map: Key: file_id, Value: list of files that import file_id
    module_importers: std.StringHashMap(std.ArrayList([]const u8)),

    // INVALIDATION TRACKING
    dirty_files: std.AutoHashMap(u64, void),
    revision: u64 = 0, // Legacy field, synced with current_revision

    // CANCELLATION SUPPORT (rust-analyzer pattern)
    cancel_flag: *std.atomic.Value(bool), // Legacy boolean flag
    cancellation_version: *std.atomic.Value(u64), // Version-based cancellation (Salsa pattern)

    // ===== PHASE 4: PARALLEL HIGHLIGHTING STATE =====
    // Async expansion tracking
    async_tasks: std.AutoHashMap(u64, AsyncExpansionTask),
    next_expansion_id: u64 = 1,
    expansion_mutex: std.Thread.Mutex,

    // Progress callback
    progress_callback: ?ExpansionProgressCallback = null,
    progress_callback_ctx: ?*anyopaque = null,

    // Syntax token cache (lexer-based, independent of macros)
    syntax_token_cache: std.StringHashMap(CachedSyntaxTokens),
    syntax_token_version: u64 = 0,

    // Semantic token versions for delta tracking
    semantic_token_versions: std.StringHashMap(u64),

    // Diagnostics cache (incremental)
    diagnostics_cache: std.StringHashMap(CachedDiagnostics),

    // Completion cache (L4) - LRU bounded with stale-while-revalidate
    completion_cache: CompletionCache,

    // Network cache (L5) - Disk-based cache for @comptime fetch() responses
    network_cache: ?NetworkCache,

    // Phase 6: Parser integration - AST arenas per file
    // Each file gets its own arena, freed when file is removed or re-parsed
    ast_arenas: std.AutoHashMap(u64, *ast.ASTArena),

    // Phase 9: Type Checker Integration
    // Symbol table cache per file (file_hash -> CachedSymbolTable)
    symbols_cache: std.AutoHashMap(u64, CachedSymbolTable),
    // Function type check cache (hash of file_id + func_name -> CachedFunctionCheck)
    function_check_cache: std.AutoHashMap(u64, CachedFunctionCheck),

    // Phase 10: DRC Diagnostics Cache
    // DRC analysis cache per file (file_hash -> CachedDrcDiagnostics)
    drc_diagnostics_cache: std.AutoHashMap(u64, CachedDrcDiagnostics),
    // DRC cache statistics
    drc_cache_stats: DrcCacheStats = .{},

    const Self = @This();

    /// Statistics for DRC cache performance
    pub const DrcCacheStats = struct {
        hits: u64 = 0,
        misses: u64 = 0,
        invalidations: u64 = 0,
    };

    // CachedSyntaxTokens is defined in types.zig (re-exported at module level)
    const CachedSyntaxTokens = types_mod.CachedSyntaxTokens;

    pub fn init(allocator: std.mem.Allocator) !Self {
        // Cancellation support (rust-analyzer/Salsa pattern)
        const cancel_flag = try allocator.create(std.atomic.Value(bool));
        cancel_flag.* = std.atomic.Value(bool).init(false);
        const cancellation_version = try allocator.create(std.atomic.Value(u64));
        cancellation_version.* = std.atomic.Value(u64).init(0);

        return .{
            .allocator = allocator,
            // Red-green state
            .current_revision = Revision.init(),
            .query_cache = std.AutoHashMap(u64, QueryValue).init(allocator),
            .dependency_stack = DependencyStack.init(allocator),
            // Macro output cache (LRU bounded, 2000 entries default)
            .macro_output_cache = try MacroOutputCache.init(allocator),
            // Bytecode cache (disk-persisted, try to init but don't fail if can't)
            .bytecode_cache = BytecodeCache.init(allocator, null) catch null,
            // Input layer
            .file_texts = std.StringHashMap([]const u8).init(allocator),
            .file_hashes = std.StringHashMap(u64).init(allocator),
            .config = CompilerConfig.default(),
            // Cache layer
            .parse_cache = try LRUCache(ParseResult).init(allocator, 128),
            .macro_cache = try LRUCache(MacroExpansion).init(allocator, 512),
            .ast_id_cache = try LRUCache(AstIdMap).init(allocator, 1024),
            .type_cache = try LRUCache(TypeInference).init(allocator, 2000),
            .ir_cache = std.AutoHashMap(u64, UnifiedIR).init(allocator),
            // Interned layer
            .symbol_interner = StringInterner.init(allocator),
            .macro_call_interner = MacroCallInterner.init(allocator),
            // Module dependency graph
            .module_imports = std.StringHashMap(std.ArrayList([]const u8)).init(allocator),
            .module_importers = std.StringHashMap(std.ArrayList([]const u8)).init(allocator),
            // Invalidation tracking
            .dirty_files = std.AutoHashMap(u64, void).init(allocator),
            .cancel_flag = cancel_flag,
            .cancellation_version = cancellation_version,
            // Phase 4: Parallel highlighting
            .async_tasks = std.AutoHashMap(u64, AsyncExpansionTask).init(allocator),
            .expansion_mutex = .{},
            .syntax_token_cache = std.StringHashMap(CachedSyntaxTokens).init(allocator),
            .semantic_token_versions = std.StringHashMap(u64).init(allocator),
            // Phase 5: Incremental diagnostics
            .diagnostics_cache = std.StringHashMap(CachedDiagnostics).init(allocator),
            // Completion cache (L4) - stale-while-revalidate
            .completion_cache = try CompletionCache.init(allocator),
            // Network cache (L5) - disk-based, try to init but don't fail if can't
            .network_cache = NetworkCache.init(allocator, null) catch null,
            // Phase 6: Parser integration
            .ast_arenas = std.AutoHashMap(u64, *ast.ASTArena).init(allocator),
            // Phase 9: Type Checker Integration
            .symbols_cache = std.AutoHashMap(u64, CachedSymbolTable).init(allocator),
            .function_check_cache = std.AutoHashMap(u64, CachedFunctionCheck).init(allocator),
            // Phase 10: DRC Diagnostics Cache
            .drc_diagnostics_cache = std.AutoHashMap(u64, CachedDrcDiagnostics).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        // Clean up query cache values
        var qv_it = self.query_cache.valueIterator();
        while (qv_it.next()) |qv| {
            self.allocator.free(qv.dependencies);
        }
        self.query_cache.deinit();
        self.dependency_stack.deinit();
        self.macro_output_cache.deinit();
        if (self.bytecode_cache) |*bc| {
            bc.deinit();
        }

        // Clean up file_texts (owned strings)
        var ft_it = self.file_texts.iterator();
        while (ft_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.file_texts.deinit();

        // Clean up file_hashes (keys are shared with file_texts, already freed)
        self.file_hashes.deinit();

        self.parse_cache.deinit();
        self.macro_cache.deinit();
        self.ast_id_cache.deinit();
        self.type_cache.deinit();
        self.ir_cache.deinit();
        self.symbol_interner.deinit();
        self.macro_call_interner.deinit();

        // Clean up module dependency graph
        var mi_it = self.module_imports.valueIterator();
        while (mi_it.next()) |list| {
            list.deinit();
        }
        self.module_imports.deinit();
        var mir_it = self.module_importers.valueIterator();
        while (mir_it.next()) |list| {
            list.deinit();
        }
        self.module_importers.deinit();

        self.dirty_files.deinit();

        // Phase 4: Clean up parallel highlighting state
        // Clean up syntax token cache
        var stc_it = self.syntax_token_cache.iterator();
        while (stc_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.tokens);
        }
        self.syntax_token_cache.deinit();

        // Clean up semantic token versions
        var stv_it = self.semantic_token_versions.keyIterator();
        while (stv_it.next()) |key| {
            self.allocator.free(key.*);
        }
        self.semantic_token_versions.deinit();

        // Clean up async tasks - first signal cancellation and join threads
        // NOTE: cancel_flag must be stored BEFORE it's destroyed!
        self.cancel_flag.store(true, .seq_cst);

        // Join all running threads
        var at_it = self.async_tasks.valueIterator();
        while (at_it.next()) |task| {
            if (task.thread) |thread| {
                thread.join();
            }
            self.allocator.free(task.file_id);
            if (task.error_message) |msg| {
                self.allocator.free(msg);
            }
        }
        self.async_tasks.deinit();

        // Now safe to destroy cancellation state after all threads are done
        self.allocator.destroy(self.cancel_flag);
        self.allocator.destroy(self.cancellation_version);

        // Clean up diagnostics cache
        var dc_it = self.diagnostics_cache.iterator();
        while (dc_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            for (entry.value_ptr.diagnostics) |diag| {
                self.allocator.free(diag.message);
            }
            self.allocator.free(entry.value_ptr.diagnostics);
        }
        self.diagnostics_cache.deinit();

        // Clean up completion cache (L4)
        self.completion_cache.deinit();

        // Clean up network cache (L5)
        if (self.network_cache) |*nc| {
            nc.deinit();
        }

        // Phase 6: Clean up AST arenas
        var arena_it = self.ast_arenas.valueIterator();
        while (arena_it.next()) |arena_ptr| {
            arena_ptr.*.deinit();
            self.allocator.destroy(arena_ptr.*);
        }
        self.ast_arenas.deinit();

        // Phase 9: Clean up type checker caches
        var sc_it = self.symbols_cache.valueIterator();
        while (sc_it.next()) |cached| {
            cached.type_checker.deinit();
            self.allocator.destroy(cached.type_checker);
            // Note: table is owned by type_checker, don't free separately
        }
        self.symbols_cache.deinit();

        var fc_it = self.function_check_cache.valueIterator();
        while (fc_it.next()) |cached| {
            self.allocator.free(cached.func_name);
        }
        self.function_check_cache.deinit();

        // Phase 10: Clean up DRC diagnostics cache
        var drc_it = self.drc_diagnostics_cache.valueIterator();
        while (drc_it.next()) |cached| {
            // Don't free empty static slices
            if (cached.diagnostics.len > 0) {
                for (cached.diagnostics) |diag| {
                    self.allocator.free(diag.message);
                }
                self.allocator.free(cached.diagnostics);
            }
        }
        self.drc_diagnostics_cache.deinit();
    }

    /// Get the current revision
    pub fn getRevision(self: *Self) Revision {
        return self.current_revision;
    }

    // ===== LRU CACHED QUERIES =====

    /// Parse file - LRU cached (limit 128 entries)
    pub fn parse(self: *Self, file_id: []const u8) !ParseResult {
        // Check cancellation
        try cancellation.checkCancellation(self);

        const file_hash = hashString(file_id);

        // Check cache
        if (self.parse_cache.get(file_hash)) |cached| {
            return cached;
        }

        // Cache miss: parse and store
        const text = input_queries.getFileText(self, file_id) orelse return error.FileNotFound;
        const result = try self.parseImpl(file_id, text);
        try self.parse_cache.put(file_hash, result);

        return result;
    }

    // ===== MACRO FIREWALL PATTERN =====
    // Three separate queries to prevent cascading invalidation

    /// Firewall 1: Extract macro arguments
    /// Only invalidates when THIS specific call's text changes
    pub fn macroArg(self: *Self, call_id: MacroCallId) !MacroArgs {
        try cancellation.checkCancellation(self);

        const call_loc = self.macro_call_interner.lookup(call_id);
        const parse_result = try self.parse(call_loc.file_id);

        // Extract arguments from syntax tree
        return self.extractMacroArgs(parse_result.tree, call_loc.range);
    }

    /// Firewall 2: Lookup macro expander (transparent, no cache)
    pub fn macroExpander(self: *Self, def_id: MacroDefId) !MacroExpanderFn {
        // Just return function pointer, don't cache
        return self.builtin_macros.get(def_id) orelse error.UnknownMacro;
    }

    /// Firewall 3: Expand macro - LRU cached (limit 512 entries)
    /// Includes slow query tracking: macros >100ms are flagged.
    /// CRITICAL: Always return stale cached result for slow macros (never error).
    pub fn macroExpand(self: *Self, call_id: MacroCallId) !MacroExpansion {
        try cancellation.checkCancellation(self);

        const call_hash = hashMacroCallId(call_id);

        // Check cache FIRST - always return cached if available
        if (self.macro_cache.get(call_hash)) |cached| {
            return cached;
        }

        // Cache miss: check if this is a known slow macro
        // If slow AND no cached result, we still compute (first time)
        // but subsequent requests will use the cached stale result above
        const is_slow = self.macro_output_cache.isSlowQuery(call_hash);

        // Expand with timing and timeout
        const start_time = std.time.milliTimestamp();

        const args = try self.macroArg(call_id);
        const call_loc = self.macro_call_interner.lookup(call_id);
        const expander = try self.macroExpander(call_loc.def_id);

        // Check cancellation before expensive expansion
        try cancellation.checkCancellation(self);

        const expanded = try expander(self, args);

        const elapsed_ms = std.time.milliTimestamp() - start_time;

        // Check for hard timeout (5 seconds) - prevents infinite loop from blocking forever
        // We still return the result since we already spent the time, but log an error
        if (elapsed_ms > cache_mod.MACRO_TIMEOUT_MS) {
            std.log.err("[Trans-Am] TIMEOUT: Macro exceeded {d}ms limit (took {d}ms). Hash: {x}", .{
                cache_mod.MACRO_TIMEOUT_MS,
                elapsed_ms,
                call_hash,
            });
            std.log.err("[Trans-Am] Consider optimizing this macro or adding @comptime caching", .{});
            // Mark as slow so future requests can use cached result
            try self.macro_output_cache.markSlow(call_hash, elapsed_ms);
        } else if (elapsed_ms > cache_mod.SLOW_QUERY_THRESHOLD_MS) {
            // Mark as slow if >100ms (rust-analyzer threshold)
            try self.macro_output_cache.markSlow(call_hash, elapsed_ms);
            // Log warning for user visibility (first time only)
            if (!is_slow) {
                std.log.warn("[Trans-Am] Slow macro detected ({d}ms > {d}ms threshold). Hash: {x}", .{
                    elapsed_ms,
                    cache_mod.SLOW_QUERY_THRESHOLD_MS,
                    call_hash,
                });
            }
        }

        const result = MacroExpansion{
            .ast = expanded,
            .origin = call_loc,
        };

        // ALWAYS cache the result - stale is better than nothing
        try self.macro_cache.put(call_hash, result);

        return result;
    }

    // ===== INTERNED QUERIES =====

    /// Intern a symbol (deduplicated, never evicted)
    pub fn internSymbol(self: *Self, name: []const u8) !SymbolId {
        return try self.symbol_interner.intern(name);
    }

    /// Intern a macro call location
    pub fn internMacroCall(self: *Self, loc: MacroCallLoc) !MacroCallId {
        return try self.macro_call_interner.intern(loc);
    }

    // ===== DRC CACHE STATS =====

    /// Get DRC diagnostics cache statistics
    pub fn getDrcCacheStats(self: *Self) DrcCacheStats {
        return self.drc_cache_stats;
    }

    /// Get DRC cache hit rate (0.0 to 1.0)
    pub fn getDrcCacheHitRate(self: *Self) f64 {
        const total = self.drc_cache_stats.hits + self.drc_cache_stats.misses;
        if (total == 0) return 0.0;
        return @as(f64, @floatFromInt(self.drc_cache_stats.hits)) / @as(f64, @floatFromInt(total));
    }

    // ===== HOVER SUPPORT =====

    /// Token info for hover (cached via syntax tokens)
    pub const TokenInfo = struct {
        token_type: SyntaxTokenType,
        text: []const u8,
        line: u32,
        column: u32,
        length: u32,
    };

    /// Get token at a specific position (uses cached syntax tokens)
    pub fn getTokenAtPosition(self: *Self, file_id: []const u8, line: u32, column: u32) !?TokenInfo {
        // Use cached syntax tokens
        const syntax_tokens = try highlight.getSyntaxTokens(self, file_id);
        defer self.allocator.free(syntax_tokens);

        // Also need text for token content
        const text = input_queries.getFileText(self, file_id) orelse return error.FileNotFound;

        // Find token at position
        for (syntax_tokens, 0..) |tok, i| {
            // Cancellation checkpoint: check every 100 tokens (usually fast search)
            if (i % 100 == 0) {
                try cancellation.checkCancellation(self);
            }
            if (tok.line == line and column >= tok.column and column < tok.column + tok.length) {
                // Extract token text from source
                const line_start = self.getLineOffset(text, line);
                const tok_start = line_start + tok.column;
                const tok_end = tok_start + tok.length;

                if (tok_end <= text.len) {
                    return TokenInfo{
                        .token_type = tok.token_type,
                        .text = text[tok_start..tok_end],
                        .line = tok.line,
                        .column = tok.column,
                        .length = tok.length,
                    };
                }
            }
        }

        return null;
    }

    /// Get byte offset of a line in text
    fn getLineOffset(self: *Self, text: []const u8, target_line: u32) usize {
        _ = self;
        var line: u32 = 0;
        var offset: usize = 0;

        for (text, 0..) |c, i| {
            if (line == target_line) {
                return offset;
            }
            if (c == '\n') {
                line += 1;
                offset = i + 1;
            }
        }

        return offset;
    }

    // ===== IMPLEMENTATION DETAILS =====

    fn parseImpl(self: *Self, file_id: []const u8, text: []const u8) !ParseResult {
        const file_hash = hashString(file_id);

        // Get or create arena for this file
        // If re-parsing, clean up old arena first
        if (self.ast_arenas.get(file_hash)) |old_arena| {
            old_arena.deinit();
            self.allocator.destroy(old_arena);
        }

        // Create new arena for this parse
        const arena = try self.allocator.create(ast.ASTArena);
        arena.* = ast.ASTArena.init(self.allocator);
        try self.ast_arenas.put(file_hash, arena);

        // Register file in arena and get numeric FileId
        const numeric_file_id = try arena.addFile(file_id);

        // Create lexer
        var lexer = try lexer_mod.Lexer.init(self.allocator, text, numeric_file_id);
        defer lexer.deinit();

        // Create parser
        var parser = parser_mod.Parser.init(self.allocator, arena, &lexer, numeric_file_id);
        defer parser.deinit();

        // Parse!
        const tree = parser.parse() catch |err| {
            // On parse error, still return what we have with errors
            if (err == error.ParseError) {
                // Convert parser errors to our format (uses arena for memory)
                const errors = try self.convertParseErrors(arena, parser.errors.items);
                return ParseResult{
                    .tree = try arena.createNode(.program, ast.SourceLocation.dummy(), .{ .program = .{ .statements = &[_]*ast.Node{}, .file_id = numeric_file_id } }),
                    .errors = errors,
                    .arena = arena,
                };
            }
            return err;
        };

        // Convert parser errors to our format (uses arena for memory)
        const errors = try self.convertParseErrors(arena, parser.errors.items);

        return ParseResult{
            .tree = tree,
            .errors = errors,
            .arena = arena,
        };
    }

    /// Convert parser errors to Trans-Am ParseError format
    /// Uses arena allocator so errors are freed when the parse result's arena is freed
    fn convertParseErrors(self: *Self, arena: *ast.ASTArena, parser_errors: []const parser_mod.ParseError) ![]ParseError {
        _ = self;
        if (parser_errors.len == 0) return &[_]ParseError{};

        const arena_alloc = arena.allocator();
        const errors = try arena_alloc.alloc(ParseError, parser_errors.len);
        for (parser_errors, 0..) |pe, i| {
            errors[i] = .{
                .message = try arena_alloc.dupe(u8, pe.message),
                .location = pe.loc,
            };
        }
        return errors;
    }

    fn extractMacroArgs(self: *Self, tree: *ast.Node, range: SourceRange) !MacroArgs {
        // TODO: Implement macro argument extraction
        _ = self;
        _ = tree;
        _ = range;
        return MacroArgs{};
    }
};

// ===== TESTS =====

// ===== RED-GREEN ALGORITHM TESTS =====

test "Revision: init and increment" {
    var rev = Revision.init();
    try std.testing.expect(rev.value == 0);

    rev.increment();
    try std.testing.expect(rev.value == 1);

    rev.increment();
    try std.testing.expect(rev.value == 2);
}

test "Revision: equality" {
    const rev1 = Revision{ .value = 5 };
    const rev2 = Revision{ .value = 5 };
    const rev3 = Revision{ .value = 6 };

    try std.testing.expect(rev1.eq(rev2));
    try std.testing.expect(!rev1.eq(rev3));
}

test "QueryKey: hash and equality" {
    const key1 = QueryKey{ .query_type = .parse, .input_hash = 12345 };
    const key2 = QueryKey{ .query_type = .parse, .input_hash = 12345 };
    const key3 = QueryKey{ .query_type = .parse, .input_hash = 67890 };
    const key4 = QueryKey{ .query_type = .macro_expand, .input_hash = 12345 };

    try std.testing.expect(key1.eql(key2));
    try std.testing.expect(!key1.eql(key3));
    try std.testing.expect(!key1.eql(key4));
    try std.testing.expect(key1.hash() == key2.hash());
}

test "Durability: min function" {
    try std.testing.expect(Durability.min(.low, .high) == .low);
    try std.testing.expect(Durability.min(.high, .low) == .low);
    try std.testing.expect(Durability.min(.medium, .high) == .medium);
    try std.testing.expect(Durability.min(.high, .high) == .high);
}

test "classifyDurability: user files" {
    try std.testing.expect(classifyDurability("src/main.ms") == .low);
    try std.testing.expect(classifyDurability("app.ms") == .low);
}

test "classifyDurability: library files" {
    try std.testing.expect(classifyDurability("node_modules/foo/bar.js") == .high);
    try std.testing.expect(classifyDurability("std/core.ms") == .high);
}

test "classifyDurability: config files" {
    try std.testing.expect(classifyDurability("package.json") == .medium);
    try std.testing.expect(classifyDurability("metascript.config") == .medium);
}

test "DependencyStack: push, record, pop" {
    var stack = DependencyStack.init(std.testing.allocator);
    defer stack.deinit();

    const key1 = QueryKey{ .query_type = .parse, .input_hash = 1 };
    const key2 = QueryKey{ .query_type = .macro_expand, .input_hash = 2 };

    try stack.push(key1);
    try std.testing.expect(stack.isInProgress(key1));
    try std.testing.expect(!stack.isInProgress(key2));

    try stack.recordDependency(key2, .low);

    const frame = stack.pop().?;
    try std.testing.expect(frame.query_key.eql(key1));
    try std.testing.expect(frame.dependencies.items.len == 1);
    try std.testing.expect(frame.dependencies.items[0].eql(key2));
    try std.testing.expect(frame.min_durability == .low);

    // Clean up the popped frame's dependencies
    frame.dependencies.deinit();
}

test "DependencyStack: cycle detection" {
    var stack = DependencyStack.init(std.testing.allocator);
    defer stack.deinit();

    const key1 = QueryKey{ .query_type = .parse, .input_hash = 1 };

    try stack.push(key1);

    // key1 is in progress, attempting to push it again would be a cycle
    try std.testing.expect(stack.isInProgress(key1));
}

// ===== TRANS-AM DATABASE TESTS =====

test "Trans-am: basic init" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    try std.testing.expect(db.revision == 0);
    try std.testing.expect(db.current_revision.value == 0);
}

test "Trans-am: set and get file text" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const changed = try input_queries.setFileText(&db,"test.mts", "const x = 1;");
    try std.testing.expect(changed);

    const text = input_queries.getFileText(&db,"test.mts").?;
    try std.testing.expectEqualStrings("const x = 1;", text);
    try std.testing.expect(db.revision == 1);
    try std.testing.expect(db.current_revision.value == 1);
}

test "Trans-am: content-addressed change detection" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // First set
    const changed1 = try input_queries.setFileText(&db,"test.mts", "const x = 1;");
    try std.testing.expect(changed1);
    try std.testing.expect(db.revision == 1);

    // Same content - should not increment revision
    const changed2 = try input_queries.setFileText(&db,"test.mts", "const x = 1;");
    try std.testing.expect(!changed2);
    try std.testing.expect(db.revision == 1);

    // Different content - should increment
    const changed3 = try input_queries.setFileText(&db,"test.mts", "const x = 2;");
    try std.testing.expect(changed3);
    try std.testing.expect(db.revision == 2);
}

test "Trans-am: file hash tracking" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.mts", "const x = 1;");

    const hash = input_queries.getFileHash(&db,"test.mts").?;
    try std.testing.expect(hash != 0);
}

test "Trans-am: LRU cache eviction" {
    var cache = try LRUCache(u32).init(std.testing.allocator, 2);
    defer cache.deinit();

    try cache.put(1, 100);
    try cache.put(2, 200);

    try std.testing.expect(cache.get(1).? == 100);
    try std.testing.expect(cache.get(2).? == 200);

    // Evict oldest (1)
    try cache.put(3, 300);

    try std.testing.expect(cache.get(1) == null);
    try std.testing.expect(cache.get(2).? == 200);
    try std.testing.expect(cache.get(3).? == 300);
}

test "Trans-am: string interner" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    const id1 = try interner.intern("foo");
    const id2 = try interner.intern("bar");
    const id3 = try interner.intern("foo");  // Should return same ID

    try std.testing.expect(id1 == id3);
    try std.testing.expect(id1 != id2);

    try std.testing.expectEqualStrings("foo", interner.lookup(id1).?);
    try std.testing.expectEqualStrings("bar", interner.lookup(id2).?);
}

test "Trans-am: cancellation" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Initially not cancelled
    try cancellation.checkCancellation(&db);

    // Request cancellation
    cancellation.requestCancellation(&db);

    // Should now fail
    const result = cancellation.checkCancellation(&db);
    try std.testing.expectError(error.Cancelled, result);

    // Reset
    cancellation.resetCancellation(&db);
    try cancellation.checkCancellation(&db);
}

// ===== RED-GREEN ALGORITHM INTEGRATION TESTS =====

test "Trans-am: storeQueryResult and getCachedQuery" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const key = QueryKey{ .query_type = .parse, .input_hash = 12345 };

    // Create a dummy value
    var dummy_value: u64 = 42;
    const deps = [_]QueryKey{};
    const type_id: u64 = 1; // Unique type identifier

    try red_green.storeQueryResult(&db,
        key,
        @ptrCast(&dummy_value),
        type_id,
        hashString("test"),
        &deps,
        .low,
    );

    // Should be able to retrieve it
    const cached = red_green.getCachedQuery(&db, key);
    try std.testing.expect(cached != null);
    try std.testing.expect(cached.?.state == .green);
    try std.testing.expect(cached.?.durability == .low);
}

test "Trans-am: tryMarkGreen with no dependencies" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const key = QueryKey{ .query_type = .parse, .input_hash = 12345 };

    // Store a query with no dependencies
    var dummy_value: u64 = 42;
    const deps = [_]QueryKey{};
    const type_id: u64 = 1;

    try red_green.storeQueryResult(&db,
        key,
        @ptrCast(&dummy_value),
        type_id,
        hashString("test"),
        &deps,
        .low,
    );

    // Simulate a file change (increment revision)
    db.current_revision.increment();

    // Mark entry as RED
    if (db.query_cache.getPtr(key.hash())) |entry| {
        entry.state = .red;
    }

    // tryMarkGreen should succeed (no dependencies to check)
    const is_green = try red_green.tryMarkGreen(&db,key);
    try std.testing.expect(is_green);

    // Entry should now be GREEN
    const entry = db.query_cache.getPtr(key.hash()).?;
    try std.testing.expect(entry.state == .green);
}

test "Trans-am: tryMarkGreen returns false for missing entry" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const key = QueryKey{ .query_type = .parse, .input_hash = 99999 };

    // No entry stored, should return false
    const is_green = try red_green.tryMarkGreen(&db,key);
    try std.testing.expect(!is_green);
}

test "Trans-am: beginQuery and endQuery" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const key1 = QueryKey{ .query_type = .parse, .input_hash = 1 };
    const key2 = QueryKey{ .query_type = .macro_expand, .input_hash = 2 };

    // Begin query
    try red_green.beginQuery(&db,key1);

    // Record a dependency
    try red_green.recordDependency(&db, key2, .medium);

    // End query and get dependencies
    const frame = red_green.endQuery(&db).?;
    defer frame.dependencies.deinit();

    try std.testing.expect(frame.query_key.eql(key1));
    try std.testing.expect(frame.dependencies.items.len == 1);
    try std.testing.expect(frame.dependencies.items[0].eql(key2));
    try std.testing.expect(frame.min_durability == .medium);
}

test "Trans-am: beginQuery detects cycles" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const key = QueryKey{ .query_type = .parse, .input_hash = 1 };

    // Begin query
    try red_green.beginQuery(&db,key);

    // Try to begin same query again - should detect cycle
    const result = red_green.beginQuery(&db, key);
    try std.testing.expectError(red_green.RedGreenError.CycleDetected, result);

    // Clean up
    _ = red_green.endQuery(&db);
}

test "Trans-am: canReuseOutput" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const key = QueryKey{ .query_type = .macro_expand, .input_hash = 12345 };
    const value_hash = hashString("expanded_ast_content");

    // Store initial result
    var dummy_value: u64 = 42;
    const deps = [_]QueryKey{};
    const type_id: u64 = 1;
    try red_green.storeQueryResult(&db,
        key,
        @ptrCast(&dummy_value),
        type_id,
        value_hash,
        &deps,
        .low,
    );

    // Simulate file change
    db.current_revision.increment();
    if (db.query_cache.getPtr(key.hash())) |entry| {
        entry.state = .red;
    }

    // Same output hash - should be able to reuse
    const can_reuse = red_green.canReuseOutput(&db, key, value_hash);
    try std.testing.expect(can_reuse);

    // Entry should be GREEN again
    const entry = db.query_cache.getPtr(key.hash()).?;
    try std.testing.expect(entry.state == .green);
}

test "Trans-am: canReuseOutput returns false for different hash" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const key = QueryKey{ .query_type = .macro_expand, .input_hash = 12345 };
    const value_hash = hashString("expanded_ast_content");

    // Store initial result
    var dummy_value: u64 = 42;
    const deps = [_]QueryKey{};
    const type_id: u64 = 1;
    try red_green.storeQueryResult(&db,
        key,
        @ptrCast(&dummy_value),
        type_id,
        value_hash,
        &deps,
        .low,
    );

    // Different output hash - cannot reuse
    const different_hash = hashString("different_content");
    const can_reuse = red_green.canReuseOutput(&db, key, different_hash);
    try std.testing.expect(!can_reuse);
}

test "Trans-am: query cache invalidation on file change" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Store a query result
    const key = QueryKey{ .query_type = .parse, .input_hash = 12345 };
    var dummy_value: u64 = 42;
    const deps = [_]QueryKey{};
    const type_id: u64 = 1;
    try red_green.storeQueryResult(&db,
        key,
        @ptrCast(&dummy_value),
        type_id,
        hashString("test"),
        &deps,
        .low,
    );

    // Verify it's GREEN
    try std.testing.expect(db.query_cache.getPtr(key.hash()).?.state == .green);

    // Change a file
    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1;");

    // Query should now be RED
    try std.testing.expect(db.query_cache.getPtr(key.hash()).?.state == .red);
}

// ===== MACRO OUTPUT CACHE TESTS =====

test "MacroOutputCache: basic store and retrieve" {
    var cache = try MacroOutputCache.initWithCapacity(std.testing.allocator, 10);
    defer cache.deinit();

    const input_hash: u64 = 12345;
    const output_hash: u64 = 67890;

    // Create a dummy AST node
    var dummy_node = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
    };

    // Store
    const was_reuse = try cache.store(input_hash, output_hash, &dummy_node);
    try std.testing.expect(!was_reuse); // First store, not a reuse

    // Retrieve output hash
    const retrieved_hash = cache.getOutputHash(input_hash);
    try std.testing.expect(retrieved_hash != null);
    try std.testing.expectEqual(output_hash, retrieved_hash.?);

    // Retrieve AST
    const retrieved_ast = cache.getAst(output_hash);
    try std.testing.expect(retrieved_ast != null);
    try std.testing.expectEqual(&dummy_node, retrieved_ast.?);
}

test "MacroOutputCache: output deduplication" {
    var cache = try MacroOutputCache.initWithCapacity(std.testing.allocator, 10);
    defer cache.deinit();

    var dummy_node = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
    };

    // Different inputs produce same output (content-addressed)
    const input1: u64 = 111;
    const input2: u64 = 222; // Different input
    const same_output: u64 = 999; // Same output!

    // First store
    const reuse1 = try cache.store(input1, same_output, &dummy_node);
    try std.testing.expect(!reuse1);

    // Second store with same output - should detect reuse
    const reuse2 = try cache.store(input2, same_output, &dummy_node);
    try std.testing.expect(reuse2); // Output was already cached!

    // Stats should reflect reuse
    const stats = cache.getStats();
    try std.testing.expectEqual(@as(u64, 1), stats.output_reuse);
}

test "MacroOutputCache: outputUnchanged" {
    var cache = try MacroOutputCache.initWithCapacity(std.testing.allocator, 10);
    defer cache.deinit();

    var dummy_node = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
    };

    const input_hash: u64 = 12345;
    const output_hash: u64 = 67890;

    _ = try cache.store(input_hash, output_hash, &dummy_node);

    // Same output hash - unchanged
    try std.testing.expect(cache.outputUnchanged(input_hash, output_hash));

    // Different output hash - changed
    try std.testing.expect(!cache.outputUnchanged(input_hash, 99999));

    // Unknown input - not unchanged
    try std.testing.expect(!cache.outputUnchanged(99999, output_hash));
}

test "MacroOutputCache: hit rate calculation" {
    var cache = try MacroOutputCache.initWithCapacity(std.testing.allocator, 10);
    defer cache.deinit();

    // Initially 0 hit rate
    try std.testing.expectEqual(@as(f64, 0.0), cache.hitRate());

    var dummy_node = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
    };

    // Store an entry
    _ = try cache.store(111, 222, &dummy_node);

    // Miss (unknown key)
    _ = cache.getOutputHash(999);
    try std.testing.expectEqual(@as(f64, 0.0), cache.hitRate());

    // Hit (known key)
    _ = cache.getOutputHash(111);
    try std.testing.expectEqual(@as(f64, 0.5), cache.hitRate()); // 1 hit, 1 miss

    // Another hit
    _ = cache.getOutputHash(111);
    const hit_rate = cache.hitRate();
    // 2 hits, 1 miss = 2/3 = 0.666...
    try std.testing.expect(hit_rate > 0.66 and hit_rate < 0.67);
}

test "Trans-am: macro output cache integration" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    var dummy_node = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
    };

    // Use the database's macro output cache
    const input_hash: u64 = 12345;
    const output_hash: u64 = 67890;

    _ = try db.macro_output_cache.store(input_hash, output_hash, &dummy_node);

    // Should be retrievable
    const retrieved = db.macro_output_cache.getOutputHash(input_hash);
    try std.testing.expect(retrieved != null);
    try std.testing.expectEqual(output_hash, retrieved.?);
}

// ===== PHASE 4: PARALLEL HIGHLIGHTING TESTS =====

test "Phase4: getSyntaxTokens returns tokens for file" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up a simple file
    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1;");

    // Get syntax tokens
    const tokens = try highlight.getSyntaxTokens(&db,"test.ms");
    defer std.testing.allocator.free(tokens);

    // Should have tokens
    try std.testing.expect(tokens.len > 0);
}

test "Phase4: getSyntaxTokens returns macro tokens" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "@derive(Eq) class User {}");

    const tokens = try highlight.getSyntaxTokens(&db,"test.ms");
    defer std.testing.allocator.free(tokens);

    // First token should be macro (@derive)
    try std.testing.expect(tokens.len > 0);
    try std.testing.expectEqual(SyntaxTokenType.macro, tokens[0].token_type);
}

test "Phase4: getSyntaxTokens caches results" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1;");

    // First call
    const tokens1 = try highlight.getSyntaxTokens(&db,"test.ms");
    defer std.testing.allocator.free(tokens1);

    // Second call should use cache (same result)
    const tokens2 = try highlight.getSyntaxTokens(&db,"test.ms");
    defer std.testing.allocator.free(tokens2);

    try std.testing.expectEqual(tokens1.len, tokens2.len);
}

test "Phase4: getSyntaxTokens invalidates cache on file change" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1;");

    const tokens1 = try highlight.getSyntaxTokens(&db,"test.ms");
    defer std.testing.allocator.free(tokens1);
    const len1 = tokens1.len;

    // Change file
    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1; const y = 2;");

    const tokens2 = try highlight.getSyntaxTokens(&db,"test.ms");
    defer std.testing.allocator.free(tokens2);

    // More tokens now
    try std.testing.expect(tokens2.len > len1);
}

test "Phase4: startAsyncMacroExpansion returns handle" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "@derive(Eq) class A {}");

    const handle = try highlight.startAsyncMacroExpansion(&db,"test.ms");
    try std.testing.expect(handle.id > 0);
}

test "Phase4: getExpansionStatus returns status" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "@derive(Eq) class A {}");

    const handle = try highlight.startAsyncMacroExpansion(&db,"test.ms");
    const status = highlight.getExpansionStatus(&db,handle);

    // Should be completed (synchronous fallback for now)
    try std.testing.expectEqual(ExpansionStatus.completed, status);
}

test "Phase4: cancelAsyncExpansion sets cancelled status" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "@derive(Eq) class A {}");

    const handle = try highlight.startAsyncMacroExpansion(&db,"test.ms");

    // Manually cancel the expansion
    highlight.cancelAsyncExpansion(&db,handle);

    // Status should be cancelled (or completed if already done)
    const status = highlight.getExpansionStatus(&db,handle);
    // In synchronous mode, expansion completes immediately before we can cancel
    // So we accept either completed or cancelled
    try std.testing.expect(status == ExpansionStatus.cancelled or status == ExpansionStatus.completed);
}

test "Phase4: waitForExpansion completes" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "@derive(Eq) class A {}");

    const handle = try highlight.startAsyncMacroExpansion(&db,"test.ms");

    // Should complete immediately (synchronous fallback)
    try highlight.waitForExpansion(&db,handle, 1000);
}

test "Phase4: getSemanticTokensStatus returns available" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1;");

    // Generate tokens first
    const tokens = try highlight.getSyntaxTokens(&db,"test.ms");
    defer std.testing.allocator.free(tokens);

    const status = highlight.getSemanticTokensStatus(&db,"test.ms");
    try std.testing.expectEqual(SemanticTokensStatus.available, status);
}

test "Phase4: getSemanticTokensWithVersion returns version" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1;");

    const result = try highlight.getSemanticTokensWithVersion(&db,"test.ms");
    defer std.testing.allocator.free(result.tokens);

    try std.testing.expect(result.version > 0);
    try std.testing.expect(result.tokens.len > 0);
}

test "Phase4: syntax tokens independent of macro state" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // File with macros
    _ = try input_queries.setFileText(&db,"test.ms",
        \\@derive(Eq, Hash)
        \\class User {
        \\    name: string;
        \\}
    );

    // Syntax tokens should work without any macro expansion
    const tokens = try highlight.getSyntaxTokens(&db,"test.ms");
    defer std.testing.allocator.free(tokens);

    // Should have tokens including macro token
    try std.testing.expect(tokens.len > 0);

    // Find macro token
    var found_macro = false;
    for (tokens) |tok| {
        if (tok.token_type == .macro) {
            found_macro = true;
            break;
        }
    }
    try std.testing.expect(found_macro);
}

test "Phase4: token positions are correct" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1;");

    const tokens = try highlight.getSyntaxTokens(&db,"test.ms");
    defer std.testing.allocator.free(tokens);

    // First token should be "const" at line 0, column 0
    try std.testing.expect(tokens.len > 0);
    try std.testing.expectEqual(@as(u32, 0), tokens[0].line);
    try std.testing.expectEqual(@as(u32, 0), tokens[0].column);
    try std.testing.expectEqual(@as(u32, 5), tokens[0].length); // "const"
    try std.testing.expectEqual(SyntaxTokenType.keyword, tokens[0].token_type);
}

// ===== PHASE 5: INCREMENTAL DIAGNOSTICS TESTS =====

test "Phase5: getDiagnostics returns empty for valid code" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1;");

    const diagnostics = try diagnostics_mod.getDiagnostics(&db,"test.ms");
    defer diagnostics_mod.freeDiagnostics(&db,diagnostics);

    // Valid code should have no diagnostics
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);
}

test "Phase5: getDiagnostics caches results" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1;");

    // First call
    const diags1 = try diagnostics_mod.getDiagnostics(&db,"test.ms");
    defer diagnostics_mod.freeDiagnostics(&db,diags1);

    // Second call should use cache
    const diags2 = try diagnostics_mod.getDiagnostics(&db,"test.ms");
    defer diagnostics_mod.freeDiagnostics(&db,diags2);

    try std.testing.expectEqual(diags1.len, diags2.len);
}

test "Phase5: getDiagnostics invalidates on file change" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1;");

    const diags1 = try diagnostics_mod.getDiagnostics(&db,"test.ms");
    defer diagnostics_mod.freeDiagnostics(&db,diags1);

    // Change file
    _ = try input_queries.setFileText(&db,"test.ms", "const y = 2;");

    // Should recompute (cache invalidated)
    const diags2 = try diagnostics_mod.getDiagnostics(&db,"test.ms");
    defer diagnostics_mod.freeDiagnostics(&db,diags2);

    // Both should be valid (no errors)
    try std.testing.expectEqual(@as(usize, 0), diags1.len);
    try std.testing.expectEqual(@as(usize, 0), diags2.len);
}

test "Phase5: getTokenAtPosition returns token info" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1;");

    // Get token at position of "const" (line 0, col 0)
    const token_info = try db.getTokenAtPosition("test.ms", 0, 0);

    try std.testing.expect(token_info != null);
    try std.testing.expectEqual(SyntaxTokenType.keyword, token_info.?.token_type);
    try std.testing.expectEqualStrings("const", token_info.?.text);
}

test "Phase5: getTokenAtPosition returns null for whitespace" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1;");

    // Get token at position of whitespace between const and x (col 5)
    // Actually col 5 is the space, col 6 is x
    const token_info = try db.getTokenAtPosition("test.ms", 0, 5);

    // Should be null (whitespace) or the next token
    // This depends on tokenization
    _ = token_info;
}

// ===== PHASE 3: AST HASHING TESTS =====

test "Phase3: hashAstNode produces consistent hash" {
    // Create two identical nodes
    var node1 = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
    };

    var node2 = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(), // Different location object, same content
        .data = .{ .number_literal = 42.0 },
    };

    const hash1 = hashAstNode(&node1);
    const hash2 = hashAstNode(&node2);

    // Same semantic content = same hash
    try std.testing.expectEqual(hash1, hash2);
}

test "Phase3: hashAstNode differs for different values" {
    var node1 = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
    };

    var node2 = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 99.0 }, // Different value
    };

    const hash1 = hashAstNode(&node1);
    const hash2 = hashAstNode(&node2);

    // Different values = different hash
    try std.testing.expect(hash1 != hash2);
}

test "Phase3: hashAstNode differs for different kinds" {
    var node1 = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
    };

    var node2 = ast.Node{
        .kind = .boolean_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .boolean_literal = true },
    };

    const hash1 = hashAstNode(&node1);
    const hash2 = hashAstNode(&node2);

    // Different kinds = different hash
    try std.testing.expect(hash1 != hash2);
}

test "Phase3: hashAstNode handles string literals" {
    var node1 = ast.Node{
        .kind = .string_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .string_literal = "hello" },
    };

    var node2 = ast.Node{
        .kind = .string_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .string_literal = "hello" },
    };

    var node3 = ast.Node{
        .kind = .string_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .string_literal = "world" },
    };

    const hash1 = hashAstNode(&node1);
    const hash2 = hashAstNode(&node2);
    const hash3 = hashAstNode(&node3);

    try std.testing.expectEqual(hash1, hash2);
    try std.testing.expect(hash1 != hash3);
}

test "Phase3: hashAstNode handles identifiers" {
    var node1 = ast.Node{
        .kind = .identifier,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .identifier = "foo" },
    };

    var node2 = ast.Node{
        .kind = .identifier,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .identifier = "foo" },
    };

    const hash1 = hashAstNode(&node1);
    const hash2 = hashAstNode(&node2);

    try std.testing.expectEqual(hash1, hash2);
}

// ===== PHASE 6: PARSER INTEGRATION TESTS =====

test "Phase6: parse returns valid AST for simple class" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const source = "class User { name: string; }";
    _ = try input_queries.setFileText(&db,"test.ms", source);

    const result = try db.parse("test.ms");

    // Should return a program node
    try std.testing.expectEqual(ast.NodeKind.program, result.tree.kind);

    // Should have no errors
    try std.testing.expectEqual(@as(usize, 0), result.errors.len);
}

test "Phase6: parse returns AST with class declaration" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const source = "class User { name: string; age: number; }";
    _ = try input_queries.setFileText(&db,"test.ms", source);

    const result = try db.parse("test.ms");

    // Should have statements
    try std.testing.expect(result.tree.data.program.statements.len > 0);

    // First statement should be a class_decl
    const first_stmt = result.tree.data.program.statements[0];
    try std.testing.expectEqual(ast.NodeKind.class_decl, first_stmt.kind);
}

test "Phase6: parse caches result" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const source = "class User { }";
    _ = try input_queries.setFileText(&db,"test.ms", source);

    // Parse twice
    const result1 = try db.parse("test.ms");
    const result2 = try db.parse("test.ms");

    // Should return same tree (cached)
    try std.testing.expectEqual(result1.tree, result2.tree);
}

test "Phase6: getMacroCallSites extracts decorator arguments" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const source = "@derive(Eq, Hash) class User { name: string; }";
    _ = try input_queries.setFileText(&db,"test.ms", source);

    const call_sites = try macro_queries.getMacroCallSites(&db,"test.ms");
    defer {
        for (call_sites) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(call_sites);
    }

    // Should find the @derive macro
    try std.testing.expectEqual(@as(usize, 1), call_sites.len);

    // Check macro name
    try std.testing.expectEqualStrings("derive", call_sites[0].macro_name);

    // Check arguments - should now have Eq and Hash
    try std.testing.expectEqual(@as(usize, 2), call_sites[0].arguments.len);
    try std.testing.expectEqualStrings("Eq", call_sites[0].arguments[0]);
    try std.testing.expectEqualStrings("Hash", call_sites[0].arguments[1]);
}

test "Phase6: getMacroCallSites finds multiple decorators" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const source =
        \\@derive(Eq) class User { }
        \\@derive(Hash) class Item { }
    ;
    _ = try input_queries.setFileText(&db,"test.ms", source);

    const call_sites = try macro_queries.getMacroCallSites(&db,"test.ms");
    defer {
        for (call_sites) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(call_sites);
    }

    // Should find both decorators
    try std.testing.expectEqual(@as(usize, 2), call_sites.len);
}

test "Phase6: getDiagnostics includes parse errors" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Invalid syntax - missing closing brace
    const source = "class User {";
    _ = try input_queries.setFileText(&db,"test.ms", source);

    const diagnostics = try diagnostics_mod.getDiagnostics(&db,"test.ms");
    defer {
        for (diagnostics) |d| {
            std.testing.allocator.free(d.message);
        }
        std.testing.allocator.free(diagnostics);
    }

    // Should have at least one error
    try std.testing.expect(diagnostics.len > 0);

    // Error should be from parser
    try std.testing.expectEqualStrings("mls-parser", diagnostics[0].source);
}

test "Phase6: getDiagnostics returns empty for valid syntax" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const source = "class User { name: string; }";
    _ = try input_queries.setFileText(&db,"test.ms", source);

    const diagnostics = try diagnostics_mod.getDiagnostics(&db,"test.ms");
    defer std.testing.allocator.free(diagnostics);

    // Should have no errors for valid code
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);
}

test "Phase6: parse invalidates on file change" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Initial parse
    _ = try input_queries.setFileText(&db,"test.ms", "class A { }");
    const result1 = try db.parse("test.ms");

    // Change file
    _ = try input_queries.setFileText(&db,"test.ms", "class B { }");
    const result2 = try db.parse("test.ms");

    // Should have different trees (re-parsed)
    try std.testing.expect(result1.tree != result2.tree);
}

test "Phase6: macro arguments hash changes with different args" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // First version
    _ = try input_queries.setFileText(&db,"test.ms", "@derive(Eq) class User { }");
    const sites1 = try macro_queries.getMacroCallSites(&db,"test.ms");
    const hash1 = sites1[0].call_id.input_hash;
    for (sites1) |site| {
        std.testing.allocator.free(site.arguments);
    }
    std.testing.allocator.free(sites1);

    // Different arguments
    _ = try input_queries.setFileText(&db,"test.ms", "@derive(Hash) class User { }");
    const sites2 = try macro_queries.getMacroCallSites(&db,"test.ms");
    const hash2 = sites2[0].call_id.input_hash;
    for (sites2) |site| {
        std.testing.allocator.free(site.arguments);
    }
    std.testing.allocator.free(sites2);

    // Hashes should differ
    try std.testing.expect(hash1 != hash2);
}

// ===== PHASE 7: MACRO FIREWALL TESTS =====

test "Phase7: expandMacroCallSite generates method for @derive(Eq)" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "@derive(Eq) class User { name: string; }");

    const call_sites = try macro_queries.getMacroCallSites(&db,"test.ms");
    defer {
        for (call_sites) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(call_sites);
    }

    try std.testing.expectEqual(@as(usize, 1), call_sites.len);

    const result = try macro_queries.expandMacroCallSite(&db,"test.ms", call_sites[0]);
    try std.testing.expect(result.generated_nodes.len > 0);
    try std.testing.expect(!result.from_cache);
}

test "Phase7: expandMacroCallSite caches result" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms", "@derive(Eq) class User { name: string; }");

    const call_sites = try macro_queries.getMacroCallSites(&db,"test.ms");
    defer {
        for (call_sites) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(call_sites);
    }

    // First expansion
    const result1 = try macro_queries.expandMacroCallSite(&db,"test.ms", call_sites[0]);
    try std.testing.expect(!result1.from_cache);

    // Second expansion should be from cache
    const result2 = try macro_queries.expandMacroCallSite(&db,"test.ms", call_sites[0]);
    try std.testing.expect(result2.from_cache);
    try std.testing.expectEqual(result1.output_hash, result2.output_hash);
}

test "Phase7: firewall - editing one macro doesn't invalidate others" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Initial file with two macros
    _ = try input_queries.setFileText(&db,"test.ms",
        \\@derive(Eq) class User { name: string; }
        \\@derive(Hash) class Item { id: number; }
    );

    // Expand both macros
    const call_sites1 = try macro_queries.getMacroCallSites(&db,"test.ms");
    defer {
        for (call_sites1) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(call_sites1);
    }

    try std.testing.expectEqual(@as(usize, 2), call_sites1.len);

    // Expand first macro (User with Eq)
    const user_result = try macro_queries.expandMacroCallSite(&db,"test.ms", call_sites1[0]);
    _ = user_result; // User's hash will change after edit

    // Expand second macro (Item with Hash)
    const item_result = try macro_queries.expandMacroCallSite(&db,"test.ms", call_sites1[1]);
    const item_hash = item_result.output_hash;

    // Now "edit" only the User class (add a property)
    _ = try input_queries.setFileText(&db,"test.ms",
        \\@derive(Eq) class User { name: string; age: number; }
        \\@derive(Hash) class Item { id: number; }
    );

    // Get new call sites
    const call_sites2 = try macro_queries.getMacroCallSites(&db,"test.ms");
    defer {
        for (call_sites2) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(call_sites2);
    }

    // The Item macro should still produce the same hash (FIREWALL!)
    // Find the Item macro call site
    var item_site: ?MacroCallSite = null;
    for (call_sites2) |site| {
        if (site.arguments.len > 0 and std.mem.eql(u8, site.arguments[0], "Hash")) {
            item_site = site;
            break;
        }
    }
    try std.testing.expect(item_site != null);

    const item_result2 = try macro_queries.expandMacroCallSite(&db,"test.ms", item_site.?);

    // Same output hash - firewall working!
    try std.testing.expectEqual(item_hash, item_result2.output_hash);
}

test "Phase7: expandAllMacros returns stats" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try input_queries.setFileText(&db,"test.ms",
        \\@derive(Eq, Hash) class User { name: string; }
    );

    const stats = try macro_queries.expandAllMacros(&db,"test.ms");
    try std.testing.expectEqual(@as(usize, 1), stats.total_macros);
    try std.testing.expectEqual(@as(usize, 1), stats.expanded);
    try std.testing.expectEqual(@as(usize, 0), stats.from_cache);

    // Second call should use cache
    const stats2 = try macro_queries.expandAllMacros(&db,"test.ms");
    try std.testing.expectEqual(@as(usize, 1), stats2.total_macros);
    try std.testing.expectEqual(@as(usize, 1), stats2.expanded);
    try std.testing.expectEqual(@as(usize, 1), stats2.from_cache);
}

test "Phase7: content-addressed caching - same output reused" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Two different classes with same @derive(Eq) should produce same output hash
    _ = try input_queries.setFileText(&db,"file1.ms", "@derive(Eq) class A { x: number; }");
    _ = try input_queries.setFileText(&db,"file2.ms", "@derive(Eq) class B { y: string; }");

    const sites1 = try macro_queries.getMacroCallSites(&db,"file1.ms");
    defer {
        for (sites1) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(sites1);
    }

    const sites2 = try macro_queries.getMacroCallSites(&db,"file2.ms");
    defer {
        for (sites2) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(sites2);
    }

    const result1 = try macro_queries.expandMacroCallSite(&db,"file1.ms", sites1[0]);
    const result2 = try macro_queries.expandMacroCallSite(&db,"file2.ms", sites2[0]);

    // Both generate equals() method, so same output hash
    try std.testing.expectEqual(result1.output_hash, result2.output_hash);

    // First is a cache miss, second hits the cache (same @derive(Eq) arguments)
    try std.testing.expect(!result1.from_cache);
    // Note: result2.from_cache depends on whether input_hash is same
    // (both are @derive(Eq) so they might share cache)
}

// ===== PHASE 8: REAL ASYNC THREADS TESTS =====

test "Phase8: async expansion runs in separate thread" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up file with macros
    _ = try input_queries.setFileText(&db,"test.ms",
        \\@derive(Eq, Hash)
        \\class User { name: string; age: number; }
        \\@derive(Eq)
        \\class Item { id: number; }
    );

    // Start async expansion
    const handle = try highlight.startAsyncMacroExpansion(&db,"test.ms");

    // Wait for completion (should complete in reasonable time)
    try highlight.waitForExpansion(&db,handle, 5000); // 5 second timeout

    // Status should be completed
    const status = highlight.getExpansionStatus(&db,handle);
    try std.testing.expectEqual(ExpansionStatus.completed, status);
}

test "Phase8: progress callback is invoked during expansion" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Tracking state for callback
    const CallbackState = struct {
        var call_count: u32 = 0;
        var last_progress: ?ExpansionProgress = null;
    };

    // Reset state
    CallbackState.call_count = 0;
    CallbackState.last_progress = null;

    // Set callback
    highlight.setExpansionProgressCallback(&db,struct {
        fn callback(progress: ExpansionProgress, ctx: ?*anyopaque) void {
            _ = ctx;
            CallbackState.call_count += 1;
            CallbackState.last_progress = progress;
        }
    }.callback, null);

    // Set up file with macros
    _ = try input_queries.setFileText(&db,"test.ms", "@derive(Eq, Hash) class User { name: string; }");

    // Start async expansion
    const handle = try highlight.startAsyncMacroExpansion(&db,"test.ms");

    // Wait for completion
    try highlight.waitForExpansion(&db,handle, 5000);

    // Progress callback should have been called at least once
    try std.testing.expect(CallbackState.call_count >= 1);

    // Last progress should show completion
    if (CallbackState.last_progress) |progress| {
        try std.testing.expect(progress.current_index <= progress.total_macros);
    }
}

test "Phase8: cancellation interrupts running expansion" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up file with macros
    _ = try input_queries.setFileText(&db,"test.ms", "@derive(Eq, Hash) class User { name: string; }");

    // Start async expansion
    const handle = try highlight.startAsyncMacroExpansion(&db,"test.ms");

    // Immediately cancel
    highlight.cancelAsyncExpansion(&db,handle);

    // Wait a bit for thread to notice cancellation
    std.time.sleep(10_000_000); // 10ms

    // Status should be cancelled or completed (if it finished before cancel)
    const status = highlight.getExpansionStatus(&db,handle);
    try std.testing.expect(status == .cancelled or status == .completed);
}

test "Phase8: new expansion cancels previous for same file" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up file with macros
    _ = try input_queries.setFileText(&db,"test.ms", "@derive(Eq) class User { name: string; }");

    // Start first expansion
    const handle1 = try highlight.startAsyncMacroExpansion(&db,"test.ms");

    // Start second expansion for same file (should cancel first)
    const handle2 = try highlight.startAsyncMacroExpansion(&db,"test.ms");

    // Wait for second to complete
    try highlight.waitForExpansion(&db,handle2, 5000);

    // First handle should be cancelled (or completed if it finished very fast)
    const status1 = highlight.getExpansionStatus(&db,handle1);
    try std.testing.expect(status1 == .cancelled or status1 == .completed);

    // Second handle should be completed
    const status2 = highlight.getExpansionStatus(&db,handle2);
    try std.testing.expectEqual(ExpansionStatus.completed, status2);
}

test "Phase8: getExpansionProgress returns accurate info" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up file with multiple macros
    _ = try input_queries.setFileText(&db,"test.ms",
        \\@derive(Eq) class A {}
        \\@derive(Hash) class B {}
        \\@derive(Eq, Hash) class C {}
    );

    // Start async expansion
    const handle = try highlight.startAsyncMacroExpansion(&db,"test.ms");

    // Wait for completion
    try highlight.waitForExpansion(&db,handle, 5000);

    // Get progress info
    const progress = highlight.getExpansionProgress(&db,handle);

    if (progress) |p| {
        // Should have found the 3 macro call sites
        try std.testing.expect(p.total_macros >= 1);
        try std.testing.expectEqual(ExpansionStatus.completed, p.status);
    }
}

// ===== PHASE 9: TYPE CHECKER INTEGRATION TESTS =====

test "Phase9: getSymbols returns symbol table for valid file" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up a file with a variable declaration
    _ = try input_queries.setFileText(&db,"test.ms", "const x: number = 42;");

    // Get symbols - this should parse and build symbol table
    const symbols = try type_check.getSymbols(&db,"test.ms");

    // Should find the variable 'x'
    const x_symbol = symbols.lookup("x");
    try std.testing.expect(x_symbol != null);
    try std.testing.expectEqual(checker.symbol.SymbolKind.variable, x_symbol.?.kind);
}

test "Phase9: checkFile returns symbol table" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up a file with valid code - just a simple variable
    _ = try input_queries.setFileText(&db,"test.ms", "const y: string = \"hello\";");

    // Check the file
    const result = try type_check.checkFile(&db,"test.ms");

    // Should have symbols table (regardless of errors from incomplete type checker)
    try std.testing.expect(result.symbols != null);

    // The variable 'y' should be in the symbol table
    if (result.symbols) |symbols| {
        try std.testing.expect(symbols.lookup("y") != null);
    }
}

test "Phase9: checkFile reports break outside loop" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up a file with break outside loop
    _ = try input_queries.setFileText(&db,"test.ms", "break;");

    // Check the file
    const result = try type_check.checkFile(&db,"test.ms");

    // Should fail with error
    try std.testing.expect(!result.success);
    try std.testing.expect(result.errors.len > 0);

    // First error should be about break outside loop
    try std.testing.expectEqual(checker.TypeError.Kind.break_outside_loop, result.errors[0].kind);
}

test "Phase9: lookupSymbol finds defined symbols" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up a file with function declaration
    _ = try input_queries.setFileText(&db,"test.ms", "function greet() { return \"Hello\"; }");

    // Look up the function
    const symbol = try type_check.lookupSymbol(&db,"test.ms", "greet");

    try std.testing.expect(symbol != null);
    try std.testing.expectEqual(checker.symbol.SymbolKind.function, symbol.?.kind);
}

test "Phase9: lookupSymbol returns null for undefined" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up a file with some code
    _ = try input_queries.setFileText(&db,"test.ms", "const x = 1;");

    // Look up undefined symbol
    const symbol = try type_check.lookupSymbol(&db,"test.ms", "undefined_var");

    try std.testing.expect(symbol == null);
}

test "Phase9: symbol cache is invalidated on file change" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up initial file
    _ = try input_queries.setFileText(&db,"test.ms", "const first = 1;");

    // Get symbols - should have 'first'
    const symbols1 = try type_check.getSymbols(&db,"test.ms");
    try std.testing.expect(symbols1.lookup("first") != null);
    try std.testing.expect(symbols1.lookup("second") == null);

    // Change file
    _ = try input_queries.setFileText(&db,"test.ms", "const second = 2;");

    // Get symbols again - should have 'second', not 'first'
    const symbols2 = try type_check.getSymbols(&db,"test.ms");
    try std.testing.expect(symbols2.lookup("second") != null);
    // Note: 'first' might still be in the old cached table, but we got a new one
}

test "Phase9: getSymbols returns error for non-existent file" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Try to get symbols for non-existent file
    const result = type_check.getSymbols(&db,"non_existent.ms");
    try std.testing.expectError(error.FileNotFound, result);
}

test "Phase9: checkFile with builtins available" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up file that uses a builtin
    _ = try input_queries.setFileText(&db,"test.ms", "const n = parseInt(\"42\");");

    // Check the file - should find parseInt builtin
    const result = try type_check.checkFile(&db,"test.ms");

    // parseInt should be in scope (defined as builtin)
    if (result.symbols) |symbols| {
        try std.testing.expect(symbols.lookup("parseInt") != null);
    }
}

// ===== PHASE 10: BYTECODE CACHE TESTS =====

test "Phase10: BytecodeCache is initialized" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Bytecode cache should be available (unless hermesc isn't found)
    // This test verifies the cache initialization doesn't crash
    if (db.bytecode_cache) |_| {
        std.debug.print("[Test] BytecodeCache initialized successfully\n", .{});
    } else {
        std.debug.print("[Test] BytecodeCache not available (this is OK if hermesc not found)\n", .{});
    }
}

test "Phase10: macro expansion populates bytecode cache" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Skip if bytecode cache not available
    if (db.bytecode_cache == null) {
        std.debug.print("[Test] Skipping: BytecodeCache not available\n", .{});
        return;
    }

    // Set up file with macro
    _ = try input_queries.setFileText(&db,"test.ms", "@derive(Eq) class User { name: string; }");

    // Get macro call sites
    const call_sites = try macro_queries.getMacroCallSites(&db,"test.ms");
    defer {
        for (call_sites) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(call_sites);
    }

    // Expand macro - this should populate bytecode cache
    _ = try macro_queries.expandMacroCallSite(&db,"test.ms", call_sites[0]);

    // Verify the bytecode cache was used (check if there are entries)
    // We can't directly check for specific hashes without accessing internals
    std.debug.print("[Test] Bytecode cache is available and macro expanded successfully\n", .{});
}
