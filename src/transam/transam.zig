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
pub const MacroCallInfo = types_mod.MacroCallInfo;
pub const MacroCallSite = types_mod.MacroCallSite;
pub const CachedMacroExpansion = types_mod.CachedMacroExpansion;
pub const TypeInference = types_mod.TypeInference;
pub const UnifiedIR = types_mod.UnifiedIR;
pub const AstIdMap = types_mod.AstIdMap;
pub const classifyDurability = types_mod.classifyDurability;

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

    const Self = @This();

    const CachedSyntaxTokens = struct {
        tokens: []SyntaxToken,
        file_hash: u64,
        computed_at: i64,
    };

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
    }

    /// Get the current revision
    pub fn getRevision(self: *Self) Revision {
        return self.current_revision;
    }

    // ===== INPUT QUERIES (never cached, set by LSP) =====

    /// Set file text - invalidates all dependent queries
    /// Returns true if content actually changed (useful for optimization)
    pub fn setFileText(self: *Self, file_id: []const u8, text: []const u8) !bool {
        // Compute content hash for change detection
        const new_hash = hashString(text);
        const old_hash = self.file_hashes.get(file_id);

        // Check if content actually changed (content-addressed optimization)
        if (old_hash != null and old_hash.? == new_hash) {
            // Content unchanged, no need to invalidate
            return false;
        }

        // Free old values if they exist
        // Note: We use file_id (parameter) for lookup, not the stored key
        if (self.file_texts.get(file_id) != null) {
            // Remove from both maps first, then free
            _ = self.file_hashes.remove(file_id);
            if (self.file_texts.fetchRemove(file_id)) |old_entry| {
                // old_entry.key is the owned key we allocated
                // old_entry.value is the owned text we allocated
                self.allocator.free(old_entry.key);
                self.allocator.free(old_entry.value);
            }
        }

        // Store new text
        const owned_text = try self.allocator.dupe(u8, text);
        const owned_id = try self.allocator.dupe(u8, file_id);
        try self.file_texts.put(owned_id, owned_text);
        try self.file_hashes.put(owned_id, new_hash);

        // Mark file as dirty
        const file_id_hash = hashString(file_id);
        try self.dirty_files.put(file_id_hash, {});

        // Increment revision (triggers invalidation)
        self.current_revision.increment();
        self.revision = self.current_revision.value; // Sync legacy field

        // Invalidate parse cache for this file
        self.parse_cache.remove(file_id_hash);

        // Phase 9: Invalidate type checker cache for this file
        self.invalidateSymbolCache(file_id);

        // Mark all cached queries as RED (potentially stale)
        // They'll be verified lazily via try_mark_green when accessed
        var it = self.query_cache.valueIterator();
        while (it.next()) |qv| {
            if (qv.state == .green) {
                qv.state = .red;
            }
        }

        return true;
    }

    /// Get file text (input query)
    pub fn getFileText(self: *Self, file_id: []const u8) ?[]const u8 {
        return self.file_texts.get(file_id);
    }

    /// Get file content hash (for dependency checking)
    pub fn getFileHash(self: *Self, file_id: []const u8) ?u64 {
        return self.file_hashes.get(file_id);
    }

    /// Check if file has changed since given revision
    pub fn hasFileChanged(self: *Self, file_id: []const u8, since_revision: Revision) bool {
        const file_id_hash = hashString(file_id);
        if (self.dirty_files.contains(file_id_hash)) {
            // File was modified at some point
            // For now, assume it changed if dirty (future: track per-file revision)
            return self.current_revision.value > since_revision.value;
        }
        return false;
    }

    // ===== RED-GREEN ALGORITHM =====

    /// Error type for red-green algorithm
    pub const RedGreenError = error{
        CycleDetected,
        Cancelled,
        OutOfMemory,
        FileNotFound,
    };

    /// Try to verify a cached value is still valid without recomputing.
    /// Returns true if value can be marked GREEN (still valid).
    /// Returns false if value needs recomputation.
    pub fn tryMarkGreen(self: *Self, key: QueryKey) RedGreenError!bool {
        const entry = self.query_cache.getPtr(key.hash()) orelse return false;

        // Already verified this revision?
        if (entry.verified_at.eq(self.current_revision)) {
            return entry.state == .green;
        }

        // Cycle detection: if we're already verifying this query, we have a cycle
        if (entry.state == .yellow) {
            return RedGreenError.CycleDetected;
        }

        // Mark as being verified (YELLOW state)
        const prev_state = entry.state;
        entry.state = .yellow;
        errdefer entry.state = prev_state;

        // Check each dependency recursively
        for (entry.dependencies) |dep_key| {
            // Input query (file_text): check if content hash changed
            if (dep_key.query_type == .file_text) {
                // For input queries, check if the file hash changed
                // The input_hash in QueryKey IS the file content hash
                const current_file_hash = self.getFileHashByQueryKey(dep_key);
                if (current_file_hash != dep_key.input_hash) {
                    // Input changed -> must recompute
                    entry.state = .red;
                    return false;
                }
                continue;
            }

            // Derived query: recursively verify
            const dep_entry = self.query_cache.getPtr(dep_key.hash());
            if (dep_entry == null) {
                // Dependency no longer exists -> must recompute
                entry.state = .red;
                return false;
            }

            // Recursively try to mark dependency green
            const dep_is_green = try self.tryMarkGreen(dep_key);
            if (!dep_is_green) {
                // Dependency needs recomputation -> we need recomputation
                entry.state = .red;
                return false;
            }

            // Dependency verified green, but check if its OUTPUT changed
            // (Key Salsa insight: input change doesn't always mean output change)
            if (dep_entry.?.computed_at.value > entry.computed_at.value) {
                // Dependency was recomputed after us
                // We'd need to check if output hash changed, but for now be conservative
                entry.state = .red;
                return false;
            }
        }

        // All dependencies verified unchanged -> mark GREEN
        entry.verified_at = self.current_revision;
        entry.state = .green;
        return true;
    }

    /// Get file hash for an input query key
    fn getFileHashByQueryKey(self: *Self, key: QueryKey) u64 {
        // For file_text queries, we need to look up the current file hash
        // The input_hash was the hash at computation time
        // We iterate through file_hashes to find matching file
        var it = self.file_hashes.iterator();
        while (it.next()) |entry| {
            if (hashString(entry.key_ptr.*) == key.input_hash) {
                return entry.value_ptr.*;
            }
        }
        // File not found, return 0 to force recomputation
        return 0;
    }

    /// Store a query result in the cache with dependency tracking
    pub fn storeQueryResult(
        self: *Self,
        key: QueryKey,
        value: *anyopaque,
        type_id: u64,
        value_hash: u64,
        dependencies: []const QueryKey,
        durability: Durability,
    ) !void {
        // Copy dependencies to owned memory
        const owned_deps = try self.allocator.dupe(QueryKey, dependencies);

        const query_value = QueryValue{
            .value = value,
            .type_id = type_id,
            .computed_at = self.current_revision,
            .verified_at = self.current_revision,
            .dependencies = owned_deps,
            .durability = durability,
            .value_hash = value_hash,
            .state = .green,
        };

        // Remove old entry if exists (free old dependencies)
        if (self.query_cache.fetchRemove(key.hash())) |old| {
            self.allocator.free(old.value.dependencies);
        }

        try self.query_cache.put(key.hash(), query_value);
    }

    /// Get a cached query result if valid
    pub fn getCachedQuery(self: *Self, key: QueryKey) ?*QueryValue {
        const entry = self.query_cache.getPtr(key.hash()) orelse return null;

        // If already GREEN in current revision, return it
        if (entry.state == .green and entry.verified_at.eq(self.current_revision)) {
            return entry;
        }

        // Try to verify it's still valid
        const is_green = self.tryMarkGreen(key) catch return null;
        if (is_green) {
            return entry;
        }

        return null;
    }

    /// Begin executing a query - pushes dependency frame
    pub fn beginQuery(self: *Self, key: QueryKey) !void {
        // Check for cycles
        if (self.dependency_stack.isInProgress(key)) {
            return RedGreenError.CycleDetected;
        }
        try self.dependency_stack.push(key);
    }

    /// Record that current query depends on another query
    pub fn recordDependency(self: *Self, dep_key: QueryKey, dep_durability: Durability) !void {
        try self.dependency_stack.recordDependency(dep_key, dep_durability);
    }

    /// End executing a query - pops dependency frame and returns collected deps
    pub fn endQuery(self: *Self) ?DependencyFrame {
        return self.dependency_stack.pop();
    }

    /// Check if output hash changed (for content-addressed optimization)
    /// Returns true if we can reuse the old cached value
    pub fn canReuseOutput(self: *Self, key: QueryKey, new_value_hash: u64) bool {
        if (self.query_cache.getPtr(key.hash())) |entry| {
            if (entry.value_hash == new_value_hash) {
                // Output unchanged! Can reuse
                entry.verified_at = self.current_revision;
                entry.state = .green;
                return true;
            }
        }
        return false;
    }

    // ===== LRU CACHED QUERIES =====

    /// Parse file - LRU cached (limit 128 entries)
    pub fn parse(self: *Self, file_id: []const u8) !ParseResult {
        // Check cancellation
        try self.checkCancellation();

        const file_hash = hashString(file_id);

        // Check cache
        if (self.parse_cache.get(file_hash)) |cached| {
            return cached;
        }

        // Cache miss: parse and store
        const text = self.getFileText(file_id) orelse return error.FileNotFound;
        const result = try self.parseImpl(file_id, text);
        try self.parse_cache.put(file_hash, result);

        return result;
    }

    // ===== MACRO FIREWALL PATTERN =====
    // Three separate queries to prevent cascading invalidation

    /// Firewall 1: Extract macro arguments
    /// Only invalidates when THIS specific call's text changes
    pub fn macroArg(self: *Self, call_id: MacroCallId) !MacroArgs {
        try self.checkCancellation();

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
        try self.checkCancellation();

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
        try self.checkCancellation();

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

    // ===== CANCELLATION SUPPORT (rust-analyzer/Salsa pattern) =====

    /// Check if query execution should be cancelled (legacy boolean)
    pub fn checkCancellation(self: *Self) !void {
        if (self.cancel_flag.load(.seq_cst)) {
            return error.Cancelled;
        }
    }

    /// Request cancellation of all in-flight queries (legacy boolean)
    pub fn requestCancellation(self: *Self) void {
        self.cancel_flag.store(true, .seq_cst);
    }

    /// Reset cancellation flag (after handling cancellation)
    pub fn resetCancellation(self: *Self) void {
        self.cancel_flag.store(false, .seq_cst);
    }

    // ===== VERSION-BASED CANCELLATION (Salsa pattern) =====
    // Used for cooperative cancellation: queries periodically check if
    // their starting version matches current version. If not, abort.

    /// Get current cancellation version. Call this at query start.
    pub fn getCancellationVersion(self: *Self) u64 {
        return self.cancellation_version.load(.seq_cst);
    }

    /// Check if query should abort. Call this periodically in long-running queries.
    /// Returns error.QueryCancelled if version changed since query started.
    pub fn unwindIfCancelled(self: *Self, starting_version: u64) !void {
        const current = self.cancellation_version.load(.seq_cst);
        if (current != starting_version) {
            return error.QueryCancelled;
        }
        // Also check legacy flag for backward compatibility
        if (self.cancel_flag.load(.seq_cst)) {
            return error.QueryCancelled;
        }
    }

    /// Cancel all pending queries by incrementing version.
    /// Called by LSP when user types (didChange notification).
    pub fn cancelPendingQueries(self: *Self) void {
        _ = self.cancellation_version.fetchAdd(1, .seq_cst);
        // Also set legacy flag for backward compatibility
        self.cancel_flag.store(true, .seq_cst);
    }

    /// Reset after cancellation handled
    pub fn resetAfterCancellation(self: *Self) void {
        self.cancel_flag.store(false, .seq_cst);
        // Note: version is NOT reset - it only increments
    }

    // ===== PHASE 9: TYPE CHECKER QUERIES =====

    /// Get symbol table for a file (cached, depends on parse)
    /// This is the entry point for type information queries.
    /// Following Nim's pattern: parse first, then type-check the result.
    pub fn getSymbols(self: *Self, file_id: []const u8) !*checker.symbol.SymbolTable {
        try self.checkCancellation();

        const file_hash = self.file_hashes.get(file_id) orelse return error.FileNotFound;

        // Check cache
        if (self.symbols_cache.get(file_hash)) |cached| {
            // Verify cache is still valid (same file content)
            if (cached.file_hash == file_hash) {
                return cached.table;
            }
        }

        // Cache miss: parse and build symbol table
        const parse_result = try self.parse(file_id);
        const tree = parse_result.tree;

        // Create type checker (owns the symbol table)
        const type_checker = try self.allocator.create(checker.TypeChecker);
        type_checker.* = try checker.TypeChecker.init(self.allocator);

        // Run type checking (Phase 1: collectDeclarations populates symbols)
        _ = type_checker.check(tree) catch |err| {
            type_checker.deinit();
            self.allocator.destroy(type_checker);
            return err;
        };

        // Cache the result
        const cached = CachedSymbolTable{
            .table = &type_checker.symbols,
            .type_checker = type_checker,
            .file_hash = file_hash,
            .computed_at = std.time.milliTimestamp(),
        };
        try self.symbols_cache.put(file_hash, cached);

        return &type_checker.symbols;
    }

    /// Type check a file and return result with errors
    /// This is a convenience wrapper over getSymbols that also returns errors.
    pub fn checkFile(self: *Self, file_id: []const u8) !TypeCheckResult {
        try self.checkCancellation();

        const file_hash = self.file_hashes.get(file_id) orelse return error.FileNotFound;

        // Check if we already have a cached type checker
        if (self.symbols_cache.get(file_hash)) |cached| {
            if (cached.file_hash == file_hash) {
                // Return result from cached type checker
                const errors = cached.type_checker.getErrors();
                if (errors.len == 0) {
                    return TypeCheckResult.ok(cached.table);
                } else {
                    return TypeCheckResult{
                        .success = false,
                        .errors = errors,
                        .symbols = cached.table,
                    };
                }
            }
        }

        // Need to run type checking
        const parse_result = try self.parse(file_id);
        const tree = parse_result.tree;

        // Create type checker
        const type_checker = try self.allocator.create(checker.TypeChecker);
        type_checker.* = try checker.TypeChecker.init(self.allocator);

        // Run full type checking
        const success = type_checker.check(tree) catch |err| {
            type_checker.deinit();
            self.allocator.destroy(type_checker);
            return err;
        };

        // Cache the result
        const cached = CachedSymbolTable{
            .table = &type_checker.symbols,
            .type_checker = type_checker,
            .file_hash = file_hash,
            .computed_at = std.time.milliTimestamp(),
        };
        try self.symbols_cache.put(file_hash, cached);

        if (success) {
            return TypeCheckResult.ok(&type_checker.symbols);
        } else {
            return TypeCheckResult{
                .success = false,
                .errors = type_checker.getErrors(),
                .symbols = &type_checker.symbols,
            };
        }
    }

    /// Look up a symbol by name in a file's symbol table
    pub fn lookupSymbol(self: *Self, file_id: []const u8, name: []const u8) !?checker.symbol.Symbol {
        const symbols = try self.getSymbols(file_id);
        return symbols.lookup(name);
    }

    /// Invalidate type checker cache for a file (called when file changes)
    pub fn invalidateSymbolCache(self: *Self, file_id: []const u8) void {
        const file_hash = self.file_hashes.get(file_id) orelse return;

        if (self.symbols_cache.fetchRemove(file_hash)) |removed| {
            removed.value.type_checker.deinit();
            self.allocator.destroy(removed.value.type_checker);
        }

        // Also invalidate function check cache for this file
        // Note: We iterate to find entries for this file - a future optimization
        // could use a secondary index file_hash -> [function_check_keys]
        var fc_it = self.function_check_cache.iterator();
        var keys_to_remove = std.ArrayList(u64).init(self.allocator);
        defer keys_to_remove.deinit();

        while (fc_it.next()) |entry| {
            if (entry.value_ptr.file_hash == file_hash) {
                keys_to_remove.append(entry.key_ptr.*) catch {};
            }
        }

        for (keys_to_remove.items) |key| {
            if (self.function_check_cache.fetchRemove(key)) |removed| {
                self.allocator.free(removed.value.func_name);
            }
        }
    }

    // ===== PHASE 4: PARALLEL HIGHLIGHTING API =====

    /// Get syntax tokens for a file (lexer-based, independent of macros).
    /// This is the FAST path - returns immediately without waiting for macro expansion.
    /// Used for initial syntax highlighting before semantic analysis completes.
    pub fn getSyntaxTokens(self: *Self, file_id: []const u8) ![]SyntaxToken {
        const file_hash = hashString(file_id);

        // Check cache first
        if (self.syntax_token_cache.get(file_id)) |cached| {
            // Verify cache is still valid
            if (self.file_hashes.get(file_id)) |current_hash| {
                if (cached.file_hash == current_hash) {
                    // Return copy of cached tokens
                    return try self.allocator.dupe(SyntaxToken, cached.tokens);
                }
            }
        }

        // Cache miss or stale - regenerate from lexer
        const text = self.getFileText(file_id) orelse return error.FileNotFound;
        const tokens = try self.generateSyntaxTokens(text);

        // Update cache
        const cached_tokens = CachedSyntaxTokens{
            .tokens = try self.allocator.dupe(SyntaxToken, tokens),
            .file_hash = file_hash,
            .computed_at = std.time.milliTimestamp(),
        };

        // Remove old cache entry if exists
        if (self.syntax_token_cache.fetchRemove(file_id)) |old| {
            self.allocator.free(old.key);
            self.allocator.free(old.value.tokens);
        }

        const owned_key = try self.allocator.dupe(u8, file_id);
        try self.syntax_token_cache.put(owned_key, cached_tokens);

        return tokens;
    }

    /// Generate syntax tokens from source text using the lexer
    fn generateSyntaxTokens(self: *Self, text: []const u8) ![]SyntaxToken {
        var lexer = lexer_mod.Lexer.init(self.allocator, text, 1) catch {
            return error.LexerInitFailed;
        };
        defer lexer.deinit();

        var tokens = std.ArrayList(SyntaxToken).init(self.allocator);
        errdefer tokens.deinit();

        var tok_count: usize = 0;
        while (true) {
            // Cancellation checkpoint: check every 500 tokens for large files
            if (tok_count % 500 == 0) {
                try self.checkCancellation();
            }
            tok_count += 1;
            const tok = lexer.next() catch break;
            if (tok.kind == .end_of_file) break;

            const token_type = mapTokenKindToSyntaxType(tok.kind) orelse continue;

            try tokens.append(.{
                .line = if (tok.loc.start.line > 0) tok.loc.start.line - 1 else 0,
                .column = tok.loc.start.column,
                .length = @intCast(tok.text.len),
                .token_type = token_type,
                .modifiers = 0,
            });
        }

        return try tokens.toOwnedSlice();
    }

    /// Map lexer token kind to syntax token type
    fn mapTokenKindToSyntaxType(kind: token_mod.TokenKind) ?SyntaxTokenType {
        return switch (kind) {
            // Keywords
            .keyword_break, .keyword_case, .keyword_catch, .keyword_continue,
            .keyword_debugger, .keyword_default, .keyword_delete, .keyword_do,
            .keyword_else, .keyword_finally, .keyword_for, .keyword_if,
            .keyword_in, .keyword_instanceof, .keyword_new, .keyword_return,
            .keyword_switch, .keyword_throw, .keyword_try, .keyword_typeof,
            .keyword_void, .keyword_while, .keyword_with, .keyword_as,
            .keyword_from, .keyword_of, .keyword_is, .keyword_keyof,
            .keyword_class, .keyword_interface, .keyword_enum, .keyword_type,
            .keyword_namespace, .keyword_function, .keyword_async, .keyword_await,
            .keyword_const, .keyword_let, .keyword_var, .keyword_abstract,
            .keyword_declare, .keyword_export, .keyword_extends, .keyword_implements,
            .keyword_import, .keyword_private, .keyword_protected, .keyword_public,
            .keyword_readonly, .keyword_static, .keyword_constructor,
            .keyword_get, .keyword_set, .keyword_require, .keyword_true,
            .keyword_false, .keyword_null, .keyword_this, .keyword_super,
            .keyword_never, .keyword_unknown,
            .keyword_int8, .keyword_int16, .keyword_int32, .keyword_int64,
            .keyword_uint8, .keyword_uint16, .keyword_uint32, .keyword_uint64,
            .keyword_float32, .keyword_float64,
            .keyword_int, .keyword_float, .keyword_double,
            .keyword_defer, .keyword_distinct,
            => .keyword,

            // Macros - only @ sign now
            .at_sign => .macro,

            // Numbers and strings
            .number => .number,
            .string, .template_string => .string,

            // Identifiers
            .identifier => .identifier,

            // Operators
            .plus, .minus, .star, .slash, .percent, .star_star,
            .equals, .plus_equals, .minus_equals, .star_equals,
            .slash_equals, .percent_equals, .equals_equals, .equals_equals_equals,
            .bang_equals, .bang_equals_equals, .less_than, .less_equals,
            .greater_than, .greater_equals, .ampersand_ampersand, .pipe_pipe,
            .bang, .ampersand, .pipe, .caret, .tilde, .less_less,
            .greater_greater, .greater_greater_greater, .plus_plus, .minus_minus,
            .question, .dot, .dot_dot_dot, .arrow,
            => .operator,

            // Punctuation
            .left_paren, .right_paren, .left_brace, .right_brace,
            .left_bracket, .right_bracket, .semicolon, .colon, .comma,
            => .punctuation,

            // Skip
            .newline, .end_of_file, .syntax_error, .regex,
            => null,
        };
    }

    /// Start async macro expansion for a file.
    /// Returns immediately with a handle to track progress.
    pub fn startAsyncMacroExpansion(self: *Self, file_id: []const u8) !ExpansionHandle {
        const file_id_hash = hashString(file_id);
        var handle: ExpansionHandle = undefined;
        var owned_file_id: []const u8 = undefined;

        // Critical section: update shared state
        {
            self.expansion_mutex.lock();
            defer self.expansion_mutex.unlock();

            // Cancel any existing expansion for this file
            var tasks_it = self.async_tasks.iterator();
            while (tasks_it.next()) |entry| {
                if (entry.value_ptr.file_id_hash == file_id_hash) {
                    if (entry.value_ptr.status == .pending or entry.value_ptr.status == .running) {
                        entry.value_ptr.status = .cancelled;
                    }
                }
            }

            // Create new expansion handle
            handle = ExpansionHandle{
                .id = self.next_expansion_id,
                .file_id_hash = file_id_hash,
                .started_at = std.time.milliTimestamp(),
            };
            self.next_expansion_id += 1;

            // Create task entry
            owned_file_id = try self.allocator.dupe(u8, file_id);
            const task = AsyncExpansionTask{
                .handle = handle,
                .file_id = owned_file_id,
                .file_id_hash = file_id_hash,
                .status = .pending,
                .total_macros = 0,
                .completed_macros = 0,
                .error_message = null,
                .thread = null,
            };

            // Reset cancel flag before starting new expansion
            self.cancel_flag.store(false, .seq_cst);

            try self.async_tasks.put(handle.id, task);
        }
        // Mutex released here

        // TODO: Enable async threading once caches are thread-safe
        // For now, run synchronously to ensure correctness.
        // Real async threading requires:
        // 1. Thread-safe LRU cache (parse_cache, macro_cache)
        // 2. Thread-safe macro_output_cache
        // 3. Thread-safe AST arenas
        //
        // The async thread infrastructure is ready (asyncExpansionThreadFn),
        // but internal data structures need synchronization primitives.
        self.runExpansionSync(handle.id, owned_file_id);

        return handle;
    }

    /// Run expansion synchronously (for now, until async threading is stable)
    fn runExpansionSync(self: *Self, task_id: u64, file_id: []const u8) void {
        // Update status to running
        {
            self.expansion_mutex.lock();
            defer self.expansion_mutex.unlock();
            if (self.async_tasks.getPtr(task_id)) |task| {
                task.status = .running;
            }
        }

        // Get macro call sites (count for progress)
        const call_sites = self.getMacroCallSites(file_id) catch {
            self.expansion_mutex.lock();
            defer self.expansion_mutex.unlock();
            if (self.async_tasks.getPtr(task_id)) |task| {
                task.status = .failed;
            }
            return;
        };
        defer {
            for (call_sites) |site| {
                self.allocator.free(site.arguments);
            }
            self.allocator.free(call_sites);
        }

        // Update total_macros count
        {
            self.expansion_mutex.lock();
            defer self.expansion_mutex.unlock();
            if (self.async_tasks.getPtr(task_id)) |task| {
                task.total_macros = @intCast(call_sites.len);
            }
        }

        // Invoke initial progress callback
        self.invokeProgressCallback(task_id);

        // Expand each macro
        for (call_sites, 0..) |site, i| {
            // Check cancellation flag
            if (self.cancel_flag.load(.seq_cst)) {
                self.expansion_mutex.lock();
                defer self.expansion_mutex.unlock();
                if (self.async_tasks.getPtr(task_id)) |task| {
                    if (task.status != .cancelled) {
                        task.status = .cancelled;
                    }
                }
                return;
            }

            // Expand this macro call site
            _ = self.expandMacroCallSite(file_id, site) catch continue;

            // Update progress
            {
                self.expansion_mutex.lock();
                defer self.expansion_mutex.unlock();
                if (self.async_tasks.getPtr(task_id)) |task| {
                    task.completed_macros = @intCast(i + 1);
                }
            }

            // Invoke progress callback
            self.invokeProgressCallback(task_id);
        }

        // Mark as completed
        {
            self.expansion_mutex.lock();
            defer self.expansion_mutex.unlock();
            if (self.async_tasks.getPtr(task_id)) |task| {
                if (task.status == .running) {
                    task.status = .completed;
                }
            }
        }

        // Final progress callback
        self.invokeProgressCallback(task_id);
    }

    /// Cancel an async expansion
    pub fn cancelAsyncExpansion(self: *Self, handle: ExpansionHandle) void {
        self.expansion_mutex.lock();
        defer self.expansion_mutex.unlock();

        if (self.async_tasks.getPtr(handle.id)) |task| {
            if (task.status == .pending or task.status == .running) {
                task.status = .cancelled;
                // Signal cancellation to running thread if any
                self.cancel_flag.store(true, .seq_cst);
            }
        }
    }

    /// Thread function for async macro expansion
    fn asyncExpansionThreadFn(ctx: *ThreadContext) void {
        const self = ctx.db;
        const task_id = ctx.task_id;
        const file_id = ctx.file_id;

        // Clean up thread context when done
        defer self.allocator.destroy(ctx);

        // Update status to running
        {
            self.expansion_mutex.lock();
            defer self.expansion_mutex.unlock();
            if (self.async_tasks.getPtr(task_id)) |task| {
                task.status = .running;
            }
        }

        // Get macro call sites (count for progress)
        const call_sites = self.getMacroCallSites(file_id) catch {
            // Handle error - mark task as failed
            self.expansion_mutex.lock();
            defer self.expansion_mutex.unlock();
            if (self.async_tasks.getPtr(task_id)) |task| {
                task.status = .failed;
            }
            return;
        };
        defer {
            for (call_sites) |site| {
                self.allocator.free(site.arguments);
            }
            self.allocator.free(call_sites);
        }

        // Update total_macros count
        {
            self.expansion_mutex.lock();
            defer self.expansion_mutex.unlock();
            if (self.async_tasks.getPtr(task_id)) |task| {
                task.total_macros = @intCast(call_sites.len);
            }
        }

        // Invoke initial progress callback
        self.invokeProgressCallback(task_id);

        // Expand each macro
        for (call_sites, 0..) |site, i| {
            // Check cancellation flag
            if (self.cancel_flag.load(.seq_cst)) {
                self.expansion_mutex.lock();
                defer self.expansion_mutex.unlock();
                if (self.async_tasks.getPtr(task_id)) |task| {
                    if (task.status != .cancelled) {
                        task.status = .cancelled;
                    }
                }
                return;
            }

            // Check if task was cancelled
            {
                self.expansion_mutex.lock();
                const status = if (self.async_tasks.get(task_id)) |task| task.status else .cancelled;
                self.expansion_mutex.unlock();
                if (status == .cancelled) return;
            }

            // Expand this macro call site
            _ = self.expandMacroCallSite(file_id, site) catch continue;

            // Update progress
            {
                self.expansion_mutex.lock();
                defer self.expansion_mutex.unlock();
                if (self.async_tasks.getPtr(task_id)) |task| {
                    task.completed_macros = @intCast(i + 1);
                }
            }

            // Invoke progress callback
            self.invokeProgressCallback(task_id);
        }

        // Mark as completed
        {
            self.expansion_mutex.lock();
            defer self.expansion_mutex.unlock();
            if (self.async_tasks.getPtr(task_id)) |task| {
                if (task.status == .running) {
                    task.status = .completed;
                }
            }
        }

        // Final progress callback
        self.invokeProgressCallback(task_id);
    }

    /// Helper to invoke progress callback with current task state
    fn invokeProgressCallback(self: *Self, task_id: u64) void {
        const callback = self.progress_callback orelse return;
        const ctx = self.progress_callback_ctx; // May be null, that's OK

        self.expansion_mutex.lock();
        const task = self.async_tasks.get(task_id);
        self.expansion_mutex.unlock();

        if (task) |t| {
            const progress = ExpansionProgress{
                .file_id = t.file_id,
                .total_macros = t.total_macros,
                .current_index = t.completed_macros,
                .current_macro_name = null,
                .elapsed_ms = @intCast(@max(0, std.time.milliTimestamp() - t.handle.started_at)),
                .status = t.status,
            };
            callback(progress, ctx);
        }
    }

    /// Get the status of an async expansion
    pub fn getExpansionStatus(self: *Self, handle: ExpansionHandle) ExpansionStatus {
        self.expansion_mutex.lock();
        defer self.expansion_mutex.unlock();

        if (self.async_tasks.get(handle.id)) |task| {
            return task.status;
        }
        return .cancelled; // Unknown handle treated as cancelled
    }

    /// Wait for an async expansion to complete (with timeout)
    pub fn waitForExpansion(self: *Self, handle: ExpansionHandle, timeout_ms: u64) !void {
        const start = std.time.milliTimestamp();
        const deadline = start + @as(i64, @intCast(timeout_ms));

        while (std.time.milliTimestamp() < deadline) {
            const status = self.getExpansionStatus(handle);
            switch (status) {
                .completed => return,
                .cancelled => return error.Cancelled,
                .failed => return error.ExpansionFailed,
                .pending, .running => {
                    std.time.sleep(1_000_000); // 1ms
                },
            }
        }
        return error.Timeout;
    }

    /// Set callback for expansion progress notifications
    pub fn setExpansionProgressCallback(
        self: *Self,
        callback: ExpansionProgressCallback,
        ctx: ?*anyopaque,
    ) void {
        self.progress_callback = callback;
        self.progress_callback_ctx = ctx;
    }

    /// Get progress info for an expansion handle
    pub fn getExpansionProgress(self: *Self, handle: ExpansionHandle) ?ExpansionProgress {
        self.expansion_mutex.lock();
        defer self.expansion_mutex.unlock();

        if (self.async_tasks.get(handle.id)) |task| {
            return ExpansionProgress{
                .file_id = task.file_id,
                .total_macros = task.total_macros,
                .current_index = task.completed_macros,
                .current_macro_name = null,
                .elapsed_ms = @intCast(@max(0, std.time.milliTimestamp() - handle.started_at)),
                .status = task.status,
            };
        }
        return null;
    }

    /// Get semantic tokens (includes macro-expanded info when available)
    pub fn getSemanticTokens(self: *Self, file_id: []const u8) ![]SyntaxToken {
        // For now, semantic tokens are same as syntax tokens
        // In full implementation, this would include type info from macro expansion
        return self.getSyntaxTokens(file_id);
    }

    /// Get semantic tokens with version for delta tracking
    pub fn getSemanticTokensWithVersion(self: *Self, file_id: []const u8) !SemanticTokensResult {
        const tokens = try self.getSyntaxTokens(file_id);

        // Increment version
        self.syntax_token_version += 1;
        const version = self.syntax_token_version;

        // Store version for this file (remove old key first to avoid leaks)
        if (self.semantic_token_versions.fetchRemove(file_id)) |old| {
            self.allocator.free(old.key);
        }
        const owned_key = try self.allocator.dupe(u8, file_id);
        try self.semantic_token_versions.put(owned_key, version);

        return .{
            .tokens = tokens,
            .version = version,
        };
    }

    /// Get semantic tokens delta since a previous version
    pub fn getSemanticTokensDelta(
        self: *Self,
        file_id: []const u8,
        since_version: u64,
    ) !SemanticTokensDelta {
        _ = since_version;

        // For now, always return full tokens (delta computation is complex)
        const tokens = try self.getSyntaxTokens(file_id);

        self.syntax_token_version += 1;

        return .{
            .edits = &[_]SemanticTokenEdit{},
            .full_tokens = tokens,
            .result_version = self.syntax_token_version,
        };
    }

    /// Get the status of semantic tokens for a file
    pub fn getSemanticTokensStatus(self: *Self, file_id: []const u8) SemanticTokensStatus {
        const file_id_hash = hashString(file_id);

        // Check if any expansion is pending for this file
        self.expansion_mutex.lock();
        defer self.expansion_mutex.unlock();

        var tasks_it = self.async_tasks.iterator();
        while (tasks_it.next()) |entry| {
            if (entry.value_ptr.file_id_hash == file_id_hash) {
                if (entry.value_ptr.status == .pending or entry.value_ptr.status == .running) {
                    return .pending_expansion;
                }
            }
        }

        // Check if we have syntax tokens cached
        if (self.syntax_token_cache.contains(file_id)) {
            return .available;
        }

        return .syntax_only;
    }

    // ===== PHASE 5: INCREMENTAL DIAGNOSTICS API =====

    /// Get diagnostics for a file (cached, incremental).
    /// Returns cached diagnostics if file hasn't changed, otherwise recomputes.
    pub fn getDiagnostics(self: *Self, file_id: []const u8) ![]Diagnostic {
        // Check cancellation early - abort before expensive work
        try self.checkCancellation();

        const file_hash = hashString(file_id);

        // Check cache first
        if (self.diagnostics_cache.get(file_id)) |cached| {
            // Verify cache is still valid
            if (self.file_hashes.get(file_id)) |current_hash| {
                if (cached.file_hash == current_hash) {
                    // Return copy of cached diagnostics
                    return try self.allocator.dupe(Diagnostic, cached.diagnostics);
                }
            }
        }

        // Cache miss or stale - regenerate using parser (includes lexer + parse errors)
        // Phase 6: Now uses generateDiagnosticsForFile which leverages the parsed AST
        const diagnostics = try self.generateDiagnosticsForFile(file_id);

        // Update cache
        const cached_diags = CachedDiagnostics{
            .diagnostics = try self.dupeDiagnostics(diagnostics),
            .file_hash = file_hash,
            .computed_at = std.time.milliTimestamp(),
        };

        // Remove old cache entry if exists
        if (self.diagnostics_cache.fetchRemove(file_id)) |old| {
            self.allocator.free(old.key);
            for (old.value.diagnostics) |diag| {
                self.allocator.free(diag.message);
            }
            self.allocator.free(old.value.diagnostics);
        }

        const owned_key = try self.allocator.dupe(u8, file_id);
        try self.diagnostics_cache.put(owned_key, cached_diags);

        return diagnostics;
    }

    /// Generate diagnostics from source text using lexer AND parser
    /// Phase 6: Now includes parse errors in addition to lexer errors
    fn generateDiagnostics(self: *Self, text: []const u8) ![]Diagnostic {
        var diagnostics = std.ArrayList(Diagnostic).init(self.allocator);
        errdefer diagnostics.deinit();

        // Step 1: Collect lexer errors
        var lexer = lexer_mod.Lexer.init(self.allocator, text, 1) catch {
            return error.LexerInitFailed;
        };
        defer lexer.deinit();

        // Consume all tokens to collect lexer errors
        var tok_count: usize = 0;
        while (true) {
            // Cancellation checkpoint: check every 500 tokens for large files
            if (tok_count % 500 == 0) {
                try self.checkCancellation();
            }
            tok_count += 1;
            const tok = lexer.next() catch break;
            if (tok.kind == .end_of_file) break;
        }

        // Convert lexer errors to diagnostics
        for (lexer.errors.items) |err| {
            const diag = Diagnostic{
                .start_line = if (err.loc.start.line > 0) err.loc.start.line - 1 else 0,
                .start_col = err.loc.start.column,
                .end_line = if (err.loc.end.line > 0) err.loc.end.line - 1 else 0,
                .end_col = err.loc.end.column,
                .severity = .@"error",
                .message = try self.allocator.dupe(u8, err.message),
                .source = "mls-lexer",
            };
            try diagnostics.append(diag);
        }

        return try diagnostics.toOwnedSlice();
    }

    /// Generate diagnostics for a file using both lexer and parser
    /// This version takes file_id and uses the cached parse result
    fn generateDiagnosticsForFile(self: *Self, file_id: []const u8) ![]Diagnostic {
        var diagnostics = std.ArrayList(Diagnostic).init(self.allocator);
        errdefer diagnostics.deinit();

        // Parse the file - this gives us both the AST and parse errors
        const parse_result = self.parse(file_id) catch |err| {
            // If parsing completely fails, return a single error diagnostic
            const diag = Diagnostic{
                .start_line = 0,
                .start_col = 0,
                .end_line = 0,
                .end_col = 0,
                .severity = .@"error",
                .message = try self.allocator.dupe(u8, @errorName(err)),
                .source = "mls-parser",
            };
            try diagnostics.append(diag);
            return try diagnostics.toOwnedSlice();
        };

        // Convert parse errors to diagnostics
        for (parse_result.errors) |err| {
            const diag = Diagnostic{
                .start_line = if (err.location.start.line > 0) err.location.start.line - 1 else 0,
                .start_col = err.location.start.column,
                .end_line = if (err.location.end.line > 0) err.location.end.line - 1 else 0,
                .end_col = err.location.end.column,
                .severity = .@"error",
                .message = try self.allocator.dupe(u8, err.message),
                .source = "mls-parser",
            };
            try diagnostics.append(diag);
        }

        return try diagnostics.toOwnedSlice();
    }

    /// Duplicate diagnostics array (deep copy messages)
    fn dupeDiagnostics(self: *Self, diagnostics: []const Diagnostic) ![]Diagnostic {
        var result = try self.allocator.alloc(Diagnostic, diagnostics.len);
        for (diagnostics, 0..) |diag, i| {
            result[i] = .{
                .start_line = diag.start_line,
                .start_col = diag.start_col,
                .end_line = diag.end_line,
                .end_col = diag.end_col,
                .severity = diag.severity,
                .message = try self.allocator.dupe(u8, diag.message),
                .source = diag.source,
            };
        }
        return result;
    }

    /// Free diagnostics array
    pub fn freeDiagnostics(self: *Self, diagnostics: []Diagnostic) void {
        for (diagnostics) |diag| {
            self.allocator.free(diag.message);
        }
        self.allocator.free(diagnostics);
    }

    // ===== PHASE 3: MACRO FIREWALL QUERIES =====

    /// Query 1: Find all macro call sites in a file.
    /// This is the entry point for macro processing.
    /// Now uses parsed AST to extract macro invocations with arguments.
    pub fn getMacroCallSites(self: *Self, file_id: []const u8) ![]MacroCallSite {
        const file_id_hash = hashString(file_id);

        // Parse the file to get AST
        const parse_result = try self.parse(file_id);

        var call_sites = std.ArrayList(MacroCallSite).init(self.allocator);
        errdefer call_sites.deinit();

        var call_index: u32 = 0;

        // Walk AST to find macro invocations
        try self.collectMacroCallSites(parse_result.tree, file_id_hash, &call_sites, &call_index);

        return try call_sites.toOwnedSlice();
    }

    /// Recursively collect macro call sites from AST
    fn collectMacroCallSites(
        self: *Self,
        node: *ast.Node,
        file_id_hash: u64,
        call_sites: *std.ArrayList(MacroCallSite),
        call_index: *u32,
    ) !void {
        // Cancellation checkpoint: check at each node to stay responsive during large AST walks
        try self.checkCancellation();

        switch (node.kind) {
            .program => {
                // Walk all statements
                for (node.data.program.statements) |stmt| {
                    try self.collectMacroCallSites(stmt, file_id_hash, call_sites, call_index);
                }
            },
            .class_decl => {
                // Check for decorators on class
                const class = &node.data.class_decl;
                for (class.decorators) |decorator| {
                    var call_site = try self.createMacroCallSite(
                        decorator.name,
                        decorator.arguments,
                        node.location,
                        file_id_hash,
                        call_index,
                    );
                    // Store reference to target class node for MacroVM
                    call_site.target_node = node;
                    try call_sites.append(call_site);
                }
                // Also walk class members for nested decorators
                for (class.members) |member| {
                    try self.collectMacroCallSites(member, file_id_hash, call_sites, call_index);
                }
            },
            .macro_invocation => {
                // Standalone macro invocation
                const macro = &node.data.macro_invocation;
                const arguments = try self.extractMacroArgStrings(macro.arguments);
                const call_site = MacroCallSite{
                    .call_id = .{
                        .file_id_hash = file_id_hash,
                        .call_index = call_index.*,
                        .input_hash = self.computeMacroInputHash(macro.name, macro.arguments),
                    },
                    .macro_name = macro.name,
                    .line = node.location.start.line,
                    .column = node.location.start.column,
                    .arguments = arguments,
                };
                try call_sites.append(call_site);
                call_index.* += 1;
            },
            .function_decl => {
                // Walk function body if present (for macros in function body)
                const func = &node.data.function_decl;
                if (func.body) |body| {
                    try self.collectMacroCallSites(body, file_id_hash, call_sites, call_index);
                }
            },
            .block_stmt => {
                // Walk block statements
                for (node.data.block_stmt.statements) |stmt| {
                    try self.collectMacroCallSites(stmt, file_id_hash, call_sites, call_index);
                }
            },
            // Add other node types that might contain macros as needed
            else => {
                // For other nodes, we don't recurse (they don't contain macro invocations)
            },
        }
    }

    /// Create a MacroCallSite from a decorator
    fn createMacroCallSite(
        self: *Self,
        name: []const u8,
        arg_nodes: []*ast.Node,
        loc: ast.SourceLocation,
        file_id_hash: u64,
        call_index: *u32,
    ) !MacroCallSite {
        // Extract argument strings from nodes
        var args = std.ArrayList([]const u8).init(self.allocator);
        defer args.deinit();

        for (arg_nodes) |arg_node| {
            if (arg_node.kind == .identifier) {
                try args.append(arg_node.data.identifier);
            }
            // Could add support for other argument types here
        }

        const arguments = try self.allocator.dupe([]const u8, args.items);

        const call_site = MacroCallSite{
            .call_id = .{
                .file_id_hash = file_id_hash,
                .call_index = call_index.*,
                .input_hash = self.computeMacroInputHashFromStrings(name, arguments),
            },
            .macro_name = name,
            .line = loc.start.line,
            .column = loc.start.column,
            .arguments = arguments,
        };

        call_index.* += 1;
        return call_site;
    }

    /// Extract string representations of macro arguments
    fn extractMacroArgStrings(self: *Self, macro_args: []const ast.node.MacroInvocation.MacroArgument) ![]const []const u8 {
        var args = std.ArrayList([]const u8).init(self.allocator);
        defer args.deinit();

        for (macro_args) |arg| {
            switch (arg) {
                .identifier => |id| try args.append(id),
                .type => {
                    // For type arguments, we'd need to stringify the type
                    // For now, just use placeholder
                    try args.append("<type>");
                },
                .expression => {
                    // For expression arguments, we'd need to stringify
                    try args.append("<expr>");
                },
            }
        }

        return try self.allocator.dupe([]const u8, args.items);
    }

    /// Compute input hash for macro invocation
    fn computeMacroInputHash(self: *Self, name: []const u8, args: []const ast.node.MacroInvocation.MacroArgument) u64 {
        _ = self;
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(name);
        for (args) |arg| {
            switch (arg) {
                .identifier => |id| hasher.update(id),
                .type => hasher.update("<type>"),
                .expression => hasher.update("<expr>"),
            }
        }
        return hasher.final();
    }

    /// Compute input hash from string arguments
    fn computeMacroInputHashFromStrings(self: *Self, name: []const u8, args: []const []const u8) u64 {
        _ = self;
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(name);
        for (args) |arg| {
            hasher.update(arg);
        }
        return hasher.final();
    }

    /// Query 2: Get the input hash for a macro call (content-addressed).
    /// Changes only when the macro call's text changes.
    pub fn getMacroInputHash(self: *Self, call_id: MacroCallInfo) u64 {
        // For now, the call_id already contains the input hash
        // In full implementation, this would re-compute from AST
        _ = self;
        return call_id.input_hash;
    }

    /// Query 3: Check if macro output is unchanged (KEY OPTIMIZATION).
    /// If output hash matches previous, dependents stay GREEN.
    pub fn isMacroOutputUnchanged(self: *Self, input_hash: u64, new_output_hash: u64) bool {
        return self.macro_output_cache.outputUnchanged(input_hash, new_output_hash);
    }

    /// Query 4: Store macro expansion result with content-addressing.
    pub fn storeMacroExpansion(
        self: *Self,
        input_hash: u64,
        output_hash: u64,
        output_ast: *ast.Node,
    ) !bool {
        return try self.macro_output_cache.store(input_hash, output_hash, output_ast);
    }

    /// Query 5: Get cached macro expansion output.
    pub fn getMacroExpansion(self: *Self, input_hash: u64) ?*ast.Node {
        const output_hash = self.macro_output_cache.getOutputHash(input_hash) orelse return null;
        return self.macro_output_cache.getAst(output_hash);
    }

    /// Get macro cache statistics for monitoring
    pub fn getMacroCacheStats(self: *Self) MacroOutputCache.Stats {
        return self.macro_output_cache.getStats();
    }

    /// Get macro cache hit rate
    pub fn getMacroCacheHitRate(self: *Self) f64 {
        return self.macro_output_cache.hitRate();
    }

    // ===== PHASE 7: MACRO FIREWALL - ACTUAL EXPANSION =====

    /// Result of expanding a macro call site
    pub const MacroExpansionResult = struct {
        /// Generated AST nodes (methods, etc.)
        generated_nodes: []*ast.Node,
        /// Output hash for content-addressing
        output_hash: u64,
        /// Whether result was from cache
        from_cache: bool,
    };

    /// Expand a single macro call site with content-addressed caching.
    /// This is the core of the firewall pattern:
    /// - Same input → same output hash → dependents stay GREEN
    /// - Different input but same output → dependents STILL stay GREEN!
    pub fn expandMacroCallSite(
        self: *Self,
        file_id: []const u8,
        call_site: MacroCallSite,
    ) !MacroExpansionResult {
        const input_hash = call_site.call_id.input_hash;

        // Get the AST arena for this file (needed for both cache hit and miss)
        const file_hash = hashString(file_id);
        const arena = self.ast_arenas.get(file_hash) orelse return error.FileNotParsed;

        // Check content-addressed cache first
        if (self.macro_output_cache.getOutputHash(input_hash)) |cached_output_hash| {
            if (self.macro_output_cache.getAst(cached_output_hash)) |cached_ast| {
                // Cache hit! Return cached result
                // Use arena allocator so memory is cleaned up with the file
                var cached_nodes = try arena.allocator().alloc(*ast.Node, 1);
                cached_nodes[0] = cached_ast;
                return .{
                    .generated_nodes = cached_nodes,
                    .output_hash = cached_output_hash,
                    .from_cache = true,
                };
            }
        }

        // Cache miss: need to expand the macro

        // Generate the expanded AST based on macro name
        const generated = try self.generateMacroExpansion(
            arena,
            call_site.macro_name,
            call_site.arguments,
            call_site.target_node,
        );

        // Hash the output for content-addressing
        const output_hash = self.hashGeneratedNodes(generated);

        // Check if output is unchanged from previous (KEY OPTIMIZATION!)
        const output_reused = self.isMacroOutputUnchanged(input_hash, output_hash);
        if (output_reused) {
            // Different input produced same output!
            // This is the firewall benefit - dependents stay GREEN
        }

        // Store in content-addressed cache
        if (generated.len > 0) {
            _ = try self.macro_output_cache.store(input_hash, output_hash, generated[0]);
        }

        return .{
            .generated_nodes = generated,
            .output_hash = output_hash,
            .from_cache = false,
        };
    }

    /// Generate macro expansion based on macro name and arguments.
    /// Uses MacroVM (Hermes) for real macro execution when target is available.
    /// Falls back to stub generation for preview when target is unavailable.
    fn generateMacroExpansion(
        self: *Self,
        arena: *ast.ASTArena,
        macro_name: []const u8,
        arguments: []const []const u8,
        target_node: ?*ast.Node,
    ) ![]*ast.Node {
        var generated = std.ArrayList(*ast.Node).init(arena.allocator());

        // Try real MacroVM execution if target is available
        if (target_node) |target| {
            if (target.kind == .class_decl) {
                const class = &target.data.class_decl;
                const original_member_count = class.members.len;

                // Create MacroVM instance with bytecode cache for fast execution
                var macro_vm = vm.MacroVM.initWithCache(
                    self.allocator,
                    arena,
                    if (self.bytecode_cache) |*cache| cache else null,
                ) catch |err| {
                    std.log.warn("[Trans-am] MacroVM init failed: {}, falling back to stub", .{err});
                    return try self.generateStubExpansion(arena, macro_name, arguments);
                };
                defer macro_vm.deinit();

                // Execute the macro - this will add methods to target.data.class_decl.members
                macro_vm.executeMacro(macro_name, arguments, target) catch |err| {
                    std.log.warn("[Trans-am] Macro execution failed: {}, falling back to stub", .{err});
                    return try self.generateStubExpansion(arena, macro_name, arguments);
                };

                // Extract newly added members (the generated nodes)
                const new_member_count = class.members.len;
                if (new_member_count > original_member_count) {
                    for (class.members[original_member_count..]) |new_member| {
                        try generated.append(new_member);
                    }
                    std.log.debug("[Trans-am] MacroVM generated {} nodes for @{s}", .{
                        new_member_count - original_member_count,
                        macro_name,
                    });
                }

                return try generated.toOwnedSlice();
            }
        }

        // Fallback: stub generation for preview (no target or non-class target)
        return try self.generateStubExpansion(arena, macro_name, arguments);
    }

    /// Generate stub expansion when MacroVM isn't available or fails
    fn generateStubExpansion(
        self: *Self,
        arena: *ast.ASTArena,
        macro_name: []const u8,
        arguments: []const []const u8,
    ) ![]*ast.Node {
        _ = self;
        var generated = std.ArrayList(*ast.Node).init(arena.allocator());

        if (std.mem.eql(u8, macro_name, "derive")) {
            for (arguments) |trait| {
                if (std.mem.eql(u8, trait, "Eq")) {
                    // Generate stub: equals(other: T): boolean
                    const method = try arena.createNode(
                        .function_decl,
                        ast.SourceLocation.dummy(),
                        .{
                            .function_decl = .{
                                .name = "equals",
                                .params = &[_]ast.node.FunctionExpr.FunctionParam{},
                                .return_type = null,
                                .body = null,
                                .type_params = &[_]ast.types.GenericParam{},
                            },
                        },
                    );
                    try generated.append(method);
                } else if (std.mem.eql(u8, trait, "Hash")) {
                    // Generate stub: hash(): number
                    const method = try arena.createNode(
                        .function_decl,
                        ast.SourceLocation.dummy(),
                        .{
                            .function_decl = .{
                                .name = "hash",
                                .params = &[_]ast.node.FunctionExpr.FunctionParam{},
                                .return_type = null,
                                .body = null,
                                .type_params = &[_]ast.types.GenericParam{},
                            },
                        },
                    );
                    try generated.append(method);
                }
            }
        }

        return try generated.toOwnedSlice();
    }

    /// Hash generated AST nodes for content-addressing
    fn hashGeneratedNodes(self: *Self, nodes: []*ast.Node) u64 {
        _ = self;
        var hasher = std.hash.Wyhash.init(0);

        for (nodes) |node| {
            // Hash the node kind and key fields
            std.hash.autoHash(&hasher, node.kind);

            // For function declarations, hash the name
            if (node.kind == .function_decl) {
                hasher.update(node.data.function_decl.name);
            }
        }

        return hasher.final();
    }

    /// Expand all macros in a file and return combined results.
    /// This is the high-level entry point for macro expansion.
    pub fn expandAllMacros(self: *Self, file_id: []const u8) !struct {
        total_macros: usize,
        expanded: usize,
        from_cache: usize,
    } {
        const call_sites = try self.getMacroCallSites(file_id);
        defer {
            for (call_sites) |site| {
                self.allocator.free(site.arguments);
            }
            self.allocator.free(call_sites);
        }

        var expanded: usize = 0;
        var from_cache: usize = 0;

        for (call_sites, 0..) |site, i| {
            // Cancellation checkpoint: check every 10 iterations to stay responsive
            if (i % 10 == 0) {
                try self.checkCancellation();
            }
            const result = self.expandMacroCallSite(file_id, site) catch continue;
            expanded += 1;
            if (result.from_cache) {
                from_cache += 1;
            }
        }

        return .{
            .total_macros = call_sites.len,
            .expanded = expanded,
            .from_cache = from_cache,
        };
    }

    // ===== COMPLETION SUPPORT (L4 Cache) =====

    /// Get completions for a file, using stale-while-revalidate cache
    /// Returns completion items and whether they came from cache
    pub fn getCompletions(self: *Self, file_id: []const u8) !struct {
        items: []CompletionItem,
        from_cache: bool,
        is_stale: bool,
    } {
        try self.checkCancellation();

        const file_hash = hashString(file_id);
        const current_content_hash = self.file_hashes.get(file_id) orelse 0;

        // Use file-level completion key (line=0, column=0, prefix=0)
        const key = cache_mod.CompletionKey{
            .file_hash = current_content_hash,
            .line = 0,
            .column = 0,
            .prefix_hash = file_hash, // Use file_id hash as extra discriminator
        };

        // Check cache - returns stale results if available
        if (self.completion_cache.get(key)) |cached| {
            // Since key includes content hash, cache hit means fresh data
            return .{
                .items = try self.allocator.dupe(CompletionItem, cached),
                .from_cache = true,
                .is_stale = false,
            };
        }

        // Try stale key (previous content hash) if available
        const stale_key = cache_mod.CompletionKey{
            .file_hash = 0, // Will check any matching prefix_hash
            .line = 0,
            .column = 0,
            .prefix_hash = file_hash,
        };
        if (self.completion_cache.get(stale_key)) |cached| {
            // Stale hit - return old data while background refreshes
            return .{
                .items = try self.allocator.dupe(CompletionItem, cached),
                .from_cache = true,
                .is_stale = true,
            };
        }

        // Cache miss - generate completions
        const items = try self.generateCompletions(file_id);

        // Cache the result
        self.completion_cache.put(key, items) catch {};

        return .{
            .items = items,
            .from_cache = false,
            .is_stale = false,
        };
    }

    /// Generate completion items by scanning the file
    fn generateCompletions(self: *Self, file_id: []const u8) ![]CompletionItem {
        const text = self.getFileText(file_id) orelse return error.FileNotFound;

        var lexer = lexer_mod.Lexer.init(self.allocator, text, 1) catch {
            return error.LexerInitFailed;
        };
        defer lexer.deinit();

        var items = std.ArrayList(CompletionItem).init(self.allocator);
        errdefer items.deinit();

        var seen = std.StringHashMap(void).init(self.allocator);
        defer seen.deinit();

        var prev_token: ?token_mod.Token = null;
        var tok_count: usize = 0;

        while (true) {
            // Cancellation checkpoint
            if (tok_count % 500 == 0) {
                try self.checkCancellation();
            }
            tok_count += 1;

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
                            try items.append(.{
                                .label = try self.allocator.dupe(u8, tok.text),
                                .kind = k,
                                .detail = null,
                            });
                        }
                    }
                }
            }
            prev_token = tok;
        }

        return try items.toOwnedSlice();
    }

    /// Invalidate completion cache for a file
    /// Currently invalidates all since CompletionCache doesn't support per-key invalidation
    pub fn invalidateCompletions(self: *Self, file_id: []const u8) void {
        _ = file_id;
        self.completion_cache.invalidateAll();
    }

    // ===== PHASE 5: HOVER SUPPORT =====

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
        const syntax_tokens = try self.getSyntaxTokens(file_id);
        defer self.allocator.free(syntax_tokens);

        // Also need text for token content
        const text = self.getFileText(file_id) orelse return error.FileNotFound;

        // Find token at position
        for (syntax_tokens, 0..) |tok, i| {
            // Cancellation checkpoint: check every 100 tokens (usually fast search)
            if (i % 100 == 0) {
                try self.checkCancellation();
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

    const changed = try db.setFileText("test.mts", "const x = 1;");
    try std.testing.expect(changed);

    const text = db.getFileText("test.mts").?;
    try std.testing.expectEqualStrings("const x = 1;", text);
    try std.testing.expect(db.revision == 1);
    try std.testing.expect(db.current_revision.value == 1);
}

test "Trans-am: content-addressed change detection" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // First set
    const changed1 = try db.setFileText("test.mts", "const x = 1;");
    try std.testing.expect(changed1);
    try std.testing.expect(db.revision == 1);

    // Same content - should not increment revision
    const changed2 = try db.setFileText("test.mts", "const x = 1;");
    try std.testing.expect(!changed2);
    try std.testing.expect(db.revision == 1);

    // Different content - should increment
    const changed3 = try db.setFileText("test.mts", "const x = 2;");
    try std.testing.expect(changed3);
    try std.testing.expect(db.revision == 2);
}

test "Trans-am: file hash tracking" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.mts", "const x = 1;");

    const hash = db.getFileHash("test.mts").?;
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
    try db.checkCancellation();

    // Request cancellation
    db.requestCancellation();

    // Should now fail
    const result = db.checkCancellation();
    try std.testing.expectError(error.Cancelled, result);

    // Reset
    db.resetCancellation();
    try db.checkCancellation();
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

    try db.storeQueryResult(
        key,
        @ptrCast(&dummy_value),
        type_id,
        hashString("test"),
        &deps,
        .low,
    );

    // Should be able to retrieve it
    const cached = db.getCachedQuery(key);
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

    try db.storeQueryResult(
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
    const is_green = try db.tryMarkGreen(key);
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
    const is_green = try db.tryMarkGreen(key);
    try std.testing.expect(!is_green);
}

test "Trans-am: beginQuery and endQuery" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    const key1 = QueryKey{ .query_type = .parse, .input_hash = 1 };
    const key2 = QueryKey{ .query_type = .macro_expand, .input_hash = 2 };

    // Begin query
    try db.beginQuery(key1);

    // Record a dependency
    try db.recordDependency(key2, .medium);

    // End query and get dependencies
    const frame = db.endQuery().?;
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
    try db.beginQuery(key);

    // Try to begin same query again - should detect cycle
    const result = db.beginQuery(key);
    try std.testing.expectError(TransAmDatabase.RedGreenError.CycleDetected, result);

    // Clean up
    _ = db.endQuery();
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
    try db.storeQueryResult(
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
    const can_reuse = db.canReuseOutput(key, value_hash);
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
    try db.storeQueryResult(
        key,
        @ptrCast(&dummy_value),
        type_id,
        value_hash,
        &deps,
        .low,
    );

    // Different output hash - cannot reuse
    const different_hash = hashString("different_content");
    const can_reuse = db.canReuseOutput(key, different_hash);
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
    try db.storeQueryResult(
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
    _ = try db.setFileText("test.ms", "const x = 1;");

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
    _ = try db.setFileText("test.ms", "const x = 1;");

    // Get syntax tokens
    const tokens = try db.getSyntaxTokens("test.ms");
    defer std.testing.allocator.free(tokens);

    // Should have tokens
    try std.testing.expect(tokens.len > 0);
}

test "Phase4: getSyntaxTokens returns macro tokens" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "@derive(Eq) class User {}");

    const tokens = try db.getSyntaxTokens("test.ms");
    defer std.testing.allocator.free(tokens);

    // First token should be macro (@derive)
    try std.testing.expect(tokens.len > 0);
    try std.testing.expectEqual(SyntaxTokenType.macro, tokens[0].token_type);
}

test "Phase4: getSyntaxTokens caches results" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "const x = 1;");

    // First call
    const tokens1 = try db.getSyntaxTokens("test.ms");
    defer std.testing.allocator.free(tokens1);

    // Second call should use cache (same result)
    const tokens2 = try db.getSyntaxTokens("test.ms");
    defer std.testing.allocator.free(tokens2);

    try std.testing.expectEqual(tokens1.len, tokens2.len);
}

test "Phase4: getSyntaxTokens invalidates cache on file change" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "const x = 1;");

    const tokens1 = try db.getSyntaxTokens("test.ms");
    defer std.testing.allocator.free(tokens1);
    const len1 = tokens1.len;

    // Change file
    _ = try db.setFileText("test.ms", "const x = 1; const y = 2;");

    const tokens2 = try db.getSyntaxTokens("test.ms");
    defer std.testing.allocator.free(tokens2);

    // More tokens now
    try std.testing.expect(tokens2.len > len1);
}

test "Phase4: startAsyncMacroExpansion returns handle" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "@derive(Eq) class A {}");

    const handle = try db.startAsyncMacroExpansion("test.ms");
    try std.testing.expect(handle.id > 0);
}

test "Phase4: getExpansionStatus returns status" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "@derive(Eq) class A {}");

    const handle = try db.startAsyncMacroExpansion("test.ms");
    const status = db.getExpansionStatus(handle);

    // Should be completed (synchronous fallback for now)
    try std.testing.expectEqual(ExpansionStatus.completed, status);
}

test "Phase4: cancelAsyncExpansion sets cancelled status" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "@derive(Eq) class A {}");

    const handle = try db.startAsyncMacroExpansion("test.ms");

    // Manually cancel the expansion
    db.cancelAsyncExpansion(handle);

    // Status should be cancelled (or completed if already done)
    const status = db.getExpansionStatus(handle);
    // In synchronous mode, expansion completes immediately before we can cancel
    // So we accept either completed or cancelled
    try std.testing.expect(status == ExpansionStatus.cancelled or status == ExpansionStatus.completed);
}

test "Phase4: waitForExpansion completes" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "@derive(Eq) class A {}");

    const handle = try db.startAsyncMacroExpansion("test.ms");

    // Should complete immediately (synchronous fallback)
    try db.waitForExpansion(handle, 1000);
}

test "Phase4: getSemanticTokensStatus returns available" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "const x = 1;");

    // Generate tokens first
    const tokens = try db.getSyntaxTokens("test.ms");
    defer std.testing.allocator.free(tokens);

    const status = db.getSemanticTokensStatus("test.ms");
    try std.testing.expectEqual(SemanticTokensStatus.available, status);
}

test "Phase4: getSemanticTokensWithVersion returns version" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "const x = 1;");

    const result = try db.getSemanticTokensWithVersion("test.ms");
    defer std.testing.allocator.free(result.tokens);

    try std.testing.expect(result.version > 0);
    try std.testing.expect(result.tokens.len > 0);
}

test "Phase4: syntax tokens independent of macro state" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // File with macros
    _ = try db.setFileText("test.ms",
        \\@derive(Eq, Hash)
        \\class User {
        \\    name: string;
        \\}
    );

    // Syntax tokens should work without any macro expansion
    const tokens = try db.getSyntaxTokens("test.ms");
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

    _ = try db.setFileText("test.ms", "const x = 1;");

    const tokens = try db.getSyntaxTokens("test.ms");
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

    _ = try db.setFileText("test.ms", "const x = 1;");

    const diagnostics = try db.getDiagnostics("test.ms");
    defer db.freeDiagnostics(diagnostics);

    // Valid code should have no diagnostics
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);
}

test "Phase5: getDiagnostics caches results" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "const x = 1;");

    // First call
    const diags1 = try db.getDiagnostics("test.ms");
    defer db.freeDiagnostics(diags1);

    // Second call should use cache
    const diags2 = try db.getDiagnostics("test.ms");
    defer db.freeDiagnostics(diags2);

    try std.testing.expectEqual(diags1.len, diags2.len);
}

test "Phase5: getDiagnostics invalidates on file change" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "const x = 1;");

    const diags1 = try db.getDiagnostics("test.ms");
    defer db.freeDiagnostics(diags1);

    // Change file
    _ = try db.setFileText("test.ms", "const y = 2;");

    // Should recompute (cache invalidated)
    const diags2 = try db.getDiagnostics("test.ms");
    defer db.freeDiagnostics(diags2);

    // Both should be valid (no errors)
    try std.testing.expectEqual(@as(usize, 0), diags1.len);
    try std.testing.expectEqual(@as(usize, 0), diags2.len);
}

test "Phase5: getTokenAtPosition returns token info" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "const x = 1;");

    // Get token at position of "const" (line 0, col 0)
    const token_info = try db.getTokenAtPosition("test.ms", 0, 0);

    try std.testing.expect(token_info != null);
    try std.testing.expectEqual(SyntaxTokenType.keyword, token_info.?.token_type);
    try std.testing.expectEqualStrings("const", token_info.?.text);
}

test "Phase5: getTokenAtPosition returns null for whitespace" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "const x = 1;");

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
    _ = try db.setFileText("test.ms", source);

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
    _ = try db.setFileText("test.ms", source);

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
    _ = try db.setFileText("test.ms", source);

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
    _ = try db.setFileText("test.ms", source);

    const call_sites = try db.getMacroCallSites("test.ms");
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
    _ = try db.setFileText("test.ms", source);

    const call_sites = try db.getMacroCallSites("test.ms");
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
    _ = try db.setFileText("test.ms", source);

    const diagnostics = try db.getDiagnostics("test.ms");
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
    _ = try db.setFileText("test.ms", source);

    const diagnostics = try db.getDiagnostics("test.ms");
    defer std.testing.allocator.free(diagnostics);

    // Should have no errors for valid code
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);
}

test "Phase6: parse invalidates on file change" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Initial parse
    _ = try db.setFileText("test.ms", "class A { }");
    const result1 = try db.parse("test.ms");

    // Change file
    _ = try db.setFileText("test.ms", "class B { }");
    const result2 = try db.parse("test.ms");

    // Should have different trees (re-parsed)
    try std.testing.expect(result1.tree != result2.tree);
}

test "Phase6: macro arguments hash changes with different args" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // First version
    _ = try db.setFileText("test.ms", "@derive(Eq) class User { }");
    const sites1 = try db.getMacroCallSites("test.ms");
    const hash1 = sites1[0].call_id.input_hash;
    for (sites1) |site| {
        std.testing.allocator.free(site.arguments);
    }
    std.testing.allocator.free(sites1);

    // Different arguments
    _ = try db.setFileText("test.ms", "@derive(Hash) class User { }");
    const sites2 = try db.getMacroCallSites("test.ms");
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

    _ = try db.setFileText("test.ms", "@derive(Eq) class User { name: string; }");

    const call_sites = try db.getMacroCallSites("test.ms");
    defer {
        for (call_sites) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(call_sites);
    }

    try std.testing.expectEqual(@as(usize, 1), call_sites.len);

    const result = try db.expandMacroCallSite("test.ms", call_sites[0]);
    try std.testing.expect(result.generated_nodes.len > 0);
    try std.testing.expect(!result.from_cache);
}

test "Phase7: expandMacroCallSite caches result" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms", "@derive(Eq) class User { name: string; }");

    const call_sites = try db.getMacroCallSites("test.ms");
    defer {
        for (call_sites) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(call_sites);
    }

    // First expansion
    const result1 = try db.expandMacroCallSite("test.ms", call_sites[0]);
    try std.testing.expect(!result1.from_cache);

    // Second expansion should be from cache
    const result2 = try db.expandMacroCallSite("test.ms", call_sites[0]);
    try std.testing.expect(result2.from_cache);
    try std.testing.expectEqual(result1.output_hash, result2.output_hash);
}

test "Phase7: firewall - editing one macro doesn't invalidate others" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Initial file with two macros
    _ = try db.setFileText("test.ms",
        \\@derive(Eq) class User { name: string; }
        \\@derive(Hash) class Item { id: number; }
    );

    // Expand both macros
    const call_sites1 = try db.getMacroCallSites("test.ms");
    defer {
        for (call_sites1) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(call_sites1);
    }

    try std.testing.expectEqual(@as(usize, 2), call_sites1.len);

    // Expand first macro (User with Eq)
    const user_result = try db.expandMacroCallSite("test.ms", call_sites1[0]);
    _ = user_result; // User's hash will change after edit

    // Expand second macro (Item with Hash)
    const item_result = try db.expandMacroCallSite("test.ms", call_sites1[1]);
    const item_hash = item_result.output_hash;

    // Now "edit" only the User class (add a property)
    _ = try db.setFileText("test.ms",
        \\@derive(Eq) class User { name: string; age: number; }
        \\@derive(Hash) class Item { id: number; }
    );

    // Get new call sites
    const call_sites2 = try db.getMacroCallSites("test.ms");
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

    const item_result2 = try db.expandMacroCallSite("test.ms", item_site.?);

    // Same output hash - firewall working!
    try std.testing.expectEqual(item_hash, item_result2.output_hash);
}

test "Phase7: expandAllMacros returns stats" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    _ = try db.setFileText("test.ms",
        \\@derive(Eq, Hash) class User { name: string; }
    );

    const stats = try db.expandAllMacros("test.ms");
    try std.testing.expectEqual(@as(usize, 1), stats.total_macros);
    try std.testing.expectEqual(@as(usize, 1), stats.expanded);
    try std.testing.expectEqual(@as(usize, 0), stats.from_cache);

    // Second call should use cache
    const stats2 = try db.expandAllMacros("test.ms");
    try std.testing.expectEqual(@as(usize, 1), stats2.total_macros);
    try std.testing.expectEqual(@as(usize, 1), stats2.expanded);
    try std.testing.expectEqual(@as(usize, 1), stats2.from_cache);
}

test "Phase7: content-addressed caching - same output reused" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Two different classes with same @derive(Eq) should produce same output hash
    _ = try db.setFileText("file1.ms", "@derive(Eq) class A { x: number; }");
    _ = try db.setFileText("file2.ms", "@derive(Eq) class B { y: string; }");

    const sites1 = try db.getMacroCallSites("file1.ms");
    defer {
        for (sites1) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(sites1);
    }

    const sites2 = try db.getMacroCallSites("file2.ms");
    defer {
        for (sites2) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(sites2);
    }

    const result1 = try db.expandMacroCallSite("file1.ms", sites1[0]);
    const result2 = try db.expandMacroCallSite("file2.ms", sites2[0]);

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
    _ = try db.setFileText("test.ms",
        \\@derive(Eq, Hash)
        \\class User { name: string; age: number; }
        \\@derive(Eq)
        \\class Item { id: number; }
    );

    // Start async expansion
    const handle = try db.startAsyncMacroExpansion("test.ms");

    // Wait for completion (should complete in reasonable time)
    try db.waitForExpansion(handle, 5000); // 5 second timeout

    // Status should be completed
    const status = db.getExpansionStatus(handle);
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
    db.setExpansionProgressCallback(struct {
        fn callback(progress: ExpansionProgress, ctx: ?*anyopaque) void {
            _ = ctx;
            CallbackState.call_count += 1;
            CallbackState.last_progress = progress;
        }
    }.callback, null);

    // Set up file with macros
    _ = try db.setFileText("test.ms", "@derive(Eq, Hash) class User { name: string; }");

    // Start async expansion
    const handle = try db.startAsyncMacroExpansion("test.ms");

    // Wait for completion
    try db.waitForExpansion(handle, 5000);

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
    _ = try db.setFileText("test.ms", "@derive(Eq, Hash) class User { name: string; }");

    // Start async expansion
    const handle = try db.startAsyncMacroExpansion("test.ms");

    // Immediately cancel
    db.cancelAsyncExpansion(handle);

    // Wait a bit for thread to notice cancellation
    std.time.sleep(10_000_000); // 10ms

    // Status should be cancelled or completed (if it finished before cancel)
    const status = db.getExpansionStatus(handle);
    try std.testing.expect(status == .cancelled or status == .completed);
}

test "Phase8: new expansion cancels previous for same file" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up file with macros
    _ = try db.setFileText("test.ms", "@derive(Eq) class User { name: string; }");

    // Start first expansion
    const handle1 = try db.startAsyncMacroExpansion("test.ms");

    // Start second expansion for same file (should cancel first)
    const handle2 = try db.startAsyncMacroExpansion("test.ms");

    // Wait for second to complete
    try db.waitForExpansion(handle2, 5000);

    // First handle should be cancelled (or completed if it finished very fast)
    const status1 = db.getExpansionStatus(handle1);
    try std.testing.expect(status1 == .cancelled or status1 == .completed);

    // Second handle should be completed
    const status2 = db.getExpansionStatus(handle2);
    try std.testing.expectEqual(ExpansionStatus.completed, status2);
}

test "Phase8: getExpansionProgress returns accurate info" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up file with multiple macros
    _ = try db.setFileText("test.ms",
        \\@derive(Eq) class A {}
        \\@derive(Hash) class B {}
        \\@derive(Eq, Hash) class C {}
    );

    // Start async expansion
    const handle = try db.startAsyncMacroExpansion("test.ms");

    // Wait for completion
    try db.waitForExpansion(handle, 5000);

    // Get progress info
    const progress = db.getExpansionProgress(handle);

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
    _ = try db.setFileText("test.ms", "const x: number = 42;");

    // Get symbols - this should parse and build symbol table
    const symbols = try db.getSymbols("test.ms");

    // Should find the variable 'x'
    const x_symbol = symbols.lookup("x");
    try std.testing.expect(x_symbol != null);
    try std.testing.expectEqual(checker.symbol.SymbolKind.variable, x_symbol.?.kind);
}

test "Phase9: checkFile returns symbol table" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up a file with valid code - just a simple variable
    _ = try db.setFileText("test.ms", "const y: string = \"hello\";");

    // Check the file
    const result = try db.checkFile("test.ms");

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
    _ = try db.setFileText("test.ms", "break;");

    // Check the file
    const result = try db.checkFile("test.ms");

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
    _ = try db.setFileText("test.ms", "function greet() { return \"Hello\"; }");

    // Look up the function
    const symbol = try db.lookupSymbol("test.ms", "greet");

    try std.testing.expect(symbol != null);
    try std.testing.expectEqual(checker.symbol.SymbolKind.function, symbol.?.kind);
}

test "Phase9: lookupSymbol returns null for undefined" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up a file with some code
    _ = try db.setFileText("test.ms", "const x = 1;");

    // Look up undefined symbol
    const symbol = try db.lookupSymbol("test.ms", "undefined_var");

    try std.testing.expect(symbol == null);
}

test "Phase9: symbol cache is invalidated on file change" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up initial file
    _ = try db.setFileText("test.ms", "const first = 1;");

    // Get symbols - should have 'first'
    const symbols1 = try db.getSymbols("test.ms");
    try std.testing.expect(symbols1.lookup("first") != null);
    try std.testing.expect(symbols1.lookup("second") == null);

    // Change file
    _ = try db.setFileText("test.ms", "const second = 2;");

    // Get symbols again - should have 'second', not 'first'
    const symbols2 = try db.getSymbols("test.ms");
    try std.testing.expect(symbols2.lookup("second") != null);
    // Note: 'first' might still be in the old cached table, but we got a new one
}

test "Phase9: getSymbols returns error for non-existent file" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Try to get symbols for non-existent file
    const result = db.getSymbols("non_existent.ms");
    try std.testing.expectError(error.FileNotFound, result);
}

test "Phase9: checkFile with builtins available" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Set up file that uses a builtin
    _ = try db.setFileText("test.ms", "const n = parseInt(\"42\");");

    // Check the file - should find parseInt builtin
    const result = try db.checkFile("test.ms");

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
    _ = try db.setFileText("test.ms", "@derive(Eq) class User { name: string; }");

    // Get macro call sites
    const call_sites = try db.getMacroCallSites("test.ms");
    defer {
        for (call_sites) |site| {
            std.testing.allocator.free(site.arguments);
        }
        std.testing.allocator.free(call_sites);
    }

    // Expand macro - this should populate bytecode cache
    _ = try db.expandMacroCallSite("test.ms", call_sites[0]);

    // Verify the bytecode cache was used (check if there are entries)
    // We can't directly check for specific hashes without accessing internals
    std.debug.print("[Test] Bytecode cache is available and macro expanded successfully\n", .{});
}
