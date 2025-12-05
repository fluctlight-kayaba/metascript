// Highlighting Module
// Handles syntax tokens, semantic tokens, and async macro expansion.
//
// This module provides:
// - getSyntaxTokens: Fast lexer-based syntax highlighting
// - getSemanticTokens: Semantic highlighting with macro expansion
// - startAsyncMacroExpansion: Background macro expansion
// - Progress tracking for long-running expansions

const std = @import("std");
const lexer_mod = @import("../lexer/lexer.zig");
const token_mod = @import("../lexer/token.zig");
const hash_mod = @import("hash.zig");
const types_mod = @import("types.zig");

const hashString = hash_mod.hashString;
const SyntaxToken = types_mod.SyntaxToken;
const SyntaxTokenType = types_mod.SyntaxTokenType;
const ExpansionHandle = types_mod.ExpansionHandle;
const ExpansionStatus = types_mod.ExpansionStatus;
const ExpansionProgress = types_mod.ExpansionProgress;
const ExpansionProgressCallback = types_mod.ExpansionProgressCallback;
const AsyncExpansionTask = types_mod.AsyncExpansionTask;
const ThreadContext = types_mod.ThreadContext;
const SemanticTokensResult = types_mod.SemanticTokensResult;
const SemanticTokensStatus = types_mod.SemanticTokensStatus;
const SemanticTokensDelta = types_mod.SemanticTokensDelta;
const SemanticTokenEdit = types_mod.SemanticTokenEdit;
const CachedSyntaxTokens = types_mod.CachedSyntaxTokens;

// Forward references
const TransAmDatabase = @import("transam.zig").TransAmDatabase;
const cancellation = @import("cancellation.zig");
const input_queries = @import("input_queries.zig");
const macro_queries = @import("macro_queries.zig");

/// Get syntax tokens for a file (lexer-based, independent of macros).
/// This is the FAST path - returns immediately without waiting for macro expansion.
pub fn getSyntaxTokens(db: *TransAmDatabase, file_id: []const u8) ![]SyntaxToken {
    const file_hash = hashString(file_id);

    // Check cache first
    if (db.syntax_token_cache.get(file_id)) |cached| {
        // Verify cache is still valid
        if (db.file_hashes.get(file_id)) |current_hash| {
            if (cached.file_hash == current_hash) {
                // Return copy of cached tokens
                return try db.allocator.dupe(SyntaxToken, cached.tokens);
            }
        }
    }

    // Cache miss or stale - regenerate from lexer
    const text = input_queries.getFileText(db, file_id) orelse return error.FileNotFound;
    const tokens = try generateSyntaxTokens(db, text);

    // Update cache (with proper error cleanup)
    const duped_tokens = try db.allocator.dupe(SyntaxToken, tokens);
    errdefer db.allocator.free(duped_tokens);

    const cached_tokens = CachedSyntaxTokens{
        .tokens = duped_tokens,
        .file_hash = file_hash,
        .computed_at = std.time.milliTimestamp(),
    };

    // Remove old cache entry if exists
    if (db.syntax_token_cache.fetchRemove(file_id)) |old| {
        db.allocator.free(old.key);
        db.allocator.free(old.value.tokens);
    }

    const owned_key = try db.allocator.dupe(u8, file_id);
    errdefer db.allocator.free(owned_key);

    try db.syntax_token_cache.put(owned_key, cached_tokens);

    return tokens;
}

/// Generate syntax tokens from source text using the lexer
fn generateSyntaxTokens(db: *TransAmDatabase, text: []const u8) ![]SyntaxToken {
    var lexer = lexer_mod.Lexer.init(db.allocator, text, 1) catch {
        return error.LexerInitFailed;
    };
    defer lexer.deinit();

    var tokens = std.ArrayList(SyntaxToken).init(db.allocator);
    errdefer tokens.deinit();

    var tok_count: usize = 0;
    while (true) {
        // Cancellation checkpoint: check every 500 tokens for large files
        if (tok_count % 500 == 0) {
            try cancellation.checkCancellation(db);
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
        .keyword_macro, .keyword_quote, .keyword_extern,
        .keyword_move,
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
        .question_question, .bang, .ampersand, .pipe, .caret, .tilde, .less_less,
        .greater_greater, .greater_greater_greater, .plus_plus, .minus_minus,
        .question, .dot, .dot_dot_dot, .arrow,
        => .operator,

        // Punctuation
        .left_paren, .right_paren, .left_brace, .right_brace,
        .left_bracket, .right_bracket, .semicolon, .colon, .comma,
        => .punctuation,

        // Comments
        .doc_comment => .comment,

        // Skip
        .newline, .end_of_file, .syntax_error, .regex,
        => null,
    };
}

/// Start async macro expansion for a file.
/// Returns immediately with a handle to track progress.
pub fn startAsyncMacroExpansion(db: *TransAmDatabase, file_id: []const u8) !ExpansionHandle {
    const file_id_hash = hashString(file_id);
    var handle: ExpansionHandle = undefined;
    var owned_file_id: []const u8 = undefined;

    // Critical section: update shared state
    {
        db.expansion_mutex.lock();
        defer db.expansion_mutex.unlock();

        // Cancel any existing expansion for this file
        var tasks_it = db.async_tasks.iterator();
        while (tasks_it.next()) |entry| {
            if (entry.value_ptr.file_id_hash == file_id_hash) {
                if (entry.value_ptr.status == .pending or entry.value_ptr.status == .running) {
                    entry.value_ptr.status = .cancelled;
                }
            }
        }

        // Create new expansion handle
        handle = ExpansionHandle{
            .id = db.next_expansion_id,
            .file_id_hash = file_id_hash,
            .started_at = std.time.milliTimestamp(),
        };
        db.next_expansion_id += 1;

        // Create task entry
        owned_file_id = try db.allocator.dupe(u8, file_id);
        errdefer db.allocator.free(owned_file_id);

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
        db.cancel_flag.store(false, .seq_cst);

        try db.async_tasks.put(handle.id, task);
    }
    // Mutex released here

    // Run synchronously for now (async threading requires thread-safe caches)
    runExpansionSync(db, handle.id, owned_file_id);

    return handle;
}

/// Run expansion synchronously (for now, until async threading is stable)
fn runExpansionSync(db: *TransAmDatabase, task_id: u64, file_id: []const u8) void {
    // Update status to running
    {
        db.expansion_mutex.lock();
        defer db.expansion_mutex.unlock();
        if (db.async_tasks.getPtr(task_id)) |task| {
            task.status = .running;
        }
    }

    // Get macro call sites (count for progress)
    const call_sites = macro_queries.getMacroCallSites(db,file_id) catch {
        db.expansion_mutex.lock();
        defer db.expansion_mutex.unlock();
        if (db.async_tasks.getPtr(task_id)) |task| {
            task.status = .failed;
        }
        return;
    };
    defer {
        for (call_sites) |site| {
            db.allocator.free(site.arguments);
        }
        db.allocator.free(call_sites);
    }

    // Update total_macros count
    {
        db.expansion_mutex.lock();
        defer db.expansion_mutex.unlock();
        if (db.async_tasks.getPtr(task_id)) |task| {
            task.total_macros = @intCast(call_sites.len);
        }
    }

    // Invoke initial progress callback
    invokeProgressCallback(db, task_id);

    // Expand each macro
    for (call_sites, 0..) |site, i| {
        // Check cancellation flag
        if (db.cancel_flag.load(.seq_cst)) {
            db.expansion_mutex.lock();
            defer db.expansion_mutex.unlock();
            if (db.async_tasks.getPtr(task_id)) |task| {
                if (task.status != .cancelled) {
                    task.status = .cancelled;
                }
            }
            return;
        }

        // Expand this macro call site
        _ = macro_queries.expandMacroCallSite(db,file_id, site) catch continue;

        // Update progress
        {
            db.expansion_mutex.lock();
            defer db.expansion_mutex.unlock();
            if (db.async_tasks.getPtr(task_id)) |task| {
                task.completed_macros = @intCast(i + 1);
            }
        }

        // Invoke progress callback
        invokeProgressCallback(db, task_id);
    }

    // Mark as completed
    {
        db.expansion_mutex.lock();
        defer db.expansion_mutex.unlock();
        if (db.async_tasks.getPtr(task_id)) |task| {
            if (task.status == .running) {
                task.status = .completed;
            }
        }
    }

    // Final progress callback
    invokeProgressCallback(db, task_id);
}

/// Cancel an async expansion
pub fn cancelAsyncExpansion(db: *TransAmDatabase, handle: ExpansionHandle) void {
    db.expansion_mutex.lock();
    defer db.expansion_mutex.unlock();

    if (db.async_tasks.getPtr(handle.id)) |task| {
        if (task.status == .pending or task.status == .running) {
            task.status = .cancelled;
            // Signal cancellation to running thread if any
            db.cancel_flag.store(true, .seq_cst);
        }
    }
}

/// Thread function for async macro expansion (currently unused - sync mode)
fn asyncExpansionThreadFn(ctx: *ThreadContext) void {
    const db = ctx.db;
    const task_id = ctx.task_id;
    const file_id = ctx.file_id;

    // Clean up thread context when done
    defer db.allocator.destroy(ctx);

    // Update status to running
    {
        db.expansion_mutex.lock();
        defer db.expansion_mutex.unlock();
        if (db.async_tasks.getPtr(task_id)) |task| {
            task.status = .running;
        }
    }

    // Get macro call sites (count for progress)
    const call_sites = macro_queries.getMacroCallSites(db,file_id) catch {
        db.expansion_mutex.lock();
        defer db.expansion_mutex.unlock();
        if (db.async_tasks.getPtr(task_id)) |task| {
            task.status = .failed;
        }
        return;
    };
    defer {
        for (call_sites) |site| {
            db.allocator.free(site.arguments);
        }
        db.allocator.free(call_sites);
    }

    // Update total_macros count
    {
        db.expansion_mutex.lock();
        defer db.expansion_mutex.unlock();
        if (db.async_tasks.getPtr(task_id)) |task| {
            task.total_macros = @intCast(call_sites.len);
        }
    }

    // Invoke initial progress callback
    invokeProgressCallback(db, task_id);

    // Expand each macro
    for (call_sites, 0..) |site, i| {
        // Check cancellation flag
        if (db.cancel_flag.load(.seq_cst)) {
            db.expansion_mutex.lock();
            defer db.expansion_mutex.unlock();
            if (db.async_tasks.getPtr(task_id)) |task| {
                if (task.status != .cancelled) {
                    task.status = .cancelled;
                }
            }
            return;
        }

        // Check if task was cancelled
        {
            db.expansion_mutex.lock();
            const status = if (db.async_tasks.get(task_id)) |task| task.status else .cancelled;
            db.expansion_mutex.unlock();
            if (status == .cancelled) return;
        }

        // Expand this macro call site
        _ = macro_queries.expandMacroCallSite(db,file_id, site) catch continue;

        // Update progress
        {
            db.expansion_mutex.lock();
            defer db.expansion_mutex.unlock();
            if (db.async_tasks.getPtr(task_id)) |task| {
                task.completed_macros = @intCast(i + 1);
            }
        }

        // Invoke progress callback
        invokeProgressCallback(db, task_id);
    }

    // Mark as completed
    {
        db.expansion_mutex.lock();
        defer db.expansion_mutex.unlock();
        if (db.async_tasks.getPtr(task_id)) |task| {
            if (task.status == .running) {
                task.status = .completed;
            }
        }
    }

    // Final progress callback
    invokeProgressCallback(db, task_id);
}

/// Helper to invoke progress callback with current task state
fn invokeProgressCallback(db: *TransAmDatabase, task_id: u64) void {
    const callback = db.progress_callback orelse return;
    const ctx = db.progress_callback_ctx; // May be null, that's OK

    db.expansion_mutex.lock();
    const task = db.async_tasks.get(task_id);
    db.expansion_mutex.unlock();

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
pub fn getExpansionStatus(db: *TransAmDatabase, handle: ExpansionHandle) ExpansionStatus {
    db.expansion_mutex.lock();
    defer db.expansion_mutex.unlock();

    if (db.async_tasks.get(handle.id)) |task| {
        return task.status;
    }
    return .cancelled; // Unknown handle treated as cancelled
}

/// Wait for an async expansion to complete (with timeout)
pub fn waitForExpansion(db: *TransAmDatabase, handle: ExpansionHandle, timeout_ms: u64) !void {
    const start = std.time.milliTimestamp();
    const deadline = start + @as(i64, @intCast(timeout_ms));

    while (std.time.milliTimestamp() < deadline) {
        const status = getExpansionStatus(db, handle);
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
    db: *TransAmDatabase,
    callback: ExpansionProgressCallback,
    ctx: ?*anyopaque,
) void {
    db.progress_callback = callback;
    db.progress_callback_ctx = ctx;
}

/// Get progress info for an expansion handle
pub fn getExpansionProgress(db: *TransAmDatabase, handle: ExpansionHandle) ?ExpansionProgress {
    db.expansion_mutex.lock();
    defer db.expansion_mutex.unlock();

    if (db.async_tasks.get(handle.id)) |task| {
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
pub fn getSemanticTokens(db: *TransAmDatabase, file_id: []const u8) ![]SyntaxToken {
    // For now, semantic tokens are same as syntax tokens
    return getSyntaxTokens(db, file_id);
}

/// Get semantic tokens with version for delta tracking
pub fn getSemanticTokensWithVersion(db: *TransAmDatabase, file_id: []const u8) !SemanticTokensResult {
    const tokens = try getSyntaxTokens(db, file_id);

    // Increment version
    db.syntax_token_version += 1;
    const version = db.syntax_token_version;

    // Store version for this file (remove old key first to avoid leaks)
    if (db.semantic_token_versions.fetchRemove(file_id)) |old| {
        db.allocator.free(old.key);
    }
    const owned_key = try db.allocator.dupe(u8, file_id);
    errdefer db.allocator.free(owned_key);

    try db.semantic_token_versions.put(owned_key, version);

    return .{
        .tokens = tokens,
        .version = version,
    };
}

/// Get semantic tokens delta since a previous version
pub fn getSemanticTokensDelta(
    db: *TransAmDatabase,
    file_id: []const u8,
    since_version: u64,
) !SemanticTokensDelta {
    _ = since_version;

    // For now, always return full tokens (delta computation is complex)
    const tokens = try getSyntaxTokens(db, file_id);

    db.syntax_token_version += 1;

    return .{
        .edits = &[_]SemanticTokenEdit{},
        .full_tokens = tokens,
        .result_version = db.syntax_token_version,
    };
}

/// Get the status of semantic tokens for a file
pub fn getSemanticTokensStatus(db: *TransAmDatabase, file_id: []const u8) SemanticTokensStatus {
    const file_id_hash = hashString(file_id);

    // Check if any expansion is pending for this file
    db.expansion_mutex.lock();
    defer db.expansion_mutex.unlock();

    var tasks_it = db.async_tasks.iterator();
    while (tasks_it.next()) |entry| {
        if (entry.value_ptr.file_id_hash == file_id_hash) {
            if (entry.value_ptr.status == .pending or entry.value_ptr.status == .running) {
                return .pending_expansion;
            }
        }
    }

    // Check if we have syntax tokens cached
    if (db.syntax_token_cache.contains(file_id)) {
        return .available;
    }

    return .syntax_only;
}
