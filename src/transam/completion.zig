// Completion Module
// Handles code completion with stale-while-revalidate caching.
//
// This module provides:
// - getCompletions: Get completion items with caching
// - generateCompletions: Generate completions from lexer tokens
// - invalidateCompletions: Invalidate completion cache

const std = @import("std");
const lexer_mod = @import("../lexer/lexer.zig");
const token_mod = @import("../lexer/token.zig");
const hash_mod = @import("hash.zig");
const cache_mod = @import("cache.zig");

const hashString = hash_mod.hashString;
const CompletionItem = cache_mod.CompletionItem;
const CompletionKey = cache_mod.CompletionKey;

// Forward reference to TransAmDatabase and cancellation
const TransAmDatabase = @import("transam.zig").TransAmDatabase;
const cancellation = @import("cancellation.zig");
const input_queries = @import("input_queries.zig");

/// Result of getCompletions
pub const CompletionResult = struct {
    items: []CompletionItem,
    from_cache: bool,
    is_stale: bool,
};

/// Get completions for a file, using stale-while-revalidate cache
/// Returns completion items and whether they came from cache
pub fn getCompletions(db: *TransAmDatabase, file_id: []const u8) !CompletionResult {
    try cancellation.checkCancellation(db);

    const file_hash = hashString(file_id);
    const current_content_hash = db.file_hashes.get(file_id) orelse 0;

    // Use file-level completion key (line=0, column=0, prefix=0)
    const key = CompletionKey{
        .file_hash = current_content_hash,
        .line = 0,
        .column = 0,
        .prefix_hash = file_hash, // Use file_id hash as extra discriminator
    };

    // Check cache - returns stale results if available
    if (db.completion_cache.get(key)) |cached| {
        // Since key includes content hash, cache hit means fresh data
        return .{
            .items = try db.allocator.dupe(CompletionItem, cached),
            .from_cache = true,
            .is_stale = false,
        };
    }

    // Try stale key (previous content hash) if available
    const stale_key = CompletionKey{
        .file_hash = 0, // Will check any matching prefix_hash
        .line = 0,
        .column = 0,
        .prefix_hash = file_hash,
    };
    if (db.completion_cache.get(stale_key)) |cached| {
        // Stale hit - return old data while background refreshes
        return .{
            .items = try db.allocator.dupe(CompletionItem, cached),
            .from_cache = true,
            .is_stale = true,
        };
    }

    // Cache miss - generate completions
    const items = try generateCompletions(db, file_id);

    // Cache the result
    db.completion_cache.put(key, items) catch {};

    return .{
        .items = items,
        .from_cache = false,
        .is_stale = false,
    };
}

/// Generate completion items by scanning the file
fn generateCompletions(db: *TransAmDatabase, file_id: []const u8) ![]CompletionItem {
    const text = input_queries.getFileText(db, file_id) orelse return error.FileNotFound;

    var lexer = lexer_mod.Lexer.init(db.allocator, text, 1) catch {
        return error.LexerInitFailed;
    };
    defer lexer.deinit();

    var items = std.ArrayList(CompletionItem).init(db.allocator);
    errdefer items.deinit();

    var seen = std.StringHashMap(void).init(db.allocator);
    defer seen.deinit();

    var prev_token: ?token_mod.Token = null;
    var tok_count: usize = 0;

    while (true) {
        // Cancellation checkpoint
        if (tok_count % 500 == 0) {
            try cancellation.checkCancellation(db);
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

                        const label = try db.allocator.dupe(u8, tok.text);
                        errdefer db.allocator.free(label);

                        try items.append(.{
                            .label = label,
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
pub fn invalidateCompletions(db: *TransAmDatabase, file_id: []const u8) void {
    _ = file_id;
    db.completion_cache.invalidateAll();
}
