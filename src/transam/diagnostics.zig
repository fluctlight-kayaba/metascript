// Diagnostics Module
// Handles incremental diagnostics generation and caching.
//
// This module provides:
// - getDiagnostics: Get cached or fresh diagnostics for a file
// - generateDiagnostics: Generate from lexer errors
// - generateDiagnosticsForFile: Generate from parser errors
// - freeDiagnostics: Clean up diagnostics memory

const std = @import("std");
const lexer_mod = @import("../lexer/lexer.zig");
const hash_mod = @import("hash.zig");
const types_mod = @import("types.zig");

const hashString = hash_mod.hashString;
const Diagnostic = types_mod.Diagnostic;
const CachedDiagnostics = types_mod.CachedDiagnostics;

// Forward references
const TransAmDatabase = @import("transam.zig").TransAmDatabase;
const cancellation = @import("cancellation.zig");

/// Get diagnostics for a file (cached, incremental).
/// Returns cached diagnostics if file hasn't changed, otherwise recomputes.
pub fn getDiagnostics(db: *TransAmDatabase, file_id: []const u8) ![]Diagnostic {
    try cancellation.checkCancellation(db);

    const file_hash = hashString(file_id);

    // Check cache first
    if (db.diagnostics_cache.get(file_id)) |cached| {
        if (db.file_hashes.get(file_id)) |current_hash| {
            if (cached.file_hash == current_hash) {
                return try db.allocator.dupe(Diagnostic, cached.diagnostics);
            }
        }
    }

    // Cache miss - regenerate
    const diagnostics = try generateDiagnosticsForFile(db, file_id);

    // Update cache
    const duped_diags = try dupeDiagnostics(db, diagnostics);
    errdefer freeDiagnostics(db, duped_diags);

    const cached_diags = CachedDiagnostics{
        .diagnostics = duped_diags,
        .file_hash = file_hash,
        .computed_at = std.time.milliTimestamp(),
    };

    // Remove old cache entry if exists
    if (db.diagnostics_cache.fetchRemove(file_id)) |old| {
        db.allocator.free(old.key);
        for (old.value.diagnostics) |diag| {
            db.allocator.free(diag.message);
        }
        db.allocator.free(old.value.diagnostics);
    }

    const owned_key = try db.allocator.dupe(u8, file_id);
    errdefer db.allocator.free(owned_key);

    try db.diagnostics_cache.put(owned_key, cached_diags);

    return diagnostics;
}

/// Generate diagnostics from source text using lexer
fn generateDiagnostics(db: *TransAmDatabase, text: []const u8) ![]Diagnostic {
    var diagnostics = std.ArrayList(Diagnostic).init(db.allocator);
    errdefer diagnostics.deinit();

    var lexer = lexer_mod.Lexer.init(db.allocator, text, 1) catch {
        return error.LexerInitFailed;
    };
    defer lexer.deinit();

    // Consume all tokens to collect lexer errors
    var tok_count: usize = 0;
    while (true) {
        if (tok_count % 500 == 0) {
            try cancellation.checkCancellation(db);
        }
        tok_count += 1;
        const tok = lexer.next() catch break;
        if (tok.kind == .end_of_file) break;
    }

    // Convert lexer errors to diagnostics
    for (lexer.errors.items) |err| {
        const message = try db.allocator.dupe(u8, err.message);
        errdefer db.allocator.free(message);

        const diag = Diagnostic{
            .start_line = if (err.loc.start.line > 0) err.loc.start.line - 1 else 0,
            .start_col = err.loc.start.column,
            .end_line = if (err.loc.end.line > 0) err.loc.end.line - 1 else 0,
            .end_col = err.loc.end.column,
            .severity = .@"error",
            .message = message,
            .source = "mls-lexer",
        };
        try diagnostics.append(diag);
    }

    return try diagnostics.toOwnedSlice();
}

/// Generate diagnostics for a file using parser
fn generateDiagnosticsForFile(db: *TransAmDatabase, file_id: []const u8) ![]Diagnostic {
    var diagnostics = std.ArrayList(Diagnostic).init(db.allocator);
    errdefer diagnostics.deinit();

    // Parse the file
    const parse_result = db.parse(file_id) catch |err| {
        const message = try db.allocator.dupe(u8, @errorName(err));
        errdefer db.allocator.free(message);

        const diag = Diagnostic{
            .start_line = 0,
            .start_col = 0,
            .end_line = 0,
            .end_col = 0,
            .severity = .@"error",
            .message = message,
            .source = "mls-parser",
        };
        try diagnostics.append(diag);
        return try diagnostics.toOwnedSlice();
    };

    // Convert parse errors to diagnostics
    for (parse_result.errors) |err| {
        const message = try db.allocator.dupe(u8, err.message);
        errdefer db.allocator.free(message);

        const diag = Diagnostic{
            .start_line = if (err.location.start.line > 0) err.location.start.line - 1 else 0,
            .start_col = err.location.start.column,
            .end_line = if (err.location.end.line > 0) err.location.end.line - 1 else 0,
            .end_col = err.location.end.column,
            .severity = .@"error",
            .message = message,
            .source = "mls-parser",
        };
        try diagnostics.append(diag);
    }

    return try diagnostics.toOwnedSlice();
}

/// Duplicate diagnostics array (deep copy messages)
fn dupeDiagnostics(db: *TransAmDatabase, diagnostics: []const Diagnostic) ![]Diagnostic {
    var result = try db.allocator.alloc(Diagnostic, diagnostics.len);
    for (diagnostics, 0..) |diag, i| {
        result[i] = .{
            .start_line = diag.start_line,
            .start_col = diag.start_col,
            .end_line = diag.end_line,
            .end_col = diag.end_col,
            .severity = diag.severity,
            .message = try db.allocator.dupe(u8, diag.message),
            .source = diag.source,
        };
    }
    return result;
}

/// Free diagnostics array
pub fn freeDiagnostics(db: *TransAmDatabase, diagnostics: []Diagnostic) void {
    for (diagnostics) |diag| {
        db.allocator.free(diag.message);
    }
    db.allocator.free(diagnostics);
}
