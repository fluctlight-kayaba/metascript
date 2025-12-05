// Type Checking Queries Module
// Handles symbol tables, type checking, and DRC analysis queries.
//
// This module provides:
// - getSymbols: Get symbol table for a file
// - checkFile: Run full type checking with error reporting
// - lookupSymbol/lookupSymbolAtPosition: Find symbols by name/position
// - getDrcDiagnostics: Run DRC analysis for memory management warnings
// - invalidateSymbolCache: Cache invalidation on file change

const std = @import("std");
const checker = @import("../checker/typechecker.zig");
const drc_mod = @import("../analysis/drc.zig");
const DrcAnalyzer = @import("../analysis/drc_analyzer.zig").DrcAnalyzer;
const types_mod = @import("types.zig");

const Drc = drc_mod.Drc;
const DrcDiagnostic = drc_mod.DrcDiagnostic;
const DrcVariable = drc_mod.Variable;
const TypeCheckResult = types_mod.TypeCheckResult;
const CachedSymbolTable = types_mod.CachedSymbolTable;

// Forward reference to TransAmDatabase and cancellation
const TransAmDatabase = @import("transam.zig").TransAmDatabase;
const cancellation = @import("cancellation.zig");

/// Variable ownership information for hover display
pub const VariableOwnership = struct {
    ownership_state: DrcVariable.OwnershipState,
    needs_rc: bool,
    is_parameter: bool,
    is_module_level: bool,
};

/// Get symbol table for a file (cached, depends on parse)
/// This is the entry point for type information queries.
/// Following Nim's pattern: parse first, then type-check the result.
pub fn getSymbols(db: *TransAmDatabase, file_id: []const u8) !*checker.symbol.SymbolTable {
    try cancellation.checkCancellation(db);

    const file_hash = db.file_hashes.get(file_id) orelse return error.FileNotFound;

    // Check cache
    if (db.symbols_cache.get(file_hash)) |cached| {
        // Verify cache is still valid (same file content)
        if (cached.file_hash == file_hash) {
            return cached.table;
        }
    }

    // Cache miss: parse and build symbol table
    const parse_result = try db.parse(file_id);
    const tree = parse_result.tree;

    // Create type checker (owns the symbol table)
    const type_checker = try db.allocator.create(checker.TypeChecker);
    type_checker.* = try checker.TypeChecker.init(db.allocator);

    // Run type checking (all phases: collect declarations, resolve types, infer types, check types)
    _ = type_checker.check(tree) catch |err| {
        type_checker.deinit();
        db.allocator.destroy(type_checker);
        return err;
    };

    // Cache the result
    const cached = CachedSymbolTable{
        .table = &type_checker.symbols,
        .type_checker = type_checker,
        .file_hash = file_hash,
        .computed_at = std.time.milliTimestamp(),
    };
    try db.symbols_cache.put(file_hash, cached);

    return &type_checker.symbols;
}

/// Type check a file and return result with errors
/// This is a convenience wrapper over getSymbols that also returns errors.
pub fn checkFile(db: *TransAmDatabase, file_id: []const u8) !TypeCheckResult {
    try cancellation.checkCancellation(db);

    const file_hash = db.file_hashes.get(file_id) orelse return error.FileNotFound;

    // Check if we already have a cached type checker
    if (db.symbols_cache.get(file_hash)) |cached| {
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
    const parse_result = try db.parse(file_id);
    const tree = parse_result.tree;

    // Create type checker
    const type_checker = try db.allocator.create(checker.TypeChecker);
    type_checker.* = try checker.TypeChecker.init(db.allocator);

    // Run full type checking
    const success = type_checker.check(tree) catch |err| {
        type_checker.deinit();
        db.allocator.destroy(type_checker);
        return err;
    };

    // Cache the result
    const cached = CachedSymbolTable{
        .table = &type_checker.symbols,
        .type_checker = type_checker,
        .file_hash = file_hash,
        .computed_at = std.time.milliTimestamp(),
    };
    try db.symbols_cache.put(file_hash, cached);

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
/// Uses lookupAll to search ALL scopes (including function-local variables)
pub fn lookupSymbol(db: *TransAmDatabase, file_id: []const u8, name: []const u8) !?checker.symbol.Symbol {
    const symbols = try getSymbols(db, file_id);
    return symbols.lookupAll(name);
}

/// Look up a symbol at a specific position (line, column)
/// Position-aware: finds the symbol visible at that location.
/// This handles variable shadowing correctly - when multiple variables
/// have the same name in different scopes, it returns the one that's
/// actually visible at the cursor position.
pub fn lookupSymbolAtPosition(db: *TransAmDatabase, file_id: []const u8, name: []const u8, line: u32, column: u32) !?checker.symbol.Symbol {
    const symbols = try getSymbols(db, file_id);
    return symbols.lookupAtPosition(name, line, column);
}

/// Run DRC (Deterministic Reference Counting) analysis and return diagnostics
/// This surfaces memory management issues like use-after-move and potential cycles
/// for LSP integration (warnings/hints in the editor).
pub fn getDrcDiagnostics(db: *TransAmDatabase, file_id: []const u8) ![]const DrcDiagnostic {
    try cancellation.checkCancellation(db);

    // Check cache first (use optional properly, not sentinel value)
    const maybe_file_hash = db.file_hashes.get(file_id);
    if (maybe_file_hash) |file_hash| {
        if (db.drc_diagnostics_cache.get(file_hash)) |cached| {
            // Cache hit
            db.drc_cache_stats.hits += 1;

            // Return copy of cached diagnostics
            if (cached.diagnostics.len == 0) {
                return &[_]DrcDiagnostic{};
            }
            return copyDrcDiagnostics(db, cached.diagnostics);
        }
    }

    // Cache miss - compute DRC diagnostics
    db.drc_cache_stats.misses += 1;

    // Get the parsed AST
    const parse_result = try db.parse(file_id);
    if (parse_result.errors.len > 0) {
        // Don't run DRC on files with parse errors
        return &[_]DrcDiagnostic{};
    }

    // Run DRC analysis
    var drc = Drc.init(db.allocator);
    defer drc.deinit();

    var analyzer = DrcAnalyzer.init(&drc);
    analyzer.analyze(parse_result.tree) catch {
        // DRC analysis failed (probably due to AST structure issues)
        return &[_]DrcDiagnostic{};
    };

    try drc.finalize();

    // Handle empty result
    const source_diags = drc.getDiagnostics();
    if (source_diags.len == 0) {
        // Cache empty result if we have a file hash
        if (maybe_file_hash) |file_hash| {
            try db.drc_diagnostics_cache.put(file_hash, .{
                .diagnostics = &[_]DrcDiagnostic{},
                .file_hash = file_hash,
                .computed_at = std.time.milliTimestamp(),
            });
        }
        return &[_]DrcDiagnostic{};
    }

    // Allocate and copy diagnostics for cache (with proper error cleanup)
    const cache_diags = try copyDrcDiagnostics(db, source_diags);

    // Store in cache if we have a file hash
    if (maybe_file_hash) |file_hash| {
        // errdefer: if put fails, we still own cache_diags and must free it
        errdefer freeDrcDiagnostics(db, cache_diags);

        try db.drc_diagnostics_cache.put(file_hash, .{
            .diagnostics = cache_diags,
            .file_hash = file_hash,
            .computed_at = std.time.milliTimestamp(),
        });
        // Return another copy for caller (cache owns cache_diags)
        return copyDrcDiagnostics(db, cache_diags);
    } else {
        // No caching possible, return the copy directly
        return cache_diags;
    }
}

/// Helper: Copy DRC diagnostics array with proper error cleanup
/// Caller owns the returned slice and must call freeDrcDiagnostics
fn copyDrcDiagnostics(db: *TransAmDatabase, source: []const DrcDiagnostic) ![]DrcDiagnostic {
    const result = try db.allocator.alloc(DrcDiagnostic, source.len);
    var initialized: usize = 0;

    errdefer {
        // Clean up partially initialized diagnostics on error
        for (result[0..initialized]) |diag| {
            db.allocator.free(diag.message);
        }
        db.allocator.free(result);
    }

    for (source, 0..) |diag, i| {
        result[i] = .{
            .line = diag.line,
            .column = diag.column,
            .end_line = diag.end_line,
            .end_column = diag.end_column,
            .severity = diag.severity,
            .message = try db.allocator.dupe(u8, diag.message),
            .code = diag.code,
        };
        initialized = i + 1;
    }

    return result;
}

/// Free DRC diagnostics returned by getDrcDiagnostics
/// Takes ownership and deallocates - caller must not use diags after this call.
/// Note: getDrcDiagnostics may return a static empty slice, so we must check length.
/// The const parameter is intentional: we're deallocating, not modifying contents.
pub fn freeDrcDiagnostics(db: *TransAmDatabase, diags: []const DrcDiagnostic) void {
    // Don't try to free empty slices - they may be static constants
    if (diags.len == 0) return;

    for (diags) |diag| {
        db.allocator.free(diag.message);
    }
    db.allocator.free(diags);
}

/// Get variable ownership information for hover display
/// Runs DRC analysis on-demand if not cached
pub fn getVariableOwnership(db: *TransAmDatabase, file_id: []const u8, var_name: []const u8) !?VariableOwnership {
    try cancellation.checkCancellation(db);

    // Get the parsed AST
    const parse_result = try db.parse(file_id);
    if (parse_result.errors.len > 0) {
        return null;
    }

    // Run DRC analysis
    var drc = Drc.init(db.allocator);
    defer drc.deinit();

    var analyzer = DrcAnalyzer.init(&drc);
    analyzer.analyze(parse_result.tree) catch {
        return null;
    };

    try drc.finalize();

    // Look up the variable
    const variable = drc.variables.get(var_name) orelse return null;

    return VariableOwnership{
        .ownership_state = variable.ownership_state,
        .needs_rc = variable.needs_rc,
        .is_parameter = variable.is_parameter,
        .is_module_level = variable.is_module_level,
    };
}

/// Invalidate type checker cache for a file (called when file changes)
pub fn invalidateSymbolCache(db: *TransAmDatabase, file_id: []const u8) void {
    const file_hash = db.file_hashes.get(file_id) orelse return;

    if (db.symbols_cache.fetchRemove(file_hash)) |removed| {
        removed.value.type_checker.deinit();
        db.allocator.destroy(removed.value.type_checker);
    }

    // Also invalidate function check cache for this file
    // Note: We iterate to find entries for this file - a future optimization
    // could use a secondary index file_hash -> [function_check_keys]
    var fc_it = db.function_check_cache.iterator();
    var keys_to_remove = std.ArrayList(u64).init(db.allocator);
    defer keys_to_remove.deinit();

    while (fc_it.next()) |entry| {
        if (entry.value_ptr.file_hash == file_hash) {
            keys_to_remove.append(entry.key_ptr.*) catch {};
        }
    }

    for (keys_to_remove.items) |key| {
        if (db.function_check_cache.fetchRemove(key)) |removed| {
            db.allocator.free(removed.value.func_name);
        }
    }

    // Also invalidate DRC diagnostics cache for this file
    if (db.drc_diagnostics_cache.fetchRemove(file_hash)) |removed| {
        db.drc_cache_stats.invalidations += 1;
        // Free cached diagnostic messages (but not empty static slices)
        if (removed.value.diagnostics.len > 0) {
            for (removed.value.diagnostics) |diag| {
                db.allocator.free(diag.message);
            }
            db.allocator.free(removed.value.diagnostics);
        }
    }
}
