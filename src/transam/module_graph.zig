// Module Graph Module
// Handles module dependency tracking for incremental invalidation.
//
// Maintains two maps:
// - module_imports: file -> [files it imports]
// - module_importers: file -> [files that import it]
//
// This enables efficient transitive invalidation when a file changes.

const std = @import("std");
const hash_mod = @import("hash.zig");

const hashString = hash_mod.hashString;

// Forward references
const TransAmDatabase = @import("transam.zig").TransAmDatabase;
const type_check = @import("type_check.zig");

/// Record that file_id imports imported_file
/// This builds the dependency graph for incremental invalidation
pub fn recordModuleImport(db: *TransAmDatabase, file_id: []const u8, imported_file: []const u8) !void {
    // Add to forward map: file_id -> [imported_file, ...]
    const imports_result = try db.module_imports.getOrPut(file_id);
    if (!imports_result.found_existing) {
        imports_result.value_ptr.* = std.ArrayList([]const u8).init(db.allocator);
    }
    // Check for duplicates before adding
    for (imports_result.value_ptr.items) |existing| {
        if (std.mem.eql(u8, existing, imported_file)) {
            return; // Already recorded
        }
    }
    try imports_result.value_ptr.append(imported_file);

    // Add to reverse map: imported_file -> [file_id, ...]
    const importers_result = try db.module_importers.getOrPut(imported_file);
    if (!importers_result.found_existing) {
        importers_result.value_ptr.* = std.ArrayList([]const u8).init(db.allocator);
    }
    // Check for duplicates
    for (importers_result.value_ptr.items) |existing| {
        if (std.mem.eql(u8, existing, file_id)) {
            return; // Already recorded
        }
    }
    try importers_result.value_ptr.append(file_id);
}

/// Get all files that import the given file (for invalidation propagation)
/// Returns null if no files import this module
pub fn getModuleImporters(db: *TransAmDatabase, file_id: []const u8) ?[]const []const u8 {
    if (db.module_importers.get(file_id)) |list| {
        if (list.items.len > 0) {
            return list.items;
        }
    }
    return null;
}

/// Get all files that the given file imports
/// Returns null if this file doesn't import any modules
pub fn getModuleImports(db: *TransAmDatabase, file_id: []const u8) ?[]const []const u8 {
    if (db.module_imports.get(file_id)) |list| {
        if (list.items.len > 0) {
            return list.items;
        }
    }
    return null;
}

/// Clear imports for a file (call before re-parsing to update the dependency graph)
/// This ensures the graph stays accurate when files are modified
pub fn clearModuleImports(db: *TransAmDatabase, file_id: []const u8) void {
    // Get the files this module imports (need mutable pointer to clear it)
    if (db.module_imports.getPtr(file_id)) |imports_list| {
        // Remove this file from the importers list of each imported file
        for (imports_list.items) |imported| {
            if (db.module_importers.getPtr(imported)) |importers_list| {
                // Find and remove file_id from the importers list
                var i: usize = 0;
                while (i < importers_list.items.len) {
                    if (std.mem.eql(u8, importers_list.items[i], file_id)) {
                        _ = importers_list.swapRemove(i);
                        break;
                    }
                    i += 1;
                }
            }
        }
        // Clear the imports list (but keep the ArrayList allocated)
        imports_list.clearRetainingCapacity();
    }
}

/// Invalidate all files that depend on the given file (transitive)
/// Returns the count of files invalidated
pub fn invalidateDependents(db: *TransAmDatabase, file_id: []const u8) !usize {
    var invalidated: usize = 0;
    var to_process = std.ArrayList([]const u8).init(db.allocator);
    defer to_process.deinit();

    try to_process.append(file_id);

    // Track visited to avoid cycles
    var visited = std.StringHashMap(void).init(db.allocator);
    defer visited.deinit();
    try visited.put(file_id, {});

    while (to_process.items.len > 0) {
        const current = to_process.pop().?;

        // Get all files that import the current file
        if (getModuleImporters(db, current)) |importers| {
            for (importers) |importer| {
                if (!visited.contains(importer)) {
                    try visited.put(importer, {});
                    try to_process.append(importer);

                    // Mark this file as dirty
                    const importer_hash = hashString(importer);
                    try db.dirty_files.put(importer_hash, {});

                    // CRITICAL: Also invalidate symbol cache for dependent files
                    // Without this, dependents return stale cached symbols even though
                    // their imports changed. This ensures type checking is re-run.
                    type_check.invalidateSymbolCache(db, importer);

                    invalidated += 1;
                }
            }
        }
    }

    return invalidated;
}
