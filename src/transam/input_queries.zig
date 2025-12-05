// Input Queries Module
// Handles file text input and change detection for the Trans-Am database.
//
// These are "input queries" in Salsa terminology - they represent external
// inputs that drive the incremental computation system.

const std = @import("std");
const hash_mod = @import("hash.zig");
const types_mod = @import("types.zig");

const hashString = hash_mod.hashString;
const Revision = types_mod.Revision;
const QueryState = types_mod.QueryState;

// Forward reference to TransAmDatabase and type_check (defined in transam.zig)
const TransAmDatabase = @import("transam.zig").TransAmDatabase;
const type_check = @import("type_check.zig");

// ===== INPUT QUERIES (never cached, set by LSP) =====

/// Set file text - invalidates all dependent queries
/// Returns true if content actually changed (useful for optimization)
pub fn setFileText(db: *TransAmDatabase, file_id: []const u8, text: []const u8) !bool {
    // Compute content hash for change detection
    const new_hash = hashString(text);
    const old_hash = db.file_hashes.get(file_id);

    // Check if content actually changed (content-addressed optimization)
    if (old_hash != null and old_hash.? == new_hash) {
        // Content unchanged, no need to invalidate
        return false;
    }

    // Free old values if they exist
    // Note: We use file_id (parameter) for lookup, not the stored key
    if (db.file_texts.get(file_id) != null) {
        // Remove from both maps first, then free
        _ = db.file_hashes.remove(file_id);
        if (db.file_texts.fetchRemove(file_id)) |old_entry| {
            // old_entry.key is the owned key we allocated
            // old_entry.value is the owned text we allocated
            db.allocator.free(old_entry.key);
            db.allocator.free(old_entry.value);
        }
    }

    // Store new text (with proper error cleanup)
    const owned_text = try db.allocator.dupe(u8, text);
    errdefer db.allocator.free(owned_text);

    const owned_id = try db.allocator.dupe(u8, file_id);
    errdefer db.allocator.free(owned_id);

    try db.file_texts.put(owned_id, owned_text);
    errdefer _ = db.file_texts.remove(owned_id);

    try db.file_hashes.put(owned_id, new_hash);

    // Mark file as dirty
    const file_id_hash = hashString(file_id);
    try db.dirty_files.put(file_id_hash, {});

    // Increment revision (triggers invalidation)
    db.current_revision.increment();
    db.revision = db.current_revision.value; // Sync legacy field

    // Invalidate parse cache for this file
    db.parse_cache.remove(file_id_hash);

    // Phase 9: Invalidate type checker cache for this file
    type_check.invalidateSymbolCache(db, file_id);

    // Mark all cached queries as RED (potentially stale)
    // They'll be verified lazily via try_mark_green when accessed
    var it = db.query_cache.valueIterator();
    while (it.next()) |qv| {
        if (qv.state == .green) {
            qv.state = .red;
        }
    }

    return true;
}

/// Get file text (input query)
pub fn getFileText(db: *TransAmDatabase, file_id: []const u8) ?[]const u8 {
    return db.file_texts.get(file_id);
}

/// Get file content hash (for dependency checking)
pub fn getFileHash(db: *TransAmDatabase, file_id: []const u8) ?u64 {
    return db.file_hashes.get(file_id);
}

/// Check if file has changed since given revision
pub fn hasFileChanged(db: *TransAmDatabase, file_id: []const u8, since_revision: Revision) bool {
    const file_id_hash = hashString(file_id);
    if (db.dirty_files.contains(file_id_hash)) {
        // File was modified at some point
        // For now, assume it changed if dirty (future: track per-file revision)
        return db.current_revision.value > since_revision.value;
    }
    return false;
}
