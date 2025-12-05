// Red-Green Algorithm Module
// Implements the Salsa-inspired incremental computation algorithm.
//
// RED-GREEN ALGORITHM:
// - GREEN: Query result verified as current in this revision
// - RED: Query result may be stale, needs verification
// - YELLOW: Currently being verified (cycle detection)
//
// Key insight: When a dependency changes, we don't immediately recompute.
// Instead, we try_mark_green() - recursively check if the OUTPUT actually
// changed. If not, we mark GREEN without recomputing dependents.

const std = @import("std");
const hash_mod = @import("hash.zig");
const types_mod = @import("types.zig");

const hashString = hash_mod.hashString;
const QueryKey = types_mod.QueryKey;
const QueryValue = types_mod.QueryValue;
const QueryState = types_mod.QueryState;
const Durability = types_mod.Durability;
const DependencyFrame = types_mod.DependencyFrame;

// Forward reference to TransAmDatabase
const TransAmDatabase = @import("transam.zig").TransAmDatabase;

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
pub fn tryMarkGreen(db: *TransAmDatabase, key: QueryKey) RedGreenError!bool {
    const entry = db.query_cache.getPtr(key.hash()) orelse return false;

    // Already verified this revision?
    if (entry.verified_at.eq(db.current_revision)) {
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
            const current_file_hash = getFileHashByQueryKey(db, dep_key);
            if (current_file_hash != dep_key.input_hash) {
                // Input changed -> must recompute
                entry.state = .red;
                return false;
            }
            continue;
        }

        // Derived query: recursively verify
        const dep_entry = db.query_cache.getPtr(dep_key.hash());
        if (dep_entry == null) {
            // Dependency no longer exists -> must recompute
            entry.state = .red;
            return false;
        }

        // Recursively try to mark dependency green
        const dep_is_green = try tryMarkGreen(db, dep_key);
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
    entry.verified_at = db.current_revision;
    entry.state = .green;
    return true;
}

/// Get file hash for an input query key
fn getFileHashByQueryKey(db: *TransAmDatabase, key: QueryKey) u64 {
    // For file_text queries, we need to look up the current file hash
    // The input_hash was the hash at computation time
    // We iterate through file_hashes to find matching file
    var it = db.file_hashes.iterator();
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
    db: *TransAmDatabase,
    key: QueryKey,
    value: *anyopaque,
    type_id: u64,
    value_hash: u64,
    dependencies: []const QueryKey,
    durability: Durability,
) !void {
    // Copy dependencies to owned memory
    const owned_deps = try db.allocator.dupe(QueryKey, dependencies);
    errdefer db.allocator.free(owned_deps);

    const query_value = QueryValue{
        .value = value,
        .type_id = type_id,
        .computed_at = db.current_revision,
        .verified_at = db.current_revision,
        .dependencies = owned_deps,
        .durability = durability,
        .value_hash = value_hash,
        .state = .green,
    };

    // Remove old entry if exists (free old dependencies)
    if (db.query_cache.fetchRemove(key.hash())) |old| {
        db.allocator.free(old.value.dependencies);
    }

    try db.query_cache.put(key.hash(), query_value);
}

/// Get a cached query result if valid
pub fn getCachedQuery(db: *TransAmDatabase, key: QueryKey) ?*QueryValue {
    const entry = db.query_cache.getPtr(key.hash()) orelse return null;

    // If already GREEN in current revision, return it
    if (entry.state == .green and entry.verified_at.eq(db.current_revision)) {
        return entry;
    }

    // Try to verify it's still valid
    const is_green = tryMarkGreen(db, key) catch return null;
    if (is_green) {
        return entry;
    }

    return null;
}

/// Begin executing a query - pushes dependency frame
pub fn beginQuery(db: *TransAmDatabase, key: QueryKey) !void {
    // Check for cycles
    if (db.dependency_stack.isInProgress(key)) {
        return RedGreenError.CycleDetected;
    }
    try db.dependency_stack.push(key);
}

/// Record that current query depends on another query
pub fn recordDependency(db: *TransAmDatabase, dep_key: QueryKey, dep_durability: Durability) !void {
    try db.dependency_stack.recordDependency(dep_key, dep_durability);
}

/// End executing a query - pops dependency frame and returns collected deps
pub fn endQuery(db: *TransAmDatabase) ?DependencyFrame {
    return db.dependency_stack.pop();
}

/// Check if output hash changed (for content-addressed optimization)
/// Returns true if we can reuse the old cached value
pub fn canReuseOutput(db: *TransAmDatabase, key: QueryKey, new_value_hash: u64) bool {
    if (db.query_cache.getPtr(key.hash())) |entry| {
        if (entry.value_hash == new_value_hash) {
            // Output unchanged! Can reuse
            entry.verified_at = db.current_revision;
            entry.state = .green;
            return true;
        }
    }
    return false;
}
