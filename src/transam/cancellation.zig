// Cancellation Support Module
// Implements rust-analyzer/Salsa-style cooperative cancellation.
//
// Two mechanisms:
// 1. Legacy boolean flag (cancel_flag) - simple on/off
// 2. Version-based (cancellation_version) - increments on each cancellation request
//
// Queries periodically check if they should abort, enabling responsive
// cancellation when user types during long-running operations.

const std = @import("std");

// Forward reference to TransAmDatabase
const TransAmDatabase = @import("transam.zig").TransAmDatabase;

// ===== LEGACY BOOLEAN CANCELLATION =====

/// Check if query execution should be cancelled (legacy boolean)
pub fn checkCancellation(db: *TransAmDatabase) !void {
    if (db.cancel_flag.load(.seq_cst)) {
        return error.Cancelled;
    }
}

/// Request cancellation of all in-flight queries (legacy boolean)
pub fn requestCancellation(db: *TransAmDatabase) void {
    db.cancel_flag.store(true, .seq_cst);
}

/// Reset cancellation flag (after handling cancellation)
pub fn resetCancellation(db: *TransAmDatabase) void {
    db.cancel_flag.store(false, .seq_cst);
}

// ===== VERSION-BASED CANCELLATION (Salsa pattern) =====
// Used for cooperative cancellation: queries periodically check if
// their starting version matches current version. If not, abort.

/// Get current cancellation version. Call this at query start.
pub fn getCancellationVersion(db: *TransAmDatabase) u64 {
    return db.cancellation_version.load(.seq_cst);
}

/// Check if query should abort. Call this periodically in long-running queries.
/// Returns error.QueryCancelled if version changed since query started.
pub fn unwindIfCancelled(db: *TransAmDatabase, starting_version: u64) !void {
    const current = db.cancellation_version.load(.seq_cst);
    if (current != starting_version) {
        return error.QueryCancelled;
    }
    // Also check legacy flag for backward compatibility
    if (db.cancel_flag.load(.seq_cst)) {
        return error.QueryCancelled;
    }
}

/// Cancel all pending queries by incrementing version.
/// Called by LSP when user types (didChange notification).
pub fn cancelPendingQueries(db: *TransAmDatabase) void {
    _ = db.cancellation_version.fetchAdd(1, .seq_cst);
    // Also set legacy flag for backward compatibility
    db.cancel_flag.store(true, .seq_cst);
}

/// Reset after cancellation handled
pub fn resetAfterCancellation(db: *TransAmDatabase) void {
    db.cancel_flag.store(false, .seq_cst);
    // Note: version is NOT reset - it only increments
}
