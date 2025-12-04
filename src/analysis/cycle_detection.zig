///! Cycle Detection Algorithm (Backend-Agnostic)
///!
///! Implements the Bacon-Rajan cycle detection algorithm for reference-counted
///! memory management. This module provides the ALGORITHM only - actual runtime
///! implementation is backend-specific.
///!
///! Used by:
///! - C backend (with ORC runtime)
///! - Zig backend (with custom RC)
///! - Swift backend (ARC handles cycles via weak refs, but we can optimize)
///! - Objective-C backend (similar to Swift)
///!
///! NOT used by:
///! - JavaScript (GC handles cycles)
///! - Erlang (immutable data, no cycles)
///! - Java (GC handles cycles)

const std = @import("std");

/// Color used in Bacon-Rajan trial deletion algorithm
pub const Color = enum {
    /// In use or free (default state)
    black,

    /// Possible root of garbage cycle
    purple,

    /// Candidate for collection (acyclic)
    gray,

    /// Candidate member of garbage cycle
    white,

    /// Confirmed garbage
    orange,
};

/// Buffered reference for deferred cycle collection
pub const BufferedRef = struct {
    /// Pointer to the object (opaque for backend flexibility)
    ptr: usize,

    /// Object's current color
    color: Color,

    /// Is this a root (RC was decremented to non-zero)?
    is_root: bool,
};

/// Cycle detection state machine
/// Backends implement this trait to integrate with their RC runtime
pub const CycleDetector = struct {
    allocator: std.mem.Allocator,

    /// Candidates for cycle detection (purple roots)
    roots: std.ArrayList(BufferedRef),

    /// Statistics for tuning
    stats: Stats,

    pub const Stats = struct {
        /// Number of times cycle detection was run
        runs: u64 = 0,

        /// Total objects scanned
        objects_scanned: u64 = 0,

        /// Cycles found and collected
        cycles_collected: u64 = 0,

        /// Objects freed due to cycles
        objects_freed: u64 = 0,
    };

    pub fn init(allocator: std.mem.Allocator) CycleDetector {
        return .{
            .allocator = allocator,
            .roots = std.ArrayList(BufferedRef).init(allocator),
            .stats = .{},
        };
    }

    pub fn deinit(self: *CycleDetector) void {
        self.roots.deinit();
    }

    /// Add a potential cycle root (called when RC decrements to non-zero)
    pub fn addRoot(self: *CycleDetector, ptr: usize) !void {
        try self.roots.append(.{
            .ptr = ptr,
            .color = .purple,
            .is_root = true,
        });
    }

    /// Remove a root (called when object is freed)
    pub fn removeRoot(self: *CycleDetector, ptr: usize) void {
        var i: usize = 0;
        while (i < self.roots.items.len) {
            if (self.roots.items[i].ptr == ptr) {
                _ = self.roots.swapRemove(i);
            } else {
                i += 1;
            }
        }
    }

    /// Get current root count (for triggering collection)
    pub fn rootCount(self: *const CycleDetector) usize {
        return self.roots.items.len;
    }

    /// Should we run cycle collection?
    /// Default heuristic: run when roots exceed threshold
    pub fn shouldCollect(self: *const CycleDetector, threshold: usize) bool {
        return self.roots.items.len >= threshold;
    }

    /// Get statistics
    pub fn getStats(self: *const CycleDetector) Stats {
        return self.stats;
    }

    /// Reset statistics
    pub fn resetStats(self: *CycleDetector) void {
        self.stats = .{};
    }
};

/// Cycle detection callbacks - backends implement this
pub const CycleCallbacks = struct {
    /// Get reference count of an object
    getRefCount: *const fn (ptr: usize) u32,

    /// Set reference count of an object
    setRefCount: *const fn (ptr: usize, count: u32) void,

    /// Get color of an object
    getColor: *const fn (ptr: usize) Color,

    /// Set color of an object
    setColor: *const fn (ptr: usize, color: Color) void,

    /// Iterate over references from an object
    /// Callback receives each referenced pointer
    forEachRef: *const fn (ptr: usize, callback: *const fn (usize) void) void,

    /// Free an object
    free: *const fn (ptr: usize) void,
};

/// Run the Bacon-Rajan cycle collection algorithm
/// This is the core algorithm - backends call this with their callbacks
pub fn collectCycles(
    detector: *CycleDetector,
    callbacks: CycleCallbacks,
) void {
    detector.stats.runs += 1;

    // Phase 1: Mark roots
    markRoots(detector, callbacks);

    // Phase 2: Scan for garbage
    scanRoots(detector, callbacks);

    // Phase 3: Collect garbage
    collectWhite(detector, callbacks);
}

/// Phase 1: Mark all purple roots as gray and decrement their children
fn markRoots(detector: *CycleDetector, callbacks: CycleCallbacks) void {
    for (detector.roots.items) |*root| {
        if (callbacks.getColor(root.ptr) == .purple) {
            markGray(root.ptr, callbacks);
        } else {
            // Not purple anymore, remove from roots
            root.is_root = false;
        }
        detector.stats.objects_scanned += 1;
    }
}

/// Mark an object gray and decrement RC of its children
fn markGray(ptr: usize, callbacks: CycleCallbacks) void {
    if (callbacks.getColor(ptr) != .gray) {
        callbacks.setColor(ptr, .gray);

        // Decrement RC of children (trial deletion)
        callbacks.forEachRef(ptr, struct {
            fn callback(child: usize) void {
                const child_rc = callbacks.getRefCount(child);
                if (child_rc > 0) {
                    callbacks.setRefCount(child, child_rc - 1);
                }
                markGray(child, callbacks);
            }
        }.callback);
    }
}

/// Phase 2: Scan roots - mark live objects black, garbage white
fn scanRoots(detector: *CycleDetector, callbacks: CycleCallbacks) void {
    for (detector.roots.items) |*root| {
        if (root.is_root) {
            scan(root.ptr, callbacks);
        }
    }
}

/// Scan an object - if RC > 0, it's live (mark black), else garbage (mark white)
fn scan(ptr: usize, callbacks: CycleCallbacks) void {
    if (callbacks.getColor(ptr) == .gray) {
        if (callbacks.getRefCount(ptr) > 0) {
            // Live - restore black and re-increment children
            scanBlack(ptr, callbacks);
        } else {
            // Garbage - mark white
            callbacks.setColor(ptr, .white);
            callbacks.forEachRef(ptr, struct {
                fn callback(child: usize) void {
                    scan(child, callbacks);
                }
            }.callback);
        }
    }
}

/// Mark an object and its children black (live)
fn scanBlack(ptr: usize, callbacks: CycleCallbacks) void {
    callbacks.setColor(ptr, .black);
    callbacks.forEachRef(ptr, struct {
        fn callback(child: usize) void {
            const child_rc = callbacks.getRefCount(child);
            callbacks.setRefCount(child, child_rc + 1);
            if (callbacks.getColor(child) != .black) {
                scanBlack(child, callbacks);
            }
        }
    }.callback);
}

/// Phase 3: Collect white (garbage) objects
fn collectWhite(detector: *CycleDetector, callbacks: CycleCallbacks) void {
    var i: usize = 0;
    while (i < detector.roots.items.len) {
        const root = detector.roots.items[i];
        if (callbacks.getColor(root.ptr) == .white) {
            // Garbage - free it
            callbacks.setColor(root.ptr, .black); // Prevent double-free
            collectWhiteChildren(root.ptr, callbacks, &detector.stats);
            callbacks.free(root.ptr);
            detector.stats.objects_freed += 1;
            detector.stats.cycles_collected += 1;
            _ = detector.roots.swapRemove(i);
        } else {
            // Restore to black
            callbacks.setColor(root.ptr, .black);
            i += 1;
        }
    }
}

fn collectWhiteChildren(ptr: usize, callbacks: CycleCallbacks, stats: *CycleDetector.Stats) void {
    callbacks.forEachRef(ptr, struct {
        fn callback(child: usize) void {
            if (callbacks.getColor(child) == .white) {
                callbacks.setColor(child, .black);
                collectWhiteChildren(child, callbacks, stats);
                callbacks.free(child);
                stats.objects_freed += 1;
            }
        }
    }.callback);
}

// ============================================================================
// Tests
// ============================================================================

test "CycleDetector basic" {
    var detector = CycleDetector.init(std.testing.allocator);
    defer detector.deinit();

    try detector.addRoot(0x1000);
    try detector.addRoot(0x2000);

    try std.testing.expectEqual(@as(usize, 2), detector.rootCount());
    try std.testing.expect(detector.shouldCollect(2));
    try std.testing.expect(!detector.shouldCollect(3));
}

test "CycleDetector stats" {
    var detector = CycleDetector.init(std.testing.allocator);
    defer detector.deinit();

    try std.testing.expectEqual(@as(u64, 0), detector.getStats().runs);
    detector.stats.runs = 5;
    try std.testing.expectEqual(@as(u64, 5), detector.getStats().runs);
    detector.resetStats();
    try std.testing.expectEqual(@as(u64, 0), detector.getStats().runs);
}
