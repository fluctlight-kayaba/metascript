const std = @import("std");
const testing = std.testing;

/// ORC Reference Header (8 bytes)
/// Layout: [rc: u32 | rootIdx: u32]
/// - rc: Reference count (0 = freed)
/// - rootIdx: Index in cycle collector roots (-1 = not in roots)
pub const RefHeader = packed struct {
    rc: u32,
    root_idx: u32,

    pub const NOT_IN_ROOTS: u32 = 0xFFFFFFFF;

    pub fn init() RefHeader {
        return RefHeader{
            .rc = 1, // Start with rc=1 (owned by creator)
            .root_idx = NOT_IN_ROOTS,
        };
    }

    pub fn isInRoots(self: RefHeader) bool {
        return self.root_idx != NOT_IN_ROOTS;
    }
};

comptime {
    // Verify RefHeader is exactly 8 bytes
    if (@sizeOf(RefHeader) != 8) {
        @compileError("RefHeader must be exactly 8 bytes");
    }
}

/// Allocation metadata: RefHeader + user data
/// Memory layout: [RefHeader (8 bytes)][User Data]
pub fn Allocation(comptime T: type) type {
    return struct {
        header: RefHeader,
        data: T,
    };
}

/// ORC Runtime - manages reference counting and cycle collection
pub const ORCRuntime = struct {
    allocator: std.mem.Allocator,
    // Cycle collector roots (objects with potential cycles)
    roots: std.ArrayList(*RefHeader),

    pub fn init(allocator: std.mem.Allocator) ORCRuntime {
        return ORCRuntime{
            .allocator = allocator,
            .roots = std.ArrayList(*RefHeader).init(allocator),
        };
    }

    pub fn deinit(self: *ORCRuntime) void {
        self.roots.deinit();
    }

    /// Allocate object with ORC header
    pub fn alloc(self: *ORCRuntime, comptime T: type) !*T {
        const AllocType = Allocation(T);
        const ptr = try self.allocator.create(AllocType);

        // Initialize header
        ptr.header = RefHeader.init();

        // Return pointer to user data (skip header)
        return &ptr.data;
    }

    /// Get RefHeader from user data pointer
    pub fn getHeader(ptr: anytype) *RefHeader {
        const T = @TypeOf(ptr.*);
        const AllocType = Allocation(T);

        // Calculate offset: user data is after header
        const data_ptr = @intFromPtr(ptr);
        const header_offset = @offsetOf(AllocType, "header");
        const data_offset = @offsetOf(AllocType, "data");
        const header_ptr = data_ptr - (data_offset - header_offset);

        return @ptrFromInt(header_ptr);
    }

    /// Increment reference count
    pub fn incref(self: *ORCRuntime, ptr: anytype) void {
        _ = self; // Runtime not needed for basic incref
        const header = getHeader(ptr);
        header.rc += 1;
    }

    /// Decrement reference count, free if rc=0
    /// IMPORTANT: Do NOT call decref on already-freed pointers (rc=0)
    /// This is undefined behavior and will likely segfault
    pub fn decref(self: *ORCRuntime, ptr: anytype) void {
        const header = getHeader(ptr);

        // Assert rc > 0 (catching double-free in debug builds)
        std.debug.assert(header.rc > 0);

        header.rc -= 1;

        if (header.rc == 0) {
            // Remove from cycle collector roots if present
            if (header.isInRoots()) {
                self.removeFromRoots(header);
            }

            // Free memory
            const T = @TypeOf(ptr.*);
            const AllocType = Allocation(T);
            const alloc_ptr: *AllocType = @ptrFromInt(@intFromPtr(header));
            self.allocator.destroy(alloc_ptr);
        }
    }

    /// Get current reference count (for testing/debugging)
    pub fn getRC(ptr: anytype) u32 {
        const header = getHeader(ptr);
        return header.rc;
    }

    /// Add object to cycle collector roots
    fn addToRoots(self: *ORCRuntime, header: *RefHeader) !void {
        if (header.isInRoots()) {
            return; // Already in roots
        }

        const idx = self.roots.items.len;
        try self.roots.append(header);
        header.root_idx = @intCast(idx);
    }

    /// Remove object from cycle collector roots
    fn removeFromRoots(self: *ORCRuntime, header: *RefHeader) void {
        if (!header.isInRoots()) {
            return; // Not in roots
        }

        const idx = header.root_idx;
        const last_idx = self.roots.items.len - 1;

        if (idx < last_idx) {
            // Swap with last element
            const last = self.roots.items[last_idx];
            self.roots.items[idx] = last;
            last.root_idx = idx;
        }

        _ = self.roots.pop();
        header.root_idx = RefHeader.NOT_IN_ROOTS;
    }

    /// Simple mark-and-sweep cycle collector (placeholder for now)
    pub fn collectCycles(self: *ORCRuntime) void {
        // TODO: Implement cycle detection and collection
        // For now, this is a no-op
        _ = self;
    }
};

// =============================================================================
// Tests (TDD: Tests define "done")
// =============================================================================

test "RefHeader: size is exactly 8 bytes" {
    try testing.expectEqual(@as(usize, 8), @sizeOf(RefHeader));
}

test "RefHeader: init sets rc=1 and not in roots" {
    const header = RefHeader.init();

    try testing.expectEqual(@as(u32, 1), header.rc);
    try testing.expectEqual(RefHeader.NOT_IN_ROOTS, header.root_idx);
    try testing.expect(!header.isInRoots());
}

test "ORCRuntime: alloc creates object with rc=1" {
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    // Allocate a simple integer
    const ptr = try runtime.alloc(i32);
    defer runtime.decref(ptr);

    // Verify RC is 1 (owned by creator)
    try testing.expectEqual(@as(u32, 1), ORCRuntime.getRC(ptr));

    // Can write to allocated memory
    ptr.* = 42;
    try testing.expectEqual(@as(i32, 42), ptr.*);
}

test "ORCRuntime: incref increments reference count" {
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    const ptr = try runtime.alloc(i32);
    defer runtime.decref(ptr);

    try testing.expectEqual(@as(u32, 1), ORCRuntime.getRC(ptr));

    runtime.incref(ptr);
    try testing.expectEqual(@as(u32, 2), ORCRuntime.getRC(ptr));

    runtime.incref(ptr);
    try testing.expectEqual(@as(u32, 3), ORCRuntime.getRC(ptr));

    // Decref back to 1 for cleanup
    runtime.decref(ptr);
    runtime.decref(ptr);
}

test "ORCRuntime: decref decrements reference count" {
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    const ptr = try runtime.alloc(i32);

    runtime.incref(ptr); // rc=2
    runtime.incref(ptr); // rc=3

    try testing.expectEqual(@as(u32, 3), ORCRuntime.getRC(ptr));

    runtime.decref(ptr); // rc=2
    try testing.expectEqual(@as(u32, 2), ORCRuntime.getRC(ptr));

    runtime.decref(ptr); // rc=1
    try testing.expectEqual(@as(u32, 1), ORCRuntime.getRC(ptr));

    runtime.decref(ptr); // rc=0, freed
}

test "ORCRuntime: decref to 0 frees memory" {
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    const ptr = try runtime.alloc(i32);
    ptr.* = 42;

    // Decref to 0 should free
    runtime.decref(ptr);

    // NOTE: Can't verify memory is freed directly (would be use-after-free)
    // But Zig's testing.allocator will catch memory leaks
}

test "ORCRuntime: multiple allocations work independently" {
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    const ptr1 = try runtime.alloc(i32);
    const ptr2 = try runtime.alloc(i32);
    const ptr3 = try runtime.alloc(i32);

    ptr1.* = 10;
    ptr2.* = 20;
    ptr3.* = 30;

    try testing.expectEqual(@as(u32, 1), ORCRuntime.getRC(ptr1));
    try testing.expectEqual(@as(u32, 1), ORCRuntime.getRC(ptr2));
    try testing.expectEqual(@as(u32, 1), ORCRuntime.getRC(ptr3));

    runtime.incref(ptr2); // Only increment ptr2

    try testing.expectEqual(@as(u32, 1), ORCRuntime.getRC(ptr1));
    try testing.expectEqual(@as(u32, 2), ORCRuntime.getRC(ptr2));
    try testing.expectEqual(@as(u32, 1), ORCRuntime.getRC(ptr3));

    runtime.decref(ptr1);
    runtime.decref(ptr2);
    runtime.decref(ptr2);
    runtime.decref(ptr3);
}

test "ORCRuntime: struct allocation" {
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    const Point = struct {
        x: i32,
        y: i32,
    };

    const ptr = try runtime.alloc(Point);
    defer runtime.decref(ptr);

    ptr.x = 10;
    ptr.y = 20;

    try testing.expectEqual(@as(i32, 10), ptr.x);
    try testing.expectEqual(@as(i32, 20), ptr.y);
    try testing.expectEqual(@as(u32, 1), ORCRuntime.getRC(ptr));
}

// NOTE: We do NOT test double-free because it's undefined behavior
// The compiler's static analysis will prevent this from happening in generated code
// In debug builds, we assert(rc > 0) to catch this bug

test "ORCRuntime: no memory leaks with testing allocator" {
    // This test verifies that testing.allocator catches any leaks
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    // Allocate and properly free
    const ptr1 = try runtime.alloc(i32);
    runtime.decref(ptr1);

    // Allocate with extra refs, then free all
    const ptr2 = try runtime.alloc(i32);
    runtime.incref(ptr2);
    runtime.incref(ptr2);
    runtime.decref(ptr2);
    runtime.decref(ptr2);
    runtime.decref(ptr2);

    // If there are any leaks, testing.allocator will fail the test
}
