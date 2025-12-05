///! Failing Allocator for Error Path Testing
///!
///! A test allocator that fails after N successful allocations.
///! Used to verify errdefer cleanup patterns work correctly.
///!
///! Usage:
///!   var failing = FailingAllocator.init(std.testing.allocator, 3);
///!   const allocator = failing.allocator();
///!
///!   // First 3 allocations succeed, 4th fails
///!   const a = try allocator.alloc(u8, 10); // OK
///!   const b = try allocator.alloc(u8, 10); // OK
///!   const c = try allocator.alloc(u8, 10); // OK
///!   const d = allocator.alloc(u8, 10); // Returns error.OutOfMemory
///!
///! For leak detection, use std.testing.allocator as the backing allocator.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;

pub const FailingAllocator = struct {
    backing_allocator: Allocator,

    /// Number of allocations before failure (0 = fail immediately)
    fail_after: usize,

    /// Current allocation count
    allocation_count: usize = 0,

    /// Track successful allocations for leak detection
    successful_allocs: usize = 0,

    /// Track frees
    free_count: usize = 0,

    pub fn init(backing: Allocator, fail_after: usize) FailingAllocator {
        return .{
            .backing_allocator = backing,
            .fail_after = fail_after,
        };
    }

    pub fn allocator(self: *FailingAllocator) Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .remap = remap,
                .free = free,
            },
        };
    }

    fn alloc(ctx: *anyopaque, len: usize, ptr_align: Alignment, ret_addr: usize) ?[*]u8 {
        const self: *FailingAllocator = @ptrCast(@alignCast(ctx));

        if (self.allocation_count >= self.fail_after) {
            // Simulate OOM
            return null;
        }

        self.allocation_count += 1;

        const result = self.backing_allocator.rawAlloc(len, ptr_align, ret_addr);
        if (result != null) {
            self.successful_allocs += 1;
        }
        return result;
    }

    fn resize(ctx: *anyopaque, buf: []u8, buf_align: Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *FailingAllocator = @ptrCast(@alignCast(ctx));

        // Resize that grows counts as allocation attempt
        if (new_len > buf.len) {
            if (self.allocation_count >= self.fail_after) {
                return false;
            }
            self.allocation_count += 1;
        }

        return self.backing_allocator.rawResize(buf, buf_align, new_len, ret_addr);
    }

    fn remap(ctx: *anyopaque, buf: []u8, buf_align: Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        const self: *FailingAllocator = @ptrCast(@alignCast(ctx));

        // Remap that grows counts as allocation attempt
        if (new_len > buf.len) {
            if (self.allocation_count >= self.fail_after) {
                return null;
            }
            self.allocation_count += 1;
        }

        return self.backing_allocator.rawRemap(buf, buf_align, new_len, ret_addr);
    }

    fn free(ctx: *anyopaque, buf: []u8, buf_align: Alignment, ret_addr: usize) void {
        const self: *FailingAllocator = @ptrCast(@alignCast(ctx));
        self.free_count += 1;
        self.backing_allocator.rawFree(buf, buf_align, ret_addr);
    }

    /// Reset counters for reuse
    pub fn reset(self: *FailingAllocator) void {
        self.allocation_count = 0;
        self.successful_allocs = 0;
        self.free_count = 0;
    }

    /// Check if all successful allocations were freed
    pub fn detectLeaks(self: *const FailingAllocator) bool {
        return self.successful_allocs != self.free_count;
    }

    /// Get leak count (positive = leaked, negative = double-free)
    pub fn leakCount(self: *const FailingAllocator) i64 {
        return @as(i64, @intCast(self.successful_allocs)) - @as(i64, @intCast(self.free_count));
    }
};

// ============================================================================
// Tests
// ============================================================================

test "FailingAllocator fails after N allocations" {
    var failing = FailingAllocator.init(std.testing.allocator, 2);
    const alloc = failing.allocator();

    // First 2 succeed
    const a = try alloc.alloc(u8, 10);
    defer alloc.free(a);

    const b = try alloc.alloc(u8, 10);
    defer alloc.free(b);

    // Third fails
    const c = alloc.alloc(u8, 10);
    try std.testing.expect(c == error.OutOfMemory);
}

test "FailingAllocator tracks allocations and frees" {
    var failing = FailingAllocator.init(std.testing.allocator, 10);
    const alloc = failing.allocator();

    const a = try alloc.alloc(u8, 10);
    const b = try alloc.alloc(u8, 20);

    try std.testing.expectEqual(@as(usize, 2), failing.successful_allocs);
    try std.testing.expectEqual(@as(usize, 0), failing.free_count);
    try std.testing.expect(failing.detectLeaks());

    alloc.free(a);
    alloc.free(b);

    try std.testing.expectEqual(@as(usize, 2), failing.free_count);
    try std.testing.expect(!failing.detectLeaks());
}

test "FailingAllocator fail_after 0 fails immediately" {
    var failing = FailingAllocator.init(std.testing.allocator, 0);
    const alloc = failing.allocator();

    const result = alloc.alloc(u8, 10);
    try std.testing.expect(result == error.OutOfMemory);
}

test "FailingAllocator reset works" {
    var failing = FailingAllocator.init(std.testing.allocator, 1);
    const alloc = failing.allocator();

    // First allocation succeeds
    const a = try alloc.alloc(u8, 10);
    alloc.free(a);

    // Second fails
    try std.testing.expect(alloc.alloc(u8, 10) == error.OutOfMemory);

    // Reset and try again
    failing.reset();
    const b = try alloc.alloc(u8, 10);
    alloc.free(b);

    try std.testing.expect(!failing.detectLeaks());
}
