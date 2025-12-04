const std = @import("std");
const testing = std.testing;

// DRC (Dynamic Reference Counting) Runtime
//
// Based on Nim ORC + Lobster ownership model.
// Uses caller-provides-type pattern for 8-byte headers.
//
// Key design decisions:
// - 8-byte RefHeader (memory efficient)
// - TypeInfo is passed by caller, not stored in object
// - Compiler decides at each site whether cycle check is needed
// - Full metadata available when needed via static TypeInfo

/// Color for Bacon-Rajan cycle detection
pub const Color = enum(u2) {
    /// In use or free (default state)
    black = 0,
    /// Possible root of garbage cycle
    purple = 1,
    /// Candidate for collection (acyclic)
    gray = 2,
    /// Candidate member of garbage cycle
    white = 3,
};

/// Flags packed into 1 byte
pub const Flags = packed struct {
    /// Color for Bacon-Rajan algorithm
    color: Color = .black,

    /// Already in root list (prevents double-add)
    buffered: bool = false,

    /// Visited during current traversal (deduplication)
    visited: bool = false,

    /// Reserved for future use
    _pad: u4 = 0,
};

/// ORC Reference Header (8 bytes)
///
/// Layout: [rc: u32 | flags: u8 | type_id: u24]
///
/// type_id enables looking up TypeInfo during cycle tracing.
/// This allows tracing children that weren't directly decremented.
/// 24-bit type_id supports up to 16 million types.
pub const RefHeader = packed struct {
    /// Reference count
    rc: u32,

    /// Flags (color, buffered, visited)
    flags: Flags,

    /// Type registry index (24-bit) - enables TypeInfo lookup during tracing
    type_id: u24 = 0,

    pub const MAX_RC: u32 = 0xFFFFFFFF;

    pub fn init() RefHeader {
        return .{
            .rc = 1, // Start with rc=1 (owned by creator)
            .flags = .{},
        };
    }

    pub fn isInRoots(self: *const RefHeader) bool {
        return self.flags.buffered;
    }

    pub fn getRc(self: *const RefHeader) u32 {
        return self.rc;
    }

    pub fn getColor(self: *const RefHeader) Color {
        return self.flags.color;
    }

    pub fn setColor(self: *RefHeader, color: Color) void {
        self.flags.color = color;
    }
};

comptime {
    // Verify RefHeader is exactly 8 bytes
    if (@sizeOf(RefHeader) != 8) {
        @compileError("RefHeader must be exactly 8 bytes");
    }
    // Verify Flags is 1 byte
    if (@sizeOf(Flags) != 1) {
        @compileError("Flags must be exactly 1 byte");
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

/// Type metadata for cycle detection (STATIC per class)
///
/// This is NOT stored in each object. Instead:
/// - Compiler generates one TypeInfo per class
/// - Caller passes TypeInfo to functions that need it
/// - Type ID stored in header enables lookup during cycle tracing
pub const TypeInfo = struct {
    /// Type name (for debugging)
    name: []const u8,

    /// Size of the type (for allocation)
    size: usize,

    /// Can this type form cycles? (compile-time determined)
    /// If false, cycle detection is skipped entirely
    is_cyclic: bool,

    /// Trace function: calls callback for each ref field in the object
    /// If null, type has no ref fields (leaf node)
    trace_fn: ?*const fn (obj: *anyopaque, callback: *const fn (*anyopaque) void) void,

    /// Destructor function (optional): called before freeing
    destroy_fn: ?*const fn (obj: *anyopaque) void,
};

/// Type Registry - maps type_id to TypeInfo
/// Type ID 0 is reserved for "unknown type"
pub const TypeRegistry = struct {
    const MAX_TYPES: usize = 4096;

    types: [MAX_TYPES]?*const TypeInfo = [_]?*const TypeInfo{null} ** MAX_TYPES,
    count: usize = 1, // Start at 1, 0 is reserved

    /// Register a type and get its ID (idempotent)
    pub fn register(self: *TypeRegistry, type_info: *const TypeInfo) u24 {
        // Check if already registered
        for (self.types[1..self.count], 1..) |ti, i| {
            if (ti == type_info) {
                return @intCast(i);
            }
        }

        // Register new type
        if (self.count >= MAX_TYPES) {
            return 0; // Registry full
        }

        const id: u24 = @intCast(self.count);
        self.types[self.count] = type_info;
        self.count += 1;
        return id;
    }

    /// Look up TypeInfo by type_id
    pub fn get(self: *const TypeRegistry, type_id: u24) ?*const TypeInfo {
        if (type_id == 0 or type_id >= self.count) return null;
        return self.types[type_id];
    }
};

/// ORC Runtime Statistics
pub const Stats = struct {
    allocations: u64 = 0,
    deallocations: u64 = 0,
    increfs: u64 = 0,
    decrefs: u64 = 0,
    cycle_collections: u64 = 0,
    objects_scanned: u64 = 0,
    cycles_freed: u64 = 0,
    acyclic_fast_path: u64 = 0, // Decrefs that skipped cycle check
};

/// ORC Runtime - manages reference counting and cycle collection
pub const ORCRuntime = struct {
    allocator: std.mem.Allocator,

    /// Cycle collector roots (pointers to headers only)
    /// TypeInfo is looked up via type_id in header
    roots: std.ArrayList(*RefHeader),

    /// Type registry - maps type_id to TypeInfo
    type_registry: TypeRegistry,

    /// Threshold for triggering cycle collection
    cycle_threshold: usize,

    /// Statistics
    stats: Stats,

    /// Enable cycle detection
    enable_cycles: bool,

    pub fn init(allocator: std.mem.Allocator) ORCRuntime {
        return initWithConfig(allocator, .{});
    }

    pub const Config = struct {
        cycle_threshold: usize = 100,
        enable_cycles: bool = true,
    };

    pub fn initWithConfig(allocator: std.mem.Allocator, config: Config) ORCRuntime {
        return .{
            .allocator = allocator,
            .roots = std.ArrayList(*RefHeader).init(allocator),
            .type_registry = .{},
            .cycle_threshold = config.cycle_threshold,
            .stats = .{},
            .enable_cycles = config.enable_cycles,
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
        self.stats.allocations += 1;

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
        const header = getHeader(ptr);
        std.debug.assert(header.rc < RefHeader.MAX_RC);
        header.rc += 1;
        header.flags.color = .black; // No longer a candidate
        self.stats.increfs += 1;
    }

    /// Decrement reference count (simple, no cycle check)
    /// Use for acyclic types or when caller knows no cycle possible
    pub fn decref(self: *ORCRuntime, ptr: anytype) void {
        const header = getHeader(ptr);

        std.debug.assert(header.rc > 0);
        header.rc -= 1;
        self.stats.decrefs += 1;
        self.stats.acyclic_fast_path += 1;

        if (header.rc == 0) {
            self.freeObject(ptr, header, null);
        } else {
            header.flags.color = .black;
        }
    }

    /// Decrement with cycle check (caller provides type info)
    /// Use for cyclic types
    ///
    /// IMPORTANT: Also sets the type_id in header so that cycle tracing
    /// can find TypeInfo for this object and its children.
    pub fn decrefTyped(self: *ORCRuntime, ptr: anytype, type_info: *const TypeInfo) !void {
        const header = getHeader(ptr);

        std.debug.assert(header.rc > 0);
        header.rc -= 1;
        self.stats.decrefs += 1;

        // Register type and store type_id in header for cycle tracing
        const type_id = self.type_registry.register(type_info);
        header.type_id = type_id;

        if (header.rc == 0) {
            self.freeObject(ptr, header, type_info);
        } else if (self.enable_cycles and type_info.is_cyclic) {
            // Non-zero RC on cyclic type = potential cycle root
            if (!header.flags.buffered) {
                header.flags.color = .purple;
                header.flags.buffered = true;
                try self.addToRoots(header);

                // Check if we should run cycle collection
                if (self.roots.items.len >= self.cycle_threshold) {
                    self.collectCycles();
                }
            }
        } else {
            // Acyclic type, just update color
            self.stats.acyclic_fast_path += 1;
            header.flags.color = .black;
        }
    }

    fn freeObject(self: *ORCRuntime, ptr: anytype, header: *RefHeader, type_info: ?*const TypeInfo) void {
        // Look up type from registry if not provided
        const ti = type_info orelse self.type_registry.get(header.type_id);

        // Call destructor if present
        if (ti) |t| {
            if (t.destroy_fn) |destroy| {
                destroy(@ptrCast(ptr));
            }
        }

        // Remove from cycle collector roots if present
        if (header.flags.buffered) {
            self.removeFromRoots(header);
        }

        // Free memory
        const T = @TypeOf(ptr.*);
        const AllocType = Allocation(T);
        const alloc_ptr: *AllocType = @ptrFromInt(@intFromPtr(header));
        self.allocator.destroy(alloc_ptr);
        self.stats.deallocations += 1;
    }

    /// Get current reference count (for testing/debugging)
    pub fn getRC(ptr: anytype) u32 {
        const header = getHeader(ptr);
        return header.getRc();
    }

    /// Get color (for cycle detection)
    pub fn getColor(ptr: anytype) Color {
        const header = getHeader(ptr);
        return header.getColor();
    }

    /// Get statistics
    pub fn getStats(self: *const ORCRuntime) Stats {
        return self.stats;
    }

    /// Get number of potential cycle roots
    pub fn getRootCount(self: *const ORCRuntime) usize {
        return self.roots.items.len;
    }

    /// Add to root list (type info stored in header's type_id)
    fn addToRoots(self: *ORCRuntime, header: *RefHeader) !void {
        try self.roots.append(header);
    }

    /// Remove from root list
    fn removeFromRoots(self: *ORCRuntime, header: *RefHeader) void {
        // Find the header in roots
        for (self.roots.items, 0..) |h, i| {
            if (h == header) {
                _ = self.roots.swapRemove(i);
                break;
            }
        }
        header.flags.buffered = false;
    }

    // Thread-local runtime pointer for use by callbacks during cycle collection
    // This is necessary because Zig doesn't support closures with captured state
    threadlocal var current_runtime: ?*ORCRuntime = null;

    /// Bacon-Rajan cycle collector with type-aware traversal
    pub fn collectCycles(self: *ORCRuntime) void {
        if (self.roots.items.len == 0) return;

        self.stats.cycle_collections += 1;

        // Set thread-local runtime for callbacks
        current_runtime = self;
        defer current_runtime = null;

        // Phase 1: Mark gray (trial deletion)
        for (self.roots.items) |header| {
            if (header.getColor() == .purple) {
                const ti = self.type_registry.get(header.type_id);
                const obj: *anyopaque = @ptrFromInt(@intFromPtr(header) + @sizeOf(RefHeader));
                markGrayImpl(header, obj, ti);
            } else {
                header.flags.buffered = false;
            }
        }

        // Phase 2: Scan - mark live black, garbage white
        for (self.roots.items) |header| {
            const ti = self.type_registry.get(header.type_id);
            const obj: *anyopaque = @ptrFromInt(@intFromPtr(header) + @sizeOf(RefHeader));
            scanImpl(header, obj, ti);
        }

        // Phase 3: Collect white (garbage) objects recursively
        // Always operate on first element since swapRemove brings new element to position 0
        while (self.roots.items.len > 0) {
            const header = self.roots.items[0];

            if (header.getColor() == .white) {
                // Recursively free this WHITE object and its WHITE children
                collectWhiteImpl(header);
            } else {
                // Live object - reset state
                header.setColor(.black);
                header.flags.buffered = false;
            }

            // Remove from roots (always from position 0)
            _ = self.roots.swapRemove(0);
        }
    }

    /// Mark object gray and decrement children's RC (trial deletion)
    fn markGrayImpl(header: *RefHeader, obj: *anyopaque, ti: ?*const TypeInfo) void {
        if (header.getColor() != .gray) {
            header.setColor(.gray);
            if (current_runtime) |rt| {
                rt.stats.objects_scanned += 1;
            }

            if (ti) |t| {
                if (t.trace_fn) |trace| {
                    trace(obj, markGrayCallback);
                }
            }
        }
    }

    fn markGrayCallback(child: *anyopaque) void {
        const child_header: *RefHeader = @ptrFromInt(@intFromPtr(child) - @sizeOf(RefHeader));
        if (child_header.rc > 0) {
            child_header.rc -= 1;
        }
        // Recursively mark gray
        if (child_header.getColor() != .gray) {
            if (current_runtime) |rt| {
                const child_ti = rt.type_registry.get(child_header.type_id);
                markGrayImpl(child_header, child, child_ti);
            }
        }
    }

    /// Scan object - if RC > 0 mark black (live), else mark white (garbage)
    fn scanImpl(header: *RefHeader, obj: *anyopaque, ti: ?*const TypeInfo) void {
        if (header.getColor() == .gray) {
            if (header.rc > 0) {
                scanBlackImpl(header, obj, ti);
            } else {
                header.setColor(.white);
                if (ti) |t| {
                    if (t.trace_fn) |trace| {
                        trace(obj, scanCallback);
                    }
                }
            }
        }
    }

    fn scanCallback(child: *anyopaque) void {
        const child_header: *RefHeader = @ptrFromInt(@intFromPtr(child) - @sizeOf(RefHeader));
        if (child_header.getColor() == .gray) {
            if (child_header.rc > 0) {
                if (current_runtime) |rt| {
                    const child_ti = rt.type_registry.get(child_header.type_id);
                    scanBlackImpl(child_header, child, child_ti);
                }
            } else {
                child_header.setColor(.white);
                if (current_runtime) |rt| {
                    const child_ti = rt.type_registry.get(child_header.type_id);
                    if (child_ti) |t| {
                        if (t.trace_fn) |trace| {
                            trace(child, scanCallback);
                        }
                    }
                }
            }
        }
    }

    /// Mark object black and restore children's RC
    fn scanBlackImpl(header: *RefHeader, obj: *anyopaque, ti: ?*const TypeInfo) void {
        header.setColor(.black);
        if (ti) |t| {
            if (t.trace_fn) |trace| {
                trace(obj, scanBlackCallback);
            }
        }
    }

    fn scanBlackCallback(child: *anyopaque) void {
        const child_header: *RefHeader = @ptrFromInt(@intFromPtr(child) - @sizeOf(RefHeader));
        child_header.rc += 1;
        if (child_header.getColor() != .black) {
            child_header.setColor(.black);
            if (current_runtime) |rt| {
                const child_ti = rt.type_registry.get(child_header.type_id);
                if (child_ti) |t| {
                    if (t.trace_fn) |trace| {
                        trace(child, scanBlackCallback);
                    }
                }
            }
        }
    }

    /// Collect (free) a WHITE object and its WHITE children recursively
    fn collectWhiteImpl(header: *RefHeader) void {
        // Only collect WHITE objects
        if (header.getColor() != .white) return;

        // Mark as BLACK to prevent double-free
        header.setColor(.black);

        const rt = current_runtime orelse return;
        const ti = rt.type_registry.get(header.type_id);
        const obj: *anyopaque = @ptrFromInt(@intFromPtr(header) + @sizeOf(RefHeader));

        // First, recursively collect WHITE children
        if (ti) |t| {
            if (t.trace_fn) |trace| {
                trace(obj, collectWhiteCallback);
            }
        }

        // Now free this object
        header.flags.buffered = false;
        if (ti) |t| {
            if (t.destroy_fn) |destroy| {
                destroy(obj);
            }
        }

        // Free the allocation
        rt.allocator.destroy(@as(*RefHeader, @ptrCast(header)));
        rt.stats.deallocations += 1;
        rt.stats.cycles_freed += 1;
    }

    fn collectWhiteCallback(child: *anyopaque) void {
        const child_header: *RefHeader = @ptrFromInt(@intFromPtr(child) - @sizeOf(RefHeader));
        collectWhiteImpl(child_header);
    }
};

// =============================================================================
// Tests
// =============================================================================

test "RefHeader: size is exactly 8 bytes" {
    try testing.expectEqual(@as(usize, 8), @sizeOf(RefHeader));
}

test "Flags: size is exactly 1 byte" {
    try testing.expectEqual(@as(usize, 1), @sizeOf(Flags));
}

test "RefHeader: init sets rc=1 and not in roots" {
    const header = RefHeader.init();

    try testing.expectEqual(@as(u32, 1), header.rc);
    try testing.expect(!header.isInRoots());
    try testing.expectEqual(Color.black, header.getColor());
}

test "TypeInfo: static type metadata" {
    const ti = TypeInfo{
        .name = "TestType",
        .size = 16,
        .is_cyclic = true,
        .trace_fn = null,
        .destroy_fn = null,
    };

    try testing.expectEqualStrings("TestType", ti.name);
    try testing.expect(ti.is_cyclic);
    try testing.expect(ti.trace_fn == null);
}

test "ORCRuntime: alloc creates object with rc=1" {
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    const ptr = try runtime.alloc(i32);
    defer runtime.decref(ptr);

    try testing.expectEqual(@as(u32, 1), ORCRuntime.getRC(ptr));

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

    runtime.decref(ptr);
    runtime.decref(ptr);
}

test "ORCRuntime: decref decrements reference count" {
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    const ptr = try runtime.alloc(i32);

    runtime.incref(ptr);
    runtime.incref(ptr);

    try testing.expectEqual(@as(u32, 3), ORCRuntime.getRC(ptr));

    runtime.decref(ptr);
    try testing.expectEqual(@as(u32, 2), ORCRuntime.getRC(ptr));

    runtime.decref(ptr);
    try testing.expectEqual(@as(u32, 1), ORCRuntime.getRC(ptr));

    runtime.decref(ptr);
}

test "ORCRuntime: decref to 0 frees memory" {
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    const ptr = try runtime.alloc(i32);
    ptr.* = 42;

    runtime.decref(ptr);
    // Memory freed, testing.allocator will catch leaks
}

test "ORCRuntime: decrefTyped with cyclic type" {
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    const cyclic_type = TypeInfo{
        .name = "CyclicNode",
        .size = 16,
        .is_cyclic = true,
        .trace_fn = null,
        .destroy_fn = null,
    };

    const ptr = try runtime.alloc(i32);
    runtime.incref(ptr); // rc=2

    try runtime.decrefTyped(ptr, &cyclic_type); // rc=1, added to roots
    try testing.expectEqual(@as(u32, 1), ORCRuntime.getRC(ptr));
    try testing.expect(runtime.getRootCount() > 0);

    runtime.decref(ptr); // rc=0, freed
}

test "ORCRuntime: decrefTyped with acyclic type skips cycle check" {
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    const acyclic_type = TypeInfo{
        .name = "Point",
        .size = 16,
        .is_cyclic = false, // Acyclic!
        .trace_fn = null,
        .destroy_fn = null,
    };

    const ptr = try runtime.alloc(i32);
    runtime.incref(ptr); // rc=2

    const before = runtime.stats.acyclic_fast_path;
    try runtime.decrefTyped(ptr, &acyclic_type); // rc=1, NO cycle check
    try testing.expect(runtime.stats.acyclic_fast_path > before);
    try testing.expectEqual(@as(usize, 0), runtime.getRootCount()); // Not added to roots

    runtime.decref(ptr);
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

test "ORCRuntime: no memory leaks with testing allocator" {
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    const ptr1 = try runtime.alloc(i32);
    runtime.decref(ptr1);

    const ptr2 = try runtime.alloc(i32);
    runtime.incref(ptr2);
    runtime.incref(ptr2);
    runtime.decref(ptr2);
    runtime.decref(ptr2);
    runtime.decref(ptr2);
}

test "ORCRuntime: statistics tracking" {
    const allocator = testing.allocator;
    var runtime = ORCRuntime.init(allocator);
    defer runtime.deinit();

    const ptr = try runtime.alloc(i32);
    try testing.expectEqual(@as(u64, 1), runtime.stats.allocations);

    runtime.incref(ptr);
    try testing.expectEqual(@as(u64, 1), runtime.stats.increfs);

    runtime.decref(ptr);
    try testing.expectEqual(@as(u64, 1), runtime.stats.decrefs);

    runtime.decref(ptr);
    try testing.expectEqual(@as(u64, 1), runtime.stats.deallocations);
}
