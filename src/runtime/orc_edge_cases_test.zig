const std = @import("std");
const orc = @import("orc.zig");
const testing = std.testing;

// =============================================================================
// Stress Tests: High Load Scenarios
// =============================================================================

// Test: Allocate 100K objects (stress test)
test "ORC stress: 100K allocations" {
    const allocator = testing.allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    defer runtime.deinit();

    const COUNT = 100_000;
    var pointers = std.ArrayList(*i32).init(allocator);
    defer pointers.deinit();

    // Allocate 100K objects
    var i: usize = 0;
    while (i < COUNT) : (i += 1) {
        const ptr = try runtime.alloc(i32);
        ptr.* = @intCast(i);
        try pointers.append(ptr);
    }

    // Verify all values correct
    for (pointers.items, 0..) |ptr, idx| {
        try testing.expectEqual(@as(i32, @intCast(idx)), ptr.*);
        try testing.expectEqual(@as(u32, 1), orc.ORCRuntime.getRC(ptr));
    }

    // Free all
    for (pointers.items) |ptr| {
        runtime.decref(ptr);
    }
}

// Test: Allocate 1M tiny objects (memory pressure)
test "ORC stress: 1M tiny allocations" {
    const allocator = testing.allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    defer runtime.deinit();

    const COUNT = 1_000_000;

    // Allocate and immediately free (no leaks)
    var i: usize = 0;
    while (i < COUNT) : (i += 1) {
        const ptr = try runtime.alloc(u8);
        ptr.* = @intCast(i % 256);
        runtime.decref(ptr);
    }
}

// Test: Large object allocation (1MB)
test "ORC stress: large object (1MB)" {
    const allocator = testing.allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    defer runtime.deinit();

    const LargeArray = [256 * 1024]u32; // 1MB array
    const ptr = try runtime.alloc(LargeArray);
    defer runtime.decref(ptr);

    // Write to array
    ptr[0] = 123;
    ptr[256 * 1024 - 1] = 456;

    try testing.expectEqual(@as(u32, 123), ptr[0]);
    try testing.expectEqual(@as(u32, 456), ptr[256 * 1024 - 1]);
    try testing.expectEqual(@as(u32, 1), orc.ORCRuntime.getRC(ptr));
}

// Test: Alternating alloc/free (fragmentation test)
test "ORC stress: fragmentation (alloc/free pattern)" {
    const allocator = testing.allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    defer runtime.deinit();

    const COUNT = 10_000;
    var kept_pointers = std.ArrayList(*i32).init(allocator);
    defer kept_pointers.deinit();

    // Allocate 10K, keep every 10th
    var i: usize = 0;
    while (i < COUNT) : (i += 1) {
        const ptr = try runtime.alloc(i32);
        ptr.* = @intCast(i);

        if (i % 10 == 0) {
            try kept_pointers.append(ptr);
        } else {
            runtime.decref(ptr); // Free immediately
        }
    }

    // Verify kept pointers
    for (kept_pointers.items, 0..) |ptr, idx| {
        try testing.expectEqual(@as(i32, @intCast(idx * 10)), ptr.*);
    }

    // Free kept pointers
    for (kept_pointers.items) |ptr| {
        runtime.decref(ptr);
    }
}

// Test: Deep reference chain (RC overflow check)
test "ORC stress: deep reference chain (1000 refs)" {
    const allocator = testing.allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    defer runtime.deinit();

    const ptr = try runtime.alloc(i32);
    ptr.* = 42;

    // Add 1000 references
    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        runtime.incref(ptr);
    }

    try testing.expectEqual(@as(u32, 1001), orc.ORCRuntime.getRC(ptr));

    // Remove 1000 references
    i = 0;
    while (i < 1000) : (i += 1) {
        runtime.decref(ptr);
    }

    try testing.expectEqual(@as(u32, 1), orc.ORCRuntime.getRC(ptr));
    runtime.decref(ptr); // Final free
}

// =============================================================================
// Boundary Conditions
// =============================================================================

// Test: RC boundary - max u32 value
test "ORC boundary: RC near u32 max" {
    const allocator = testing.allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    defer runtime.deinit();

    const ptr = try runtime.alloc(i32);
    defer runtime.decref(ptr);

    // Set RC to near max (simulate many refs)
    // Note: We can't actually create 4 billion refs, so we test the data structure
    const header = orc.ORCRuntime.getHeader(ptr);
    header.rc = std.math.maxInt(u32) - 10;

    try testing.expectEqual(std.math.maxInt(u32) - 10, header.rc);

    // Increment should work (but overflow check in production)
    runtime.incref(ptr);
    try testing.expectEqual(std.math.maxInt(u32) - 9, header.rc);

    // Reset for cleanup
    header.rc = 1;
}

// Test: Zero-size type allocation
test "ORC boundary: zero-size type" {
    const allocator = testing.allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    defer runtime.deinit();

    const Empty = struct {};
    const ptr = try runtime.alloc(Empty);
    defer runtime.decref(ptr);

    try testing.expectEqual(@as(u32, 1), orc.ORCRuntime.getRC(ptr));
}

// =============================================================================
// Error Cases and Edge Conditions
// =============================================================================

// Test: Decref on same pointer many times (verify RC tracking)
test "ORC edge: sequential decref tracking" {
    const allocator = testing.allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    defer runtime.deinit();

    const ptr = try runtime.alloc(i32);
    ptr.* = 100;

    // Add 10 refs
    var i: usize = 0;
    while (i < 10) : (i += 1) {
        runtime.incref(ptr);
    }

    try testing.expectEqual(@as(u32, 11), orc.ORCRuntime.getRC(ptr));

    // Remove 10 refs
    i = 0;
    while (i < 10) : (i += 1) {
        runtime.decref(ptr);
        try testing.expectEqual(@as(u32, @intCast(11 - (i + 1))), orc.ORCRuntime.getRC(ptr));
    }

    runtime.decref(ptr); // Final free
}

// Test: Incref/decref interleaved
test "ORC edge: interleaved incref/decref" {
    const allocator = testing.allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    defer runtime.deinit();

    const ptr = try runtime.alloc(i32);
    ptr.* = 42;

    try testing.expectEqual(@as(u32, 1), orc.ORCRuntime.getRC(ptr));

    runtime.incref(ptr); // 2
    runtime.incref(ptr); // 3
    runtime.decref(ptr); // 2
    runtime.incref(ptr); // 3
    runtime.decref(ptr); // 2
    runtime.decref(ptr); // 1

    try testing.expectEqual(@as(u32, 1), orc.ORCRuntime.getRC(ptr));
    runtime.decref(ptr); // 0, freed
}

// Test: Allocate many different sizes (alignment test)
test "ORC edge: mixed sizes and alignments" {
    const allocator = testing.allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    defer runtime.deinit();

    // Different sized types
    const ptr1 = try runtime.alloc(u8); // 1 byte
    const ptr2 = try runtime.alloc(u16); // 2 bytes
    const ptr3 = try runtime.alloc(u32); // 4 bytes
    const ptr4 = try runtime.alloc(u64); // 8 bytes
    const ptr5 = try runtime.alloc([100]u8); // 100 bytes
    const ptr6 = try runtime.alloc([1000]u8); // 1000 bytes

    ptr1.* = 1;
    ptr2.* = 2;
    ptr3.* = 3;
    ptr4.* = 4;
    ptr5[0] = 5;
    ptr6[0] = 6;

    try testing.expectEqual(@as(u8, 1), ptr1.*);
    try testing.expectEqual(@as(u16, 2), ptr2.*);
    try testing.expectEqual(@as(u32, 3), ptr3.*);
    try testing.expectEqual(@as(u64, 4), ptr4.*);
    try testing.expectEqual(@as(u8, 5), ptr5[0]);
    try testing.expectEqual(@as(u8, 6), ptr6[0]);

    runtime.decref(ptr1);
    runtime.decref(ptr2);
    runtime.decref(ptr3);
    runtime.decref(ptr4);
    runtime.decref(ptr5);
    runtime.decref(ptr6);
}

// =============================================================================
// Real-World Patterns
// =============================================================================

// Test: Linked list pattern (many objects, sequential refs)
test "ORC pattern: linked list (1000 nodes)" {
    const allocator = testing.allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    defer runtime.deinit();

    const Node = struct {
        value: i32,
        next: ?*@This(),
    };

    var head: ?*Node = null;
    var nodes = std.ArrayList(*Node).init(allocator);
    defer nodes.deinit();

    // Build list of 1000 nodes
    var i: i32 = 0;
    while (i < 1000) : (i += 1) {
        const node = try runtime.alloc(Node);
        node.value = i;
        node.next = head;
        head = node;
        try nodes.append(node);
    }

    // Verify list structure
    var current = head;
    var count: i32 = 999;
    while (current) |node| : (count -= 1) {
        try testing.expectEqual(count, node.value);
        current = node.next;
    }

    // Clean up (reverse order)
    i = 999;
    while (i >= 0) : (i -= 1) {
        runtime.decref(nodes.items[@intCast(i)]);
    }
}

// Test: Binary tree pattern (balanced tree of 127 nodes)
test "ORC pattern: binary tree (127 nodes)" {
    const allocator = testing.allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    defer runtime.deinit();

    const TreeNode = struct {
        value: i32,
        left: ?*@This(),
        right: ?*@This(),
    };

    var nodes = std.ArrayList(*TreeNode).init(allocator);
    defer nodes.deinit();

    // Build complete binary tree (7 levels, 127 nodes)
    var i: i32 = 0;
    while (i < 127) : (i += 1) {
        const node = try runtime.alloc(TreeNode);
        node.value = i;
        node.left = null;
        node.right = null;
        try nodes.append(node);
    }

    // Connect tree (parent at i, left at 2i+1, right at 2i+2)
    i = 0;
    while (i < 63) : (i += 1) {
        const idx: usize = @intCast(i);
        const left_idx: usize = @intCast(2 * i + 1);
        const right_idx: usize = @intCast(2 * i + 2);

        if (left_idx < 127) nodes.items[idx].left = nodes.items[left_idx];
        if (right_idx < 127) nodes.items[idx].right = nodes.items[right_idx];
    }

    // Verify root
    try testing.expectEqual(@as(i32, 0), nodes.items[0].value);
    try testing.expect(nodes.items[0].left != null);
    try testing.expect(nodes.items[0].right != null);

    // Clean up all nodes
    for (nodes.items) |node| {
        runtime.decref(node);
    }
}

// Test: Graph with shared nodes (DAG pattern)
test "ORC pattern: DAG with shared nodes" {
    const allocator = testing.allocator;
    var runtime = orc.ORCRuntime.init(allocator);
    defer runtime.deinit();

    const GraphNode = struct {
        id: i32,
        neighbors: [3]?*@This(),
    };

    // Create 5 nodes
    const n1 = try runtime.alloc(GraphNode);
    const n2 = try runtime.alloc(GraphNode);
    const n3 = try runtime.alloc(GraphNode);
    const n4 = try runtime.alloc(GraphNode);
    const n5 = try runtime.alloc(GraphNode);

    n1.id = 1;
    n2.id = 2;
    n3.id = 3;
    n4.id = 4;
    n5.id = 5;

    // Build DAG: n1 -> n2, n3; n2 -> n4, n5; n3 -> n4, n5 (n4, n5 shared)
    n1.neighbors = .{ n2, n3, null };
    n2.neighbors = .{ n4, n5, null };
    n3.neighbors = .{ n4, n5, null };
    n4.neighbors = .{ null, null, null };
    n5.neighbors = .{ null, null, null };

    // n4 and n5 are referenced by both n2 and n3 (RC should be higher)
    runtime.incref(n4); // Shared by n2 and n3
    runtime.incref(n5); // Shared by n2 and n3

    try testing.expectEqual(@as(u32, 2), orc.ORCRuntime.getRC(n4));
    try testing.expectEqual(@as(u32, 2), orc.ORCRuntime.getRC(n5));

    // Clean up
    runtime.decref(n1);
    runtime.decref(n2);
    runtime.decref(n3);
    runtime.decref(n4);
    runtime.decref(n4); // Second ref
    runtime.decref(n5);
    runtime.decref(n5); // Second ref
}
