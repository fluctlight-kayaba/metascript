// Trans-Am Cache Implementations
// LRU Cache and Content-Addressed Macro Output Cache

const std = @import("std");
const ast = @import("../ast/ast.zig");

// ===== LRU CACHE =====

/// Generic LRU (Least Recently Used) cache with configurable capacity.
/// Used for caching parse results, macro expansions, etc.
pub fn LRUCache(comptime V: type) type {
    return struct {
        const Self = @This();
        const Node = struct {
            key: u64,
            value: V,
            prev: ?*Node = null,
            next: ?*Node = null,
        };

        allocator: std.mem.Allocator,
        capacity: usize,
        map: std.AutoHashMap(u64, *Node),
        head: ?*Node = null, // Most recently used
        tail: ?*Node = null, // Least recently used
        count: usize = 0,

        pub fn init(allocator: std.mem.Allocator, capacity: usize) !Self {
            return .{
                .allocator = allocator,
                .capacity = capacity,
                .map = std.AutoHashMap(u64, *Node).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            var it = self.map.valueIterator();
            while (it.next()) |node_ptr| {
                self.allocator.destroy(node_ptr.*);
            }
            self.map.deinit();
        }

        pub fn get(self: *Self, key: u64) ?V {
            const node = self.map.get(key) orelse return null;

            // Move to front (most recently used)
            self.moveToFront(node);

            return node.value;
        }

        pub fn put(self: *Self, key: u64, value: V) !void {
            // Update existing
            if (self.map.get(key)) |node| {
                node.value = value;
                self.moveToFront(node);
                return;
            }

            // Evict if at capacity
            if (self.count >= self.capacity) {
                try self.evictLRU();
            }

            // Insert new node
            const node = try self.allocator.create(Node);
            node.* = .{ .key = key, .value = value };
            try self.map.put(key, node);
            self.addToFront(node);
            self.count += 1;
        }

        pub fn remove(self: *Self, key: u64) void {
            if (self.map.fetchRemove(key)) |entry| {
                self.removeNode(entry.value);
                self.allocator.destroy(entry.value);
                self.count -= 1;
            }
        }

        fn evictLRU(self: *Self) !void {
            const node = self.tail orelse return;
            _ = self.map.remove(node.key);
            self.removeNode(node);
            self.allocator.destroy(node);
            self.count -= 1;
        }

        fn moveToFront(self: *Self, node: *Node) void {
            if (self.head == node) return; // Already at front
            self.removeNode(node);
            self.addToFront(node);
        }

        fn addToFront(self: *Self, node: *Node) void {
            node.next = self.head;
            node.prev = null;
            if (self.head) |head| head.prev = node;
            self.head = node;
            if (self.tail == null) self.tail = node;
        }

        fn removeNode(self: *Self, node: *Node) void {
            if (node.prev) |prev| prev.next = node.next;
            if (node.next) |next| next.prev = node.prev;
            if (self.head == node) self.head = node.next;
            if (self.tail == node) self.tail = node.prev;
        }
    };
}

// ===== MACRO OUTPUT CACHE (Content-Addressed) =====

/// MacroOutputCache provides content-addressed caching for macro expansion.
/// Key insight: Same input often produces same output, even after changes.
///
/// Example:
///   @derive(Eq) class User { name: string; }
///
/// If we change whitespace or comments, the macro input hash changes,
/// but the OUTPUT might be identical. Content-addressing detects this
/// and marks dependents GREEN without recomputation.
pub const MacroOutputCache = struct {
    allocator: std.mem.Allocator,

    /// Maps input_hash -> output_hash (content-addressed)
    /// If we've seen this exact input before, we know the output hash
    input_to_output: std.AutoHashMap(u64, u64),

    /// Maps output_hash -> cached AST pointer
    /// Deduplicates identical outputs across different inputs
    output_to_ast: std.AutoHashMap(u64, *ast.Node),

    /// Statistics for performance monitoring
    stats: Stats,

    pub const Stats = struct {
        hits: u64 = 0, // Input hash found in cache
        misses: u64 = 0, // Had to execute macro
        output_reuse: u64 = 0, // Different input, same output (KEY OPTIMIZATION)
    };

    pub fn init(allocator: std.mem.Allocator) MacroOutputCache {
        return .{
            .allocator = allocator,
            .input_to_output = std.AutoHashMap(u64, u64).init(allocator),
            .output_to_ast = std.AutoHashMap(u64, *ast.Node).init(allocator),
            .stats = .{},
        };
    }

    pub fn deinit(self: *MacroOutputCache) void {
        self.input_to_output.deinit();
        self.output_to_ast.deinit();
    }

    /// Check if we have a cached output for this input hash.
    /// Returns the output hash if found, null otherwise.
    pub fn getOutputHash(self: *MacroOutputCache, input_hash: u64) ?u64 {
        if (self.input_to_output.get(input_hash)) |output_hash| {
            self.stats.hits += 1;
            return output_hash;
        }
        self.stats.misses += 1;
        return null;
    }

    /// Get the cached AST for an output hash.
    pub fn getAst(self: *MacroOutputCache, output_hash: u64) ?*ast.Node {
        return self.output_to_ast.get(output_hash);
    }

    /// Store a new macro expansion result.
    /// Returns true if this output_hash was already in the cache (reuse).
    pub fn store(
        self: *MacroOutputCache,
        input_hash: u64,
        output_hash: u64,
        output_ast: *ast.Node,
    ) !bool {
        // Store input -> output mapping
        try self.input_to_output.put(input_hash, output_hash);

        // Check if this output already exists (content-addressed dedup)
        if (self.output_to_ast.contains(output_hash)) {
            self.stats.output_reuse += 1;
            return true; // Output reused
        }

        // Store new output
        try self.output_to_ast.put(output_hash, output_ast);
        return false;
    }

    /// Check if a new output hash matches the previous output for this input.
    /// This is the KEY optimization: if output unchanged, dependents stay GREEN.
    pub fn outputUnchanged(self: *MacroOutputCache, input_hash: u64, new_output_hash: u64) bool {
        if (self.input_to_output.get(input_hash)) |old_output_hash| {
            return old_output_hash == new_output_hash;
        }
        return false;
    }

    /// Get cache statistics for monitoring
    pub fn getStats(self: *MacroOutputCache) Stats {
        return self.stats;
    }

    /// Calculate hit rate (0.0 - 1.0)
    pub fn hitRate(self: *MacroOutputCache) f64 {
        const total = self.stats.hits + self.stats.misses;
        if (total == 0) return 0.0;
        return @as(f64, @floatFromInt(self.stats.hits)) / @as(f64, @floatFromInt(total));
    }
};

/// Shallow type context available during macro expansion.
/// This avoids circular dependencies: macros need type info,
/// but type checking needs macro expansion results.
pub const ShallowTypeContext = struct {
    /// Class/interface name being processed
    type_name: []const u8,
    /// Property names (without resolved types)
    property_names: []const []const u8,
    /// Property type names as strings (not resolved)
    property_type_names: []const []const u8,
    /// Method names
    method_names: []const []const u8,
    /// Whether this is a class or interface
    is_class: bool,
};

// ===== TESTS =====

test "LRUCache: basic get and put" {
    var cache = try LRUCache(u32).init(std.testing.allocator, 3);
    defer cache.deinit();

    try cache.put(1, 100);
    try cache.put(2, 200);

    try std.testing.expectEqual(@as(?u32, 100), cache.get(1));
    try std.testing.expectEqual(@as(?u32, 200), cache.get(2));
    try std.testing.expectEqual(@as(?u32, null), cache.get(3));
}

test "LRUCache: eviction at capacity" {
    var cache = try LRUCache(u32).init(std.testing.allocator, 2);
    defer cache.deinit();

    try cache.put(1, 100);
    try cache.put(2, 200);
    try cache.put(3, 300); // Should evict key 1

    try std.testing.expectEqual(@as(?u32, null), cache.get(1));
    try std.testing.expectEqual(@as(?u32, 200), cache.get(2));
    try std.testing.expectEqual(@as(?u32, 300), cache.get(3));
}

test "LRUCache: access updates recency" {
    var cache = try LRUCache(u32).init(std.testing.allocator, 2);
    defer cache.deinit();

    try cache.put(1, 100);
    try cache.put(2, 200);
    _ = cache.get(1); // Access key 1, making it most recent
    try cache.put(3, 300); // Should evict key 2, not key 1

    try std.testing.expectEqual(@as(?u32, 100), cache.get(1));
    try std.testing.expectEqual(@as(?u32, null), cache.get(2));
    try std.testing.expectEqual(@as(?u32, 300), cache.get(3));
}

test "LRUCache: remove" {
    var cache = try LRUCache(u32).init(std.testing.allocator, 3);
    defer cache.deinit();

    try cache.put(1, 100);
    cache.remove(1);

    try std.testing.expectEqual(@as(?u32, null), cache.get(1));
}

test "MacroOutputCache: basic operations" {
    var cache = MacroOutputCache.init(std.testing.allocator);
    defer cache.deinit();

    // Initially empty
    try std.testing.expectEqual(@as(?u64, null), cache.getOutputHash(12345));
    try std.testing.expectEqual(@as(u64, 0), cache.stats.hits);
    try std.testing.expectEqual(@as(u64, 1), cache.stats.misses);
}

test "MacroOutputCache: store and retrieve" {
    var cache = MacroOutputCache.init(std.testing.allocator);
    defer cache.deinit();

    // Create a dummy AST node for testing
    var dummy_node = ast.Node{
        .kind = .program,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &.{}, .file_id = 0 } },
    };

    // Store a mapping
    const input_hash: u64 = 12345;
    const output_hash: u64 = 67890;
    const was_reused = try cache.store(input_hash, output_hash, &dummy_node);
    try std.testing.expect(!was_reused);

    // Retrieve
    try std.testing.expectEqual(@as(?u64, output_hash), cache.getOutputHash(input_hash));
    try std.testing.expect(cache.getAst(output_hash) != null);
}

test "MacroOutputCache: output unchanged detection" {
    var cache = MacroOutputCache.init(std.testing.allocator);
    defer cache.deinit();

    var dummy_node = ast.Node{
        .kind = .program,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &.{}, .file_id = 0 } },
    };

    const input_hash: u64 = 12345;
    const output_hash: u64 = 67890;
    _ = try cache.store(input_hash, output_hash, &dummy_node);

    // Same output hash should be detected as unchanged
    try std.testing.expect(cache.outputUnchanged(input_hash, output_hash));
    // Different output hash should be detected as changed
    try std.testing.expect(!cache.outputUnchanged(input_hash, 99999));
}

test "MacroOutputCache: output reuse statistics" {
    var cache = MacroOutputCache.init(std.testing.allocator);
    defer cache.deinit();

    var dummy_node = ast.Node{
        .kind = .program,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &.{}, .file_id = 0 } },
    };

    const output_hash: u64 = 67890;

    // First store
    _ = try cache.store(11111, output_hash, &dummy_node);
    try std.testing.expectEqual(@as(u64, 0), cache.stats.output_reuse);

    // Second store with same output hash (different input)
    const was_reused = try cache.store(22222, output_hash, &dummy_node);
    try std.testing.expect(was_reused);
    try std.testing.expectEqual(@as(u64, 1), cache.stats.output_reuse);
}
