// Trans-Am Cache Implementations
// LRU Cache and Content-Addressed Macro Output Cache

const std = @import("std");
const ast = @import("../ast/ast.zig");

// ===== LRU CACHE =====

/// Eviction callback type - called when entries are evicted from cache
pub fn EvictCallback(comptime V: type) type {
    return *const fn (allocator: std.mem.Allocator, value: V) void;
}

/// Generic LRU (Least Recently Used) cache with configurable capacity.
/// Used for caching parse results, macro expansions, etc.
/// Supports optional eviction callback for cleaning up owned memory.
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
        on_evict: ?EvictCallback(V) = null, // Optional cleanup callback

        pub fn init(allocator: std.mem.Allocator, capacity: usize) !Self {
            return .{
                .allocator = allocator,
                .capacity = capacity,
                .map = std.AutoHashMap(u64, *Node).init(allocator),
            };
        }

        /// Initialize with eviction callback for memory cleanup
        pub fn initWithCallback(allocator: std.mem.Allocator, capacity: usize, on_evict: EvictCallback(V)) !Self {
            return .{
                .allocator = allocator,
                .capacity = capacity,
                .map = std.AutoHashMap(u64, *Node).init(allocator),
                .on_evict = on_evict,
            };
        }

        pub fn deinit(self: *Self) void {
            // Call eviction callback for all remaining entries
            if (self.on_evict) |callback| {
                var it = self.map.valueIterator();
                while (it.next()) |node_ptr| {
                    callback(self.allocator, node_ptr.*.value);
                }
            }
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
                // Call eviction callback to free owned memory
                if (self.on_evict) |callback| {
                    callback(self.allocator, entry.value.value);
                }
                self.removeNode(entry.value);
                self.allocator.destroy(entry.value);
                self.count -= 1;
            }
        }

        fn evictLRU(self: *Self) !void {
            const node = self.tail orelse return;
            // Call eviction callback to free owned memory BEFORE destroying node
            if (self.on_evict) |callback| {
                callback(self.allocator, node.value);
            }
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

// ===== FRESHNESS-AWARE LRU CACHE (Stale-While-Revalidate) =====

/// LRU cache with freshness tracking for stale-while-revalidate pattern.
/// Key insight from rust-analyzer: completions can return stale data for
/// responsiveness, while diagnostics MUST always be fresh.
///
/// Usage:
///   - Completions: getAllowStale() - returns cached even if stale
///   - Diagnostics: getFreshOnly() - returns null if stale
///   - On file change: markStale(affected_keys)
pub fn FreshnessLRUCache(comptime V: type) type {
    return struct {
        const Self = @This();

        /// Cached entry with freshness metadata
        pub const Entry = struct {
            value: V,
            is_fresh: bool,
            cached_at_version: u64,
        };

        inner: LRUCache(Entry),
        current_version: u64 = 0,
        /// Optional callback to free V's owned memory (called on eviction)
        value_cleanup: ?*const fn (allocator: std.mem.Allocator, value: V) void = null,
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator, capacity: usize) !Self {
            return .{
                .inner = try LRUCache(Entry).init(allocator, capacity),
                .allocator = allocator,
            };
        }

        /// Initialize with cleanup callback for Entry type (wraps V)
        /// Callback receives the full Entry, caller should extract .value
        pub fn initWithEntryCleanup(
            allocator: std.mem.Allocator,
            capacity: usize,
            cleanup: EvictCallback(Entry),
        ) !Self {
            return .{
                .inner = try LRUCache(Entry).initWithCallback(allocator, capacity, cleanup),
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.inner.deinit();
        }

        /// Get entry, returning stale if available (for completions/hover)
        pub fn getAllowStale(self: *Self, key: u64) ?V {
            if (self.inner.get(key)) |entry| {
                return entry.value;
            }
            return null;
        }

        /// Get entry only if fresh (for diagnostics - never stale)
        pub fn getFreshOnly(self: *Self, key: u64) ?V {
            if (self.inner.get(key)) |entry| {
                if (entry.is_fresh) {
                    return entry.value;
                }
            }
            return null;
        }

        /// Check if entry exists and is fresh
        pub fn isFresh(self: *Self, key: u64) bool {
            if (self.inner.get(key)) |entry| {
                return entry.is_fresh;
            }
            return false;
        }

        /// Put a fresh entry
        pub fn put(self: *Self, key: u64, value: V) !void {
            try self.inner.put(key, .{
                .value = value,
                .is_fresh = true,
                .cached_at_version = self.current_version,
            });
        }

        /// Mark specific entry as stale (keeps value for stale-while-revalidate)
        pub fn markStale(self: *Self, key: u64) void {
            if (self.inner.map.get(key)) |node| {
                node.value.is_fresh = false;
            }
        }

        /// Increment version and mark all entries as stale
        /// Called on file change to invalidate all cached data
        pub fn invalidateAll(self: *Self) void {
            self.current_version += 1;
            var it = self.inner.map.valueIterator();
            while (it.next()) |node_ptr| {
                node_ptr.*.value.is_fresh = false;
            }
        }

        /// Remove entry completely
        pub fn remove(self: *Self, key: u64) void {
            self.inner.remove(key);
        }

        /// Get current cache size
        pub fn count(self: *Self) usize {
            return self.inner.count;
        }
    };
}

// ===== MACRO OUTPUT CACHE (Content-Addressed + LRU Bounded) =====

/// Default capacity based on rust-analyzer research:
/// 2000 entries ~= 50MB memory for macro cache
pub const DEFAULT_MACRO_CACHE_CAPACITY: usize = 2000;

/// Slow query threshold in milliseconds (from rust-analyzer)
/// Queries exceeding this are flagged for future reference
pub const SLOW_QUERY_THRESHOLD_MS: i64 = 100;

/// Hard timeout for macro expansion in milliseconds
/// Prevents infinite loops from hanging the LSP
pub const MACRO_TIMEOUT_MS: i64 = 5000;

/// MacroOutputCache provides content-addressed caching for macro expansion
/// with LRU eviction to bound memory usage.
///
/// Key insight: Same input often produces same output, even after changes.
///
/// Example:
///   @derive(Eq) class User { name: string; }
///
/// If we change whitespace or comments, the macro input hash changes,
/// but the OUTPUT might be identical. Content-addressing detects this
/// and marks dependents GREEN without recomputation.
///
/// Memory bounded: LRU eviction at 2000 entries (~50MB) based on rust-analyzer defaults.
pub const MacroOutputCache = struct {
    allocator: std.mem.Allocator,

    /// Maps input_hash -> output_hash (content-addressed) with LRU eviction
    /// If we've seen this exact input before, we know the output hash
    input_to_output: LRUCache(u64),

    /// Maps output_hash -> cached AST pointer with LRU eviction
    /// Deduplicates identical outputs across different inputs
    output_to_ast: LRUCache(*ast.Node),

    /// Tracks queries that exceeded SLOW_QUERY_THRESHOLD_MS
    /// These are skipped on subsequent LSP requests (return fallback instead)
    slow_queries: std.AutoHashMap(u64, SlowQueryInfo),

    /// Statistics for performance monitoring
    stats: Stats,

    pub const SlowQueryInfo = struct {
        last_duration_ms: i64,
        hit_count: u32 = 1,
        first_seen_ms: i64,
    };

    pub const Stats = struct {
        hits: u64 = 0, // Input hash found in cache
        misses: u64 = 0, // Had to execute macro
        output_reuse: u64 = 0, // Different input, same output (KEY OPTIMIZATION)
        evictions: u64 = 0, // LRU evictions
        slow_queries_flagged: u64 = 0, // Queries that exceeded threshold
        slow_queries_skipped: u64 = 0, // Requests skipped due to slow flag
    };

    pub fn init(allocator: std.mem.Allocator) !MacroOutputCache {
        return initWithCapacity(allocator, DEFAULT_MACRO_CACHE_CAPACITY);
    }

    pub fn initWithCapacity(allocator: std.mem.Allocator, capacity: usize) !MacroOutputCache {
        return .{
            .allocator = allocator,
            .input_to_output = try LRUCache(u64).init(allocator, capacity),
            .output_to_ast = try LRUCache(*ast.Node).init(allocator, capacity),
            .slow_queries = std.AutoHashMap(u64, SlowQueryInfo).init(allocator),
            .stats = .{},
        };
    }

    pub fn deinit(self: *MacroOutputCache) void {
        self.input_to_output.deinit();
        self.output_to_ast.deinit();
        self.slow_queries.deinit();
    }

    /// Check if this query is known to be slow (>100ms).
    /// If slow, caller should return fallback immediately.
    pub fn isSlowQuery(self: *MacroOutputCache, input_hash: u64) bool {
        if (self.slow_queries.getPtr(input_hash)) |entry| {
            // Update hit count
            entry.hit_count += 1;
            self.stats.slow_queries_skipped += 1;
            return true;
        }
        return false;
    }

    /// Mark a query as slow after measuring its duration.
    /// Called when expansion takes >100ms.
    pub fn markSlow(self: *MacroOutputCache, input_hash: u64, duration_ms: i64) !void {
        try self.slow_queries.put(input_hash, .{
            .last_duration_ms = duration_ms,
            .first_seen_ms = std.time.milliTimestamp(),
        });
        self.stats.slow_queries_flagged += 1;
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
        // Track evictions before put
        const input_count_before = self.input_to_output.count;

        // Store input -> output mapping (LRU bounded)
        try self.input_to_output.put(input_hash, output_hash);

        // Check if eviction occurred
        if (self.input_to_output.count <= input_count_before and input_count_before >= DEFAULT_MACRO_CACHE_CAPACITY) {
            self.stats.evictions += 1;
        }

        // Check if this output already exists (content-addressed dedup)
        if (self.output_to_ast.get(output_hash) != null) {
            self.stats.output_reuse += 1;
            return true; // Output reused
        }

        // Store new output (LRU bounded)
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

    /// Get current entry count
    pub fn entryCount(self: *MacroOutputCache) usize {
        return self.input_to_output.count;
    }

    /// Get slow query count
    pub fn slowQueryCount(self: *MacroOutputCache) usize {
        return self.slow_queries.count();
    }

    /// Clear slow query flags (e.g., after user modifies macro source)
    pub fn clearSlowQueries(self: *MacroOutputCache) void {
        self.slow_queries.clearAndFree();
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

// ===== COMPLETION CACHE (L4) =====

/// Default completion cache capacity
pub const DEFAULT_COMPLETION_CACHE_CAPACITY: usize = 500;

/// Key for completion cache: identifies a specific completion context
pub const CompletionKey = struct {
    /// Hash of file content
    file_hash: u64,
    /// Line number (0-indexed)
    line: u32,
    /// Column number (0-indexed)
    column: u32,
    /// Hash of prefix being completed (e.g., "user." -> hash("user."))
    prefix_hash: u64,

    pub fn hash(self: CompletionKey) u64 {
        var h: u64 = self.file_hash;
        h = h *% 31 +% self.line;
        h = h *% 31 +% self.column;
        h = h *% 31 +% self.prefix_hash;
        return h;
    }
};

/// A single completion item (simplified for caching)
pub const CompletionItem = struct {
    /// Display label
    label: []const u8,
    /// LSP CompletionItemKind (1=Text, 3=Function, 6=Variable, 7=Class, etc.)
    kind: u8,
    /// Optional detail text
    detail: ?[]const u8 = null,
};

/// Completion cache for fast repeated completions at same location.
/// Uses FreshnessLRUCache for stale-while-revalidate support.
/// Memory is properly freed on LRU eviction via cleanup callback.
pub const CompletionCache = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    /// LRU cache of completion results with freshness tracking
    cache: FreshnessLRUCache(CachedCompletions),
    /// Statistics
    stats: Stats,

    pub const CachedCompletions = struct {
        items: []CompletionItem,
        cached_at: i64,
    };

    pub const Stats = struct {
        hits: u64 = 0,
        misses: u64 = 0,
        stale_hits: u64 = 0,
    };

    /// Static cleanup function for LRU eviction - frees owned strings
    /// Takes Entry wrapper because that's what LRU stores
    fn cleanupEntry(allocator: std.mem.Allocator, entry: FreshnessLRUCache(CachedCompletions).Entry) void {
        for (entry.value.items) |item| {
            allocator.free(item.label);
            if (item.detail) |d| allocator.free(d);
        }
        allocator.free(entry.value.items);
    }

    pub fn init(allocator: std.mem.Allocator) !Self {
        return .{
            .allocator = allocator,
            // Use initWithEntryCleanup to properly free memory on LRU eviction
            .cache = try FreshnessLRUCache(CachedCompletions).initWithEntryCleanup(
                allocator,
                DEFAULT_COMPLETION_CACHE_CAPACITY,
                cleanupEntry,
            ),
            .stats = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        // LRU cache handles cleanup via callback on deinit
        self.cache.deinit();
    }

    /// Get cached completions (allows stale for responsiveness)
    pub fn get(self: *Self, key: CompletionKey) ?[]CompletionItem {
        const hash_key = key.hash();

        if (self.cache.getAllowStale(hash_key)) |cached| {
            if (self.cache.isFresh(hash_key)) {
                self.stats.hits += 1;
            } else {
                self.stats.stale_hits += 1;
            }
            return cached.items;
        }

        self.stats.misses += 1;
        return null;
    }

    /// Store completion results
    pub fn put(self: *Self, key: CompletionKey, items: []const CompletionItem) !void {
        const hash_key = key.hash();

        // Copy items to owned memory
        var owned_items = try self.allocator.alloc(CompletionItem, items.len);
        errdefer self.allocator.free(owned_items);

        for (items, 0..) |item, i| {
            owned_items[i] = .{
                .label = try self.allocator.dupe(u8, item.label),
                .kind = item.kind,
                .detail = if (item.detail) |d| try self.allocator.dupe(u8, d) else null,
            };
        }

        try self.cache.put(hash_key, .{
            .items = owned_items,
            .cached_at = std.time.milliTimestamp(),
        });
    }

    /// Invalidate all completions (called on file change)
    pub fn invalidateAll(self: *Self) void {
        self.cache.invalidateAll();
    }

    /// Get hit rate
    pub fn hitRate(self: *Self) f64 {
        const total = self.stats.hits + self.stats.stale_hits + self.stats.misses;
        if (total == 0) return 0.0;
        return @as(f64, @floatFromInt(self.stats.hits + self.stats.stale_hits)) / @as(f64, @floatFromInt(total));
    }

    /// Get stats
    pub fn getStats(self: *Self) Stats {
        return self.stats;
    }
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

// ===== FRESHNESS LRU CACHE TESTS =====

test "FreshnessLRUCache: fresh entries returned by both methods" {
    var cache = try FreshnessLRUCache(u32).init(std.testing.allocator, 10);
    defer cache.deinit();

    try cache.put(1, 100);

    // Fresh entry accessible via both methods
    try std.testing.expectEqual(@as(?u32, 100), cache.getAllowStale(1));
    try std.testing.expectEqual(@as(?u32, 100), cache.getFreshOnly(1));
    try std.testing.expect(cache.isFresh(1));
}

test "FreshnessLRUCache: stale entries only via getAllowStale" {
    var cache = try FreshnessLRUCache(u32).init(std.testing.allocator, 10);
    defer cache.deinit();

    try cache.put(1, 100);
    cache.markStale(1);

    // Stale entry: getAllowStale returns value, getFreshOnly returns null
    try std.testing.expectEqual(@as(?u32, 100), cache.getAllowStale(1));
    try std.testing.expectEqual(@as(?u32, null), cache.getFreshOnly(1));
    try std.testing.expect(!cache.isFresh(1));
}

test "FreshnessLRUCache: invalidateAll marks everything stale" {
    var cache = try FreshnessLRUCache(u32).init(std.testing.allocator, 10);
    defer cache.deinit();

    try cache.put(1, 100);
    try cache.put(2, 200);
    try cache.put(3, 300);

    // All fresh initially
    try std.testing.expect(cache.isFresh(1));
    try std.testing.expect(cache.isFresh(2));
    try std.testing.expect(cache.isFresh(3));

    cache.invalidateAll();

    // All stale after invalidateAll
    try std.testing.expect(!cache.isFresh(1));
    try std.testing.expect(!cache.isFresh(2));
    try std.testing.expect(!cache.isFresh(3));

    // But values still accessible via getAllowStale
    try std.testing.expectEqual(@as(?u32, 100), cache.getAllowStale(1));
    try std.testing.expectEqual(@as(?u32, 200), cache.getAllowStale(2));
    try std.testing.expectEqual(@as(?u32, 300), cache.getAllowStale(3));
}

test "FreshnessLRUCache: re-put makes entry fresh again" {
    var cache = try FreshnessLRUCache(u32).init(std.testing.allocator, 10);
    defer cache.deinit();

    try cache.put(1, 100);
    cache.markStale(1);
    try std.testing.expect(!cache.isFresh(1));

    // Re-putting makes it fresh
    try cache.put(1, 150);
    try std.testing.expect(cache.isFresh(1));
    try std.testing.expectEqual(@as(?u32, 150), cache.getFreshOnly(1));
}

test "MacroOutputCache: basic operations" {
    var cache = try MacroOutputCache.initWithCapacity(std.testing.allocator, 10);
    defer cache.deinit();

    // Initially empty
    try std.testing.expectEqual(@as(?u64, null), cache.getOutputHash(12345));
    try std.testing.expectEqual(@as(u64, 0), cache.stats.hits);
    try std.testing.expectEqual(@as(u64, 1), cache.stats.misses);
}

test "MacroOutputCache: store and retrieve" {
    var cache = try MacroOutputCache.initWithCapacity(std.testing.allocator, 10);
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
    var cache = try MacroOutputCache.initWithCapacity(std.testing.allocator, 10);
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
    var cache = try MacroOutputCache.initWithCapacity(std.testing.allocator, 10);
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

test "MacroOutputCache: LRU eviction at capacity" {
    // Use small capacity to test eviction
    var cache = try MacroOutputCache.initWithCapacity(std.testing.allocator, 2);
    defer cache.deinit();

    var dummy_node = ast.Node{
        .kind = .program,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &.{}, .file_id = 0 } },
    };

    // Fill cache
    _ = try cache.store(1, 101, &dummy_node);
    _ = try cache.store(2, 102, &dummy_node);
    try std.testing.expectEqual(@as(usize, 2), cache.entryCount());

    // Add third entry, should evict oldest (key 1)
    _ = try cache.store(3, 103, &dummy_node);
    try std.testing.expectEqual(@as(usize, 2), cache.entryCount());

    // Key 1 should be evicted
    try std.testing.expectEqual(@as(?u64, null), cache.getOutputHash(1));
    // Keys 2 and 3 should exist
    try std.testing.expect(cache.getOutputHash(2) != null);
    try std.testing.expect(cache.getOutputHash(3) != null);
}

test "MacroOutputCache: slow query tracking" {
    var cache = try MacroOutputCache.initWithCapacity(std.testing.allocator, 10);
    defer cache.deinit();

    const input_hash: u64 = 12345;

    // Initially not slow
    try std.testing.expect(!cache.isSlowQuery(input_hash));

    // Mark as slow
    try cache.markSlow(input_hash, 150); // 150ms > 100ms threshold

    // Now should be detected as slow
    try std.testing.expect(cache.isSlowQuery(input_hash));
    try std.testing.expectEqual(@as(u64, 1), cache.stats.slow_queries_flagged);
    try std.testing.expectEqual(@as(u64, 1), cache.stats.slow_queries_skipped);

    // Clear slow queries
    cache.clearSlowQueries();
    try std.testing.expect(!cache.isSlowQuery(input_hash));
}

// ===== COMPLETION CACHE TESTS =====

test "CompletionCache: basic put and get" {
    var cache = try CompletionCache.init(std.testing.allocator);
    defer cache.deinit();

    const key = CompletionKey{
        .file_hash = 12345,
        .line = 10,
        .column = 5,
        .prefix_hash = 67890,
    };

    const items = [_]CompletionItem{
        .{ .label = "foo", .kind = 6 },
        .{ .label = "bar", .kind = 3, .detail = "A function" },
    };

    try cache.put(key, &items);

    const retrieved = cache.get(key);
    try std.testing.expect(retrieved != null);
    try std.testing.expectEqual(@as(usize, 2), retrieved.?.len);
    try std.testing.expectEqualStrings("foo", retrieved.?[0].label);
    try std.testing.expectEqualStrings("bar", retrieved.?[1].label);
}

test "CompletionCache: invalidateAll marks stale" {
    var cache = try CompletionCache.init(std.testing.allocator);
    defer cache.deinit();

    const key = CompletionKey{
        .file_hash = 11111,
        .line = 1,
        .column = 1,
        .prefix_hash = 22222,
    };

    const items = [_]CompletionItem{
        .{ .label = "test", .kind = 1 },
    };

    try cache.put(key, &items);

    // Should be fresh hit
    _ = cache.get(key);
    try std.testing.expectEqual(@as(u64, 1), cache.stats.hits);

    // Invalidate
    cache.invalidateAll();

    // Should be stale hit (still returns data)
    const stale = cache.get(key);
    try std.testing.expect(stale != null);
    try std.testing.expectEqual(@as(u64, 1), cache.stats.stale_hits);
}

test "CompletionKey: hash is deterministic" {
    const key1 = CompletionKey{
        .file_hash = 100,
        .line = 10,
        .column = 5,
        .prefix_hash = 200,
    };

    const key2 = CompletionKey{
        .file_hash = 100,
        .line = 10,
        .column = 5,
        .prefix_hash = 200,
    };

    const key3 = CompletionKey{
        .file_hash = 100,
        .line = 11, // Different line
        .column = 5,
        .prefix_hash = 200,
    };

    try std.testing.expectEqual(key1.hash(), key2.hash());
    try std.testing.expect(key1.hash() != key3.hash());
}
