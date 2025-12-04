// Network Cache (L5) - Disk-based cache for @comptime fetch() responses
//
// Caches network responses across LSP sessions to avoid repeated fetches.
// Location: .metascript-cache/network/<url-hash>.json
// Default TTL: 24 hours (following haxe-lsp pattern)

const std = @import("std");
const fs = std.fs;

/// Default TTL in milliseconds (24 hours)
pub const DEFAULT_TTL_MS: i64 = 24 * 60 * 60 * 1000;

/// Network fetch timeout in milliseconds
pub const NETWORK_TIMEOUT_MS: u64 = 2000;

/// Cached network response
pub const CachedResponse = struct {
    /// Original URL
    url: []const u8,
    /// Response body
    body: []const u8,
    /// When the response was fetched
    fetched_at_ms: i64,
    /// TTL in milliseconds
    ttl_ms: i64,
    /// HTTP status code (if available)
    status_code: u16,
    /// Content type (if available)
    content_type: ?[]const u8,
};

/// Network cache for @comptime fetch() operations
pub const NetworkCache = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    /// Cache directory path
    cache_dir: []const u8,
    /// Statistics
    stats: Stats,

    pub const Stats = struct {
        hits: u64 = 0,
        misses: u64 = 0,
        expired: u64 = 0,
        errors: u64 = 0,
    };

    /// Initialize network cache with project root
    /// Cache location: <project_root>/.metascript-cache/network/
    pub fn init(allocator: std.mem.Allocator, project_root: ?[]const u8) !Self {
        const root = project_root orelse ".";
        const cache_dir = try std.fs.path.join(allocator, &.{ root, ".metascript-cache", "network" });

        // Create cache directory if it doesn't exist
        fs.cwd().makePath(cache_dir) catch |err| {
            if (err != error.PathAlreadyExists) {
                std.log.warn("[NetworkCache] Failed to create cache dir: {}", .{err});
            }
        };

        return .{
            .allocator = allocator,
            .cache_dir = cache_dir,
            .stats = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.cache_dir);
    }

    /// Get cached response for URL (returns null if missing or expired)
    pub fn get(self: *Self, url: []const u8) ?CachedResponse {
        const cache_path = self.getCachePath(url) catch {
            self.stats.errors += 1;
            return null;
        };
        defer self.allocator.free(cache_path);

        const file = fs.cwd().openFile(cache_path, .{}) catch {
            self.stats.misses += 1;
            return null;
        };
        defer file.close();

        // Read and parse cached response
        const content = file.readToEndAlloc(self.allocator, 10 * 1024 * 1024) catch {
            self.stats.errors += 1;
            return null;
        };
        defer self.allocator.free(content);

        const parsed = std.json.parseFromSlice(CacheEntry, self.allocator, content, .{}) catch {
            self.stats.errors += 1;
            return null;
        };
        defer parsed.deinit();

        const entry = parsed.value;

        // Check expiration
        const now = std.time.milliTimestamp();
        if (now > entry.fetched_at_ms + entry.ttl_ms) {
            self.stats.expired += 1;
            return null;
        }

        self.stats.hits += 1;

        // Return a copy with owned memory
        return CachedResponse{
            .url = self.allocator.dupe(u8, entry.url) catch return null,
            .body = self.allocator.dupe(u8, entry.body) catch return null,
            .fetched_at_ms = entry.fetched_at_ms,
            .ttl_ms = entry.ttl_ms,
            .status_code = entry.status_code,
            .content_type = if (entry.content_type) |ct| self.allocator.dupe(u8, ct) catch null else null,
        };
    }

    /// Store response in cache
    pub fn put(self: *Self, response: CachedResponse) !void {
        const cache_path = try self.getCachePath(response.url);
        defer self.allocator.free(cache_path);

        const entry = CacheEntry{
            .url = response.url,
            .body = response.body,
            .fetched_at_ms = response.fetched_at_ms,
            .ttl_ms = response.ttl_ms,
            .status_code = response.status_code,
            .content_type = response.content_type,
        };

        var file = try fs.cwd().createFile(cache_path, .{});
        defer file.close();

        try std.json.stringify(entry, .{}, file.writer());
    }

    /// Clear all cached responses
    pub fn clear(self: *Self) !void {
        var dir = fs.cwd().openDir(self.cache_dir, .{ .iterate = true }) catch return;
        defer dir.close();

        var iter = dir.iterate();
        while (try iter.next()) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".json")) {
                dir.deleteFile(entry.name) catch {};
            }
        }
    }

    /// Free a CachedResponse returned by get()
    pub fn freeResponse(self: *Self, response: CachedResponse) void {
        self.allocator.free(response.url);
        self.allocator.free(response.body);
        if (response.content_type) |ct| self.allocator.free(ct);
    }

    /// Get cache file path for URL
    fn getCachePath(self: *Self, url: []const u8) ![]const u8 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(url);
        const hash = hasher.final();

        var hash_str: [16]u8 = undefined;
        _ = std.fmt.bufPrint(&hash_str, "{x:0>16}", .{hash}) catch unreachable;

        return std.fs.path.join(self.allocator, &.{ self.cache_dir, &hash_str ++ ".json" });
    }

    /// Get statistics
    pub fn getStats(self: *Self) Stats {
        return self.stats;
    }

    /// Internal cache entry format
    const CacheEntry = struct {
        url: []const u8,
        body: []const u8,
        fetched_at_ms: i64,
        ttl_ms: i64,
        status_code: u16,
        content_type: ?[]const u8,
    };
};

// ===== TESTS =====

test "NetworkCache: init creates directory" {
    var cache = try NetworkCache.init(std.testing.allocator, "/tmp/test-metascript-cache");
    defer cache.deinit();

    // Directory should exist
    const stat = try std.fs.cwd().statFile("/tmp/test-metascript-cache/.metascript-cache/network");
    try std.testing.expect(stat.kind == .directory);

    // Cleanup
    std.fs.cwd().deleteTree("/tmp/test-metascript-cache") catch {};
}

test "NetworkCache: put and get" {
    var cache = try NetworkCache.init(std.testing.allocator, "/tmp/test-metascript-cache2");
    defer cache.deinit();
    defer std.fs.cwd().deleteTree("/tmp/test-metascript-cache2") catch {};

    const response = CachedResponse{
        .url = "https://example.com/api",
        .body = "{\"data\": 123}",
        .fetched_at_ms = std.time.milliTimestamp(),
        .ttl_ms = DEFAULT_TTL_MS,
        .status_code = 200,
        .content_type = "application/json",
    };

    try cache.put(response);

    const retrieved = cache.get("https://example.com/api");
    try std.testing.expect(retrieved != null);

    if (retrieved) |r| {
        defer cache.freeResponse(r);
        try std.testing.expectEqualStrings("{\"data\": 123}", r.body);
        try std.testing.expectEqual(@as(u16, 200), r.status_code);
    }
}

test "NetworkCache: expired entries return null" {
    var cache = try NetworkCache.init(std.testing.allocator, "/tmp/test-metascript-cache3");
    defer cache.deinit();
    defer std.fs.cwd().deleteTree("/tmp/test-metascript-cache3") catch {};

    const response = CachedResponse{
        .url = "https://example.com/old",
        .body = "old data",
        .fetched_at_ms = std.time.milliTimestamp() - 50000, // 50 seconds ago
        .ttl_ms = 1000, // 1 second TTL (expired)
        .status_code = 200,
        .content_type = null,
    };

    try cache.put(response);

    const retrieved = cache.get("https://example.com/old");
    try std.testing.expectEqual(@as(?CachedResponse, null), retrieved);
    try std.testing.expectEqual(@as(u64, 1), cache.stats.expired);
}
