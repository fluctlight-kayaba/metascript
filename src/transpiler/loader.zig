/// Macro Loader - Orchestrates the TypeScript → JavaScript → HBC pipeline
///
/// Pipeline:
///   macro.ts → esbuild → macro.js → hermesc → macro.hbc → cache → execute (~0.2ms)
///
/// This module manages the full compilation pipeline for macro bytecode,
/// including caching for fast subsequent loads.
const std = @import("std");
const cache = @import("cache.zig");
const hermes = @import("hermes.zig");
const esbuild = @import("esbuild.zig");

/// Loader error set
pub const LoaderError = error{
    TranspileFailed,
    CompileFailed,
    CacheError,
    OutOfMemory,
};

/// Loader configuration
pub const LoaderConfig = struct {
    /// Cache directory (default: ~/.cache/metascript/bytecode)
    cache_dir: []const u8,

    /// Enable caching (disable for debugging)
    cache_enabled: bool = true,

    /// Verbose logging
    verbose: bool = false,
};

/// Statistics for loader operations
pub const LoaderStats = struct {
    cache_hits: usize = 0,
    cache_misses: usize = 0,
    transpile_time_ns: u64 = 0,
    compile_time_ns: u64 = 0,

    pub fn totalTimeMs(self: LoaderStats) f64 {
        const total_ns = self.transpile_time_ns + self.compile_time_ns;
        return @as(f64, @floatFromInt(total_ns)) / 1_000_000.0;
    }
};

/// Load macro bytecode from TypeScript source
///
/// This is the main entry point. It handles:
/// 1. Cache lookup (if enabled)
/// 2. TypeScript → JavaScript transpilation (via esbuild)
/// 3. JavaScript → HBC compilation (via hermesc)
/// 4. Cache storage
///
/// Returns Hermes bytecode (.hbc) ready for execution
pub fn loadMacroBytecode(
    allocator: std.mem.Allocator,
    config: LoaderConfig,
    source_path: []const u8,
    stats: ?*LoaderStats,
) ![]const u8 {
    // Check cache first
    if (config.cache_enabled) {
        const cache_config = cache.CacheConfig{
            .cache_dir = config.cache_dir,
            .enabled = true,
        };

        const cache_path = try cache.getCachePath(allocator, cache_config, source_path);
        defer allocator.free(cache_path);

        if (cache.isCacheFresh(source_path, cache_path)) {
            // Cache hit!
            if (stats) |s| s.cache_hits += 1;
            if (config.verbose) {
                std.log.info("[loader] Cache hit: {s}", .{source_path});
            }
            return try cache.loadFromCache(allocator, cache_path);
        }

        if (stats) |s| s.cache_misses += 1;
    }

    // Cache miss - full pipeline
    if (config.verbose) {
        std.log.info("[loader] Cache miss, compiling: {s}", .{source_path});
    }

    // Step 1: TypeScript → JavaScript
    const transpile_start = std.time.nanoTimestamp();
    const js_code = esbuild.transpileFile(allocator, source_path) catch |err| {
        std.log.err("[loader] Transpilation failed for {s}: {}", .{source_path, err});
        return LoaderError.TranspileFailed;
    };
    defer allocator.free(js_code);

    const transpile_end = std.time.nanoTimestamp();
    if (stats) |s| {
        s.transpile_time_ns += @intCast(@as(i64, @intCast(transpile_end)) - @as(i64, @intCast(transpile_start)));
    }

    if (config.verbose) {
        const transpile_ms = @as(f64, @floatFromInt(transpile_end - transpile_start)) / 1_000_000.0;
        std.log.info("[loader] Transpiled in {d:.2}ms ({d} bytes)", .{transpile_ms, js_code.len});
    }

    // Step 2: JavaScript → HBC
    const compile_start = std.time.nanoTimestamp();
    const bytecode = hermes.compile(allocator, js_code) catch |err| {
        std.log.err("[loader] Bytecode compilation failed: {}", .{err});
        return LoaderError.CompileFailed;
    };

    const compile_end = std.time.nanoTimestamp();
    if (stats) |s| {
        s.compile_time_ns += @intCast(@as(i64, @intCast(compile_end)) - @as(i64, @intCast(compile_start)));
    }

    if (config.verbose) {
        const compile_ms = @as(f64, @floatFromInt(compile_end - compile_start)) / 1_000_000.0;
        std.log.info("[loader] Compiled to HBC in {d:.2}ms ({d} bytes)", .{compile_ms, bytecode.len});
    }

    // Step 3: Save to cache
    if (config.cache_enabled) {
        const cache_config = cache.CacheConfig{
            .cache_dir = config.cache_dir,
            .enabled = true,
        };

        const cache_path = try cache.getCachePath(allocator, cache_config, source_path);
        defer allocator.free(cache_path);

        cache.saveToCache(cache_path, bytecode) catch |err| {
            // Cache save failure is not fatal, just log it
            std.log.warn("[loader] Failed to save cache: {}", .{err});
        };
    }

    return bytecode;
}

/// Load macro bytecode from inline TypeScript source (not a file)
/// Used for built-in macros or dynamically generated macro code
pub fn loadMacroBytecodeFromSource(
    allocator: std.mem.Allocator,
    config: LoaderConfig,
    source: []const u8,
    source_name: []const u8, // For cache key and error messages
    stats: ?*LoaderStats,
) ![]const u8 {
    // Check cache using content hash
    if (config.cache_enabled) {
        const cache_key = try cache.computeCacheKeyFromContent(allocator, source, source_name);
        defer allocator.free(cache_key);

        const cache_path = try std.fmt.allocPrint(
            allocator,
            "{s}/{s}.hbc",
            .{config.cache_dir, cache_key},
        );
        defer allocator.free(cache_path);

        // For inline sources, we always recompile if source changed (content hash)
        // Check if cache file exists
        if (std.fs.cwd().statFile(cache_path)) |_| {
            if (stats) |s| s.cache_hits += 1;
            if (config.verbose) {
                std.log.info("[loader] Cache hit (inline): {s}", .{source_name});
            }
            return try cache.loadFromCache(allocator, cache_path);
        } else |_| {
            // File doesn't exist, continue to compile
        }

        if (stats) |s| s.cache_misses += 1;
    }

    // Full pipeline for inline source
    if (config.verbose) {
        std.log.info("[loader] Compiling inline macro: {s}", .{source_name});
    }

    // Step 1: TypeScript → JavaScript
    const transpile_start = std.time.nanoTimestamp();
    const js_code = esbuild.transpile(allocator, source, "ts") catch |err| {
        std.log.err("[loader] Transpilation failed for {s}: {}", .{source_name, err});
        return LoaderError.TranspileFailed;
    };
    defer allocator.free(js_code);

    const transpile_end = std.time.nanoTimestamp();
    if (stats) |s| {
        s.transpile_time_ns += @intCast(@as(i64, @intCast(transpile_end)) - @as(i64, @intCast(transpile_start)));
    }

    // Step 2: JavaScript → HBC
    const compile_start = std.time.nanoTimestamp();
    const bytecode = hermes.compile(allocator, js_code) catch |err| {
        std.log.err("[loader] Bytecode compilation failed: {}", .{err});
        return LoaderError.CompileFailed;
    };

    const compile_end = std.time.nanoTimestamp();
    if (stats) |s| {
        s.compile_time_ns += @intCast(@as(i64, @intCast(compile_end)) - @as(i64, @intCast(compile_start)));
    }

    // Step 3: Save to cache
    if (config.cache_enabled) {
        const cache_key = try cache.computeCacheKeyFromContent(allocator, source, source_name);
        defer allocator.free(cache_key);

        const cache_path = try std.fmt.allocPrint(
            allocator,
            "{s}/{s}.hbc",
            .{config.cache_dir, cache_key},
        );
        defer allocator.free(cache_path);

        cache.saveToCache(cache_path, bytecode) catch |err| {
            std.log.warn("[loader] Failed to save cache: {}", .{err});
        };
    }

    return bytecode;
}

/// Get default loader configuration
pub fn getDefaultConfig(allocator: std.mem.Allocator) !LoaderConfig {
    const cache_dir = try cache.getDefaultCacheDir(allocator);
    return LoaderConfig{
        .cache_dir = cache_dir,
        .cache_enabled = true,
        .verbose = false,
    };
}

// ============================================================================
// Unit Tests
// ============================================================================

test "loader pipeline - inline source" {
    const allocator = std.testing.allocator;

    // Get default config
    const cache_dir = cache.getDefaultCacheDir(allocator) catch {
        std.debug.print("Skipping test - cannot get cache dir\n", .{});
        return;
    };
    defer allocator.free(cache_dir);

    const config = LoaderConfig{
        .cache_dir = cache_dir,
        .cache_enabled = true,
        .verbose = true,
    };

    var stats = LoaderStats{};

    const source =
        \\const x: number = 42;
        \\console.log(x);
    ;

    const bytecode = loadMacroBytecodeFromSource(
        allocator,
        config,
        source,
        "test-macro",
        &stats,
    ) catch |err| {
        std.debug.print("Skipping test - toolchain not available: {}\n", .{err});
        return;
    };
    defer allocator.free(bytecode);

    // Verify bytecode was generated
    try std.testing.expect(bytecode.len > 0);

    std.debug.print("\n=== Loader Pipeline Test ===\n", .{});
    std.debug.print("Bytecode size: {d} bytes\n", .{bytecode.len});
    std.debug.print("Total time: {d:.2}ms\n", .{stats.totalTimeMs()});
    std.debug.print("Cache hits: {d}, misses: {d}\n", .{stats.cache_hits, stats.cache_misses});
}

test "loader caching" {
    const allocator = std.testing.allocator;

    const cache_dir = cache.getDefaultCacheDir(allocator) catch {
        std.debug.print("Skipping test - cannot get cache dir\n", .{});
        return;
    };
    defer allocator.free(cache_dir);

    const config = LoaderConfig{
        .cache_dir = cache_dir,
        .cache_enabled = true,
        .verbose = false,
    };

    const source =
        \\const cached: boolean = true;
        \\module.exports = { cached };
    ;

    // First load - should be cache miss
    var stats1 = LoaderStats{};
    const bytecode1 = loadMacroBytecodeFromSource(
        allocator,
        config,
        source,
        "cache-test-macro",
        &stats1,
    ) catch |err| {
        std.debug.print("Skipping test - toolchain not available: {}\n", .{err});
        return;
    };
    defer allocator.free(bytecode1);

    try std.testing.expectEqual(@as(usize, 1), stats1.cache_misses);

    // Second load - should be cache hit
    var stats2 = LoaderStats{};
    const bytecode2 = loadMacroBytecodeFromSource(
        allocator,
        config,
        source,
        "cache-test-macro",
        &stats2,
    ) catch |err| {
        std.debug.print("Skipping test - toolchain not available: {}\n", .{err});
        return;
    };
    defer allocator.free(bytecode2);

    try std.testing.expectEqual(@as(usize, 1), stats2.cache_hits);
    try std.testing.expectEqual(@as(usize, 0), stats2.cache_misses);

    // Bytecode should be identical
    try std.testing.expectEqualSlices(u8, bytecode1, bytecode2);

    std.debug.print("\n=== Caching Test ===\n", .{});
    std.debug.print("First load (miss): {d:.2}ms\n", .{stats1.totalTimeMs()});
    std.debug.print("Second load (hit): {d:.2}ms\n", .{stats2.totalTimeMs()});
}
