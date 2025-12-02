// Disk-based cache for .metascript-cache directory
// Shared between msc and mls for macro bytecode (.hbc) and expansion results
//
// CACHE SAFETY DESIGN (Reviewed by 3 Principal Engineers - Round 2):
// 1. Version check FIRST - if Hermes/Metascript version changes, invalidate all
// 2. Atomic writes - write to .tmp with random suffix, fsync, then rename
// 3. Checksum verification - SHA256 stored alongside bytecode, verified on load
// 4. Fallback on ANY error - source execution is always the fallback
// 5. File locking - per-hash locks prevent race conditions between msc/mls
// 6. TOCTOU protection - checksum written FIRST for better failure mode
// 7. Bytecode validation - Hermes magic bytes checked before execution
// 8. LRU eviction - bounded memory usage with configurable limit
// 9. Symlink protection - O_NOFOLLOW on POSIX prevents atomic symlink attacks
// 10. SHA256 for cache keys - collision-resistant (not Wyhash)
// 11. Thread safety - Mutex protects all LRU operations
// 12. Path traversal protection - validates project_root has no ".."
// 13. Atomic disabled flag - prevents race conditions
// 14. Lock files not deleted - prevents lock acquisition race
// 15. O_NOFOLLOW defense-in-depth - openFileNoFollow() rejects symlinks atomically
//
// Directory structure:
//   .metascript-cache/
//   ├── cache.version          # "4|hermes-0.12|msc-0.1.0"
//   ├── bytecode/
//   │   ├── <sha256-prefix>.hbc           # Compiled bytecode files
//   │   ├── <sha256-prefix>.hbc.checksum  # SHA256 checksum
//   │   └── <sha256-prefix>.hbc.lock      # Per-file lock (persistent)
//   └── expansions/            # Cached macro expansion results (Level 2)

const std = @import("std");
const ast = @import("../ast/ast.zig");
const builtin = @import("builtin");
const posix = std.posix;

pub const CACHE_DIR = ".metascript-cache";
pub const BYTECODE_DIR = "bytecode";
pub const EXPANSIONS_DIR = "expansions";
pub const VERSION_FILE = "cache.version";

// Version format: "format_version|hermes_version|msc_version"
// IMPORTANT: Increment CACHE_FORMAT_VERSION when changing cache format!
pub const CACHE_FORMAT_VERSION = "4"; // Bumped for thread safety + security fixes
pub const HERMES_VERSION = "0.12"; // Update when Hermes is upgraded
pub const MSC_VERSION = "0.1.0"; // Update on breaking AST API changes

// Maximum allowed cache entries to prevent DoS via memory exhaustion
pub const MAX_ALLOWED_CACHE_ENTRIES = 10000;

// Hermes bytecode magic bytes (from Hermes source)
pub const HERMES_MAGIC: [4]u8 = .{ 0xc6, 0x1f, 0xbc, 0x03 };

// LRU cache configuration
pub const DEFAULT_MAX_CACHE_ENTRIES = 128; // Max bytecode entries in memory
pub const MAX_BYTECODE_SIZE = 10 * 1024 * 1024; // 10 MB max per file

/// Build the full version string for cache validation
pub fn buildVersionString() []const u8 {
    return CACHE_FORMAT_VERSION ++ "|" ++ HERMES_VERSION ++ "|" ++ MSC_VERSION;
}

/// Manifest entry for a cached bytecode file
pub const ManifestEntry = struct {
    source_hash: [32]u8, // SHA256 hash (256-bit, collision resistant)
    checksum: [64]u8, // SHA256 hex string of bytecode
    compiled_at: i64, // Unix timestamp
    source_name: []const u8, // Original macro name for debugging

    pub fn checksumSlice(self: *const ManifestEntry) []const u8 {
        return &self.checksum;
    }
};

/// Result of cache validation
pub const CacheValidation = enum {
    valid,
    version_mismatch, // Entire cache must be invalidated
    missing, // Cache doesn't exist yet
    corrupted, // Cache exists but is corrupted
};

/// Error types for cache operations
pub const CacheError = error{
    CacheNotValidated,
    ChecksumMissing,
    ChecksumReadFailed,
    ChecksumMismatch,
    InvalidBytecodeMagic,
    BytecodeTooLarge,
    NotARegularFile,
    LockAcquisitionFailed,
    DiskFull,
    OutOfMemory,
    FileNotFound,
    AccessDenied,
    Unexpected,
    PathTraversalDetected, // Security: ".." in path
    MaxEntriesExceeded, // DoS protection
};

/// LRU node for bytecode cache eviction
const LRUNode = struct {
    hash: [32]u8,
    bytes: []const u8,
    checksum: [64]u8,
    prev: ?*LRUNode,
    next: ?*LRUNode,
};

/// Disk-based bytecode cache for compiled macros
/// Thread-safe for concurrent access via Mutex (in-memory) and file locking (on-disk)
pub const BytecodeCache = struct {
    allocator: std.mem.Allocator,
    cache_dir: []const u8,
    project_root: []const u8,

    /// LRU-ordered cache: SHA256 hash -> bytecode
    bytecode_index: std.AutoHashMap([32]u8, *LRUNode),
    lru_head: ?*LRUNode, // Most recently used
    lru_tail: ?*LRUNode, // Least recently used
    cache_count: usize,
    max_cache_entries: usize,

    /// Whether cache has been validated this session
    validated: bool,

    /// Disabled flag for graceful degradation (e.g., disk full)
    /// ATOMIC to prevent race conditions between threads
    disabled: std.atomic.Value(bool),

    /// Mutex for thread-safe LRU operations
    /// Protects: bytecode_index, lru_head, lru_tail, cache_count
    mutex: std.Thread.Mutex,
    // PE4: Removed shared PRNG - now using getrandom() per call for thread safety

    pub fn init(allocator: std.mem.Allocator, project_root: ?[]const u8) !BytecodeCache {
        return initWithCapacity(allocator, project_root, DEFAULT_MAX_CACHE_ENTRIES);
    }

    pub fn initWithCapacity(allocator: std.mem.Allocator, project_root: ?[]const u8, max_entries: usize) !BytecodeCache {
        const root = project_root orelse ".";

        // SECURITY: Validate path doesn't contain traversal sequences
        if (containsPathTraversal(root)) {
            std.log.err("[Cache] Path traversal detected in project_root: {s}", .{root});
            return error.PathTraversalDetected;
        }

        // DoS protection: limit max entries
        const actual_max = @min(max_entries, MAX_ALLOWED_CACHE_ENTRIES);
        if (max_entries > MAX_ALLOWED_CACHE_ENTRIES) {
            std.log.warn("[Cache] Requested max_entries {d} exceeds limit, using {d}", .{ max_entries, MAX_ALLOWED_CACHE_ENTRIES });
        }

        const root_owned = try allocator.dupe(u8, root);
        errdefer allocator.free(root_owned);

        const cache_dir = try std.fs.path.join(allocator, &.{ root, CACHE_DIR });
        errdefer allocator.free(cache_dir);

        // Pre-allocate hash map to avoid repeated resizing
        var index = std.AutoHashMap([32]u8, *LRUNode).init(allocator);
        errdefer index.deinit(); // PE2 FIX: Free HashMap if subsequent operations fail
        try index.ensureTotalCapacity(@intCast(actual_max));

        // PE4: Random state removed - using getrandom() per-call for thread safety

        var self = BytecodeCache{
            .allocator = allocator,
            .cache_dir = cache_dir,
            .project_root = root_owned,
            .bytecode_index = index,
            .lru_head = null,
            .lru_tail = null,
            .cache_count = 0,
            .max_cache_entries = actual_max,
            .validated = false,
            .disabled = std.atomic.Value(bool).init(false), // PE2 FIX: Atomic initialization
            .mutex = .{}, // PE1 FIX: Thread safety
        };

        // Validate and potentially invalidate cache
        const validation = self.validateCache() catch .corrupted;
        switch (validation) {
            .valid => {
                self.validated = true;
                std.log.debug("[Cache] Valid cache found at {s}", .{cache_dir});
            },
            .version_mismatch => {
                std.log.warn("[Cache] Version mismatch, invalidating entire cache (expected: {s})", .{buildVersionString()});
                self.invalidateAll() catch {};
                self.ensureDirectories() catch |err| {
                    std.log.warn("[Cache] Failed to create directories: {}, disabling cache", .{err});
                    self.disabled.store(true, .release);
                };
                self.validated = !self.disabled.load(.acquire);
            },
            .missing => {
                std.log.debug("[Cache] No cache found, creating new", .{});
                self.ensureDirectories() catch |err| {
                    std.log.warn("[Cache] Failed to create directories: {}, disabling cache", .{err});
                    self.disabled.store(true, .release);
                };
                self.validated = !self.disabled.load(.acquire);
            },
            .corrupted => {
                std.log.warn("[Cache] Corrupted cache detected, recreating", .{});
                self.invalidateAll() catch {};
                self.ensureDirectories() catch |err| {
                    std.log.warn("[Cache] Failed to create directories: {}, disabling cache", .{err});
                    self.disabled.store(true, .release);
                };
                self.validated = !self.disabled.load(.acquire);
            },
        }

        return self;
    }

    /// Clean up cache resources
    /// PE4 FIX: Thread-safe - acquires mutex to prevent concurrent access during cleanup
    /// WARNING: Caller must ensure no other threads will access cache after deinit begins
    pub fn deinit(self: *BytecodeCache) void {
        // Acquire mutex to block concurrent operations during shutdown
        // We don't unlock because we're destroying the mutex along with the struct
        self.mutex.lock();

        // Free all LRU nodes
        var node = self.lru_head;
        while (node) |n| {
            const next = n.next;
            self.allocator.free(n.bytes);
            self.allocator.destroy(n);
            node = next;
        }
        self.bytecode_index.deinit();
        self.allocator.free(self.cache_dir);
        self.allocator.free(self.project_root);
        // Note: mutex is NOT unlocked - struct is being destroyed
    }

    /// Validate cache version and integrity
    fn validateCache(self: *BytecodeCache) !CacheValidation {
        const version_path = try std.fs.path.join(self.allocator, &.{ self.cache_dir, VERSION_FILE });
        defer self.allocator.free(version_path);

        const file = std.fs.cwd().openFile(version_path, .{}) catch |err| {
            if (err == error.FileNotFound) return .missing;
            return .corrupted;
        };
        defer file.close();

        var buf: [128]u8 = undefined;
        const len = file.readAll(&buf) catch return .corrupted;
        const stored_version = buf[0..len];

        const expected = buildVersionString();
        if (!std.mem.eql(u8, stored_version, expected)) {
            std.log.info("[Cache] Version mismatch: expected '{s}', found '{s}'", .{ expected, stored_version });
            return .version_mismatch;
        }

        return .valid;
    }

    /// Invalidate entire cache (delete all files)
    /// PE4 FIX: Thread-safe via mutex protection
    fn invalidateAll(self: *BytecodeCache) !void {
        std.log.info("[Cache] Invalidating all cached bytecode", .{});

        // PE4 FIX: Hold mutex while clearing LRU
        self.mutex.lock();

        // Clear in-memory LRU cache
        var node = self.lru_head;
        while (node) |n| {
            const next = n.next;
            self.allocator.free(n.bytes);
            self.allocator.destroy(n);
            node = next;
        }
        self.lru_head = null;
        self.lru_tail = null;
        self.cache_count = 0;
        self.bytecode_index.clearRetainingCapacity();

        // Release mutex before disk I/O (disk ops are independent)
        self.mutex.unlock();

        // Delete cache directory
        std.fs.cwd().deleteTree(self.cache_dir) catch |err| {
            if (err != error.FileNotFound) {
                std.log.warn("[Cache] Failed to delete cache: {}", .{err});
            }
        };
    }

    /// Ensure cache directory structure exists with version file
    fn ensureDirectories(self: *BytecodeCache) !void {
        const bytecode_path = try std.fs.path.join(self.allocator, &.{ self.cache_dir, BYTECODE_DIR });
        defer self.allocator.free(bytecode_path);

        const expansions_path = try std.fs.path.join(self.allocator, &.{ self.cache_dir, EXPANSIONS_DIR });
        defer self.allocator.free(expansions_path);

        // Create directories
        std.fs.cwd().makePath(bytecode_path) catch |err| {
            if (err != error.PathAlreadyExists) return err;
        };
        std.fs.cwd().makePath(expansions_path) catch |err| {
            if (err != error.PathAlreadyExists) return err;
        };

        // Write version file (atomic)
        const version_path = try std.fs.path.join(self.allocator, &.{ self.cache_dir, VERSION_FILE });
        defer self.allocator.free(version_path);

        const tmp_path = try std.fmt.allocPrint(self.allocator, "{s}.tmp", .{version_path});
        defer self.allocator.free(tmp_path);

        // Write to temp file
        const file = std.fs.cwd().createFile(tmp_path, .{ .mode = 0o644 }) catch |err| {
            return self.handleDiskError(err);
        };
        file.writeAll(buildVersionString()) catch |err| {
            file.close();
            std.fs.cwd().deleteFile(tmp_path) catch {};
            return self.handleDiskError(err);
        };
        file.close();

        // Atomic rename
        std.fs.cwd().rename(tmp_path, version_path) catch |err| {
            std.fs.cwd().deleteFile(tmp_path) catch {};
            return err;
        };
    }

    /// Handle disk errors gracefully
    fn handleDiskError(self: *BytecodeCache, err: anyerror) anyerror {
        if (err == error.NoSpaceLeft or err == error.DiskQuota) {
            std.log.warn("[Cache] Disk full, disabling cache for this session", .{});
            self.disabled.store(true, .release);
            return error.DiskFull;
        }
        return err;
    }

    /// Get cached bytecode for a macro source hash (SHA256)
    /// Returns null if not cached, checksum mismatch, or invalid bytecode
    /// Thread-safe via mutex protection
    ///
    /// PE4 FIX: Returns OWNED memory - caller MUST free with same allocator
    /// This prevents use-after-free if another thread evicts the entry
    pub fn getBytecode(self: *BytecodeCache, source_hash: [32]u8) ?[]const u8 {
        if (!self.validated or self.disabled.load(.acquire)) return null;

        // PE1 FIX: Mutex protection for LRU access
        self.mutex.lock();
        defer self.mutex.unlock();

        // Check in-memory LRU cache first
        if (self.bytecode_index.getPtr(source_hash)) |node_ptr| {
            // Move to front of LRU
            self.moveToFront(node_ptr.*);
            // PE4 FIX: Clone bytes before returning to prevent use-after-free
            // Caller owns this memory and must free it
            return self.allocator.dupe(u8, node_ptr.*.bytes) catch null;
        }

        // Try to load from disk (mutex still held to prevent races)
        const bytes = self.loadFromDisk(source_hash) catch |err| {
            std.log.debug("[Cache] Failed to load bytecode: {}", .{err});
            return null;
        };

        // PE4 FIX: loadFromDisk result is stored in cache, clone for caller
        return self.allocator.dupe(u8, bytes) catch null;
    }

    /// Load bytecode from disk with TOCTOU-safe checksum verification
    /// SECURITY: Reads checksum BEFORE bytecode to prevent tampering
    /// SECURITY: Uses lstat to check for symlinks BEFORE opening
    fn loadFromDisk(self: *BytecodeCache, source_hash: [32]u8) ![]const u8 {
        const filename = try self.hashToFilename(&source_hash, ".hbc");
        defer self.allocator.free(filename);

        const filepath = try std.fs.path.join(self.allocator, &.{ self.cache_dir, BYTECODE_DIR, filename });
        defer self.allocator.free(filepath);

        const checksum_path = try std.fmt.allocPrint(self.allocator, "{s}.checksum", .{filepath});
        defer self.allocator.free(checksum_path);

        // PE4 FIX: Read checksum FIRST with symlink protection
        // Both pre-check (readLink) AND post-check (stat) for defense-in-depth
        var expected_checksum: [64]u8 = undefined;
        {
            // Pre-check: detect obvious symlinks
            var link_buf: [std.fs.max_path_bytes]u8 = undefined;
            if (std.fs.cwd().readLink(checksum_path, &link_buf)) |_| {
                std.log.warn("[Cache] {s}.checksum is a symlink, rejecting", .{filename});
                return error.NotARegularFile;
            } else |err| {
                if (err != error.NotLink and err != error.FileNotFound) {
                    return error.Unexpected;
                }
            }

            const checksum_file = openFileNoFollow(checksum_path) catch |err| {
                if (err == error.SymLinkLoop) {
                    std.log.warn("[Cache] {s}.checksum is a symlink (O_NOFOLLOW), rejecting", .{filename});
                    return error.NotARegularFile;
                }
                std.log.debug("[Cache] Missing checksum file for {s}", .{filename});
                return error.ChecksumMissing;
            };
            defer checksum_file.close();

            // Post-check: verify file type AFTER opening (closes TOCTOU gap)
            const cstat = checksum_file.stat() catch return error.Unexpected;
            if (cstat.kind != .file) {
                std.log.warn("[Cache] {s}.checksum is not a regular file after open", .{filename});
                return error.NotARegularFile;
            }

            const read_len = checksum_file.readAll(&expected_checksum) catch {
                return error.ChecksumReadFailed;
            };
            if (read_len != 64) {
                return error.ChecksumReadFailed;
            }
        }

        // PE4 FIX: Symlink protection for bytecode file
        // Pre-check: detect obvious symlinks
        var link_buf: [std.fs.max_path_bytes]u8 = undefined;
        if (std.fs.cwd().readLink(filepath, &link_buf)) |_| {
            std.log.warn("[Cache] {s} is a symlink (possible attack), rejecting", .{filename});
            return error.NotARegularFile;
        } else |err| {
            if (err == error.NotLink) {
                // Good - it's not a symlink, continue
            } else if (err == error.FileNotFound) {
                return error.FileNotFound;
            } else {
                return error.Unexpected;
            }
        }

        // Open bytecode file with O_NOFOLLOW (defense-in-depth)
        const file = openFileNoFollow(filepath) catch |err| {
            if (err == error.SymLinkLoop) {
                std.log.warn("[Cache] {s} is a symlink (O_NOFOLLOW), rejecting", .{filename});
                return error.NotARegularFile;
            }
            if (err == error.FileNotFound) return error.FileNotFound;
            return error.Unexpected;
        };
        defer file.close();

        // PE4 FIX: Post-check - verify file type AFTER opening (closes TOCTOU gap)
        // This is the authoritative check since stat() operates on the open file descriptor
        const stat = file.stat() catch return error.Unexpected;
        if (stat.kind != .file) {
            std.log.warn("[Cache] {s} is not a regular file after open (TOCTOU attack?)", .{filename});
            return error.NotARegularFile;
        }
        if (stat.size > MAX_BYTECODE_SIZE) {
            std.log.warn("[Cache] Bytecode too large: {d} bytes", .{stat.size});
            return error.BytecodeTooLarge;
        }

        const bytes = try file.readToEndAlloc(self.allocator, MAX_BYTECODE_SIZE);
        errdefer self.allocator.free(bytes);

        // BYTECODE MAGIC VALIDATION
        if (!validateHermesMagic(bytes)) {
            std.log.warn("[Cache] Invalid Hermes bytecode magic in {s}", .{filename});
            self.allocator.free(bytes);
            // Delete corrupted files
            std.fs.cwd().deleteFile(filepath) catch {};
            std.fs.cwd().deleteFile(checksum_path) catch {};
            return error.InvalidBytecodeMagic;
        }

        // Verify checksum
        var actual_checksum: [64]u8 = undefined;
        computeChecksum(bytes, &actual_checksum);

        // CONSTANT-TIME COMPARISON for security
        if (!constantTimeEqual(&actual_checksum, &expected_checksum)) {
            std.log.warn("[Cache] Checksum mismatch for {s}, invalidating", .{filename});
            self.allocator.free(bytes);
            // Delete corrupted files
            std.fs.cwd().deleteFile(filepath) catch {};
            std.fs.cwd().deleteFile(checksum_path) catch {};
            return error.ChecksumMismatch;
        }

        // Add to LRU cache (may evict old entries)
        try self.addToLRU(source_hash, bytes, actual_checksum);

        std.log.debug("[Cache] Loaded bytecode for hash (first 8 bytes: {x}) ({d} bytes)", .{
            source_hash[0..8].*,
            bytes.len,
        });
        return bytes;
    }

    /// Store bytecode for a macro source hash (atomic with file locking)
    /// Thread-safe via mutex protection
    pub fn storeBytecode(self: *BytecodeCache, source_hash: [32]u8, bytecode: []const u8) !void {
        if (!self.validated or self.disabled.load(.acquire)) return error.CacheNotValidated;

        // BYTECODE VALIDATION: Check magic before storing
        if (!validateHermesMagic(bytecode)) {
            std.log.warn("[Cache] Refusing to cache invalid Hermes bytecode", .{});
            return error.InvalidBytecodeMagic;
        }

        // Compute checksum
        var checksum: [64]u8 = undefined;
        computeChecksum(bytecode, &checksum);

        // Store in LRU cache
        const owned = try self.allocator.dupe(u8, bytecode);
        errdefer self.allocator.free(owned);

        // PE1 FIX: Mutex protection for LRU access
        {
            self.mutex.lock();
            defer self.mutex.unlock();
            try self.addToLRU(source_hash, owned, checksum);
        }

        // Persist to disk with locking (file locks handle concurrent disk access)
        self.writeToDiskWithLock(source_hash, bytecode, &checksum) catch |err| {
            // Don't fail the whole operation if disk write fails
            // The in-memory cache is still valid
            if (err == error.DiskFull) {
                std.log.warn("[Cache] Disk full, bytecode stored in memory only", .{});
                self.disabled.store(true, .release);
            } else {
                std.log.warn("[Cache] Failed to persist bytecode: {}", .{err});
            }
        };

        std.log.debug("[Cache] Stored bytecode for hash (first 8 bytes: {x}) ({d} bytes)", .{
            source_hash[0..8].*,
            bytecode.len,
        });
    }

    /// Write bytecode to disk with file locking for concurrent safety
    /// PE1/PE3 FIXES:
    /// - Random temp file names to prevent hijacking
    /// - fsync before rename for durability
    /// - Don't delete lock files to prevent race conditions
    /// - Write checksum FIRST for better failure mode
    fn writeToDiskWithLock(self: *BytecodeCache, source_hash: [32]u8, bytecode: []const u8, checksum: *const [64]u8) !void {
        const filename = try self.hashToFilename(&source_hash, ".hbc");
        defer self.allocator.free(filename);

        const filepath = try std.fs.path.join(self.allocator, &.{ self.cache_dir, BYTECODE_DIR, filename });
        defer self.allocator.free(filepath);

        const lock_path = try std.fmt.allocPrint(self.allocator, "{s}.lock", .{filepath});
        defer self.allocator.free(lock_path);

        // ACQUIRE LOCK (per-file lock for concurrent msc/mls)
        // PE1 FIX: Lock files are persistent - don't delete on release
        // PE4 FIX: Keep lock open until AFTER renames complete
        const lock_file = std.fs.cwd().createFile(lock_path, .{
            .exclusive = false, // Don't fail if exists
            .lock = .exclusive, // Exclusive lock
        }) catch |err| {
            std.log.debug("[Cache] Failed to acquire lock: {}", .{err});
            return error.LockAcquisitionFailed;
        };
        // PE4 FIX: errdefer ensures lock is released on ANY error path
        errdefer lock_file.close();

        // PE4 FIX: Generate random suffix using system entropy (thread-safe)
        // Using getrandom instead of shared PRNG to avoid race conditions
        var random_bytes: [8]u8 = undefined;
        std.posix.getrandom(&random_bytes) catch {
            // Fallback: use timestamp + hash prefix if getrandom fails (less secure but works)
            const ts: u64 = @truncate(@as(u128, @bitCast(std.time.nanoTimestamp())));
            const hash_prefix = std.mem.readInt(u64, source_hash[0..8], .little);
            std.mem.writeInt(u64, &random_bytes, ts ^ hash_prefix, .little);
        };
        const random_suffix = std.mem.readInt(u64, &random_bytes, .little);
        const tmp_path = try std.fmt.allocPrint(self.allocator, "{s}.tmp.{x}", .{ filepath, random_suffix });
        defer self.allocator.free(tmp_path);

        const checksum_path = try std.fmt.allocPrint(self.allocator, "{s}.checksum", .{filepath});
        defer self.allocator.free(checksum_path);

        const checksum_tmp = try std.fmt.allocPrint(self.allocator, "{s}.tmp.{x}", .{ checksum_path, random_suffix });
        defer self.allocator.free(checksum_tmp);

        // PE3 FIX: Write CHECKSUM FIRST - better failure mode
        // If we crash after checksum but before bytecode, reader sees "checksum without bytecode" = cache miss
        // If we crash after bytecode but before checksum, reader sees "bytecode without checksum" = also cache miss
        // Writing checksum first is simpler because loadFromDisk reads checksum first

        // Write checksum to temp file
        {
            const file = std.fs.cwd().createFile(checksum_tmp, .{ .mode = 0o600 }) catch |err| {
                return self.handleDiskError(err);
            };
            errdefer std.fs.cwd().deleteFile(checksum_tmp) catch {};
            defer file.close();
            file.writeAll(checksum) catch |err| {
                std.fs.cwd().deleteFile(checksum_tmp) catch {};
                return self.handleDiskError(err);
            };
            // PE1 FIX: fsync before rename for durability
            file.sync() catch {};
        }

        // Write bytecode to temp file with restricted permissions
        {
            const file = std.fs.cwd().createFile(tmp_path, .{ .mode = 0o600 }) catch |err| {
                std.fs.cwd().deleteFile(checksum_tmp) catch {};
                return self.handleDiskError(err);
            };
            errdefer std.fs.cwd().deleteFile(tmp_path) catch {};
            defer file.close();
            file.writeAll(bytecode) catch |err| {
                std.fs.cwd().deleteFile(tmp_path) catch {};
                std.fs.cwd().deleteFile(checksum_tmp) catch {};
                return self.handleDiskError(err);
            };
            // PE1 FIX: fsync before rename for durability
            file.sync() catch {};
        }

        // Atomic renames: CHECKSUM FIRST, then bytecode
        // PE4 FIX: Lock held during entire rename sequence (errdefer closes on error)
        // Rationale: loadFromDisk reads checksum first, so having checksum without
        // bytecode results in clean "file not found" error, not "checksum mismatch"
        std.fs.cwd().rename(checksum_tmp, checksum_path) catch |err| {
            // errdefer will close lock_file
            std.fs.cwd().deleteFile(tmp_path) catch {};
            std.fs.cwd().deleteFile(checksum_tmp) catch {};
            return err;
        };

        std.fs.cwd().rename(tmp_path, filepath) catch |err| {
            // errdefer will close lock_file
            // Checksum is already renamed, delete it for consistency
            std.fs.cwd().deleteFile(checksum_path) catch {};
            std.fs.cwd().deleteFile(tmp_path) catch {};
            return err;
        };

        // PE4 FIX: Release lock only AFTER both renames complete successfully
        // Must close explicitly since errdefer only triggers on error
        lock_file.close();
    }

    /// Add entry to LRU cache, evicting oldest if at capacity
    /// NOTE: Caller must hold mutex lock
    fn addToLRU(self: *BytecodeCache, hash: [32]u8, bytes: []const u8, checksum: [64]u8) !void {
        // Check if already exists
        if (self.bytecode_index.getPtr(hash)) |existing| {
            // Update and move to front
            self.allocator.free(existing.*.bytes);
            existing.*.bytes = bytes;
            existing.*.checksum = checksum;
            self.moveToFront(existing.*);
            return;
        }

        // Evict if at capacity
        while (self.cache_count >= self.max_cache_entries) {
            self.evictOldest();
        }

        // Create new node
        const node = try self.allocator.create(LRUNode);
        // PE2 FIX: errdefer to prevent leak if put() fails
        errdefer self.allocator.destroy(node);

        node.* = .{
            .hash = hash,
            .bytes = bytes,
            .checksum = checksum,
            .prev = null,
            .next = self.lru_head,
        };

        // Insert into index FIRST - if this fails, errdefer cleans up node
        try self.bytecode_index.put(hash, node);

        // Insert at head (only after index insert succeeds)
        if (self.lru_head) |head| {
            head.prev = node;
        }
        self.lru_head = node;
        if (self.lru_tail == null) {
            self.lru_tail = node;
        }

        self.cache_count += 1;
    }

    /// Move node to front of LRU list
    fn moveToFront(self: *BytecodeCache, node: *LRUNode) void {
        if (self.lru_head == node) return; // Already at front

        // Remove from current position
        if (node.prev) |prev| {
            prev.next = node.next;
        }
        if (node.next) |next| {
            next.prev = node.prev;
        }
        if (self.lru_tail == node) {
            self.lru_tail = node.prev;
        }

        // Insert at front
        node.prev = null;
        node.next = self.lru_head;
        if (self.lru_head) |head| {
            head.prev = node;
        }
        self.lru_head = node;
    }

    /// Evict least recently used entry
    fn evictOldest(self: *BytecodeCache) void {
        const tail = self.lru_tail orelse return;

        // Remove from index
        _ = self.bytecode_index.remove(tail.hash);

        // Remove from list
        if (tail.prev) |prev| {
            prev.next = null;
        }
        self.lru_tail = tail.prev;
        if (self.lru_head == tail) {
            self.lru_head = null;
        }

        // Free memory
        self.allocator.free(tail.bytes);
        self.allocator.destroy(tail);
        self.cache_count -= 1;

        std.log.debug("[Cache] Evicted LRU entry, count now: {d}", .{self.cache_count});
    }

    /// Check if bytecode exists for a source hash
    /// Thread-safe via mutex protection
    pub fn hasBytecode(self: *BytecodeCache, source_hash: [32]u8) bool {
        if (!self.validated or self.disabled.load(.acquire)) return false;

        // PE1 FIX: Mutex protection for LRU access
        self.mutex.lock();
        const in_memory = self.bytecode_index.contains(source_hash);
        self.mutex.unlock();

        if (in_memory) return true;

        // Check disk (no mutex needed - file access is independent)
        const filename = self.hashToFilename(&source_hash, ".hbc") catch return false;
        defer self.allocator.free(filename);

        const filepath = std.fs.path.join(self.allocator, &.{ self.cache_dir, BYTECODE_DIR, filename }) catch return false;
        defer self.allocator.free(filepath);

        std.fs.cwd().access(filepath, .{}) catch return false;
        return true;
    }

    /// Convert SHA256 hash to filename (first 32 hex chars + extension)
    /// Caller must free the returned slice
    fn hashToFilename(self: *BytecodeCache, hash: *const [32]u8, extension: []const u8) ![]const u8 {
        const hex_chars = "0123456789abcdef";

        // PE4 FIX: Check for integer overflow in size calculation
        // 32 hex chars (from 16 bytes) + extension
        const total_len = std.math.add(usize, 32, extension.len) catch {
            return error.OutOfMemory; // Overflow would cause buffer overflow
        };

        const result = try self.allocator.alloc(u8, total_len);
        errdefer self.allocator.free(result);

        // Convert first 16 bytes to 32 hex characters
        for (hash[0..16], 0..) |byte, i| {
            result[i * 2] = hex_chars[byte >> 4];
            result[i * 2 + 1] = hex_chars[byte & 0x0f];
        }
        // Append extension
        @memcpy(result[32..], extension);
        return result;
    }

    /// Get the cache directory path
    pub fn getCacheDir(self: *BytecodeCache) []const u8 {
        return self.cache_dir;
    }

    /// Invalidate a specific entry
    /// Thread-safe via mutex protection
    pub fn invalidateEntry(self: *BytecodeCache, source_hash: [32]u8) void {
        // PE1 FIX: Mutex protection for LRU access
        self.mutex.lock();
        defer self.mutex.unlock();

        // Remove from LRU cache
        if (self.bytecode_index.fetchRemove(source_hash)) |kv| {
            const node = kv.value;

            // Remove from LRU list
            if (node.prev) |prev| {
                prev.next = node.next;
            } else {
                self.lru_head = node.next;
            }
            if (node.next) |next| {
                next.prev = node.prev;
            } else {
                self.lru_tail = node.prev;
            }

            self.allocator.free(node.bytes);
            self.allocator.destroy(node);
            self.cache_count -= 1;
        }

        // Remove from disk (mutex still held for simplicity)
        const filename = self.hashToFilename(&source_hash, ".hbc") catch return;
        defer self.allocator.free(filename);

        const filepath = std.fs.path.join(self.allocator, &.{ self.cache_dir, BYTECODE_DIR, filename }) catch return;
        defer self.allocator.free(filepath);

        const checksum_path = std.fmt.allocPrint(self.allocator, "{s}.checksum", .{filepath}) catch return;
        defer self.allocator.free(checksum_path);

        std.fs.cwd().deleteFile(filepath) catch {};
        std.fs.cwd().deleteFile(checksum_path) catch {};
    }

    /// Check if cache is operational
    pub fn isOperational(self: *BytecodeCache) bool {
        return self.validated and !self.disabled.load(.acquire);
    }
};

/// PE3 FIX: Check for path traversal sequences in a path
/// Returns true if the path contains ".." component which could be used for directory traversal
fn containsPathTraversal(path: []const u8) bool {
    // Look for ".." as a path component (not as part of "..." or ".name")
    var i: usize = 0;
    while (i < path.len) {
        // Check if we're at the start of a ".." sequence
        if (path[i] == '.') {
            // Check if next char is also '.'
            if (i + 1 < path.len and path[i + 1] == '.') {
                // Found ".." - check if it's a complete component (not "..." etc)
                const before_ok = (i == 0) or (path[i - 1] == '/') or (path[i - 1] == '\\');
                const after_idx = i + 2;
                const after_ok = (after_idx >= path.len) or (path[after_idx] == '/') or (path[after_idx] == '\\');

                if (before_ok and after_ok) {
                    // This is ".." as a complete path component - traversal detected
                    return true;
                }
            }
        }
        i += 1;
    }
    return false;
}

/// Validate Hermes bytecode magic bytes
pub fn validateHermesMagic(bytecode: []const u8) bool {
    if (bytecode.len < 4) return false;
    return bytecode[0] == HERMES_MAGIC[0] and
        bytecode[1] == HERMES_MAGIC[1] and
        bytecode[2] == HERMES_MAGIC[2] and
        bytecode[3] == HERMES_MAGIC[3];
}

/// Compute SHA256 checksum as hex string
fn computeChecksum(data: []const u8, out: *[64]u8) void {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(data);
    const digest = hasher.finalResult();

    // Convert to hex
    const hex_chars = "0123456789abcdef";
    for (digest, 0..) |byte, i| {
        out[i * 2] = hex_chars[byte >> 4];
        out[i * 2 + 1] = hex_chars[byte & 0x0f];
    }
}

/// Constant-time comparison to prevent timing attacks
fn constantTimeEqual(a: *const [64]u8, b: *const [64]u8) bool {
    var result: u8 = 0;
    for (a, b) |ai, bi| {
        result |= ai ^ bi;
    }
    return result == 0;
}

/// Open a file without following symlinks (defense-in-depth against symlink attacks)
/// On POSIX: uses O_NOFOLLOW flag, returns error.SymLinkLoop if path is a symlink
/// On Windows: falls back to regular openFile (symlinks less common in cache directories)
fn openFileNoFollow(path: []const u8) !std.fs.File {
    // Windows doesn't support O_NOFOLLOW, fall back to regular open
    if (comptime builtin.os.tag == .windows) {
        return std.fs.cwd().openFile(path, .{});
    }

    // POSIX: Convert path to null-terminated and use openat with NOFOLLOW
    if (path.len >= std.fs.max_path_bytes) {
        return error.NameTooLong;
    }
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    @memcpy(path_buf[0..path.len], path);
    path_buf[path.len] = 0;
    const path_z: [*:0]const u8 = path_buf[0..path.len :0];

    const fd = posix.openatZ(posix.AT.FDCWD, path_z, .{
        .ACCMODE = .RDONLY,
        .NOFOLLOW = true,
    }, 0) catch |err| {
        // Map POSIX errors to fs.File.OpenError equivalents
        return switch (err) {
            error.SymLinkLoop => error.SymLinkLoop, // Preserve for caller to detect
            error.FileNotFound => error.FileNotFound,
            error.AccessDenied => error.AccessDenied,
            else => error.Unexpected,
        };
    };

    return .{ .handle = fd };
}

/// Hash a file's contents for cache key (SHA256, collision-resistant)
pub fn hashFileContents(contents: []const u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(contents);
    return hasher.finalResult();
}

/// Hash a macro source (name + code) using SHA256 for collision resistance
/// Returns 256-bit hash (32 bytes)
pub fn hashMacroSource(name: []const u8, source: []const u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    // Include length prefixes to prevent hash collisions from concatenation
    var name_len_buf: [8]u8 = undefined;
    std.mem.writeInt(u64, &name_len_buf, name.len, .little);
    hasher.update(&name_len_buf);
    hasher.update(name);

    var source_len_buf: [8]u8 = undefined;
    std.mem.writeInt(u64, &source_len_buf, source.len, .little);
    hasher.update(&source_len_buf);
    hasher.update(source);

    return hasher.finalResult();
}

// ===== TESTS =====

test "BytecodeCache: init creates directories" {
    const allocator = std.testing.allocator;

    // Use temp directory
    var cache = try BytecodeCache.init(allocator, "/tmp/metascript-test-v3");
    defer cache.deinit();

    // Check directory exists
    const bytecode_dir = try std.fs.path.join(allocator, &.{ cache.cache_dir, BYTECODE_DIR });
    defer allocator.free(bytecode_dir);

    try std.fs.cwd().access(bytecode_dir, .{});
}

test "BytecodeCache: LRU eviction works" {
    const allocator = std.testing.allocator;

    // Create cache with small capacity
    var cache = try BytecodeCache.initWithCapacity(allocator, "/tmp/metascript-test-v3", 3);
    defer cache.deinit();

    // Create fake bytecode with valid Hermes magic
    var fake_bytecode: [100]u8 = undefined;
    @memcpy(fake_bytecode[0..4], &HERMES_MAGIC);

    // Add 4 entries (should evict first)
    const hash1 = hashMacroSource("macro1", "source1");
    const hash2 = hashMacroSource("macro2", "source2");
    const hash3 = hashMacroSource("macro3", "source3");
    const hash4 = hashMacroSource("macro4", "source4");

    try cache.storeBytecode(hash1, &fake_bytecode);
    try cache.storeBytecode(hash2, &fake_bytecode);
    try cache.storeBytecode(hash3, &fake_bytecode);

    try std.testing.expectEqual(@as(usize, 3), cache.cache_count);

    // This should evict hash1 (oldest)
    try cache.storeBytecode(hash4, &fake_bytecode);

    try std.testing.expectEqual(@as(usize, 3), cache.cache_count);
    try std.testing.expect(cache.bytecode_index.get(hash1) == null);
    try std.testing.expect(cache.bytecode_index.get(hash4) != null);
}

test "BytecodeCache: validates Hermes magic" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.init(allocator, "/tmp/metascript-test-v3");
    defer cache.deinit();

    // Try to store invalid bytecode
    const invalid_bytecode = "not valid hermes bytecode";
    const hash = hashMacroSource("test", "source");

    const result = cache.storeBytecode(hash, invalid_bytecode);
    try std.testing.expectError(error.InvalidBytecodeMagic, result);
}

test "validateHermesMagic: correct magic passes" {
    var valid: [100]u8 = undefined;
    @memcpy(valid[0..4], &HERMES_MAGIC);

    try std.testing.expect(validateHermesMagic(&valid));
}

test "validateHermesMagic: wrong magic fails" {
    var invalid: [100]u8 = undefined;
    invalid[0] = 0x00;
    invalid[1] = 0x00;
    invalid[2] = 0x00;
    invalid[3] = 0x00;

    try std.testing.expect(!validateHermesMagic(&invalid));
}

test "validateHermesMagic: too short fails" {
    const short = [_]u8{ 0xc6, 0x1f, 0xbc };
    try std.testing.expect(!validateHermesMagic(&short));
}

test "hashMacroSource: SHA256 returns 32 bytes" {
    const hash = hashMacroSource("derive", "function derive() {}");
    try std.testing.expectEqual(@as(usize, 32), hash.len);
}

test "hashMacroSource: different inputs produce different hashes" {
    const hash1 = hashMacroSource("macro1", "source");
    const hash2 = hashMacroSource("macro2", "source");
    const hash3 = hashMacroSource("macro1", "different");

    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash2));
    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash3));
}

test "constantTimeEqual: equal arrays" {
    var a: [64]u8 = undefined;
    var b: [64]u8 = undefined;
    @memset(&a, 0x42);
    @memset(&b, 0x42);

    try std.testing.expect(constantTimeEqual(&a, &b));
}

test "constantTimeEqual: different arrays" {
    var a: [64]u8 = undefined;
    var b: [64]u8 = undefined;
    @memset(&a, 0x42);
    @memset(&b, 0x43);

    try std.testing.expect(!constantTimeEqual(&a, &b));
}

test "computeChecksum: consistent output" {
    var checksum1: [64]u8 = undefined;
    var checksum2: [64]u8 = undefined;

    computeChecksum("test data", &checksum1);
    computeChecksum("test data", &checksum2);

    try std.testing.expectEqualSlices(u8, &checksum1, &checksum2);

    // Different data should produce different checksum
    var checksum3: [64]u8 = undefined;
    computeChecksum("different data", &checksum3);
    try std.testing.expect(!std.mem.eql(u8, &checksum1, &checksum3));
}

// ===== EDGE CASE TESTS =====

test "LRU: access moves entry to front, prevents eviction" {
    const allocator = std.testing.allocator;

    // Cache with capacity 3
    var cache = try BytecodeCache.initWithCapacity(allocator, "/tmp/metascript-test-lru", 3);
    defer cache.deinit();

    var fake_bytecode: [100]u8 = undefined;
    @memcpy(fake_bytecode[0..4], &HERMES_MAGIC);

    const hash1 = hashMacroSource("m1", "s1");
    const hash2 = hashMacroSource("m2", "s2");
    const hash3 = hashMacroSource("m3", "s3");
    const hash4 = hashMacroSource("m4", "s4");

    // Add 3 entries
    try cache.storeBytecode(hash1, &fake_bytecode);
    try cache.storeBytecode(hash2, &fake_bytecode);
    try cache.storeBytecode(hash3, &fake_bytecode);

    // Access hash1 - moves it to front
    const accessed = cache.getBytecode(hash1);
    if (accessed) |a| allocator.free(a); // PE4: caller must free

    // Add hash4 - should evict hash2 (now oldest), NOT hash1
    try cache.storeBytecode(hash4, &fake_bytecode);

    try std.testing.expect(cache.bytecode_index.get(hash1) != null); // Still there (was accessed)
    try std.testing.expect(cache.bytecode_index.get(hash2) == null); // Evicted
    try std.testing.expect(cache.bytecode_index.get(hash3) != null);
    try std.testing.expect(cache.bytecode_index.get(hash4) != null);
}

test "LRU: capacity of 1 always keeps latest" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.initWithCapacity(allocator, "/tmp/metascript-test-lru1", 1);
    defer cache.deinit();

    var fake_bytecode: [100]u8 = undefined;
    @memcpy(fake_bytecode[0..4], &HERMES_MAGIC);

    const hash1 = hashMacroSource("m1", "s1");
    const hash2 = hashMacroSource("m2", "s2");

    try cache.storeBytecode(hash1, &fake_bytecode);
    try std.testing.expectEqual(@as(usize, 1), cache.cache_count);
    try std.testing.expect(cache.bytecode_index.get(hash1) != null);

    try cache.storeBytecode(hash2, &fake_bytecode);
    try std.testing.expectEqual(@as(usize, 1), cache.cache_count);
    try std.testing.expect(cache.bytecode_index.get(hash1) == null); // Evicted
    try std.testing.expect(cache.bytecode_index.get(hash2) != null); // Latest kept
}

test "hashMacroSource: empty strings produce valid hash" {
    const hash1 = hashMacroSource("", "");
    const hash2 = hashMacroSource("", "source");
    const hash3 = hashMacroSource("name", "");

    // All should produce valid 32-byte hashes
    try std.testing.expectEqual(@as(usize, 32), hash1.len);
    try std.testing.expectEqual(@as(usize, 32), hash2.len);
    try std.testing.expectEqual(@as(usize, 32), hash3.len);

    // All should be different
    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash2));
    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash3));
    try std.testing.expect(!std.mem.eql(u8, &hash2, &hash3));
}

test "hashMacroSource: length prefix prevents collision" {
    // Without length prefix, "ab" + "cd" would hash same as "a" + "bcd"
    // Our implementation uses length prefixes to prevent this
    const hash1 = hashMacroSource("ab", "cd");
    const hash2 = hashMacroSource("a", "bcd");
    const hash3 = hashMacroSource("abc", "d");
    const hash4 = hashMacroSource("abcd", "");

    // All should be different due to length-prefixed hashing
    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash2));
    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash3));
    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash4));
    try std.testing.expect(!std.mem.eql(u8, &hash2, &hash3));
    try std.testing.expect(!std.mem.eql(u8, &hash2, &hash4));
    try std.testing.expect(!std.mem.eql(u8, &hash3, &hash4));
}

test "hashMacroSource: deterministic output" {
    const hash1 = hashMacroSource("test", "source");
    const hash2 = hashMacroSource("test", "source");

    try std.testing.expectEqualSlices(u8, &hash1, &hash2);
}

test "getBytecode: returns null for non-existent entry" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.init(allocator, "/tmp/metascript-test-miss");
    defer cache.deinit();

    const hash = hashMacroSource("nonexistent", "macro");
    const result = cache.getBytecode(hash);

    try std.testing.expect(result == null);
}

test "store and retrieve roundtrip" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.init(allocator, "/tmp/metascript-test-roundtrip");
    defer cache.deinit();

    // Create bytecode with valid magic
    var bytecode: [50]u8 = undefined;
    @memcpy(bytecode[0..4], &HERMES_MAGIC);
    @memset(bytecode[4..], 0xAB); // Fill rest with pattern

    const hash = hashMacroSource("roundtrip", "test");

    try cache.storeBytecode(hash, &bytecode);
    const retrieved = cache.getBytecode(hash);
    defer if (retrieved) |r| allocator.free(r); // PE4: caller must free

    try std.testing.expect(retrieved != null);
    try std.testing.expectEqualSlices(u8, &bytecode, retrieved.?);
}

test "update existing entry replaces content" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.init(allocator, "/tmp/metascript-test-update");
    defer cache.deinit();

    const hash = hashMacroSource("update", "test");

    // First bytecode
    var bytecode1: [50]u8 = undefined;
    @memcpy(bytecode1[0..4], &HERMES_MAGIC);
    @memset(bytecode1[4..], 0x11);

    try cache.storeBytecode(hash, &bytecode1);

    // Second bytecode with same hash (update)
    var bytecode2: [60]u8 = undefined;
    @memcpy(bytecode2[0..4], &HERMES_MAGIC);
    @memset(bytecode2[4..], 0x22);

    try cache.storeBytecode(hash, &bytecode2);

    // Should get updated bytecode
    const retrieved = cache.getBytecode(hash);
    defer if (retrieved) |r| allocator.free(r); // PE4: caller must free
    try std.testing.expect(retrieved != null);
    try std.testing.expectEqual(@as(usize, 60), retrieved.?.len);
    try std.testing.expectEqual(@as(u8, 0x22), retrieved.?[4]);
}

test "invalidateEntry: removes from memory" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.init(allocator, "/tmp/metascript-test-invalidate");
    defer cache.deinit();

    var bytecode: [50]u8 = undefined;
    @memcpy(bytecode[0..4], &HERMES_MAGIC);

    const hash = hashMacroSource("invalidate", "test");

    try cache.storeBytecode(hash, &bytecode);
    const before = cache.getBytecode(hash);
    defer if (before) |b| allocator.free(b); // PE4: caller must free
    try std.testing.expect(before != null);

    cache.invalidateEntry(hash);

    try std.testing.expect(cache.getBytecode(hash) == null);
    try std.testing.expect(!cache.bytecode_index.contains(hash));
}

test "invalidateEntry: non-existent entry is no-op" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.init(allocator, "/tmp/metascript-test-invalidate-noop");
    defer cache.deinit();

    const hash = hashMacroSource("nonexistent", "macro");

    // Should not panic or error
    cache.invalidateEntry(hash);

    try std.testing.expect(cache.getBytecode(hash) == null);
}

test "hasBytecode: checks memory cache" {
    const allocator = std.testing.allocator;
    const test_dir = "/tmp/metascript-test-hasbytecode";

    // Clean up from previous runs
    std.fs.cwd().deleteTree(test_dir) catch {};

    var cache = try BytecodeCache.init(allocator, test_dir);
    defer cache.deinit();

    var bytecode: [50]u8 = undefined;
    @memcpy(bytecode[0..4], &HERMES_MAGIC);

    const hash = hashMacroSource("hasbytecode", "test");

    try std.testing.expect(!cache.hasBytecode(hash));

    try cache.storeBytecode(hash, &bytecode);

    try std.testing.expect(cache.hasBytecode(hash));

    // Cleanup
    std.fs.cwd().deleteTree(test_dir) catch {};
}

test "isOperational: reflects cache state" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.init(allocator, "/tmp/metascript-test-operational");
    defer cache.deinit();

    // Should be operational after init
    try std.testing.expect(cache.isOperational());

    // Manually disable (using atomic operations)
    cache.disabled.store(true, .release);
    try std.testing.expect(!cache.isOperational());

    // Re-enable
    cache.disabled.store(false, .release);
    try std.testing.expect(cache.isOperational());
}

test "disabled cache: getBytecode returns null" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.init(allocator, "/tmp/metascript-test-disabled-get");
    defer cache.deinit();

    var bytecode: [50]u8 = undefined;
    @memcpy(bytecode[0..4], &HERMES_MAGIC);

    const hash = hashMacroSource("disabled", "test");
    try cache.storeBytecode(hash, &bytecode);

    // Disable cache (using atomic operations)
    cache.disabled.store(true, .release);

    // Should return null even though entry exists
    try std.testing.expect(cache.getBytecode(hash) == null);
}

test "disabled cache: storeBytecode returns error" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.init(allocator, "/tmp/metascript-test-disabled-store");
    defer cache.deinit();

    cache.disabled.store(true, .release);

    var bytecode: [50]u8 = undefined;
    @memcpy(bytecode[0..4], &HERMES_MAGIC);

    const hash = hashMacroSource("disabled", "test");
    const result = cache.storeBytecode(hash, &bytecode);

    try std.testing.expectError(error.CacheNotValidated, result);
}

test "validateHermesMagic: exactly 4 bytes passes" {
    const exact = HERMES_MAGIC;
    try std.testing.expect(validateHermesMagic(&exact));
}

test "validateHermesMagic: empty bytecode fails" {
    const empty: []const u8 = "";
    try std.testing.expect(!validateHermesMagic(empty));
}

test "validateHermesMagic: partial magic fails" {
    // Only first 2 bytes match
    const partial = [_]u8{ 0xc6, 0x1f, 0x00, 0x00, 0x00 };
    try std.testing.expect(!validateHermesMagic(&partial));
}

test "validateHermesMagic: off-by-one in each position" {
    // Each position wrong by 1
    const wrong0 = [_]u8{ 0xc7, 0x1f, 0xbc, 0x03 }; // First byte +1
    const wrong1 = [_]u8{ 0xc6, 0x20, 0xbc, 0x03 }; // Second byte +1
    const wrong2 = [_]u8{ 0xc6, 0x1f, 0xbd, 0x03 }; // Third byte +1
    const wrong3 = [_]u8{ 0xc6, 0x1f, 0xbc, 0x04 }; // Fourth byte +1

    try std.testing.expect(!validateHermesMagic(&wrong0));
    try std.testing.expect(!validateHermesMagic(&wrong1));
    try std.testing.expect(!validateHermesMagic(&wrong2));
    try std.testing.expect(!validateHermesMagic(&wrong3));
}

test "constantTimeEqual: single bit difference" {
    var a: [64]u8 = undefined;
    var b: [64]u8 = undefined;
    @memset(&a, 0x00);
    @memset(&b, 0x00);

    // Flip single bit
    b[32] = 0x01;

    try std.testing.expect(!constantTimeEqual(&a, &b));
}

test "constantTimeEqual: first byte different" {
    var a: [64]u8 = undefined;
    var b: [64]u8 = undefined;
    @memset(&a, 0xFF);
    @memset(&b, 0xFF);

    a[0] = 0x00;

    try std.testing.expect(!constantTimeEqual(&a, &b));
}

test "constantTimeEqual: last byte different" {
    var a: [64]u8 = undefined;
    var b: [64]u8 = undefined;
    @memset(&a, 0xFF);
    @memset(&b, 0xFF);

    b[63] = 0x00;

    try std.testing.expect(!constantTimeEqual(&a, &b));
}

test "computeChecksum: empty input produces valid checksum" {
    var checksum: [64]u8 = undefined;
    computeChecksum("", &checksum);

    // SHA256 of empty string is well-known
    // e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
    const expected = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
    try std.testing.expectEqualSlices(u8, expected, &checksum);
}

test "computeChecksum: known SHA256 vector" {
    var checksum: [64]u8 = undefined;
    computeChecksum("hello", &checksum);

    // SHA256("hello") = 2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
    const expected = "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824";
    try std.testing.expectEqualSlices(u8, expected, &checksum);
}

test "hashFileContents: SHA256 returns 32 bytes" {
    const hash = hashFileContents("file content");
    try std.testing.expectEqual(@as(usize, 32), hash.len);
}

test "hashFileContents: deterministic" {
    const hash1 = hashFileContents("same content");
    const hash2 = hashFileContents("same content");
    try std.testing.expectEqualSlices(u8, &hash1, &hash2);
}

test "hashFileContents: different content different hash" {
    const hash1 = hashFileContents("content A");
    const hash2 = hashFileContents("content B");
    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash2));
}

test "buildVersionString: format correct" {
    const version = buildVersionString();

    // Should contain format version, hermes version, and msc version
    try std.testing.expect(std.mem.indexOf(u8, version, CACHE_FORMAT_VERSION) != null);
    try std.testing.expect(std.mem.indexOf(u8, version, HERMES_VERSION) != null);
    try std.testing.expect(std.mem.indexOf(u8, version, MSC_VERSION) != null);

    // Should have pipe separators
    var pipe_count: usize = 0;
    for (version) |c| {
        if (c == '|') pipe_count += 1;
    }
    try std.testing.expectEqual(@as(usize, 2), pipe_count);
}

test "LRU: head and tail pointers correct after operations" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.initWithCapacity(allocator, "/tmp/metascript-test-lru-ptrs", 3);
    defer cache.deinit();

    // Initially empty
    try std.testing.expect(cache.lru_head == null);
    try std.testing.expect(cache.lru_tail == null);

    var bytecode: [50]u8 = undefined;
    @memcpy(bytecode[0..4], &HERMES_MAGIC);

    const hash1 = hashMacroSource("ptr1", "s1");
    const hash2 = hashMacroSource("ptr2", "s2");

    // Add first entry
    try cache.storeBytecode(hash1, &bytecode);
    try std.testing.expect(cache.lru_head != null);
    try std.testing.expect(cache.lru_tail != null);
    try std.testing.expect(cache.lru_head == cache.lru_tail); // Single element

    // Add second entry
    try cache.storeBytecode(hash2, &bytecode);
    try std.testing.expect(cache.lru_head != cache.lru_tail); // Two elements
    try std.testing.expect(cache.lru_head.?.next == cache.lru_tail);
    try std.testing.expect(cache.lru_tail.?.prev == cache.lru_head);
}

test "cache count stays consistent through operations" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.initWithCapacity(allocator, "/tmp/metascript-test-count", 5);
    defer cache.deinit();

    var bytecode: [50]u8 = undefined;
    @memcpy(bytecode[0..4], &HERMES_MAGIC);

    try std.testing.expectEqual(@as(usize, 0), cache.cache_count);

    // Add entries
    const hash1 = hashMacroSource("cnt1", "s1");
    const hash2 = hashMacroSource("cnt2", "s2");
    const hash3 = hashMacroSource("cnt3", "s3");

    try cache.storeBytecode(hash1, &bytecode);
    try std.testing.expectEqual(@as(usize, 1), cache.cache_count);

    try cache.storeBytecode(hash2, &bytecode);
    try std.testing.expectEqual(@as(usize, 2), cache.cache_count);

    try cache.storeBytecode(hash3, &bytecode);
    try std.testing.expectEqual(@as(usize, 3), cache.cache_count);

    // Update existing entry (should not increase count)
    try cache.storeBytecode(hash2, &bytecode);
    try std.testing.expectEqual(@as(usize, 3), cache.cache_count);

    // Invalidate entry
    cache.invalidateEntry(hash1);
    try std.testing.expectEqual(@as(usize, 2), cache.cache_count);

    // Invalidate non-existent (should not decrease)
    cache.invalidateEntry(hash1);
    try std.testing.expectEqual(@as(usize, 2), cache.cache_count);
}

// ===== DISK PERSISTENCE TESTS =====

test "disk persistence: store and reload from new cache instance" {
    const allocator = std.testing.allocator;
    const test_dir = "/tmp/metascript-test-persist";

    // Clean up from previous runs
    std.fs.cwd().deleteTree(test_dir) catch {};

    // Create bytecode with valid magic and distinctive pattern
    var bytecode: [100]u8 = undefined;
    @memcpy(bytecode[0..4], &HERMES_MAGIC);
    for (bytecode[4..], 0..) |*b, i| {
        b.* = @truncate(i); // Distinctive pattern
    }

    const hash = hashMacroSource("persist", "test");

    // Store in first cache instance
    {
        var cache1 = try BytecodeCache.init(allocator, test_dir);
        defer cache1.deinit();
        try cache1.storeBytecode(hash, &bytecode);
    }

    // Load from new cache instance
    {
        var cache2 = try BytecodeCache.init(allocator, test_dir);
        defer cache2.deinit();

        const retrieved = cache2.getBytecode(hash);
        defer if (retrieved) |r| allocator.free(r); // PE4: caller must free
        try std.testing.expect(retrieved != null);
        try std.testing.expectEqualSlices(u8, &bytecode, retrieved.?);
    }

    // Cleanup
    std.fs.cwd().deleteTree(test_dir) catch {};
}

test "disk persistence: hasBytecode finds disk entries" {
    const allocator = std.testing.allocator;
    const test_dir = "/tmp/metascript-test-has-disk";

    std.fs.cwd().deleteTree(test_dir) catch {};

    var bytecode: [50]u8 = undefined;
    @memcpy(bytecode[0..4], &HERMES_MAGIC);

    const hash = hashMacroSource("hasdisk", "test");

    // Store in first cache
    {
        var cache1 = try BytecodeCache.init(allocator, test_dir);
        defer cache1.deinit();
        try cache1.storeBytecode(hash, &bytecode);
    }

    // Check in new cache (memory empty, disk has it)
    {
        var cache2 = try BytecodeCache.init(allocator, test_dir);
        defer cache2.deinit();

        // Memory is empty, but disk should have it
        try std.testing.expect(!cache2.bytecode_index.contains(hash));
        try std.testing.expect(cache2.hasBytecode(hash));
    }

    std.fs.cwd().deleteTree(test_dir) catch {};
}

test "LRU stress: many insertions maintain invariants" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.initWithCapacity(allocator, "/tmp/metascript-test-stress", 10);
    defer cache.deinit();

    var bytecode: [50]u8 = undefined;
    @memcpy(bytecode[0..4], &HERMES_MAGIC);

    // Insert 100 entries into cache of size 10
    var i: usize = 0;
    while (i < 100) : (i += 1) {
        var name_buf: [20]u8 = undefined;
        const name = std.fmt.bufPrint(&name_buf, "stress{d}", .{i}) catch unreachable;
        const hash = hashMacroSource(name, "source");
        try cache.storeBytecode(hash, &bytecode);

        // Invariants should hold after every operation
        try std.testing.expect(cache.cache_count <= 10);
        try std.testing.expect(cache.lru_head != null);
        try std.testing.expect(cache.lru_tail != null);
    }

    // Final state should have exactly 10 entries
    try std.testing.expectEqual(@as(usize, 10), cache.cache_count);
}

test "LRU stress: random access pattern maintains consistency" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.initWithCapacity(allocator, "/tmp/metascript-test-random", 5);
    defer cache.deinit();

    var bytecode: [50]u8 = undefined;
    @memcpy(bytecode[0..4], &HERMES_MAGIC);

    // Pre-compute hashes
    var hashes: [20][32]u8 = undefined;
    for (&hashes, 0..) |*h, i| {
        var name_buf: [20]u8 = undefined;
        const name = std.fmt.bufPrint(&name_buf, "rand{d}", .{i}) catch unreachable;
        h.* = hashMacroSource(name, "source");
    }

    // Insert first 5
    for (hashes[0..5]) |hash| {
        try cache.storeBytecode(hash, &bytecode);
    }

    // Simulate random access pattern (access, insert, access, insert...)
    // PE4: getBytecode returns owned memory, must free
    if (cache.getBytecode(hashes[0])) |b| allocator.free(b); // Access first
    try cache.storeBytecode(hashes[5], &bytecode); // Insert new
    if (cache.getBytecode(hashes[2])) |b| allocator.free(b); // Access middle
    try cache.storeBytecode(hashes[6], &bytecode); // Insert new
    if (cache.getBytecode(hashes[0])) |b| allocator.free(b); // Re-access first

    // Count should still be 5
    try std.testing.expectEqual(@as(usize, 5), cache.cache_count);

    // Entry 0 should still exist (was accessed)
    try std.testing.expect(cache.bytecode_index.contains(hashes[0]));
}

test "max bytecode size constant is reasonable" {
    // MAX_BYTECODE_SIZE should be large enough for real macros
    // but not so large it allows DoS
    try std.testing.expect(MAX_BYTECODE_SIZE >= 1024 * 1024); // At least 1MB
    try std.testing.expect(MAX_BYTECODE_SIZE <= 100 * 1024 * 1024); // At most 100MB
}

test "default cache entries constant is reasonable" {
    // Should cache enough macros for a large project
    try std.testing.expect(DEFAULT_MAX_CACHE_ENTRIES >= 64);
    try std.testing.expect(DEFAULT_MAX_CACHE_ENTRIES <= 1024);
}

test "getCacheDir returns correct path" {
    const allocator = std.testing.allocator;
    const test_root = "/tmp/metascript-test-dir";

    var cache = try BytecodeCache.init(allocator, test_root);
    defer cache.deinit();

    const cache_dir = cache.getCacheDir();
    try std.testing.expect(std.mem.startsWith(u8, cache_dir, test_root));
    try std.testing.expect(std.mem.endsWith(u8, cache_dir, CACHE_DIR));
}

test "multiple invalidations don't corrupt LRU list" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.initWithCapacity(allocator, "/tmp/metascript-test-multi-inv", 5);
    defer cache.deinit();

    var bytecode: [50]u8 = undefined;
    @memcpy(bytecode[0..4], &HERMES_MAGIC);

    const hash1 = hashMacroSource("inv1", "s1");
    const hash2 = hashMacroSource("inv2", "s2");
    const hash3 = hashMacroSource("inv3", "s3");

    try cache.storeBytecode(hash1, &bytecode);
    try cache.storeBytecode(hash2, &bytecode);
    try cache.storeBytecode(hash3, &bytecode);

    // Invalidate middle entry
    cache.invalidateEntry(hash2);
    try std.testing.expectEqual(@as(usize, 2), cache.cache_count);

    // LRU list should still be valid
    try std.testing.expect(cache.lru_head != null);
    try std.testing.expect(cache.lru_tail != null);

    // Invalidate head
    cache.invalidateEntry(hash3);
    try std.testing.expectEqual(@as(usize, 1), cache.cache_count);
    try std.testing.expect(cache.lru_head == cache.lru_tail);

    // Invalidate last entry
    cache.invalidateEntry(hash1);
    try std.testing.expectEqual(@as(usize, 0), cache.cache_count);
    try std.testing.expect(cache.lru_head == null);
    try std.testing.expect(cache.lru_tail == null);
}

test "bytecode with minimum valid size" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.init(allocator, "/tmp/metascript-test-minsize");
    defer cache.deinit();

    // Minimum valid bytecode: just the magic bytes
    const min_bytecode = HERMES_MAGIC;
    const hash = hashMacroSource("min", "test");

    try cache.storeBytecode(hash, &min_bytecode);
    const retrieved = cache.getBytecode(hash);
    defer if (retrieved) |r| allocator.free(r); // PE4: caller must free

    try std.testing.expect(retrieved != null);
    try std.testing.expectEqual(@as(usize, 4), retrieved.?.len);
}

test "hash uniqueness for similar macro names" {
    // Macros with similar names should have different hashes
    const hash_derive = hashMacroSource("derive", "body");
    const hash_derived = hashMacroSource("derived", "body");
    const hash_Derive = hashMacroSource("Derive", "body");
    const hash_derive1 = hashMacroSource("derive1", "body");
    const hash__derive = hashMacroSource("_derive", "body");

    try std.testing.expect(!std.mem.eql(u8, &hash_derive, &hash_derived));
    try std.testing.expect(!std.mem.eql(u8, &hash_derive, &hash_Derive));
    try std.testing.expect(!std.mem.eql(u8, &hash_derive, &hash_derive1));
    try std.testing.expect(!std.mem.eql(u8, &hash_derive, &hash__derive));
}

test "cache handles unicode macro names" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.init(allocator, "/tmp/metascript-test-unicode");
    defer cache.deinit();

    var bytecode: [50]u8 = undefined;
    @memcpy(bytecode[0..4], &HERMES_MAGIC);

    // Unicode macro name
    const hash = hashMacroSource("派生", "body"); // "derive" in Chinese

    try cache.storeBytecode(hash, &bytecode);
    const retrieved = cache.getBytecode(hash);
    defer if (retrieved) |r| allocator.free(r); // PE4: caller must free
    try std.testing.expect(retrieved != null);
}

// ===== SECURITY TESTS (PE Round 2) =====

test "containsPathTraversal: detects .." {
    try std.testing.expect(containsPathTraversal(".."));
    try std.testing.expect(containsPathTraversal("../foo"));
    try std.testing.expect(containsPathTraversal("foo/.."));
    try std.testing.expect(containsPathTraversal("foo/../bar"));
    try std.testing.expect(containsPathTraversal("/path/to/../secret"));
}

test "containsPathTraversal: allows safe paths" {
    try std.testing.expect(!containsPathTraversal("/tmp/metascript"));
    try std.testing.expect(!containsPathTraversal("./relative/path"));
    try std.testing.expect(!containsPathTraversal(".hidden"));
    try std.testing.expect(!containsPathTraversal("file.ms"));
    try std.testing.expect(!containsPathTraversal("/path/to/project"));
    try std.testing.expect(!containsPathTraversal("")); // Empty path
}

test "containsPathTraversal: allows ... (three dots)" {
    // Three dots is weird but not path traversal
    try std.testing.expect(!containsPathTraversal("..."));
    try std.testing.expect(!containsPathTraversal("foo/..."));
    try std.testing.expect(!containsPathTraversal(".../bar"));
}

test "init rejects path traversal in project_root" {
    const allocator = std.testing.allocator;

    // These should all fail with PathTraversalDetected
    const bad_paths = [_][]const u8{
        "../secret",
        "/tmp/../etc/passwd",
        "foo/../../../etc",
        "..",
    };

    for (bad_paths) |bad_path| {
        if (BytecodeCache.init(allocator, bad_path)) |_| {
            // If init succeeded, it's an error - we expected rejection
            return error.ExpectedPathTraversalError;
        } else |err| {
            try std.testing.expectEqual(error.PathTraversalDetected, err);
        }
    }
}

test "init accepts safe project_root paths" {
    const allocator = std.testing.allocator;

    // These should all succeed
    var cache1 = try BytecodeCache.init(allocator, "/tmp/safe-project");
    defer cache1.deinit();

    var cache2 = try BytecodeCache.init(allocator, "./relative");
    defer cache2.deinit();

    var cache3 = try BytecodeCache.init(allocator, null); // Default "."
    defer cache3.deinit();
}

test "max entries capped at MAX_ALLOWED_CACHE_ENTRIES" {
    const allocator = std.testing.allocator;

    // Request more than max allowed
    var cache = try BytecodeCache.initWithCapacity(allocator, "/tmp/metascript-test-max", MAX_ALLOWED_CACHE_ENTRIES + 1000);
    defer cache.deinit();

    // Should be capped at MAX_ALLOWED_CACHE_ENTRIES
    try std.testing.expectEqual(MAX_ALLOWED_CACHE_ENTRIES, cache.max_cache_entries);
}

test "atomic disabled flag thread safety" {
    const allocator = std.testing.allocator;

    var cache = try BytecodeCache.init(allocator, "/tmp/metascript-test-atomic");
    defer cache.deinit();

    // Test atomic load/store operations
    try std.testing.expect(!cache.disabled.load(.acquire));

    cache.disabled.store(true, .release);
    try std.testing.expect(cache.disabled.load(.acquire));

    cache.disabled.store(false, .release);
    try std.testing.expect(!cache.disabled.load(.acquire));
}
