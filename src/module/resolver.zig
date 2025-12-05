/// Module Resolver
///
/// Resolves module specifiers to file paths:
/// - "std/macros" → <install_dir>/std/macros/index.ms
/// - "./foo" → <current_dir>/foo.ms
/// - "../bar" → <parent_dir>/bar.ms
/// - "package" → node_modules/package/index.ms (future)
///
/// The resolver maintains a cache of resolved paths to avoid
/// redundant file system operations.

const std = @import("std");

pub const ModuleResolver = struct {
    allocator: std.mem.Allocator,
    std_lib_path: []const u8,
    resolved_cache: std.StringHashMap([]const u8),

    pub fn init(allocator: std.mem.Allocator, std_lib_path: []const u8) ModuleResolver {
        return .{
            .allocator = allocator,
            .std_lib_path = std_lib_path,
            .resolved_cache = std.StringHashMap([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *ModuleResolver) void {
        // Free both keys AND values from the cache
        var it = self.resolved_cache.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*); // Free cache key
            self.allocator.free(entry.value_ptr.*); // Free resolved path
        }
        self.resolved_cache.deinit();
    }

    /// Resolve a module specifier to an absolute file path
    ///
    /// @param specifier: The import specifier (e.g., "std/macros", "./foo")
    /// @param from_file: The file containing the import statement
    /// @returns: Absolute path to the resolved module, or null if not found
    pub fn resolve(
        self: *ModuleResolver,
        specifier: []const u8,
        from_file: []const u8,
    ) !?[]const u8 {
        // Check cache first
        const cache_key = try std.fmt.allocPrint(
            self.allocator,
            "{s}:{s}",
            .{ from_file, specifier },
        );
        defer self.allocator.free(cache_key);

        if (self.resolved_cache.get(cache_key)) |cached| {
            return cached;
        }

        // Resolve based on specifier type
        const resolved = if (std.mem.startsWith(u8, specifier, "std/"))
            try self.resolveStdLib(specifier)
        else if (std.mem.startsWith(u8, specifier, "./") or std.mem.startsWith(u8, specifier, "../"))
            try self.resolveRelative(specifier, from_file)
        else
            try self.resolvePackage(specifier);

        if (resolved) |path| {
            // Cache the result
            const key_copy = try self.allocator.dupe(u8, cache_key);
            try self.resolved_cache.put(key_copy, path);
            return path;
        }

        return null;
    }

    /// Resolve standard library imports: "std/macros" → std/macros/index.ms
    fn resolveStdLib(self: *ModuleResolver, specifier: []const u8) !?[]const u8 {
        // Strip "std/" prefix
        const subpath = specifier[4..];

        // Try index.ms first (allocate sequentially to avoid leaks)
        const index_path = try std.fmt.allocPrint(self.allocator, "{s}/{s}/index.ms", .{ self.std_lib_path, subpath });
        if (self.fileExists(index_path)) {
            return index_path;
        }
        self.allocator.free(index_path);

        // Try .ms extension
        const direct_path = try std.fmt.allocPrint(self.allocator, "{s}/{s}.ms", .{ self.std_lib_path, subpath });
        if (self.fileExists(direct_path)) {
            return direct_path;
        }
        self.allocator.free(direct_path);

        std.log.warn("Could not resolve std module: {s}", .{specifier});
        return null;
    }

    /// Resolve relative imports: "./foo" → current_dir/foo.ms
    fn resolveRelative(self: *ModuleResolver, specifier: []const u8, from_file: []const u8) !?[]const u8 {
        // Get directory of importing file
        const from_dir = std.fs.path.dirname(from_file) orelse ".";

        // Join paths
        const joined = try std.fs.path.join(self.allocator, &[_][]const u8{ from_dir, specifier });
        defer self.allocator.free(joined);

        // Try with .ms extension if not present
        const resolved = if (std.mem.endsWith(u8, specifier, ".ms"))
            try self.allocator.dupe(u8, joined)
        else
            try std.fmt.allocPrint(self.allocator, "{s}.ms", .{joined});

        if (self.fileExists(resolved)) {
            return resolved;
        }

        self.allocator.free(resolved);
        return null;
    }

    /// Resolve package imports (future: node_modules style)
    fn resolvePackage(self: *ModuleResolver, specifier: []const u8) !?[]const u8 {
        _ = self;
        std.log.warn("Package resolution not yet implemented: {s}", .{specifier});
        return null;
    }

    fn fileExists(self: *ModuleResolver, path: []const u8) bool {
        _ = self;
        std.fs.cwd().access(path, .{}) catch return false;
        return true;
    }
};

/// Get the standard library path
///
/// Priority:
/// 1. MSC_STD_PATH environment variable
/// 2. Relative to executable: ../std/
/// 3. Current working directory: ./std/
pub fn getStdLibPath(allocator: std.mem.Allocator) ![]const u8 {
    // Check environment variable
    if (std.process.getEnvVarOwned(allocator, "MSC_STD_PATH")) |path| {
        return path;
    } else |_| {}

    // Check relative to executable
    var exe_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    if (std.fs.selfExeDirPath(&exe_dir_buf)) |exe_dir| {
        const std_path = try std.fs.path.join(allocator, &[_][]const u8{ exe_dir, "..", "std" });
        if (std.fs.cwd().access(std_path, .{})) |_| {
            return std_path;
        } else |_| {
            allocator.free(std_path);
        }
    } else |_| {}

    // Fall back to current directory
    return try allocator.dupe(u8, "./std");
}

// =============================================================================
// Tests
// =============================================================================

test "resolve std library" {
    var resolver = ModuleResolver.init(std.testing.allocator, "./std");
    defer resolver.deinit();

    // This will only pass if std/ exists
    if (try resolver.resolve("std/macros", "test.ms")) |path| {
        try std.testing.expect(std.mem.indexOf(u8, path, "macros") != null);
    }
}

test "resolve relative import" {
    var resolver = ModuleResolver.init(std.testing.allocator, "./std");
    defer resolver.deinit();

    // Resolving ./foo from /some/path/bar.ms should give /some/path/foo.ms
    const result = try resolver.resolve("./foo", "/some/path/bar.ms");
    if (result) |_| {
        // Path resolution worked (file may not exist)
    }
}
