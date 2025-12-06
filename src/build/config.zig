// Build Configuration Schema
// Vite-style configuration types for build.ms
//
// Matches the JavaScript API defined in std/build/runtime.js

const std = @import("std");

/// Build target backend
pub const Target = enum {
    native, // C backend → native binary
    js, // JavaScript backend
    erlang, // Erlang/BEAM backend
    wasm, // WebAssembly (via JS backend)

    pub fn fromString(s: []const u8) ?Target {
        if (std.mem.eql(u8, s, "native")) return .native;
        if (std.mem.eql(u8, s, "js")) return .js;
        if (std.mem.eql(u8, s, "erlang")) return .erlang;
        if (std.mem.eql(u8, s, "wasm")) return .wasm;
        return null;
    }
};

/// Optimization level
pub const Optimize = enum {
    debug, // No optimization, debug symbols
    release, // Full optimization
    release_small, // Optimize for size
    release_safe, // Optimize with safety checks

    pub fn fromString(s: []const u8) ?Optimize {
        if (std.mem.eql(u8, s, "debug")) return .debug;
        if (std.mem.eql(u8, s, "release")) return .release;
        if (std.mem.eql(u8, s, "release-small")) return .release_small;
        if (std.mem.eql(u8, s, "release-safe")) return .release_safe;
        return null;
    }
};

/// JS output format
pub const JsFormat = enum {
    esm,
    cjs,
    iife,

    pub fn fromString(s: []const u8) ?JsFormat {
        if (std.mem.eql(u8, s, "esm")) return .esm;
        if (std.mem.eql(u8, s, "cjs")) return .cjs;
        if (std.mem.eql(u8, s, "iife")) return .iife;
        return null;
    }
};

/// Transform configuration
pub const TransformConfig = struct {
    name: []const u8,
    builtin: bool = true,
    path: ?[]const u8 = null,
    run_after: []const []const u8 = &.{},
    run_before: []const []const u8 = &.{},
    options: ?std.json.Value = null,
};

/// Per-target build configuration (for multi-target builds)
pub const TargetConfig = struct {
    target: Target,
    out_dir: ?[]const u8 = null,
    out_file: ?[]const u8 = null,
    optimize: ?Optimize = null,
    minify: ?bool = null,
    sourcemap: ?bool = null,
};

/// Rollup-style output options (for JS target)
pub const RollupOutput = struct {
    format: JsFormat = .esm,
    dir: []const u8,
    name: ?[]const u8 = null, // For IIFE
};

/// C compiler configuration (for native target)
pub const CCompilerConfig = struct {
    /// C compiler to use (default: auto-detect clang/gcc/cc)
    compiler: []const u8 = "cc",

    /// Additional include paths
    include_paths: []const []const u8 = &.{},

    /// Additional library paths
    lib_paths: []const []const u8 = &.{},

    /// Libraries to link
    libs: []const []const u8 = &.{},

    /// Additional compiler flags
    cflags: []const []const u8 = &.{},

    /// Additional linker flags
    ldflags: []const []const u8 = &.{},

    /// Keep intermediate .c file after compilation
    keep_c: bool = false,

    /// Enable compiler warnings
    warnings: bool = true,
};

/// Build section configuration
pub const BuildSection = struct {
    target: Target = .native,
    out_dir: []const u8 = "out",
    out_file: ?[]const u8 = null,
    optimize: Optimize = .debug,
    minify: bool = false,
    sourcemap: bool = false,

    // Multi-target support
    targets: []const TargetConfig = &.{},

    // JS-specific options
    rollup_outputs: []const RollupOutput = &.{},

    // C compiler options (native target)
    cc: CCompilerConfig = .{},

    // Skip C compilation, only generate .c file
    emit_c_only: bool = false,
};

/// Resolve section configuration
pub const ResolveSection = struct {
    /// Path aliases (like webpack)
    alias: std.StringHashMap([]const u8),

    /// File extensions to try
    extensions: []const []const u8 = &.{ ".ms", ".ts", ".js" },

    /// Package.json fields to check
    main_fields: []const []const u8 = &.{ "module", "main" },

    /// Enable Node.js compatibility aliases
    node_compat: bool = false,

    pub fn init(allocator: std.mem.Allocator) ResolveSection {
        return .{
            .alias = std.StringHashMap([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *ResolveSection) void {
        self.alias.deinit();
    }
};

/// Server section configuration (dev mode)
pub const ServerSection = struct {
    port: u16 = 3000,
    host: []const u8 = "localhost",
    open: bool = false,
    hmr: bool = true,
};

/// Test section configuration
pub const TestSection = struct {
    include: []const []const u8 = &.{ "tests/**/*.test.ms", "src/**/*.test.ms" },
    exclude: []const []const u8 = &.{"tests/fixtures/**"},
    parallel: bool = true,
    timeout: u32 = 5000,
    setup_files: []const []const u8 = &.{},
};

/// Coverage configuration
pub const CoverageSection = struct {
    enabled: bool = false,
    include: []const []const u8 = &.{"src/**/*.ms"},
    exclude: []const []const u8 = &.{"src/**/*.test.ms"},
    reporters: []const []const u8 = &.{ "text", "html" },
    thresholds: struct {
        lines: u8 = 0,
        functions: u8 = 0,
        branches: u8 = 0,
    } = .{},
};

/// Complete build configuration
pub const BuildConfig = struct {
    /// Entry point file (required)
    root: []const u8,

    /// Build options
    build: BuildSection = .{},

    /// Transform plugins
    transforms: []const TransformConfig = &.{},

    /// Module resolution
    resolve: ResolveSection,

    /// Compile-time constants
    define: std.StringHashMap([]const u8),

    /// Dev server options
    server: ServerSection = .{},

    /// Test options
    test_config: TestSection = .{},

    /// Coverage options
    coverage: CoverageSection = .{},

    /// Workspace packages (monorepo)
    workspace: []const []const u8 = &.{},

    allocator: std.mem.Allocator,

    /// Track if strings were allocated (for proper cleanup)
    owns_strings: bool = false,

    pub fn init(allocator: std.mem.Allocator) BuildConfig {
        return .{
            .root = "src/main.ms",
            .resolve = ResolveSection.init(allocator),
            .define = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
            .owns_strings = false,
        };
    }

    pub fn deinit(self: *const BuildConfig, allocator: std.mem.Allocator) void {
        // Free duplicated root string (if allocated - check by tracking ownership)
        if (self.owns_strings) {
            allocator.free(self.root);
            allocator.free(self.build.out_dir);
            if (self.build.out_file) |out_file| {
                allocator.free(out_file);
            }
        }

        // Free transforms
        for (self.transforms) |t| {
            if (t.name.len > 0) {
                allocator.free(t.name);
            }
            if (t.path) |p| {
                allocator.free(p);
            }
        }
        if (self.transforms.len > 0) {
            allocator.free(self.transforms);
        }

        // Free define keys and values
        var define = self.define;
        var it = define.iterator();
        while (it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.*);
        }
        define.deinit();

        // Free resolve aliases
        var resolve = self.resolve;
        var alias_it = resolve.alias.iterator();
        while (alias_it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.*);
        }
        resolve.deinit();
    }
};

/// Load configuration from build.ms or use defaults
pub fn loadConfig(allocator: std.mem.Allocator, options: anytype) !BuildConfig {
    const loader = @import("loader.zig");

    // Check if build.ms exists
    const build_ms_path = "build.ms";
    const file = std.fs.cwd().openFile(build_ms_path, .{}) catch |err| {
        if (err == error.FileNotFound) {
            // No build.ms - use defaults with CLI overrides
            return loadDefaultConfig(allocator, options);
        }
        return err;
    };
    file.close();

    // Try to load and parse build.ms (silently)
    return loader.loadBuildMs(allocator, build_ms_path, options) catch {
        // Fall back to defaults silently
        return loadDefaultConfig(allocator, options);
    };
}

fn loadDefaultConfig(allocator: std.mem.Allocator, options: anytype) !BuildConfig {
    var config = BuildConfig.init(allocator);

    // Apply CLI overrides
    if (@hasField(@TypeOf(options), "target")) {
        if (options.target) |target_str| {
            if (Target.fromString(target_str)) |target| {
                config.build.target = target;
            }
        }
    }

    // Look for common entry points
    const entry_points = [_][]const u8{
        "src/main.ms",
        "src/index.ms",
        "main.ms",
        "index.ms",
    };

    for (entry_points) |entry| {
        if (std.fs.cwd().access(entry, .{})) |_| {
            config.root = entry;
            break;
        } else |_| {}
    }

    std.debug.print("  {s}Entry:{s} {s}\n", .{
        "\x1b[2m",
        "\x1b[0m",
        config.root,
    });

    return config;
}

/// Parse JSON configuration from Hermes output
pub fn parseJsonConfig(allocator: std.mem.Allocator, json_str: []const u8) !BuildConfig {
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_str, .{});
    defer parsed.deinit();

    var config = BuildConfig.init(allocator);
    const root = parsed.value;

    // Parse root
    if (root.object.get("root")) |root_val| {
        if (root_val == .string) {
            config.root = try allocator.dupe(u8, root_val.string);
        }
    }

    // Parse build section
    if (root.object.get("build")) |build_val| {
        if (build_val == .object) {
            const build = build_val.object;

            if (build.get("target")) |t| {
                if (t == .string) {
                    if (Target.fromString(t.string)) |target| {
                        config.build.target = target;
                    }
                }
            }

            if (build.get("outDir")) |d| {
                if (d == .string) {
                    config.build.out_dir = try allocator.dupe(u8, d.string);
                }
            }

            if (build.get("outFile")) |f| {
                if (f == .string) {
                    config.build.out_file = try allocator.dupe(u8, f.string);
                }
            }

            if (build.get("optimize")) |o| {
                if (o == .string) {
                    if (Optimize.fromString(o.string)) |opt| {
                        config.build.optimize = opt;
                    }
                }
            }

            if (build.get("minify")) |m| {
                if (m == .bool) {
                    config.build.minify = m.bool;
                }
            }

            if (build.get("sourcemap")) |s| {
                if (s == .bool) {
                    config.build.sourcemap = s.bool;
                }
            }
        }
    }

    // Parse transforms
    if (root.object.get("transforms")) |transforms_val| {
        if (transforms_val == .array) {
            var transforms = std.ArrayList(TransformConfig).init(allocator);
            for (transforms_val.array.items) |t| {
                if (t == .object) {
                    const transform = t.object;
                    var tc = TransformConfig{
                        .name = "",
                    };

                    if (transform.get("name")) |n| {
                        if (n == .string) {
                            tc.name = try allocator.dupe(u8, n.string);
                        }
                    }

                    if (transform.get("builtin")) |b| {
                        if (b == .bool) {
                            tc.builtin = b.bool;
                        }
                    }

                    if (transform.get("path")) |p| {
                        if (p == .string) {
                            tc.path = try allocator.dupe(u8, p.string);
                        }
                    }

                    try transforms.append(tc);
                }
            }
            config.transforms = try transforms.toOwnedSlice();
        }
    }

    // Parse define
    if (root.object.get("define")) |define_val| {
        if (define_val == .object) {
            var it = define_val.object.iterator();
            while (it.next()) |entry| {
                if (entry.value_ptr.* == .string) {
                    try config.define.put(
                        try allocator.dupe(u8, entry.key_ptr.*),
                        try allocator.dupe(u8, entry.value_ptr.string),
                    );
                }
            }
        }
    }

    // Parse resolve.alias
    if (root.object.get("resolve")) |resolve_val| {
        if (resolve_val == .object) {
            if (resolve_val.object.get("alias")) |alias_val| {
                if (alias_val == .object) {
                    var it = alias_val.object.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == .string) {
                            try config.resolve.alias.put(
                                try allocator.dupe(u8, entry.key_ptr.*),
                                try allocator.dupe(u8, entry.value_ptr.string),
                            );
                        }
                    }
                }
            }

            if (resolve_val.object.get("nodeCompat")) |nc| {
                if (nc == .bool) {
                    config.resolve.node_compat = nc.bool;
                }
            }
        }
    }

    // Mark that we allocated strings that need freeing
    config.owns_strings = true;

    return config;
}

// Tests
test "Target.fromString" {
    const testing = std.testing;

    try testing.expectEqual(Target.native, Target.fromString("native").?);
    try testing.expectEqual(Target.js, Target.fromString("js").?);
    try testing.expectEqual(Target.erlang, Target.fromString("erlang").?);
    try testing.expectEqual(Target.wasm, Target.fromString("wasm").?);
    try testing.expectEqual(@as(?Target, null), Target.fromString("invalid"));
}

test "Optimize.fromString" {
    const testing = std.testing;

    try testing.expectEqual(Optimize.debug, Optimize.fromString("debug").?);
    try testing.expectEqual(Optimize.release, Optimize.fromString("release").?);
    try testing.expectEqual(Optimize.release_small, Optimize.fromString("release-small").?);
    try testing.expectEqual(@as(?Optimize, null), Optimize.fromString("invalid"));
}

test "BuildConfig.init" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var config = BuildConfig.init(allocator);
    defer config.deinit(allocator);

    try testing.expectEqualStrings("src/main.ms", config.root);
    try testing.expectEqual(Target.native, config.build.target);
    try testing.expectEqualStrings("dist", config.build.out_dir);
}

test "parseJsonConfig basic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const json =
        \\{
        \\  "root": "src/app.ms",
        \\  "build": {
        \\    "target": "js",
        \\    "outDir": "build",
        \\    "minify": true
        \\  },
        \\  "define": {
        \\    "__DEV__": "false"
        \\  }
        \\}
    ;

    var config = try parseJsonConfig(allocator, json);
    defer config.deinit(allocator);

    try testing.expectEqualStrings("src/app.ms", config.root);
    try testing.expectEqual(Target.js, config.build.target);
    try testing.expectEqualStrings("build", config.build.out_dir);
    try testing.expect(config.build.minify);
    try testing.expectEqualStrings("false", config.define.get("__DEV__").?);
}
