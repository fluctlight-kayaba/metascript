// CLI Command: build
// Vite-style build system - loads build.ms config and executes build
//
// Usage:
//   msc build                    # Build with default config
//   msc build --target=js        # Override target
//   msc build --mode=production  # Set mode for .env loading
//   msc build --watch            # Watch mode

const std = @import("std");
const builtin = @import("builtin");
const config_loader = @import("../build/config.zig");
const compile = @import("compile.zig");
const colors = @import("colors.zig");

pub const BuildOptions = struct {
    target: ?[]const u8 = null,
    mode: []const u8 = "production",
    watch: bool = false,
    minify: ?bool = null,
    sourcemap: ?bool = null,
    verbose: bool = false,
};

pub fn run(allocator: std.mem.Allocator, args: []const []const u8) !void {
    const start_time = std.time.milliTimestamp();

    // Parse CLI arguments
    var options = BuildOptions{};
    for (args) |arg| {
        if (std.mem.startsWith(u8, arg, "--target=")) {
            options.target = arg[9..];
        } else if (std.mem.startsWith(u8, arg, "--mode=")) {
            options.mode = arg[7..];
        } else if (std.mem.eql(u8, arg, "--watch") or std.mem.eql(u8, arg, "-w")) {
            options.watch = true;
        } else if (std.mem.eql(u8, arg, "--minify")) {
            options.minify = true;
        } else if (std.mem.eql(u8, arg, "--no-minify")) {
            options.minify = false;
        } else if (std.mem.eql(u8, arg, "--sourcemap")) {
            options.sourcemap = true;
        } else if (std.mem.eql(u8, arg, "--no-sourcemap")) {
            options.sourcemap = false;
        } else if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            options.verbose = true;
        }
    }

    // Load build.ms config
    std.debug.print("{s}msc{s} {s}build{s}\n\n", .{
        colors.Color.bright_cyan.code(),
        colors.Color.reset.code(),
        colors.Color.bright_white.code(),
        colors.Color.reset.code(),
    });

    const config = config_loader.loadConfig(allocator, options) catch |err| {
        std.debug.print("{s}error:{s} Failed to load build.ms: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return;
    };
    defer config.deinit(allocator);

    if (options.verbose) {
        std.debug.print("{s}Config:{s}\n", .{
            colors.dim_text.code(),
            colors.Color.reset.code(),
        });
        std.debug.print("  root: {s}\n", .{config.root});
        std.debug.print("  target: {s}\n", .{@tagName(config.build.target)});
        std.debug.print("  outDir: {s}\n", .{config.build.out_dir});
        std.debug.print("  optimize: {s}\n", .{@tagName(config.build.optimize)});
        std.debug.print("  transforms: {d}\n", .{config.transforms.len});
        std.debug.print("\n", .{});
    }

    // Execute build
    if (config.build.targets.len > 0) {
        // Multi-target build
        for (config.build.targets) |target_config| {
            try executeBuild(allocator, config, target_config, options);
        }
    } else {
        // Single target build
        try executeBuild(allocator, config, null, options);
    }

    const elapsed = std.time.milliTimestamp() - start_time;
    std.debug.print("\n{s}✓{s} Build completed in {d}ms\n", .{
        colors.success.code(),
        colors.Color.reset.code(),
        elapsed,
    });

    // Watch mode
    if (options.watch) {
        std.debug.print("\n{s}Watching for changes...{s}\n", .{
            colors.dim_text.code(),
            colors.Color.reset.code(),
        });
        // TODO: Implement file watcher
        std.debug.print("{s}warning:{s} Watch mode not yet implemented\n", .{
            colors.warning.code(),
            colors.Color.reset.code(),
        });
    }
}

fn executeBuild(
    allocator: std.mem.Allocator,
    config: config_loader.BuildConfig,
    target_override: ?config_loader.TargetConfig,
    options: BuildOptions,
) !void {
    // Determine effective settings
    const target = if (target_override) |t| t.target else config.build.target;
    const out_dir = if (target_override) |t| t.out_dir orelse config.build.out_dir else config.build.out_dir;
    const optimize = if (target_override) |t| t.optimize orelse config.build.optimize else config.build.optimize;
    const minify = options.minify orelse config.build.minify;
    const sourcemap = options.sourcemap orelse config.build.sourcemap;

    _ = optimize;
    _ = minify;
    _ = sourcemap;

    std.debug.print("{s}Building{s} {s} → {s} ({s})\n", .{
        colors.info.code(),
        colors.Color.reset.code(),
        config.root,
        out_dir,
        @tagName(target),
    });

    // Map target to backend
    const backend: compile.Backend = switch (target) {
        .native => .c,
        .js => .js,
        .erlang => .erlang,
        .wasm => .js, // WASM uses JS backend for now
    };

    // Determine output path
    const out_file = config.build.out_file orelse blk: {
        // Extract filename from root without extension
        const basename = std.fs.path.basename(config.root);
        if (std.mem.lastIndexOf(u8, basename, ".")) |dot_idx| {
            break :blk basename[0..dot_idx];
        }
        break :blk basename;
    };

    const extension = switch (backend) {
        .c => ".c",
        .js => ".js",
        .erlang => ".erl",
    };

    const output_path = try std.fmt.allocPrint(allocator, "{s}/{s}{s}", .{
        out_dir,
        out_file,
        extension,
    });
    defer allocator.free(output_path);

    // Ensure output directory exists
    std.fs.cwd().makePath(out_dir) catch |err| {
        if (err != error.PathAlreadyExists) {
            std.debug.print("{s}error:{s} Failed to create output directory: {s}\n", .{
                colors.error_color.code(),
                colors.Color.reset.code(),
                @errorName(err),
            });
            return err;
        }
    };

    // Run compilation with build config (includes transforms!)
    try compile.runWithBuildConfig(allocator, config.root, backend, output_path, true, &config);
}

pub fn printUsage() void {
    std.debug.print(
        \\Usage: msc build [options]
        \\
        \\Build project using build.ms configuration.
        \\
        \\Options:
        \\  --target=<target>    Override build target (native, js, erlang, wasm)
        \\  --mode=<mode>        Set mode for .env loading (default: production)
        \\  --watch, -w          Watch for changes and rebuild
        \\  --minify             Enable minification (JS target)
        \\  --no-minify          Disable minification
        \\  --sourcemap          Enable source maps
        \\  --no-sourcemap       Disable source maps
        \\  --verbose, -v        Verbose output
        \\
        \\Examples:
        \\  msc build                    Build with defaults
        \\  msc build --target=js        Build for JavaScript
        \\  msc build --mode=staging     Use .env.staging
        \\  msc build --watch            Watch mode
        \\
    , .{});
}
