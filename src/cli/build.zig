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
const executor = @import("../build/executor.zig");
const colors = @import("colors.zig");

pub const BuildOptions = struct {
    target: ?[]const u8 = null,
    mode: []const u8 = "production",
    watch: bool = false,
    minify: ?bool = null,
    sourcemap: ?bool = null,
    emit_c_only: bool = false,
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
        } else if (std.mem.eql(u8, arg, "--emit-c")) {
            options.emit_c_only = true;
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
    // Apply target override if provided
    var effective_config = config;
    if (target_override) |t| {
        effective_config.build.target = t.target;
        if (t.out_dir) |dir| effective_config.build.out_dir = dir;
        if (t.optimize) |opt| effective_config.build.optimize = opt;
    }

    // Print build info
    std.debug.print("{s}Building{s} {s} → {s} ({s})\n", .{
        colors.info.code(),
        colors.Color.reset.code(),
        effective_config.root,
        effective_config.build.out_dir,
        @tagName(effective_config.build.target),
    });

    // Use shared executor
    const emit_c_only = options.emit_c_only or config.build.emit_c_only;
    const result = try executor.execute(allocator, &effective_config, .{
        .run_after_build = false,
        .emit_c_only = emit_c_only,
        .keep_c = config.build.cc.keep_c,
        .verbose = options.verbose,
    });

    // Free allocated paths
    allocator.free(result.output_path);
    if (result.exe_path) |p| allocator.free(p);
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
        \\  --emit-c             Only emit C code, don't compile to native
        \\  --verbose, -v        Verbose output
        \\
        \\Examples:
        \\  msc build                    Build with defaults (native executable)
        \\  msc build --target=js        Build for JavaScript
        \\  msc build --emit-c           Generate C only (no native compile)
        \\  msc build --mode=staging     Use .env.staging
        \\  msc build --watch            Watch mode
        \\
    , .{});
}
