// CLI Command: run
// Build using build.ms and execute the resulting binary
//
// Usage:
//   msc run              # Build and run (uses build.ms)
//   msc run src/main.ms  # Build specific file and run

const std = @import("std");
const config_loader = @import("../build/config.zig");
const executor = @import("../build/executor.zig");
const colors = @import("colors.zig");

pub fn run(allocator: std.mem.Allocator, args: []const []const u8) !void {
    // Check for explicit file argument vs build.ms mode
    var explicit_file: ?[]const u8 = null;
    for (args) |arg| {
        if (!std.mem.startsWith(u8, arg, "-")) {
            explicit_file = arg;
            break;
        }
    }

    // Load build.ms config (or use defaults)
    var config = config_loader.loadConfig(allocator, .{}) catch |err| {
        std.debug.print("{s}error:{s} Failed to load config: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return;
    };
    defer config.deinit(allocator);

    // Override entry point if explicit file provided
    if (explicit_file) |file| {
        // Free the old root if it was allocated
        if (config.owns_strings) {
            allocator.free(config.root);
            // Dupe the new value to maintain ownership consistency
            config.root = try allocator.dupe(u8, file);
        } else {
            // Config doesn't own strings, just point to CLI arg
            config.root = file;
        }
    }

    // Force native target for run command
    config.build.target = .native;

    // Cargo-style: show what we're compiling
    std.debug.print("{s}Compiling{s} {s}\n", .{
        colors.info.code(),
        colors.Color.reset.code(),
        config.root,
    });

    // Use shared executor with run_after_build = true
    const result = executor.execute(allocator, &config, .{
        .run_after_build = true,
        .keep_c = false, // Always clean up for run
        .emit_c_only = false,
        .verbose = false,
    }) catch |err| {
        std.debug.print("{s}error:{s} Build failed: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return;
    };

    // Free allocated paths
    allocator.free(result.output_path);
    if (result.exe_path) |p| allocator.free(p);
}

pub fn printUsage() void {
    std.debug.print(
        \\Usage: msc run [file] [options]
        \\
        \\Build and run a Metascript program.
        \\
        \\If no file is specified, uses build.ms configuration.
        \\If a file is specified, compiles and runs that file directly.
        \\
        \\Examples:
        \\  msc run                    Build and run (uses build.ms)
        \\  msc run src/main.ms        Build and run specific file
        \\
    , .{});
}
