// Build Executor
// Shared logic for msc build and msc run commands
//
// Handles: codegen → C compilation → (optional) execution

const std = @import("std");
const builtin = @import("builtin");
const config_mod = @import("config.zig");
const cc = @import("cc.zig");
const compile = @import("../cli/compile.zig");
const colors = @import("../cli/colors.zig");

pub const ExecuteOptions = struct {
    /// Run the compiled binary after building
    run_after_build: bool = false,

    /// Keep intermediate .c file (if false, deleted after native compile)
    keep_c: bool = false,

    /// Only emit C code, don't compile to native
    emit_c_only: bool = false,

    /// Show verbose output
    verbose: bool = false,
};

pub const ExecuteResult = struct {
    success: bool,
    output_path: []const u8,
    exe_path: ?[]const u8,
    build_time_ms: i64,
    exit_code: ?u8,
};

/// Execute the full build pipeline
pub fn execute(
    allocator: std.mem.Allocator,
    config: *const config_mod.BuildConfig,
    options: ExecuteOptions,
) !ExecuteResult {
    const start_time = std.time.milliTimestamp();

    const target = config.build.target;
    const out_dir = config.build.out_dir;

    // Determine output filename
    const out_file = config.build.out_file orelse blk: {
        const basename = std.fs.path.basename(config.root);
        if (std.mem.lastIndexOf(u8, basename, ".")) |dot_idx| {
            break :blk basename[0..dot_idx];
        }
        break :blk basename;
    };

    // Map target to backend
    const backend: compile.Backend = switch (target) {
        .native => .c,
        .js => .js,
        .erlang => .erlang,
        .wasm => .js,
    };

    const extension = switch (backend) {
        .c => ".c",
        .js => ".js",
        .erlang => ".erl",
    };

    // Build output path
    const output_path = try std.fmt.allocPrint(allocator, "{s}/{s}{s}", .{
        out_dir,
        out_file,
        extension,
    });
    errdefer allocator.free(output_path);

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

    // Run codegen pipeline
    try compile.runWithBuildConfig(allocator, config.root, backend, output_path, !options.run_after_build, config);

    var exe_path: ?[]const u8 = null;
    var exit_code: ?u8 = null;

    // For native target: compile C to executable
    if (target == .native and !options.emit_c_only) {
        if (options.verbose) {
            std.debug.print("{s}Compiling{s} to native...\n", .{
                colors.info.code(),
                colors.Color.reset.code(),
            });
        }

        const exe_extension = if (builtin.os.tag == .windows) ".exe" else "";
        exe_path = try std.fmt.allocPrint(allocator, "{s}/{s}{s}", .{
            out_dir,
            out_file,
            exe_extension,
        });

        const cc_result = try cc.compile(allocator, output_path, exe_path.?, config);
        defer cc_result.deinit(allocator);

        if (cc_result.success) {
            // Delete intermediate .c file unless keep_c
            const should_keep = options.keep_c or config.build.cc.keep_c;
            if (!should_keep) {
                std.fs.cwd().deleteFile(output_path) catch {};
            }
        } else {
            std.debug.print("{s}error:{s} Native compilation failed\n", .{
                colors.error_color.code(),
                colors.Color.reset.code(),
            });
            if (cc_result.stderr.len > 0) {
                std.debug.print("\n{s}\n", .{cc_result.stderr});
            }
            return error.CompilationFailed;
        }
    }

    const build_time = std.time.milliTimestamp() - start_time;

    // Run the executable if requested
    if (options.run_after_build and exe_path != null) {
        std.debug.print("\n{s}✓{s} Built in {d}ms\n\n", .{
            colors.success.code(),
            colors.Color.reset.code(),
            build_time,
        });

        exit_code = try runExecutable(allocator, exe_path.?);
    }

    return ExecuteResult{
        .success = true,
        .output_path = output_path,
        .exe_path = exe_path,
        .build_time_ms = build_time,
        .exit_code = exit_code,
    };
}

/// Run an executable and return exit code
fn runExecutable(allocator: std.mem.Allocator, exe_path: []const u8) !u8 {
    std.debug.print("{s}Running{s} {s}\n", .{
        colors.info.code(),
        colors.Color.reset.code(),
        exe_path,
    });
    std.debug.print("{s}─────────────────────────────{s}\n", .{
        colors.dim_text.code(),
        colors.Color.reset.code(),
    });

    var child = std.process.Child.init(&.{exe_path}, allocator);
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;
    child.stdin_behavior = .Inherit;

    child.spawn() catch |err| {
        std.debug.print("{s}error:{s} Failed to execute: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return 1;
    };

    const term = child.wait() catch |err| {
        std.debug.print("{s}error:{s} Process error: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return 1;
    };

    std.debug.print("{s}─────────────────────────────{s}\n", .{
        colors.dim_text.code(),
        colors.Color.reset.code(),
    });

    const exit_code: u8 = switch (term) {
        .Exited => |code| code,
        .Signal => |sig| blk: {
            std.debug.print("{s}✗{s} Killed by signal {d}\n", .{
                colors.error_color.code(),
                colors.Color.reset.code(),
                sig,
            });
            break :blk 128 + @as(u8, @truncate(sig));
        },
        else => blk: {
            std.debug.print("{s}✗{s} Process terminated abnormally\n", .{
                colors.error_color.code(),
                colors.Color.reset.code(),
            });
            break :blk 1;
        },
    };

    if (exit_code == 0) {
        std.debug.print("{s}✓{s} Exited with code 0\n", .{
            colors.success.code(),
            colors.Color.reset.code(),
        });
    } else {
        std.debug.print("{s}✗{s} Exited with code {d}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            exit_code,
        });
    }

    return exit_code;
}

/// Get the output filename from config (without extension)
pub fn getOutFile(config: *const config_mod.BuildConfig) []const u8 {
    return config.build.out_file orelse blk: {
        const basename = std.fs.path.basename(config.root);
        if (std.mem.lastIndexOf(u8, basename, ".")) |dot_idx| {
            break :blk basename[0..dot_idx];
        }
        break :blk basename;
    };
}
