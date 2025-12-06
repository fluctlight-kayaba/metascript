// C Compiler Invocation
// Compiles generated C code to native executable
//
// Usage:
//   const result = try cc.compile(allocator, "out/app.c", "out/app", config);

const std = @import("std");
const config_mod = @import("config.zig");
const colors = @import("../cli/colors.zig");

pub const CompileError = error{
    CompilerNotFound,
    CompilationFailed,
    RuntimeNotFound,
    OutOfMemory,
};

pub const CompileResult = struct {
    success: bool,
    output_path: []const u8,
    stderr: []const u8,
    exit_code: u8,
    /// Whether stderr was allocated and needs to be freed
    owns_stderr: bool = false,

    pub fn deinit(self: CompileResult, allocator: std.mem.Allocator) void {
        if (self.owns_stderr and self.stderr.len > 0) {
            allocator.free(self.stderr);
        }
    }
};

/// Find the Metascript runtime directory
fn findRuntimeDir(allocator: std.mem.Allocator) ![]const u8 {
    // Try to find runtime relative to executable
    var path_buf: [4096]u8 = undefined;
    const exe_path = std.fs.selfExePath(&path_buf) catch |err| {
        std.debug.print("  {s}warning:{s} Could not get executable path: {s}\n", .{
            colors.warning.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return error.RuntimeNotFound;
    };

    const exe_dir = std.fs.path.dirname(exe_path) orelse return error.RuntimeNotFound;

    // Check common locations relative to msc binary
    const candidates = [_][]const u8{
        // Development: zig-out/bin/msc -> src/runtime
        "../../src/runtime",
        // Installed: /usr/local/bin/msc -> /usr/local/share/metascript/runtime
        "../share/metascript/runtime",
        // Same directory
        "runtime",
    };

    for (candidates) |candidate| {
        const runtime_path = try std.fs.path.join(allocator, &.{ exe_dir, candidate });
        defer allocator.free(runtime_path);

        // Check if orc.h exists
        const orc_path = try std.fs.path.join(allocator, &.{ runtime_path, "orc.h" });
        defer allocator.free(orc_path);

        if (std.fs.cwd().access(orc_path, .{})) |_| {
            return try allocator.dupe(u8, runtime_path);
        } else |_| {}
    }

    return error.RuntimeNotFound;
}

/// Find a working C compiler
fn findCompiler(preferred: []const u8) []const u8 {
    if (!std.mem.eql(u8, preferred, "cc")) {
        return preferred;
    }

    // Auto-detect: prefer clang, fall back to gcc, then cc
    const compilers = [_][]const u8{ "clang", "gcc", "cc" };
    for (compilers) |compiler| {
        // Simple check - try to run --version
        var child = std.process.Child.init(&.{ compiler, "--version" }, std.heap.page_allocator);
        child.stderr_behavior = .Ignore;
        child.stdout_behavior = .Ignore;
        _ = child.spawnAndWait() catch continue;
        return compiler;
    }
    return "cc";
}

/// Compile C code to native executable
pub fn compile(
    allocator: std.mem.Allocator,
    c_source: []const u8,
    output_path: []const u8,
    build_config: *const config_mod.BuildConfig,
) !CompileResult {
    const cc_config = build_config.build.cc;

    // Find runtime directory
    const runtime_dir = findRuntimeDir(allocator) catch |err| {
        std.debug.print("  {s}error:{s} Could not find Metascript runtime: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        std.debug.print("  {s}hint:{s} Ensure orc.h and ms_string.h are in src/runtime/\n", .{
            colors.dim_text.code(),
            colors.Color.reset.code(),
        });
        return CompileResult{
            .success = false,
            .output_path = output_path,
            .stderr = "Runtime not found",
            .exit_code = 1,
        };
    };
    defer allocator.free(runtime_dir);

    // Find compiler
    const compiler = findCompiler(cc_config.compiler);

    // Build argument list
    var args = std.ArrayList([]const u8).init(allocator);
    defer args.deinit();

    try args.append(compiler);

    // Input file
    try args.append(c_source);

    // Output file
    try args.append("-o");
    try args.append(output_path);

    // Include runtime directory
    const include_flag = try std.fmt.allocPrint(allocator, "-I{s}", .{runtime_dir});
    defer allocator.free(include_flag);
    try args.append(include_flag);

    // Optimization flags based on build mode
    switch (build_config.build.optimize) {
        .debug => {
            try args.append("-g");
            try args.append("-O0");
        },
        .release => {
            try args.append("-O3");
            try args.append("-DNDEBUG");
        },
        .release_small => {
            try args.append("-Os");
            try args.append("-DNDEBUG");
        },
        .release_safe => {
            try args.append("-O2");
            try args.append("-g");
        },
    }

    // Warnings
    if (cc_config.warnings) {
        try args.append("-Wall");
        try args.append("-Wextra");
        // Suppress some common warnings from generated code
        try args.append("-Wno-unused-parameter");
        try args.append("-Wno-unused-variable");
    }

    // Standard
    try args.append("-std=c11");

    // Math library (for floor, ceil, etc.)
    try args.append("-lm");

    // Additional include paths
    for (cc_config.include_paths) |path| {
        const flag = try std.fmt.allocPrint(allocator, "-I{s}", .{path});
        defer allocator.free(flag);
        try args.append(flag);
    }

    // Additional library paths
    for (cc_config.lib_paths) |path| {
        const flag = try std.fmt.allocPrint(allocator, "-L{s}", .{path});
        defer allocator.free(flag);
        try args.append(flag);
    }

    // Libraries to link
    for (cc_config.libs) |lib| {
        const flag = try std.fmt.allocPrint(allocator, "-l{s}", .{lib});
        defer allocator.free(flag);
        try args.append(flag);
    }

    // Additional cflags
    for (cc_config.cflags) |flag| {
        try args.append(flag);
    }

    // Additional ldflags
    for (cc_config.ldflags) |flag| {
        try args.append(flag);
    }

    // Run compiler
    var child = std.process.Child.init(args.items, allocator);
    child.stderr_behavior = .Pipe;
    child.stdout_behavior = .Pipe;

    child.spawn() catch |err| {
        std.debug.print("  {s}error:{s} Failed to spawn compiler '{s}': {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            compiler,
            @errorName(err),
        });
        return CompileResult{
            .success = false,
            .output_path = output_path,
            .stderr = "Compiler spawn failed",
            .exit_code = 1,
        };
    };

    // Read stderr for error messages
    var stderr_content: []const u8 = "";
    var owns_stderr = false;
    if (child.stderr) |stderr| {
        stderr_content = stderr.reader().readAllAlloc(allocator, 64 * 1024) catch "";
        owns_stderr = stderr_content.len > 0; // Only owned if actually allocated content
    }

    const term = child.wait() catch |err| {
        std.debug.print("  {s}error:{s} Compiler process error: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return CompileResult{
            .success = false,
            .output_path = output_path,
            .stderr = "Compiler wait failed",
            .exit_code = 1,
        };
    };

    const exit_code: u8 = switch (term) {
        .Exited => |code| code,
        else => 1,
    };

    return CompileResult{
        .success = exit_code == 0,
        .output_path = output_path,
        .stderr = stderr_content,
        .exit_code = exit_code,
        .owns_stderr = owns_stderr,
    };
}

/// Print compilation command for debugging
pub fn printCommand(args: []const []const u8) void {
    std.debug.print("  {s}$", .{colors.dim_text.code()});
    for (args) |arg| {
        std.debug.print(" {s}", .{arg});
    }
    std.debug.print("{s}\n", .{colors.Color.reset.code()});
}
