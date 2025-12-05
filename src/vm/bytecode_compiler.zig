/// Bytecode Compiler - Compiles JavaScript to Hermes bytecode (.hbc)
///
/// Uses the hermesc CLI tool from the Hermes build.
/// Bytecode execution is ~100x faster than source evaluation.
///
/// Usage:
///   const bytecode = try compileToBytescode(allocator, js_source, "macro_name");
///   defer allocator.free(bytecode);
///   // Store in BytecodeCache for reuse

const std = @import("std");
const builtin = @import("builtin");

/// Path to hermesc compiler (relative to project root)
const HERMESC_PATH = "vendor/hermes/build/bin/hermesc";

/// Result of bytecode compilation
pub const CompileResult = struct {
    bytecode: []const u8,
    source_hash: u64,
};

/// Compilation error details
pub const CompileError = struct {
    message: []const u8,
    line: ?u32,
    column: ?u32,
};

/// Compile JavaScript source to Hermes bytecode
/// Returns owned bytecode slice that caller must free
pub fn compileToBytescode(
    allocator: std.mem.Allocator,
    source: []const u8,
    source_name: []const u8,
) ![]const u8 {
    // Create temp files for source and output
    const tmp_dir = std.fs.cwd().openDir("/tmp", .{}) catch {
        return error.TmpDirNotAccessible;
    };

    // Generate unique filenames using source hash
    var hasher = std.hash.Wyhash.init(0);
    hasher.update(source);
    hasher.update(source_name);
    const hash = hasher.final();

    var source_filename_buf: [64]u8 = undefined;
    const source_filename = std.fmt.bufPrint(&source_filename_buf, "msc_{x:0>16}.js", .{hash}) catch unreachable;

    var output_filename_buf: [64]u8 = undefined;
    const output_filename = std.fmt.bufPrint(&output_filename_buf, "msc_{x:0>16}.hbc", .{hash}) catch unreachable;

    // Write source to temp file
    {
        const source_file = try tmp_dir.createFile(source_filename, .{});
        defer source_file.close();
        try source_file.writeAll(source);
    }

    // Build full paths
    var source_path_buf: [256]u8 = undefined;
    const source_path = std.fmt.bufPrint(&source_path_buf, "/tmp/{s}", .{source_filename}) catch unreachable;

    var output_path_buf: [256]u8 = undefined;
    const output_path = std.fmt.bufPrint(&output_path_buf, "/tmp/{s}", .{output_filename}) catch unreachable;

    // Run hermesc
    const result = try runHermesc(allocator, source_path, output_path);
    defer {
        // Clean up temp source file
        tmp_dir.deleteFile(source_filename) catch {};
    }

    if (result.term.Exited != 0) {
        // Clean up on failure
        tmp_dir.deleteFile(output_filename) catch {};

        std.log.warn("[hermesc] Compilation failed for {s}:", .{source_name});
        if (result.stderr.len > 0) {
            std.log.warn("[hermesc] {s}", .{result.stderr});
        }
        allocator.free(result.stderr);
        return error.CompilationFailed;
    }
    allocator.free(result.stderr);

    // Read compiled bytecode
    const output_file = tmp_dir.openFile(output_filename, .{}) catch {
        return error.BytecodeReadFailed;
    };
    defer output_file.close();
    defer tmp_dir.deleteFile(output_filename) catch {};

    const bytecode = try output_file.readToEndAlloc(allocator, 10 * 1024 * 1024);

    std.log.debug("[hermesc] Compiled {s}: {d} bytes JS → {d} bytes HBC", .{
        source_name,
        source.len,
        bytecode.len,
    });

    return bytecode;
}

/// Run hermesc and capture output
fn runHermesc(
    allocator: std.mem.Allocator,
    source_path: []const u8,
    output_path: []const u8,
) !struct { term: std.process.Child.Term, stderr: []const u8 } {
    const argv = [_][]const u8{
        HERMESC_PATH,
        "-emit-binary",
        "-out",
        output_path,
        "-O", // Optimize
        source_path,
    };

    var child = std.process.Child.init(&argv, allocator);
    child.stderr_behavior = .Pipe;
    child.stdout_behavior = .Ignore;

    try child.spawn();

    // Read stderr for error messages
    var stderr_list = std.ArrayList(u8).init(allocator);
    errdefer stderr_list.deinit();

    if (child.stderr) |stderr| {
        try stderr.reader().readAllArrayList(&stderr_list, 64 * 1024);
    }

    const term = try child.wait();

    return .{
        .term = term,
        .stderr = try stderr_list.toOwnedSlice(),
    };
}

/// Check if hermesc is available
pub fn isHermescAvailable() bool {
    std.fs.cwd().access(HERMESC_PATH, .{}) catch return false;
    return true;
}

/// Get the path to hermesc
pub fn getHermescPath() []const u8 {
    return HERMESC_PATH;
}

// ===== TESTS =====

test "compileToBytescode: simple script" {
    if (!isHermescAvailable()) {
        std.debug.print("Skipping test: hermesc not available\n", .{});
        return;
    }

    const allocator = std.testing.allocator;

    const source = "var x = 1 + 2;";
    const bytecode = try compileToBytescode(allocator, source, "test.js");
    defer allocator.free(bytecode);

    // Verify it's valid Hermes bytecode (magic bytes)
    try std.testing.expect(bytecode.len > 0);
    // Hermes bytecode starts with magic: 0xc61fbc03
    try std.testing.expectEqual(@as(u8, 0xc6), bytecode[0]);
    try std.testing.expectEqual(@as(u8, 0x1f), bytecode[1]);
}

test "compileToBytescode: syntax error" {
    if (!isHermescAvailable()) {
        std.debug.print("Skipping test: hermesc not available\n", .{});
        return;
    }

    const allocator = std.testing.allocator;

    const source = "var x = {{{"; // Invalid syntax
    const result = compileToBytescode(allocator, source, "bad.js");

    try std.testing.expectError(error.CompilationFailed, result);
}

test "isHermescAvailable" {
    // This test just verifies the function doesn't crash
    const available = isHermescAvailable();
    std.debug.print("hermesc available: {}\n", .{available});
}
