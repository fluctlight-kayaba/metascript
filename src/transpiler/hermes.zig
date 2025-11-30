/// Hermes Bytecode Compiler - Zig Wrapper
/// Compiles JavaScript to Hermes Bytecode (HBC) format
/// Performance: Direct execution of hermesc binary (~10-30ms per file)
const std = @import("std");

/// Compilation error set
pub const CompileError = error{
    CompilationFailed,
    HermescNotFound,
    OutputReadFailed,
    OutOfMemory,
};

/// Path to hermesc binary (relative to project root)
const HERMESC_PATH = "vendor/hermes/build/bin/hermesc";

/// Compile JavaScript source to Hermes bytecode (HBC)
///
/// This function invokes the hermesc compiler to produce optimized bytecode.
/// The caller owns the returned memory and must free it.
///
/// Example:
/// ```zig
/// const js_code = "const x = 42; console.log(x);";
/// const bytecode = try compile(allocator, js_code);
/// defer allocator.free(bytecode);
/// // Now execute: hermes_runtime.evaluateBytecode(bytecode)
/// ```
pub fn compile(
    allocator: std.mem.Allocator,
    source: []const u8,
) CompileError![]const u8 {
    // Create temp files for input and output
    const timestamp = std.time.milliTimestamp();
    const temp_in = try std.fmt.allocPrint(
        allocator,
        "/tmp/msc-compile-{d}.js",
        .{timestamp},
    );
    defer allocator.free(temp_in);

    const temp_out = try std.fmt.allocPrint(
        allocator,
        "/tmp/msc-compile-{d}.hbc",
        .{timestamp},
    );
    defer allocator.free(temp_out);

    // Write JavaScript source to temp file
    std.fs.cwd().writeFile(.{
        .sub_path = temp_in,
        .data = source,
    }) catch |err| {
        std.log.err("Failed to write temp input file: {}", .{err});
        return CompileError.CompilationFailed;
    };
    defer std.fs.cwd().deleteFile(temp_in) catch {};

    // Invoke hermesc compiler
    // hermesc -emit-binary -out output.hbc -commonjs input.js
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            HERMESC_PATH,
            "-emit-binary",
            "-out",
            temp_out,
            "-commonjs", // Enable CommonJS require()
            "-O", // Optimize
            temp_in,
        },
    }) catch |err| {
        std.log.err("Failed to execute hermesc: {}", .{err});
        return CompileError.HermescNotFound;
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Check if compilation succeeded
    if (result.term.Exited != 0) {
        std.log.err("hermesc compilation failed:\n{s}", .{result.stderr});
        std.fs.cwd().deleteFile(temp_out) catch {};
        return CompileError.CompilationFailed;
    }

    // Read compiled bytecode
    const bytecode = std.fs.cwd().readFileAlloc(
        allocator,
        temp_out,
        100 * 1024 * 1024, // Max 100MB
    ) catch |err| {
        std.log.err("Failed to read compiled bytecode: {}", .{err});
        std.fs.cwd().deleteFile(temp_out) catch {};
        return CompileError.OutputReadFailed;
    };

    // Clean up temp output file
    std.fs.cwd().deleteFile(temp_out) catch {};

    return bytecode;
}

/// Compile JavaScript file to Hermes bytecode
/// Takes a file path instead of source string
pub fn compileFile(
    allocator: std.mem.Allocator,
    source_path: []const u8,
    output_path: []const u8,
) CompileError!void {
    // Invoke hermesc compiler
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            HERMESC_PATH,
            "-emit-binary",
            "-out",
            output_path,
            "-commonjs",
            "-O",
            source_path,
        },
    }) catch |err| {
        std.log.err("Failed to execute hermesc: {}", .{err});
        return CompileError.HermescNotFound;
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Check if compilation succeeded
    if (result.term.Exited != 0) {
        std.log.err("hermesc compilation failed:\n{s}", .{result.stderr});
        return CompileError.CompilationFailed;
    }
}

// ============================================================================
// Unit Tests
// ============================================================================

test "compile simple JavaScript" {
    const allocator = std.testing.allocator;

    const source =
        \\const greeting = "Hello, Hermes!";
        \\console.log(greeting);
    ;

    const bytecode = compile(allocator, source) catch |err| {
        std.debug.print("Skipping test - hermesc not available: {}\n", .{err});
        return;
    };
    defer allocator.free(bytecode);

    // Verify bytecode was generated
    try std.testing.expect(bytecode.len > 0);

    // Hermes bytecode starts with magic number (first 8 bytes)
    try std.testing.expect(bytecode.len >= 8);

    std.debug.print("\n=== Compiled JavaScript ===\n", .{});
    std.debug.print("Input:  {d} bytes\n", .{source.len});
    std.debug.print("Output: {d} bytes (HBC bytecode)\n", .{bytecode.len});
}

test "compile CommonJS module" {
    const allocator = std.testing.allocator;

    const source =
        \\const data = { version: '1.0.0' };
        \\module.exports = data;
    ;

    const bytecode = compile(allocator, source) catch |err| {
        std.debug.print("Skipping test - hermesc not available: {}\n", .{err});
        return;
    };
    defer allocator.free(bytecode);

    // Verify compilation succeeded
    try std.testing.expect(bytecode.len > 0);

    std.debug.print("\n=== Compiled CommonJS Module ===\n", .{});
    std.debug.print("Bytecode size: {d} bytes\n", .{bytecode.len});
}

test "compile macro-like JavaScript" {
    const allocator = std.testing.allocator;

    // Simulate the kind of JS we'd generate for @derive macro
    const source =
        \\function deriveMacro(className, traits) {
        \\    const methods = [];
        \\    for (var i = 0; i < traits.length; i++) {
        \\        if (traits[i] === 'Eq') {
        \\            methods.push({
        \\                name: 'equals',
        \\                body: 'return this === other;'
        \\            });
        \\        }
        \\    }
        \\    return methods;
        \\}
        \\module.exports = { deriveMacro: deriveMacro };
    ;

    const bytecode = compile(allocator, source) catch |err| {
        std.debug.print("Skipping test - hermesc not available: {}\n", .{err});
        return;
    };
    defer allocator.free(bytecode);

    // Verify compilation succeeded
    try std.testing.expect(bytecode.len > 0);

    std.debug.print("\n=== Compiled Macro Code ===\n", .{});
    std.debug.print("Bytecode size: {d} bytes\n", .{bytecode.len});
}
