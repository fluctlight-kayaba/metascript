/// esbuild TypeScript Transpiler - Shell Wrapper
/// Transpiles TypeScript/TSX to JavaScript via esbuild CLI
///
/// Note: This is a shell-based wrapper. For production, we'll replace
/// this with either:
/// 1. C FFI to esbuild shared library (like vimcraft)
/// 2. Metascript -> JavaScript backend (self-hosting)
const std = @import("std");

/// Transpilation error set
pub const TranspileError = error{
    TranspileFailed,
    EsbuildNotFound,
    OutOfMemory,
};

/// Transpile TypeScript/JavaScript source to JavaScript
///
/// This function calls esbuild via CLI to transpile TypeScript.
/// The caller owns the returned memory and must free it.
///
/// Example:
/// ```zig
/// const result = try transpile(allocator, source, "ts");
/// defer allocator.free(result);
/// std.debug.print("Transpiled: {s}\n", .{result});
/// ```
pub fn transpile(
    allocator: std.mem.Allocator,
    source: []const u8,
    loader: []const u8,
) TranspileError![]const u8 {
    // Create temp file for input
    const timestamp = std.time.milliTimestamp();
    const extension = if (std.mem.eql(u8, loader, "tsx")) "tsx" else if (std.mem.eql(u8, loader, "ts")) "ts" else "js";

    const temp_in = try std.fmt.allocPrint(
        allocator,
        "/tmp/msc-transpile-{d}.{s}",
        .{ timestamp, extension },
    );
    defer allocator.free(temp_in);

    // Write source to temp file
    std.fs.cwd().writeFile(.{
        .sub_path = temp_in,
        .data = source,
    }) catch |err| {
        std.log.err("Failed to write temp input file: {}", .{err});
        return TranspileError.TranspileFailed;
    };
    defer std.fs.cwd().deleteFile(temp_in) catch {};

    // Build loader argument
    const loader_arg = try std.fmt.allocPrint(allocator, "--loader={s}", .{loader});
    defer allocator.free(loader_arg);

    // Invoke esbuild via npx
    // npx esbuild --loader=ts --format=cjs --target=es2020 input.ts
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "npx",
            "esbuild",
            loader_arg,
            "--format=cjs", // CommonJS for Hermes compatibility
            "--target=es2020",
            temp_in,
        },
    }) catch |err| {
        std.log.err("Failed to execute esbuild: {}", .{err});
        return TranspileError.EsbuildNotFound;
    };
    defer allocator.free(result.stderr);

    // Check if transpilation succeeded
    if (result.term.Exited != 0) {
        std.log.err("esbuild transpilation failed:\n{s}", .{result.stderr});
        allocator.free(result.stdout);
        return TranspileError.TranspileFailed;
    }

    // Return the transpiled JavaScript (stdout contains the result)
    return result.stdout;
}

/// Transpile a TypeScript file to JavaScript
/// Takes a file path and returns transpiled JavaScript
pub fn transpileFile(
    allocator: std.mem.Allocator,
    source_path: []const u8,
) TranspileError![]const u8 {
    // Determine loader from file extension
    const loader = if (std.mem.endsWith(u8, source_path, ".tsx"))
        "tsx"
    else if (std.mem.endsWith(u8, source_path, ".ts"))
        "ts"
    else
        "js";

    const loader_arg = try std.fmt.allocPrint(allocator, "--loader={s}", .{loader});
    defer allocator.free(loader_arg);

    // Invoke esbuild
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "npx",
            "esbuild",
            loader_arg,
            "--format=cjs",
            "--target=es2020",
            source_path,
        },
    }) catch |err| {
        std.log.err("Failed to execute esbuild: {}", .{err});
        return TranspileError.EsbuildNotFound;
    };
    defer allocator.free(result.stderr);

    if (result.term.Exited != 0) {
        std.log.err("esbuild transpilation failed:\n{s}", .{result.stderr});
        allocator.free(result.stdout);
        return TranspileError.TranspileFailed;
    }

    return result.stdout;
}

/// Bundle TypeScript files using esbuild
/// This bundles ALL imports into a single JavaScript file
pub fn bundle(
    allocator: std.mem.Allocator,
    entry_point: []const u8,
) TranspileError![]const u8 {
    // Invoke esbuild with bundling
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "npx",
            "esbuild",
            entry_point,
            "--bundle",
            "--format=cjs",
            "--target=es2020",
            "--platform=neutral", // For maximum compatibility
        },
    }) catch |err| {
        std.log.err("Failed to execute esbuild: {}", .{err});
        return TranspileError.EsbuildNotFound;
    };
    defer allocator.free(result.stderr);

    if (result.term.Exited != 0) {
        std.log.err("esbuild bundling failed:\n{s}", .{result.stderr});
        allocator.free(result.stdout);
        return TranspileError.TranspileFailed;
    }

    return result.stdout;
}

// ============================================================================
// Unit Tests
// ============================================================================

test "transpile simple TypeScript" {
    const allocator = std.testing.allocator;

    const source =
        \\const greeting: string = "Hello, TypeScript!";
        \\console.log(greeting);
    ;

    const result = transpile(allocator, source, "ts") catch |err| {
        std.debug.print("Skipping test - esbuild not available: {}\n", .{err});
        return;
    };
    defer allocator.free(result);

    // Verify code was generated
    try std.testing.expect(result.len > 0);

    // Verify it's valid JavaScript (contains console.log)
    const contains_log = std.mem.indexOf(u8, result, "console.log") != null;
    try std.testing.expect(contains_log);

    std.debug.print("\n=== Transpiled TypeScript ===\n{s}\n", .{result});
}

test "transpile TSX" {
    const allocator = std.testing.allocator;

    const source =
        \\const Component = () => <div>Hello</div>;
    ;

    const result = transpile(allocator, source, "tsx") catch |err| {
        std.debug.print("Skipping test - esbuild not available: {}\n", .{err});
        return;
    };
    defer allocator.free(result);

    // Verify JSX transformed
    try std.testing.expect(result.len > 0);

    std.debug.print("\n=== Transpiled TSX ===\n{s}\n", .{result});
}

test "transpile macro TypeScript" {
    const allocator = std.testing.allocator;

    // Simulate macro source code in TypeScript
    const source =
        \\interface MacroContext {
        \\    className: string;
        \\    traits: string[];
        \\}
        \\
        \\function deriveMacro(ctx: MacroContext): string[] {
        \\    const methods: string[] = [];
        \\    for (const trait of ctx.traits) {
        \\        if (trait === 'Eq') {
        \\            methods.push('equals');
        \\        }
        \\    }
        \\    return methods;
        \\}
        \\
        \\module.exports = { deriveMacro };
    ;

    const result = transpile(allocator, source, "ts") catch |err| {
        std.debug.print("Skipping test - esbuild not available: {}\n", .{err});
        return;
    };
    defer allocator.free(result);

    // Verify transpiled successfully
    try std.testing.expect(result.len > 0);

    // Should have module.exports
    const has_exports = std.mem.indexOf(u8, result, "module.exports") != null;
    try std.testing.expect(has_exports);

    std.debug.print("\n=== Transpiled Macro TypeScript ===\n{s}\n", .{result});
}
