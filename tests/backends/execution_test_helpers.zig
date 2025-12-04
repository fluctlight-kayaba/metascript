/// Execution Testing - Phase 2
///
/// Tests that generated code:
/// 1. Compiles with external tools (gcc, node, erlc)
/// 2. Executes correctly
/// 3. Produces expected output
/// 4. Embedded unit tests pass

const std = @import("std");
const testing = std.testing;
const helpers = @import("backend_test_helpers.zig");

/// Result of compiling and executing generated code
pub const ExecutionResult = struct {
    compiled: bool,
    executed: bool,
    exit_code: i32,
    stdout: []const u8,
    stderr: []const u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *ExecutionResult) void {
        self.allocator.free(self.stdout);
        self.allocator.free(self.stderr);
    }

    pub fn success(self: ExecutionResult) bool {
        return self.compiled and self.executed and self.exit_code == 0;
    }
};

// ============================================================================
// C Backend Execution
// ============================================================================

/// Compile MetaScript to C, then compile with gcc and execute
pub fn compileAndRunC(
    allocator: std.mem.Allocator,
    source: []const u8,
) !ExecutionResult {
    // Step 1: Generate C code
    var compile_result = try helpers.compile(allocator, source, .c);
    defer compile_result.deinit();

    // Step 2: Write to temp file
    const temp_dir = "/tmp/metascript_exec_test";
    try std.fs.makeDirAbsolute(temp_dir);

    const c_file = try std.fmt.allocPrint(allocator, "{s}/test.c", .{temp_dir});
    defer allocator.free(c_file);

    const out_file = try std.fmt.allocPrint(allocator, "{s}/test", .{temp_dir});
    defer allocator.free(out_file);

    {
        const file = try std.fs.createFileAbsolute(c_file, .{});
        defer file.close();
        try file.writeAll(compile_result.output);
    }

    // Step 3: Compile with gcc
    const gcc_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "gcc", c_file, "-o", out_file, "-lm" },
    });
    defer allocator.free(gcc_result.stdout);
    defer allocator.free(gcc_result.stderr);

    if (gcc_result.term.Exited != 0) {
        return ExecutionResult{
            .compiled = false,
            .executed = false,
            .exit_code = @intCast(gcc_result.term.Exited),
            .stdout = try allocator.dupe(u8, gcc_result.stdout),
            .stderr = try allocator.dupe(u8, gcc_result.stderr),
            .allocator = allocator,
        };
    }

    // Step 4: Execute
    const exec_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{out_file},
    });

    return ExecutionResult{
        .compiled = true,
        .executed = true,
        .exit_code = @intCast(exec_result.term.Exited),
        .stdout = exec_result.stdout,
        .stderr = exec_result.stderr,
        .allocator = allocator,
    };
}

// ============================================================================
// JavaScript Backend Execution
// ============================================================================

/// Compile MetaScript to JS, then run with Node.js
pub fn compileAndRunJS(
    allocator: std.mem.Allocator,
    source: []const u8,
) !ExecutionResult {
    // Step 1: Generate JS code
    var compile_result = try helpers.compile(allocator, source, .javascript);
    defer compile_result.deinit();

    // Step 2: Write to temp file
    const temp_dir = "/tmp/metascript_exec_test";
    try std.fs.makeDirAbsolute(temp_dir);

    const js_file = try std.fmt.allocPrint(allocator, "{s}/test.js", .{temp_dir});
    defer allocator.free(js_file);

    {
        const file = try std.fs.createFileAbsolute(js_file, .{});
        defer file.close();
        try file.writeAll(compile_result.output);
    }

    // Step 3: Check syntax with node --check
    const check_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "node", "--check", js_file },
    });
    defer allocator.free(check_result.stdout);
    defer allocator.free(check_result.stderr);

    if (check_result.term.Exited != 0) {
        return ExecutionResult{
            .compiled = false,
            .executed = false,
            .exit_code = @intCast(check_result.term.Exited),
            .stdout = try allocator.dupe(u8, check_result.stdout),
            .stderr = try allocator.dupe(u8, check_result.stderr),
            .allocator = allocator,
        };
    }

    // Step 4: Execute with node
    const exec_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "node", js_file },
    });

    return ExecutionResult{
        .compiled = true,
        .executed = true,
        .exit_code = @intCast(exec_result.term.Exited),
        .stdout = exec_result.stdout,
        .stderr = exec_result.stderr,
        .allocator = allocator,
    };
}

// ============================================================================
// Erlang Backend Execution
// ============================================================================

/// Compile MetaScript to Erlang, compile with erlc, and execute
pub fn compileAndRunErlang(
    allocator: std.mem.Allocator,
    source: []const u8,
) !ExecutionResult {
    // Step 1: Generate Erlang code
    var compile_result = try helpers.compile(allocator, source, .erlang);
    defer compile_result.deinit();

    // Step 2: Write to temp file
    const temp_dir = "/tmp/metascript_exec_test";
    try std.fs.makeDirAbsolute(temp_dir);

    const erl_file = try std.fmt.allocPrint(allocator, "{s}/test.erl", .{temp_dir});
    defer allocator.free(erl_file);

    {
        const file = try std.fs.createFileAbsolute(erl_file, .{});
        defer file.close();
        try file.writeAll(compile_result.output);
    }

    // Step 3: Compile with erlc
    const erlc_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "erlc", "-o", temp_dir, erl_file },
    });
    defer allocator.free(erlc_result.stdout);
    defer allocator.free(erlc_result.stderr);

    if (erlc_result.term.Exited != 0) {
        return ExecutionResult{
            .compiled = false,
            .executed = false,
            .exit_code = @intCast(erlc_result.term.Exited),
            .stdout = try allocator.dupe(u8, erlc_result.stdout),
            .stderr = try allocator.dupe(u8, erlc_result.stderr),
            .allocator = allocator,
        };
    }

    // Step 4: Execute with erl
    // Note: Need to call a specific function - assume main/0 or test/0
    const exec_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "erl", "-noshell", "-pa", temp_dir, "-s", "test", "main", "-s", "init", "stop" },
    });

    return ExecutionResult{
        .compiled = true,
        .executed = true,
        .exit_code = @intCast(exec_result.term.Exited),
        .stdout = exec_result.stdout,
        .stderr = exec_result.stderr,
        .allocator = allocator,
    };
}

// ============================================================================
// Output Verification
// ============================================================================

/// Verify execution output contains expected string
pub fn expectOutput(result: ExecutionResult, expected: []const u8) !void {
    if (!result.success()) {
        std.debug.print("\n❌ Execution failed:\n", .{});
        std.debug.print("   Exit code: {d}\n", .{result.exit_code});
        std.debug.print("   Stdout: {s}\n", .{result.stdout});
        std.debug.print("   Stderr: {s}\n", .{result.stderr});
        return error.ExecutionFailed;
    }

    if (std.mem.indexOf(u8, result.stdout, expected) == null) {
        std.debug.print("\n❌ Expected output not found:\n", .{});
        std.debug.print("   Expected: {s}\n", .{expected});
        std.debug.print("   Got: {s}\n", .{result.stdout});
        return error.OutputMismatch;
    }
}

/// Verify execution output matches exactly
pub fn expectExactOutput(result: ExecutionResult, expected: []const u8) !void {
    if (!result.success()) {
        std.debug.print("\n❌ Execution failed:\n", .{});
        std.debug.print("   Exit code: {d}\n", .{result.exit_code});
        std.debug.print("   Stdout: {s}\n", .{result.stdout});
        std.debug.print("   Stderr: {s}\n", .{result.stderr});
        return error.ExecutionFailed;
    }

    if (!std.mem.eql(u8, result.stdout, expected)) {
        std.debug.print("\n❌ Output mismatch:\n", .{});
        std.debug.print("   Expected: {s}\n", .{expected});
        std.debug.print("   Got: {s}\n", .{result.stdout});
        return error.OutputMismatch;
    }
}

/// Verify execution succeeded with exit code 0
pub fn expectSuccess(result: ExecutionResult) !void {
    if (!result.success()) {
        std.debug.print("\n❌ Execution failed:\n", .{});
        std.debug.print("   Compiled: {}\n", .{result.compiled});
        std.debug.print("   Executed: {}\n", .{result.executed});
        std.debug.print("   Exit code: {d}\n", .{result.exit_code});
        std.debug.print("   Stderr: {s}\n", .{result.stderr});
        return error.ExecutionFailed;
    }
}
