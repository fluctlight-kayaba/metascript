/// Backend Testing Utilities
///
/// Provides helpers for testing code generation across all backends (C, JS, Erlang).
/// These utilities enable TDD for backend development: write test first, then implement.

const std = @import("std");
const testing = std.testing;

// Import compiler modules
const main_mod = @import("src");
const lexer_mod = main_mod.lexer_mod;
const parser_mod = main_mod.parser;
const ast_mod = main_mod.ast;
const typechecker_mod = main_mod.checker;

// Backend code generators (imported via main module to avoid duplicate imports)
const c_codegen = main_mod.cgen;
const js_codegen = main_mod.jsgen;
const erlang_codegen = main_mod.erlgen;

// DRC analysis (required for C backend)
const drc_mod = main_mod.drc;

// ============================================================================
// Backend Target Enum
// ============================================================================

pub const Backend = enum {
    c,
    javascript,
    erlang,

    pub fn toString(self: Backend) []const u8 {
        return switch (self) {
            .c => "C",
            .javascript => "JavaScript",
            .erlang => "Erlang",
        };
    }

    pub fn fileExtension(self: Backend) []const u8 {
        return switch (self) {
            .c => ".c",
            .javascript => ".js",
            .erlang => ".erl",
        };
    }
};

// ============================================================================
// Compilation Result
// ============================================================================

pub const CompilationResult = struct {
    source: []const u8,
    output: []const u8,
    backend: Backend,
    success: bool,
    error_message: ?[]const u8 = null,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *CompilationResult) void {
        self.allocator.free(self.output);
        if (self.error_message) |msg| {
            self.allocator.free(msg);
        }
    }
};

// ============================================================================
// Core Compilation Functions
// ============================================================================

/// Compile TypeScript/MetaScript source to specified backend
pub fn compile(
    allocator: std.mem.Allocator,
    source: []const u8,
    backend: Backend,
) !CompilationResult {
    // Parse source
    var arena = ast_mod.ASTArena.init(allocator);
    errdefer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    var lexer = try lexer_mod.Lexer.init(allocator, source, file_id);
    defer lexer.deinit();

    var parser = parser_mod.Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    const ast = try parser.parse();

    // Check for lexer errors
    if (lexer.errors.items.len > 0) {
        const err_msg = try std.fmt.allocPrint(allocator, "Lexer errors: {d} error(s)", .{lexer.errors.items.len});
        return CompilationResult{
            .source = source,
            .output = "",
            .backend = backend,
            .success = false,
            .error_message = err_msg,
            .allocator = allocator,
        };
    }

    // Check for parser errors
    if (parser.errors.items.len > 0) {
        const err_msg = try std.fmt.allocPrint(allocator, "Parser errors: {d} error(s)", .{parser.errors.items.len});
        return CompilationResult{
            .source = source,
            .output = "",
            .backend = backend,
            .success = false,
            .error_message = err_msg,
            .allocator = allocator,
        };
    }

    // Type check (required for proper codegen - member access, string concat, etc.)
    var checker = try typechecker_mod.TypeChecker.init(allocator);
    defer checker.deinit();
    _ = checker.check(ast) catch |err| {
        const err_msg = try std.fmt.allocPrint(allocator, "Type check error: {s}", .{@errorName(err)});
        return CompilationResult{
            .source = source,
            .output = "",
            .backend = backend,
            .success = false,
            .error_message = err_msg,
            .allocator = allocator,
        };
    };

    // Generate code for backend
    const output = switch (backend) {
        .c => blk: {
            // DRC analysis is required for C backend
            var drc = drc_mod.Drc.init(allocator);
            defer drc.deinit();
            drc.annotate(ast) catch {}; // Annotate AST with RC operations

            var generator = c_codegen.CGenerator.init(allocator, &drc);
            defer generator.deinit();
            break :blk try generator.generate(ast);
        },
        .javascript => blk: {
            var generator = js_codegen.JSGenerator.init(allocator);
            defer generator.deinit();
            break :blk try generator.generate(ast);
        },
        .erlang => blk: {
            var generator = try erlang_codegen.ErlangGenerator.init(allocator, "test.ms");
            defer generator.deinit();
            break :blk try generator.generate(ast);
        },
    };

    arena.deinit();

    return CompilationResult{
        .source = source,
        .output = output,
        .backend = backend,
        .success = true,  // Only true if we got here without errors
        .error_message = null,
        .allocator = allocator,
    };
}

/// Compile and expect success (returns mutable result for caller to deinit)
pub fn expectCompiles(
    allocator: std.mem.Allocator,
    source: []const u8,
    backend: Backend,
) !CompilationResult {
    const result = compile(allocator, source, backend) catch |err| {
        std.debug.print("\n❌ Expected compilation to succeed for {s} backend\n", .{backend.toString()});
        std.debug.print("   Source:\n{s}\n", .{source});
        return err;
    };
    return result;
}

/// Compile and expect failure
pub fn expectCompileFails(
    allocator: std.mem.Allocator,
    source: []const u8,
    backend: Backend,
) !void {
    const result = compile(allocator, source, backend);
    if (result) |_| {
        std.debug.print("\n❌ Expected compilation to FAIL for {s} backend\n", .{backend.toString()});
        std.debug.print("   Source:\n{s}\n", .{source});
        return error.ExpectedCompilationFailure;
    } else |_| {
        // Expected failure
        return;
    }
}

// ============================================================================
// Output Verification Helpers
// ============================================================================

/// Assert generated code contains a substring
pub fn expectContains(output: []const u8, needle: []const u8) !void {
    if (std.mem.indexOf(u8, output, needle) == null) {
        std.debug.print("\n❌ Expected output to contain: \"{s}\"\n", .{needle});
        std.debug.print("   Output:\n{s}\n", .{output});
        return error.OutputMissingExpectedString;
    }
}

/// Assert generated code does NOT contain a substring
pub fn expectNotContains(output: []const u8, needle: []const u8) !void {
    if (std.mem.indexOf(u8, output, needle) != null) {
        std.debug.print("\n❌ Expected output to NOT contain: \"{s}\"\n", .{needle});
        std.debug.print("   Output:\n{s}\n", .{output});
        return error.OutputContainsUnexpectedString;
    }
}

/// Assert generated code has a function with given name
pub fn expectHasFunction(output: []const u8, backend: Backend, function_name: []const u8) !void {
    const pattern = switch (backend) {
        // C: just check for function name with opening paren (return type varies)
        .c => try std.fmt.allocPrint(testing.allocator, " {s}(", .{function_name}),
        .javascript => try std.fmt.allocPrint(testing.allocator, "function {s}(", .{function_name}),
        .erlang => try std.fmt.allocPrint(testing.allocator, "{s}(", .{function_name}),
    };
    defer testing.allocator.free(pattern);

    try expectContains(output, pattern);
}

/// Assert generated code matches a pattern (regex-like)
pub fn expectPattern(output: []const u8, pattern: []const u8) !void {
    // Simple pattern matching for now (can be enhanced with regex later)
    try expectContains(output, pattern);
}

// ============================================================================
// Cross-Backend Comparison
// ============================================================================

/// Compile same source to all backends and return results
pub fn compileAllBackends(
    allocator: std.mem.Allocator,
    source: []const u8,
) !struct {
    c: CompilationResult,
    js: CompilationResult,
    erlang: CompilationResult,
} {
    var c_result = try compile(allocator, source, .c);
    errdefer c_result.deinit();

    var js_result = try compile(allocator, source, .javascript);
    errdefer js_result.deinit();

    const erlang_result = try compile(allocator, source, .erlang);

    return .{
        .c = c_result,
        .js = js_result,
        .erlang = erlang_result,
    };
}

/// Verify all backends produce valid output
pub fn expectAllBackendsSucceed(
    allocator: std.mem.Allocator,
    source: []const u8,
) !void {
    var results = try compileAllBackends(allocator, source);
    defer results.c.deinit();
    defer results.js.deinit();
    defer results.erlang.deinit();

    if (!results.c.success) {
        std.debug.print("\n❌ C backend failed\n", .{});
        return error.CBackendFailed;
    }
    if (!results.js.success) {
        std.debug.print("\n❌ JavaScript backend failed\n", .{});
        return error.JSBackendFailed;
    }
    if (!results.erlang.success) {
        std.debug.print("\n❌ Erlang backend failed\n", .{});
        return error.ErlangBackendFailed;
    }
}

// ============================================================================
// External Compilation (compile generated code with external tools)
// ============================================================================

pub const ExternalCompileResult = struct {
    success: bool,
    stdout: []const u8,
    stderr: []const u8,
    exit_code: u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *ExternalCompileResult) void {
        self.allocator.free(self.stdout);
        self.allocator.free(self.stderr);
    }
};

/// Compile generated C code with Zig CC (faster caching than GCC)
pub fn compileWithGCC(
    allocator: std.mem.Allocator,
    c_code: []const u8,
) !ExternalCompileResult {
    // Write to temp file
    const temp_dir = "/tmp/metascript_test";
    std.fs.makeDirAbsolute(temp_dir) catch {};

    const c_file = try std.fmt.allocPrint(allocator, "{s}/test.c", .{temp_dir});
    defer allocator.free(c_file);

    const file = try std.fs.createFileAbsolute(c_file, .{});
    defer file.close();
    try file.writeAll(c_code);

    // Compile with zig cc (excellent caching, faster rebuilds)
    // Falls back to gcc if zig is not available
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "zig", "cc", "-I", "src/runtime", "-c", c_file, "-o", "/tmp/metascript_test/test.o" },
    });

    return ExternalCompileResult{
        .success = result.term.Exited == 0,
        .stdout = result.stdout,
        .stderr = result.stderr,
        .exit_code = result.term.Exited,
        .allocator = allocator,
    };
}

/// Check generated JS with Node.js syntax checker
pub fn checkWithNode(
    allocator: std.mem.Allocator,
    js_code: []const u8,
) !ExternalCompileResult {
    const temp_dir = "/tmp/metascript_test";
    std.fs.makeDirAbsolute(temp_dir) catch {};

    const js_file = try std.fmt.allocPrint(allocator, "{s}/test.js", .{temp_dir});
    defer allocator.free(js_file);

    const file = try std.fs.createFileAbsolute(js_file, .{});
    defer file.close();
    try file.writeAll(js_code);

    // Check syntax with node --check
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "node", "--check", js_file },
    });

    return ExternalCompileResult{
        .success = result.term.Exited == 0,
        .stdout = result.stdout,
        .stderr = result.stderr,
        .exit_code = result.term.Exited,
        .allocator = allocator,
    };
}

/// Compile generated Erlang with erlc
pub fn compileWithErlc(
    allocator: std.mem.Allocator,
    erl_code: []const u8,
) !ExternalCompileResult {
    const temp_dir = "/tmp/metascript_test";
    std.fs.makeDirAbsolute(temp_dir) catch {};

    const erl_file = try std.fmt.allocPrint(allocator, "{s}/test.erl", .{temp_dir});
    defer allocator.free(erl_file);

    const file = try std.fs.createFileAbsolute(erl_file, .{});
    defer file.close();
    try file.writeAll(erl_code);

    // Compile with erlc
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "erlc", erl_file },
    });

    return ExternalCompileResult{
        .success = result.term.Exited == 0,
        .stdout = result.stdout,
        .stderr = result.stderr,
        .exit_code = result.term.Exited,
        .allocator = allocator,
    };
}

// ============================================================================
// Debug Helpers
// ============================================================================

/// Print generated code for debugging
pub fn debugPrintOutput(result: CompilationResult) void {
    std.debug.print("\n{'='repeated 60}\n", .{});
    std.debug.print("Generated {s} Code:\n", .{result.backend.toString()});
    std.debug.print("{'='repeated 60}\n", .{});
    std.debug.print("{s}\n", .{result.output});
    std.debug.print("{'='repeated 60}\n\n", .{});
}

/// Compare outputs side-by-side
pub fn debugCompareOutputs(c: []const u8, js: []const u8, erl: []const u8) void {
    std.debug.print("\n{'='repeated 80}\n", .{});
    std.debug.print("Backend Output Comparison\n", .{});
    std.debug.print("{'='repeated 80}\n\n", .{});

    std.debug.print("=== C ===\n{s}\n\n", .{c});
    std.debug.print("=== JavaScript ===\n{s}\n\n", .{js});
    std.debug.print("=== Erlang ===\n{s}\n\n", .{erl});

    std.debug.print("{'='repeated 80}\n\n", .{});
}
