/// Macro VM - Executes Metascript macros on Hermes runtime
///
/// This is the compile-time VM that runs macro code (TypeScript)
/// to transform AST nodes.
///
/// Usage:
///   var vm = try MacroVM.init(allocator, arena);
///   defer vm.deinit();
///
///   // Execute @derive(Eq) on a class
///   try vm.executeMacro("derive", &[_][]const u8{"Eq"}, class_node);
///
/// Bytecode Mode (Fast):
///   // Pre-compiled macros execute in ~0.2ms vs ~45ms for source
///   try vm.executeBytecode(bytecode, "macro.hbc", target_node);

const std = @import("std");
const ast = @import("../ast/ast.zig");
const ast_api = @import("ast_api.zig");
const c_api = @import("c_api.zig");
const c = c_api.c;

pub const MacroVM = struct {
    allocator: std.mem.Allocator,
    runtime: *c.MSHermesRuntime,
    ast_ctx: *ast_api.ASTContext,

    /// Built-in macro implementations (TypeScript source)
    const BUILTIN_MACROS = struct {
        /// @derive(Eq) - generates equals() method
        pub const derive_eq =
            \\// @derive(Eq) macro implementation
            \\(function() {
            \\    const props = target.properties;
            \\    if (props.length === 0) return;
            \\
            \\    // Build: this.prop1 === other.prop1 && this.prop2 === other.prop2 ...
            \\    let expr = null;
            \\    for (const prop of props) {
            \\        const thisAccess = ast.createMemberExpr(
            \\            ast.createIdentifier("this"),
            \\            ast.createIdentifier(prop)
            \\        );
            \\        const otherAccess = ast.createMemberExpr(
            \\            ast.createIdentifier("other"),
            \\            ast.createIdentifier(prop)
            \\        );
            \\        const cmp = ast.createBinaryExpr("===", thisAccess, otherAccess);
            \\
            \\        if (expr === null) {
            \\            expr = cmp;
            \\        } else {
            \\            expr = ast.createBinaryExpr("&&", expr, cmp);
            \\        }
            \\    }
            \\
            \\    // Create: return <expr>;
            \\    const returnStmt = ast.createReturnStmt(expr);
            \\    const body = ast.createBlock([returnStmt]);
            \\
            \\    // Create: equals(other: ClassName): boolean { ... }
            \\    const method = ast.createMethod("equals", body);
            \\    target.addMethod(method);
            \\})();
        ;

        /// @derive(Hash) - generates hash() method
        pub const derive_hash =
            \\// @derive(Hash) macro implementation
            \\(function() {
            \\    const props = target.properties;
            \\    // TODO: Implement hash generation
            \\    const method = ast.createMethod("hash", ast.createBlock([]));
            \\    target.addMethod(method);
            \\})();
        ;
    };

    pub fn init(allocator: std.mem.Allocator, arena: *ast.ASTArena) !MacroVM {
        // Create Hermes runtime
        const runtime = c.ms_hermes_create() orelse {
            return error.HermesInitFailed;
        };

        // Create AST context
        const ast_ctx = try allocator.create(ast_api.ASTContext);
        ast_ctx.* = .{
            .arena = arena,
            .allocator = allocator,
            .current_node = null,
        };

        // Register AST APIs
        ast_api.register(runtime, ast_ctx);

        // Register console.log for debugging
        c.ms_hermes_register_function(runtime, "console_log", consoleLog, null);

        return .{
            .allocator = allocator,
            .runtime = runtime,
            .ast_ctx = ast_ctx,
        };
    }

    pub fn deinit(self: *MacroVM) void {
        c.ms_hermes_destroy(self.runtime);
        self.allocator.destroy(self.ast_ctx);
    }

    /// Execute a macro on a target node
    ///
    /// @param macro_name: e.g., "derive"
    /// @param args: e.g., ["Eq", "Hash"]
    /// @param target: The AST node to transform (e.g., class declaration)
    pub fn executeMacro(
        self: *MacroVM,
        macro_name: []const u8,
        args: []const []const u8,
        target: *ast.Node,
    ) !void {
        // Set current target
        self.ast_ctx.current_node = target;
        defer self.ast_ctx.current_node = null;

        // Find and execute macro for each argument
        if (std.mem.eql(u8, macro_name, "derive")) {
            for (args) |trait| {
                try self.executeDeriveTrail(trait);
            }
        } else {
            std.log.warn("Unknown macro: @{s}", .{macro_name});
        }
    }

    fn executeDeriveTrail(self: *MacroVM, trait: []const u8) !void {
        const source = if (std.mem.eql(u8, trait, "Eq"))
            BUILTIN_MACROS.derive_eq
        else if (std.mem.eql(u8, trait, "Hash"))
            BUILTIN_MACROS.derive_hash
        else {
            std.log.warn("Unknown trait: {s}", .{trait});
            return;
        };

        // Execute the macro code
        const result = c.ms_hermes_eval(
            self.runtime,
            source.ptr,
            source.len,
            "derive_macro.js",
        );

        // Check for errors
        if (c.ms_hermes_has_exception(self.runtime)) {
            const msg = c.ms_hermes_get_exception(self.runtime);
            if (msg) |m| {
                std.log.err("Macro error: {s}", .{std.mem.span(m)});
            }
            c.ms_hermes_clear_exception(self.runtime);
            return error.MacroExecutionFailed;
        }

        if (result) |r| {
            c.ms_value_destroy(r);
        }
    }

    /// Execute arbitrary macro source code (JavaScript)
    pub fn executeSource(self: *MacroVM, source: []const u8, target: *ast.Node) !void {
        self.ast_ctx.current_node = target;
        defer self.ast_ctx.current_node = null;

        const result = c.ms_hermes_eval(
            self.runtime,
            source.ptr,
            source.len,
            "custom_macro.js",
        );

        if (c.ms_hermes_has_exception(self.runtime)) {
            const msg = c.ms_hermes_get_exception(self.runtime);
            if (msg) |m| {
                std.log.err("Macro error: {s}", .{std.mem.span(m)});
            }
            c.ms_hermes_clear_exception(self.runtime);
            return error.MacroExecutionFailed;
        }

        if (result) |r| {
            c.ms_value_destroy(r);
        }
    }

    /// Execute pre-compiled Hermes bytecode (HBC)
    ///
    /// This is ~100x faster than executeSource() for cached macros.
    /// Bytecode should be produced by hermesc or the transpiler/hermes module.
    ///
    /// Example:
    ///   const bytecode = try loader.loadMacroBytecode(allocator, config, "macros/derive.ts");
    ///   defer allocator.free(bytecode);
    ///   try vm.executeBytecode(bytecode, "derive.hbc", target_node);
    pub fn executeBytecode(self: *MacroVM, bytecode: []const u8, source_url: []const u8, target: *ast.Node) !void {
        self.ast_ctx.current_node = target;
        defer self.ast_ctx.current_node = null;

        // Call the bytecode execution function
        const result = c.ms_hermes_eval_bytecode(
            self.runtime,
            bytecode.ptr,
            bytecode.len,
            source_url.ptr,
        );

        if (c.ms_hermes_has_exception(self.runtime)) {
            const msg = c.ms_hermes_get_exception(self.runtime);
            if (msg) |m| {
                std.log.err("Macro bytecode error: {s}", .{std.mem.span(m)});
            }
            c.ms_hermes_clear_exception(self.runtime);
            return error.MacroExecutionFailed;
        }

        if (result) |r| {
            c.ms_value_destroy(r);
        }
    }

    /// Benchmark: Compare source vs bytecode execution time
    pub fn benchmarkExecution(self: *MacroVM, source: []const u8, bytecode: []const u8, iterations: usize) !struct { source_ns: u64, bytecode_ns: u64 } {
        var dummy_node = ast.Node{
            .kind = .program,
            .data = undefined,
            .loc = .{ .start = .{ .line = 0, .column = 0 }, .end = .{ .line = 0, .column = 0 } },
        };

        // Benchmark source execution
        const source_start = std.time.nanoTimestamp();
        for (0..iterations) |_| {
            try self.executeSource(source, &dummy_node);
        }
        const source_end = std.time.nanoTimestamp();
        const source_ns: u64 = @intCast(@as(i64, @intCast(source_end)) - @as(i64, @intCast(source_start)));

        // Benchmark bytecode execution
        const bytecode_start = std.time.nanoTimestamp();
        for (0..iterations) |_| {
            try self.executeBytecode(bytecode, "benchmark.hbc", &dummy_node);
        }
        const bytecode_end = std.time.nanoTimestamp();
        const bytecode_ns: u64 = @intCast(@as(i64, @intCast(bytecode_end)) - @as(i64, @intCast(bytecode_start)));

        return .{
            .source_ns = source_ns / iterations,
            .bytecode_ns = bytecode_ns / iterations,
        };
    }
};

/// console.log implementation for debugging macros
fn consoleLog(
    runtime: ?*c.MSHermesRuntime,
    context: ?*anyopaque,
    args: [*c]?*c.MSHermesValue,
    arg_count: usize,
) callconv(.c) ?*c.MSHermesValue {
    _ = context;
    const rt = runtime orelse return null;

    var output = std.ArrayList(u8).init(std.heap.page_allocator);
    defer output.deinit();

    for (0..arg_count) |i| {
        if (i > 0) output.appendSlice(" ") catch {};

        const arg = args[i] orelse continue;

        if (c.ms_value_is_string(arg)) {
            var len: usize = 0;
            const str = c.ms_value_get_string(rt, arg, &len);
            if (str) |s| {
                output.appendSlice(s[0..len]) catch {};
            }
        } else if (c.ms_value_is_number(arg)) {
            const num = c.ms_value_get_number(arg);
            std.fmt.format(output.writer(), "{d}", .{num}) catch {};
        } else if (c.ms_value_is_object(arg)) {
            output.appendSlice("[object]") catch {};
        } else {
            output.appendSlice("[?]") catch {};
        }
    }

    std.debug.print("[macro] {s}\n", .{output.items});
    return c.ms_value_undefined(rt);
}

// =============================================================================
// Tests
// =============================================================================

test "MacroVM initialization" {
    // This test requires Hermes runtime - skip in CI
    // const allocator = std.testing.allocator;
    // var arena = ast.ASTArena.init(allocator);
    // defer arena.deinit();
    //
    // var vm = try MacroVM.init(allocator, &arena);
    // defer vm.deinit();
}
