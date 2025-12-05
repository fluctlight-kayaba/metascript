/// Source-Defined Macro Registry
///
/// Collects @macro function declarations from AST and manages their
/// compilation/execution pipeline:
///
/// 1. AST Collection: Find all macro_decl nodes
/// 2. Transpilation: Convert macro body AST → JavaScript
/// 3. Compilation: JavaScript → Hermes bytecode (optional caching)
/// 4. Execution: Run in Hermes VM with AST API access
///
/// This enables Nim-style macros where users define macros in source files
/// using @macro function syntax.

const std = @import("std");
const ast = @import("../ast/ast.zig");

/// Represents a source-defined macro
pub const SourceMacro = struct {
    name: []const u8,
    params: []ast.node.FunctionExpr.FunctionParam,
    body: *ast.Node,
    location: ast.SourceLocation,

    // Cached JavaScript representation
    js_source: ?[]const u8 = null,

    // SHA256 hash of transpiled JS for bytecode cache lookup (collision-resistant)
    js_hash: ?[32]u8 = null,
};

/// Registry for source-defined macros
pub const SourceMacroRegistry = struct {
    allocator: std.mem.Allocator,
    macros: std.StringHashMap(SourceMacro),

    pub fn init(allocator: std.mem.Allocator) SourceMacroRegistry {
        return .{
            .allocator = allocator,
            .macros = std.StringHashMap(SourceMacro).init(allocator),
        };
    }

    pub fn deinit(self: *SourceMacroRegistry) void {
        // Free allocated strings
        var it = self.macros.iterator();
        while (it.next()) |entry| {
            // Free the name (key and value.name point to same alloc)
            self.allocator.free(entry.key_ptr.*);
            // Free JS source if allocated
            if (entry.value_ptr.js_source) |js| {
                self.allocator.free(js);
            }
        }
        self.macros.deinit();
    }

    /// Register a macro from a macro_decl AST node
    pub fn registerFromNode(self: *SourceMacroRegistry, node: *ast.Node) !void {
        std.debug.assert(node.kind == .macro_decl);

        const decl = &node.data.macro_decl;

        // IMPORTANT: Duplicate the name since the source buffer may be freed
        const name_copy = try self.allocator.dupe(u8, decl.name);

        try self.macros.put(name_copy, .{
            .name = name_copy,
            .params = decl.params,
            .body = decl.body,
            .location = node.location,
            .js_source = null,
        });

        std.log.info("[SourceMacroRegistry] Registered macro: @{s}", .{name_copy});
    }

    /// Look up a macro by name
    pub fn get(self: *SourceMacroRegistry, name: []const u8) ?*SourceMacro {
        return self.macros.getPtr(name);
    }

    /// Check if a macro exists
    pub fn contains(self: *SourceMacroRegistry, name: []const u8) bool {
        const result = self.macros.contains(name);
        if (!result) {
            // Debug: show what we have
            std.log.debug("[SourceMacroRegistry] '{s}' not found. Have: ", .{name});
            var it = self.macros.keyIterator();
            while (it.next()) |key| {
                std.log.debug("  - '{s}'", .{key.*});
            }
        }
        return result;
    }

    /// Get number of registered macros
    pub fn count(self: *const SourceMacroRegistry) usize {
        return self.macros.count();
    }

    /// Compile macro body AST to JavaScript
    /// Returns cached JS if already compiled
    /// Also computes js_hash for bytecode cache lookup
    pub fn getJavaScript(self: *SourceMacroRegistry, name: []const u8) !?[]const u8 {
        const macro = self.macros.getPtr(name) orelse return null;

        // Return cached JS if available
        if (macro.js_source) |js| {
            return js;
        }

        // Transpile macro body to JavaScript
        const js = try self.transpileToJS(macro);
        macro.js_source = js;

        // Compute SHA256 hash for bytecode cache lookup (collision-resistant)
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        // Include length prefixes to prevent hash collisions from concatenation
        var name_len_buf: [8]u8 = undefined;
        std.mem.writeInt(u64, &name_len_buf, name.len, .little);
        hasher.update(&name_len_buf);
        hasher.update(name);
        var js_len_buf: [8]u8 = undefined;
        std.mem.writeInt(u64, &js_len_buf, js.len, .little);
        hasher.update(&js_len_buf);
        hasher.update(js);
        macro.js_hash = hasher.finalResult();

        return js;
    }

    /// Get the SHA256 hash of transpiled JS for a macro (for bytecode cache lookup)
    pub fn getJSHash(self: *SourceMacroRegistry, name: []const u8) ?[32]u8 {
        const macro = self.macros.getPtr(name) orelse return null;
        return macro.js_hash;
    }

    /// Transpile macro body AST to JavaScript source
    fn transpileToJS(self: *SourceMacroRegistry, macro: *SourceMacro) ![]const u8 {
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        const writer = buffer.writer();

        // Generate function wrapper
        try writer.print("(function {s}(target) {{\n", .{macro.name});

        // Transpile the body
        try self.transpileNode(macro.body, writer, 1);

        try writer.writeAll("\n})");

        return try buffer.toOwnedSlice();
    }

    /// Transpile an AST node to JavaScript
    fn transpileNode(self: *SourceMacroRegistry, node: *ast.Node, writer: anytype, indent: usize) !void {
        try self.writeIndent(writer, indent);

        switch (node.kind) {
            .block_stmt => {
                const block = &node.data.block_stmt;
                for (block.statements) |stmt| {
                    try self.transpileNode(stmt, writer, indent);
                }
            },

            .variable_stmt => {
                const var_stmt = &node.data.variable_stmt;
                const kind_str = switch (var_stmt.kind) {
                    .@"const" => "const",
                    .let => "let",
                    .@"var" => "var",
                };

                for (var_stmt.declarations) |decl| {
                    try writer.print("{s} {s}", .{kind_str, decl.name});
                    if (decl.init) |init_expr| {
                        try writer.writeAll(" = ");
                        try self.transpileExpr(init_expr, writer);
                    }
                    try writer.writeAll(";\n");
                }
            },

            .return_stmt => {
                try writer.writeAll("return ");
                if (node.data.return_stmt.argument) |arg| {
                    try self.transpileExpr(arg, writer);
                }
                try writer.writeAll(";\n");
            },

            .expression_stmt => {
                try self.transpileExpr(node.data.expression_stmt, writer);
                try writer.writeAll(";\n");
            },

            .if_stmt => {
                const if_stmt = &node.data.if_stmt;
                try writer.writeAll("if (");
                try self.transpileExpr(if_stmt.condition, writer);
                try writer.writeAll(") {\n");
                try self.transpileNode(if_stmt.consequent, writer, indent + 1);
                try self.writeIndent(writer, indent);
                try writer.writeAll("}");
                if (if_stmt.alternate) |alt| {
                    try writer.writeAll(" else {\n");
                    try self.transpileNode(alt, writer, indent + 1);
                    try self.writeIndent(writer, indent);
                    try writer.writeAll("}");
                }
                try writer.writeAll("\n");
            },

            .while_stmt => {
                const while_stmt = &node.data.while_stmt;
                try writer.writeAll("while (");
                try self.transpileExpr(while_stmt.condition, writer);
                try writer.writeAll(") {\n");
                try self.transpileNode(while_stmt.body, writer, indent + 1);
                try self.writeIndent(writer, indent);
                try writer.writeAll("}\n");
            },

            .for_stmt => {
                const for_stmt = &node.data.for_stmt;
                try writer.writeAll("for (");
                if (for_stmt.init) |for_init| {
                    // Handle variable declaration or expression
                    if (for_init.kind == .variable_stmt) {
                        const var_stmt = &for_init.data.variable_stmt;
                        const kind_str = switch (var_stmt.kind) {
                            .@"const" => "const",
                            .let => "let",
                            .@"var" => "var",
                        };
                        if (var_stmt.declarations.len > 0) {
                            const decl = var_stmt.declarations[0];
                            try writer.print("{s} {s}", .{ kind_str, decl.name });
                            if (decl.init) |init_expr| {
                                try writer.writeAll(" = ");
                                try self.transpileExpr(init_expr, writer);
                            }
                        }
                    } else {
                        try self.transpileExpr(for_init, writer);
                    }
                }
                try writer.writeAll("; ");
                if (for_stmt.condition) |cond| {
                    try self.transpileExpr(cond, writer);
                }
                try writer.writeAll("; ");
                if (for_stmt.update) |update| {
                    try self.transpileExpr(update, writer);
                }
                try writer.writeAll(") {\n");
                try self.transpileNode(for_stmt.body, writer, indent + 1);
                try self.writeIndent(writer, indent);
                try writer.writeAll("}\n");
            },

            .break_stmt => {
                try writer.writeAll("break;\n");
            },

            .continue_stmt => {
                try writer.writeAll("continue;\n");
            },

            else => {
                try writer.print("/* TODO: {s} */", .{@tagName(node.kind)});
            },
        }
    }

    /// Transpile an expression node to JavaScript
    fn transpileExpr(self: *SourceMacroRegistry, node: *ast.Node, writer: anytype) anyerror!void {
        switch (node.kind) {
            .identifier => {
                try writer.writeAll(node.data.identifier);
            },

            .number_literal => {
                try writer.print("{d}", .{node.data.number_literal});
            },

            .string_literal => {
                try writer.print("\"{s}\"", .{node.data.string_literal});
            },

            .member_expr => {
                const member = &node.data.member_expr;
                try self.transpileExpr(member.object, writer);
                if (member.computed) {
                    try writer.writeAll("[");
                    try self.transpileExpr(member.property, writer);
                    try writer.writeAll("]");
                } else {
                    try writer.writeAll(".");
                    try self.transpileExpr(member.property, writer);
                }
            },

            .call_expr => {
                const call = &node.data.call_expr;
                try self.transpileExpr(call.callee, writer);
                try writer.writeAll("(");
                for (call.arguments, 0..) |arg, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try self.transpileExpr(arg, writer);
                }
                try writer.writeAll(")");
            },

            .binary_expr => {
                const binary = &node.data.binary_expr;
                try self.transpileExpr(binary.left, writer);
                try writer.print(" {s} ", .{binaryOpToJS(binary.op)});
                try self.transpileExpr(binary.right, writer);
            },

            .array_expr => {
                const array = &node.data.array_expr;
                try writer.writeAll("[");
                for (array.elements, 0..) |elem, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try self.transpileExpr(elem, writer);
                }
                try writer.writeAll("]");
            },

            .object_expr => {
                const obj = &node.data.object_expr;
                try writer.writeAll("{");
                for (obj.properties, 0..) |prop, i| {
                    if (i > 0) try writer.writeAll(", ");
                    switch (prop) {
                        .property => |p| {
                            try self.transpileExpr(p.key, writer);
                            if (!p.shorthand) {
                                try writer.writeAll(": ");
                                try self.transpileExpr(p.value, writer);
                            }
                        },
                        .spread => |spread_node| {
                            try writer.writeAll("...");
                            try self.transpileExpr(spread_node.data.spread_element.argument, writer);
                        },
                    }
                }
                try writer.writeAll("}");
            },

            .unary_expr => {
                const unary = &node.data.unary_expr;
                const op_str = switch (unary.op) {
                    .neg => "-",
                    .not => "!",
                    .bit_not => "~",
                    .typeof => "typeof ",
                    .void => "void ",
                };
                try writer.writeAll(op_str);
                try self.transpileExpr(unary.argument, writer);
            },

            .conditional_expr => {
                const cond = &node.data.conditional_expr;
                try writer.writeAll("(");
                try self.transpileExpr(cond.condition, writer);
                try writer.writeAll(" ? ");
                try self.transpileExpr(cond.consequent, writer);
                try writer.writeAll(" : ");
                try self.transpileExpr(cond.alternate, writer);
                try writer.writeAll(")");
            },

            .boolean_literal => {
                try writer.writeAll(if (node.data.boolean_literal) "true" else "false");
            },

            .null_literal => {
                try writer.writeAll("null");
            },

            .new_expr => {
                const new_expr = &node.data.new_expr;
                try writer.writeAll("new ");
                try self.transpileExpr(new_expr.callee, writer);
                try writer.writeAll("(");
                for (new_expr.arguments, 0..) |arg, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try self.transpileExpr(arg, writer);
                }
                try writer.writeAll(")");
            },

            .function_expr => {
                const func = &node.data.function_expr;
                if (func.is_arrow) {
                    try writer.writeAll("(");
                    for (func.params, 0..) |param, i| {
                        if (i > 0) try writer.writeAll(", ");
                        try writer.writeAll(param.name);
                    }
                    try writer.writeAll(") => ");
                    if (func.body.kind == .block_stmt) {
                        try writer.writeAll("{\n");
                        try self.transpileNode(func.body, writer, 0);
                        try writer.writeAll("}");
                    } else {
                        try self.transpileExpr(func.body, writer);
                    }
                } else {
                    try writer.writeAll("function");
                    if (func.name) |name| {
                        try writer.writeAll(" ");
                        try writer.writeAll(name);
                    }
                    try writer.writeAll("(");
                    for (func.params, 0..) |param, i| {
                        if (i > 0) try writer.writeAll(", ");
                        try writer.writeAll(param.name);
                    }
                    try writer.writeAll(") {\n");
                    try self.transpileNode(func.body, writer, 0);
                    try writer.writeAll("}");
                }
            },

            else => {
                try writer.print("/* expr: {s} */", .{@tagName(node.kind)});
            },
        }
    }

    fn writeIndent(self: *SourceMacroRegistry, writer: anytype, indent: usize) !void {
        _ = self;
        for (0..indent) |_| {
            try writer.writeAll("  ");
        }
    }
};

fn binaryOpToJS(op: ast.BinaryOp) []const u8 {
    return switch (op) {
        .assign => "=",
        .add => "+",
        .sub => "-",
        .mul => "*",
        .div => "/",
        .mod => "%",
        .eq => "===",
        .ne => "!==",
        .lt => "<",
        .le => "<=",
        .gt => ">",
        .ge => ">=",
        .@"and" => "&&",
        .@"or" => "||",
        .bit_and => "&",
        .bit_or => "|",
        .bit_xor => "^",
        .shl => "<<",
        .shr => ">>",
        .nullish_coalesce => "??",
    };
}

/// Collector to find all macro_decl nodes in an AST
pub const MacroDeclCollector = struct {
    registry: *SourceMacroRegistry,

    pub fn init(registry: *SourceMacroRegistry) MacroDeclCollector {
        return .{ .registry = registry };
    }

    /// Visit a program and collect all macro declarations
    pub fn collectFromProgram(self: *MacroDeclCollector, program: *ast.Node) !void {
        std.debug.assert(program.kind == .program);

        for (program.data.program.statements) |stmt| {
            if (stmt.kind == .macro_decl) {
                try self.registry.registerFromNode(stmt);
            }
        }
    }
};

// =============================================================================
// Tests
// =============================================================================

test "source macro registry basic" {
    var arena = ast.ASTArena.init(std.testing.allocator);
    defer arena.deinit();

    var registry = SourceMacroRegistry.init(std.testing.allocator);
    defer registry.deinit();

    try std.testing.expectEqual(@as(usize, 0), registry.count());
    try std.testing.expect(!registry.contains("myMacro"));
}

test "source macro transpilation" {
    var arena = ast.ASTArena.init(std.testing.allocator);
    defer arena.deinit();

    var registry = SourceMacroRegistry.init(std.testing.allocator);
    defer registry.deinit();

    // Create a simple macro body: { return target; }
    const loc = ast.SourceLocation.dummy();

    const return_target = try arena.createNode(.identifier, loc, .{ .identifier = "target" });
    const return_stmt = try arena.createNode(.return_stmt, loc, .{ .return_stmt = .{ .argument = return_target } });

    const stmts = try std.testing.allocator.alloc(*ast.Node, 1);
    defer std.testing.allocator.free(stmts);
    stmts[0] = return_stmt;

    const body = try arena.createNode(.block_stmt, loc, .{ .block_stmt = .{ .statements = stmts } });

    const macro_node = try arena.createNode(.macro_decl, loc, .{
        .macro_decl = .{
            .name = "testMacro",
            .type_params = &[_]ast.types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = null,
            .body = body,
        },
    });

    try registry.registerFromNode(macro_node);
    try std.testing.expectEqual(@as(usize, 1), registry.count());

    // Get JavaScript
    const js = try registry.getJavaScript("testMacro");
    try std.testing.expect(js != null);

    // Should contain function definition
    try std.testing.expect(std.mem.indexOf(u8, js.?, "function testMacro") != null);
    try std.testing.expect(std.mem.indexOf(u8, js.?, "return target") != null);
}
