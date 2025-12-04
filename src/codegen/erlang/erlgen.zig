/// Erlang Code Generator - Main Module
/// Direct emission from Typed AST to Erlang source code
///
/// **Phase 1: Simple Backend (José Valim's Recommendation)**
/// - Basic class → tagged tuple codegen
/// - Methods → functions with receiver
/// - NO Lobster integration (Phase 2 only)
/// - Target: Debuggable, idiomatic Erlang
///
/// Design inspired by Gleam's erlang.rs:
/// - Document builder for pretty-printing
/// - Erlang source (not Abstract Format)
/// - Snapshot testable output
///
/// Type Mappings:
///   number → float() or integer()
///   string → binary()
///   boolean → boolean()
///   class → tagged tuple {ClassName, Field1, Field2, ...}
///
/// Module Structure:
///   erlgen.zig - Main generator (this file)
///   (Phase 2): declarations.zig, expressions.zig, statements.zig
///
/// Usage:
///   var gen = ErlangGenerator.init(allocator);
///   defer gen.deinit();
///   const erl_code = try gen.generate(typed_ast);

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const types_mod = @import("../../ast/types.zig");
const node_mod = @import("../../ast/node.zig");

/// Environment for tracking variable shadowing across scopes
/// Inspired by Gleam's approach: variable counters never decrease,
/// even across scope boundaries (function-level tracking)
pub const ErlangEnv = struct {
    allocator: std.mem.Allocator,
    /// Tracks variable generation count (never decreases)
    var_counters: std.StringHashMap(usize),
    /// Stack of scopes for tracking active variables
    scopes: std.ArrayList(Scope),

    const Self = @This();

    const Scope = struct {
        /// Variables active in this scope (for future use)
        active_vars: std.StringHashMap(usize),

        fn init(allocator: std.mem.Allocator) Scope {
            return .{
                .active_vars = std.StringHashMap(usize).init(allocator),
            };
        }

        fn deinit(self: Scope) void {
            var mut_self = self;
            mut_self.active_vars.deinit();
        }
    };

    pub fn init(allocator: std.mem.Allocator) !Self {
        return .{
            .allocator = allocator,
            .var_counters = std.StringHashMap(usize).init(allocator),
            .scopes = std.ArrayList(Scope).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.var_counters.deinit();
        for (self.scopes.items) |scope| {
            scope.deinit();
        }
        self.scopes.deinit();
    }

    /// Generate next variable name with shadowing suffix
    /// First use: "X", second: "X@1", third: "X@2", etc.
    pub fn nextVarName(self: *Self, name: []const u8) ![]const u8 {
        const count = self.var_counters.get(name) orelse 0;
        try self.var_counters.put(name, count + 1);

        // Capitalize first letter for Erlang variable convention
        const capitalized = try self.capitalizeVar(name);
        defer self.allocator.free(capitalized);

        if (count == 0) {
            // First use: just capitalize
            return try self.allocator.dupe(u8, capitalized);
        } else {
            // Subsequent uses: add @N suffix
            return try std.fmt.allocPrint(
                self.allocator,
                "{s}@{d}",
                .{ capitalized, count },
            );
        }
    }

    /// Get current variable name without incrementing counter
    /// Used when referencing a variable (not declaring/reassigning)
    pub fn getCurrentVarName(self: *Self, name: []const u8) ![]const u8 {
        const count = self.var_counters.get(name) orelse 0;

        // Capitalize first letter for Erlang variable convention
        const capitalized = try self.capitalizeVar(name);
        defer self.allocator.free(capitalized);

        if (count == 0) {
            // Never declared yet - just capitalize
            return try self.allocator.dupe(u8, capitalized);
        } else if (count == 1) {
            // First declaration, no @N suffix yet
            return try self.allocator.dupe(u8, capitalized);
        } else {
            // Use current generation: count-1 because we've incremented past current
            return try std.fmt.allocPrint(
                self.allocator,
                "{s}@{d}",
                .{ capitalized, count - 1 },
            );
        }
    }

    /// Enter a new scope
    pub fn enterScope(self: *Self) !void {
        try self.scopes.append(Scope.init(self.allocator));
    }

    /// Exit current scope
    pub fn exitScope(self: *Self) void {
        if (self.scopes.items.len > 0) {
            // Deinit the last scope before popping
            const last_idx = self.scopes.items.len - 1;
            self.scopes.items[last_idx].active_vars.deinit();
            _ = self.scopes.pop();
        }
    }

    /// Capitalize first letter of variable name
    fn capitalizeVar(self: *Self, name: []const u8) ![]const u8 {
        if (name.len == 0) return try self.allocator.dupe(u8, "_");

        var result = try self.allocator.alloc(u8, name.len);
        result[0] = std.ascii.toUpper(name[0]);
        @memcpy(result[1..], name[1..]);
        return result;
    }
};

pub const ErlangGenerator = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),
    indent_level: usize,
    /// Module name (derived from file or default)
    module_name: []const u8,
    /// Exported functions (name + arity)
    exports: std.ArrayList(Export),
    /// Environment for variable shadowing
    env: ErlangEnv,
    /// Counter for generating unique loop function names
    loop_counter: usize,

    const Self = @This();

    const Export = struct {
        name: []const u8,
        arity: u32,
    };

    pub fn init(allocator: std.mem.Allocator, file_path: ?[]const u8) !Self {
        // Derive module name from file path
        const module_name = if (file_path) |path| blk: {
            // Extract basename and remove .ms extension
            const basename = std.fs.path.basename(path);
            const name_without_ext = if (std.mem.endsWith(u8, basename, ".ms"))
                basename[0 .. basename.len - 3]
            else
                basename;
            break :blk name_without_ext;
        } else "metascript_module";

        return .{
            .allocator = allocator,
            .output = std.ArrayList(u8).init(allocator),
            .indent_level = 0,
            .module_name = module_name,
            .exports = std.ArrayList(Export).init(allocator),
            .env = try ErlangEnv.init(allocator),
            .loop_counter = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.output.deinit();

        // Free all export names to prevent memory leaks
        for (self.exports.items) |exp| {
            self.allocator.free(exp.name);
        }
        self.exports.deinit();

        // Clean up environment
        self.env.deinit();
    }

    /// Generate Erlang from a typed AST
    pub fn generate(self: *Self, program: *ast.Node) ![]const u8 {
        std.debug.assert(program.kind == .program);

        // Phase 1: Collect exports (scan all top-level declarations)
        for (program.data.program.statements) |stmt| {
            try self.collectExports(stmt);
        }

        // Phase 2: Emit module header
        try self.emitModuleHeader();

        // Phase 3: Emit declarations
        for (program.data.program.statements) |stmt| {
            try self.emitNode(stmt);
        }

        return try self.output.toOwnedSlice();
    }

    fn collectExports(self: *Self, node: *ast.Node) !void {
        switch (node.kind) {
            .function_decl => {
                const name = node.data.function_decl.name;
                const name_owned = try self.allocator.dupe(u8, name);
                const arity = @as(u32, @intCast(node.data.function_decl.params.len));
                try self.exports.append(.{ .name = name_owned, .arity = arity });
            },
            .class_decl => {
                // Class generates constructor: new_classname/N
                const class_name = node.data.class_decl.name;
                const lower_name = try std.ascii.allocLowerString(self.allocator, class_name);
                defer self.allocator.free(lower_name);

                const ctor_name = try std.fmt.allocPrint(
                    self.allocator,
                    "new_{s}",
                    .{lower_name},
                );
                defer self.allocator.free(ctor_name);

                // Count properties for constructor arity
                var prop_count: u32 = 0;
                for (node.data.class_decl.members) |member| {
                    if (member.kind == .property_decl) prop_count += 1;
                }

                const ctor_name_owned = try self.allocator.dupe(u8, ctor_name);
                try self.exports.append(.{ .name = ctor_name_owned, .arity = prop_count });

                // Methods become exports: classname_method/N+1 (+1 for receiver)
                for (node.data.class_decl.members) |member| {
                    if (member.kind == .method_decl) {
                        const method_name = try std.fmt.allocPrint(
                            self.allocator,
                            "{s}_{s}",
                            .{ lower_name, member.data.method_decl.name },
                        );
                        defer self.allocator.free(method_name);

                        const method_arity = @as(u32, @intCast(member.data.method_decl.params.len + 1));
                        const method_name_owned = try self.allocator.dupe(u8, method_name);
                        try self.exports.append(.{ .name = method_name_owned, .arity = method_arity });
                    }
                }
            },
            else => {},
        }
    }

    fn emitModuleHeader(self: *Self) !void {
        // -module(name).
        try self.emit("-module(");
        try self.emit(self.module_name);
        try self.emit(").\n\n");

        // -export([func1/2, func2/1, ...]).
        if (self.exports.items.len > 0) {
            try self.emit("-export([\n");
            for (self.exports.items, 0..) |exp, i| {
                try self.emitIndent();
                try self.emit("    ");
                try self.emit(exp.name);
                try self.emit("/");
                try self.emitInt(exp.arity);
                if (i < self.exports.items.len - 1) {
                    try self.emit(",");
                }
                try self.emit("\n");
            }
            try self.emitIndent();
            try self.emit("]).\n\n");
        }
    }

    /// Main dispatch for node emission
    pub fn emitNode(self: *Self, node: *ast.Node) anyerror!void {
        switch (node.kind) {
            .function_decl => try self.emitFunctionDecl(node),
            .class_decl => try self.emitClassDecl(node),
            .variable_stmt => try self.emitVariableStmt(node),

            // Statements
            .block_stmt => try self.emitBlockStmt(node),
            .expression_stmt => try self.emitExpressionStmt(node),
            .return_stmt => try self.emitReturnStmt(node),
            .if_stmt => try self.emitIfStmt(node),
            .while_stmt => try self.emitWhileStmt(node),
            .for_stmt => try self.emitForStmt(node),

            // Expressions
            .number_literal => try self.emitNumberLiteral(node),
            .string_literal => try self.emitStringLiteral(node),
            .boolean_literal => try self.emitBooleanLiteral(node),
            .null_literal => try self.emit("undefined"), // Erlang's undefined atom
            .identifier => try self.emitIdentifier(node),
            .binary_expr => try self.emitBinaryExpr(node),
            .unary_expr => try self.emitUnaryExpr(node),
            .call_expr => try self.emitCallExpr(node),
            .member_expr => try self.emitMemberExpr(node),
            .array_expr => try self.emitArrayExpr(node),
            .object_expr => try self.emitObjectExpr(node),
            .new_expr => try self.emitNewExpr(node),

            // Type-only nodes (no output)
            .interface_decl, .type_alias_decl => {},

            else => {
                // Unsupported node type - emit comment
                try self.emit("%% TODO: ");
                try self.emit(@tagName(node.kind));
                try self.emit("\n");
            },
        }
    }

    // =========================================================================
    // Declarations
    // =========================================================================

    fn emitFunctionDecl(self: *Self, node: *ast.Node) !void {
        const func = node.data.function_decl;

        // function_name(Param1, Param2) ->
        try self.emit(func.name);
        try self.emit("(");
        for (func.params, 0..) |param, i| {
            try self.emitParameter(param);
            if (i < func.params.len - 1) {
                try self.emit(", ");
            }
        }
        try self.emit(") ->\n");

        // Body (for now, just emit as block)
        self.indent_level += 1;
        if (func.body) |body| {
            try self.emitFunctionBody(body);
        } else {
            try self.emitIndent();
            try self.emit("ok"); // Placeholder for empty function
        }
        self.indent_level -= 1;
        try self.emit(".\n\n");
    }

    fn emitParameter(self: *Self, param: node_mod.FunctionExpr.FunctionParam) !void {
        // Convert to capitalized Erlang variable
        const erl_name = try self.toErlangVar(param.name);
        defer self.allocator.free(erl_name);
        try self.emit(erl_name);
    }

    fn emitFunctionBody(self: *Self, body: *ast.Node) !void {
        // Enter new scope for function body
        try self.env.enterScope();
        defer self.env.exitScope();

        // For now, emit block contents
        if (body.kind == .block_stmt) {
            const stmts = body.data.block_stmt.statements;
            if (stmts.len == 0) {
                try self.emitIndent();
                try self.emit("ok");
                try self.emit("\n");
                return;
            }

            for (stmts, 0..) |stmt, i| {
                try self.emitIndent();
                try self.emitNode(stmt);
                if (i < stmts.len - 1) {
                    try self.emit(",\n"); // Erlang statement separator
                }
            }
        } else {
            try self.emitIndent();
            try self.emitNode(body);
        }
        try self.emit("\n");
    }

    fn emitClassDecl(self: *Self, node: *ast.Node) !void {
        const class = node.data.class_decl;
        const lower_name = try std.ascii.allocLowerString(self.allocator, class.name);
        defer self.allocator.free(lower_name);

        // Filter members into properties and methods
        var properties = std.ArrayList(*ast.Node).init(self.allocator);
        defer properties.deinit();
        var methods = std.ArrayList(*ast.Node).init(self.allocator);
        defer methods.deinit();

        for (class.members) |member| {
            if (member.kind == .property_decl) {
                try properties.append(member);
            } else if (member.kind == .method_decl) {
                try methods.append(member);
            }
        }

        // Constructor: new_classname(Field1, Field2) -> {classname, Field1, Field2}.
        try self.emit("new_");
        try self.emit(lower_name);
        try self.emit("(");
        for (properties.items, 0..) |prop, i| {
            const erl_var = try self.toErlangVar(prop.data.property_decl.name);
            defer self.allocator.free(erl_var);
            try self.emit(erl_var);
            if (i < properties.items.len - 1) {
                try self.emit(", ");
            }
        }
        try self.emit(") ->\n");
        try self.emitIndent();
        try self.emit("    {");
        try self.emit(lower_name);
        for (properties.items) |prop| {
            try self.emit(", ");
            const erl_var = try self.toErlangVar(prop.data.property_decl.name);
            defer self.allocator.free(erl_var);
            try self.emit(erl_var);
        }
        try self.emit("}.\n\n");

        // Methods: classname_method(This, Param1) -> ...
        for (methods.items) |method| {
            try self.emit(lower_name);
            try self.emit("_");
            try self.emit(method.data.method_decl.name);
            try self.emit("(");

            // First parameter: This (the instance)
            const receiver_var = try self.toErlangVar(class.name);
            defer self.allocator.free(receiver_var);
            try self.emit(receiver_var);

            // Remaining parameters
            const params = method.data.method_decl.params;
            if (params.len > 0) {
                try self.emit(", ");
                for (params, 0..) |param, i| {
                    try self.emitParameter(param);
                    if (i < params.len - 1) {
                        try self.emit(", ");
                    }
                }
            }
            try self.emit(") ->\n");

            // Method body
            self.indent_level += 1;
            if (method.data.method_decl.body) |body| {
                try self.emitFunctionBody(body);
            } else {
                try self.emitIndent();
                try self.emit("ok");
                try self.emit("\n");
            }
            self.indent_level -= 1;
            try self.emit(".\n\n");
        }
    }

    fn emitVariableStmt(self: *Self, node: *ast.Node) !void {
        const var_stmt = node.data.variable_stmt;
        for (var_stmt.declarations, 0..) |decl, i| {
            // Use environment for variable shadowing
            const erl_var = try self.env.nextVarName(decl.name);
            defer self.allocator.free(erl_var);

            try self.emit(erl_var);
            try self.emit(" = ");
            if (decl.init) |initializer| {
                try self.emitNode(initializer);
            } else {
                try self.emit("undefined");
            }
            if (i < var_stmt.declarations.len - 1) {
                try self.emit(",\n");
                try self.emitIndent();
            }
        }
    }

    // =========================================================================
    // Statements
    // =========================================================================

    fn emitBlockStmt(self: *Self, node: *ast.Node) !void {
        const stmts = node.data.block_stmt.statements;

        // Enter new scope for block
        try self.env.enterScope();
        defer self.env.exitScope();

        for (stmts, 0..) |stmt, i| {
            try self.emitNode(stmt);
            if (i < stmts.len - 1) {
                try self.emit(",\n");
                try self.emitIndent();
            }
        }
    }

    fn emitExpressionStmt(self: *Self, node: *ast.Node) !void {
        try self.emitNode(node.data.expression_stmt);
    }

    fn emitReturnStmt(self: *Self, node: *ast.Node) !void {
        if (node.data.return_stmt.argument) |arg| {
            try self.emitNode(arg);
        } else {
            try self.emit("ok");
        }
    }

    fn emitIfStmt(self: *Self, node: *ast.Node) !void {
        const if_stmt = node.data.if_stmt;

        try self.emit("case ");
        try self.emitNode(if_stmt.condition);
        try self.emit(" of\n");
        self.indent_level += 1;
        try self.emitIndent();
        try self.emit("true ->\n");
        self.indent_level += 1;
        try self.emitIndent();

        // Enter scope for consequent branch
        try self.env.enterScope();
        try self.emitNode(if_stmt.consequent);
        self.env.exitScope();

        self.indent_level -= 1;
        try self.emit(";\n");
        try self.emitIndent();
        try self.emit("false ->\n");
        self.indent_level += 1;
        try self.emitIndent();

        // Enter scope for alternate branch
        try self.env.enterScope();
        if (if_stmt.alternate) |alternate| {
            try self.emitNode(alternate);
        } else {
            try self.emit("ok");
        }
        self.env.exitScope();

        self.indent_level -= 1;
        try self.emit("\n");
        self.indent_level -= 1;
        try self.emitIndent();
        try self.emit("end");
    }

    fn emitWhileStmt(self: *Self, node: *ast.Node) !void {
        const while_stmt = node.data.while_stmt;

        // Generate unique loop function name
        const loop_id = self.loop_counter;
        self.loop_counter += 1;

        const loop_name = try std.fmt.allocPrint(
            self.allocator,
            "Loop_{d}",
            .{loop_id},
        );
        defer self.allocator.free(loop_name);

        // Erlang pattern: use a recursive local function
        // Loop_N = fun Loop_N_Inner() -> case Condition of ... end end, Loop_N()

        // Define the loop function
        try self.emit(loop_name);
        try self.emit(" = fun ");
        try self.emit(loop_name);
        try self.emit("_Inner() ->\n");
        self.indent_level += 1;
        try self.emitIndent();

        // Case on condition
        try self.emit("case ");
        try self.emitNode(while_stmt.condition);
        try self.emit(" of\n");
        self.indent_level += 1;
        try self.emitIndent();
        try self.emit("true ->\n");
        self.indent_level += 1;
        try self.emitIndent();

        // Emit body
        try self.env.enterScope();
        try self.emitNode(while_stmt.body);
        try self.emit(",\n");
        try self.emitIndent();

        // Recursive call
        try self.emit(loop_name);
        try self.emit("_Inner()");
        self.env.exitScope();

        self.indent_level -= 1;
        try self.emit(";\n");
        try self.emitIndent();
        try self.emit("false ->\n");
        self.indent_level += 1;
        try self.emitIndent();
        try self.emit("ok\n");
        self.indent_level -= 1;
        self.indent_level -= 1;
        try self.emitIndent();
        try self.emit("end\n");
        self.indent_level -= 1;
        try self.emitIndent();
        try self.emit("end,\n");
        try self.emitIndent();

        // Call the loop function
        try self.emit(loop_name);
        try self.emit("()");
    }

    fn emitForStmt(self: *Self, node: *ast.Node) !void {
        const for_stmt = node.data.for_stmt;

        // Transform for loop to while loop:
        // for (init; condition; update) { body }
        // becomes:
        // init, while (condition) { body, update }

        // Emit init if present
        if (for_stmt.init) |init_node| {
            try self.emitNode(init_node);
            try self.emit(",\n");
            try self.emitIndent();
        }

        // Generate unique loop function name
        const loop_id = self.loop_counter;
        self.loop_counter += 1;

        const loop_name = try std.fmt.allocPrint(
            self.allocator,
            "Loop_{d}",
            .{loop_id},
        );
        defer self.allocator.free(loop_name);

        // Define the loop function
        try self.emit(loop_name);
        try self.emit(" = fun ");
        try self.emit(loop_name);
        try self.emit("_Inner() ->\n");
        self.indent_level += 1;
        try self.emitIndent();

        // Case on condition (or true if no condition)
        try self.emit("case ");
        if (for_stmt.condition) |cond| {
            try self.emitNode(cond);
        } else {
            try self.emit("true");
        }
        try self.emit(" of\n");
        self.indent_level += 1;
        try self.emitIndent();
        try self.emit("true ->\n");
        self.indent_level += 1;
        try self.emitIndent();

        // Emit body
        try self.env.enterScope();
        try self.emitNode(for_stmt.body);

        // Emit update if present
        if (for_stmt.update) |upd| {
            try self.emit(",\n");
            try self.emitIndent();
            try self.emitNode(upd);
        }

        try self.emit(",\n");
        try self.emitIndent();

        // Recursive call
        try self.emit(loop_name);
        try self.emit("_Inner()");
        self.env.exitScope();

        self.indent_level -= 1;
        try self.emit(";\n");
        try self.emitIndent();
        try self.emit("false ->\n");
        self.indent_level += 1;
        try self.emitIndent();
        try self.emit("ok\n");
        self.indent_level -= 1;
        self.indent_level -= 1;
        try self.emitIndent();
        try self.emit("end\n");
        self.indent_level -= 1;
        try self.emitIndent();
        try self.emit("end,\n");
        try self.emitIndent();

        // Call the loop function
        try self.emit(loop_name);
        try self.emit("()");
    }

    // =========================================================================
    // Expressions
    // =========================================================================

    fn emitNumberLiteral(self: *Self, node: *ast.Node) !void {
        const value = node.data.number_literal;
        // Erlang supports both integer and float
        if (@floor(value) == value) {
            try self.emitInt(@as(i64, @intFromFloat(value)));
        } else {
            try self.emitFloat(value);
        }
    }

    fn emitStringLiteral(self: *Self, node: *ast.Node) !void {
        // Erlang string literals use double quotes, but we emit binaries
        try self.emit("<<\"");
        try self.emitEscapedString(node.data.string_literal);
        try self.emit("\">>");
    }

    fn emitBooleanLiteral(self: *Self, node: *ast.Node) !void {
        if (node.data.boolean_literal) {
            try self.emit("true");
        } else {
            try self.emit("false");
        }
    }

    fn emitIdentifier(self: *Self, node: *ast.Node) !void {
        const name = node.data.identifier;
        // Use environment to get current shadowed variable name
        const erl_var = try self.env.getCurrentVarName(name);
        defer self.allocator.free(erl_var);
        try self.emit(erl_var);
    }

    fn emitBinaryExpr(self: *Self, node: *ast.Node) !void {
        const binary = node.data.binary_expr;

        // Special case: assignment (variable shadowing)
        if (binary.op == .assign and binary.left.kind == .identifier) {
            const var_name = binary.left.data.identifier;

            // Get the NEXT variable name WITHOUT incrementing yet
            const count = self.env.var_counters.get(var_name) orelse 0;
            const next_name = blk: {
                const capitalized = try self.env.capitalizeVar(var_name);
                defer self.allocator.free(capitalized);

                if (count == 0) {
                    break :blk try self.allocator.dupe(u8, capitalized);
                } else {
                    break :blk try std.fmt.allocPrint(
                        self.allocator,
                        "{s}@{d}",
                        .{ capitalized, count },
                    );
                }
            };
            defer self.allocator.free(next_name);

            // Emit LHS
            try self.emit(next_name);
            try self.emit(" = ");

            // Emit RHS (this will use current variable names)
            try self.emitNode(binary.right);

            // NOW increment the counter for next use
            try self.env.var_counters.put(var_name, count + 1);

            return;
        }

        // Special case: string concatenation
        if (binary.op == .add and self.isStringType(binary.left)) {
            // Phase 1: Simple binary concat (Phase 2: optimize to iolist)
            try self.emit("<<");
            try self.emitNode(binary.left);
            try self.emit("/binary, ");
            try self.emitNode(binary.right);
            try self.emit("/binary>>");
            return;
        }

        // Normal binary expression
        try self.emitNode(binary.left);
        try self.emit(" ");
        try self.emit(self.binaryOpToErlang(binary.op));
        try self.emit(" ");
        try self.emitNode(binary.right);
    }

    fn emitUnaryExpr(self: *Self, node: *ast.Node) !void {
        const unary = node.data.unary_expr;
        try self.emit(self.unaryOpToErlang(unary.op));
        try self.emitNode(unary.argument);
    }

    fn emitCallExpr(self: *Self, node: *ast.Node) !void {
        const call = node.data.call_expr;

        // Handle method calls: obj.method(args)
        if (call.callee.kind == .member_expr) {
            const member = call.callee.data.member_expr;

            // Special case: console.log() → io:format()
            if (member.object.kind == .identifier and
                std.mem.eql(u8, member.object.data.identifier, "console") and
                member.property.kind == .identifier and
                std.mem.eql(u8, member.property.data.identifier, "log"))
            {
                // Emit as io:format("~p~n", [Args])
                try self.emit("io:format(\"");

                // Build format string with ~p for each argument
                for (call.arguments, 0..) |_, i| {
                    if (i > 0) try self.emit(" ");
                    try self.emit("~p");
                }
                try self.emit("~n\", [");

                // Emit arguments
                for (call.arguments, 0..) |arg, i| {
                    try self.emitNode(arg);
                    if (i < call.arguments.len - 1) {
                        try self.emit(", ");
                    }
                }
                try self.emit("])");
                return;
            }

            // Handle built-in array/list methods
            if (member.property.kind == .identifier) {
                const method_name = member.property.data.identifier;

                // Array methods
                if (std.mem.eql(u8, method_name, "push")) {
                    // arr.push(x) → lists:append(Arr, [X])
                    try self.emit("lists:append(");
                    try self.emitNode(member.object);
                    try self.emit(", [");
                    for (call.arguments, 0..) |arg, i| {
                        try self.emitNode(arg);
                        if (i < call.arguments.len - 1) {
                            try self.emit(", ");
                        }
                    }
                    try self.emit("])");
                    return;
                } else if (std.mem.eql(u8, method_name, "length")) {
                    // arr.length or str.length → length(Arr) or byte_size(Str)
                    // For simplicity, use length() which works for lists
                    try self.emit("length(");
                    try self.emitNode(member.object);
                    try self.emit(")");
                    return;
                } else if (std.mem.eql(u8, method_name, "map")) {
                    // arr.map(fn) → lists:map(Fn, Arr)
                    try self.emit("lists:map(");
                    if (call.arguments.len > 0) {
                        try self.emitNode(call.arguments[0]);
                        try self.emit(", ");
                    }
                    try self.emitNode(member.object);
                    try self.emit(")");
                    return;
                } else if (std.mem.eql(u8, method_name, "filter")) {
                    // arr.filter(fn) → lists:filter(Fn, Arr)
                    try self.emit("lists:filter(");
                    if (call.arguments.len > 0) {
                        try self.emitNode(call.arguments[0]);
                        try self.emit(", ");
                    }
                    try self.emitNode(member.object);
                    try self.emit(")");
                    return;
                }
            }

            // For class methods or unknown methods:
            // Without type information, we can't determine the correct function name
            // Emit a TODO comment for now
            try self.emit("%% TODO: method call - ");
            try self.emitNode(member.object);
            try self.emit(".");
            if (member.property.kind == .identifier) {
                try self.emit(member.property.data.identifier);
            } else {
                try self.emitNode(member.property);
            }
            try self.emit("(");
            for (call.arguments, 0..) |arg, i| {
                try self.emitNode(arg);
                if (i < call.arguments.len - 1) {
                    try self.emit(", ");
                }
            }
            try self.emit(")");
            return;
        }

        // Regular function call
        // Special handling: if callee is an identifier, emit as-is (lowercase function name)
        // Otherwise emit normally (for member expressions, etc.)
        if (call.callee.kind == .identifier) {
            // Function names stay lowercase in Erlang
            try self.emit(call.callee.data.identifier);
        } else {
            try self.emitNode(call.callee);
        }

        try self.emit("(");
        for (call.arguments, 0..) |arg, i| {
            try self.emitNode(arg);
            if (i < call.arguments.len - 1) {
                try self.emit(", ");
            }
        }
        try self.emit(")");
    }

    fn emitMemberExpr(self: *Self, node: *ast.Node) !void {
        const member = node.data.member_expr;

        // Special case: console.log is handled in emitCallExpr
        // Other console.* members would need special handling too
        if (member.object.kind == .identifier and
            std.mem.eql(u8, member.object.data.identifier, "console")) {
            // This is likely console.log or similar, already handled in emitCallExpr
            // If we reach here, it's a standalone reference (rare)
            try self.emit("console_");
            if (member.property.kind == .identifier) {
                try self.emit(member.property.data.identifier);
            } else {
                try self.emitNode(member.property);
            }
            return;
        }

        // Special case: array.length → length(Array)
        // In JavaScript, .length is a property, but in Erlang it's a function
        if (member.property.kind == .identifier and
            std.mem.eql(u8, member.property.data.identifier, "length")) {
            try self.emit("length(");
            try self.emitNode(member.object);
            try self.emit(")");
            return;
        }

        // Property access: obj.prop → maps:get(prop, obj)
        // This works for object literals (which emit as Erlang maps)
        // For class instances (tagged tuples), would need element(N, Tuple)
        // but that requires knowing property index from class definition

        try self.emit("maps:get(");

        // Property name as atom
        if (member.property.kind == .identifier) {
            // Use atom for static property access
            try self.emit(member.property.data.identifier);
        } else {
            // Computed property access
            try self.emitNode(member.property);
        }

        try self.emit(", ");
        try self.emitNode(member.object);
        try self.emit(")");
    }

    fn emitArrayExpr(self: *Self, node: *ast.Node) !void {
        const elements = node.data.array_expr.elements;
        try self.emit("[");
        for (elements, 0..) |elem, i| {
            try self.emitNode(elem);
            if (i < elements.len - 1) {
                try self.emit(", ");
            }
        }
        try self.emit("]");
    }

    fn emitObjectExpr(self: *Self, node: *ast.Node) !void {
        const properties = node.data.object_expr.properties;
        // Erlang map literal
        try self.emit("#{");
        var first = true;
        for (properties) |prop| {
            switch (prop) {
                .property => |p| {
                    if (!first) try self.emit(", ");
                    first = false;
                    // Key (for now, assume it's an identifier)
                    if (p.key.kind == .identifier) {
                        try self.emit(p.key.data.identifier);
                    } else {
                        try self.emitNode(p.key);
                    }
                    try self.emit(" => ");
                    try self.emitNode(p.value);
                },
                .spread => |_| {
                    // TODO: Handle object spread
                    try self.emit("%% TODO: object spread");
                },
            }
        }
        try self.emit("}");
    }

    fn emitNewExpr(self: *Self, node: *ast.Node) !void {
        const new_expr = node.data.new_expr;
        // new ClassName(args) -> new_classname(args)
        if (new_expr.callee.kind == .identifier) {
            const class_name = new_expr.callee.data.identifier;
            const lower_name = try std.ascii.allocLowerString(self.allocator, class_name);
            defer self.allocator.free(lower_name);

            try self.emit("new_");
            try self.emit(lower_name);
            try self.emit("(");
            for (new_expr.arguments, 0..) |arg, i| {
                try self.emitNode(arg);
                if (i < new_expr.arguments.len - 1) {
                    try self.emit(", ");
                }
            }
            try self.emit(")");
        }
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    fn emit(self: *Self, str: []const u8) !void {
        try self.output.appendSlice(str);
    }

    fn emitIndent(self: *Self) !void {
        var i: usize = 0;
        while (i < self.indent_level * 4) : (i += 1) {
            try self.output.append(' ');
        }
    }

    fn emitInt(self: *Self, value: anytype) !void {
        const str = try std.fmt.allocPrint(self.allocator, "{d}", .{value});
        defer self.allocator.free(str);
        try self.emit(str);
    }

    fn emitFloat(self: *Self, value: f64) !void {
        const str = try std.fmt.allocPrint(self.allocator, "{d}", .{value});
        defer self.allocator.free(str);
        try self.emit(str);
    }

    fn emitEscapedString(self: *Self, str: []const u8) !void {
        for (str) |c| {
            switch (c) {
                '"' => try self.emit("\\\""),
                '\\' => try self.emit("\\\\"),
                '\n' => try self.emit("\\n"),
                '\r' => try self.emit("\\r"),
                '\t' => try self.emit("\\t"),
                else => try self.output.append(c),
            }
        }
    }

    /// Convert TypeScript identifier to Erlang variable (capitalize first letter)
    fn toErlangVar(self: *Self, name: []const u8) ![]const u8 {
        if (name.len == 0) return try self.allocator.dupe(u8, "_");

        // Capitalize first letter
        var result = try self.allocator.alloc(u8, name.len);
        result[0] = std.ascii.toUpper(name[0]);
        @memcpy(result[1..], name[1..]);
        return result;
    }

    fn binaryOpToErlang(self: *Self, op: node_mod.BinaryOp) []const u8 {
        _ = self;
        return switch (op) {
            .assign => "=",
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "rem", // Erlang uses 'rem' for modulo
            .eq => "==",
            .ne => "/=", // Erlang uses /= for not equal
            .lt => "<",
            .le => "=<", // Erlang uses =< not <=
            .gt => ">",
            .ge => ">=",
            .@"and" => "andalso",
            .@"or" => "orelse",
            .bit_and => "band",
            .bit_or => "bor",
            .bit_xor => "bxor",
            .shl => "bsl",
            .shr => "bsr",
        };
    }

    fn unaryOpToErlang(self: *Self, op: node_mod.UnaryOp) []const u8 {
        _ = self;
        return switch (op) {
            .neg => "-",
            .not => "not ",
            .bit_not => "bnot ",
            .typeof => "%% typeof ", // No direct equivalent
            .void => "",
        };
    }

    fn isStringType(self: *Self, node: *ast.Node) bool {
        _ = self;
        // Simplified: check if it's a string literal
        return node.kind == .string_literal;
    }
};
