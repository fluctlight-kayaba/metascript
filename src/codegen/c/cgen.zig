/// C Code Generator - Main Module
/// Direct emission from Typed AST to C (C99+)
///
/// Generates native C code with structs, functions, and basic runtime support.
/// Type mappings:
///   int8/16/32/64 → int8_t/int16_t/int32_t/int64_t
///   uint8/16/32/64 → uint8_t/uint16_t/uint32_t/uint64_t
///   float32 → float
///   float64 → double
///   number → double (default)
///   string → char*
///   boolean → bool
///
/// Module Structure:
///   cgen.zig        - Main generator, orchestration
///   emit.zig        - Low-level output helpers
///   types.zig       - Type mapping helpers
///   declarations.zig - Struct/function declarations
///
/// Usage:
///   var gen = CGenerator.init(allocator);
///   defer gen.deinit();
///   const c_code = try gen.generate(typed_ast);

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const types_mod = @import("../../ast/types.zig");
const node_mod = @import("../../ast/node.zig");

pub const CGenerator = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),
    indent_level: usize,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .output = std.ArrayList(u8).init(allocator),
            .indent_level = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.output.deinit();
    }

    /// Convert BinaryOp enum to C operator string
    fn binaryOpToString(op: node_mod.BinaryOp) []const u8 {
        return switch (op) {
            .assign => "=",
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "%",
            .eq => "==",
            .ne => "!=",
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
        };
    }

    /// Convert UnaryOp enum to C operator string
    fn unaryOpToString(op: node_mod.UnaryOp) []const u8 {
        return switch (op) {
            .neg => "-",
            .not => "!",
            .bit_not => "~",
            .typeof => "typeof",  // Note: typeof not standard C
            .void => "(void)",
        };
    }

    /// Generate C from a typed AST
    pub fn generate(self: *Self, program: *ast.Node) ![]const u8 {
        std.debug.assert(program.kind == .program);

        // Emit includes
        try self.emitIncludes();
        try self.emitNewline();

        // Emit forward declarations for classes/functions
        for (program.data.program.statements) |stmt| {
            if (stmt.kind == .class_decl or stmt.kind == .function_decl) {
                try self.emitNode(stmt);
            }
        }

        // Emit main function containing all code
        try self.emit("int main(void) {\n");
        self.indent_level += 1;

        // Emit program statements inside main
        for (program.data.program.statements) |stmt| {
            // Skip class/function declarations (already emitted)
            if (stmt.kind != .class_decl and stmt.kind != .function_decl) {
                try self.emitIndent();
                try self.emitNode(stmt);
            }
        }

        // Return 0
        try self.emitIndent();
        try self.emit("return 0;\n");

        self.indent_level -= 1;
        try self.emit("}\n");

        return try self.output.toOwnedSlice();
    }

    /// Emit standard C includes
    fn emitIncludes(self: *Self) !void {
        try self.emit("#include <stdio.h>\n");
        try self.emit("#include <stdint.h>\n");
        try self.emit("#include <stdbool.h>\n");
        try self.emit("#include <string.h>\n");
        try self.emit("#include <stdlib.h>\n");
        try self.emit("\n// Metascript ORC Runtime (compile with: -I<metascript>/src/runtime)\n");
        try self.emit("#include \"orc.h\"\n");
        try self.emit("#include \"ms_string.h\"\n");
    }

    /// Main dispatch for node emission
    pub fn emitNode(self: *Self, node: *ast.Node) anyerror!void {
        switch (node.kind) {
            // Declarations
            .class_decl => try self.emitClassDecl(node),
            .function_decl => try self.emitFunctionDecl(node),

            // Statements
            .variable_stmt => try self.emitVariableStmt(node),
            .expression_stmt => try self.emitExpressionStmt(node),
            .block_stmt => try self.emitBlockStmt(node),
            .if_stmt => try self.emitIfStmt(node),
            .while_stmt => try self.emitWhileStmt(node),
            .for_stmt => try self.emitForStmt(node),
            .return_stmt => try self.emitReturnStmt(node),
            .break_stmt => try self.emitBreakStmt(),
            .continue_stmt => try self.emitContinueStmt(),

            // Skip type-only declarations
            .interface_decl, .type_alias_decl => {},

            // Others not implemented yet
            else => {
                // Placeholder comment for unsupported nodes
                try self.emit("// TODO: ");
                try self.emit(@tagName(node.kind));
                try self.emitNewline();
            },
        }
    }

    /// Emit a class as a C struct
    fn emitClassDecl(self: *Self, node: *ast.Node) !void {
        const class = &node.data.class_decl;

        // typedef struct ClassName {
        try self.emit("typedef struct ");
        try self.emit(class.name);
        try self.emit(" {\n");
        self.indent_level += 1;

        // Emit properties as struct fields
        for (class.members) |member| {
            if (member.kind == .property_decl) {
                const prop = &member.data.property_decl;
                try self.emitIndent();
                try self.emitType(prop.type);
                try self.emit(" ");
                try self.emit(prop.name);
                try self.emit(";\n");
            }
        }

        self.indent_level -= 1;
        try self.emit("} ");
        try self.emit(class.name);
        try self.emit(";\n\n");

        // Emit method prototypes
        for (class.members) |member| {
            if (member.kind == .method_decl) {
                try self.emitMethodPrototype(class.name, member);
            }
        }
    }

    /// Emit method prototype: returnType ClassName_methodName(ClassName* this, params...)
    fn emitMethodPrototype(self: *Self, class_name: []const u8, node: *ast.Node) !void {
        const method = &node.data.method_decl;

        // Return type
        if (method.return_type) |ret| {
            try self.emitType(ret);
        } else {
            try self.emit("void");
        }
        try self.emit(" ");

        // ClassName_methodName
        try self.emit(class_name);
        try self.emit("_");
        try self.emit(method.name);
        try self.emit("(");

        // First param: ClassName* this
        try self.emit(class_name);
        try self.emit("* this");

        // Other params
        for (method.params) |param| {
            try self.emit(", ");
            if (param.type) |ptype| {
                try self.emitType(ptype);
            } else {
                try self.emit("void*");
            }
            try self.emit(" ");
            try self.emit(param.name);
        }

        try self.emit(")");

        // For now, just semicolon (no body)
        try self.emit(";\n");
    }

    /// Emit function declaration (placeholder)
    fn emitFunctionDecl(self: *Self, node: *ast.Node) !void {
        const func = &node.data.function_decl;

        // Emit return type
        if (func.return_type) |ret_type| {
            try self.emitType(ret_type);
        } else {
            try self.emit("void");
        }
        try self.emit(" ");

        // Emit function name
        try self.emit(func.name);

        // Emit parameters
        try self.emit("(");
        for (func.params, 0..) |param, i| {
            if (i > 0) try self.emit(", ");

            // Emit parameter type
            if (param.type) |param_type| {
                try self.emitType(param_type);
            } else {
                try self.emit("void*"); // default to void* for untyped params
            }
            try self.emit(" ");

            // Emit parameter name
            try self.emit(param.name);
        }
        try self.emit(")");

        // Emit body if present
        if (func.body) |body| {
            try self.emit(" ");
            if (body.kind == .block_stmt) {
                try self.emitBlockStmt(body);
            } else {
                // Single expression body - wrap in block with return
                try self.emit("{\n");
                self.indent_level += 1;
                try self.emitIndent();
                try self.emit("return ");
                try self.emitExpression(body);
                try self.emit(";\n");
                self.indent_level -= 1;
                try self.emit("}\n");
            }
        } else {
            // Forward declaration
            try self.emit(";\n");
        }
    }

    /// Emit variable statement: const x = ...; or let x = ...;
    fn emitVariableStmt(self: *Self, node: *ast.Node) !void {
        const var_stmt = &node.data.variable_stmt;

        for (var_stmt.declarations) |decl| {
            // Check if initializer is a non-constant object literal
            const needs_split = if (decl.init) |init_expr|
                init_expr.kind == .object_expr and self.objectHasVariableReferences(init_expr)
            else
                false;

            if (needs_split) {
                // Declare variable, then assign fields separately
                if (decl.type) |typ| {
                    try self.emitType(typ);
                } else if (decl.init) |init_expr| {
                    // Try to use the inferred type from the initializer
                    if (init_expr.type) |inferred_type| {
                        try self.emitType(inferred_type);
                    } else if (init_expr.kind == .array_expr) {
                        try self.emit("double");
                    } else if (init_expr.kind == .number_literal) {
                        // Heuristic: if it's an integer literal, use int; otherwise double
                        const num = init_expr.data.number_literal;
                        if (@floor(num) == num and num >= -2147483648 and num <= 2147483647) {
                            try self.emit("int");
                        } else {
                            try self.emit("double");
                        }
                    } else if (init_expr.kind == .member_expr) {
                        // Member access or array indexing - default to double
                        try self.emit("double");
                    } else {
                        try self.emit("void*");
                    }
                } else {
                    try self.emit("void*");
                }
                try self.emit(" ");
                try self.emit(decl.name);

                // Emit array brackets if type is array
                if (decl.type) |typ| {
                    if (typ.kind == .array) {
                        if (decl.init) |init_expr| {
                            if (init_expr.kind == .array_expr) {
                                const array = &init_expr.data.array_expr;
                                const len_str = try std.fmt.allocPrint(self.allocator, "{d}", .{array.elements.len});
                                defer self.allocator.free(len_str);
                                try self.emit("[");
                                try self.emit(len_str);
                                try self.emit("]");
                            }
                        } else {
                            try self.emit("[]");
                        }
                    }
                } else if (decl.init) |init_expr| {
                    if (init_expr.kind == .array_expr) {
                        const array = &init_expr.data.array_expr;
                        const len_str = try std.fmt.allocPrint(self.allocator, "{d}", .{array.elements.len});
                        defer self.allocator.free(len_str);
                        try self.emit("[");
                        try self.emit(len_str);
                        try self.emit("]");
                    }
                }
                try self.emit(";\n");

                // Emit field assignments
                const obj = &decl.init.?.data.object_expr;
                for (obj.properties) |prop| {
                    switch (prop) {
                        .property => |p| {
                            try self.emitIndent();
                            try self.emit(decl.name);
                            try self.emit(".");
                            if (p.key.kind == .identifier) {
                                try self.emit(p.key.data.identifier);
                            }
                            try self.emit(" = ");
                            try self.emitExpression(p.value);
                            try self.emit(";\n");
                        },
                        .spread => {
                            // Spreads should be normalized away
                        },
                    }
                }
            } else {
                // Normal initialization (constants only)
                // Special case: new_expr gets ClassName* type (must check before decl.type)
                if (decl.init) |init_expr| {
                    if (init_expr.kind == .new_expr) {
                        const new_e = &init_expr.data.new_expr;
                        if (new_e.callee.kind == .identifier) {
                            try self.emit(new_e.callee.data.identifier);
                            try self.emit("*");
                        } else {
                            try self.emit("void*");
                        }
                    } else if (decl.type) |typ| {
                        try self.emitType(typ);
                    } else if (init_expr.type) |inferred_type| {
                        // Try to use the inferred type from the initializer
                        try self.emitType(inferred_type);
                    } else if (init_expr.kind == .array_expr) {
                        try self.emit("double");
                    } else if (init_expr.kind == .number_literal) {
                        // Heuristic: if it's an integer literal, use int; otherwise double
                        const num = init_expr.data.number_literal;
                        if (@floor(num) == num and num >= -2147483648 and num <= 2147483647) {
                            try self.emit("int");
                        } else {
                            try self.emit("double");
                        }
                    } else if (init_expr.kind == .member_expr) {
                        // Member access or array indexing - default to double
                        try self.emit("double");
                    } else {
                        try self.emit("void*");
                    }
                } else if (decl.type) |typ| {
                    // No initializer, but have explicit type
                    try self.emitType(typ);
                } else {
                    try self.emit("void*");
                }

                try self.emit(" ");
                try self.emit(decl.name);

                // Emit array brackets if type is array
                if (decl.type) |typ| {
                    if (typ.kind == .array) {
                        if (decl.init) |init_expr2| {
                            if (init_expr2.kind == .array_expr) {
                                const array = &init_expr2.data.array_expr;
                                const len_str = try std.fmt.allocPrint(self.allocator, "{d}", .{array.elements.len});
                                defer self.allocator.free(len_str);
                                try self.emit("[");
                                try self.emit(len_str);
                                try self.emit("]");
                            }
                        } else {
                            try self.emit("[]");
                        }
                    }
                } else if (decl.init) |init_expr| {
                    if (init_expr.kind == .array_expr) {
                        const array = &init_expr.data.array_expr;
                        const len_str = try std.fmt.allocPrint(self.allocator, "{d}", .{array.elements.len});
                        defer self.allocator.free(len_str);
                        try self.emit("[");
                        try self.emit(len_str);
                        try self.emit("]");
                    }
                }

                // Emit initializer if present
                if (decl.init) |init_expr| {
                    try self.emit(" = ");
                    try self.emitExpression(init_expr);
                }

                try self.emit(";\n");
            }
        }
    }

    /// Check if an object literal contains variable references
    fn objectHasVariableReferences(self: *Self, node: *ast.Node) bool {
        _ = self;
        if (node.kind != .object_expr) return false;

        const obj = &node.data.object_expr;
        for (obj.properties) |prop| {
            switch (prop) {
                .property => |p| {
                    if (p.value.kind == .identifier) return true;
                    if (p.value.kind == .member_expr) return true;
                },
                .spread => return true,
            }
        }
        return false;
    }

    /// Emit expression statement: expr;
    fn emitExpressionStmt(self: *Self, node: *ast.Node) !void {
        const expr = node.data.expression_stmt;

        // Map console.log to printf
        if (expr.kind == .call_expr) {
            const call = &expr.data.call_expr;
            if (call.callee.kind == .member_expr) {
                const member = &call.callee.data.member_expr;
                if (member.object.kind == .identifier and
                    std.mem.eql(u8, member.object.data.identifier, "console"))
                {
                    try self.emitConsoleCall(call);
                    return;
                }
            }
        }

        try self.emitExpression(expr);
        try self.emit(";\n");
    }

    /// Emit a block statement
    fn emitBlockStmt(self: *Self, node: *ast.Node) !void {
        const block = &node.data.block_stmt;

        try self.emit("{\n");
        self.indent_level += 1;

        for (block.statements) |stmt| {
            try self.emitIndent();
            try self.emitNode(stmt);
        }

        self.indent_level -= 1;
        try self.emitIndent();
        try self.emit("}\n");
    }

    /// Emit an if statement
    fn emitIfStmt(self: *Self, node: *ast.Node) !void {
        const if_stmt = &node.data.if_stmt;

        try self.emit("if (");
        try self.emitExpression(if_stmt.condition);
        try self.emit(") ");

        // Emit consequent (always present)
        if (if_stmt.consequent.kind == .block_stmt) {
            try self.emitBlockStmt(if_stmt.consequent);
        } else {
            try self.emit("{\n");
            self.indent_level += 1;
            try self.emitIndent();
            try self.emitNode(if_stmt.consequent);
            self.indent_level -= 1;
            try self.emitIndent();
            try self.emit("}\n");
        }

        // Emit alternate (optional else)
        if (if_stmt.alternate) |alternate| {
            try self.emitIndent();
            try self.emit("else ");

            if (alternate.kind == .if_stmt) {
                // else if - don't add extra braces
                try self.emitIfStmt(alternate);
            } else if (alternate.kind == .block_stmt) {
                try self.emitBlockStmt(alternate);
            } else {
                try self.emit("{\n");
                self.indent_level += 1;
                try self.emitIndent();
                try self.emitNode(alternate);
                self.indent_level -= 1;
                try self.emitIndent();
                try self.emit("}\n");
            }
        }
    }

    /// Emit a while loop
    fn emitWhileStmt(self: *Self, node: *ast.Node) !void {
        const while_stmt = &node.data.while_stmt;

        try self.emit("while (");
        try self.emitExpression(while_stmt.condition);
        try self.emit(") ");

        if (while_stmt.body.kind == .block_stmt) {
            try self.emitBlockStmt(while_stmt.body);
        } else {
            try self.emit("{\n");
            self.indent_level += 1;
            try self.emitIndent();
            try self.emitNode(while_stmt.body);
            self.indent_level -= 1;
            try self.emitIndent();
            try self.emit("}\n");
        }
    }

    /// Emit a for loop
    fn emitForStmt(self: *Self, node: *ast.Node) !void {
        const for_stmt = &node.data.for_stmt;

        try self.emit("for (");

        // Init clause
        if (for_stmt.init) |init_node| {
            // For init, we emit the variable declaration or expression without trailing semicolon/newline
            if (init_node.kind == .variable_stmt) {
                const var_stmt = &init_node.data.variable_stmt;
                for (var_stmt.declarations, 0..) |decl, i| {
                    if (i > 0) try self.emit(", ");

                    // Emit type - refine generic 'number' type for integer literals
                    if (decl.type) |typ| {
                        // If type is generic 'number' and init is integer literal, use int
                        if (typ.kind == .number and decl.init != null) {
                            const init_expr = decl.init.?;
                            if (init_expr.kind == .number_literal) {
                                const num = init_expr.data.number_literal;
                                if (@floor(num) == num and num >= -2147483648 and num <= 2147483647) {
                                    try self.emit("int");
                                } else {
                                    try self.emit("double");
                                }
                            } else {
                                try self.emitType(typ);
                            }
                        } else {
                            try self.emitType(typ);
                        }
                    } else if (decl.init) |init_expr| {
                        // No explicit type - try to infer from initializer
                        if (init_expr.type) |inferred_type| {
                            try self.emitType(inferred_type);
                        } else if (init_expr.kind == .number_literal) {
                            const num = init_expr.data.number_literal;
                            if (@floor(num) == num and num >= -2147483648 and num <= 2147483647) {
                                try self.emit("int");
                            } else {
                                try self.emit("double");
                            }
                        } else {
                            try self.emit("double");
                        }
                    } else {
                        try self.emit("double");
                    }

                    try self.emit(" ");
                    try self.emit(decl.name);
                    if (decl.init) |init_expr| {
                        try self.emit(" = ");
                        try self.emitExpression(init_expr);
                    }
                }
            } else {
                try self.emitExpression(init_node);
            }
        }
        try self.emit("; ");

        // Condition clause
        if (for_stmt.condition) |cond| {
            try self.emitExpression(cond);
        }
        try self.emit("; ");

        // Update clause
        if (for_stmt.update) |update| {
            try self.emitExpression(update);
        }
        try self.emit(") ");

        // Body
        if (for_stmt.body.kind == .block_stmt) {
            try self.emitBlockStmt(for_stmt.body);
        } else {
            try self.emit("{\n");
            self.indent_level += 1;
            try self.emitIndent();
            try self.emitNode(for_stmt.body);
            self.indent_level -= 1;
            try self.emitIndent();
            try self.emit("}\n");
        }
    }

    /// Emit a break statement
    fn emitBreakStmt(self: *Self) !void {
        try self.emit("break;\n");
    }

    /// Emit a continue statement
    fn emitContinueStmt(self: *Self) !void {
        try self.emit("continue;\n");
    }

    /// Emit a return statement
    fn emitReturnStmt(self: *Self, node: *ast.Node) !void {
        const ret = &node.data.return_stmt;

        try self.emit("return");
        if (ret.argument) |arg| {
            try self.emit(" ");
            try self.emitExpression(arg);
        }
        try self.emit(";\n");
    }

    /// Emit console.log/error/warn as printf
    fn emitConsoleCall(self: *Self, call: *const node_mod.CallExpr) !void {
        // console.log(...) → printf("...\n")

        // Build format string
        var format_parts = std.ArrayList(u8).init(self.allocator);
        defer format_parts.deinit();

        try format_parts.appendSlice("\"");
        for (call.arguments, 0..) |arg, i| {
            if (i > 0) try format_parts.appendSlice(" ");

            // Determine format specifier based on argument type
            if (arg.type) |arg_type| {
                switch (arg_type.kind) {
                    .number, .float32, .float64 => try format_parts.appendSlice("%g"),
                    .int32, .int64 => try format_parts.appendSlice("%ld"),
                    .string => try format_parts.appendSlice("%s"),
                    .boolean => try format_parts.appendSlice("%d"),
                    .object => {
                        // For objects, we'll print field by field
                        const obj_type = arg_type.data.object;
                        try format_parts.appendSlice("{");
                        for (obj_type.properties, 0..) |prop, j| {
                            if (j > 0) try format_parts.appendSlice(", ");
                            try format_parts.appendSlice(prop.name);
                            try format_parts.appendSlice(": ");
                            // Add format specifier based on property type
                            switch (prop.type.kind) {
                                .number, .float32, .float64 => try format_parts.appendSlice("%g"),
                                .int32, .int64 => try format_parts.appendSlice("%ld"),
                                .string => try format_parts.appendSlice("%s"),
                                .boolean => try format_parts.appendSlice("%d"),
                                else => try format_parts.appendSlice("%g"),
                            }
                        }
                        try format_parts.appendSlice("}");
                    },
                    else => {
                        // For unknown types, default to %g (number) as most common case
                        // %p is rarely what we want for user-facing output
                        try format_parts.appendSlice("%g");
                    },
                }
            } else {
                // Default to number format (common case)
                try format_parts.appendSlice("%g");
            }
        }
        try format_parts.appendSlice("\\n\"");

        // Emit printf call
        try self.emit("printf(");
        try self.emit(format_parts.items);

        // Emit arguments
        for (call.arguments) |arg| {
            if (arg.type) |arg_type| {
                if (arg_type.kind == .object) {
                    // For objects, emit each property value
                    const obj_type = arg_type.data.object;
                    for (obj_type.properties) |prop| {
                        try self.emit(", ");
                        try self.emitExpression(arg);
                        try self.emit(".");
                        try self.emit(prop.name);
                    }
                    continue;
                }
                // Strings: emit directly without cast
                if (arg_type.kind == .string) {
                    try self.emit(", ");
                    try self.emitExpression(arg);
                    continue;
                }
                // Booleans: emit directly without cast
                if (arg_type.kind == .boolean) {
                    try self.emit(", ");
                    try self.emitExpression(arg);
                    continue;
                }
                // For numeric types, cast to double to ensure correct printf behavior
                // This handles the case where we emitted 'int' in C but printf expects 'double' for %g
                if (arg_type.kind == .number or arg_type.kind == .float32 or arg_type.kind == .float64 or
                    arg_type.kind == .int32 or arg_type.kind == .int64)
                {
                    try self.emit(", (double)(");
                    try self.emitExpression(arg);
                    try self.emit(")");
                    continue;
                }
            }
            // For unknown types, cast identifiers to double for safety with %g format
            // (most unknown types are numbers in practice)
            if (arg.kind == .identifier) {
                try self.emit(", (double)(");
                try self.emitExpression(arg);
                try self.emit(")");
                continue;
            }
            try self.emit(", ");
            try self.emitExpression(arg);
        }

        try self.emit(");\n");
    }

    /// Emit an expression (recursive)
    fn emitExpression(self: *Self, node: *ast.Node) anyerror!void {
        switch (node.kind) {
            .number_literal => {
                const value = node.data.number_literal;
                const num_str = try std.fmt.allocPrint(self.allocator, "{d}", .{value});
                defer self.allocator.free(num_str);
                try self.emit(num_str);
            },
            .string_literal => {
                try self.emit("\"");
                try self.emit(node.data.string_literal);
                try self.emit("\"");
            },
            .boolean_literal => {
                if (node.data.boolean_literal) {
                    try self.emit("true");
                } else {
                    try self.emit("false");
                }
            },
            .null_literal => {
                try self.emit("NULL");
            },
            .identifier => {
                try self.emit(node.data.identifier);
            },
            .binary_expr => {
                const binary = &node.data.binary_expr;
                try self.emit("(");
                try self.emitExpression(binary.left);
                try self.emit(" ");
                try self.emit(binaryOpToString(binary.op));
                try self.emit(" ");
                try self.emitExpression(binary.right);
                try self.emit(")");
            },
            .unary_expr => {
                const unary = &node.data.unary_expr;
                try self.emit(unaryOpToString(unary.op));
                try self.emitExpression(unary.argument);
            },
            .object_expr => {
                try self.emitObjectLiteral(node);
            },
            .member_expr => {
                const member = &node.data.member_expr;
                try self.emitExpression(member.object);
                if (member.computed) {
                    try self.emit("[");
                    try self.emitExpression(member.property);
                    try self.emit("]");
                } else {
                    // Use -> for pointer types (class instances allocated with ms_alloc)
                    const is_pointer = if (member.object.type) |obj_type|
                        obj_type.kind == .type_reference or obj_type.kind == .object
                    else
                        false;
                    if (is_pointer) {
                        try self.emit("->");
                    } else {
                        try self.emit(".");
                    }
                    try self.emitExpression(member.property);
                }
            },
            .call_expr => {
                const call = &node.data.call_expr;
                try self.emitExpression(call.callee);
                try self.emit("(");
                for (call.arguments, 0..) |arg, i| {
                    if (i > 0) try self.emit(", ");
                    try self.emitExpression(arg);
                }
                try self.emit(")");
            },
            .array_expr => {
                const array = &node.data.array_expr;
                try self.emit("{");
                for (array.elements, 0..) |elem, i| {
                    if (i > 0) try self.emit(", ");
                    try self.emitExpression(elem);
                }
                try self.emit("}");
            },
            .new_expr => {
                // Emit: (ClassName*)ms_alloc(sizeof(ClassName))
                const new_e = &node.data.new_expr;
                if (new_e.callee.kind == .identifier) {
                    const class_name = new_e.callee.data.identifier;
                    try self.emit("(");
                    try self.emit(class_name);
                    try self.emit("*)ms_alloc(sizeof(");
                    try self.emit(class_name);
                    try self.emit("))");
                } else {
                    try self.emit("/* unsupported: new with non-identifier */");
                }
            },
            else => {
                // Unsupported expression type
                try self.emit("/* unsupported: ");
                try self.emit(@tagName(node.kind));
                try self.emit(" */");
            },
        }
    }

    /// Emit object literal as C compound literal
    fn emitObjectLiteral(self: *Self, node: *ast.Node) !void {
        const obj = &node.data.object_expr;

        // For now, emit as struct initializer
        // Type annotation would help here, but we'll use generic struct syntax
        try self.emit("{ ");

        for (obj.properties, 0..) |prop, i| {
            if (i > 0) try self.emit(", ");

            switch (prop) {
                .property => |p| {
                    // .field_name = value
                    try self.emit(".");
                    if (p.key.kind == .identifier) {
                        try self.emit(p.key.data.identifier);
                    } else {
                        try self.emit("field");
                    }
                    try self.emit(" = ");
                    try self.emitExpression(p.value);
                },
                .spread => {
                    // Spreads should have been normalized away by now
                    try self.emit("/* spread should be normalized */");
                },
            }
        }

        try self.emit(" }");
    }

    /// Emit a type as C type
    fn emitType(self: *Self, typ: ?*types_mod.Type) !void {
        if (typ == null) {
            try self.emit("void");
            return;
        }

        const t = typ.?;
        switch (t.kind) {
            .int8 => try self.emit("int8_t"),
            .int16 => try self.emit("int16_t"),
            .int32 => try self.emit("int32_t"),
            .int64 => try self.emit("int64_t"),
            .uint8 => try self.emit("uint8_t"),
            .uint16 => try self.emit("uint16_t"),
            .uint32 => try self.emit("uint32_t"),
            .uint64 => try self.emit("uint64_t"),
            .float32 => try self.emit("float"),
            .float64 => try self.emit("double"),
            .number => try self.emit("double"),
            .string => try self.emit("char*"),
            .boolean => try self.emit("bool"),
            .void => try self.emit("void"),
            .type_reference => {
                // Type references (e.g., string, number, User)
                const type_ref = t.data.type_reference;

                // Check if resolved to actual type
                if (type_ref.resolved) |resolved| {
                    return try self.emitType(resolved);
                }

                // Map common type names to C types
                const name = type_ref.name;
                if (std.mem.eql(u8, name, "string")) {
                    try self.emit("char*");
                } else if (std.mem.eql(u8, name, "number")) {
                    try self.emit("double");
                } else if (std.mem.eql(u8, name, "boolean")) {
                    try self.emit("bool");
                } else if (std.mem.eql(u8, name, "void")) {
                    try self.emit("void");
                } else if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "int32")) {
                    try self.emit("int32_t");
                } else if (std.mem.eql(u8, name, "int8")) {
                    try self.emit("int8_t");
                } else if (std.mem.eql(u8, name, "int16")) {
                    try self.emit("int16_t");
                } else if (std.mem.eql(u8, name, "int64")) {
                    try self.emit("int64_t");
                } else if (std.mem.eql(u8, name, "uint8")) {
                    try self.emit("uint8_t");
                } else if (std.mem.eql(u8, name, "uint16")) {
                    try self.emit("uint16_t");
                } else if (std.mem.eql(u8, name, "uint32")) {
                    try self.emit("uint32_t");
                } else if (std.mem.eql(u8, name, "uint64")) {
                    try self.emit("uint64_t");
                } else if (std.mem.eql(u8, name, "float") or std.mem.eql(u8, name, "float32")) {
                    try self.emit("float");
                } else if (std.mem.eql(u8, name, "double") or std.mem.eql(u8, name, "float64")) {
                    try self.emit("double");
                } else {
                    // Assume it's a custom type (struct)
                    try self.emit(name);
                    try self.emit("*");
                }
            },
            .object => {
                // Object types: emit as anonymous struct
                const obj = t.data.object;
                try self.emit("struct { ");

                // Emit properties as struct fields
                for (obj.properties, 0..) |prop, i| {
                    if (i > 0) try self.emit("; ");
                    try self.emitType(prop.type);
                    try self.emit(" ");
                    try self.emit(prop.name);
                }

                if (obj.properties.len > 0) {
                    try self.emit("; ");
                }

                try self.emit("}");
            },
            .array => {
                // For arrays, emit just the element type
                // The array brackets will be emitted in variable declaration
                const elem_type = t.data.array;
                try self.emitType(elem_type);
            },
            else => try self.emit("void*"), // Fallback
        }
    }

    /// Low-level emit helpers
    fn emit(self: *Self, text: []const u8) !void {
        try self.output.appendSlice(text);
    }

    fn emitIndent(self: *Self) !void {
        for (0..self.indent_level) |_| {
            try self.emit("    ");
        }
    }

    fn emitNewline(self: *Self) !void {
        try self.emit("\n");
    }
};
