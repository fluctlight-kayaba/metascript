/// AST Pretty Printer
/// Converts AST back to Metascript source code
///
/// Usage:
///   var printer = Printer.init(allocator);
///   defer printer.deinit();
///   const source = try printer.print(ast_node);
///   defer allocator.free(source);

const std = @import("std");
const ast = @import("ast.zig");
const node_mod = @import("node.zig");

pub const PrintError = error{OutOfMemory};

pub const Printer = struct {
    buffer: std.ArrayList(u8),
    indent_level: usize,
    indent_str: []const u8,

    pub fn init(allocator: std.mem.Allocator) Printer {
        return .{
            .buffer = std.ArrayList(u8).init(allocator),
            .indent_level = 0,
            .indent_str = "    ", // 4 spaces
        };
    }

    pub fn deinit(self: *Printer) void {
        self.buffer.deinit();
    }

    /// Print AST to Metascript source code
    pub fn print(self: *Printer, node: *node_mod.Node) ![]const u8 {
        self.buffer.clearRetainingCapacity();
        try self.printNode(node);
        return try self.buffer.toOwnedSlice();
    }

    /// Print without transferring ownership (for debugging)
    pub fn printToSlice(self: *Printer, node: *node_mod.Node) ![]const u8 {
        self.buffer.clearRetainingCapacity();
        try self.printNode(node);
        return self.buffer.items;
    }

    fn printNode(self: *Printer, node: *node_mod.Node) PrintError!void {
        switch (node.kind) {
            .program => try self.printProgram(node),
            .class_decl => try self.printClass(node),
            .property_decl => try self.printProperty(node),
            .method_decl => try self.printMethod(node),
            .function_decl => try self.printFunction(node),
            .block_stmt => try self.printBlock(node),
            .return_stmt => try self.printReturn(node),
            .if_stmt => try self.printIf(node),
            .variable_stmt => try self.printVariable(node),
            .expression_stmt => try self.printExpressionStmt(node),
            .binary_expr => try self.printBinary(node),
            .unary_expr => try self.printUnary(node),
            .call_expr => try self.printCall(node),
            .member_expr => try self.printMember(node),
            .identifier => try self.printIdentifier(node),
            .number_literal => try self.printNumber(node),
            .string_literal => try self.printString(node),
            .boolean_literal => try self.printBoolean(node),
            .null_literal => try self.write("null"),
            .array_expr => try self.printArray(node),
            .object_expr => try self.printObject(node),
            else => try self.write(@tagName(node.kind)),
        }
    }

    fn printProgram(self: *Printer, node: *node_mod.Node) !void {
        for (node.data.program.statements, 0..) |stmt, i| {
            if (i > 0) try self.write("\n");
            try self.printNode(stmt);
        }
    }

    fn printClass(self: *Printer, node: *node_mod.Node) !void {
        const class = &node.data.class_decl;

        // Print decorators
        for (class.decorators) |dec| {
            try self.writeIndent();
            try self.write("@");
            try self.write(dec.name);
            if (dec.arguments.len > 0) {
                try self.write("(");
                for (dec.arguments, 0..) |arg, i| {
                    if (i > 0) try self.write(", ");
                    try self.printNode(arg);
                }
                try self.write(")");
            }
            try self.write("\n");
        }

        // Print class declaration
        try self.writeIndent();
        try self.write("class ");
        try self.write(class.name);

        // Print extends
        if (class.extends) |super_type| {
            try self.write(" extends ");
            try self.printTypeRef(super_type);
        }

        try self.write(" {\n");
        self.indent_level += 1;

        // Print members
        for (class.members, 0..) |member, i| {
            if (i > 0) try self.write("\n");
            try self.printNode(member);
        }

        self.indent_level -= 1;
        try self.writeIndent();
        try self.write("}\n");
    }

    fn printProperty(self: *Printer, node: *node_mod.Node) PrintError!void {
        const prop = &node.data.property_decl;

        try self.writeIndent();

        // Modifiers
        if (prop.readonly) try self.write("readonly ");
        try self.write(prop.name);

        // Type annotation (PropertyDecl.type is *types.Type)
        if (prop.type) |type_info| {
            try self.write(": ");
            try self.printTypeRef(type_info);
        }

        // Initializer
        if (prop.init) |initializer| {
            try self.write(" = ");
            try self.printNode(initializer);
        }

        try self.write(";\n");
    }

    fn printMethod(self: *Printer, node: *node_mod.Node) !void {
        const method = &node.data.method_decl;

        try self.writeIndent();

        // Method name
        try self.write(method.name);
        try self.write("(");

        // Parameters
        for (method.params, 0..) |param, i| {
            if (i > 0) try self.write(", ");
            try self.write(param.name);
            if (param.type) |type_info| {
                try self.write(": ");
                try self.printTypeRef(type_info);
            }
        }

        try self.write(")");

        // Return type
        if (method.return_type) |ret_type| {
            try self.write(": ");
            try self.printTypeRef(ret_type);
        }

        // Body
        if (method.body) |body| {
            try self.write(" ");
            try self.printNode(body);
        } else {
            try self.write(";\n");
        }
    }

    fn printFunction(self: *Printer, node: *node_mod.Node) !void {
        const func = &node.data.function_decl;

        try self.writeIndent();
        try self.write("function ");
        try self.write(func.name);
        try self.write("(");

        // Parameters
        for (func.params, 0..) |param, i| {
            if (i > 0) try self.write(", ");
            try self.write(param.name);
            if (param.type) |type_info| {
                try self.write(": ");
                try self.printTypeRef(type_info);
            }
        }

        try self.write(")");

        // Return type
        if (func.return_type) |ret_type| {
            try self.write(": ");
            try self.printTypeRef(ret_type);
        }

        // Body
        if (func.body) |body| {
            try self.write(" ");
            try self.printNode(body);
        } else {
            try self.write(";\n");
        }
    }

    fn printBlock(self: *Printer, node: *node_mod.Node) !void {
        const block = &node.data.block_stmt;

        try self.write("{\n");
        self.indent_level += 1;

        for (block.statements) |stmt| {
            try self.printNode(stmt);
        }

        self.indent_level -= 1;
        try self.writeIndent();
        try self.write("}\n");
    }

    fn printReturn(self: *Printer, node: *node_mod.Node) !void {
        try self.writeIndent();
        try self.write("return");

        if (node.data.return_stmt.argument) |arg| {
            try self.write(" ");
            try self.printNode(arg);
        }

        try self.write(";\n");
    }

    fn printIf(self: *Printer, node: *node_mod.Node) !void {
        const if_stmt = &node.data.if_stmt;

        try self.writeIndent();
        try self.write("if (");
        try self.printNode(if_stmt.condition);
        try self.write(") ");
        try self.printNode(if_stmt.consequent);

        if (if_stmt.alternate) |alternate| {
            try self.writeIndent();
            try self.write("else ");
            try self.printNode(alternate);
        }
    }

    fn printVariable(self: *Printer, node: *node_mod.Node) !void {
        const var_stmt = &node.data.variable_stmt;

        try self.writeIndent();

        // const or let
        switch (var_stmt.kind) {
            .@"const" => try self.write("const "),
            .let => try self.write("let "),
            .@"var" => try self.write("var "),
        }

        // Print each declaration
        for (var_stmt.declarations, 0..) |decl, i| {
            if (i > 0) try self.write(", ");
            try self.write(decl.name);

            // Type annotation
            if (decl.type) |type_info| {
                try self.write(": ");
                try self.printTypeRef(type_info);
            }

            // Initializer
            if (decl.init) |initializer| {
                try self.write(" = ");
                try self.printNode(initializer);
            }
        }

        try self.write(";\n");
    }

    fn printExpressionStmt(self: *Printer, node: *node_mod.Node) !void {
        try self.writeIndent();
        // expression_stmt is just *Node (the expression itself)
        try self.printNode(node.data.expression_stmt);
        try self.write(";\n");
    }

    fn printBinary(self: *Printer, node: *node_mod.Node) !void {
        const binary = &node.data.binary_expr;

        try self.printNode(binary.left);
        try self.write(" ");
        // Convert operator enum to string
        const op_str = switch (binary.op) {
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
        };
        try self.write(op_str);
        try self.write(" ");
        try self.printNode(binary.right);
    }

    fn printUnary(self: *Printer, node: *node_mod.Node) !void {
        const unary = &node.data.unary_expr;

        // Print operator
        try self.write(@tagName(unary.op));
        try self.printNode(unary.argument);
    }

    fn printCall(self: *Printer, node: *node_mod.Node) !void {
        const call = &node.data.call_expr;

        try self.printNode(call.callee);
        try self.write("(");

        for (call.arguments, 0..) |arg, i| {
            if (i > 0) try self.write(", ");
            try self.printNode(arg);
        }

        try self.write(")");
    }

    fn printMember(self: *Printer, node: *node_mod.Node) !void {
        const member = &node.data.member_expr;

        try self.printNode(member.object);
        try self.write(".");
        try self.printNode(member.property);
    }

    fn printIdentifier(self: *Printer, node: *node_mod.Node) !void {
        try self.write(node.data.identifier);
    }

    fn printNumber(self: *Printer, node: *node_mod.Node) !void {
        var buf: [32]u8 = undefined;
        const num = node.data.number_literal;
        const result = std.fmt.formatFloat(buf[0..], num, .{ .mode = .decimal }) catch {
            try self.write("0");
            return;
        };
        try self.write(result);
    }

    fn printString(self: *Printer, node: *node_mod.Node) !void {
        try self.write("\"");
        try self.write(node.data.string_literal);
        try self.write("\"");
    }

    fn printBoolean(self: *Printer, node: *node_mod.Node) !void {
        if (node.data.boolean_literal) {
            try self.write("true");
        } else {
            try self.write("false");
        }
    }

    fn printArray(self: *Printer, node: *node_mod.Node) !void {
        const arr = &node.data.array_expr;

        try self.write("[");
        for (arr.elements, 0..) |elem, i| {
            if (i > 0) try self.write(", ");
            try self.printNode(elem);
        }
        try self.write("]");
    }

    fn printObject(self: *Printer, node: *node_mod.Node) !void {
        const obj = &node.data.object_expr;

        try self.write("{ ");
        for (obj.properties, 0..) |prop, i| {
            if (i > 0) try self.write(", ");
            try self.printNode(prop.key);
            if (!prop.shorthand) {
                try self.write(": ");
                try self.printNode(prop.value);
            }
        }
        try self.write(" }");
    }

    fn printType(self: *Printer, type_node: *node_mod.Node) PrintError!void {
        switch (type_node.kind) {
            .identifier => {
                try self.write(type_node.data.identifier);
            },
            else => {
                try self.write(@tagName(type_node.kind));
            },
        }
    }

    /// Print a types.Type reference
    fn printTypeRef(self: *Printer, type_info: *const ast.types.Type) PrintError!void {
        switch (type_info.kind) {
            .type_reference => {
                const ref = type_info.data.type_reference;
                try self.write(ref.name);
                if (ref.type_args.len > 0) {
                    try self.write("<");
                    for (ref.type_args, 0..) |arg, i| {
                        if (i > 0) try self.write(", ");
                        try self.printTypeRef(arg);
                    }
                    try self.write(">");
                }
            },
            .number => try self.write("number"),
            .string => try self.write("string"),
            .boolean => try self.write("boolean"),
            .void => try self.write("void"),
            .unknown => try self.write("unknown"),
            .never => try self.write("never"),
            else => try self.write(@tagName(type_info.kind)),
        }
    }

    fn write(self: *Printer, str: []const u8) !void {
        try self.buffer.appendSlice(str);
    }

    fn writeIndent(self: *Printer) !void {
        for (0..self.indent_level) |_| {
            try self.buffer.appendSlice(self.indent_str);
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "print simple class" {
    // Test requires building AST manually - skip for now
}
