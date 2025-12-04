///! DRC AST Analyzer
///!
///! Walks the typed AST and populates DRC with RC operations.
///! This bridges the gap between the AST and the DRC analysis module.
///!
///! Usage:
///!   var drc = Drc.init(allocator);
///!   defer drc.deinit();
///!
///!   var analyzer = DrcAnalyzer.init(&drc);
///!   try analyzer.analyze(typed_ast);
///!
///!   try drc.finalize();
///!   // Now codegen can query drc.getOpsForLine(line)

const std = @import("std");
const ast = @import("../ast/ast.zig");
const types = @import("../ast/types.zig");
const Drc = @import("drc.zig").Drc;
const ownership = @import("ownership.zig");

pub const DrcAnalyzer = struct {
    drc: *Drc,

    pub fn init(drc: *Drc) DrcAnalyzer {
        return .{ .drc = drc };
    }

    /// Analyze the entire AST
    pub fn analyze(self: *DrcAnalyzer, program: *ast.Node) anyerror!void {
        std.debug.assert(program.kind == .program);

        // Enter global scope
        try self.drc.enterScope(1);

        for (program.data.program.statements) |stmt| {
            try self.analyzeNode(stmt);
        }

        // Exit global scope
        const last_line = self.getLastLine(program);
        try self.drc.exitScope(last_line, 1);
    }

    /// Analyze a single AST node
    fn analyzeNode(self: *DrcAnalyzer, node: *ast.Node) anyerror!void {
        switch (node.kind) {
            .class_decl => {}, // Classes are type declarations, no RC ops
            .function_decl => try self.analyzeFunction(node),
            .variable_stmt => try self.analyzeVariableStmt(node),
            .block_stmt => try self.analyzeBlock(node),
            .if_stmt => try self.analyzeIfStmt(node),
            .while_stmt => try self.analyzeWhileStmt(node),
            .for_stmt => try self.analyzeForStmt(node),
            .return_stmt => try self.analyzeReturnStmt(node),
            .expression_stmt => try self.analyzeExpressionStmt(node),
            else => {},
        }
    }

    /// Analyze a function declaration
    fn analyzeFunction(self: *DrcAnalyzer, node: *ast.Node) anyerror!void {
        const func = &node.data.function_decl;
        const start_line = node.location.start.line;

        // Enter function scope
        try self.drc.enterScope(start_line);

        // Register parameters as borrowed (caller owns them)
        for (func.params) |param| {
            const type_kind = self.getTypeKind(param.type);
            try self.drc.registerVariable(
                param.name,
                type_kind,
                start_line,
                1,
                true, // is_parameter
            );
        }

        // Analyze function body
        if (func.body) |body| {
            try self.analyzeNode(body);
        }

        // Exit function scope
        const end_line = if (func.body) |body| self.getLastLine(body) else start_line;
        try self.drc.exitScope(end_line, 1);
    }

    /// Analyze a variable statement (let/const)
    fn analyzeVariableStmt(self: *DrcAnalyzer, node: *ast.Node) anyerror!void {
        const var_stmt = &node.data.variable_stmt;
        const line = node.location.start.line;
        const col = node.location.start.column;

        for (var_stmt.declarations) |decl| {
            // Determine type kind from initializer or explicit type
            var type_kind = ownership.TypeKind.value;

            if (decl.init) |init_expr| {
                if (init_expr.kind == .new_expr) {
                    // new expressions create object types
                    type_kind = .object;

                    // Register allocation
                    try self.drc.registerAllocation(decl.name, line, col);
                } else {
                    type_kind = self.getExprTypeKind(init_expr);
                }
            } else if (decl.type) |typ| {
                type_kind = self.getTypeKind(typ);
            }

            // Register the variable
            try self.drc.registerVariable(
                decl.name,
                type_kind,
                line,
                col,
                false, // not a parameter
            );

            // Analyze the initializer expression for uses
            if (decl.init) |init_expr| {
                try self.analyzeExpression(init_expr);
            }
        }
    }

    /// Analyze a block statement
    fn analyzeBlock(self: *DrcAnalyzer, node: *ast.Node) anyerror!void {
        const block = &node.data.block_stmt;
        const start_line = node.location.start.line;

        try self.drc.enterScope(start_line);

        for (block.statements) |stmt| {
            try self.analyzeNode(stmt);
        }

        const end_line = self.getLastLine(node);
        try self.drc.exitScope(end_line, 1);
    }

    /// Analyze an if statement
    fn analyzeIfStmt(self: *DrcAnalyzer, node: *ast.Node) anyerror!void {
        const if_stmt = &node.data.if_stmt;

        // Analyze condition
        try self.analyzeExpression(if_stmt.condition);

        // Analyze branches (they create their own scopes if blocks)
        try self.analyzeNode(if_stmt.consequent);

        if (if_stmt.alternate) |alt| {
            try self.analyzeNode(alt);
        }
    }

    /// Analyze a while statement
    fn analyzeWhileStmt(self: *DrcAnalyzer, node: *ast.Node) anyerror!void {
        const while_stmt = &node.data.while_stmt;

        try self.analyzeExpression(while_stmt.condition);
        try self.analyzeNode(while_stmt.body);
    }

    /// Analyze a for statement
    fn analyzeForStmt(self: *DrcAnalyzer, node: *ast.Node) anyerror!void {
        const for_stmt = &node.data.for_stmt;
        const start_line = node.location.start.line;

        // For loops create an implicit scope
        try self.drc.enterScope(start_line);

        // Analyze init/condition/update
        if (for_stmt.init) |init_node| {
            try self.analyzeNode(init_node);
        }
        if (for_stmt.condition) |cond| {
            try self.analyzeExpression(cond);
        }
        if (for_stmt.update) |update| {
            try self.analyzeExpression(update);
        }

        // Analyze body
        try self.analyzeNode(for_stmt.body);

        const end_line = self.getLastLine(node);
        try self.drc.exitScope(end_line, 1);
    }

    /// Analyze a return statement
    fn analyzeReturnStmt(self: *DrcAnalyzer, node: *ast.Node) anyerror!void {
        const ret = &node.data.return_stmt;
        const line = node.location.start.line;
        const col = node.location.start.column;

        if (ret.argument) |arg| {
            // Track returned value
            if (arg.kind == .identifier) {
                const name = arg.data.identifier;
                try self.drc.trackUse(name, line, col, .returned);
            }
            try self.analyzeExpression(arg);
        }
    }

    /// Analyze an expression statement
    fn analyzeExpressionStmt(self: *DrcAnalyzer, node: *ast.Node) anyerror!void {
        const expr = node.data.expression_stmt;
        try self.analyzeExpression(expr);
    }

    /// Analyze an expression for variable uses
    fn analyzeExpression(self: *DrcAnalyzer, node: *ast.Node) anyerror!void {
        const line = node.location.start.line;
        const col = node.location.start.column;

        switch (node.kind) {
            .identifier => {
                const name = node.data.identifier;
                try self.drc.trackUse(name, line, col, .read);
            },
            .binary_expr => {
                const bin = &node.data.binary_expr;
                try self.analyzeExpression(bin.left);
                try self.analyzeExpression(bin.right);

                // Check for assignment to field
                if (bin.op == .assign) {
                    if (bin.left.kind == .member_expr) {
                        // Storing in field - need incref
                        if (bin.right.kind == .identifier) {
                            const name = bin.right.data.identifier;
                            try self.drc.trackUse(name, line, col, .field_store);
                        }
                    } else if (bin.left.kind == .identifier) {
                        // Reassignment
                        const name = bin.left.data.identifier;
                        try self.drc.trackReassignment(name, line, col);
                    }
                }
            },
            .call_expr => {
                const call = &node.data.call_expr;
                try self.analyzeExpression(call.callee);
                for (call.arguments) |arg| {
                    // Track args as borrowed by default
                    if (arg.kind == .identifier) {
                        const name = arg.data.identifier;
                        try self.drc.trackUse(name, line, col, .function_arg_borrowed);
                    }
                    try self.analyzeExpression(arg);
                }
            },
            .member_expr => {
                const mem = &node.data.member_expr;
                try self.analyzeExpression(mem.object);
            },
            .new_expr => {
                // new expressions are handled in variable declaration
            },
            .array_expr => {
                const arr = &node.data.array_expr;
                for (arr.elements) |elem| {
                    try self.analyzeExpression(elem);
                }
            },
            .unary_expr => {
                const un = &node.data.unary_expr;
                try self.analyzeExpression(un.argument);
            },
            else => {},
        }
    }

    /// Get type kind for DRC from a type
    fn getTypeKind(self: *DrcAnalyzer, typ: ?*types.Type) ownership.TypeKind {
        _ = self;
        if (typ == null) return .unknown;

        const t = typ.?;
        return switch (t.kind) {
            .object, .type_reference => .object,
            .array => .array,
            .string => .string,
            .number, .boolean, .void, .unknown => .value,
            .int8, .int16, .int32, .int64 => .value,
            .uint8, .uint16, .uint32, .uint64 => .value,
            .float32, .float64 => .value,
            .function => .function,
            else => .unknown,
        };
    }

    /// Get type kind from an expression
    fn getExprTypeKind(self: *DrcAnalyzer, node: *ast.Node) ownership.TypeKind {
        // Check node's type annotation
        if (node.type) |typ| {
            return self.getTypeKind(typ);
        }

        // Infer from expression kind
        return switch (node.kind) {
            .new_expr, .object_expr => .object,
            .array_expr => .array,
            .string_literal => .string,
            .number_literal, .boolean_literal => .value,
            else => .unknown,
        };
    }

    /// Get the last line of a node (for scope exit)
    fn getLastLine(self: *DrcAnalyzer, node: *ast.Node) u32 {
        _ = self;
        // For blocks, try to get the last statement's line
        if (node.kind == .block_stmt) {
            const block = &node.data.block_stmt;
            if (block.statements.len > 0) {
                return block.statements[block.statements.len - 1].location.start.line;
            }
        } else if (node.kind == .program) {
            const prog = &node.data.program;
            if (prog.statements.len > 0) {
                return prog.statements[prog.statements.len - 1].location.start.line;
            }
        }
        return node.location.start.line;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "DrcAnalyzer: can analyze empty program" {
    const allocator = std.testing.allocator;
    var drc = Drc.init(allocator);
    defer drc.deinit();

    const analyzer = DrcAnalyzer.init(&drc);
    _ = analyzer;

    // Would need a mock AST to test fully
}
