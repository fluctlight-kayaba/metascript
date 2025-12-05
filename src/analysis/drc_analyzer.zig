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
const ast_node = @import("../ast/node.zig");
const types = @import("../ast/types.zig");
const drc_mod = @import("drc.zig");
const Drc = drc_mod.Drc;
const Lifetime = drc_mod.Lifetime;
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
            var is_copy_from_var = false;
            var source_var_name: ?[]const u8 = null;
            var creates_allocation = false;

            if (decl.init) |init_expr| {
                if (init_expr.kind == .new_expr) {
                    // new expressions create object types
                    type_kind = .object;
                    creates_allocation = true;
                } else if (init_expr.kind == .string_literal) {
                    // String literals are INTERNED - they don't need RC!
                    // The interned string pool manages them statically with infinite lifetime.
                    //
                    // NOTE: This is semantically a "hack" - string literals produce msString*
                    // which is technically a reference type. However, since interned strings:
                    // 1. Are allocated at compile-time in static storage
                    // 2. Never need to be freed (program lifetime)
                    // 3. Have RC that's never modified (conceptually infinite)
                    // We mark them as .value to skip RC operations entirely.
                    //
                    // This is safe because ms_intern_get() returns a borrowed reference
                    // to statically-allocated storage.
                    type_kind = .value; // Treat as value type to skip RC
                } else if (init_expr.kind == .identifier) {
                    // Variable-to-variable copy: let x = y
                    type_kind = self.getExprTypeKind(init_expr);
                    if (type_kind == .object or type_kind == .array or type_kind == .string) {
                        is_copy_from_var = true;
                        source_var_name = init_expr.data.identifier;
                    }
                } else if (init_expr.kind == .binary_expr) {
                    // Binary expressions may create allocations
                    type_kind = self.getExprTypeKind(init_expr);
                    // String concatenation creates a new heap string
                    if (type_kind == .string) {
                        const bin = &init_expr.data.binary_expr;
                        if (bin.op == .add) {
                            creates_allocation = true;
                        }
                    }
                } else if (init_expr.kind == .call_expr) {
                    // Function calls may return heap-allocated values
                    type_kind = self.getExprTypeKind(init_expr);
                    // If result is reference type, assume it's allocated
                    if (type_kind == .object or type_kind == .array or type_kind == .string) {
                        creates_allocation = true;
                    }
                } else {
                    type_kind = self.getExprTypeKind(init_expr);
                }
            } else if (decl.type) |typ| {
                type_kind = self.getTypeKind(typ);
            }

            // Register allocation if this expression creates heap memory
            if (creates_allocation) {
                try self.drc.registerAllocation(decl.name, line, col);
            }

            // Register the variable
            try self.drc.registerVariable(
                decl.name,
                type_kind,
                line,
                col,
                false, // not a parameter
            );

            // If this is a variable copy, register the incref
            if (is_copy_from_var) {
                try self.drc.trackVariableCopy(
                    decl.name,
                    source_var_name.?,
                    line,
                    col,
                );
            }

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
    /// CRITICAL: Must emit cleanup for all owned variables EXCEPT the returned one
    fn analyzeReturnStmt(self: *DrcAnalyzer, node: *ast.Node) anyerror!void {
        const ret = &node.data.return_stmt;
        const line = node.location.start.line;
        const col = node.location.start.column;

        // Get the returned variable name (if returning an identifier)
        var returned_var_name: ?[]const u8 = null;
        if (ret.argument) |arg| {
            if (arg.kind == .identifier) {
                returned_var_name = arg.data.identifier;
                try self.drc.trackUse(returned_var_name.?, line, col, .returned);
            }
            try self.analyzeExpression(arg);
        }

        // Emit cleanup for all other owned variables in current scope
        // This handles early returns that skip normal scope exit
        try self.drc.emitEarlyReturnCleanup(line, col, returned_var_name);
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

                // Check for assignment to field or variable
                if (bin.op == .assign) {
                    if (bin.left.kind == .member_expr) {
                        // Storing in field - need incref
                        if (bin.right.kind == .identifier) {
                            const name = bin.right.data.identifier;
                            try self.drc.trackUse(name, line, col, .field_store);
                        }
                    } else if (bin.left.kind == .identifier) {
                        // Reassignment: x = expr
                        const name = bin.left.data.identifier;
                        try self.drc.trackReassignment(name, line, col);

                        // CRITICAL: If RHS is a variable, we need to incref it
                        // because now two variables point to the same value
                        if (bin.right.kind == .identifier) {
                            const source = bin.right.data.identifier;
                            try self.drc.trackVariableCopy(name, source, line, col);
                        }
                    }
                }
            },
            .call_expr => {
                const call = &node.data.call_expr;
                try self.analyzeExpression(call.callee);

                // CRITICAL: Count occurrences of each variable in this call's arguments
                // to prevent incorrect move optimization on duplicates like f(x, x)
                for (call.arguments, 0..) |arg, arg_idx| {
                    // Use lifetime system for function arguments (Lobster-style)
                    if (arg.kind == .identifier) {
                        const name = arg.data.identifier;
                        const arg_line = arg.location.start.line;
                        const arg_col = arg.location.start.column;

                        // Count how many times this variable appears in the call args
                        var occurrence_count: usize = 0;
                        for (call.arguments) |other_arg| {
                            if (other_arg.kind == .identifier) {
                                if (std.mem.eql(u8, other_arg.data.identifier, name)) {
                                    occurrence_count += 1;
                                }
                            }
                        }

                        // Get the variable's current lifetime
                        const var_lt = self.drc.getVariableLifetime(name);

                        // Infer what the callee expects (default: borrow)
                        const callee_lt = self.inferCalleeArgLifetime(call, arg_idx);

                        // LOBSTER OPTIMIZATION: Last-use move for ownership-taking calls
                        // If callee takes ownership (KEEP) AND it's the last use,
                        // we can move instead of copy (no incref needed, mark as moved).
                        // NOTE: We do NOT apply this for borrowing calls - even if it's
                        // the last use, we still own the value and need to clean it up.
                        // ALSO: Cannot move if variable appears multiple times in args!
                        const is_last_use = self.drc.ownership_analyzer.isLastUse(name, arg_line, arg_col);
                        const is_owned_to_keep = (var_lt == .keep and callee_lt == .keep);
                        const is_single_occurrence = (occurrence_count == 1);

                        if (is_last_use and is_owned_to_keep and is_single_occurrence) {
                            // Last use of owned → keep: transfer ownership (no incref)
                            // Mark as moved to skip scope cleanup decref
                            if (self.drc.variables.getPtr(name)) |vptr| {
                                vptr.ownership_state = .moved;
                                self.drc.stats.ops_elided += 1; // Elided scope decref
                                self.drc.stats.moves += 1;
                            }
                            // No RC ops needed - just pass the value
                            try self.analyzeExpression(arg);
                            continue;
                        }

                        // Adjust lifetime if needed (may emit incref/decref)
                        const adjusted = try self.drc.adjustLifetime(
                            var_lt,
                            callee_lt,
                            name,
                            arg_line,
                            arg_col,
                        );

                        // If no adjustment needed, track as borrowed or owned
                        if (!adjusted) {
                            const context: Drc.UseContext = if (callee_lt == .keep)
                                .function_arg_owned
                            else
                                .function_arg_borrowed;
                            try self.drc.trackUse(name, arg_line, arg_col, context);
                        }
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
        // Check node's type annotation first (most reliable)
        if (node.type) |typ| {
            return self.getTypeKind(typ);
        }

        // Infer from expression kind
        return switch (node.kind) {
            .new_expr, .object_expr => .object,
            .array_expr => .array,
            .string_literal => .string,
            .number_literal, .boolean_literal => .value,
            .binary_expr => {
                // For binary expressions, infer from operands
                const bin = &node.data.binary_expr;
                const left_kind = self.getExprTypeKind(bin.left);
                const right_kind = self.getExprTypeKind(bin.right);
                // String + anything = string
                if (left_kind == .string or right_kind == .string) {
                    return .string;
                }
                // Otherwise use left operand type
                return left_kind;
            },
            .call_expr => {
                // For function calls, check the callee's return type
                // For now, default to unknown (needs type info from checker)
                return .unknown;
            },
            .identifier => {
                // For identifiers, we need the variable's type
                // This should be resolved from the type checker
                return .unknown;
            },
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

    // ========================================================================
    // Lifetime Inference (Lobster-style)
    // ========================================================================

    /// Infer what lifetime a callee expects for an argument
    /// This is the key to Lobster's ownership inference.
    ///
    /// Rules (from Lobster):
    /// 1. If the function is known to store the arg (e.g., container.push) → LT_KEEP
    /// 2. If the function modifies in-place → LT_KEEP
    /// 3. Default: LT_BORROW (caller retains ownership)
    ///
    /// In the future, this should read from function signatures/attributes.
    fn inferCalleeArgLifetime(self: *DrcAnalyzer, call: *const ast_node.CallExpr, arg_idx: usize) Lifetime {
        _ = arg_idx;

        // Check if we have type info on the callee
        if (call.callee.type) |callee_type| {
            if (callee_type.kind == .function) {
                // Could look at function signature here
                // For now, default to borrow
                _ = self;
            }
        }

        // Check callee name for well-known ownership-taking functions
        if (call.callee.kind == .member_expr) {
            const member = &call.callee.data.member_expr;
            // For non-computed member access (obj.method), property is an identifier
            if (!member.computed and member.property.kind == .identifier) {
                const prop_name = member.property.data.identifier;

                // Container methods that take ownership of their arguments
                if (std.mem.eql(u8, prop_name, "push") or
                    std.mem.eql(u8, prop_name, "append") or
                    std.mem.eql(u8, prop_name, "insert") or
                    std.mem.eql(u8, prop_name, "set") or
                    std.mem.eql(u8, prop_name, "add"))
                {
                    return .keep; // Container takes ownership
                }
            }
        }

        // Default: borrow (caller retains ownership)
        // This is safe and matches TypeScript semantics
        return .borrow;
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
