// Var Hoisting Transform
// Implements JavaScript-style var hoisting for TypeScript compatibility
//
// In JavaScript, `var` declarations are hoisted to the top of their function scope.
// This transform rewrites:
//
//   function foo() {
//       console.log(x);  // undefined (not ReferenceError)
//       var x = 5;
//       if (cond) {
//           var y = 10;  // hoisted to function scope
//       }
//       console.log(y);  // accessible
//   }
//
// To:
//
//   function foo() {
//       var x;           // hoisted declaration (deduplicated)
//       var y;           // hoisted declaration
//       console.log(x);
//       x = 5;           // assignment stays in place
//       if (cond) {
//           y = 10;
//       }
//       console.log(y);
//   }
//
// Edge cases handled:
// - Duplicate var declarations (deduplicated)
// - For loop initializers (hoisted)
// - Nested functions (NOT hoisted - separate scope)
// - Var without initializer (hoisted, original removed)

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const node_mod = @import("../../ast/node.zig");
const pipeline = @import("../pipeline.zig");

const TransformContext = pipeline.TransformContext;
const TransformResult = pipeline.TransformResult;
const TransformError = pipeline.TransformError;

/// Collected var declarations for hoisting
const VarDecl = struct {
    name: []const u8,
    location: ast.SourceLocation,
};

/// Transform entry point - called by pipeline
pub fn transform(ctx: *TransformContext, root: *node_mod.Node) TransformError!TransformResult {
    return hoistVars(ctx, root);
}

/// Main hoisting logic - finds function scopes and hoists vars within them
fn hoistVars(ctx: *TransformContext, root: *node_mod.Node) TransformError!TransformResult {
    var changed = false;
    var total_hoisted: usize = 0;

    // Walk the AST looking for function scopes
    switch (root.kind) {
        .program => {
            const program = &root.data.program;
            for (program.statements, 0..) |stmt, i| {
                const result = try hoistVars(ctx, stmt);
                if (result.changed) {
                    program.statements[i] = result.node;
                    changed = true;
                    total_hoisted += result.transform_count;
                }
            }
        },
        .function_decl => {
            if (root.data.function_decl.body) |body| {
                const result = try hoistInFunction(ctx, body);
                if (result.changed) {
                    root.data.function_decl.body = result.node;
                    changed = true;
                    total_hoisted += result.transform_count;
                }
            }
        },
        .function_expr => {
            const result = try hoistInFunction(ctx, root.data.function_expr.body);
            if (result.changed) {
                root.data.function_expr.body = result.node;
                changed = true;
                total_hoisted += result.transform_count;
            }
        },
        .method_decl => {
            if (root.data.method_decl.body) |body| {
                const result = try hoistInFunction(ctx, body);
                if (result.changed) {
                    root.data.method_decl.body = result.node;
                    changed = true;
                    total_hoisted += result.transform_count;
                }
            }
        },
        .constructor_decl => {
            const result = try hoistInFunction(ctx, root.data.constructor_decl.body);
            if (result.changed) {
                root.data.constructor_decl.body = result.node;
                changed = true;
                total_hoisted += result.transform_count;
            }
        },
        .class_decl => {
            const class = &root.data.class_decl;
            for (class.members, 0..) |member, i| {
                const result = try hoistVars(ctx, member);
                if (result.changed) {
                    class.members[i] = result.node;
                    changed = true;
                    total_hoisted += result.transform_count;
                }
            }
        },
        .block_stmt => {
            // Non-function blocks: recurse but don't hoist here
            const block = &root.data.block_stmt;
            for (block.statements, 0..) |stmt, i| {
                const result = try hoistVars(ctx, stmt);
                if (result.changed) {
                    block.statements[i] = result.node;
                    changed = true;
                    total_hoisted += result.transform_count;
                }
            }
        },
        .if_stmt => {
            const if_s = &root.data.if_stmt;
            var result = try hoistVars(ctx, if_s.consequent);
            if (result.changed) {
                if_s.consequent = result.node;
                changed = true;
                total_hoisted += result.transform_count;
            }
            if (if_s.alternate) |alt| {
                result = try hoistVars(ctx, alt);
                if (result.changed) {
                    if_s.alternate = result.node;
                    changed = true;
                    total_hoisted += result.transform_count;
                }
            }
        },
        .while_stmt => {
            const result = try hoistVars(ctx, root.data.while_stmt.body);
            if (result.changed) {
                root.data.while_stmt.body = result.node;
                changed = true;
                total_hoisted += result.transform_count;
            }
        },
        .for_stmt => {
            const result = try hoistVars(ctx, root.data.for_stmt.body);
            if (result.changed) {
                root.data.for_stmt.body = result.node;
                changed = true;
                total_hoisted += result.transform_count;
            }
        },
        else => {},
    }

    return .{
        .node = root,
        .changed = changed,
        .transform_count = total_hoisted,
    };
}

/// Hoist var declarations within a function body
fn hoistInFunction(ctx: *TransformContext, body: *node_mod.Node) TransformError!TransformResult {
    if (body.kind != .block_stmt) {
        return .{ .node = body, .changed = false, .transform_count = 0 };
    }

    // Collect all var declarations in this function scope (deduplicated)
    var seen_names = std.StringHashMap(void).init(ctx.allocator);
    defer seen_names.deinit();

    var vars = std.ArrayList(VarDecl).init(ctx.allocator);
    defer vars.deinit();

    collectVarDecls(body, &vars, &seen_names);

    if (vars.items.len == 0) {
        return .{ .node = body, .changed = false, .transform_count = 0 };
    }

    // Build new statements: hoisted declarations + transformed original statements
    const block = &body.data.block_stmt;
    const hoisted_count = vars.items.len;

    var new_statements = std.ArrayList(*node_mod.Node).init(ctx.allocator);
    defer new_statements.deinit();

    // Add hoisted var declarations (without initializers, deduplicated)
    for (vars.items) |v| {
        const hoisted_decl = ctx.arena.createNode(
            .variable_stmt,
            v.location,
            .{
                .variable_stmt = .{
                    .kind = .@"var",
                    .declarations = ctx.arena.allocator().alloc(node_mod.VariableStmt.VariableDeclarator, 1) catch return error.OutOfMemory,
                },
            },
        ) catch return error.OutOfMemory;

        hoisted_decl.data.variable_stmt.declarations[0] = .{
            .name = v.name,
            .type = null,
            .init = null, // No initializer - hoisted
        };

        new_statements.append(hoisted_decl) catch return error.OutOfMemory;
    }

    // Process original statements, converting var declarations to assignments
    for (block.statements) |stmt| {
        try convertVarToAssignments(ctx, stmt, &new_statements);
    }

    // Create new block with hoisted declarations
    const new_block = ctx.arena.createNode(
        .block_stmt,
        body.location,
        .{
            .block_stmt = .{
                .statements = new_statements.toOwnedSlice() catch return error.OutOfMemory,
            },
        },
    ) catch return error.OutOfMemory;

    ctx.stats.transforms_applied += hoisted_count;

    return .{
        .node = new_block,
        .changed = true,
        .transform_count = hoisted_count,
    };
}

/// Recursively collect all var declarations in a scope (deduplicated, excludes nested functions)
fn collectVarDecls(node_ptr: *node_mod.Node, vars: *std.ArrayList(VarDecl), seen: *std.StringHashMap(void)) void {
    switch (node_ptr.kind) {
        .variable_stmt => {
            const var_s = &node_ptr.data.variable_stmt;
            if (var_s.kind == .@"var") {
                for (var_s.declarations) |decl| {
                    // Deduplicate: only add if not already seen
                    if (seen.get(decl.name) == null) {
                        seen.put(decl.name, {}) catch {};
                        vars.append(.{
                            .name = decl.name,
                            .location = node_ptr.location,
                        }) catch {};
                    }
                }
            }
        },
        .block_stmt => {
            for (node_ptr.data.block_stmt.statements) |stmt| {
                collectVarDecls(stmt, vars, seen);
            }
        },
        .if_stmt => {
            collectVarDecls(node_ptr.data.if_stmt.consequent, vars, seen);
            if (node_ptr.data.if_stmt.alternate) |alt| {
                collectVarDecls(alt, vars, seen);
            }
        },
        .while_stmt => {
            collectVarDecls(node_ptr.data.while_stmt.body, vars, seen);
        },
        .for_stmt => {
            // For loop initializers ARE hoisted in JavaScript
            if (node_ptr.data.for_stmt.init) |init| {
                collectVarDecls(init, vars, seen);
            }
            collectVarDecls(node_ptr.data.for_stmt.body, vars, seen);
        },
        // Don't descend into nested functions - they have their own scope
        .function_decl, .function_expr, .method_decl, .constructor_decl => {},
        else => {},
    }
}

/// Convert var declarations to assignments and append to statement list
/// This removes the var keyword and keeps only the assignment
fn convertVarToAssignments(ctx: *TransformContext, node_ptr: *node_mod.Node, out: *std.ArrayList(*node_mod.Node)) TransformError!void {
    switch (node_ptr.kind) {
        .variable_stmt => {
            const var_s = &node_ptr.data.variable_stmt;
            if (var_s.kind != .@"var") {
                // let/const pass through unchanged
                out.append(node_ptr) catch return error.OutOfMemory;
                return;
            }

            // Convert each var declaration with initializer to assignment
            for (var_s.declarations) |decl| {
                if (decl.init) |init| {
                    // Create: varName = init;
                    const ident = ctx.arena.createNode(
                        .identifier,
                        node_ptr.location,
                        .{ .identifier = decl.name },
                    ) catch return error.OutOfMemory;

                    const assign = ctx.arena.createNode(
                        .binary_expr,
                        node_ptr.location,
                        .{
                            .binary_expr = .{
                                .op = .assign,
                                .left = ident,
                                .right = init,
                            },
                        },
                    ) catch return error.OutOfMemory;

                    const expr_stmt = ctx.arena.createNode(
                        .expression_stmt,
                        node_ptr.location,
                        .{ .expression_stmt = assign },
                    ) catch return error.OutOfMemory;

                    out.append(expr_stmt) catch return error.OutOfMemory;
                }
                // Var without initializer: already hoisted, nothing to emit here
            }
        },
        .block_stmt => {
            // Recursively process block contents
            const block = &node_ptr.data.block_stmt;
            var new_stmts = std.ArrayList(*node_mod.Node).init(ctx.allocator);
            defer new_stmts.deinit();

            for (block.statements) |stmt| {
                try convertVarToAssignments(ctx, stmt, &new_stmts);
            }

            // Only emit block if it has statements
            if (new_stmts.items.len > 0) {
                node_ptr.data.block_stmt.statements = new_stmts.toOwnedSlice() catch return error.OutOfMemory;
                out.append(node_ptr) catch return error.OutOfMemory;
            }
        },
        .if_stmt => {
            const if_s = &node_ptr.data.if_stmt;

            // Convert consequent
            var cons_stmts = std.ArrayList(*node_mod.Node).init(ctx.allocator);
            defer cons_stmts.deinit();
            try convertVarToAssignments(ctx, if_s.consequent, &cons_stmts);

            if (cons_stmts.items.len == 1) {
                if_s.consequent = cons_stmts.items[0];
            } else if (cons_stmts.items.len > 1) {
                if_s.consequent = ctx.arena.createNode(
                    .block_stmt,
                    if_s.consequent.location,
                    .{ .block_stmt = .{ .statements = cons_stmts.toOwnedSlice() catch return error.OutOfMemory } },
                ) catch return error.OutOfMemory;
            }

            // Convert alternate if present
            if (if_s.alternate) |alt| {
                var alt_stmts = std.ArrayList(*node_mod.Node).init(ctx.allocator);
                defer alt_stmts.deinit();
                try convertVarToAssignments(ctx, alt, &alt_stmts);

                if (alt_stmts.items.len == 1) {
                    if_s.alternate = alt_stmts.items[0];
                } else if (alt_stmts.items.len > 1) {
                    if_s.alternate = ctx.arena.createNode(
                        .block_stmt,
                        alt.location,
                        .{ .block_stmt = .{ .statements = alt_stmts.toOwnedSlice() catch return error.OutOfMemory } },
                    ) catch return error.OutOfMemory;
                } else {
                    if_s.alternate = null;
                }
            }

            out.append(node_ptr) catch return error.OutOfMemory;
        },
        .while_stmt => {
            var body_stmts = std.ArrayList(*node_mod.Node).init(ctx.allocator);
            defer body_stmts.deinit();
            try convertVarToAssignments(ctx, node_ptr.data.while_stmt.body, &body_stmts);

            if (body_stmts.items.len == 1) {
                node_ptr.data.while_stmt.body = body_stmts.items[0];
            } else if (body_stmts.items.len > 1) {
                node_ptr.data.while_stmt.body = ctx.arena.createNode(
                    .block_stmt,
                    node_ptr.data.while_stmt.body.location,
                    .{ .block_stmt = .{ .statements = body_stmts.toOwnedSlice() catch return error.OutOfMemory } },
                ) catch return error.OutOfMemory;
            }

            out.append(node_ptr) catch return error.OutOfMemory;
        },
        .for_stmt => {
            // Handle for loop initializer (var i = 0 -> i = 0)
            if (node_ptr.data.for_stmt.init) |init| {
                if (init.kind == .variable_stmt and init.data.variable_stmt.kind == .@"var") {
                    // Convert var in for init to assignment expression
                    const var_s = &init.data.variable_stmt;
                    if (var_s.declarations.len > 0 and var_s.declarations[0].init != null) {
                        const decl = var_s.declarations[0];
                        const ident = ctx.arena.createNode(
                            .identifier,
                            init.location,
                            .{ .identifier = decl.name },
                        ) catch return error.OutOfMemory;

                        const assign = ctx.arena.createNode(
                            .binary_expr,
                            init.location,
                            .{
                                .binary_expr = .{
                                    .op = .assign,
                                    .left = ident,
                                    .right = decl.init.?,
                                },
                            },
                        ) catch return error.OutOfMemory;

                        node_ptr.data.for_stmt.init = assign;
                    } else {
                        node_ptr.data.for_stmt.init = null;
                    }
                }
            }

            // Handle for loop body
            var body_stmts = std.ArrayList(*node_mod.Node).init(ctx.allocator);
            defer body_stmts.deinit();
            try convertVarToAssignments(ctx, node_ptr.data.for_stmt.body, &body_stmts);

            if (body_stmts.items.len == 1) {
                node_ptr.data.for_stmt.body = body_stmts.items[0];
            } else if (body_stmts.items.len > 1) {
                node_ptr.data.for_stmt.body = ctx.arena.createNode(
                    .block_stmt,
                    node_ptr.data.for_stmt.body.location,
                    .{ .block_stmt = .{ .statements = body_stmts.toOwnedSlice() catch return error.OutOfMemory } },
                ) catch return error.OutOfMemory;
            }

            out.append(node_ptr) catch return error.OutOfMemory;
        },
        // Don't transform inside nested functions - they have their own hoisting
        .function_decl, .function_expr, .method_decl, .constructor_decl => {
            out.append(node_ptr) catch return error.OutOfMemory;
        },
        else => {
            out.append(node_ptr) catch return error.OutOfMemory;
        },
    }
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;
const ast_arena = @import("../../ast/ast.zig").ASTArena;
const location = @import("../../ast/location.zig");

fn createTestContext() !struct { arena: ast_arena, ctx: TransformContext } {
    var arena = ast_arena.init(testing.allocator);
    const ctx = TransformContext.init(&arena, testing.allocator, "test.ms");
    return .{ .arena = arena, .ctx = ctx };
}

fn createVarDecl(arena: *ast_arena, name: []const u8, init_val: ?f64) !*node_mod.Node {
    const loc = location.SourceLocation.dummy();

    var decls = try arena.allocator().alloc(node_mod.VariableStmt.VariableDeclarator, 1);
    decls[0] = .{
        .name = name,
        .type = null,
        .init = if (init_val) |val| try arena.createNode(
            .number_literal,
            loc,
            .{ .number_literal = val },
        ) else null,
    };

    return arena.createNode(.variable_stmt, loc, .{
        .variable_stmt = .{
            .kind = .@"var",
            .declarations = decls,
        },
    });
}

fn createLetDecl(arena: *ast_arena, name: []const u8, init_val: f64) !*node_mod.Node {
    const loc = location.SourceLocation.dummy();

    var decls = try arena.allocator().alloc(node_mod.VariableStmt.VariableDeclarator, 1);
    decls[0] = .{
        .name = name,
        .type = null,
        .init = try arena.createNode(.number_literal, loc, .{ .number_literal = init_val }),
    };

    return arena.createNode(.variable_stmt, loc, .{
        .variable_stmt = .{
            .kind = .let,
            .declarations = decls,
        },
    });
}

fn createBlockStmt(arena: *ast_arena, stmts: []*node_mod.Node) !*node_mod.Node {
    return arena.createNode(.block_stmt, location.SourceLocation.dummy(), .{
        .block_stmt = .{ .statements = stmts },
    });
}

fn createFunctionDecl(arena: *ast_arena, name: []const u8, body: *node_mod.Node) !*node_mod.Node {
    return arena.createNode(.function_decl, location.SourceLocation.dummy(), .{
        .function_decl = .{
            .name = name,
            .params = &[_]node_mod.FunctionDecl.Parameter{},
            .return_type = null,
            .body = body,
            .is_async = false,
            .is_generator = false,
        },
    });
}

test "var_hoist: no vars unchanged" {
    var t = try createTestContext();
    defer t.arena.deinit();

    // Create: function foo() { let x = 1; }
    const let_decl = try createLetDecl(&t.arena, "x", 1.0);
    var stmts = [_]*node_mod.Node{let_decl};
    const body = try createBlockStmt(&t.arena, &stmts);
    const func = try createFunctionDecl(&t.arena, "foo", body);

    // Transform
    const result = try hoistVars(&t.ctx, func);

    // Should not change (no var declarations)
    try testing.expect(!result.changed);
    try testing.expectEqual(@as(usize, 0), result.transform_count);
}

test "var_hoist: simple var hoisting" {
    var t = try createTestContext();
    defer t.arena.deinit();

    // Create: function foo() { var x = 5; }
    const var_decl = try createVarDecl(&t.arena, "x", 5.0);
    var stmts = [_]*node_mod.Node{var_decl};
    const body = try createBlockStmt(&t.arena, &stmts);
    const func = try createFunctionDecl(&t.arena, "foo", body);

    // Transform
    const result = try hoistVars(&t.ctx, func);

    // Should have changed
    try testing.expect(result.changed);
    try testing.expectEqual(@as(usize, 1), result.transform_count);

    // Verify: body should now have 2 statements (hoisted var + assignment)
    const new_body = result.node.data.function_decl.body.?;
    try testing.expectEqual(@as(usize, 2), new_body.data.block_stmt.statements.len);

    // First statement should be: var x; (no init)
    const hoisted = new_body.data.block_stmt.statements[0];
    try testing.expectEqual(node_mod.NodeKind.variable_stmt, hoisted.kind);
    try testing.expectEqual(node_mod.VariableStmt.Kind.@"var", hoisted.data.variable_stmt.kind);
    try testing.expect(hoisted.data.variable_stmt.declarations[0].init == null);

    // Second statement should be: x = 5; (assignment)
    const assign_stmt = new_body.data.block_stmt.statements[1];
    try testing.expectEqual(node_mod.NodeKind.expression_stmt, assign_stmt.kind);
    const assign = assign_stmt.data.expression_stmt;
    try testing.expectEqual(node_mod.BinaryOp.assign, assign.data.binary_expr.op);
}

test "var_hoist: duplicate vars deduplicated" {
    var t = try createTestContext();
    defer t.arena.deinit();

    // Create: function foo() { var x = 1; var x = 2; }
    const var1 = try createVarDecl(&t.arena, "x", 1.0);
    const var2 = try createVarDecl(&t.arena, "x", 2.0);
    var stmts = [_]*node_mod.Node{ var1, var2 };
    const body = try createBlockStmt(&t.arena, &stmts);
    const func = try createFunctionDecl(&t.arena, "foo", body);

    // Transform
    const result = try hoistVars(&t.ctx, func);

    try testing.expect(result.changed);

    // Should have 3 statements: 1 hoisted var (deduplicated) + 2 assignments
    const new_body = result.node.data.function_decl.body.?;
    try testing.expectEqual(@as(usize, 3), new_body.data.block_stmt.statements.len);

    // First statement should be single var x;
    const hoisted = new_body.data.block_stmt.statements[0];
    try testing.expectEqual(node_mod.NodeKind.variable_stmt, hoisted.kind);
    try testing.expectEqualStrings("x", hoisted.data.variable_stmt.declarations[0].name);
}

test "var_hoist: var without initializer removed" {
    var t = try createTestContext();
    defer t.arena.deinit();

    // Create: function foo() { var x; }
    const var_decl = try createVarDecl(&t.arena, "x", null);
    var stmts = [_]*node_mod.Node{var_decl};
    const body = try createBlockStmt(&t.arena, &stmts);
    const func = try createFunctionDecl(&t.arena, "foo", body);

    // Transform
    const result = try hoistVars(&t.ctx, func);

    try testing.expect(result.changed);

    // Should have 1 statement: just the hoisted var (no assignment needed)
    const new_body = result.node.data.function_decl.body.?;
    try testing.expectEqual(@as(usize, 1), new_body.data.block_stmt.statements.len);

    // The statement should be: var x;
    const hoisted = new_body.data.block_stmt.statements[0];
    try testing.expectEqual(node_mod.NodeKind.variable_stmt, hoisted.kind);
}

test "var_hoist: collectVarDecls finds vars in nested blocks" {
    var t = try createTestContext();
    defer t.arena.deinit();

    // Create: { var x = 1; { var y = 2; } }
    const var_x = try createVarDecl(&t.arena, "x", 1.0);
    const var_y = try createVarDecl(&t.arena, "y", 2.0);

    var inner_stmts = [_]*node_mod.Node{var_y};
    const inner_block = try createBlockStmt(&t.arena, &inner_stmts);

    var outer_stmts = [_]*node_mod.Node{ var_x, inner_block };
    const outer_block = try createBlockStmt(&t.arena, &outer_stmts);

    // Collect vars
    var seen = std.StringHashMap(void).init(testing.allocator);
    defer seen.deinit();

    var vars = std.ArrayList(VarDecl).init(testing.allocator);
    defer vars.deinit();

    collectVarDecls(outer_block, &vars, &seen);

    // Should find both x and y
    try testing.expectEqual(@as(usize, 2), vars.items.len);
}

test "var_hoist: collectVarDecls skips let/const" {
    var t = try createTestContext();
    defer t.arena.deinit();

    // Create: { let x = 1; const y = 2; var z = 3; }
    const let_x = try createLetDecl(&t.arena, "x", 1.0);

    const loc = location.SourceLocation.dummy();
    var const_decls = try t.arena.allocator().alloc(node_mod.VariableStmt.VariableDeclarator, 1);
    const_decls[0] = .{
        .name = "y",
        .type = null,
        .init = try t.arena.createNode(.number_literal, loc, .{ .number_literal = 2.0 }),
    };
    const const_y = try t.arena.createNode(.variable_stmt, loc, .{
        .variable_stmt = .{
            .kind = .@"const",
            .declarations = const_decls,
        },
    });

    const var_z = try createVarDecl(&t.arena, "z", 3.0);

    var stmts = [_]*node_mod.Node{ let_x, const_y, var_z };
    const block = try createBlockStmt(&t.arena, &stmts);

    // Collect vars
    var seen = std.StringHashMap(void).init(testing.allocator);
    defer seen.deinit();

    var vars = std.ArrayList(VarDecl).init(testing.allocator);
    defer vars.deinit();

    collectVarDecls(block, &vars, &seen);

    // Should only find z
    try testing.expectEqual(@as(usize, 1), vars.items.len);
    try testing.expectEqualStrings("z", vars.items[0].name);
}

test "var_hoist: collectVarDecls stops at function boundaries" {
    var t = try createTestContext();
    defer t.arena.deinit();

    // Create: { var x = 1; function inner() { var y = 2; } }
    const var_x = try createVarDecl(&t.arena, "x", 1.0);
    const var_y = try createVarDecl(&t.arena, "y", 2.0);

    var inner_stmts = [_]*node_mod.Node{var_y};
    const inner_body = try createBlockStmt(&t.arena, &inner_stmts);
    const inner_func = try createFunctionDecl(&t.arena, "inner", inner_body);

    var outer_stmts = [_]*node_mod.Node{ var_x, inner_func };
    const outer_block = try createBlockStmt(&t.arena, &outer_stmts);

    // Collect vars from outer block only
    var seen = std.StringHashMap(void).init(testing.allocator);
    defer seen.deinit();

    var vars = std.ArrayList(VarDecl).init(testing.allocator);
    defer vars.deinit();

    collectVarDecls(outer_block, &vars, &seen);

    // Should only find x (y is in nested function scope)
    try testing.expectEqual(@as(usize, 1), vars.items.len);
    try testing.expectEqualStrings("x", vars.items[0].name);
}

test "var_hoist: transform preserves let/const" {
    var t = try createTestContext();
    defer t.arena.deinit();

    // Create: function foo() { let a = 1; var x = 2; const b = 3; }
    const let_a = try createLetDecl(&t.arena, "a", 1.0);
    const var_x = try createVarDecl(&t.arena, "x", 2.0);

    const loc = location.SourceLocation.dummy();
    var const_decls = try t.arena.allocator().alloc(node_mod.VariableStmt.VariableDeclarator, 1);
    const_decls[0] = .{
        .name = "b",
        .type = null,
        .init = try t.arena.createNode(.number_literal, loc, .{ .number_literal = 3.0 }),
    };
    const const_b = try t.arena.createNode(.variable_stmt, loc, .{
        .variable_stmt = .{
            .kind = .@"const",
            .declarations = const_decls,
        },
    });

    var stmts = [_]*node_mod.Node{ let_a, var_x, const_b };
    const body = try createBlockStmt(&t.arena, &stmts);
    const func = try createFunctionDecl(&t.arena, "foo", body);

    // Transform
    const result = try hoistVars(&t.ctx, func);

    try testing.expect(result.changed);

    // Should have 4 statements: hoisted var x, let a, x = 2, const b
    const new_body = result.node.data.function_decl.body.?;
    try testing.expectEqual(@as(usize, 4), new_body.data.block_stmt.statements.len);

    // First should be hoisted var x
    const hoisted = new_body.data.block_stmt.statements[0];
    try testing.expectEqual(node_mod.VariableStmt.Kind.@"var", hoisted.data.variable_stmt.kind);

    // Second should be let a
    const let_stmt = new_body.data.block_stmt.statements[1];
    try testing.expectEqual(node_mod.VariableStmt.Kind.let, let_stmt.data.variable_stmt.kind);
}
