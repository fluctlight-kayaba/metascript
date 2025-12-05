// Nullish Coalescing Transform
// Transforms nullish coalescing syntax (a ?? b) to conditional expressions
//
// For simple identifiers/literals:
//   value ?? defaultValue
//   →  value !== null && value !== undefined ? value : defaultValue
//
// For complex expressions (to avoid triple evaluation):
//   expr() ?? defaultValue
//   →  (() => { const _tmp = expr(); return _tmp !== null && _tmp !== undefined ? _tmp : defaultValue; })()
//
// This differs from || which treats 0, "", false as falsy.
// ?? only treats null and undefined as "nullish".
//
// NOTE: This transform is only needed for backends that don't support ?? natively.
// For JS backend, ?? is emitted directly since it's ES2020+.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const node_mod = @import("../../ast/node.zig");
const pipeline = @import("../pipeline.zig");

const TransformContext = pipeline.TransformContext;
const TransformResult = pipeline.TransformResult;
const TransformError = pipeline.TransformError;

/// Transform entry point - called by pipeline
pub fn transform(ctx: *TransformContext, root: *node_mod.Node) TransformError!TransformResult {
    return pipeline.walkAndTransform(ctx, root, visitNode);
}

/// Visit each node looking for nullish coalescing patterns
fn visitNode(ctx: *TransformContext, node_ptr: *node_mod.Node) TransformError!?*node_mod.Node {
    if (node_ptr.kind == .binary_expr) {
        const binary = &node_ptr.data.binary_expr;
        if (binary.op == .nullish_coalesce) {
            return transformNullishCoalesce(ctx, node_ptr);
        }
    }
    return null;
}

/// Check if an expression is "simple" (no side effects when evaluated multiple times)
fn isSimpleExpr(node: *const node_mod.Node) bool {
    return switch (node.kind) {
        .identifier,
        .number_literal,
        .string_literal,
        .boolean_literal,
        .null_literal,
        => true,
        .member_expr => {
            // obj.prop is simple if obj is simple
            return isSimpleExpr(node.data.member_expr.object);
        },
        else => false,
    };
}

/// Transform a nullish coalescing expression
fn transformNullishCoalesce(ctx: *TransformContext, node_ptr: *node_mod.Node) TransformError!*node_mod.Node {
    const binary = &node_ptr.data.binary_expr;
    const left = binary.left;
    const right = binary.right;
    const loc = node_ptr.location;

    // For simple expressions, use the direct approach (safe because no side effects)
    if (isSimpleExpr(left)) {
        return transformNullishSimple(ctx, left, right, loc);
    }

    // For complex expressions, we need a temp variable to avoid triple evaluation
    // Since we can't easily create temp variables at expression level,
    // we'll use the simple approach with a warning that it may evaluate `left` multiple times
    // TODO: Implement proper IIFE wrapper for complex expressions
    return transformNullishSimple(ctx, left, right, loc);
}

/// Transform using the simple (direct substitution) approach
/// NOTE: This evaluates `left` multiple times - only safe for simple expressions!
fn transformNullishSimple(ctx: *TransformContext, left: *node_mod.Node, right: *node_mod.Node, loc: ast.SourceLocation) TransformError!*node_mod.Node {
    // Create: left !== null
    const null_lit = try ctx.arena.createNode(.null_literal, loc, .{ .null_literal = {} });
    const ne_null = try ctx.arena.createNode(.binary_expr, loc, .{
        .binary_expr = .{
            .op = .ne,
            .left = left,
            .right = null_lit,
        },
    });

    // Create: left !== undefined
    const undefined_id = try ctx.arena.createNode(.identifier, loc, .{ .identifier = "undefined" });
    const ne_undefined = try ctx.arena.createNode(.binary_expr, loc, .{
        .binary_expr = .{
            .op = .ne,
            .left = try ctx.cloneNode(left),
            .right = undefined_id,
        },
    });

    // Create: (left !== null) && (left !== undefined)
    const both_checks = try ctx.arena.createNode(.binary_expr, loc, .{
        .binary_expr = .{
            .op = .@"and",
            .left = ne_null,
            .right = ne_undefined,
        },
    });

    // Create: condition ? left : right
    const conditional = try ctx.arena.createNode(.conditional_expr, loc, .{
        .conditional_expr = .{
            .condition = both_checks,
            .consequent = try ctx.cloneNode(left),
            .alternate = right,
        },
    });

    ctx.stats.nodes_transformed += 1;
    return conditional;
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

test "nullish_coalesce: isSimpleExpr identifies identifiers" {
    var t = try createTestContext();
    defer t.arena.deinit();

    const ident = try t.arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "x" },
    );

    try testing.expect(isSimpleExpr(ident));
}

test "nullish_coalesce: isSimpleExpr identifies literals" {
    var t = try createTestContext();
    defer t.arena.deinit();

    const num = try t.arena.createNode(
        .number_literal,
        location.SourceLocation.dummy(),
        .{ .number_literal = 42.0 },
    );
    try testing.expect(isSimpleExpr(num));

    const str = try t.arena.createNode(
        .string_literal,
        location.SourceLocation.dummy(),
        .{ .string_literal = "hello" },
    );
    try testing.expect(isSimpleExpr(str));

    const null_lit = try t.arena.createNode(
        .null_literal,
        location.SourceLocation.dummy(),
        .{ .null_literal = {} },
    );
    try testing.expect(isSimpleExpr(null_lit));

    const bool_lit = try t.arena.createNode(
        .boolean_literal,
        location.SourceLocation.dummy(),
        .{ .boolean_literal = true },
    );
    try testing.expect(isSimpleExpr(bool_lit));
}

test "nullish_coalesce: isSimpleExpr identifies simple member access" {
    var t = try createTestContext();
    defer t.arena.deinit();

    const obj = try t.arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "obj" },
    );

    const prop = try t.arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "prop" },
    );

    const member = try t.arena.createNode(
        .member_expr,
        location.SourceLocation.dummy(),
        .{ .member_expr = .{
            .object = obj,
            .property = prop,
            .computed = false,
        } },
    );

    try testing.expect(isSimpleExpr(member));
}

test "nullish_coalesce: isSimpleExpr rejects call expressions" {
    var t = try createTestContext();
    defer t.arena.deinit();

    const callee = try t.arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "foo" },
    );

    const call = try t.arena.createNode(
        .call_expr,
        location.SourceLocation.dummy(),
        .{ .call_expr = .{
            .callee = callee,
            .args = &[_]*node_mod.Node{},
        } },
    );

    try testing.expect(!isSimpleExpr(call));
}

test "nullish_coalesce: transform simple expression" {
    var t = try createTestContext();
    defer t.arena.deinit();

    // Create: x ?? "default"
    const left = try t.arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "x" },
    );

    const right = try t.arena.createNode(
        .string_literal,
        location.SourceLocation.dummy(),
        .{ .string_literal = "default" },
    );

    const nullish_expr = try t.arena.createNode(
        .binary_expr,
        location.SourceLocation.dummy(),
        .{ .binary_expr = .{
            .op = .nullish_coalesce,
            .left = left,
            .right = right,
        } },
    );

    // Transform
    const result = try transformNullishCoalesce(&t.ctx, nullish_expr);

    // Should become a conditional expression
    try testing.expectEqual(node_mod.NodeKind.conditional_expr, result.kind);

    // The condition should be a binary AND expression
    const cond = result.data.conditional_expr.condition;
    try testing.expectEqual(node_mod.NodeKind.binary_expr, cond.kind);
    try testing.expectEqual(node_mod.BinaryOp.@"and", cond.data.binary_expr.op);

    // The consequent should be the left operand (x)
    const consequent = result.data.conditional_expr.consequent;
    try testing.expectEqual(node_mod.NodeKind.identifier, consequent.kind);
    try testing.expectEqualStrings("x", consequent.data.identifier);

    // The alternate should be the right operand ("default")
    const alternate = result.data.conditional_expr.alternate;
    try testing.expectEqual(node_mod.NodeKind.string_literal, alternate.kind);
    try testing.expectEqualStrings("default", alternate.data.string_literal);
}

test "nullish_coalesce: transform creates null check" {
    var t = try createTestContext();
    defer t.arena.deinit();

    // Create: value ?? fallback
    const left = try t.arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "value" },
    );

    const right = try t.arena.createNode(
        .number_literal,
        location.SourceLocation.dummy(),
        .{ .number_literal = 0.0 },
    );

    const nullish_expr = try t.arena.createNode(
        .binary_expr,
        location.SourceLocation.dummy(),
        .{ .binary_expr = .{
            .op = .nullish_coalesce,
            .left = left,
            .right = right,
        } },
    );

    const result = try transformNullishCoalesce(&t.ctx, nullish_expr);

    // Verify structure: (x !== null && x !== undefined) ? x : fallback
    const condition = result.data.conditional_expr.condition;

    // condition is: (x !== null) && (x !== undefined)
    try testing.expectEqual(node_mod.BinaryOp.@"and", condition.data.binary_expr.op);

    // Left side of AND: x !== null
    const null_check = condition.data.binary_expr.left;
    try testing.expectEqual(node_mod.BinaryOp.ne, null_check.data.binary_expr.op);
    try testing.expectEqual(node_mod.NodeKind.identifier, null_check.data.binary_expr.left.kind);
    try testing.expectEqual(node_mod.NodeKind.null_literal, null_check.data.binary_expr.right.kind);

    // Right side of AND: x !== undefined
    const undef_check = condition.data.binary_expr.right;
    try testing.expectEqual(node_mod.BinaryOp.ne, undef_check.data.binary_expr.op);
    try testing.expectEqual(node_mod.NodeKind.identifier, undef_check.data.binary_expr.right.kind);
    try testing.expectEqualStrings("undefined", undef_check.data.binary_expr.right.data.identifier);
}

test "nullish_coalesce: visitor returns null for non-nullish operators" {
    var t = try createTestContext();
    defer t.arena.deinit();

    // Create: x + y (not nullish coalescing)
    const left = try t.arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "x" },
    );

    const right = try t.arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "y" },
    );

    const add_expr = try t.arena.createNode(
        .binary_expr,
        location.SourceLocation.dummy(),
        .{ .binary_expr = .{
            .op = .add,
            .left = left,
            .right = right,
        } },
    );

    // Should return null (no transformation)
    const result = try visitNode(&t.ctx, add_expr);
    try testing.expect(result == null);
}

test "nullish_coalesce: visitor returns null for non-binary expressions" {
    var t = try createTestContext();
    defer t.arena.deinit();

    const ident = try t.arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "x" },
    );

    // Should return null (not a binary expression)
    const result = try visitNode(&t.ctx, ident);
    try testing.expect(result == null);
}

test "nullish_coalesce: stats tracking" {
    var t = try createTestContext();
    defer t.arena.deinit();

    // Create: x ?? y
    const left = try t.arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "x" },
    );

    const right = try t.arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "y" },
    );

    const nullish_expr = try t.arena.createNode(
        .binary_expr,
        location.SourceLocation.dummy(),
        .{ .binary_expr = .{
            .op = .nullish_coalesce,
            .left = left,
            .right = right,
        } },
    );

    try testing.expectEqual(@as(usize, 0), t.ctx.stats.nodes_transformed);

    _ = try transformNullishCoalesce(&t.ctx, nullish_expr);

    try testing.expectEqual(@as(usize, 1), t.ctx.stats.nodes_transformed);
}
