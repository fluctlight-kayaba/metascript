/// Test for AST normalization passes
///
/// These tests validate that the normalization framework:
/// 1. Runs without crashing
/// 2. Preserves AST structure when nothing needs normalization
/// 3. Tracks statistics correctly
///
/// Future tests will validate actual transformations once:
/// - Spread elements are implemented in parser
/// - Array method chains are parsed
/// - Arrow functions with capture are supported

const std = @import("std");
const testing = std.testing;
const src = @import("src");
const ast = src.ast;
const normalize = src.macro.normalize;
const location = src.ast.location;

test "NormalizeContext: initialization" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const ctx = normalize.NormalizeContext.init(&arena, testing.allocator, null);

    try testing.expectEqual(@as(usize, 0), ctx.stats.object_spreads_normalized);
    try testing.expectEqual(@as(usize, 0), ctx.stats.array_chains_fused);
    try testing.expectEqual(@as(usize, 0), ctx.stats.closures_inlined);
}

test "normalizeAST: empty program unchanged" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, null);

    // Create empty program
    const program = try arena.createNode(
        .program,
        location.SourceLocation.dummy(),
        .{ .program = .{
            .statements = &[_]*ast.Node{},
            .file_id = 0,
        } },
    );

    // Normalize it
    const result = try normalize.normalizeAST(&ctx, program);

    // Should return same node (nothing to normalize)
    try testing.expectEqual(program, result);

    // Stats should be 0 (nothing was normalized)
    try testing.expectEqual(@as(usize, 0), ctx.stats.object_spreads_normalized);
    try testing.expectEqual(@as(usize, 0), ctx.stats.array_chains_fused);
    try testing.expectEqual(@as(usize, 0), ctx.stats.closures_inlined);
}

test "normalizeAST: simple expression statement preserved" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, null);

    // Create: 1 + 2
    const left = try arena.createNode(
        .number_literal,
        location.SourceLocation.dummy(),
        .{ .number_literal = 1.0 },
    );

    const right = try arena.createNode(
        .number_literal,
        location.SourceLocation.dummy(),
        .{ .number_literal = 2.0 },
    );

    const binary = try arena.createNode(
        .binary_expr,
        location.SourceLocation.dummy(),
        .{ .binary_expr = .{
            .op = .add,
            .left = left,
            .right = right,
        } },
    );

    const expr_stmt = try arena.createNode(
        .expression_stmt,
        location.SourceLocation.dummy(),
        .{ .expression_stmt = binary },
    );

    var stmts = [_]*ast.Node{expr_stmt};
    const program = try arena.createNode(
        .program,
        location.SourceLocation.dummy(),
        .{ .program = .{
            .statements = &stmts,
            .file_id = 0,
        } },
    );

    // Normalize
    const result = try normalize.normalizeAST(&ctx, program);

    // Should preserve structure (no transformations applied)
    try testing.expectEqual(program, result);
    try testing.expectEqual(@as(usize, 1), result.data.program.statements.len);
}

test "normalizeAST: object expression preserved (no spreads)" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, null);

    // Create: const x = { a: 1, b: 2 }
    const key_a = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "a" },
    );

    const value_a = try arena.createNode(
        .number_literal,
        location.SourceLocation.dummy(),
        .{ .number_literal = 1.0 },
    );

    const key_b = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "b" },
    );

    const value_b = try arena.createNode(
        .number_literal,
        location.SourceLocation.dummy(),
        .{ .number_literal = 2.0 },
    );

    var props = [_]ast.node.ObjectExpr.ObjectProperty{
        .{ .property = .{ .key = key_a, .value = value_a, .shorthand = false } },
        .{ .property = .{ .key = key_b, .value = value_b, .shorthand = false } },
    };

    const obj = try arena.createNode(
        .object_expr,
        location.SourceLocation.dummy(),
        .{ .object_expr = .{ .properties = &props } },
    );

    // Normalize just the object expression
    const result = try normalize.normalizeObjectSpreads(&ctx, obj);

    // Should return same node (no spreads to normalize)
    try testing.expectEqual(obj, result);

    // No normalizations performed
    try testing.expectEqual(@as(usize, 0), ctx.stats.object_spreads_normalized);
}

test "normalizeAST: runs all three passes" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, null);

    // Create simple program
    const program = try arena.createNode(
        .program,
        location.SourceLocation.dummy(),
        .{ .program = .{
            .statements = &[_]*ast.Node{},
            .file_id = 0,
        } },
    );

    // Normalize - should run all 3 passes
    const result = try normalize.normalizeAST(&ctx, program);

    // Should complete without errors
    try testing.expectEqual(program, result);

    // All passes ran (even if they did nothing)
    // Stats are 0 because there's nothing to transform yet
    try testing.expectEqual(@as(usize, 0), ctx.stats.object_spreads_normalized);
    try testing.expectEqual(@as(usize, 0), ctx.stats.array_chains_fused);
    try testing.expectEqual(@as(usize, 0), ctx.stats.closures_inlined);
}
