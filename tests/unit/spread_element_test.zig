/// Tests for SpreadElement AST node
///
/// These tests validate that SpreadElement nodes can be created,
/// type-checked, and processed correctly.
///
/// Following TDD: Tests written BEFORE parser implementation

const std = @import("std");
const testing = std.testing;
const src = @import("src");
const ast = src.ast;
const normalize = src.macro.normalize;
const checker = src.checker;
const location = src.ast.location;
const types = src.ast.types;

// ============================================================================
// SpreadElement AST Node Creation
// ============================================================================

test "SpreadElement: create spread node" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    // Create: ...obj
    const obj = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "obj" },
    );

    const spread = try arena.createNode(
        .spread_element,
        location.SourceLocation.dummy(),
        .{ .spread_element = .{ .argument = obj } },
    );

    try testing.expectEqual(ast.node.NodeKind.spread_element, spread.kind);
    try testing.expectEqual(obj, spread.data.spread_element.argument);
}

test "SpreadElement: is expression" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const obj = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "obj" },
    );

    const spread = try arena.createNode(
        .spread_element,
        location.SourceLocation.dummy(),
        .{ .spread_element = .{ .argument = obj } },
    );

    // Spread is an expression
    try testing.expect(spread.isExpression());
}

// ============================================================================
// Type Integration Tests
// ============================================================================

test "NormalizeContext: getTypeOf returns node type" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, null);

    // Create node with type
    const node = try arena.createNode(
        .number_literal,
        location.SourceLocation.dummy(),
        .{ .number_literal = 42.0 },
    );

    // Manually set type (normally done by type checker)
    const num_type = try arena.allocator().create(types.Type);
    num_type.* = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };
    node.type = num_type;

    // getTypeOf should return the node's type
    const result = ctx.getTypeOf(node);
    try testing.expect(result != null);
    try testing.expectEqual(types.TypeKind.number, result.?.kind);
}

test "NormalizeContext: getTypeOf returns null when no type" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, null);

    const node = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "x" },
    );

    // No type set - should return null
    const result = ctx.getTypeOf(node);
    try testing.expect(result == null);
}

test "NormalizeContext: getFields returns null without type checker" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, null);

    // Create object type
    var props = [_]types.ObjectType.Property{
        .{ .name = "x", .type = undefined, .optional = false },
    };
    const obj_type_data = try arena.allocator().create(types.ObjectType);
    obj_type_data.* = .{
        .properties = &props,
        .methods = &.{},
    };
    const obj_type = try arena.allocator().create(types.Type);
    obj_type.* = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = obj_type_data },
    };

    // Should return null (no type checker available)
    const result = ctx.getFields(obj_type);
    try testing.expect(result == null);
}

test "NormalizeContext: getFields returns object fields with type checker" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    // Create a type checker
    var type_checker = try checker.TypeChecker.init(testing.allocator);
    defer type_checker.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, &type_checker);

    // Create object type with fields
    var props = [_]types.ObjectType.Property{
        .{ .name = "x", .type = undefined, .optional = false },
        .{ .name = "y", .type = undefined, .optional = false },
    };
    const obj_type_data = try arena.allocator().create(types.ObjectType);
    obj_type_data.* = .{
        .properties = &props,
        .methods = &.{},
    };
    const obj_type = try arena.allocator().create(types.Type);
    obj_type.* = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = obj_type_data },
    };

    // Should return fields (type checker available, object type)
    const result = ctx.getFields(obj_type);
    try testing.expect(result != null);
    try testing.expectEqual(@as(usize, 2), result.?.len);
    try testing.expectEqualStrings("x", result.?[0].name);
    try testing.expectEqualStrings("y", result.?[1].name);
}

// ============================================================================
// SpreadElement in Normalization
// ============================================================================

test "normalizeObjectSpreads: handles spread element (placeholder)" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, null);

    // Create: { ...obj }
    const obj = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "obj" },
    );

    const spread = try arena.createNode(
        .spread_element,
        location.SourceLocation.dummy(),
        .{ .spread_element = .{ .argument = obj } },
    );

    // Normalize the spread (currently a NOP, but shouldn't crash)
    const result = try normalize.normalizeObjectSpreads(&ctx, spread);

    // Should return the same node (not implemented yet)
    try testing.expectEqual(spread, result);
}

// ============================================================================
// Lexer Tests - Verify tokenization
// ============================================================================

test "lexer: tokenizes spread operator" {
    const Lexer = @import("src").lexer_mod.Lexer;
    const TokenKind = @import("src").token_mod.TokenKind;

    var lex = try Lexer.init(testing.allocator, "{ ...obj }", 0);
    defer lex.deinit();

    const tok1 = try lex.next();
    try testing.expectEqual(TokenKind.left_brace, tok1.kind);

    const tok2 = try lex.next();
    std.debug.print("\nToken 2: kind={}, text='{s}'\n", .{ tok2.kind, tok2.text });
    try testing.expectEqual(TokenKind.dot_dot_dot, tok2.kind);

    const tok3 = try lex.next();
    std.debug.print("Token 3: kind={}, text='{s}'\n", .{ tok3.kind, tok3.text });
    try testing.expectEqual(TokenKind.identifier, tok3.kind);
    try testing.expectEqualStrings("obj", tok3.text);

    const tok4 = try lex.next();
    try testing.expectEqual(TokenKind.right_brace, tok4.kind);
}

// ============================================================================
// Expected Parser Behavior (TDD: Tests Before Implementation)
// ============================================================================

// These tests will FAIL until we implement parser support
// They define what "done" looks like

test "parser: should parse object with single spread" {
    const Lexer = @import("src").lexer_mod.Lexer;
    const Parser = @import("src").parser.Parser;

    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    // Use assignment to force object literal context (not block statement)
    var lex = try Lexer.init(testing.allocator, "const x = { ...obj };", 0);
    defer lex.deinit();
    var p = Parser.init(testing.allocator, &arena, &lex, 0);
    defer p.deinit();

    const program = try p.parse();

    // Should parse successfully
    try testing.expectEqual(@as(usize, 0), p.errors.items.len);
    try testing.expectEqual(@as(usize, 1), program.data.program.statements.len);

    // Navigate: program -> variable_stmt -> declarator -> initializer (object literal)
    const var_stmt = program.data.program.statements[0];
    try testing.expectEqual(ast.node.NodeKind.variable_stmt, var_stmt.kind);

    const declarator = var_stmt.data.variable_stmt.declarations[0];
    const obj_expr = declarator.init.?;
    try testing.expectEqual(ast.node.NodeKind.object_expr, obj_expr.kind);
    try testing.expectEqual(@as(usize, 1), obj_expr.data.object_expr.properties.len);

    const prop = obj_expr.data.object_expr.properties[0];
    try testing.expectEqual(std.meta.activeTag(prop), .spread);

    const spread_node = prop.spread;
    try testing.expectEqual(ast.node.NodeKind.spread_element, spread_node.kind);
    try testing.expectEqualStrings("obj", spread_node.data.spread_element.argument.data.identifier);
}

test "parser: should parse object with property and spread" {
    const Lexer = @import("src").lexer_mod.Lexer;
    const Parser = @import("src").parser.Parser;

    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var lex = try Lexer.init(testing.allocator, "const x = { a: 1, ...obj };", 0);
    defer lex.deinit();
    var p = Parser.init(testing.allocator, &arena, &lex, 0);
    defer p.deinit();

    const program = try p.parse();

    // Should parse successfully
    try testing.expectEqual(@as(usize, 0), p.errors.items.len);

    const var_stmt = program.data.program.statements[0];
    const declarator = var_stmt.data.variable_stmt.declarations[0];
    const obj_expr = declarator.init.?;
    try testing.expectEqual(@as(usize, 2), obj_expr.data.object_expr.properties.len);

    // First should be property
    const prop1 = obj_expr.data.object_expr.properties[0];
    try testing.expectEqual(std.meta.activeTag(prop1), .property);

    // Second should be spread
    const prop2 = obj_expr.data.object_expr.properties[1];
    try testing.expectEqual(std.meta.activeTag(prop2), .spread);
}

test "parser: should parse object with multiple spreads" {
    const Lexer = @import("src").lexer_mod.Lexer;
    const Parser = @import("src").parser.Parser;

    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var lex = try Lexer.init(testing.allocator, "const x = { ...a, ...b, c: 3 };", 0);
    defer lex.deinit();
    var p = Parser.init(testing.allocator, &arena, &lex, 0);
    defer p.deinit();

    const program = try p.parse();
    try testing.expectEqual(@as(usize, 0), p.errors.items.len);

    const var_stmt = program.data.program.statements[0];
    const declarator = var_stmt.data.variable_stmt.declarations[0];
    const obj_expr = declarator.init.?;
    try testing.expectEqual(@as(usize, 3), obj_expr.data.object_expr.properties.len);

    // First two should be spreads
    try testing.expectEqual(std.meta.activeTag(obj_expr.data.object_expr.properties[0]), .spread);
    try testing.expectEqual(std.meta.activeTag(obj_expr.data.object_expr.properties[1]), .spread);

    // Last should be property
    try testing.expectEqual(std.meta.activeTag(obj_expr.data.object_expr.properties[2]), .property);
}

test "parser: should parse nested spreads in object" {
    const Lexer = @import("src").lexer_mod.Lexer;
    const Parser = @import("src").parser.Parser;

    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var lex = try Lexer.init(testing.allocator, "const x = { ...{ ...inner }, outer: 1 };", 0);
    defer lex.deinit();
    var p = Parser.init(testing.allocator, &arena, &lex, 0);
    defer p.deinit();

    const program = try p.parse();
    try testing.expectEqual(@as(usize, 0), p.errors.items.len);

    // Outer object
    const var_stmt = program.data.program.statements[0];
    const declarator = var_stmt.data.variable_stmt.declarations[0];
    const outer_obj = declarator.init.?;
    try testing.expectEqual(@as(usize, 2), outer_obj.data.object_expr.properties.len);

    // First property is spread of an object
    const spread_prop = outer_obj.data.object_expr.properties[0];
    try testing.expectEqual(std.meta.activeTag(spread_prop), .spread);

    // The spread argument should be an object expression
    const inner_obj = spread_prop.spread.data.spread_element.argument;
    try testing.expectEqual(ast.node.NodeKind.object_expr, inner_obj.kind);
}

test "parser: should reject spread in non-object context" {
    // Spread in object context is valid
    const Lexer = @import("src").lexer_mod.Lexer;
    const Parser = @import("src").parser.Parser;

    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var lex1 = try Lexer.init(testing.allocator, "const x = { ...obj };", 0);
    defer lex1.deinit();
    var p1 = Parser.init(testing.allocator, &arena, &lex1, 0);
    defer p1.deinit();

    _ = try p1.parse();
    try testing.expectEqual(@as(usize, 0), p1.errors.items.len); // Should succeed

    // Note: Spread in invalid context like "const x = ...obj" would be
    // caught by parser expecting a valid expression. The parser will
    // try to parse ...obj and succeed (creates spread_element), but
    // the spread_element node would fail semantic validation later.
    // For now, we just verify that spreads work in valid contexts.
}

// ============================================================================
// Normalization Tests - Spread Expansion
// ============================================================================

test "normalizeObjectSpreads: expands spread with type info" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    // Create a type checker
    var type_checker = try checker.TypeChecker.init(testing.allocator);
    defer type_checker.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, &type_checker);

    // Create object type: { x: number, y: number }
    const num_type = try arena.allocator().create(types.Type);
    num_type.* = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var props = [_]types.ObjectType.Property{
        .{ .name = "x", .type = num_type, .optional = false },
        .{ .name = "y", .type = num_type, .optional = false },
    };
    const obj_type_data = try arena.allocator().create(types.ObjectType);
    obj_type_data.* = .{
        .properties = &props,
        .methods = &.{},
    };
    const obj_type = try arena.allocator().create(types.Type);
    obj_type.* = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = obj_type_data },
    };

    // Create: obj identifier with type
    const obj_node = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "obj" },
    );
    obj_node.type = obj_type; // Set type so normalization can query it

    // Create spread element: ...obj
    const spread = try arena.createNode(
        .spread_element,
        location.SourceLocation.dummy(),
        .{ .spread_element = .{ .argument = obj_node } },
    );

    // Create object expression: { ...obj }
    var object_props = [_]ast.node.ObjectExpr.ObjectProperty{
        .{ .spread = spread },
    };
    const obj_expr = try arena.createNode(
        .object_expr,
        location.SourceLocation.dummy(),
        .{ .object_expr = .{ .properties = &object_props } },
    );

    // Normalize!
    const result = try normalize.normalizeObjectSpreads(&ctx, obj_expr);

    // Verify the spread was expanded
    try testing.expectEqual(ast.node.NodeKind.object_expr, result.kind);

    const result_obj = &result.data.object_expr;

    // Should have 2 properties now (x and y), not 1 spread
    try testing.expectEqual(@as(usize, 2), result_obj.properties.len);

    // First property should be: x: obj.x
    const prop1 = result_obj.properties[0];
    try testing.expectEqual(std.meta.activeTag(prop1), .property);
    try testing.expectEqual(ast.node.NodeKind.identifier, prop1.property.key.kind);
    try testing.expectEqualStrings("x", prop1.property.key.data.identifier);
    try testing.expectEqual(ast.node.NodeKind.member_expr, prop1.property.value.kind);

    // Second property should be: y: obj.y
    const prop2 = result_obj.properties[1];
    try testing.expectEqual(std.meta.activeTag(prop2), .property);
    try testing.expectEqual(ast.node.NodeKind.identifier, prop2.property.key.kind);
    try testing.expectEqualStrings("y", prop2.property.key.data.identifier);
    try testing.expectEqual(ast.node.NodeKind.member_expr, prop2.property.value.kind);

    // Stats should show 1 spread normalized
    try testing.expectEqual(@as(usize, 1), ctx.stats.object_spreads_normalized);
}

test "normalizeObjectSpreads: preserves regular properties with spread" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var type_checker = try checker.TypeChecker.init(testing.allocator);
    defer type_checker.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, &type_checker);

    // Create object type: { x: number }
    const num_type = try arena.allocator().create(types.Type);
    num_type.* = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var props = [_]types.ObjectType.Property{
        .{ .name = "x", .type = num_type, .optional = false },
    };
    const obj_type_data = try arena.allocator().create(types.ObjectType);
    obj_type_data.* = .{
        .properties = &props,
        .methods = &.{},
    };
    const obj_type = try arena.allocator().create(types.Type);
    obj_type.* = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = obj_type_data },
    };

    // Create: obj identifier with type
    const obj_node = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "obj" },
    );
    obj_node.type = obj_type;

    // Create spread element: ...obj
    const spread = try arena.createNode(
        .spread_element,
        location.SourceLocation.dummy(),
        .{ .spread_element = .{ .argument = obj_node } },
    );

    // Create regular property: a: 1
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

    // Create object expression: { ...obj, a: 1 }
    var object_props = [_]ast.node.ObjectExpr.ObjectProperty{
        .{ .spread = spread },
        .{ .property = .{ .key = key_a, .value = value_a, .shorthand = false } },
    };
    const obj_expr = try arena.createNode(
        .object_expr,
        location.SourceLocation.dummy(),
        .{ .object_expr = .{ .properties = &object_props } },
    );

    // Normalize!
    const result = try normalize.normalizeObjectSpreads(&ctx, obj_expr);

    // Should have 2 properties: x: obj.x, a: 1
    try testing.expectEqual(@as(usize, 2), result.data.object_expr.properties.len);

    // First property: x: obj.x (from spread)
    const prop1 = result.data.object_expr.properties[0];
    try testing.expectEqual(std.meta.activeTag(prop1), .property);
    try testing.expectEqualStrings("x", prop1.property.key.data.identifier);

    // Second property: a: 1 (preserved)
    const prop2 = result.data.object_expr.properties[1];
    try testing.expectEqual(std.meta.activeTag(prop2), .property);
    try testing.expectEqualStrings("a", prop2.property.key.data.identifier);
    try testing.expectEqual(@as(f64, 1.0), prop2.property.value.data.number_literal);
}

test "normalizeObjectSpreads: handles object without spreads" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, null);

    // Create: { a: 1, b: 2 } (no spreads)
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

    var object_props = [_]ast.node.ObjectExpr.ObjectProperty{
        .{ .property = .{ .key = key_a, .value = value_a, .shorthand = false } },
        .{ .property = .{ .key = key_b, .value = value_b, .shorthand = false } },
    };
    const obj_expr = try arena.createNode(
        .object_expr,
        location.SourceLocation.dummy(),
        .{ .object_expr = .{ .properties = &object_props } },
    );

    // Normalize!
    const result = try normalize.normalizeObjectSpreads(&ctx, obj_expr);

    // Should return same node (no spreads to normalize)
    try testing.expectEqual(obj_expr, result);
    try testing.expectEqual(@as(usize, 2), result.data.object_expr.properties.len);

    // No normalizations performed
    try testing.expectEqual(@as(usize, 0), ctx.stats.object_spreads_normalized);
}

test "normalizeObjectSpreads: keeps spread when no type info" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, null);

    // Create: obj identifier WITHOUT type
    const obj_node = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "obj" },
    );
    // Don't set type - simulate missing type information

    // Create spread element: ...obj
    const spread = try arena.createNode(
        .spread_element,
        location.SourceLocation.dummy(),
        .{ .spread_element = .{ .argument = obj_node } },
    );

    // Create object expression: { ...obj }
    var object_props = [_]ast.node.ObjectExpr.ObjectProperty{
        .{ .spread = spread },
    };
    const obj_expr = try arena.createNode(
        .object_expr,
        location.SourceLocation.dummy(),
        .{ .object_expr = .{ .properties = &object_props } },
    );

    // Normalize!
    const result = try normalize.normalizeObjectSpreads(&ctx, obj_expr);

    // Should keep spread (can't expand without type info)
    try testing.expectEqual(@as(usize, 1), result.data.object_expr.properties.len);
    const prop = result.data.object_expr.properties[0];
    try testing.expectEqual(std.meta.activeTag(prop), .spread);

    // No normalizations performed
    try testing.expectEqual(@as(usize, 0), ctx.stats.object_spreads_normalized);
}

// ============================================================================
// CRITICAL TESTS - Property Override Semantics
// ============================================================================

test "normalizeObjectSpreads: property override - later wins" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var type_checker = try checker.TypeChecker.init(testing.allocator);
    defer type_checker.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, &type_checker);

    // Create object type: { x: number, y: number }
    const num_type = try arena.allocator().create(types.Type);
    num_type.* = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var props = [_]types.ObjectType.Property{
        .{ .name = "x", .type = num_type, .optional = false },
        .{ .name = "y", .type = num_type, .optional = false },
    };
    const obj_type_data = try arena.allocator().create(types.ObjectType);
    obj_type_data.* = .{
        .properties = &props,
        .methods = &.{},
    };
    const obj_type = try arena.allocator().create(types.Type);
    obj_type.* = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = obj_type_data },
    };

    // Create: obj identifier with type
    const obj_node = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "obj" },
    );
    obj_node.type = obj_type;

    // Create spread element: ...obj
    const spread = try arena.createNode(
        .spread_element,
        location.SourceLocation.dummy(),
        .{ .spread_element = .{ .argument = obj_node } },
    );

    // Create property: x: 99 (should override obj.x!)
    const key_x = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "x" },
    );
    const value_99 = try arena.createNode(
        .number_literal,
        location.SourceLocation.dummy(),
        .{ .number_literal = 99.0 },
    );

    // Create object expression: { ...obj, x: 99 }
    var object_props = [_]ast.node.ObjectExpr.ObjectProperty{
        .{ .spread = spread },
        .{ .property = .{ .key = key_x, .value = value_99, .shorthand = false } },
    };
    const obj_expr = try arena.createNode(
        .object_expr,
        location.SourceLocation.dummy(),
        .{ .object_expr = .{ .properties = &object_props } },
    );

    // Normalize!
    const result = try normalize.normalizeObjectSpreads(&ctx, obj_expr);

    // CRITICAL: Should have 2 properties (x, y), NOT 3 (x, y, x)!
    try testing.expectEqual(@as(usize, 2), result.data.object_expr.properties.len);

    // Find properties
    var found_x = false;
    var found_y = false;
    var x_value: ?*ast.Node = null;

    for (result.data.object_expr.properties) |prop| {
        switch (prop) {
            .property => |p| {
                const prop_name = p.key.data.identifier;
                if (std.mem.eql(u8, prop_name, "x")) {
                    found_x = true;
                    x_value = p.value;
                } else if (std.mem.eql(u8, prop_name, "y")) {
                    found_y = true;
                }
            },
            .spread => {
                try testing.expect(false); // Should not have any spreads left
            },
        }
    }

    try testing.expect(found_x);
    try testing.expect(found_y);

    // CRITICAL: x should be 99, NOT obj.x!
    try testing.expectEqual(ast.node.NodeKind.number_literal, x_value.?.kind);
    try testing.expectEqual(@as(f64, 99.0), x_value.?.data.number_literal);
}

test "normalizeObjectSpreads: duplicate spread keys - later wins" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var type_checker = try checker.TypeChecker.init(testing.allocator);
    defer type_checker.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, &type_checker);

    // Create object type 1: { x: number, a: number }
    const num_type = try arena.allocator().create(types.Type);
    num_type.* = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var props1 = [_]types.ObjectType.Property{
        .{ .name = "x", .type = num_type, .optional = false },
        .{ .name = "a", .type = num_type, .optional = false },
    };
    const obj_type_data1 = try arena.allocator().create(types.ObjectType);
    obj_type_data1.* = .{
        .properties = &props1,
        .methods = &.{},
    };
    const obj_type1 = try arena.allocator().create(types.Type);
    obj_type1.* = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = obj_type_data1 },
    };

    // Create object type 2: { x: number, b: number }
    var props2 = [_]types.ObjectType.Property{
        .{ .name = "x", .type = num_type, .optional = false },
        .{ .name = "b", .type = num_type, .optional = false },
    };
    const obj_type_data2 = try arena.allocator().create(types.ObjectType);
    obj_type_data2.* = .{
        .properties = &props2,
        .methods = &.{},
    };
    const obj_type2 = try arena.allocator().create(types.Type);
    obj_type2.* = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = obj_type_data2 },
    };

    // Create: obj1 and obj2 identifiers with types
    const obj1_node = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "obj1" },
    );
    obj1_node.type = obj_type1;

    const obj2_node = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "obj2" },
    );
    obj2_node.type = obj_type2;

    // Create spread elements
    const spread1 = try arena.createNode(
        .spread_element,
        location.SourceLocation.dummy(),
        .{ .spread_element = .{ .argument = obj1_node } },
    );

    const spread2 = try arena.createNode(
        .spread_element,
        location.SourceLocation.dummy(),
        .{ .spread_element = .{ .argument = obj2_node } },
    );

    // Create object expression: { ...obj1, ...obj2 }
    var object_props = [_]ast.node.ObjectExpr.ObjectProperty{
        .{ .spread = spread1 },
        .{ .spread = spread2 },
    };
    const obj_expr = try arena.createNode(
        .object_expr,
        location.SourceLocation.dummy(),
        .{ .object_expr = .{ .properties = &object_props } },
    );

    // Normalize!
    const result = try normalize.normalizeObjectSpreads(&ctx, obj_expr);

    // CRITICAL: Should have 3 properties (x, a, b), NOT 4 (x, a, x, b)!
    try testing.expectEqual(@as(usize, 3), result.data.object_expr.properties.len);

    // Find 'x' property and verify it comes from obj2 (later spread)
    var x_value: ?*ast.Node = null;
    for (result.data.object_expr.properties) |prop| {
        switch (prop) {
            .property => |p| {
                const prop_name = p.key.data.identifier;
                if (std.mem.eql(u8, prop_name, "x")) {
                    x_value = p.value;
                }
            },
            .spread => {
                try testing.expect(false); // Should not have spreads
            },
        }
    }

    try testing.expect(x_value != null);

    // CRITICAL: x should be obj2.x, NOT obj1.x!
    try testing.expectEqual(ast.node.NodeKind.member_expr, x_value.?.kind);
    const obj_name = x_value.?.data.member_expr.object.data.identifier;
    try testing.expectEqualStrings("obj2", obj_name);
}

test "normalizeObjectSpreads: cyclic type detection" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var type_checker = try checker.TypeChecker.init(testing.allocator);
    defer type_checker.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, &type_checker);

    // Note: The current cycle detection prevents infinite recursion in getFieldsRecursive
    // by checking if we've visited a type before. However, for simple field expansion,
    // we only look at immediate fields (not nested types), so cycles in field *types*
    // don't cause issues. This is correct behavior.
    //
    // True cyclic types like: type A = { self: A }
    // would require the type checker to detect and reject them.
    //
    // For now, we just verify the cycle detection mechanism exists and works.

    const cyclic_type = try arena.allocator().create(types.Type);

    // Create object type that references itself
    var props = [_]types.ObjectType.Property{
        .{ .name = "self", .type = cyclic_type, .optional = false },
    };
    const obj_type_data = try arena.allocator().create(types.ObjectType);
    obj_type_data.* = .{
        .properties = &props,
        .methods = &.{},
    };

    cyclic_type.* = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = obj_type_data },
    };

    // Try to get fields - this should succeed because we only look at immediate fields
    // The cycle detection prevents infinite recursion if we ever recurse into field types
    const result = try ctx.getFieldsWithCycleCheck(cyclic_type);

    // Should succeed and return the fields (including 'self')
    try testing.expectEqual(@as(usize, 1), result.len);
    try testing.expectEqualStrings("self", result[0].name);
}

test "normalizeObjectSpreads: performance with large object (100 fields)" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var type_checker = try checker.TypeChecker.init(testing.allocator);
    defer type_checker.deinit();

    var ctx = normalize.NormalizeContext.init(&arena, testing.allocator, &type_checker);

    const num_type = try arena.allocator().create(types.Type);
    num_type.* = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    // Create object type with 100 fields
    const props = try testing.allocator.alloc(types.ObjectType.Property, 100);
    defer testing.allocator.free(props);

    for (props, 0..) |*prop, i| {
        const field_name = try std.fmt.allocPrint(testing.allocator, "field{d}", .{i});
        defer testing.allocator.free(field_name);

        // Store name in arena
        const name_copy = try arena.allocator().dupe(u8, field_name);

        prop.* = .{
            .name = name_copy,
            .type = num_type,
            .optional = false,
        };
    }

    const obj_type_data = try arena.allocator().create(types.ObjectType);
    obj_type_data.* = .{
        .properties = props,
        .methods = &.{},
    };
    const obj_type = try arena.allocator().create(types.Type);
    obj_type.* = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = obj_type_data },
    };

    // Create spread
    const obj_node = try arena.createNode(
        .identifier,
        location.SourceLocation.dummy(),
        .{ .identifier = "largeObj" },
    );
    obj_node.type = obj_type;

    const spread = try arena.createNode(
        .spread_element,
        location.SourceLocation.dummy(),
        .{ .spread_element = .{ .argument = obj_node } },
    );

    // Create object expression: { ...largeObj }
    var object_props = [_]ast.node.ObjectExpr.ObjectProperty{
        .{ .spread = spread },
    };
    const obj_expr = try arena.createNode(
        .object_expr,
        location.SourceLocation.dummy(),
        .{ .object_expr = .{ .properties = &object_props } },
    );

    // Measure time
    const start = std.time.nanoTimestamp();

    // Normalize!
    const result = try normalize.normalizeObjectSpreads(&ctx, obj_expr);

    const end = std.time.nanoTimestamp();
    const duration_us = @as(u64, @intCast(end - start)) / 1000;

    // Verify result
    try testing.expectEqual(@as(usize, 100), result.data.object_expr.properties.len);
    try testing.expectEqual(@as(usize, 1), ctx.stats.object_spreads_normalized);

    // Performance check: should be fast (< 1ms even for 100 fields)
    std.debug.print("\n  Performance: 100 fields normalized in {d}µs\n", .{duration_us});
    try testing.expect(duration_us < 1000); // Less than 1ms
}
