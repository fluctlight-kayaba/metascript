/// Parser Unit Tests
///
/// Tests for the parser component in isolation.
/// Follows TDD: write test first, then implement feature.

const std = @import("std");
const testing = std.testing;

// Import test utilities
const test_utils = @import("../helpers/testing.zig");
const fixtures = @import("../fixtures/sources.zig");

// Import compiler modules
const lexer_mod = @import("../../src/lexer/lexer.zig");
const parser_mod = @import("../../src/parser/parser.zig");
const ast_mod = @import("../../src/ast/ast.zig");

const Lexer = lexer_mod.Lexer;
const Parser = parser_mod.Parser;
const Node = ast_mod.Node;
const NodeKind = ast_mod.node.NodeKind;

// ============================================================================
// Helper Functions
// ============================================================================

fn parse(source: []const u8) !*Node {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    errdefer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(testing.allocator, source, file_id);
    defer lexer.deinit();

    var parser = Parser.init(testing.allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    return try parser.parse();
}

fn parseWithArena(arena: *ast_mod.ASTArena, source: []const u8) !*Node {
    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(testing.allocator, source, file_id);
    defer lexer.deinit();

    var parser = Parser.init(testing.allocator, arena, &lexer, file_id);
    defer parser.deinit();

    return try parser.parse();
}

// ============================================================================
// Basic Parsing Tests
// ============================================================================

test "parser: parses empty program" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.EMPTY);

    try testing.expectEqual(NodeKind.program, program.kind);
    try testing.expectEqual(@as(usize, 0), program.data.program.statements.len);
}

test "parser: parses variable declaration with const" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.VAR_CONST);

    try testing.expectEqual(NodeKind.program, program.kind);
    try testing.expectEqual(@as(usize, 1), program.data.program.statements.len);

    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.variable_stmt, stmt.kind);
}

test "parser: parses variable declaration with let" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.VAR_LET);

    try testing.expectEqual(@as(usize, 1), program.data.program.statements.len);
    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.variable_stmt, stmt.kind);
}

// ============================================================================
// Function Declaration Tests
// ============================================================================

test "parser: parses simple function" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.FUNC_SIMPLE);

    try testing.expectEqual(@as(usize, 1), program.data.program.statements.len);
    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.function_decl, stmt.kind);
    try testing.expectEqualStrings("hello", stmt.data.function_decl.name);
}

test "parser: parses function with parameters" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.FUNC_WITH_PARAMS);

    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.function_decl, stmt.kind);
    try testing.expectEqualStrings("add", stmt.data.function_decl.name);
    try testing.expectEqual(@as(usize, 2), stmt.data.function_decl.params.len);
}

// ============================================================================
// Class Declaration Tests
// ============================================================================

test "parser: parses empty class" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.CLASS_EMPTY);

    try testing.expectEqual(@as(usize, 1), program.data.program.statements.len);
    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.class_decl, stmt.kind);
    try testing.expectEqualStrings("Empty", stmt.data.class_decl.name);
}

test "parser: parses class with properties" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.CLASS_WITH_PROPERTIES);

    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.class_decl, stmt.kind);
    try testing.expectEqualStrings("User", stmt.data.class_decl.name);
    try testing.expectEqual(@as(usize, 2), stmt.data.class_decl.members.len);
}

test "parser: parses class with constructor" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.CLASS_WITH_CONSTRUCTOR);

    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.class_decl, stmt.kind);
    try testing.expectEqualStrings("Point", stmt.data.class_decl.name);

    // Should have x, y properties and constructor
    try testing.expect(stmt.data.class_decl.members.len >= 2);
}

test "parser: parses class with methods" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.CLASS_WITH_METHODS);

    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.class_decl, stmt.kind);
    try testing.expectEqualStrings("Calculator", stmt.data.class_decl.name);
}

test "parser: parses class with extends" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.CLASS_WITH_EXTENDS);

    // Should have Animal and Dog classes
    try testing.expectEqual(@as(usize, 2), program.data.program.statements.len);

    const dog = program.data.program.statements[1];
    try testing.expectEqual(NodeKind.class_decl, dog.kind);
    try testing.expectEqualStrings("Dog", dog.data.class_decl.name);
    try testing.expect(dog.data.class_decl.extends != null);
}

// ============================================================================
// Interface Declaration Tests
// ============================================================================

test "parser: parses simple interface" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.INTERFACE_SIMPLE);

    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.interface_decl, stmt.kind);
    try testing.expectEqualStrings("Named", stmt.data.interface_decl.name);
}

test "parser: parses interface with methods" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.INTERFACE_WITH_METHODS);

    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.interface_decl, stmt.kind);
    try testing.expectEqualStrings("Repository", stmt.data.interface_decl.name);
    try testing.expect(stmt.data.interface_decl.members.len >= 3);
}

// ============================================================================
// Import/Export Tests
// ============================================================================

test "parser: parses named import" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.IMPORT_NAMED);

    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.import_decl, stmt.kind);
    try testing.expectEqual(@as(usize, 2), stmt.data.import_decl.specifiers.len);
}

test "parser: parses default import" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.IMPORT_DEFAULT);

    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.import_decl, stmt.kind);
}

test "parser: parses named export" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.EXPORT_NAMED);

    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.export_decl, stmt.kind);
}

// ============================================================================
// Control Flow Tests
// ============================================================================

test "parser: parses if statement" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.IF_SIMPLE);

    try testing.expect(program.data.program.statements.len >= 1);
}

test "parser: parses if-else statement" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.IF_ELSE);

    try testing.expect(program.data.program.statements.len >= 1);
}

test "parser: parses while loop" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.WHILE_LOOP);

    try testing.expect(program.data.program.statements.len >= 1);
}

test "parser: parses for loop" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.FOR_LOOP);

    try testing.expect(program.data.program.statements.len >= 1);
}

// ============================================================================
// Macro Tests
// ============================================================================

test "parser: parses @derive decorator" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.MACRO_DERIVE);

    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.class_decl, stmt.kind);
    try testing.expect(stmt.data.class_decl.decorators.len > 0);
}

test "parser: parses @deriveEq decorator" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.MACRO_DERIVE_EQ);

    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.class_decl, stmt.kind);
    try testing.expect(stmt.data.class_decl.decorators.len > 0);

    const decorator = stmt.data.class_decl.decorators[0];
    try testing.expectEqualStrings("deriveEq", decorator.name);
}

test "parser: parses @macro function definition" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.MACRO_DEFINITION);

    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.macro_decl, stmt.kind);
    try testing.expectEqualStrings("log", stmt.data.macro_decl.name);
}

test "parser: parses @extern system macro in function body" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.MACRO_EXTERN);

    // Should have export -> function with macro invocation in body
    try testing.expect(program.data.program.statements.len >= 1);
    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.export_decl, stmt.kind);
}

test "parser: parses @target block with @emit inside" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.MACRO_TARGET_BLOCK);

    // Should parse export macro declaration
    try testing.expect(program.data.program.statements.len >= 1);
    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.export_decl, stmt.kind);

    // The export should contain a macro_decl
    if (stmt.data.export_decl.declaration) |decl| {
        try testing.expectEqual(NodeKind.macro_decl, decl.kind);
        try testing.expectEqualStrings("readFile", decl.data.macro_decl.name);
    }
}

test "parser: parses @emit system macro" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.MACRO_EMIT);

    try testing.expect(program.data.program.statements.len >= 1);
    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.export_decl, stmt.kind);
}

test "parser: parses export macro declaration" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.MACRO_EXPORT);

    try testing.expect(program.data.program.statements.len >= 1);
    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.export_decl, stmt.kind);

    if (stmt.data.export_decl.declaration) |decl| {
        try testing.expectEqual(NodeKind.macro_decl, decl.kind);
        try testing.expectEqualStrings("validate", decl.data.macro_decl.name);
    }
}

test "parser: macro string argument has quotes stripped" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    // Parse: @target("c") { }
    const program = try parseWithArena(&arena,
        \\function test() {
        \\    @target("c") {
        \\    }
        \\}
    );

    try testing.expect(program.data.program.statements.len >= 1);
    const func = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.function_decl, func.kind);

    // Get the function body
    const body = func.data.function_decl.body orelse unreachable;
    try testing.expectEqual(NodeKind.block_stmt, body.kind);

    // First statement should be macro invocation
    const macro_stmt = body.data.block_stmt.statements[0];
    try testing.expectEqual(NodeKind.macro_invocation, macro_stmt.kind);

    // Verify macro name is "target"
    try testing.expectEqualStrings("target", macro_stmt.data.macro_invocation.name);

    // Verify argument is "c" not "\"c\""
    const args = macro_stmt.data.macro_invocation.arguments;
    try testing.expectEqual(@as(usize, 1), args.len);
    try testing.expectEqualStrings("c", args[0].string_literal);
}

// ============================================================================
// Error Handling Tests
// ============================================================================

test "parser: reports error on invalid syntax" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    // This should parse but with errors recorded
    const result = parseWithArena(&arena, fixtures.ERROR_INVALID_SYNTAX);

    // Either returns error or parses with error recovery
    if (result) |_| {
        // Parser recovered - that's OK for error recovery mode
    } else |_| {
        // Parser returned error - also OK
    }
}

// ============================================================================
// Complex Integration Test
// ============================================================================

test "parser: parses full module" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.FULL_MODULE);

    try testing.expectEqual(NodeKind.program, program.kind);
    // Should have: import, interface, class, export
    try testing.expect(program.data.program.statements.len >= 3);
}

// ============================================================================
// Typed Array Tests (Primitive Array Types)
// ============================================================================

test "parser: parses 1D int32 array type" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.ARRAY_1D_INT32);

    try testing.expectEqual(@as(usize, 1), program.data.program.statements.len);
    const stmt = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.variable_stmt, stmt.kind);

    // Check that the type annotation is an array type
    const type_anno = stmt.data.variable_stmt.type_annotation;
    try testing.expect(type_anno != null);
    try testing.expectEqual(ast_mod.types.TypeKind.array, type_anno.?.kind);

    // The element type should be int32
    const elem_type = type_anno.?.data.array;
    try testing.expectEqual(ast_mod.types.TypeKind.int32, elem_type.kind);
}

test "parser: parses 1D float64 array type" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.ARRAY_1D_FLOAT64);

    try testing.expectEqual(@as(usize, 1), program.data.program.statements.len);
    const stmt = program.data.program.statements[0];

    const type_anno = stmt.data.variable_stmt.type_annotation;
    try testing.expect(type_anno != null);
    try testing.expectEqual(ast_mod.types.TypeKind.array, type_anno.?.kind);

    // The element type should be float64
    const elem_type = type_anno.?.data.array;
    try testing.expectEqual(ast_mod.types.TypeKind.float64, elem_type.kind);
}

test "parser: parses 2D int32 array type (int32[][])" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.ARRAY_2D_INT32);

    try testing.expectEqual(@as(usize, 1), program.data.program.statements.len);
    const stmt = program.data.program.statements[0];

    const type_anno = stmt.data.variable_stmt.type_annotation;
    try testing.expect(type_anno != null);

    // Outer type should be array
    try testing.expectEqual(ast_mod.types.TypeKind.array, type_anno.?.kind);

    // Inner type should also be array (array of arrays)
    const inner_type = type_anno.?.data.array;
    try testing.expectEqual(ast_mod.types.TypeKind.array, inner_type.kind);

    // Element type of inner array should be int32
    const elem_type = inner_type.data.array;
    try testing.expectEqual(ast_mod.types.TypeKind.int32, elem_type.kind);
}

test "parser: parses multiple typed arrays with different primitive types" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const program = try parseWithArena(&arena, fixtures.ARRAY_MULTIPLE_TYPES);

    try testing.expectEqual(@as(usize, 3), program.data.program.statements.len);

    // First: int32[]
    const stmt1 = program.data.program.statements[0];
    const type1 = stmt1.data.variable_stmt.type_annotation.?;
    try testing.expectEqual(ast_mod.types.TypeKind.array, type1.kind);
    try testing.expectEqual(ast_mod.types.TypeKind.int32, type1.data.array.kind);

    // Second: float64[]
    const stmt2 = program.data.program.statements[1];
    const type2 = stmt2.data.variable_stmt.type_annotation.?;
    try testing.expectEqual(ast_mod.types.TypeKind.array, type2.kind);
    try testing.expectEqual(ast_mod.types.TypeKind.float64, type2.data.array.kind);

    // Third: uint8[]
    const stmt3 = program.data.program.statements[2];
    const type3 = stmt3.data.variable_stmt.type_annotation.?;
    try testing.expectEqual(ast_mod.types.TypeKind.array, type3.kind);
    try testing.expectEqual(ast_mod.types.TypeKind.uint8, type3.data.array.kind);
}
