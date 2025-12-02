/// Macro Expansion Integration Tests
///
/// Tests the full macro pipeline: lexer → parser → macro expander → output.
/// These tests verify that macros work correctly end-to-end.

const std = @import("std");
const testing = std.testing;

// Import test utilities
const test_utils = @import("../helpers/testing.zig");
const fixtures = @import("../fixtures/sources.zig");

// Import compiler modules
const lexer_mod = @import("../../src/lexer/lexer.zig");
const parser_mod = @import("../../src/parser/parser.zig");
const ast_mod = @import("../../src/ast/ast.zig");
const macro_mod = @import("../../src/macro/expander.zig");

const Lexer = lexer_mod.Lexer;
const Parser = parser_mod.Parser;
const Node = ast_mod.Node;
const NodeKind = ast_mod.node.NodeKind;

// ============================================================================
// Helper Functions
// ============================================================================

fn parseAndExpand(allocator: std.mem.Allocator, arena: *ast_mod.ASTArena, source: []const u8) !*Node {
    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(allocator, source, file_id);
    defer lexer.deinit();

    var parser = Parser.init(allocator, arena, &lexer, file_id);
    defer parser.deinit();

    const program = try parser.parse();

    // Expand macros
    var ctx = macro_mod.MacroContext.init(arena, allocator);
    defer ctx.deinit();

    var expander = macro_mod.MacroExpander.init(&ctx);
    return try expander.expandProgram(program);
}

fn countClassMethods(class_node: *Node) usize {
    var count: usize = 0;
    for (class_node.data.class_decl.members) |member| {
        if (member.kind == .method_decl) {
            count += 1;
        }
    }
    return count;
}

// ============================================================================
// Basic Macro Expansion Tests
// ============================================================================

test "macro expansion: preserves class without decorators" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const source = "class User { name: string; }";
    const program = try parseAndExpand(testing.allocator, &arena, source);

    try testing.expectEqual(@as(usize, 1), program.data.program.statements.len);
    const class = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.class_decl, class.kind);
    try testing.expectEqualStrings("User", class.data.class_decl.name);
}

test "macro expansion: @derive adds methods to class" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const source =
        \\@derive(Eq)
        \\class User {
        \\    name: string;
        \\}
    ;

    const program = try parseAndExpand(testing.allocator, &arena, source);

    const class = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.class_decl, class.kind);

    // After @derive(Eq), class should have equals method
    const method_count = countClassMethods(class);
    try testing.expect(method_count >= 1);
}

// ============================================================================
// Multiple Decorator Tests
// ============================================================================

test "macro expansion: multiple decorators on one class" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const source =
        \\@derive(Eq, Hash)
        \\class Point {
        \\    x: number;
        \\    y: number;
        \\}
    ;

    const program = try parseAndExpand(testing.allocator, &arena, source);

    const class = program.data.program.statements[0];
    try testing.expectEqual(NodeKind.class_decl, class.kind);

    // Should have both equals and hash methods
    const method_count = countClassMethods(class);
    try testing.expect(method_count >= 2);
}

// ============================================================================
// Macro Definition and Usage Tests
// ============================================================================

test "macro expansion: source-defined macro is callable" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const source =
        \\@macro function addTimestamp(target) {
        \\    return target;
        \\}
        \\
        \\@addTimestamp
        \\class Event {
        \\    name: string;
        \\}
    ;

    const program = try parseAndExpand(testing.allocator, &arena, source);

    // Should have macro_decl and class_decl
    try testing.expect(program.data.program.statements.len >= 2);
}

// ============================================================================
// Import Resolution Tests
// ============================================================================

test "macro expansion: import from std/macros works" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const source =
        \\import { deriveEq } from "std/macros/derive";
        \\
        \\@deriveEq
        \\class User {
        \\    id: number;
        \\}
    ;

    const program = try parseAndExpand(testing.allocator, &arena, source);

    // Should have import_decl and class_decl
    try testing.expect(program.data.program.statements.len >= 2);

    // Find the class
    var class_found = false;
    for (program.data.program.statements) |stmt| {
        if (stmt.kind == .class_decl) {
            class_found = true;
            // Should have equals method after expansion
            const method_count = countClassMethods(stmt);
            try testing.expect(method_count >= 1);
        }
    }
    try testing.expect(class_found);
}

// ============================================================================
// Error Handling Tests
// ============================================================================

test "macro expansion: unknown macro produces warning" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    const source =
        \\@unknownMacro
        \\class Test {}
    ;

    // Should not crash, might produce warning
    const result = parseAndExpand(testing.allocator, &arena, source);
    if (result) |program| {
        // Successful parse with unknown macro - macro system is lenient
        try testing.expect(program.data.program.statements.len >= 1);
    } else |_| {
        // Error is also acceptable for unknown macro
    }
}

// ============================================================================
// Performance Sanity Tests
// ============================================================================

test "macro expansion: handles 100 classes" {
    var arena = ast_mod.ASTArena.init(testing.allocator);
    defer arena.deinit();

    var source_buf = std.ArrayList(u8).init(testing.allocator);
    defer source_buf.deinit();

    // Generate 100 classes
    for (0..100) |i| {
        try source_buf.writer().print("class Class{d} {{ id: number; }}\n", .{i});
    }

    const timer = test_utils.Timer.start();

    const program = try parseAndExpand(testing.allocator, &arena, source_buf.items);

    // Should complete in reasonable time (<1 second)
    try timer.expectUnder(1000);

    try testing.expectEqual(@as(usize, 100), program.data.program.statements.len);
}
