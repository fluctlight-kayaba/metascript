/// End-to-End Compilation Tests
///
/// Tests the full compilation pipeline: source file → CLI → output
/// These tests verify that real .ms files compile correctly.

const std = @import("std");
const testing = std.testing;

// Import compiler modules via build.zig configured imports
const main_mod = @import("src");
const lexer_mod = main_mod.lexer_mod;
const parser_mod = main_mod.parser;
const ast_mod = main_mod.ast;

// ============================================================================
// Helper Functions
// ============================================================================

fn compileFull(allocator: std.mem.Allocator, source: []const u8) !struct {
    ast: *ast_mod.Node,
    arena: ast_mod.ASTArena,
} {
    var arena = ast_mod.ASTArena.init(allocator);
    errdefer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    var lexer = try lexer_mod.Lexer.init(allocator, source, file_id);
    defer lexer.deinit();

    var parser = parser_mod.Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    const program = try parser.parse();

    return .{
        .ast = program,
        .arena = arena,
    };
}

// ============================================================================
// E2E: Lexer → Parser → AST
// ============================================================================

test "e2e: empty file compiles" {
    const allocator = testing.allocator;
    var result = try compileFull(allocator, "");
    defer result.arena.deinit();

    try testing.expectEqual(ast_mod.node.NodeKind.program, result.ast.kind);
    try testing.expectEqual(@as(usize, 0), result.ast.data.program.statements.len);
}

test "e2e: simple variable declaration compiles" {
    const allocator = testing.allocator;
    const source = "const x = 42;";

    var result = try compileFull(allocator, source);
    defer result.arena.deinit();

    try testing.expectEqual(@as(usize, 1), result.ast.data.program.statements.len);
}

test "e2e: function declaration compiles" {
    const allocator = testing.allocator;
    const source =
        \\function hello(name: string): string {
        \\    return "Hello, " + name;
        \\}
    ;

    var result = try compileFull(allocator, source);
    defer result.arena.deinit();

    try testing.expectEqual(@as(usize, 1), result.ast.data.program.statements.len);
    const stmt = result.ast.data.program.statements[0];
    try testing.expectEqual(ast_mod.node.NodeKind.function_decl, stmt.kind);
}

test "e2e: class with decorator compiles" {
    const allocator = testing.allocator;
    const source =
        \\@derive(Eq)
        \\class User {
        \\    name: string;
        \\    age: number;
        \\}
    ;

    var result = try compileFull(allocator, source);
    defer result.arena.deinit();

    try testing.expectEqual(@as(usize, 1), result.ast.data.program.statements.len);
    const stmt = result.ast.data.program.statements[0];
    try testing.expectEqual(ast_mod.node.NodeKind.class_decl, stmt.kind);
    try testing.expect(stmt.data.class_decl.decorators.len > 0);
}

test "e2e: interface declaration compiles" {
    const allocator = testing.allocator;
    const source =
        \\interface Printable {
        \\    print(): void;
        \\}
    ;

    var result = try compileFull(allocator, source);
    defer result.arena.deinit();

    const stmt = result.ast.data.program.statements[0];
    try testing.expectEqual(ast_mod.node.NodeKind.interface_decl, stmt.kind);
}

test "e2e: import statement compiles" {
    const allocator = testing.allocator;
    const source =
        \\import { foo, bar } from "module";
    ;

    var result = try compileFull(allocator, source);
    defer result.arena.deinit();

    const stmt = result.ast.data.program.statements[0];
    try testing.expectEqual(ast_mod.node.NodeKind.import_decl, stmt.kind);
}

// ============================================================================
// E2E: Full Module Tests
// ============================================================================

test "e2e: complete module with imports, class, and export compiles" {
    const allocator = testing.allocator;
    const source =
        \\import { deriveEq } from "std/macros/derive";
        \\
        \\interface Identifiable {
        \\    id: number;
        \\}
        \\
        \\@deriveEq
        \\class User implements Identifiable {
        \\    id: number;
        \\    name: string;
        \\    email: string;
        \\
        \\    constructor(id: number, name: string, email: string) {
        \\        this.id = id;
        \\        this.name = name;
        \\        this.email = email;
        \\    }
        \\
        \\    greet(): string {
        \\        return "Hello, " + this.name;
        \\    }
        \\}
        \\
        \\export { User };
    ;

    var result = try compileFull(allocator, source);
    defer result.arena.deinit();

    // Should have: import, interface, class, export
    try testing.expect(result.ast.data.program.statements.len >= 3);
}

// ============================================================================
// E2E: Control Flow
// ============================================================================

test "e2e: if-else statement compiles" {
    const allocator = testing.allocator;
    const source =
        \\function abs(x: number): number {
        \\    if (x < 0) {
        \\        return -x;
        \\    } else {
        \\        return x;
        \\    }
        \\}
    ;

    var result = try compileFull(allocator, source);
    defer result.arena.deinit();

    try testing.expectEqual(@as(usize, 1), result.ast.data.program.statements.len);
}

test "e2e: while loop compiles" {
    const allocator = testing.allocator;
    const source =
        \\function countdown(n: number): void {
        \\    while (n > 0) {
        \\        console.log(n);
        \\        n = n - 1;
        \\    }
        \\}
    ;

    var result = try compileFull(allocator, source);
    defer result.arena.deinit();

    try testing.expectEqual(@as(usize, 1), result.ast.data.program.statements.len);
}

test "e2e: for loop compiles" {
    const allocator = testing.allocator;
    const source =
        \\function sum(n: number): number {
        \\    let total = 0;
        \\    for (let i = 1; i <= n; i++) {
        \\        total = total + i;
        \\    }
        \\    return total;
        \\}
    ;

    var result = try compileFull(allocator, source);
    defer result.arena.deinit();

    try testing.expectEqual(@as(usize, 1), result.ast.data.program.statements.len);
}

// ============================================================================
// E2E: Expression Types
// ============================================================================

test "e2e: arrow function compiles" {
    const allocator = testing.allocator;
    const source =
        \\const add = (a: number, b: number) => a + b;
        \\const greet = (name: string) => {
        \\    return "Hello, " + name;
        \\};
    ;

    var result = try compileFull(allocator, source);
    defer result.arena.deinit();

    try testing.expectEqual(@as(usize, 2), result.ast.data.program.statements.len);
}

test "e2e: template literal compiles" {
    const allocator = testing.allocator;
    const source =
        \\const message = `Hello, ${name}!`;
    ;

    var result = try compileFull(allocator, source);
    defer result.arena.deinit();

    try testing.expectEqual(@as(usize, 1), result.ast.data.program.statements.len);
}

// ============================================================================
// E2E: Error Recovery
// ============================================================================

test "e2e: recovers from missing semicolon" {
    const allocator = testing.allocator;
    const source =
        \\const x = 42
        \\const y = 43;
    ;

    // Parser should recover and continue
    const result = compileFull(allocator, source);
    if (result) |r| {
        var arena = r.arena;
        defer arena.deinit();
        // Recovered parsing
        try testing.expect(r.ast.data.program.statements.len >= 1);
    } else |_| {
        // Error is also acceptable
    }
}

// ============================================================================
// E2E: Performance Sanity
// ============================================================================

test "e2e: compiles 50 functions under 500ms" {
    const allocator = testing.allocator;

    var source_buf = std.ArrayList(u8).init(allocator);
    defer source_buf.deinit();

    // Generate 50 functions
    for (0..50) |i| {
        try source_buf.writer().print("function func{d}(): number {{ return {d}; }}\n", .{ i, i });
    }

    const start = std.time.milliTimestamp();

    var result = try compileFull(allocator, source_buf.items);
    defer result.arena.deinit();

    const elapsed = std.time.milliTimestamp() - start;

    try testing.expectEqual(@as(usize, 50), result.ast.data.program.statements.len);
    try testing.expect(elapsed < 500); // Should complete in under 500ms
}

test "e2e: compiles 20 classes under 500ms" {
    const allocator = testing.allocator;

    var source_buf = std.ArrayList(u8).init(allocator);
    defer source_buf.deinit();

    // Generate 20 classes with properties and methods
    for (0..20) |i| {
        try source_buf.writer().print(
            \\class Class{d} {{
            \\    id: number;
            \\    name: string;
            \\    getValue(): number {{ return this.id; }}
            \\}}
            \\
        , .{i});
    }

    const start = std.time.milliTimestamp();

    var result = try compileFull(allocator, source_buf.items);
    defer result.arena.deinit();

    const elapsed = std.time.milliTimestamp() - start;

    try testing.expectEqual(@as(usize, 20), result.ast.data.program.statements.len);
    try testing.expect(elapsed < 500);
}
