const std = @import("std");
const testing = std.testing;
const CGen = @import("cgen.zig");
const ast = @import("../../ast/ast.zig");
const types = @import("../../ast/types.zig");
const parser = @import("../../parser/parser.zig");
const lexer = @import("../../lexer/lexer.zig");

/// Helper function to compile Metascript source to C code
fn compileToC(source: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    // Create lexer and parser
    var lex = lexer.Lexer.init(allocator, source, "test.ms");
    defer lex.deinit();

    var parse = try parser.Parser.init(allocator, &lex);
    defer parse.deinit();

    // Parse the source
    const program = try parse.parseProgram();

    // Create AST arena
    var ast_arena = ast.ASTArena.init(allocator);
    defer ast_arena.deinit();

    // Create C code generator
    var cgen = try CGen.CGen.init(allocator, program);
    defer cgen.deinit();

    // Generate C code
    const c_code = try cgen.generate();
    return c_code;
}

test "for loop with integer literal uses int type" {
    const source =
        \\for (let i = 0; i < 5; i = i + 1) {
        \\    console.log(i);
        \\}
    ;

    const c_code = try compileToC(source, testing.allocator);
    defer testing.allocator.free(c_code);

    // Should emit "int i = 0" not "double i = 0"
    try testing.expect(std.mem.indexOf(u8, c_code, "int i = 0") != null);
    try testing.expect(std.mem.indexOf(u8, c_code, "double i = 0") == null);
}

test "for loop with float literal uses double type" {
    const source =
        \\for (let i = 0.5; i < 5.0; i = i + 0.1) {
        \\    console.log(i);
        \\}
    ;

    const c_code = try compileToC(source, testing.allocator);
    defer testing.allocator.free(c_code);

    // Should emit "double i = 0.5" for float literals
    try testing.expect(std.mem.indexOf(u8, c_code, "double i = 0.5") != null);
}

test "variable initialized from array indexing uses double type" {
    const source =
        \\const numbers = [10, 20, 30];
        \\const first = numbers[0];
    ;

    const c_code = try compileToC(source, testing.allocator);
    defer testing.allocator.free(c_code);

    // Array element access should infer double type
    try testing.expect(std.mem.indexOf(u8, c_code, "double first") != null);
}

test "array declaration with integer elements" {
    const source =
        \\const numbers = [10, 20, 30, 40, 50];
    ;

    const c_code = try compileToC(source, testing.allocator);
    defer testing.allocator.free(c_code);

    // Should emit proper array declaration with size
    try testing.expect(std.mem.indexOf(u8, c_code, "double numbers[5]") != null);
    try testing.expect(std.mem.indexOf(u8, c_code, "{10, 20, 30, 40, 50}") != null);
}

test "integer loop variable for array iteration" {
    const source =
        \\const numbers = [1, 2, 3];
        \\let sum = 0;
        \\for (let i = 0; i < 3; i = i + 1) {
        \\    sum = sum + numbers[i];
        \\}
    ;

    const c_code = try compileToC(source, testing.allocator);
    defer testing.allocator.free(c_code);

    // Loop variable should be int, not double
    try testing.expect(std.mem.indexOf(u8, c_code, "int i = 0") != null);
    // Array indexing should work
    try testing.expect(std.mem.indexOf(u8, c_code, "numbers[i]") != null);
}

test "break and continue in for loops" {
    const source =
        \\for (let i = 0; i < 10; i = i + 1) {
        \\    if (i > 5) {
        \\        break;
        \\    }
        \\    if (i < 3) {
        \\        continue;
        \\    }
        \\}
    ;

    const c_code = try compileToC(source, testing.allocator);
    defer testing.allocator.free(c_code);

    // Should emit break and continue statements
    try testing.expect(std.mem.indexOf(u8, c_code, "break;") != null);
    try testing.expect(std.mem.indexOf(u8, c_code, "continue;") != null);
}

test "object literal printing with inline field access" {
    const source =
        \\const point = { x: 10, y: 20 };
        \\console.log(point);
    ;

    const c_code = try compileToC(source, testing.allocator);
    defer testing.allocator.free(c_code);

    // Should generate inline field access for object printing
    try testing.expect(std.mem.indexOf(u8, c_code, "point.x") != null);
    try testing.expect(std.mem.indexOf(u8, c_code, "point.y") != null);
    // Should have format string with field names
    try testing.expect(std.mem.indexOf(u8, c_code, "{x:") != null);
    try testing.expect(std.mem.indexOf(u8, c_code, "y:") != null);
}

test "number type with explicit integer initializer uses int" {
    const source =
        \\let count = 0;
        \\count = count + 1;
    ;

    const c_code = try compileToC(source, testing.allocator);
    defer testing.allocator.free(c_code);

    // Even with generic 'number' type, integer literal should emit int
    try testing.expect(std.mem.indexOf(u8, c_code, "int count = 0") != null);
}

test "large integer literal uses double type" {
    const source =
        \\let big = 9999999999;
    ;

    const c_code = try compileToC(source, testing.allocator);
    defer testing.allocator.free(c_code);

    // Integer too large for int32 should use double
    try testing.expect(std.mem.indexOf(u8, c_code, "double big") != null);
}

test "nested for loops with integer counters" {
    const source =
        \\for (let i = 0; i < 3; i = i + 1) {
        \\    for (let j = 0; j < 3; j = j + 1) {
        \\        console.log(i * j);
        \\    }
        \\}
    ;

    const c_code = try compileToC(source, testing.allocator);
    defer testing.allocator.free(c_code);

    // Both loop variables should be int
    try testing.expect(std.mem.indexOf(u8, c_code, "int i = 0") != null);
    try testing.expect(std.mem.indexOf(u8, c_code, "int j = 0") != null);
}
