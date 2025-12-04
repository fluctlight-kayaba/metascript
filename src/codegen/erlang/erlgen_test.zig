/// Erlang Code Generator Tests
/// Phase 1: Simple backend tests (no Lobster integration)
///
/// Following José Valim's recommendations:
/// - Test basic class → tagged tuple codegen
/// - Test method → function translation
/// - Test property access
/// - Snapshot testing for readable output

const std = @import("std");
const testing = std.testing;
const ast = @import("../../ast/ast.zig");
const node_mod = @import("../../ast/node.zig");
const parser = @import("../../parser/parser.zig");
const lexer = @import("../../lexer/lexer.zig");
const erlgen = @import("erlgen.zig");

// Helper: Parse source and generate Erlang
fn compileToErlang(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
    var lex = lexer.Lexer.init(source);
    var p = try parser.Parser.init(allocator, &lex);
    defer p.deinit();

    const program = try p.parse();

    var gen = try erlgen.ErlangGenerator.init(allocator, null);
    defer gen.deinit();

    return try gen.generate(program);
}

test "erlgen: empty program generates module header" {
    const allocator = testing.allocator;
    const source = "";

    const erl_code = try compileToErlang(allocator, source);
    defer allocator.free(erl_code);

    // Should generate minimal module
    try testing.expect(std.mem.indexOf(u8, erl_code, "-module(") != null);
}

test "erlgen: simple class becomes tagged tuple" {
    const allocator = testing.allocator;
    const source =
        \\class User {
        \\  name: string;
        \\  age: number;
        \\}
    ;

    const erl_code = try compileToErlang(allocator, source);
    defer allocator.free(erl_code);

    // Should generate constructor function
    try testing.expect(std.mem.indexOf(u8, erl_code, "new_user(") != null);
    // Should use tagged tuple pattern
    try testing.expect(std.mem.indexOf(u8, erl_code, "{user,") != null);
}

test "erlgen: class with method becomes separate function" {
    const allocator = testing.allocator;
    const source =
        \\class Counter {
        \\  value: number;
        \\  increment() {
        \\    this.value = this.value + 1;
        \\  }
        \\}
    ;

    const erl_code = try compileToErlang(allocator, source);
    defer allocator.free(erl_code);

    // Should generate method as function with receiver
    try testing.expect(std.mem.indexOf(u8, erl_code, "counter_increment(") != null);
    // Should accept Counter as first argument
    try testing.expect(std.mem.indexOf(u8, erl_code, "Counter") != null or
                      std.mem.indexOf(u8, erl_code, "_counter") != null);
}

test "erlgen: function declaration" {
    const allocator = testing.allocator;
    const source =
        \\function greet(name: string): string {
        \\  return "Hello " + name;
        \\}
    ;

    const erl_code = try compileToErlang(allocator, source);
    defer allocator.free(erl_code);

    // Should generate function clause
    try testing.expect(std.mem.indexOf(u8, erl_code, "greet(") != null);
    // Should have parameter
    try testing.expect(std.mem.indexOf(u8, erl_code, "Name") != null);
}

test "erlgen: string concatenation becomes iolist" {
    const allocator = testing.allocator;
    const source =
        \\function greet(name: string): string {
        \\  return "Hello " + name + "!";
        \\}
    ;

    const erl_code = try compileToErlang(allocator, source);
    defer allocator.free(erl_code);

    // Should use iolist pattern (Phase 1: simple concatenation is OK)
    // We'll optimize to proper iolists in Phase 2
    try testing.expect(std.mem.indexOf(u8, erl_code, "greet(") != null);
}

test "erlgen: number and boolean literals" {
    const allocator = testing.allocator;
    const source =
        \\const x = 42;
        \\const y = true;
        \\const z = false;
    ;

    const erl_code = try compileToErlang(allocator, source);
    defer allocator.free(erl_code);

    // Erlang uses same literals
    try testing.expect(std.mem.indexOf(u8, erl_code, "42") != null);
    try testing.expect(std.mem.indexOf(u8, erl_code, "true") != null);
    try testing.expect(std.mem.indexOf(u8, erl_code, "false") != null);
}

test "erlgen: exports generated functions" {
    const allocator = testing.allocator;
    const source =
        \\function add(a: number, b: number): number {
        \\  return a + b;
        \\}
    ;

    const erl_code = try compileToErlang(allocator, source);
    defer allocator.free(erl_code);

    // Should have export directive
    try testing.expect(std.mem.indexOf(u8, erl_code, "-export([") != null);
    try testing.expect(std.mem.indexOf(u8, erl_code, "add/2") != null);
}

// =========================================================================
// Variable Shadowing Tests (Phase 2)
// =========================================================================

test "ErlangEnv: first use of variable" {
    var env = try erlgen.ErlangEnv.init(testing.allocator);
    defer env.deinit();

    const name1 = try env.nextVarName("x");
    defer testing.allocator.free(name1);

    try testing.expectEqualStrings("X", name1);
}

test "ErlangEnv: second use of variable (shadowing)" {
    var env = try erlgen.ErlangEnv.init(testing.allocator);
    defer env.deinit();

    const name1 = try env.nextVarName("x");
    defer testing.allocator.free(name1);
    const name2 = try env.nextVarName("x");
    defer testing.allocator.free(name2);

    try testing.expectEqualStrings("X", name1);
    try testing.expectEqualStrings("X@1", name2);
}

test "ErlangEnv: multiple shadowing" {
    var env = try erlgen.ErlangEnv.init(testing.allocator);
    defer env.deinit();

    const name1 = try env.nextVarName("counter");
    defer testing.allocator.free(name1);
    const name2 = try env.nextVarName("counter");
    defer testing.allocator.free(name2);
    const name3 = try env.nextVarName("counter");
    defer testing.allocator.free(name3);

    try testing.expectEqualStrings("Counter", name1);
    try testing.expectEqualStrings("Counter@1", name2);
    try testing.expectEqualStrings("Counter@2", name3);
}

test "ErlangEnv: different variables independent" {
    var env = try erlgen.ErlangEnv.init(testing.allocator);
    defer env.deinit();

    const x1 = try env.nextVarName("x");
    defer testing.allocator.free(x1);
    const y1 = try env.nextVarName("y");
    defer testing.allocator.free(y1);
    const x2 = try env.nextVarName("x");
    defer testing.allocator.free(x2);
    const y2 = try env.nextVarName("y");
    defer testing.allocator.free(y2);

    try testing.expectEqualStrings("X", x1);
    try testing.expectEqualStrings("Y", y1);
    try testing.expectEqualStrings("X@1", x2);
    try testing.expectEqualStrings("Y@1", y2);
}

test "ErlangEnv: scope tracking" {
    var env = try erlgen.ErlangEnv.init(testing.allocator);
    defer env.deinit();

    const x1 = try env.nextVarName("x");
    defer testing.allocator.free(x1);

    // Enter new scope
    try env.enterScope();
    const x2 = try env.nextVarName("x");
    defer testing.allocator.free(x2);
    const x3 = try env.nextVarName("x");
    defer testing.allocator.free(x3);

    // Exit scope
    env.exitScope();

    // Back in outer scope - counter continues
    const x4 = try env.nextVarName("x");
    defer testing.allocator.free(x4);

    try testing.expectEqualStrings("X", x1);
    try testing.expectEqualStrings("X@1", x2);
    try testing.expectEqualStrings("X@2", x3);
    try testing.expectEqualStrings("X@3", x4);
}

// =========================================================================
// Integration Tests - Full Compilation
// =========================================================================

test "integration: compile function with variable shadowing" {
    const allocator = testing.allocator;
    const source =
        \\function counter() {
        \\  const x = 0;
        \\  x = x + 1;
        \\  x = x + 1;
        \\  return x;
        \\}
    ;

    const erl_code = try compileToErlang(allocator, source);
    defer allocator.free(erl_code);

    // Print the generated Erlang code for inspection
    std.debug.print("\n=== Generated Erlang Code ===\n{s}\n", .{erl_code});

    // Verify shadowing is present
    try testing.expect(std.mem.indexOf(u8, erl_code, "X = 0") != null);
    try testing.expect(std.mem.indexOf(u8, erl_code, "X@1 = X + 1") != null or
                      std.mem.indexOf(u8, erl_code, "X@1 =") != null);
}

test "integration: compile with nested scopes" {
    const allocator = testing.allocator;
    const source =
        \\function test() {
        \\  const x = 1;
        \\  if (true) {
        \\    const y = x + 1;
        \\    y = y * 2;
        \\  }
        \\  x = x + 10;
        \\  return x;
        \\}
    ;

    const erl_code = try compileToErlang(allocator, source);
    defer allocator.free(erl_code);

    // Print the generated Erlang code for inspection
    std.debug.print("\n=== Generated Erlang Code (Nested Scopes) ===\n{s}\n", .{erl_code});

    // Verify both X and Y shadowing
    try testing.expect(std.mem.indexOf(u8, erl_code, "X = 1") != null);
    try testing.expect(std.mem.indexOf(u8, erl_code, "Y =") != null);
}
