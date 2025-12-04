const std = @import("std");
const testing = std.testing;

// Import individual components to trace through manually
const lexer_mod = @import("src").lexer_mod;
const parser_mod = @import("src").parser;
const ast_mod = @import("src").ast;

test "trace: manually follow the entire compilation flow" {
    const allocator = testing.allocator;

    // Start with intentionally broken code
    const broken_source = "function test( { return 1; }";  // Missing parameter

    std.debug.print("\n=== MANUAL TRACE START ===\n", .{});
    std.debug.print("Source: {s}\n\n", .{broken_source});

    // Step 1: Create AST arena
    var arena = ast_mod.ASTArena.init(allocator);
    defer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    std.debug.print("Step 1: Created AST arena, file_id = {}\n", .{file_id});

    // Step 2: Lexer
    var lexer = try lexer_mod.Lexer.init(allocator, broken_source, file_id);
    defer lexer.deinit();

    std.debug.print("Step 2: Created lexer\n", .{});
    std.debug.print("  Lexer errors before parsing: {}\n", .{lexer.errors.items.len});

    // Step 3: Parser
    var parser = parser_mod.Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    std.debug.print("Step 3: Created parser\n", .{});
    std.debug.print("  Parser errors before parsing: {}\n", .{parser.errors.items.len});

    // Step 4: Parse (this is where errors should be detected)
    std.debug.print("\nStep 4: Calling parser.parse()...\n", .{});

    const parse_result = parser.parse();

    if (parse_result) |_| {
        std.debug.print("  parse() returned AST (no exception thrown)\n", .{});
    } else |err| {
        std.debug.print("  parse() threw error: {s}\n", .{@errorName(err)});
        return err;
    }

    // Step 5: Check error counts AFTER parsing
    std.debug.print("\nStep 5: Check errors after parsing\n", .{});
    std.debug.print("  Lexer errors: {}\n", .{lexer.errors.items.len});
    std.debug.print("  Parser errors: {}\n", .{parser.errors.items.len});

    // Print actual errors if any
    if (parser.errors.items.len > 0) {
        std.debug.print("\n  Parser error details:\n", .{});
        for (parser.errors.items, 0..) |err, i| {
            std.debug.print("    Error {}: {s}\n", .{i, err.message});
        }
    }

    std.debug.print("\n=== MANUAL TRACE END ===\n", .{});

    // Now compare with our compile() function
    const helpers = @import("backend_test_helpers.zig");
    std.debug.print("\n=== USING compile() HELPER ===\n", .{});
    var result = try helpers.compile(allocator, broken_source, .c);
    defer result.deinit();

    std.debug.print("Result.success: {}\n", .{result.success});
    std.debug.print("Result.error_message: {?s}\n", .{result.error_message});

    std.debug.print("=========================\n\n", .{});
}

test "trace: valid code for comparison" {
    const allocator = testing.allocator;
    const valid_source = "function test() { return 1; }";

    std.debug.print("\n=== VALID CODE TRACE ===\n", .{});
    std.debug.print("Source: {s}\n\n", .{valid_source});

    var arena = ast_mod.ASTArena.init(allocator);
    defer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    var lexer = try lexer_mod.Lexer.init(allocator, valid_source, file_id);
    defer lexer.deinit();

    var parser = parser_mod.Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    _ = try parser.parse();

    std.debug.print("Lexer errors: {}\n", .{lexer.errors.items.len});
    std.debug.print("Parser errors: {}\n", .{parser.errors.items.len});

    const helpers = @import("backend_test_helpers.zig");
    var result = try helpers.compile(allocator, valid_source, .c);
    defer result.deinit();

    std.debug.print("Result.success: {}\n", .{result.success});
    std.debug.print("=========================\n\n", .{});
}
