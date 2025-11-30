/// CLI: Macro Expansion Test
/// Usage: msc expand-macro <file.ms>
///
/// Parses a Metascript file, expands all macros using the Hermes VM,
/// and prints the resulting AST.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const Lexer = @import("../lexer/lexer.zig").Lexer;
const Parser = @import("../parser/parser.zig").Parser;

// VM-based macro expansion (only available with -Denable-vm=true)
const vm_expander = @import("../macro/vm_expander.zig");

pub fn run(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 1) {
        std.debug.print("Usage: msc expand-macro <file.ms>\n", .{});
        return;
    }

    const file_path = args[0];

    // Read source file
    const source = std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {}\n", .{ file_path, err });
        return;
    };
    defer allocator.free(source);

    std.debug.print("\n=== Source ===\n{s}\n", .{source});

    // Create AST arena
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    // Tokenize
    const file_id: ast.FileId = 1;
    var lexer = try Lexer.init(allocator, source, file_id);
    defer lexer.deinit();

    // Parse
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    const program = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        for (parser.errors.items) |parse_err| {
            std.debug.print("  {s} at line {}\n", .{ parse_err.message, parse_err.loc.line });
        }
        return;
    };

    std.debug.print("\n=== Parsed AST ===\n", .{});
    printAST(program, 0);

    // Expand macros using Hermes VM
    std.debug.print("\n=== Expanding Macros with Hermes VM ===\n", .{});

    const expanded = vm_expander.expandAllMacros(&arena, allocator, program) catch |err| {
        std.debug.print("Macro expansion error: {}\n", .{err});
        return;
    };

    std.debug.print("\n=== Expanded AST ===\n", .{});
    printAST(expanded, 0);

    std.debug.print("\n=== Done ===\n", .{});
}

fn printAST(node: *ast.Node, indent: usize) void {
    const prefix = "  " ** 20;
    const ind = prefix[0 .. indent * 2];

    switch (node.kind) {
        .program => {
            std.debug.print("{s}Program:\n", .{ind});
            for (node.data.program.statements) |stmt| {
                printAST(stmt, indent + 1);
            }
        },
        .class_decl => {
            const class = &node.data.class_decl;
            std.debug.print("{s}Class: {s}\n", .{ ind, class.name });

            if (class.decorators.len > 0) {
                std.debug.print("{s}  Decorators:\n", .{ind});
                for (class.decorators) |dec| {
                    std.debug.print("{s}    @{s}\n", .{ ind, dec.name });
                }
            }

            std.debug.print("{s}  Members ({} total):\n", .{ ind, class.members.len });
            for (class.members) |member| {
                printAST(member, indent + 2);
            }
        },
        .property_decl => {
            const prop = &node.data.property_decl;
            std.debug.print("{s}Property: {s}\n", .{ ind, prop.name });
        },
        .method_decl => {
            const method = &node.data.method_decl;
            std.debug.print("{s}Method: {s}()\n", .{ ind, method.name });
        },
        .function_decl => {
            const func = &node.data.function_decl;
            std.debug.print("{s}Function: {s}\n", .{ ind, func.name });
        },
        .variable_decl => {
            std.debug.print("{s}VariableDecl\n", .{ind});
        },
        .block_stmt => {
            std.debug.print("{s}Block:\n", .{ind});
            for (node.data.block_stmt.statements) |stmt| {
                printAST(stmt, indent + 1);
            }
        },
        .return_stmt => {
            std.debug.print("{s}Return\n", .{ind});
        },
        .binary_expr => {
            std.debug.print("{s}BinaryExpr\n", .{ind});
        },
        .identifier => {
            std.debug.print("{s}Identifier: {s}\n", .{ ind, node.data.identifier });
        },
        else => {
            std.debug.print("{s}{s}\n", .{ ind, @tagName(node.kind) });
        },
    }
}
