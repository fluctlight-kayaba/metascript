/// CLI Command: expand
/// Shows macro expansion output using the Hermes VM
///
/// Usage: msc expand <file.ms>
///
/// This parses a Metascript file, expands all macros (@derive, etc.)
/// using the Hermes VM, and prints the before/after AST.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const Lexer = @import("../lexer/lexer.zig").Lexer;
const Parser = @import("../parser/parser.zig").Parser;
const vm_expander = @import("../macro/vm_expander.zig");
const colors = @import("colors.zig");

pub fn run(allocator: std.mem.Allocator, path: []const u8) !void {
    // Read source file
    const source = std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1024) catch |err| {
        std.debug.print("{s}Error:{s} Failed to read '{s}': {}\n", .{
            colors.error_color.code(), colors.Color.reset.code(), path, err,
        });
        return;
    };
    defer allocator.free(source);

    // Header
    std.debug.print("\n{s}{s}=== MACRO EXPANSION: {s} ==={s}\n\n", .{
        colors.header.code(), colors.Color.bold.code(), path, colors.Color.reset.code(),
    });

    // Show source
    std.debug.print("{s}Source:{s}\n", .{ colors.info.code(), colors.Color.reset.code() });
    std.debug.print("{s}{s}{s}\n\n", .{ colors.dim_text.code(), source, colors.Color.reset.code() });

    // Create AST arena
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    // Tokenize
    const file_id: ast.FileId = 1;
    var lexer = Lexer.init(allocator, source, file_id) catch |err| {
        std.debug.print("{s}Lexer error:{s} {}\n", .{ colors.error_color.code(), colors.Color.reset.code(), err });
        return;
    };
    defer lexer.deinit();

    // Parse
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    const program = parser.parse() catch |err| {
        std.debug.print("{s}Parse error:{s} {}\n", .{ colors.error_color.code(), colors.Color.reset.code(), err });
        for (parser.errors.items) |parse_err| {
            std.debug.print("  Line {}: {s}\n", .{ parse_err.loc.start.line + 1, parse_err.message });
        }
        return;
    };

    std.debug.print("{s}Parsed AST (before expansion):{s}\n", .{
        colors.info.code(), colors.Color.reset.code(),
    });
    printAST(program, 0);

    // Expand macros using Hermes VM
    std.debug.print("\n{s}Expanding macros with Hermes VM...{s}\n\n", .{
        colors.success.code(), colors.Color.reset.code(),
    });

    const expanded = vm_expander.expandAllMacros(&arena, allocator, program) catch |err| {
        std.debug.print("{s}Macro expansion error:{s} {}\n", .{
            colors.error_color.code(), colors.Color.reset.code(), err,
        });
        return;
    };

    std.debug.print("{s}Expanded AST (after expansion):{s}\n", .{
        colors.success.code(), colors.Color.reset.code(),
    });
    printAST(expanded, 0);

    std.debug.print("\n{s}Done.{s}\n", .{ colors.success.code(), colors.Color.reset.code() });
}

fn printAST(node: *ast.Node, indent: usize) void {
    const prefix = "  " ** 20;
    const ind = prefix[0..@min(indent * 2, 40)];

    switch (node.kind) {
        .program => {
            std.debug.print("{s}Program:\n", .{ind});
            for (node.data.program.statements) |stmt| {
                printAST(stmt, indent + 1);
            }
        },
        .class_decl => {
            const class = &node.data.class_decl;
            std.debug.print("{s}{s}Class:{s} {s}\n", .{
                ind, colors.Color.bright_cyan.code(), colors.Color.reset.code(), class.name,
            });

            if (class.decorators.len > 0) {
                std.debug.print("{s}  Decorators:\n", .{ind});
                for (class.decorators) |dec| {
                    std.debug.print("{s}    {s}@{s}{s}\n", .{
                        ind, colors.Color.bright_magenta.code(), dec.name, colors.Color.reset.code(),
                    });
                }
            }

            std.debug.print("{s}  Members ({} total):\n", .{ ind, class.members.len });
            for (class.members) |member| {
                printAST(member, indent + 2);
            }
        },
        .property_decl => {
            const prop = &node.data.property_decl;
            std.debug.print("{s}{s}Property:{s} {s}\n", .{
                ind, colors.Color.bright_green.code(), colors.Color.reset.code(), prop.name,
            });
        },
        .method_decl => {
            const method = &node.data.method_decl;
            std.debug.print("{s}{s}Method:{s} {s}()\n", .{
                ind, colors.Color.bright_yellow.code(), colors.Color.reset.code(), method.name,
            });
        },
        .function_decl => {
            const func = &node.data.function_decl;
            std.debug.print("{s}Function: {s}\n", .{ ind, func.name });
        },
        .variable_stmt => {
            std.debug.print("{s}VariableStmt\n", .{ind});
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
