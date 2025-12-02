// CLI Command: dump-ast
// Shows parsed AST structure

const std = @import("std");
const ast = @import("../ast/ast.zig");
const Lexer = @import("../lexer/lexer.zig").Lexer;
const Parser = @import("../parser/parser.zig").Parser;

pub fn run(allocator: std.mem.Allocator, path: []const u8) !void {
    std.debug.print("=== AST DUMP: {s} ===\n\n", .{path});

    // Read source file
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.debug.print("Error: Could not open file: {}\n", .{err});
        return;
    };
    defer file.close();

    const source = file.readToEndAlloc(allocator, 1024 * 1024) catch |err| {
        std.debug.print("Error: Could not read file: {}\n", .{err});
        return;
    };
    defer allocator.free(source);

    // Create AST arena
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    const file_id = try arena.addFile(path);

    // Create lexer and parser
    var lexer = try Lexer.init(allocator, source, file_id);
    defer lexer.deinit();
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    // Parse
    const program = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return;
    };

    // Show parse errors
    if (parser.errors.items.len > 0) {
        std.debug.print("Parse errors:\n", .{});
        for (parser.errors.items) |parse_err| {
            std.debug.print("  Line {}: {s}\n", .{ parse_err.loc.start.line, parse_err.message });
        }
        std.debug.print("\n", .{});
    }

    // Print AST
    std.debug.print("Program ({} statements):\n", .{program.data.program.statements.len});
    for (program.data.program.statements, 0..) |stmt, i| {
        printNode(stmt, 1, i);
    }

    std.debug.print("\n", .{});
}

fn printNode(node: *const ast.Node, indent: usize, index: usize) void {
    printIndent(indent);
    std.debug.print("[{}] ", .{index});

    switch (node.kind) {
        .program => std.debug.print("Program\n", .{}),

        // Declarations
        .class_decl => {
            const class = &node.data.class_decl;
            std.debug.print("ClassDecl: {s}", .{class.name});
            if (class.decorators.len > 0) {
                std.debug.print(" [", .{});
                for (class.decorators, 0..) |dec, di| {
                    if (di > 0) std.debug.print(", ", .{});
                    std.debug.print("@{s}", .{dec.name});
                    if (dec.arguments.len > 0) {
                        std.debug.print("({} args)", .{dec.arguments.len});
                    }
                }
                std.debug.print("]", .{});
            }
            if (class.extends) |_| std.debug.print(" extends ...", .{});
            if (class.implements.len > 0) std.debug.print(" implements {} types", .{class.implements.len});
            std.debug.print(" ({} members)\n", .{class.members.len});
            for (class.members, 0..) |member, j| {
                printNode(member, indent + 1, j);
            }
        },
        .function_decl => {
            const func = &node.data.function_decl;
            std.debug.print("FunctionDecl: {s}({} params)", .{ func.name, func.params.len });
            if (func.return_type) |_| std.debug.print(" -> ...", .{});
            std.debug.print("\n", .{});
            if (func.body) |body| {
                printNode(body, indent + 1, 0);
            }
        },
        .interface_decl => {
            const iface = &node.data.interface_decl;
            std.debug.print("InterfaceDecl: {s} ({} members)\n", .{ iface.name, iface.members.len });
            for (iface.members, 0..) |member, j| {
                printNode(member, indent + 1, j);
            }
        },
        .type_alias_decl => {
            const alias = &node.data.type_alias_decl;
            std.debug.print("TypeAliasDecl: {s}\n", .{alias.name});
        },
        .import_decl => {
            const imp = &node.data.import_decl;
            std.debug.print("ImportDecl: from {s} ({} specifiers)\n", .{ imp.source, imp.specifiers.len });
        },
        .export_decl => {
            const exp = &node.data.export_decl;
            if (exp.declaration) |decl| {
                std.debug.print("ExportDecl:\n", .{});
                printNode(decl, indent + 1, 0);
            } else {
                std.debug.print("ExportDecl: {} specifiers\n", .{exp.specifiers.len});
            }
        },

        // Class members
        .property_decl => {
            const prop = &node.data.property_decl;
            std.debug.print("PropertyDecl: {s}", .{prop.name});
            if (prop.readonly) std.debug.print(" (readonly)", .{});
            std.debug.print("\n", .{});
        },
        .method_decl => {
            const method = &node.data.method_decl;
            std.debug.print("MethodDecl: {s}({} params)\n", .{ method.name, method.params.len });
            if (method.body) |body| {
                printNode(body, indent + 1, 0);
            }
        },
        .constructor_decl => {
            const ctor = &node.data.constructor_decl;
            std.debug.print("ConstructorDecl: ({} params)\n", .{ctor.params.len});
            printNode(ctor.body, indent + 1, 0);
        },

        // Statements
        .variable_stmt => {
            const var_stmt = &node.data.variable_stmt;
            const kind_str = switch (var_stmt.kind) {
                .@"const" => "const",
                .let => "let",
                .@"var" => "var",
            };
            std.debug.print("VariableStmt: {s} ({} declarations)\n", .{ kind_str, var_stmt.declarations.len });
            for (var_stmt.declarations, 0..) |decl, j| {
                printIndent(indent + 1);
                std.debug.print("[{}] {s}", .{ j, decl.name });
                if (decl.init) |_| std.debug.print(" = ...", .{});
                std.debug.print("\n", .{});
            }
        },
        .block_stmt => {
            const block = &node.data.block_stmt;
            std.debug.print("BlockStmt: ({} statements)\n", .{block.statements.len});
            for (block.statements, 0..) |stmt, j| {
                printNode(stmt, indent + 1, j);
            }
        },
        .if_stmt => {
            std.debug.print("IfStmt:\n", .{});
            printIndent(indent + 1);
            std.debug.print("condition: ", .{});
            printExprBrief(node.data.if_stmt.condition);
            std.debug.print("\n", .{});
        },
        .while_stmt => {
            std.debug.print("WhileStmt:\n", .{});
        },
        .for_stmt => {
            std.debug.print("ForStmt:\n", .{});
        },
        .return_stmt => {
            std.debug.print("ReturnStmt", .{});
            if (node.data.return_stmt.argument) |arg| {
                std.debug.print(": ", .{});
                printExprBrief(arg);
            }
            std.debug.print("\n", .{});
        },
        .break_stmt => std.debug.print("BreakStmt\n", .{}),
        .continue_stmt => std.debug.print("ContinueStmt\n", .{}),
        .expression_stmt => {
            std.debug.print("ExpressionStmt: ", .{});
            printExprBrief(node.data.expression_stmt);
            std.debug.print("\n", .{});
        },

        // Expressions (brief)
        .binary_expr, .unary_expr, .call_expr, .member_expr, .new_expr => {
            printExprBrief(node);
            std.debug.print("\n", .{});
        },

        // Literals
        .number_literal => std.debug.print("NumberLiteral: {d}\n", .{node.data.number_literal}),
        .string_literal => std.debug.print("StringLiteral: \"{s}\"\n", .{node.data.string_literal}),
        .boolean_literal => std.debug.print("BooleanLiteral: {}\n", .{node.data.boolean_literal}),
        .null_literal => std.debug.print("NullLiteral\n", .{}),
        .identifier => std.debug.print("Identifier: {s}\n", .{node.data.identifier}),

        // Macros
        .macro_decl => {
            const macro = &node.data.macro_decl;
            std.debug.print("MacroDecl: @macro function {s}({} params)\n", .{ macro.name, macro.params.len });
            printIndent(indent + 1);
            std.debug.print("body:\n", .{});
            printNode(macro.body, indent + 2, 0);
        },
        .macro_invocation => {
            const macro = &node.data.macro_invocation;
            std.debug.print("MacroInvocation: @{s}({} args)\n", .{ macro.name, macro.arguments.len });
        },
        .comptime_block => {
            std.debug.print("ComptimeBlock:\n", .{});
            printNode(node.data.comptime_block.body, indent + 1, 0);
        },

        else => std.debug.print("{s}\n", .{@tagName(node.kind)}),
    }
}

fn printExprBrief(node: *const ast.Node) void {
    switch (node.kind) {
        .number_literal => std.debug.print("{d}", .{node.data.number_literal}),
        .string_literal => std.debug.print("\"{s}\"", .{node.data.string_literal}),
        .boolean_literal => std.debug.print("{}", .{node.data.boolean_literal}),
        .null_literal => std.debug.print("null", .{}),
        .identifier => std.debug.print("{s}", .{node.data.identifier}),
        .binary_expr => {
            const binary = &node.data.binary_expr;
            printExprBrief(binary.left);
            std.debug.print(" {s} ", .{@tagName(binary.op)});
            printExprBrief(binary.right);
        },
        .unary_expr => {
            const unary = &node.data.unary_expr;
            std.debug.print("{s}", .{@tagName(unary.op)});
            printExprBrief(unary.argument);
        },
        .call_expr => {
            const call = &node.data.call_expr;
            printExprBrief(call.callee);
            std.debug.print("({} args)", .{call.arguments.len});
        },
        .member_expr => {
            const member = &node.data.member_expr;
            printExprBrief(member.object);
            if (member.computed) {
                std.debug.print("[...]", .{});
            } else {
                std.debug.print(".{s}", .{member.property.data.identifier});
            }
        },
        .new_expr => {
            const new_ex = &node.data.new_expr;
            std.debug.print("new ", .{});
            printExprBrief(new_ex.callee);
            std.debug.print("({} args)", .{new_ex.arguments.len});
        },
        .array_expr => {
            std.debug.print("[{} elements]", .{node.data.array_expr.elements.len});
        },
        .object_expr => {
            std.debug.print("{{ {} props }}", .{node.data.object_expr.properties.len});
        },
        .conditional_expr => {
            std.debug.print("... ? ... : ...", .{});
        },
        else => std.debug.print("({s})", .{@tagName(node.kind)}),
    }
}

fn printIndent(n: usize) void {
    for (0..n) |_| {
        std.debug.print("  ", .{});
    }
}
