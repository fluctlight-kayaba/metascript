/// Statement emission for JavaScript code generation
/// Handles control flow, blocks, and statement nodes.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const emit_mod = @import("emit.zig");
const decl_mod = @import("declarations.zig");

const JSGenerator = @import("jsgen.zig").JSGenerator;

// =============================================================================
// Block Statements
// =============================================================================

pub fn emitBlockStmt(gen: *JSGenerator, node: *ast.Node) !void {
    const block = &node.data.block_stmt;

    try emit_mod.emit(gen, "{\n");
    gen.indent_level += 1;

    for (block.statements) |stmt| {
        try gen.emitNode(stmt);
    }

    gen.indent_level -= 1;
    try emit_mod.emitIndent(gen);
    try emit_mod.emit(gen, "}");
}

pub fn emitExpressionStmt(gen: *JSGenerator, node: *ast.Node) !void {
    // expression_stmt is just a *Node directly
    const expr = node.data.expression_stmt;

    try emit_mod.emitIndent(gen);
    try gen.emitNode(expr);
    try emit_mod.emitSemicolon(gen);
}

// =============================================================================
// Control Flow
// =============================================================================

pub fn emitIfStmt(gen: *JSGenerator, node: *ast.Node) !void {
    const if_stmt = &node.data.if_stmt;

    try emit_mod.emitIndent(gen);
    try emit_mod.emit(gen, "if (");
    try gen.emitNode(if_stmt.condition);
    try emit_mod.emit(gen, ") ");
    try gen.emitNode(if_stmt.consequent);

    if (if_stmt.alternate) |alternate| {
        try emit_mod.emit(gen, " else ");
        try gen.emitNode(alternate);
    }
    try emit_mod.emitNewline(gen);
}

pub fn emitWhileStmt(gen: *JSGenerator, node: *ast.Node) !void {
    const while_stmt = &node.data.while_stmt;

    try emit_mod.emitIndent(gen);
    try emit_mod.emit(gen, "while (");
    try gen.emitNode(while_stmt.condition);
    try emit_mod.emit(gen, ") ");
    try gen.emitNode(while_stmt.body);
    try emit_mod.emitNewline(gen);
}

pub fn emitForStmt(gen: *JSGenerator, node: *ast.Node) !void {
    const for_stmt = &node.data.for_stmt;

    try emit_mod.emitIndent(gen);
    try emit_mod.emit(gen, "for (");

    // Initializer
    if (for_stmt.init) |init| {
        // Variable declaration or expression
        if (init.kind == .variable_stmt) {
            const var_stmt = &init.data.variable_stmt;
            const keyword = switch (var_stmt.kind) {
                .@"const" => "const ",
                .let => "let ",
                .@"var" => "var ",
            };
            try emit_mod.emit(gen, keyword);
            for (var_stmt.declarations, 0..) |decl, i| {
                if (i > 0) try emit_mod.emit(gen, ", ");
                try emit_mod.emit(gen, decl.name);
                if (decl.init) |decl_init| {
                    try emit_mod.emit(gen, " = ");
                    try gen.emitNode(decl_init);
                }
            }
        } else {
            try gen.emitNode(init);
        }
    }
    try emit_mod.emit(gen, "; ");

    // Condition
    if (for_stmt.condition) |condition| {
        try gen.emitNode(condition);
    }
    try emit_mod.emit(gen, "; ");

    // Update
    if (for_stmt.update) |update| {
        try gen.emitNode(update);
    }
    try emit_mod.emit(gen, ") ");

    try gen.emitNode(for_stmt.body);
    try emit_mod.emitNewline(gen);
}

pub fn emitReturnStmt(gen: *JSGenerator, node: *ast.Node) !void {
    const ret = &node.data.return_stmt;

    try emit_mod.emitIndent(gen);
    try emit_mod.emit(gen, "return");

    if (ret.argument) |arg| {
        try emit_mod.emit(gen, " ");
        try gen.emitNode(arg);
    }

    try emit_mod.emitSemicolon(gen);
}

// =============================================================================
// Tests
// =============================================================================

test "statements: emitBlockStmt" {
    // Block statement tests would go here
    // Requires setting up proper AST nodes
    try std.testing.expect(true);
}
