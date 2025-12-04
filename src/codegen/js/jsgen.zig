/// JavaScript Code Generator - Main Module
/// Direct emission from Typed AST to JavaScript (ES2020+)
///
/// Follows the Nim/Haxe pattern: AST IS the IR, no separate lowering step.
/// JavaScript is the easiest backend because it's closest to TypeScript syntax.
///
/// Module Structure:
///   jsgen.zig       - Main generator, orchestration
///   emit.zig        - Low-level output helpers
///   expressions.zig - Expression emission
///   statements.zig  - Statement emission
///   declarations.zig - Class/function/variable declarations
///
/// Usage:
///   var gen = JSGenerator.init(allocator);
///   defer gen.deinit();
///   const js_code = try gen.generate(typed_ast);

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const types = @import("../../ast/types.zig");
const node_mod = @import("../../ast/node.zig");

// Submodules
const emit_mod = @import("emit.zig");
const expr_mod = @import("expressions.zig");
const stmt_mod = @import("statements.zig");
const decl_mod = @import("declarations.zig");

pub const JSGenerator = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),
    indent_level: usize,
    /// Track if we're in a class context (for 'this' handling)
    in_class: bool,
    /// Track current class name (for constructor super calls)
    current_class: ?[]const u8,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .output = std.ArrayList(u8).init(allocator),
            .indent_level = 0,
            .in_class = false,
            .current_class = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.output.deinit();
    }

    /// Generate JavaScript from a typed AST
    pub fn generate(self: *Self, program: *ast.Node) ![]const u8 {
        std.debug.assert(program.kind == .program);

        // Emit program statements
        for (program.data.program.statements) |stmt| {
            try self.emitNode(stmt);
        }

        return try self.output.toOwnedSlice();
    }

    /// Main dispatch for node emission
    pub fn emitNode(self: *Self, node: *ast.Node) anyerror!void {
        switch (node.kind) {
            // Declarations
            .function_decl => try decl_mod.emitFunctionDecl(self, node),
            .class_decl => try decl_mod.emitClassDecl(self, node),
            .interface_decl => {}, // Interfaces are type-only, no JS output
            .type_alias_decl => {}, // Type aliases are type-only
            .variable_stmt => try decl_mod.emitVariableStmt(self, node),
            .import_decl => try decl_mod.emitImportDecl(self, node),
            .export_decl => try decl_mod.emitExportDecl(self, node),

            // Statements
            .block_stmt => try stmt_mod.emitBlockStmt(self, node),
            .expression_stmt => try stmt_mod.emitExpressionStmt(self, node),
            .if_stmt => try stmt_mod.emitIfStmt(self, node),
            .while_stmt => try stmt_mod.emitWhileStmt(self, node),
            .for_stmt => try stmt_mod.emitForStmt(self, node),
            .return_stmt => try stmt_mod.emitReturnStmt(self, node),
            .break_stmt => try emit_mod.emitSimpleStmt(self, "break"),
            .continue_stmt => try emit_mod.emitSimpleStmt(self, "continue"),

            // Expressions
            .number_literal => try expr_mod.emitNumberLiteral(self, node),
            .string_literal => try expr_mod.emitStringLiteral(self, node),
            .boolean_literal => try expr_mod.emitBooleanLiteral(self, node),
            .null_literal => try emit_mod.emit(self, "null"),
            .identifier => try expr_mod.emitIdentifier(self, node),
            .binary_expr => try expr_mod.emitBinaryExpr(self, node),
            .unary_expr => try expr_mod.emitUnaryExpr(self, node),
            .call_expr => try expr_mod.emitCallExpr(self, node),
            .member_expr => try expr_mod.emitMemberExpr(self, node),
            .new_expr => try expr_mod.emitNewExpr(self, node),
            .array_expr => try expr_mod.emitArrayExpr(self, node),
            .object_expr => try expr_mod.emitObjectExpr(self, node),
            .function_expr => try expr_mod.emitFunctionExpr(self, node),
            .conditional_expr => try expr_mod.emitConditionalExpr(self, node),
            .spread_element => {
                // Spread elements should be normalized away before codegen
                // If we reach here, just emit the spread syntax (fallback)
                try emit_mod.emit(self, "...");
                try self.emitNode(node.data.spread_element.argument);
            },

            // Class members (handled in declarations.zig)
            .property_decl, .method_decl, .constructor_decl => {},

            // Macro nodes should be expanded before codegen
            .macro_decl, .macro_invocation, .comptime_block, .compile_error, .quote_expr => {},

            // Program handled at top level
            .program => unreachable,

            // Type annotation - no JS output
            .type_annotation => {},
        }
    }

    // Re-export emit helpers for submodules
    pub const emit = emit_mod.emit;
    pub const emitIndent = emit_mod.emitIndent;
};

// Re-export for external use
pub const emit = emit_mod.emit;
pub const emitIndent = emit_mod.emitIndent;

// =============================================================================
// Tests
// =============================================================================

test "jsgen: number literal" {
    const allocator = std.testing.allocator;

    var num_node = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
    };

    var gen = JSGenerator.init(allocator);
    defer gen.deinit();

    try gen.emitNode(&num_node);
    const output = try gen.output.toOwnedSlice();
    defer allocator.free(output);

    try std.testing.expectEqualStrings("42", output);
}

test "jsgen: string literal" {
    const allocator = std.testing.allocator;

    var str_node = ast.Node{
        .kind = .string_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .string_literal = "hello" },
    };

    var gen = JSGenerator.init(allocator);
    defer gen.deinit();

    try gen.emitNode(&str_node);
    const output = try gen.output.toOwnedSlice();
    defer allocator.free(output);

    try std.testing.expectEqualStrings("\"hello\"", output);
}

test "jsgen: binary expression" {
    const allocator = std.testing.allocator;

    var left = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 1.0 },
    };

    var right = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 2.0 },
    };

    var binary = ast.Node{
        .kind = .binary_expr,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .add,
            .left = &left,
            .right = &right,
        } },
    };

    var gen = JSGenerator.init(allocator);
    defer gen.deinit();

    try gen.emitNode(&binary);
    const output = try gen.output.toOwnedSlice();
    defer allocator.free(output);

    try std.testing.expectEqualStrings("(1 + 2)", output);
}
