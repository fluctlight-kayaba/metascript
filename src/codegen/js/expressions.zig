/// Expression emission for JavaScript code generation
/// Handles all expression node types: literals, operators, calls, etc.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const node_mod = @import("../../ast/node.zig");
const emit_mod = @import("emit.zig");

const JSGenerator = @import("jsgen.zig").JSGenerator;

// =============================================================================
// Literals
// =============================================================================

pub fn emitNumberLiteral(gen: *JSGenerator, node: *ast.Node) !void {
    const num = node.data.number_literal;
    var buf: [32]u8 = undefined;
    const str = std.fmt.bufPrint(&buf, "{d}", .{num}) catch "0";
    try emit_mod.emit(gen, str);
}

pub fn emitStringLiteral(gen: *JSGenerator, node: *ast.Node) !void {
    const str = node.data.string_literal;
    try emit_mod.emit(gen, "\"");
    // Escape special characters
    const escaped = try emit_mod.escapeString(gen.allocator, str);
    defer gen.allocator.free(escaped);
    try emit_mod.emit(gen, escaped);
    try emit_mod.emit(gen, "\"");
}

pub fn emitBooleanLiteral(gen: *JSGenerator, node: *ast.Node) !void {
    const val = node.data.boolean_literal;
    try emit_mod.emit(gen, if (val) "true" else "false");
}

pub fn emitIdentifier(gen: *JSGenerator, node: *ast.Node) !void {
    try emit_mod.emit(gen, node.data.identifier);
}

// =============================================================================
// Operators
// =============================================================================

pub fn emitBinaryExpr(gen: *JSGenerator, node: *ast.Node) !void {
    const expr = &node.data.binary_expr;

    try emit_mod.emit(gen, "(");
    try gen.emitNode(expr.left);
    try emit_mod.emit(gen, " ");
    try emit_mod.emit(gen, emit_mod.binaryOpToJS(expr.op));
    try emit_mod.emit(gen, " ");
    try gen.emitNode(expr.right);
    try emit_mod.emit(gen, ")");
}

pub fn emitUnaryExpr(gen: *JSGenerator, node: *ast.Node) !void {
    const expr = &node.data.unary_expr;

    const op = emit_mod.unaryOpToJS(expr.op);
    try emit_mod.emit(gen, op);
    try gen.emitNode(expr.argument);
}

// =============================================================================
// Complex Expressions
// =============================================================================

pub fn emitCallExpr(gen: *JSGenerator, node: *ast.Node) !void {
    const call = &node.data.call_expr;

    try gen.emitNode(call.callee);
    try emit_mod.emit(gen, "(");

    for (call.arguments, 0..) |arg, i| {
        if (i > 0) try emit_mod.emit(gen, ", ");
        try gen.emitNode(arg);
    }

    try emit_mod.emit(gen, ")");
}

pub fn emitMemberExpr(gen: *JSGenerator, node: *ast.Node) !void {
    const expr = &node.data.member_expr;

    try gen.emitNode(expr.object);

    if (expr.computed) {
        try emit_mod.emit(gen, "[");
        try gen.emitNode(expr.property);
        try emit_mod.emit(gen, "]");
    } else {
        try emit_mod.emit(gen, ".");
        try gen.emitNode(expr.property);
    }
}

pub fn emitNewExpr(gen: *JSGenerator, node: *ast.Node) !void {
    const new_expr = &node.data.new_expr;

    try emit_mod.emit(gen, "new ");
    try gen.emitNode(new_expr.callee);
    try emit_mod.emit(gen, "(");

    for (new_expr.arguments, 0..) |arg, i| {
        if (i > 0) try emit_mod.emit(gen, ", ");
        try gen.emitNode(arg);
    }

    try emit_mod.emit(gen, ")");
}

pub fn emitArrayExpr(gen: *JSGenerator, node: *ast.Node) !void {
    const arr = &node.data.array_expr;

    try emit_mod.emit(gen, "[");
    for (arr.elements, 0..) |elem, i| {
        if (i > 0) try emit_mod.emit(gen, ", ");
        try gen.emitNode(elem);
    }
    try emit_mod.emit(gen, "]");
}

pub fn emitObjectExpr(gen: *JSGenerator, node: *ast.Node) !void {
    const obj = &node.data.object_expr;

    try emit_mod.emit(gen, "{ ");
    for (obj.properties, 0..) |prop, i| {
        if (i > 0) try emit_mod.emit(gen, ", ");
        switch (prop) {
            .property => |p| {
                try gen.emitNode(p.key);
                if (!p.shorthand) {
                    try emit_mod.emit(gen, ": ");
                    try gen.emitNode(p.value);
                }
            },
            .spread => |spread_node| {
                try emit_mod.emit(gen, "...");
                try gen.emitNode(spread_node.data.spread_element.argument);
            },
        }
    }
    try emit_mod.emit(gen, " }");
}

pub fn emitFunctionExpr(gen: *JSGenerator, node: *ast.Node) !void {
    const func = &node.data.function_expr;

    // Arrow function or function expression
    if (func.is_arrow) {
        try emit_mod.emit(gen, "(");
        try emitParams(gen, func.params);
        try emit_mod.emit(gen, ") => ");
        try gen.emitNode(func.body);
    } else {
        try emit_mod.emit(gen, "function");
        if (func.name) |name| {
            try emit_mod.emit(gen, " ");
            try emit_mod.emit(gen, name);
        }
        try emit_mod.emit(gen, "(");
        try emitParams(gen, func.params);
        try emit_mod.emit(gen, ") ");
        try gen.emitNode(func.body);
    }
}

pub fn emitConditionalExpr(gen: *JSGenerator, node: *ast.Node) !void {
    const cond = &node.data.conditional_expr;

    try emit_mod.emit(gen, "(");
    try gen.emitNode(cond.condition);
    try emit_mod.emit(gen, " ? ");
    try gen.emitNode(cond.consequent);
    try emit_mod.emit(gen, " : ");
    try gen.emitNode(cond.alternate);
    try emit_mod.emit(gen, ")");
}

// =============================================================================
// Helpers
// =============================================================================

pub fn emitParams(gen: *JSGenerator, params: []const node_mod.FunctionExpr.FunctionParam) !void {
    for (params, 0..) |param, i| {
        if (i > 0) try emit_mod.emit(gen, ", ");
        try emit_mod.emit(gen, param.name);
        if (param.default_value) |default| {
            try emit_mod.emit(gen, " = ");
            try gen.emitNode(default);
        }
    }
}
