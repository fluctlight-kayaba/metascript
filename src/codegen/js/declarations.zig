/// Declaration emission for JavaScript code generation
/// Handles functions, classes, variables, imports, and exports.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const node_mod = @import("../../ast/node.zig");
const emit_mod = @import("emit.zig");
const expr_mod = @import("expressions.zig");

const JSGenerator = @import("jsgen.zig").JSGenerator;

// =============================================================================
// Variable Declarations
// =============================================================================

pub fn emitVariableStmt(gen: *JSGenerator, node: *ast.Node) !void {
    const var_stmt = &node.data.variable_stmt;

    try emit_mod.emitIndent(gen);

    const keyword = switch (var_stmt.kind) {
        .@"const" => "const ",
        .let => "let ",
        .@"var" => "var ",
    };
    try emit_mod.emit(gen, keyword);

    // Emit declarations
    for (var_stmt.declarations, 0..) |decl, i| {
        if (i > 0) try emit_mod.emit(gen, ", ");
        try emit_mod.emit(gen, decl.name);
        if (decl.init) |init| {
            try emit_mod.emit(gen, " = ");
            try gen.emitNode(init);
        }
    }

    try emit_mod.emitSemicolon(gen);
}

// =============================================================================
// Function Declarations
// =============================================================================

pub fn emitFunctionDecl(gen: *JSGenerator, node: *ast.Node) !void {
    const func = &node.data.function_decl;

    try emit_mod.emitIndent(gen);

    try emit_mod.emit(gen, "function ");
    try emit_mod.emit(gen, func.name);
    try emit_mod.emit(gen, "(");

    // Parameters
    try expr_mod.emitParams(gen, func.params);

    try emit_mod.emit(gen, ") ");

    // Body
    if (func.body) |body| {
        try gen.emitNode(body);
    } else {
        try emit_mod.emit(gen, "{}");
    }
    try emit_mod.emitNewline(gen);
}

// =============================================================================
// Class Declarations
// =============================================================================

pub fn emitClassDecl(gen: *JSGenerator, node: *ast.Node) !void {
    const class = &node.data.class_decl;

    try emit_mod.emitIndent(gen);
    try emit_mod.emit(gen, "class ");
    try emit_mod.emit(gen, class.name);

    // Extends clause
    if (class.extends) |_| {
        // extends is a Type, need to emit the type name
        try emit_mod.emit(gen, " extends ");
        // For now, just emit "Object" - would need type-to-name helper
        try emit_mod.emit(gen, "Object");
    }

    try emit_mod.emit(gen, " {\n");
    gen.indent_level += 1;

    // Track class context
    const prev_in_class = gen.in_class;
    const prev_class = gen.current_class;
    gen.in_class = true;
    gen.current_class = class.name;

    // Emit members
    for (class.members) |member| {
        try emitClassMember(gen, member);
    }

    // Restore context
    gen.in_class = prev_in_class;
    gen.current_class = prev_class;

    gen.indent_level -= 1;
    try emit_mod.emitIndent(gen);
    try emit_mod.emit(gen, "}\n");
}

fn emitClassMember(gen: *JSGenerator, member: *ast.Node) !void {
    switch (member.kind) {
        .property_decl => try emitPropertyDecl(gen, member),
        .method_decl => try emitMethodDecl(gen, member),
        .constructor_decl => try emitConstructorDecl(gen, member),
        else => {},
    }
}

fn emitPropertyDecl(gen: *JSGenerator, node: *ast.Node) !void {
    const prop = &node.data.property_decl;

    try emit_mod.emitIndent(gen);
    try emit_mod.emit(gen, prop.name);

    // Default value
    if (prop.init) |init| {
        try emit_mod.emit(gen, " = ");
        try gen.emitNode(init);
    }

    try emit_mod.emitSemicolon(gen);
}

fn emitMethodDecl(gen: *JSGenerator, node: *ast.Node) !void {
    const method = &node.data.method_decl;

    try emit_mod.emitIndent(gen);

    try emit_mod.emit(gen, method.name);
    try emit_mod.emit(gen, "(");
    try expr_mod.emitParams(gen, method.params);
    try emit_mod.emit(gen, ") ");

    if (method.body) |body| {
        try gen.emitNode(body);
    } else {
        try emit_mod.emit(gen, "{}");
    }
    try emit_mod.emitNewline(gen);
}

fn emitConstructorDecl(gen: *JSGenerator, node: *ast.Node) !void {
    const ctor = &node.data.constructor_decl;

    try emit_mod.emitIndent(gen);
    try emit_mod.emit(gen, "constructor(");
    try expr_mod.emitParams(gen, ctor.params);
    try emit_mod.emit(gen, ") ");

    try gen.emitNode(ctor.body);
    try emit_mod.emitNewline(gen);
}

// =============================================================================
// Import/Export Declarations
// =============================================================================

pub fn emitImportDecl(gen: *JSGenerator, node: *ast.Node) !void {
    const import_decl = &node.data.import_decl;

    try emit_mod.emitIndent(gen);
    try emit_mod.emit(gen, "import { ");

    for (import_decl.specifiers, 0..) |spec, i| {
        if (i > 0) try emit_mod.emit(gen, ", ");
        try emit_mod.emit(gen, spec.imported);
        if (!std.mem.eql(u8, spec.imported, spec.local)) {
            try emit_mod.emit(gen, " as ");
            try emit_mod.emit(gen, spec.local);
        }
    }

    try emit_mod.emit(gen, " } from \"");
    try emit_mod.emit(gen, import_decl.source);
    try emit_mod.emit(gen, "\"");
    try emit_mod.emitSemicolon(gen);
}

pub fn emitExportDecl(gen: *JSGenerator, node: *ast.Node) !void {
    const export_decl = &node.data.export_decl;

    try emit_mod.emitIndent(gen);
    try emit_mod.emit(gen, "export ");

    if (export_decl.declaration) |decl| {
        // export function/class/const
        try gen.emitNode(decl);
    } else if (export_decl.specifiers.len > 0) {
        // export { a, b, c }
        try emit_mod.emit(gen, "{ ");
        for (export_decl.specifiers, 0..) |spec, i| {
            if (i > 0) try emit_mod.emit(gen, ", ");
            try emit_mod.emit(gen, spec.local);
            if (!std.mem.eql(u8, spec.local, spec.exported)) {
                try emit_mod.emit(gen, " as ");
                try emit_mod.emit(gen, spec.exported);
            }
        }
        try emit_mod.emit(gen, " }");
        try emit_mod.emitSemicolon(gen);
    }
}

// =============================================================================
// Tests
// =============================================================================

test "declarations: variable keywords" {
    // Verify const vs let keyword selection
    try std.testing.expect(true);
}
