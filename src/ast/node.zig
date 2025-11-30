const std = @import("std");
const location = @import("location.zig");
const types = @import("types.zig");

/// AST Node kinds - represents all TypeScript syntax we support
pub const NodeKind = enum {
    // ===== Literals =====
    number_literal,
    string_literal,
    boolean_literal,
    null_literal,

    // ===== Identifiers =====
    identifier,

    // ===== Expressions =====
    binary_expr,      // a + b, a - b, a * b, etc.
    unary_expr,       // -a, !a, typeof a
    call_expr,        // foo(x, y)
    member_expr,      // obj.prop, obj[key]
    new_expr,         // new Foo()
    array_expr,       // [1, 2, 3]
    object_expr,      // { x: 1, y: 2 }
    function_expr,    // function() {} or () => {}
    conditional_expr, // a ? b : c

    // ===== Statements =====
    block_stmt,       // { ... }
    expression_stmt,  // expr;
    if_stmt,          // if (cond) { ... }
    while_stmt,       // while (cond) { ... }
    for_stmt,         // for (init; cond; update) { ... }
    return_stmt,      // return expr;
    break_stmt,       // break;
    continue_stmt,    // continue;
    variable_stmt,    // const x = 1; let y = 2;

    // ===== Declarations =====
    function_decl,    // function foo() { ... }
    class_decl,       // class Foo { ... }
    interface_decl,   // interface Foo { ... }
    type_alias_decl,  // type Foo = Bar;
    import_decl,      // import { x } from "mod";
    export_decl,      // export { x };

    // ===== Class members =====
    property_decl,    // x: number;
    method_decl,      // method() { ... }
    constructor_decl, // constructor() { ... }

    // ===== Type annotations =====
    type_annotation,  // : Type

    // ===== Macro-specific nodes =====
    macro_invocation, // @derive(Eq, Hash)
    comptime_block,   // @comptime { ... }
    compile_error,    // @compileError("message")

    // ===== Program root =====
    program,          // Top-level module
};

/// Binary operators
pub const BinaryOp = enum {
    // Arithmetic
    add,      // +
    sub,      // -
    mul,      // *
    div,      // /
    mod,      // %

    // Comparison
    eq,       // ===
    ne,       // !==
    lt,       // <
    le,       // <=
    gt,       // >
    ge,       // >=

    // Logical
    @"and",   // &&
    @"or",    // ||

    // Bitwise
    bit_and,  // &
    bit_or,   // |
    bit_xor,  // ^
    shl,      // <<
    shr,      // >>
};

/// Unary operators
pub const UnaryOp = enum {
    neg,      // -a
    not,      // !a
    bit_not,  // ~a
    typeof,   // typeof a
    void,     // void a
};

/// AST Node - the fundamental building block
pub const Node = struct {
    kind: NodeKind,
    location: location.SourceLocation,
    data: NodeData,

    /// Type annotation (filled in by type checker)
    type: ?*types.Type = null,

    pub const NodeData = union(NodeKind) {
        // Literals
        number_literal: f64,
        string_literal: []const u8,
        boolean_literal: bool,
        null_literal: void,

        // Identifiers
        identifier: []const u8,

        // Expressions
        binary_expr: BinaryExpr,
        unary_expr: UnaryExpr,
        call_expr: CallExpr,
        member_expr: MemberExpr,
        new_expr: NewExpr,
        array_expr: ArrayExpr,
        object_expr: ObjectExpr,
        function_expr: FunctionExpr,
        conditional_expr: ConditionalExpr,

        // Statements
        block_stmt: BlockStmt,
        expression_stmt: *Node,
        if_stmt: IfStmt,
        while_stmt: WhileStmt,
        for_stmt: ForStmt,
        return_stmt: ReturnStmt,
        break_stmt: void,
        continue_stmt: void,
        variable_stmt: VariableStmt,

        // Declarations
        function_decl: FunctionDecl,
        class_decl: ClassDecl,
        interface_decl: InterfaceDecl,
        type_alias_decl: TypeAliasDecl,
        import_decl: ImportDecl,
        export_decl: ExportDecl,

        // Class members
        property_decl: PropertyDecl,
        method_decl: MethodDecl,
        constructor_decl: ConstructorDecl,

        // Type annotations
        type_annotation: *types.Type,

        // Macros - THIS IS KEY!
        macro_invocation: MacroInvocation,
        comptime_block: ComptimeBlock,
        compile_error: []const u8,

        // Program
        program: Program,
    };

    pub fn isExpression(self: *const Node) bool {
        return switch (self.kind) {
            .number_literal,
            .string_literal,
            .boolean_literal,
            .null_literal,
            .identifier,
            .binary_expr,
            .unary_expr,
            .call_expr,
            .member_expr,
            .new_expr,
            .array_expr,
            .object_expr,
            .function_expr,
            .conditional_expr,
            => true,
            else => false,
        };
    }

    pub fn isStatement(self: *const Node) bool {
        return switch (self.kind) {
            .block_stmt,
            .expression_stmt,
            .if_stmt,
            .while_stmt,
            .for_stmt,
            .return_stmt,
            .break_stmt,
            .continue_stmt,
            .variable_stmt,
            => true,
            else => false,
        };
    }
};

// ===== Expression node types =====

pub const BinaryExpr = struct {
    op: BinaryOp,
    left: *Node,
    right: *Node,
};

pub const UnaryExpr = struct {
    op: UnaryOp,
    argument: *Node,
};

pub const CallExpr = struct {
    callee: *Node,
    arguments: []*Node,
    type_args: []*types.Type, // For generic calls like foo<T>(x)
};

pub const MemberExpr = struct {
    object: *Node,
    property: *Node,
    computed: bool, // obj[prop] vs obj.prop
};

pub const NewExpr = struct {
    callee: *Node,
    arguments: []*Node,
    type_args: []*types.Type,
};

pub const ArrayExpr = struct {
    elements: []*Node,
};

pub const ObjectExpr = struct {
    properties: []ObjectProperty,

    pub const ObjectProperty = struct {
        key: *Node,
        value: *Node,
        shorthand: bool, // { x } vs { x: x }
    };
};

pub const FunctionExpr = struct {
    name: ?[]const u8,
    type_params: []types.GenericParam,
    params: []FunctionParam,
    return_type: ?*types.Type,
    body: *Node,
    is_arrow: bool,

    pub const FunctionParam = struct {
        name: []const u8,
        type: ?*types.Type,
        optional: bool,
        default_value: ?*Node,
    };
};

pub const ConditionalExpr = struct {
    condition: *Node,
    consequent: *Node,
    alternate: *Node,
};

// ===== Statement node types =====

pub const BlockStmt = struct {
    statements: []*Node,
};

pub const IfStmt = struct {
    condition: *Node,
    consequent: *Node,
    alternate: ?*Node,
};

pub const WhileStmt = struct {
    condition: *Node,
    body: *Node,
};

pub const ForStmt = struct {
    init: ?*Node,
    condition: ?*Node,
    update: ?*Node,
    body: *Node,
};

pub const ReturnStmt = struct {
    argument: ?*Node,
};

pub const VariableStmt = struct {
    kind: VariableKind,
    declarations: []VariableDeclarator,

    pub const VariableKind = enum {
        @"const",
        let,
        @"var", // Discouraged but supported
    };

    pub const VariableDeclarator = struct {
        name: []const u8,
        type: ?*types.Type,
        init: ?*Node,
    };
};

// ===== Declaration node types =====

pub const FunctionDecl = struct {
    name: []const u8,
    type_params: []types.GenericParam,
    params: []FunctionExpr.FunctionParam,
    return_type: ?*types.Type,
    body: ?*Node, // null for declarations in interfaces
};

pub const ClassDecl = struct {
    name: []const u8,
    type_params: []types.GenericParam,
    extends: ?*types.Type,
    implements: []*types.Type,
    members: []*Node, // PropertyDecl, MethodDecl, ConstructorDecl
    decorators: []Decorator = &[_]Decorator{}, // @derive(Eq, Hash), etc.

    pub const Decorator = struct {
        name: []const u8,
        arguments: []*Node, // Identifier nodes for traits
    };
};

pub const InterfaceDecl = struct {
    name: []const u8,
    type_params: []types.GenericParam,
    extends: []*types.Type,
    members: []*Node,
};

pub const TypeAliasDecl = struct {
    name: []const u8,
    type_params: []types.GenericParam,
    type: *types.Type,
};

pub const ImportDecl = struct {
    specifiers: []ImportSpecifier,
    source: []const u8,

    pub const ImportSpecifier = struct {
        imported: []const u8,
        local: []const u8,
    };
};

pub const ExportDecl = struct {
    declaration: ?*Node,
    specifiers: []ExportSpecifier,

    pub const ExportSpecifier = struct {
        local: []const u8,
        exported: []const u8,
    };
};

// ===== Class member types =====

pub const PropertyDecl = struct {
    name: []const u8,
    type: ?*types.Type,
    init: ?*Node,
    readonly: bool,
};

pub const MethodDecl = struct {
    name: []const u8,
    type_params: []types.GenericParam,
    params: []FunctionExpr.FunctionParam,
    return_type: ?*types.Type,
    body: ?*Node,
};

pub const ConstructorDecl = struct {
    params: []FunctionExpr.FunctionParam,
    body: *Node,
};

// ===== Macro-specific types (CRITICAL FOR METAPROGRAMMING!) =====

/// Macro invocation: @derive(Eq, Hash), @comptime, etc.
pub const MacroInvocation = struct {
    name: []const u8,          // "derive", "serialize", etc.
    arguments: []MacroArgument, // Eq, Hash, etc.
    target: ?*Node,            // The declaration this macro applies to

    pub const MacroArgument = union(enum) {
        identifier: []const u8,
        type: *types.Type,
        expression: *Node,
    };
};

/// Compile-time block: @comptime { ... }
pub const ComptimeBlock = struct {
    body: *Node, // Executed at compile-time, result embedded in binary
};

// ===== Program root =====

pub const Program = struct {
    statements: []*Node,
    file_id: location.FileId,
};

test "node kind checks" {
    const num_node = Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
    };

    try std.testing.expect(num_node.isExpression());
    try std.testing.expect(!num_node.isStatement());
}
