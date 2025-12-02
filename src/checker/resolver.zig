/// Type Resolver for Metascript Type Checker
///
/// Resolves type annotations and references:
/// 1. Resolves type references (e.g., "User" -> actual User type)
/// 2. Validates type annotations exist
/// 3. Stores resolved types on AST nodes
///
/// This runs before type inference - it handles explicit annotations.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const types = @import("../ast/types.zig");
const location = @import("../ast/location.zig");
const symbol_mod = @import("symbol.zig");

pub const TypeResolver = struct {
    allocator: std.mem.Allocator,
    symbols: *symbol_mod.SymbolTable,
    errors: std.ArrayList(Error),

    /// Type resolution error
    pub const Error = struct {
        message: []const u8,
        location: location.SourceLocation,

        pub fn format(self: Error) []const u8 {
            return self.message;
        }
    };

    pub fn init(allocator: std.mem.Allocator, symbols: *symbol_mod.SymbolTable) TypeResolver {
        return .{
            .allocator = allocator,
            .symbols = symbols,
            .errors = std.ArrayList(Error).init(allocator),
        };
    }

    pub fn deinit(self: *TypeResolver) void {
        self.errors.deinit();
    }

    /// Resolve all type references in an AST
    pub fn resolve(self: *TypeResolver, node: *ast.Node) !void {
        try self.resolveNode(node);
    }

    /// Resolve types in a single node and recurse
    fn resolveNode(self: *TypeResolver, node: *ast.Node) !void {
        switch (node.kind) {
            .program => {
                for (node.data.program.statements) |stmt| {
                    try self.resolveNode(stmt);
                }
            },
            .variable_stmt => {
                for (node.data.variable_stmt.declarations) |*decl| {
                    // Resolve type annotation if present
                    if (decl.type) |type_ann| {
                        try self.resolveType(type_ann);
                    }
                    // Resolve initializer
                    if (decl.init) |init_expr| {
                        try self.resolveNode(init_expr);
                    }
                }
            },
            .function_decl => {
                const func = &node.data.function_decl;

                // Resolve parameter types
                for (func.params) |*param| {
                    if (param.type) |type_ann| {
                        try self.resolveType(type_ann);
                    }
                    if (param.default_value) |default| {
                        try self.resolveNode(default);
                    }
                }

                // Resolve return type
                if (func.return_type) |ret| {
                    try self.resolveType(ret);
                }

                // Resolve function body
                if (func.body) |body| {
                    try self.resolveNode(body);
                }
            },
            .class_decl => {
                const class = &node.data.class_decl;

                // Resolve extends
                if (class.extends) |ext| {
                    try self.resolveType(ext);
                }

                // Resolve implements
                for (class.implements) |impl| {
                    try self.resolveType(impl);
                }

                // Resolve members
                for (class.members) |member| {
                    try self.resolveNode(member);
                }
            },
            .interface_decl => {
                const iface = &node.data.interface_decl;

                // Resolve extends
                for (iface.extends) |ext| {
                    try self.resolveType(ext);
                }

                // Resolve members
                for (iface.members) |member| {
                    try self.resolveNode(member);
                }
            },
            .property_decl => {
                const prop = &node.data.property_decl;
                if (prop.type) |type_ann| {
                    try self.resolveType(type_ann);
                }
                if (prop.init) |prop_init| {
                    try self.resolveNode(prop_init);
                }
            },
            .method_decl => {
                const method = &node.data.method_decl;

                // Resolve parameter types
                for (method.params) |*param| {
                    if (param.type) |type_ann| {
                        try self.resolveType(type_ann);
                    }
                }

                // Resolve return type
                if (method.return_type) |ret| {
                    try self.resolveType(ret);
                }

                // Resolve body
                if (method.body) |body| {
                    try self.resolveNode(body);
                }
            },
            .constructor_decl => {
                const ctor = &node.data.constructor_decl;

                // Resolve parameter types
                for (ctor.params) |*param| {
                    if (param.type) |type_ann| {
                        try self.resolveType(type_ann);
                    }
                }

                // Constructor body is always present
                try self.resolveNode(ctor.body);
            },
            .block_stmt => {
                for (node.data.block_stmt.statements) |stmt| {
                    try self.resolveNode(stmt);
                }
            },
            .if_stmt => {
                try self.resolveNode(node.data.if_stmt.condition);
                try self.resolveNode(node.data.if_stmt.consequent);
                if (node.data.if_stmt.alternate) |alt| {
                    try self.resolveNode(alt);
                }
            },
            .while_stmt => {
                try self.resolveNode(node.data.while_stmt.condition);
                try self.resolveNode(node.data.while_stmt.body);
            },
            .for_stmt => {
                if (node.data.for_stmt.init) |for_init| try self.resolveNode(for_init);
                if (node.data.for_stmt.condition) |cond| try self.resolveNode(cond);
                if (node.data.for_stmt.update) |update| try self.resolveNode(update);
                try self.resolveNode(node.data.for_stmt.body);
            },
            .return_stmt => {
                if (node.data.return_stmt.argument) |arg| {
                    try self.resolveNode(arg);
                }
            },
            .expression_stmt => {
                try self.resolveNode(node.data.expression_stmt);
            },
            .binary_expr => {
                try self.resolveNode(node.data.binary_expr.left);
                try self.resolveNode(node.data.binary_expr.right);
            },
            .unary_expr => {
                try self.resolveNode(node.data.unary_expr.argument);
            },
            .call_expr => {
                try self.resolveNode(node.data.call_expr.callee);
                for (node.data.call_expr.arguments) |arg| {
                    try self.resolveNode(arg);
                }
                // Resolve type arguments
                for (node.data.call_expr.type_args) |type_arg| {
                    try self.resolveType(type_arg);
                }
            },
            .member_expr => {
                try self.resolveNode(node.data.member_expr.object);
                try self.resolveNode(node.data.member_expr.property);
            },
            .new_expr => {
                try self.resolveNode(node.data.new_expr.callee);
                for (node.data.new_expr.arguments) |arg| {
                    try self.resolveNode(arg);
                }
            },
            .array_expr => {
                for (node.data.array_expr.elements) |elem| {
                    try self.resolveNode(elem);
                }
            },
            .object_expr => {
                for (node.data.object_expr.properties) |prop| {
                    switch (prop) {
                        .property => |p| {
                            try self.resolveNode(p.key);
                            try self.resolveNode(p.value);
                        },
                        .spread => |spread_node| {
                            try self.resolveNode(spread_node);
                        },
                    }
                }
            },
            .function_expr => {
                const func = &node.data.function_expr;
                for (func.params) |*param| {
                    if (param.type) |type_ann| {
                        try self.resolveType(type_ann);
                    }
                }
                if (func.return_type) |ret| {
                    try self.resolveType(ret);
                }
                try self.resolveNode(func.body);
            },
            .conditional_expr => {
                try self.resolveNode(node.data.conditional_expr.condition);
                try self.resolveNode(node.data.conditional_expr.consequent);
                try self.resolveNode(node.data.conditional_expr.alternate);
            },
            .spread_element => {
                try self.resolveNode(node.data.spread_element.argument);
            },
            // Type alias - resolve the aliased type
            .type_alias_decl => {
                try self.resolveType(node.data.type_alias_decl.type);
            },
            // Nodes that don't need type resolution
            .number_literal,
            .string_literal,
            .boolean_literal,
            .null_literal,
            .identifier,
            .break_stmt,
            .continue_stmt,
            .import_decl,
            .export_decl,
            .macro_decl,
            .macro_invocation,
            .comptime_block,
            .compile_error,
            .type_annotation,
            => {},
        }
    }

    /// Resolve a type, checking that type references exist
    fn resolveType(self: *TypeResolver, t: *types.Type) !void {
        switch (t.kind) {
            // Primitives need no resolution
            .number, .string, .boolean, .void, .unknown, .never,
            .int8, .int16, .int32, .int64,
            .uint8, .uint16, .uint32, .uint64,
            .float32, .float64 => {},

            // Reference types - resolve the inner type
            .ref => {
                try self.resolveType(t.data.ref);
            },
            .lent => {
                try self.resolveType(t.data.lent);
            },

            // Type reference - look up in symbol table
            .type_reference => {
                const ref = t.data.type_reference;
                const name = ref.name;

                // Check if type exists in symbol table
                if (self.symbols.lookup(name)) |sym| {
                    // Verify it's a type (class, interface, type_alias)
                    switch (sym.kind) {
                        .class, .interface, .type_alias => {
                            // Valid type reference
                        },
                        else => {
                            try self.errors.append(.{
                                .message = "not a type",
                                .location = t.location,
                            });
                        },
                    }
                } else {
                    // Unknown type - check if it's a built-in
                    if (!isBuiltinType(name)) {
                        try self.errors.append(.{
                            .message = "unknown type",
                            .location = t.location,
                        });
                    }
                }

                // Resolve type arguments
                for (ref.type_args) |type_arg| {
                    try self.resolveType(type_arg);
                }
            },

            // Array - resolve element type
            .array => {
                try self.resolveType(t.data.array);
            },

            // Tuple - resolve element types
            .tuple => {
                for (t.data.tuple.elements) |elem| {
                    try self.resolveType(elem);
                }
            },

            // Function - resolve param and return types
            .function => {
                const func = t.data.function;
                for (func.params) |param| {
                    try self.resolveType(param.type);
                }
                try self.resolveType(func.return_type);
            },

            // Object - resolve property types
            .object => {
                const obj = t.data.object;
                for (obj.properties) |prop| {
                    try self.resolveType(prop.type);
                }
                for (obj.methods) |method| {
                    try self.resolveType(method.type);
                }
            },

            // Union - resolve all types
            .@"union" => {
                for (t.data.@"union".types) |member| {
                    try self.resolveType(member);
                }
            },

            // Intersection - resolve all types
            .intersection => {
                for (t.data.intersection.types) |member| {
                    try self.resolveType(member);
                }
            },

            // Generic parameter - check constraint
            .generic_param => {
                if (t.data.generic_param.constraint) |constraint| {
                    try self.resolveType(constraint);
                }
                if (t.data.generic_param.default) |default| {
                    try self.resolveType(default);
                }
            },

            // Generic instance - resolve base and type args
            .generic_instance => {
                try self.resolveType(t.data.generic_instance.base);
                for (t.data.generic_instance.type_args) |arg| {
                    try self.resolveType(arg);
                }
            },
        }
    }

    /// Check if a type name is a built-in type
    fn isBuiltinType(name: []const u8) bool {
        const builtins = [_][]const u8{
            // Primitive types (TypeScript/JavaScript)
            "string",
            "number",
            "boolean",
            "void",
            "null",
            "undefined",
            "any",
            "unknown",
            "never",
            // Sized integer types (Metascript/C)
            "int8",
            "int16",
            "int32",
            "int64",
            "uint8",
            "uint16",
            "uint32",
            "uint64",
            // Sized floating-point types (Metascript/C)
            "float32",
            "float64",
            // Type aliases
            "int",
            "float",
            "double",
            // Built-in object types
            "Array",
            "Map",
            "Set",
            "Promise",
            "Date",
            "RegExp",
            "Error",
            "Object",
            "Function",
            "Symbol",
            "BigInt",
        };

        for (builtins) |builtin| {
            if (std.mem.eql(u8, name, builtin)) return true;
        }
        return false;
    }

    /// Check if there are any errors
    pub fn hasErrors(self: *TypeResolver) bool {
        return self.errors.items.len > 0;
    }

    /// Get all errors
    pub fn getErrors(self: *TypeResolver) []const Error {
        return self.errors.items;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "resolver: primitive types need no resolution" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var resolver = TypeResolver.init(allocator, &symbols);
    defer resolver.deinit();

    // Create a primitive type
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    try resolver.resolveType(&num_type);
    try std.testing.expect(!resolver.hasErrors());
}

test "resolver: unknown type reference reports error" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var resolver = TypeResolver.init(allocator, &symbols);
    defer resolver.deinit();

    // Create a type reference to unknown type
    var ref = types.TypeReference{
        .name = "UnknownType",
        .type_args = &[_]*types.Type{},
    };

    var ref_type = types.Type{
        .kind = .type_reference,
        .location = location.SourceLocation.dummy(),
        .data = .{ .type_reference = &ref },
    };

    try resolver.resolveType(&ref_type);

    // Should have an error
    try std.testing.expect(resolver.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), resolver.errors.items.len);
}

test "resolver: builtin types are allowed" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var resolver = TypeResolver.init(allocator, &symbols);
    defer resolver.deinit();

    // Test Array (builtin)
    var ref = types.TypeReference{
        .name = "Array",
        .type_args = &[_]*types.Type{},
    };

    var ref_type = types.Type{
        .kind = .type_reference,
        .location = location.SourceLocation.dummy(),
        .data = .{ .type_reference = &ref },
    };

    try resolver.resolveType(&ref_type);

    // Should NOT have an error
    try std.testing.expect(!resolver.hasErrors());
}

test "resolver: defined types are allowed" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    // Define a class type
    try symbols.define(symbol_mod.Symbol.init("User", .class, location.SourceLocation.dummy()));

    var resolver = TypeResolver.init(allocator, &symbols);
    defer resolver.deinit();

    // Reference the defined type
    var ref = types.TypeReference{
        .name = "User",
        .type_args = &[_]*types.Type{},
    };

    var ref_type = types.Type{
        .kind = .type_reference,
        .location = location.SourceLocation.dummy(),
        .data = .{ .type_reference = &ref },
    };

    try resolver.resolveType(&ref_type);

    // Should NOT have an error
    try std.testing.expect(!resolver.hasErrors());
}

test "resolver: non-type symbol reports error" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    // Define a variable (not a type)
    try symbols.define(symbol_mod.Symbol.init("myVar", .variable, location.SourceLocation.dummy()));

    var resolver = TypeResolver.init(allocator, &symbols);
    defer resolver.deinit();

    // Try to use variable as a type
    var ref = types.TypeReference{
        .name = "myVar",
        .type_args = &[_]*types.Type{},
    };

    var ref_type = types.Type{
        .kind = .type_reference,
        .location = location.SourceLocation.dummy(),
        .data = .{ .type_reference = &ref },
    };

    try resolver.resolveType(&ref_type);

    // Should have an error (myVar is not a type)
    try std.testing.expect(resolver.hasErrors());
}

test "resolver: isBuiltinType" {
    try std.testing.expect(TypeResolver.isBuiltinType("Array"));
    try std.testing.expect(TypeResolver.isBuiltinType("Map"));
    try std.testing.expect(TypeResolver.isBuiltinType("Promise"));
    try std.testing.expect(!TypeResolver.isBuiltinType("CustomType"));
    try std.testing.expect(!TypeResolver.isBuiltinType("User"));
}
