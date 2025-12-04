/// Type Factory
///
/// Factory functions for creating type objects.
/// Extracted from typechecker.zig to improve modularity.
///
/// All functions take an allocator for type allocation.

const std = @import("std");
const types = @import("../ast/types.zig");
const ast = @import("../ast/ast.zig");
const location = @import("../ast/location.zig");

/// Type factory for creating type objects
pub const TypeFactory = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) TypeFactory {
        return .{ .allocator = allocator };
    }

    /// Create a void type
    pub fn createVoidType(self: *TypeFactory, loc: location.SourceLocation) !*types.Type {
        const t = try self.allocator.create(types.Type);
        t.* = .{
            .kind = .void,
            .location = loc,
            .data = .{ .void = {} },
        };
        return t;
    }

    /// Create an unknown type
    pub fn createUnknownType(self: *TypeFactory, loc: location.SourceLocation) !*types.Type {
        const t = try self.allocator.create(types.Type);
        t.* = .{
            .kind = .unknown,
            .location = loc,
            .data = .{ .unknown = {} },
        };
        return t;
    }

    /// Create a function type from type parameters, parameters and return type
    pub fn createFunctionType(
        self: *TypeFactory,
        type_params: []const types.GenericParam,
        params: []const ast.node.FunctionExpr.FunctionParam,
        return_type: ?*types.Type,
        loc: location.SourceLocation,
    ) !*types.Type {
        // Copy type parameters (generics like T, U)
        const func_type_params = try self.allocator.alloc(types.GenericParam, type_params.len);
        for (type_params, 0..) |tp, i| {
            func_type_params[i] = .{
                .name = tp.name,
                .constraint = tp.constraint,
                .default = tp.default,
            };
        }

        // Create function parameters
        const func_params = try self.allocator.alloc(types.FunctionType.FunctionParam, params.len);
        for (params, 0..) |param, i| {
            func_params[i] = .{
                .name = param.name,
                .type = param.type orelse try self.createUnknownType(loc),
                .optional = param.optional,
            };
        }

        // Create void type if no return type specified
        const ret_type = return_type orelse try self.createVoidType(loc);

        // Create function type data
        const func_type_data = try self.allocator.create(types.FunctionType);
        func_type_data.* = .{
            .type_params = func_type_params,
            .params = func_params,
            .return_type = ret_type,
        };

        // Create the function type
        const func_type = try self.allocator.create(types.Type);
        func_type.* = .{
            .kind = .function,
            .location = loc,
            .data = .{ .function = func_type_data },
        };

        return func_type;
    }

    /// Create an object type from class members
    /// Also computes is_cyclic for ORC optimization
    pub fn createClassType(self: *TypeFactory, members: []*ast.Node, loc: location.SourceLocation) !*types.Type {
        return self.createClassTypeNamed(null, members, loc);
    }

    /// Create an object type from class members with a name
    /// Also computes is_cyclic for ORC optimization
    pub fn createClassTypeNamed(self: *TypeFactory, name: ?[]const u8, members: []*ast.Node, loc: location.SourceLocation) !*types.Type {
        // Count properties and methods
        var prop_count: usize = 0;
        var method_count: usize = 0;
        for (members) |member| {
            switch (member.kind) {
                .property_decl => prop_count += 1,
                .method_decl => method_count += 1,
                else => {},
            }
        }

        // Create property array and check for reference types (is_cyclic detection)
        const properties = try self.allocator.alloc(types.ObjectType.Property, prop_count);
        var prop_idx: usize = 0;
        var has_ref_field = false;

        for (members) |member| {
            if (member.kind == .property_decl) {
                const prop = &member.data.property_decl;
                const prop_type = prop.type orelse try self.createUnknownType(loc);
                properties[prop_idx] = .{
                    .name = prop.name,
                    .type = prop_type,
                    .optional = false,
                };
                prop_idx += 1;

                // Check if this property can form cycles
                // MVP: any reference type field means potentially cyclic
                if (isReferenceType(prop_type)) {
                    has_ref_field = true;
                }
            }
        }

        // Create method array
        const methods = try self.allocator.alloc(types.ObjectType.Property, method_count);
        var method_idx: usize = 0;
        for (members) |member| {
            if (member.kind == .method_decl) {
                const method = &member.data.method_decl;
                methods[method_idx] = .{
                    .name = method.name,
                    .type = try self.createFunctionType(method.type_params, method.params, method.return_type, loc),
                    .optional = false,
                };
                method_idx += 1;
            }
        }

        // Create object type with is_cyclic computed
        const obj_type_data = try self.allocator.create(types.ObjectType);
        obj_type_data.* = .{
            .properties = properties,
            .methods = methods,
            .name = name,
            .is_cyclic = has_ref_field, // MVP: ref field = potentially cyclic
        };

        const obj_type = try self.allocator.create(types.Type);
        obj_type.* = .{
            .kind = .object,
            .location = loc,
            .data = .{ .object = obj_type_data },
        };

        return obj_type;
    }

    /// Check if a type is a reference type (can participate in cycles)
    /// Public for testing
    pub fn isReferenceType(t: *types.Type) bool {
        return switch (t.kind) {
            // Reference types that need RC management
            .object, .array, .type_reference => true,
            // Primitives - no RC needed
            .number, .string, .boolean, .void, .unknown, .never => false,
            .int8, .int16, .int32, .int64 => false,
            .uint8, .uint16, .uint32, .uint64 => false,
            .float32, .float64 => false,
            // Special cases
            .function => false, // Functions don't form data cycles
            .tuple => false, // TODO: check element types
            .generic_param, .generic_instance => false,
            .ref, .lent => true, // Explicit ref types
            .@"union", .intersection => false, // TODO: check member types
        };
    }

    /// Create a number type
    pub fn createNumberType(self: *TypeFactory, loc: location.SourceLocation) !*types.Type {
        const t = try self.allocator.create(types.Type);
        t.* = .{
            .kind = .number,
            .location = loc,
            .data = .{ .number = {} },
        };
        return t;
    }

    /// Create a string type
    pub fn createStringType(self: *TypeFactory, loc: location.SourceLocation) !*types.Type {
        const t = try self.allocator.create(types.Type);
        t.* = .{
            .kind = .string,
            .location = loc,
            .data = .{ .string = {} },
        };
        return t;
    }

    /// Create a boolean type
    pub fn createBooleanType(self: *TypeFactory, loc: location.SourceLocation) !*types.Type {
        const t = try self.allocator.create(types.Type);
        t.* = .{
            .kind = .boolean,
            .location = loc,
            .data = .{ .boolean = {} },
        };
        return t;
    }

    /// Create an array type
    pub fn createArrayType(self: *TypeFactory, element_type: *types.Type, loc: location.SourceLocation) !*types.Type {
        const t = try self.allocator.create(types.Type);
        t.* = .{
            .kind = .array,
            .location = loc,
            .data = .{ .array = element_type },
        };
        return t;
    }

    /// Create a generic parameter type
    pub fn createGenericParamType(self: *TypeFactory, param: types.GenericParam, loc: location.SourceLocation) !*types.Type {
        const gp_data = try self.allocator.create(types.GenericParam);
        gp_data.* = param;
        const t = try self.allocator.create(types.Type);
        t.* = .{
            .kind = .generic_param,
            .location = loc,
            .data = .{ .generic_param = gp_data },
        };
        return t;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "TypeFactory: create void type" {
    const testing = std.testing;
    var factory = TypeFactory.init(testing.allocator);

    const void_type = try factory.createVoidType(location.SourceLocation.dummy());
    defer testing.allocator.destroy(void_type);

    try testing.expectEqual(types.TypeKind.void, void_type.kind);
}

test "TypeFactory: create unknown type" {
    const testing = std.testing;
    var factory = TypeFactory.init(testing.allocator);

    const unknown_type = try factory.createUnknownType(location.SourceLocation.dummy());
    defer testing.allocator.destroy(unknown_type);

    try testing.expectEqual(types.TypeKind.unknown, unknown_type.kind);
}

test "TypeFactory: create number type" {
    const testing = std.testing;
    var factory = TypeFactory.init(testing.allocator);

    const num_type = try factory.createNumberType(location.SourceLocation.dummy());
    defer testing.allocator.destroy(num_type);

    try testing.expectEqual(types.TypeKind.number, num_type.kind);
}

test "TypeFactory: create function type with no params" {
    const testing = std.testing;
    var factory = TypeFactory.init(testing.allocator);

    const func_type = try factory.createFunctionType(
        &[_]types.GenericParam{},
        &[_]ast.node.FunctionExpr.FunctionParam{},
        null, // void return
        location.SourceLocation.dummy(),
    );

    try testing.expectEqual(types.TypeKind.function, func_type.kind);
    try testing.expectEqual(@as(usize, 0), func_type.data.function.params.len);
    try testing.expectEqual(types.TypeKind.void, func_type.data.function.return_type.kind);

    // Cleanup
    testing.allocator.free(func_type.data.function.type_params);
    testing.allocator.free(func_type.data.function.params);
    testing.allocator.destroy(func_type.data.function.return_type);
    testing.allocator.destroy(func_type.data.function);
    testing.allocator.destroy(func_type);
}

// ============================================================================
// is_cyclic detection tests (ORC integration)
// ============================================================================

test "TypeFactory: acyclic class (primitives only) has is_cyclic=false" {
    // class Point { x: number; y: number; }
    const testing = std.testing;
    var factory = TypeFactory.init(testing.allocator);
    const loc = location.SourceLocation.dummy();

    // Create number type for properties
    const num_type = try factory.createNumberType(loc);

    // Create property nodes
    var prop_x = ast.Node{
        .kind = .property_decl,
        .location = loc,
        .type = null,
        .data = .{ .property_decl = .{
            .name = "x",
            .type = num_type,
            .init = null,
            .readonly = false,
        } },
    };
    var prop_y = ast.Node{
        .kind = .property_decl,
        .location = loc,
        .type = null,
        .data = .{ .property_decl = .{
            .name = "y",
            .type = num_type,
            .init = null,
            .readonly = false,
        } },
    };

    var members = [_]*ast.Node{ &prop_x, &prop_y };

    const class_type = try factory.createClassTypeNamed("Point", &members, loc);

    // Point has only primitive fields → NOT cyclic
    try testing.expectEqual(false, class_type.data.object.is_cyclic.?);
    try testing.expectEqualStrings("Point", class_type.data.object.name.?);

    // Cleanup
    testing.allocator.free(class_type.data.object.properties);
    testing.allocator.free(class_type.data.object.methods);
    testing.allocator.destroy(class_type.data.object);
    testing.allocator.destroy(class_type);
    testing.allocator.destroy(num_type);
}

test "TypeFactory: cyclic class (self-reference) has is_cyclic=true" {
    // class Node { next: Node; }
    const testing = std.testing;
    var factory = TypeFactory.init(testing.allocator);
    const loc = location.SourceLocation.dummy();

    // Create a type_reference for "Node" (self-reference)
    const type_ref_data = try testing.allocator.create(types.TypeReference);
    type_ref_data.* = .{
        .name = "Node",
        .type_args = &[_]*types.Type{},
        .resolved = null,
    };
    const node_type_ref = try testing.allocator.create(types.Type);
    node_type_ref.* = .{
        .kind = .type_reference,
        .location = loc,
        .data = .{ .type_reference = type_ref_data },
    };

    // Create property node
    var prop_next = ast.Node{
        .kind = .property_decl,
        .location = loc,
        .type = null,
        .data = .{ .property_decl = .{
            .name = "next",
            .type = node_type_ref,
            .init = null,
            .readonly = false,
        } },
    };

    var members = [_]*ast.Node{&prop_next};

    const class_type = try factory.createClassTypeNamed("Node", &members, loc);

    // Node has a type_reference field → IS cyclic
    try testing.expectEqual(true, class_type.data.object.is_cyclic.?);
    try testing.expectEqualStrings("Node", class_type.data.object.name.?);

    // Cleanup
    testing.allocator.free(class_type.data.object.properties);
    testing.allocator.free(class_type.data.object.methods);
    testing.allocator.destroy(class_type.data.object);
    testing.allocator.destroy(class_type);
    testing.allocator.destroy(node_type_ref);
    testing.allocator.destroy(type_ref_data);
}

test "TypeFactory: class with array field has is_cyclic=true" {
    // class Container { items: Item[]; }
    const testing = std.testing;
    var factory = TypeFactory.init(testing.allocator);
    const loc = location.SourceLocation.dummy();

    // Create array type
    const elem_type = try factory.createNumberType(loc);
    const array_type = try factory.createArrayType(elem_type, loc);

    // Create property node
    var prop_items = ast.Node{
        .kind = .property_decl,
        .location = loc,
        .type = null,
        .data = .{ .property_decl = .{
            .name = "items",
            .type = array_type,
            .init = null,
            .readonly = false,
        } },
    };

    var members = [_]*ast.Node{&prop_items};

    const class_type = try factory.createClassTypeNamed("Container", &members, loc);

    // Container has an array field → IS cyclic (conservative)
    try testing.expectEqual(true, class_type.data.object.is_cyclic.?);

    // Cleanup
    testing.allocator.free(class_type.data.object.properties);
    testing.allocator.free(class_type.data.object.methods);
    testing.allocator.destroy(class_type.data.object);
    testing.allocator.destroy(class_type);
    testing.allocator.destroy(array_type);
    testing.allocator.destroy(elem_type);
}

test "TypeFactory: isReferenceType correctly identifies types" {
    const testing = std.testing;
    var factory = TypeFactory.init(testing.allocator);
    const loc = location.SourceLocation.dummy();

    // Primitives → NOT reference types
    const num_type = try factory.createNumberType(loc);
    try testing.expectEqual(false, TypeFactory.isReferenceType(num_type));

    const bool_type = try factory.createBooleanType(loc);
    try testing.expectEqual(false, TypeFactory.isReferenceType(bool_type));

    const void_type = try factory.createVoidType(loc);
    try testing.expectEqual(false, TypeFactory.isReferenceType(void_type));

    // Array → IS reference type
    const array_type = try factory.createArrayType(num_type, loc);
    try testing.expectEqual(true, TypeFactory.isReferenceType(array_type));

    // Cleanup
    testing.allocator.destroy(num_type);
    testing.allocator.destroy(bool_type);
    testing.allocator.destroy(void_type);
    testing.allocator.destroy(array_type);
}
