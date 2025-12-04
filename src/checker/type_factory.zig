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
    pub fn createClassType(self: *TypeFactory, members: []*ast.Node, loc: location.SourceLocation) !*types.Type {
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

        // Create property array
        const properties = try self.allocator.alloc(types.ObjectType.Property, prop_count);
        var prop_idx: usize = 0;
        for (members) |member| {
            if (member.kind == .property_decl) {
                const prop = &member.data.property_decl;
                properties[prop_idx] = .{
                    .name = prop.name,
                    .type = prop.type orelse try self.createUnknownType(loc),
                    .optional = false,
                };
                prop_idx += 1;
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

        // Create object type
        const obj_type_data = try self.allocator.create(types.ObjectType);
        obj_type_data.* = .{
            .properties = properties,
            .methods = methods,
        };

        const obj_type = try self.allocator.create(types.Type);
        obj_type.* = .{
            .kind = .object,
            .location = loc,
            .data = .{ .object = obj_type_data },
        };

        return obj_type;
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
