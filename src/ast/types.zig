const std = @import("std");
const location = @import("location.zig");

/// Type kinds in Metascript type system
pub const TypeKind = enum {
    // Primitives
    number,
    string,
    boolean,
    void,
    unknown,
    never,

    // Compound types
    object,
    array,
    tuple,
    function,

    // Generic types
    generic_param,
    generic_instance,

    // Special
    @"union",
    intersection,
    type_reference,
};

/// Type representation in AST
pub const Type = struct {
    kind: TypeKind,
    location: location.SourceLocation,
    data: TypeData,

    pub const TypeData = union(TypeKind) {
        number: void,
        string: void,
        boolean: void,
        void: void,
        unknown: void,
        never: void,

        object: *ObjectType,
        array: *Type,
        tuple: *TupleType,
        function: *FunctionType,

        generic_param: *GenericParam,
        generic_instance: *GenericInstance,

        @"union": *UnionType,
        intersection: *IntersectionType,
        type_reference: *TypeReference,
    };

    pub fn isPrimitive(self: Type) bool {
        return switch (self.kind) {
            .number, .string, .boolean, .void, .unknown, .never => true,
            else => false,
        };
    }
};

/// Object type (interfaces and classes)
pub const ObjectType = struct {
    properties: []Property,
    methods: []Property,

    pub const Property = struct {
        name: []const u8,
        type: *Type,
        optional: bool,
    };
};

/// Tuple type
pub const TupleType = struct {
    elements: []*Type,
};

/// Function type
pub const FunctionType = struct {
    type_params: []GenericParam,
    params: []FunctionParam,
    return_type: *Type,

    pub const FunctionParam = struct {
        name: []const u8,
        type: *Type,
        optional: bool,
    };
};

/// Generic type parameter
pub const GenericParam = struct {
    name: []const u8,
    constraint: ?*Type,
    default: ?*Type,
};

/// Generic type instantiation
pub const GenericInstance = struct {
    base: *Type,
    type_args: []*Type,
};

/// Union type (A | B)
pub const UnionType = struct {
    types: []*Type,
};

/// Intersection type (A & B)
pub const IntersectionType = struct {
    types: []*Type,
};

/// Type reference (named type)
pub const TypeReference = struct {
    name: []const u8,
    type_args: []*Type,
};

test "type primitives" {
    const primitive = Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    try std.testing.expect(primitive.isPrimitive());
}
