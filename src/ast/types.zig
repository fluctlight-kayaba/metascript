const std = @import("std");
const location = @import("location.zig");

/// Type kinds in Metascript type system
pub const TypeKind = enum {
    // Primitives - legacy
    number, // Alias for float64 (JS compatibility)
    string,
    boolean,
    void,
    unknown,
    never,

    // Sized integer types
    int8,
    int16,
    int32,
    int64,
    uint8,
    uint16,
    uint32,
    uint64,

    // Sized floating-point types
    float32,
    float64,

    // Compound types
    object,
    array,
    tuple,
    function,

    // Generic types
    generic_param,
    generic_instance,

    // Memory management types (ORC)
    ref,  // ORC-managed heap reference (e.g., ref User)
    lent, // Borrowed reference (zero-copy, no RC, e.g., lent User)

    // Special
    @"union",
    intersection,
    type_reference,
};

/// Ownership kind for ORC optimization (compile-time RC elimination)
pub const OwnershipKind = enum {
    owned,    // Single owner, can move without RC
    borrowed, // Temporary borrow, no RC needed
    shared,   // Multiple owners, requires RC operations
};

/// Type representation in AST
pub const Type = struct {
    kind: TypeKind,
    location: location.SourceLocation,
    data: TypeData,

    /// Ownership tracking for ORC optimization (optional, computed by type checker)
    ownership: ?OwnershipKind = null,

    pub const TypeData = union(TypeKind) {
        number: void,
        string: void,
        boolean: void,
        void: void,
        unknown: void,
        never: void,

        // Sized integer types
        int8: void,
        int16: void,
        int32: void,
        int64: void,
        uint8: void,
        uint16: void,
        uint32: void,
        uint64: void,

        // Sized floating-point types
        float32: void,
        float64: void,

        object: *ObjectType,
        array: *Type,
        tuple: *TupleType,
        function: *FunctionType,

        generic_param: *GenericParam,
        generic_instance: *GenericInstance,

        // Memory management types (ORC)
        ref: *Type,   // Wrapped type (ref User → User)
        lent: *Type,  // Borrowed type (lent User → User)

        @"union": *UnionType,
        intersection: *IntersectionType,
        type_reference: *TypeReference,
    };

    pub fn isPrimitive(self: Type) bool {
        return switch (self.kind) {
            .number, .string, .boolean, .void, .unknown, .never,
            .int8, .int16, .int32, .int64,
            .uint8, .uint16, .uint32, .uint64,
            .float32, .float64 => true,
            else => false,
        };
    }

    /// Check if this type is a reference type (requires ORC management)
    pub fn isRefType(self: Type) bool {
        return switch (self.kind) {
            .ref => true,
            .string, .array, .object => true, // Built-in ref types
            else => false,
        };
    }

    /// Check if this type is a borrowed reference (no RC needed)
    pub fn isLentType(self: Type) bool {
        return self.kind == .lent;
    }
};

/// Object type (interfaces and classes)
pub const ObjectType = struct {
    properties: []Property,
    methods: []Property,
    /// Type name (for cyclicity detection and debugging)
    name: ?[]const u8 = null,
    /// Cached cyclicity result (computed lazily by type checker)
    /// null = not computed, true = can form cycles, false = acyclic
    is_cyclic: ?bool = null,

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
    /// Rest parameter for variadic functions (...args: T[])
    /// When set, the function accepts unlimited additional arguments of this type
    rest_param: ?*FunctionParam = null,

    pub const FunctionParam = struct {
        name: []const u8,
        type: *Type,
        optional: bool,
    };

    /// Check if this function is variadic (accepts unlimited args)
    pub fn isVariadic(self: *const FunctionType) bool {
        return self.rest_param != null;
    }
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
    /// Resolved type (filled by type checker)
    resolved: ?*Type = null,
};

// =============================================================================
// Type Resolution: follow() pattern (from Haxe)
// =============================================================================
// Every backend MUST call follow() before emitting code.
// This resolves type aliases, type variables, and lazy types to their concrete form.

/// Follow a type to its concrete representation.
/// This resolves:
/// - Type aliases (type UserId = number → number)
/// - Type references with resolved types
/// - Generic type variables with bounds
/// - ref/lent wrappers (ref User → User, lent User → User)
///
/// Example:
///   type UserId = number;
///   const x: UserId = 42;
///   follow(x.type) → number (not UserId)
///
///   const y: ref User = ...;
///   follow(y.type) → User (unwraps ref)
pub fn follow(t: ?*Type) ?*Type {
    if (t == null) return null;
    const typ = t.?;

    return switch (typ.kind) {
        // Type reference: check if resolved, otherwise return as-is
        .type_reference => {
            const ref = typ.data.type_reference;
            if (ref.resolved) |resolved| {
                // Recursively follow the resolved type
                return follow(resolved);
            }
            // Not resolved yet - return as-is (may be a builtin or forward ref)
            return typ;
        },

        // Generic parameter: if bound, follow the bound
        .generic_param => {
            // TODO: when generics are instantiated, the param will have a bound
            // For now, return the param as-is
            return typ;
        },

        // Generic instance: the base type is the concrete type
        .generic_instance => {
            // The instance itself is concrete (e.g., Array<number>)
            return typ;
        },

        // Memory management wrappers: follow through to base type
        // Note: For codegen, we need to know if something is ref/lent,
        // so backends should check BEFORE calling follow() if they need that info
        .ref => follow(typ.data.ref),
        .lent => follow(typ.data.lent),

        // All other types are already concrete
        .number, .string, .boolean, .void, .unknown, .never,
        .object, .array, .tuple, .function, .@"union", .intersection,
        .int8, .int16, .int32, .int64,
        .uint8, .uint16, .uint32, .uint64,
        .float32, .float64,
        => typ,
    };
}

// =============================================================================
// Cyclicity Detection (for ORC optimization)
// =============================================================================
// A type is cyclic if it can transitively contain a reference to itself.
// Examples:
//   class Node { next: Node }  → cyclic (directly references self)
//   class A { b: B } class B { a: A } → cyclic (A→B→A)
//   class Point { x: number; y: number } → acyclic (no ref fields)
//   class Container { items: number[] } → acyclic (array of primitives)
//
// This is a compile-time analysis used by codegen to decide:
//   - Cyclic types: use ms_decref_typed() with type_info
//   - Acyclic types: use ms_decref() (faster, no cycle check)

/// Check if a type can form reference cycles.
/// This is used for ORC optimization: acyclic types skip cycle detection.
///
/// Returns:
///   true = type can form cycles (needs cycle collector)
///   false = type cannot form cycles (fast path)
pub fn isCyclic(t: ?*Type) bool {
    return isCyclicWithVisited(t, &[_]*Type{});
}

/// Internal helper that tracks visited types to detect cycles
fn isCyclicWithVisited(t: ?*Type, visited: []const *Type) bool {
    if (t == null) return false;
    const typ = t.?;

    // Follow through type references and wrappers
    const resolved = follow(typ);
    if (resolved == null) return false;
    const resolved_type = resolved.?;

    // Check if we've already visited this type (cycle detected!)
    for (visited) |v| {
        if (v == resolved_type) return true;
    }

    return switch (resolved_type.kind) {
        // Primitives cannot form cycles
        .number, .string, .boolean, .void, .unknown, .never,
        .int8, .int16, .int32, .int64,
        .uint8, .uint16, .uint32, .uint64,
        .float32, .float64,
        => false,

        // Object types: check all properties for cycles
        .object => {
            const obj = resolved_type.data.object;

            // Use cached result if available
            if (obj.is_cyclic) |cached| return cached;

            // Build new visited list including this type
            // Note: Using fixed buffer for simplicity; in production use allocator
            var new_visited: [32]*Type = undefined;
            var i: usize = 0;
            for (visited) |v| {
                if (i >= 31) break;
                new_visited[i] = v;
                i += 1;
            }
            new_visited[i] = resolved_type;
            const new_slice = new_visited[0 .. i + 1];

            // Check each property
            for (obj.properties) |prop| {
                if (isCyclicWithVisited(prop.type, new_slice)) {
                    return true;
                }
            }
            return false;
        },

        // Array types: check element type
        .array => isCyclicWithVisited(resolved_type.data.array, visited),

        // Tuple types: check all elements
        .tuple => {
            const tuple = resolved_type.data.tuple;
            for (tuple.elements) |elem| {
                if (isCyclicWithVisited(elem, visited)) return true;
            }
            return false;
        },

        // Function types generally don't cause cycles
        // (closures may capture cyclic data, but that's handled at runtime)
        .function => false,

        // Generic instances: check base type
        .generic_instance => {
            const inst = resolved_type.data.generic_instance;
            return isCyclicWithVisited(inst.base, visited);
        },

        // Generic params: conservative (may be cyclic when instantiated)
        .generic_param => true,

        // Union/intersection: any member being cyclic makes it cyclic
        .@"union" => {
            for (resolved_type.data.@"union".types) |member| {
                if (isCyclicWithVisited(member, visited)) return true;
            }
            return false;
        },
        .intersection => {
            for (resolved_type.data.intersection.types) |member| {
                if (isCyclicWithVisited(member, visited)) return true;
            }
            return false;
        },

        // Type references should be resolved by now
        .type_reference => false,

        // ref/lent: check wrapped type
        .ref => isCyclicWithVisited(resolved_type.data.ref, visited),
        .lent => isCyclicWithVisited(resolved_type.data.lent, visited),
    };
}

/// Check if two types are structurally compatible after following.
/// This is used by type checker for type compatibility checks.
pub fn typesEqual(a: ?*Type, b: ?*Type) bool {
    const ta = follow(a);
    const tb = follow(b);

    if (ta == null and tb == null) return true;
    if (ta == null or tb == null) return false;

    // Same kind required
    if (ta.?.kind != tb.?.kind) return false;

    // For primitives, kind match is enough
    if (ta.?.isPrimitive()) return true;

    // For complex types, compare structure
    return switch (ta.?.kind) {
        .array => typesEqual(ta.?.data.array, tb.?.data.array),
        .function => {
            const fa = ta.?.data.function;
            const fb = tb.?.data.function;
            if (fa.params.len != fb.params.len) return false;
            for (fa.params, fb.params) |pa, pb| {
                if (!typesEqual(pa.type, pb.type)) return false;
            }
            return typesEqual(fa.return_type, fb.return_type);
        },
        .type_reference => {
            // After follow(), type_reference means unresolved (builtins)
            const ra = ta.?.data.type_reference;
            const rb = tb.?.data.type_reference;
            return std.mem.eql(u8, ra.name, rb.name);
        },
        else => true, // TODO: implement for other complex types
    };
}

/// Get a human-readable string for a type (for error messages)
/// Note: Does NOT follow ref/lent wrappers - shows them explicitly
pub fn typeToString(allocator: std.mem.Allocator, t: ?*Type) ![]const u8 {
    if (t == null) return try allocator.dupe(u8, "unknown");
    const typ = t.?;

    return switch (typ.kind) {
        .number => try allocator.dupe(u8, "number"),
        .string => try allocator.dupe(u8, "string"),
        .boolean => try allocator.dupe(u8, "boolean"),
        .void => try allocator.dupe(u8, "void"),
        .unknown => try allocator.dupe(u8, "unknown"),
        .never => try allocator.dupe(u8, "never"),

        // Sized integers
        .int8 => try allocator.dupe(u8, "int8"),
        .int16 => try allocator.dupe(u8, "int16"),
        .int32 => try allocator.dupe(u8, "int32"),
        .int64 => try allocator.dupe(u8, "int64"),
        .uint8 => try allocator.dupe(u8, "uint8"),
        .uint16 => try allocator.dupe(u8, "uint16"),
        .uint32 => try allocator.dupe(u8, "uint32"),
        .uint64 => try allocator.dupe(u8, "uint64"),

        // Sized floats
        .float32 => try allocator.dupe(u8, "float32"),
        .float64 => try allocator.dupe(u8, "float64"),

        // Memory management wrappers
        .ref => blk: {
            const base_str = try typeToString(allocator, typ.data.ref);
            defer allocator.free(base_str);
            break :blk try std.fmt.allocPrint(allocator, "ref {s}", .{base_str});
        },
        .lent => blk: {
            const base_str = try typeToString(allocator, typ.data.lent);
            defer allocator.free(base_str);
            break :blk try std.fmt.allocPrint(allocator, "lent {s}", .{base_str});
        },

        .array => blk: {
            const elem_str = try typeToString(allocator, typ.data.array);
            defer allocator.free(elem_str);
            break :blk try std.fmt.allocPrint(allocator, "{s}[]", .{elem_str});
        },
        .type_reference => try allocator.dupe(u8, typ.data.type_reference.name),
        .function => try allocator.dupe(u8, "function"),
        else => try allocator.dupe(u8, "<complex>"),
    };
}

test "type primitives" {
    const primitive = Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    try std.testing.expect(primitive.isPrimitive());
}

test "follow: primitives return self" {
    var num_type = Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    const result = follow(&num_type);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(TypeKind.number, result.?.kind);
}

test "follow: null returns null" {
    const result = follow(null);
    try std.testing.expect(result == null);
}

test "follow: type reference with resolved follows through" {
    // Create: type UserId = number
    var num_type = Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var ref = TypeReference{
        .name = "UserId",
        .type_args = &[_]*Type{},
        .resolved = &num_type, // Resolved to number
    };

    var ref_type = Type{
        .kind = .type_reference,
        .location = location.SourceLocation.dummy(),
        .data = .{ .type_reference = &ref },
    };

    // follow(UserId) should return number
    const result = follow(&ref_type);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(TypeKind.number, result.?.kind);
}

test "follow: unresolved type reference returns self" {
    var ref = TypeReference{
        .name = "UnknownType",
        .type_args = &[_]*Type{},
        .resolved = null, // Not resolved
    };

    var ref_type = Type{
        .kind = .type_reference,
        .location = location.SourceLocation.dummy(),
        .data = .{ .type_reference = &ref },
    };

    // follow(UnknownType) should return the reference itself
    const result = follow(&ref_type);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(TypeKind.type_reference, result.?.kind);
}

test "typesEqual: same primitives" {
    var a = Type{ .kind = .number, .location = location.SourceLocation.dummy(), .data = .{ .number = {} } };
    var b = Type{ .kind = .number, .location = location.SourceLocation.dummy(), .data = .{ .number = {} } };

    try std.testing.expect(typesEqual(&a, &b));
}

test "typesEqual: different primitives" {
    var a = Type{ .kind = .number, .location = location.SourceLocation.dummy(), .data = .{ .number = {} } };
    var b = Type{ .kind = .string, .location = location.SourceLocation.dummy(), .data = .{ .string = {} } };

    try std.testing.expect(!typesEqual(&a, &b));
}

test "typeToString: primitives" {
    const allocator = std.testing.allocator;

    var num = Type{ .kind = .number, .location = location.SourceLocation.dummy(), .data = .{ .number = {} } };
    const str = try typeToString(allocator, &num);
    defer allocator.free(str);

    try std.testing.expectEqualStrings("number", str);
}

test "ref type: isRefType returns true" {
    var user_type = Type{ .kind = .object, .location = location.SourceLocation.dummy(), .data = .{ .object = undefined } };
    var ref_user = Type{ .kind = .ref, .location = location.SourceLocation.dummy(), .data = .{ .ref = &user_type } };

    try std.testing.expect(ref_user.isRefType());
}

test "lent type: isLentType returns true" {
    var user_type = Type{ .kind = .object, .location = location.SourceLocation.dummy(), .data = .{ .object = undefined } };
    var lent_user = Type{ .kind = .lent, .location = location.SourceLocation.dummy(), .data = .{ .lent = &user_type } };

    try std.testing.expect(lent_user.isLentType());
}

test "follow: ref type unwraps to base type" {
    var user_type = Type{ .kind = .object, .location = location.SourceLocation.dummy(), .data = .{ .object = undefined } };
    var ref_user = Type{ .kind = .ref, .location = location.SourceLocation.dummy(), .data = .{ .ref = &user_type } };

    const result = follow(&ref_user);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(TypeKind.object, result.?.kind);
}

test "follow: lent type unwraps to base type" {
    var user_type = Type{ .kind = .object, .location = location.SourceLocation.dummy(), .data = .{ .object = undefined } };
    var lent_user = Type{ .kind = .lent, .location = location.SourceLocation.dummy(), .data = .{ .lent = &user_type } };

    const result = follow(&lent_user);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(TypeKind.object, result.?.kind);
}

test "typeToString: ref type shows wrapper" {
    const allocator = std.testing.allocator;

    var num = Type{ .kind = .number, .location = location.SourceLocation.dummy(), .data = .{ .number = {} } };
    var ref_num = Type{ .kind = .ref, .location = location.SourceLocation.dummy(), .data = .{ .ref = &num } };

    const str = try typeToString(allocator, &ref_num);
    defer allocator.free(str);

    try std.testing.expectEqualStrings("ref number", str);
}

test "typeToString: lent type shows wrapper" {
    const allocator = std.testing.allocator;

    var num = Type{ .kind = .number, .location = location.SourceLocation.dummy(), .data = .{ .number = {} } };
    var lent_num = Type{ .kind = .lent, .location = location.SourceLocation.dummy(), .data = .{ .lent = &num } };

    const str = try typeToString(allocator, &lent_num);
    defer allocator.free(str);

    try std.testing.expectEqualStrings("lent number", str);
}

test "ownership: default is null" {
    const num = Type{ .kind = .number, .location = location.SourceLocation.dummy(), .data = .{ .number = {} } };
    try std.testing.expect(num.ownership == null);
}

test "ownership: can be set to owned" {
    const num = Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
        .ownership = .owned,
    };
    try std.testing.expectEqual(OwnershipKind.owned, num.ownership.?);
}

// =============================================================================
// Cyclicity Detection Tests
// =============================================================================

test "isCyclic: primitive types are not cyclic" {
    var num = Type{ .kind = .number, .location = location.SourceLocation.dummy(), .data = .{ .number = {} } };
    var str = Type{ .kind = .string, .location = location.SourceLocation.dummy(), .data = .{ .string = {} } };
    var boolean = Type{ .kind = .boolean, .location = location.SourceLocation.dummy(), .data = .{ .boolean = {} } };

    try std.testing.expect(!isCyclic(&num));
    try std.testing.expect(!isCyclic(&str));
    try std.testing.expect(!isCyclic(&boolean));
}

test "isCyclic: null type is not cyclic" {
    try std.testing.expect(!isCyclic(null));
}

test "isCyclic: object with only primitive properties is not cyclic" {
    // class Point { x: number; y: number }
    var num1 = Type{ .kind = .number, .location = location.SourceLocation.dummy(), .data = .{ .number = {} } };
    var num2 = Type{ .kind = .number, .location = location.SourceLocation.dummy(), .data = .{ .number = {} } };

    var props = [_]ObjectType.Property{
        .{ .name = "x", .type = &num1, .optional = false },
        .{ .name = "y", .type = &num2, .optional = false },
    };

    var obj = ObjectType{
        .properties = &props,
        .methods = &[_]ObjectType.Property{},
        .name = "Point",
    };

    var point_type = Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = &obj },
    };

    try std.testing.expect(!isCyclic(&point_type));
}

test "isCyclic: self-referential type is cyclic" {
    // class Node { next: Node }
    var obj = ObjectType{
        .properties = undefined, // Will be set below
        .methods = &[_]ObjectType.Property{},
        .name = "Node",
    };

    var node_type = Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = &obj },
    };

    var props = [_]ObjectType.Property{
        .{ .name = "next", .type = &node_type, .optional = false },
    };
    obj.properties = &props;

    try std.testing.expect(isCyclic(&node_type));
}

test "isCyclic: array of primitives is not cyclic" {
    // number[]
    var num = Type{ .kind = .number, .location = location.SourceLocation.dummy(), .data = .{ .number = {} } };
    var arr = Type{
        .kind = .array,
        .location = location.SourceLocation.dummy(),
        .data = .{ .array = &num },
    };

    try std.testing.expect(!isCyclic(&arr));
}

test "isCyclic: ref wrapper is checked for inner type" {
    // ref number (not cyclic)
    var num = Type{ .kind = .number, .location = location.SourceLocation.dummy(), .data = .{ .number = {} } };
    var ref_num = Type{
        .kind = .ref,
        .location = location.SourceLocation.dummy(),
        .data = .{ .ref = &num },
    };

    try std.testing.expect(!isCyclic(&ref_num));
}
