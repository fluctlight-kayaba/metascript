/// Type Compatibility Checking
///
/// This module handles all type compatibility logic for the type checker.
/// Extracted from typechecker.zig to improve modularity.
///
/// Key responsibilities:
/// - Primitive type checking (numeric, string, boolean)
/// - Structural type compatibility (objects, functions)
/// - Union type compatibility
/// - Type reference resolution for compatibility

const std = @import("std");
const types = @import("../ast/types.zig");

// ============================================================================
// Primitive Type Helpers
// ============================================================================

/// Check if a TypeKind is a numeric type
pub fn isNumericType(kind: types.TypeKind) bool {
    return switch (kind) {
        .number, .int8, .int16, .int32, .int64, .uint8, .uint16, .uint32, .uint64, .float32, .float64 => true,
        else => false,
    };
}

/// Check if a type is numeric, including type_reference to numeric names
pub fn isNumericTypeResolved(t: *types.Type) bool {
    if (isNumericType(t.kind)) return true;
    // Handle type_reference to built-in numeric types
    if (t.kind == .type_reference) {
        const name = t.data.type_reference.name;
        return std.mem.eql(u8, name, "number") or
            std.mem.eql(u8, name, "int8") or
            std.mem.eql(u8, name, "int16") or
            std.mem.eql(u8, name, "int32") or
            std.mem.eql(u8, name, "int64") or
            std.mem.eql(u8, name, "uint8") or
            std.mem.eql(u8, name, "uint16") or
            std.mem.eql(u8, name, "uint32") or
            std.mem.eql(u8, name, "uint64") or
            std.mem.eql(u8, name, "float32") or
            std.mem.eql(u8, name, "float64");
    }
    return false;
}

/// Check if a type is string, including type_reference
pub fn isStringType(t: *types.Type) bool {
    if (t.kind == .string) return true;
    if (t.kind == .type_reference) {
        return std.mem.eql(u8, t.data.type_reference.name, "string");
    }
    return false;
}

/// Check if a type is boolean, including type_reference
pub fn isBooleanType(t: *types.Type) bool {
    if (t.kind == .boolean) return true;
    if (t.kind == .type_reference) {
        return std.mem.eql(u8, t.data.type_reference.name, "boolean");
    }
    return false;
}

/// Check if a type kind is an integer type (for bitwise operations)
/// Note: .number is NOT included because it's an alias for float64
pub fn isIntegerType(kind: types.TypeKind) bool {
    return switch (kind) {
        .int8, .int16, .int32, .int64, .uint8, .uint16, .uint32, .uint64 => true,
        else => false,
    };
}

// ============================================================================
// Type Compatibility Checking
// ============================================================================

/// Check if two types are compatible
/// Uses structural typing for objects and functions
pub fn typesCompatible(target: *types.Type, source: *types.Type) bool {
    // Handle type_reference - resolve to builtin type name comparison
    if (target.kind == .type_reference and source.kind == .type_reference) {
        // Both are type references - compare names
        return std.mem.eql(u8, target.data.type_reference.name, source.data.type_reference.name);
    }

    // type_reference to a builtin primitive or class type
    if (target.kind == .type_reference) {
        const target_name = target.data.type_reference.name;
        // Check if source matches the referenced type
        if (std.mem.eql(u8, target_name, "number") and isNumericTypeResolved(source)) return true;
        if (std.mem.eql(u8, target_name, "string") and isStringType(source)) return true;
        if (std.mem.eql(u8, target_name, "boolean") and isBooleanType(source)) return true;
        // Check if source is an object type with matching class name
        if (source.kind == .object) {
            if (source.data.object.name) |source_class_name| {
                if (std.mem.eql(u8, target_name, source_class_name)) return true;
            }
        }
    }
    if (source.kind == .type_reference) {
        const source_name = source.data.type_reference.name;
        // Check if target matches the referenced type
        if (std.mem.eql(u8, source_name, "number") and isNumericTypeResolved(target)) return true;
        if (std.mem.eql(u8, source_name, "string") and isStringType(target)) return true;
        if (std.mem.eql(u8, source_name, "boolean") and isBooleanType(target)) return true;
        // Check if target is an object type with matching class name
        if (target.kind == .object) {
            if (target.data.object.name) |target_class_name| {
                if (std.mem.eql(u8, source_name, target_class_name)) return true;
            }
        }
    }

    // Same kind is often compatible, but need deeper checks for complex types
    if (target.kind == source.kind) {
        // For objects, check structural compatibility
        if (target.kind == .object) {
            return objectTypesCompatible(target.data.object, source.data.object);
        }
        // For arrays, check element type compatibility
        if (target.kind == .array) {
            return typesCompatible(target.data.array, source.data.array);
        }
        // For functions, check signature compatibility
        if (target.kind == .function) {
            return functionTypesCompatible(target.data.function, source.data.function);
        }
        // Primitives with same kind are compatible
        return true;
    }

    // unknown is compatible with anything
    if (target.kind == .unknown or source.kind == .unknown) return true;

    // never is compatible with anything (bottom type)
    if (target.kind == .never or source.kind == .never) return true;

    // number and sized integer types are compatible
    if (isNumericTypeResolved(target) and isNumericTypeResolved(source)) {
        return true;
    }

    // Union type handling
    // Source is compatible with target union if source is compatible with any member
    if (target.kind == .@"union") {
        const union_types = target.data.@"union".types;
        for (union_types) |member| {
            if (typesCompatible(member, source)) {
                return true;
            }
        }
        return false;
    }

    // Source union is compatible with target if all members are compatible
    if (source.kind == .@"union") {
        const union_types = source.data.@"union".types;
        for (union_types) |member| {
            if (!typesCompatible(target, member)) {
                return false;
            }
        }
        return true;
    }

    return false;
}

/// Check if an object type is compatible with another (structural typing)
/// Source must have all required properties of target with compatible types
pub fn objectTypesCompatible(target: *types.ObjectType, source: *types.ObjectType) bool {
    // Fast path: if both have the same class name, they're compatible (nominal typing for classes)
    if (target.name != null and source.name != null) {
        if (std.mem.eql(u8, target.name.?, source.name.?)) {
            return true;
        }
    }

    // Fall back to structural typing for anonymous objects or different class names
    // For each property in target, source must have a compatible property
    for (target.properties) |target_prop| {
        var found = false;
        for (source.properties) |source_prop| {
            if (std.mem.eql(u8, target_prop.name, source_prop.name)) {
                // Found matching property, check type compatibility
                if (!typesCompatible(target_prop.type, source_prop.type)) {
                    return false; // Property types don't match
                }
                found = true;
                break;
            }
        }
        // If property not found and not optional, types are incompatible
        if (!found and !target_prop.optional) {
            return false;
        }
    }

    // For each method in target, source must have a compatible method
    for (target.methods) |target_method| {
        var found = false;
        for (source.methods) |source_method| {
            if (std.mem.eql(u8, target_method.name, source_method.name)) {
                if (!typesCompatible(target_method.type, source_method.type)) {
                    return false;
                }
                found = true;
                break;
            }
        }
        if (!found and !target_method.optional) {
            return false;
        }
    }

    return true;
}

/// Check if two function types are compatible
/// Parameter types are contravariant, return type is covariant
pub fn functionTypesCompatible(target: *types.FunctionType, source: *types.FunctionType) bool {
    // Check parameter count (source can have fewer if target has optional params)
    if (source.params.len < target.params.len) {
        // Check if extra target params are all optional
        for (target.params[source.params.len..]) |param| {
            if (!param.optional) return false;
        }
    }

    // Check parameter types (contravariant)
    const min_params = @min(target.params.len, source.params.len);
    for (0..min_params) |i| {
        // For contravariance: source param should accept what target param accepts
        // Simplified: just check for compatibility in either direction
        if (!typesCompatible(target.params[i].type, source.params[i].type) and
            !typesCompatible(source.params[i].type, target.params[i].type))
        {
            return false;
        }
    }

    // Check return type (covariant)
    return typesCompatible(target.return_type, source.return_type);
}

// ============================================================================
// Tests
// ============================================================================

test "isNumericType: primitive numeric types" {
    const testing = std.testing;
    try testing.expect(isNumericType(.number));
    try testing.expect(isNumericType(.int32));
    try testing.expect(isNumericType(.float64));
    try testing.expect(!isNumericType(.string));
    try testing.expect(!isNumericType(.boolean));
}

test "isIntegerType: excludes number (which is float64)" {
    const testing = std.testing;
    try testing.expect(isIntegerType(.int32));
    try testing.expect(isIntegerType(.uint64));
    try testing.expect(!isIntegerType(.number));
    try testing.expect(!isIntegerType(.float64));
}
