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

    // Check parameter types (CONTRAVARIANT)
    // For a function assignment: target = source
    // Source function must accept AT LEAST what target function accepts.
    // This means source's parameter type must be a SUPERTYPE of target's parameter type.
    //
    // Example: target: (Animal) => void, source: (Dog) => void
    // - Source only handles Dogs, but target may receive any Animal
    // - This should FAIL because Dog <: Animal, not Animal <: Dog
    //
    // Correct check: source.params[i].type must accept target.params[i].type
    // Which means: typesCompatible(source.params[i].type, target.params[i].type)
    const min_params = @min(target.params.len, source.params.len);
    for (0..min_params) |i| {
        // Contravariant: source param must accept target param (source is supertype)
        if (!typesCompatible(source.params[i].type, target.params[i].type)) {
            return false;
        }
    }

    // Check return type (covariant)
    return typesCompatible(target.return_type, source.return_type);
}

// ============================================================================
// Tests
// ============================================================================

test "functionTypesCompatible: contravariant parameters with incompatible types" {
    // Test parameter contravariance with clearly incompatible types (number vs string)
    // Function (number) => void is NOT compatible with (string) => void
    // Because a function expecting a number can't handle a string, and vice versa.
    //
    // Contravariance: source param type must be compatible with target param type
    // in the correct direction (source can accept what target accepts).

    const testing = std.testing;
    const location = @import("../ast/location.zig");
    const dummy_loc = location.SourceLocation.dummy();

    // Create number type
    var number_type = types.Type{
        .kind = .number,
        .location = dummy_loc,
        .data = .{ .number = {} },
    };

    // Create string type
    var string_type = types.Type{
        .kind = .string,
        .location = dummy_loc,
        .data = .{ .string = {} },
    };

    // Create void type for return
    var void_type = types.Type{
        .kind = .void,
        .location = dummy_loc,
        .data = .{ .void = {} },
    };

    // Create function type: (number) => void
    const number_param = types.FunctionType.FunctionParam{
        .name = "n",
        .type = &number_type,
        .optional = false,
    };
    var fn_takes_number = types.FunctionType{
        .params = @constCast(&[_]types.FunctionType.FunctionParam{number_param}),
        .return_type = &void_type,
        .type_params = &[_]types.GenericParam{},
    };

    // Create function type: (string) => void
    const string_param = types.FunctionType.FunctionParam{
        .name = "s",
        .type = &string_type,
        .optional = false,
    };
    var fn_takes_string = types.FunctionType{
        .params = @constCast(&[_]types.FunctionType.FunctionParam{string_param}),
        .return_type = &void_type,
        .type_params = &[_]types.GenericParam{},
    };

    // (number) => void should NOT be compatible with (string) => void
    // Because string is not compatible with number
    try testing.expect(!functionTypesCompatible(&fn_takes_number, &fn_takes_string));
    try testing.expect(!functionTypesCompatible(&fn_takes_string, &fn_takes_number));
}

test "functionTypesCompatible: contravariant parameters with structural subtyping" {
    // Test TRUE contravariance with structural subtyping
    //
    // Given: Dog <: Animal (Dog is a structural subtype of Animal - Dog has all Animal's properties)
    //
    // Function assignment: let f: (Animal) => void = g
    // - If g: (Animal) => void → YES (same type)
    // - If g: (Dog) => void → NO! (g only handles Dogs, can't handle all Animals)
    //
    // CONTRAVARIANCE: Source function's parameter must be a SUPERTYPE of target's parameter
    // So source param must ACCEPT what target param accepts.
    //
    // This is counterintuitive but correct:
    // - Target says "I need a function that handles Animals"
    // - A function that handles "any Object" (supertype) is fine
    // - A function that only handles "Dogs" (subtype) is NOT fine

    const testing = std.testing;
    const location = @import("../ast/location.zig");
    const dummy_loc = location.SourceLocation.dummy();

    // Create Animal type with property "name: string"
    var name_prop_type = types.Type{
        .kind = .string,
        .location = dummy_loc,
        .data = .{ .string = {} },
    };
    const animal_name_prop = types.ObjectType.Property{
        .name = "name",
        .type = &name_prop_type,
        .optional = false,
    };
    var animal_obj = types.ObjectType{
        .name = "Animal",
        .properties = @constCast(&[_]types.ObjectType.Property{animal_name_prop}),
        .methods = &[_]types.ObjectType.Property{},
    };
    var animal_type = types.Type{
        .kind = .object,
        .location = dummy_loc,
        .data = .{ .object = &animal_obj },
    };

    // Create Dog type with properties "name: string" AND "breed: string" (Dog <: Animal)
    const dog_name_prop = types.ObjectType.Property{
        .name = "name",
        .type = &name_prop_type,
        .optional = false,
    };
    var breed_prop_type = types.Type{
        .kind = .string,
        .location = dummy_loc,
        .data = .{ .string = {} },
    };
    const dog_breed_prop = types.ObjectType.Property{
        .name = "breed",
        .type = &breed_prop_type,
        .optional = false,
    };
    var dog_obj = types.ObjectType{
        .name = "Dog",
        .properties = @constCast(&[_]types.ObjectType.Property{ dog_name_prop, dog_breed_prop }),
        .methods = &[_]types.ObjectType.Property{},
    };
    var dog_type = types.Type{
        .kind = .object,
        .location = dummy_loc,
        .data = .{ .object = &dog_obj },
    };

    // Verify structural subtyping: Dog <: Animal (Dog has all Animal's properties)
    try testing.expect(typesCompatible(&animal_type, &dog_type)); // Dog assignable to Animal
    try testing.expect(!typesCompatible(&dog_type, &animal_type)); // Animal NOT assignable to Dog

    // Create void type for return
    var void_type = types.Type{
        .kind = .void,
        .location = dummy_loc,
        .data = .{ .void = {} },
    };

    // Create function type: (Animal) => void
    const animal_param = types.FunctionType.FunctionParam{
        .name = "a",
        .type = &animal_type,
        .optional = false,
    };
    var fn_takes_animal = types.FunctionType{
        .params = @constCast(&[_]types.FunctionType.FunctionParam{animal_param}),
        .return_type = &void_type,
        .type_params = &[_]types.GenericParam{},
    };

    // Create function type: (Dog) => void
    const dog_param = types.FunctionType.FunctionParam{
        .name = "d",
        .type = &dog_type,
        .optional = false,
    };
    var fn_takes_dog = types.FunctionType{
        .params = @constCast(&[_]types.FunctionType.FunctionParam{dog_param}),
        .return_type = &void_type,
        .type_params = &[_]types.GenericParam{},
    };

    // CONTRAVARIANCE TEST:
    // Can we assign (Dog) => void to (Animal) => void?
    // Target expects function that handles any Animal
    // Source only handles Dogs - SHOULD FAIL (violates Liskov Substitution)
    //
    // CURRENT BUG: Symmetrical check passes because Dog <: Animal
    // CORRECT: Should fail because Dog (source param) doesn't accept Animal (target param)
    try testing.expect(!functionTypesCompatible(&fn_takes_animal, &fn_takes_dog));

    // Can we assign (Animal) => void to (Dog) => void?
    // Target expects function that handles Dogs
    // Source handles all Animals (including Dogs) - SHOULD PASS
    try testing.expect(functionTypesCompatible(&fn_takes_dog, &fn_takes_animal));
}

test "functionTypesCompatible: covariant return types" {
    // Return types are covariant: if target returns Animal, source can return Dog (subtype)
    // But if target returns Dog, source cannot return Animal (supertype)

    const testing = std.testing;
    const location = @import("../ast/location.zig");
    const dummy_loc = location.SourceLocation.dummy();

    // Create number and string types for testing
    var number_type = types.Type{
        .kind = .number,
        .location = dummy_loc,
        .data = .{ .number = {} },
    };
    var string_type = types.Type{
        .kind = .string,
        .location = dummy_loc,
        .data = .{ .string = {} },
    };

    // Create function type: () => number
    var fn_returns_number = types.FunctionType{
        .params = &[_]types.FunctionType.FunctionParam{},
        .return_type = &number_type,
        .type_params = &[_]types.GenericParam{},
    };

    // Create function type: () => string
    var fn_returns_string = types.FunctionType{
        .params = &[_]types.FunctionType.FunctionParam{},
        .return_type = &string_type,
        .type_params = &[_]types.GenericParam{},
    };

    // number and string are incompatible, so neither function should be assignable to the other
    try testing.expect(!functionTypesCompatible(&fn_returns_number, &fn_returns_string));
    try testing.expect(!functionTypesCompatible(&fn_returns_string, &fn_returns_number));
}

test "functionTypesCompatible: same types are compatible" {
    const testing = std.testing;
    const location = @import("../ast/location.zig");
    const dummy_loc = location.SourceLocation.dummy();

    var number_type = types.Type{
        .kind = .number,
        .location = dummy_loc,
        .data = .{ .number = {} },
    };

    const number_param = types.FunctionType.FunctionParam{
        .name = "n",
        .type = &number_type,
        .optional = false,
    };

    var fn_type = types.FunctionType{
        .params = @constCast(&[_]types.FunctionType.FunctionParam{number_param}),
        .return_type = &number_type,
        .type_params = &[_]types.GenericParam{},
    };

    // Same function type should be compatible with itself
    try testing.expect(functionTypesCompatible(&fn_type, &fn_type));
}

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
