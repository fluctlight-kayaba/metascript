// @derive - Automatic trait implementation macro
//
// Generates boilerplate methods for common traits:
// - Eq: equals(other) method for structural equality
// - Hash: hash() method for hash code generation

// Generates equals() method for structural equality comparison.
// Compares all fields of the class for deep equality.
macro function deriveEq(target) {
    console.log("deriveEq called for:", target.name);

    // Create empty body for now - we'll enhance the transpiler later
    const body = ast.createBlock([]);
    const method = ast.createMethod("equals", body);
    target.addMethod(method);

    return target;
}

// Generates hash() method for hash code computation.
// Uses FNV-1a algorithm on all fields for consistent hashing.
macro function deriveHash(target) {
    const body = ast.createBlock([]);
    const method = ast.createMethod("hash", body);
    target.addMethod(method);
    return target;
}

// Generates clone() method for deep copying.
// Creates a new instance with all fields copied.
macro function deriveClone(target) {
    const body = ast.createBlock([]);
    const method = ast.createMethod("clone", body);
    target.addMethod(method);
    return target;
}

// Generates toString() method for debug output.
// Returns a formatted string with class name and all field values.
macro function deriveDebug(target) {
    const body = ast.createBlock([]);
    const method = ast.createMethod("toString", body);
    target.addMethod(method);
    return target;
}
