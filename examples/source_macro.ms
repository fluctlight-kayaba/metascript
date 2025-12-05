// Source-defined macro example
// This file demonstrates Nim-style source-defined macros

// Define a simple macro that adds a toString method
@macro function addToString(target) {
    // Log what we're doing
    console.log("addToString macro called for:", target.name);

    // Create a simple method body
    const body = ast.createBlock([]);

    // Create the method
    const method = ast.createMethod("toString", body);

    // Add to class
    target.addMethod(method);

    return target;
}

// Use the source-defined macro
@addToString
class User {
    id: number;
    name: string;
}

// Can also use built-in macros
@derive(Eq, Hash)
class Product {
    sku: string;
    price: number;
}
