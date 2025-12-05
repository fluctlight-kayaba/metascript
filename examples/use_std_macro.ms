// Test using macros from std/macros/derive.ms

// Use @deriveEq macro defined in std/macros/derive.ms
@deriveEq
class User {
    id: number;
    name: string;
}

// Use multiple std macros
@deriveHash
class Product {
    sku: string;
    price: number;
}
