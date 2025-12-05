// Test file for new Nim-style macro syntax
// Syntax: macro name(ctx) { ... } (no @ when defining)
// Usage: @macroName(...) (always @ when using)

// Define a simple macro (Nim-style, no @ prefix)
macro derive(ctx: MacroContext) {
    // Macro body - generates code at compile-time
    return ctx;
}

// Use the macro (always @ prefix)
@derive(Eq)
class User {
    name: string;
    age: number;
}

// The 'quote' keyword is also available for AST quotation
// (full implementation pending)
// Example: quote { const x = 42; }

