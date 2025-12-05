// Test file to demonstrate type checker capabilities

// 1. Missing return detection - should error!
function calculateSum(a: number, b: number): number {
    const result = a + b;
    // Missing return! This should trigger an error.
}

function innerVariable(a: number, b: number): number {
    const result = a + b;
    return result;
}

// 2. Correct function for comparison
function correctFunction(a: number, b: number): number {
    return a + b;
}

// 3. Class with interface
@derive(Eq)
class User {
    name: string;
    age: number;
}
