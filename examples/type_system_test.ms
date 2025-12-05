// Comprehensive Type System Test
// Tests: variadic functions, const enforcement, type checking, error messages

// ============================================================================
// 1. Variadic Functions (console.log)
// ============================================================================
function testVariadic(): void {
  console.log("Single arg");
  console.log("Two", "args");
  console.log("Three", "args", "here");
  console.log("With number:", 42);
  console.log("Mix:", "hello", 123, true);
}

// ============================================================================
// 2. Proper Const vs Let
// ============================================================================
function testMutability(): number {
  // Let is mutable
  let counter = 0;
  counter = counter + 10;
  counter = counter * 2;

  // Const is immutable (compile-time enforced)
  const multiplier = 5;
  // multiplier = 10;  // This would error: cannot assign to const variable 'multiplier'

  return counter * multiplier;
}

// ============================================================================
// 3. Type Checking
// ============================================================================
function add(a: number, b: number): number {
  return a + b;
}

function greet(name: string): string {
  return "Hello, " + name;
}

function isPositive(n: number): boolean {
  if (n > 0) {
    return true;
  }
  return false;
}

// ============================================================================
// 4. Control Flow
// ============================================================================
function fib(n: number): number {
  if (n <= 1) {
    return n;
  }
  return fib(n - 1) + fib(n - 2);
}

function factorial(n: number): number {
  let result = 1;
  let i = 2;
  while (i <= n) {
    result = result * i;
    i = i + 1;
  }
  return result;
}

// ============================================================================
// 5. Main test execution
// ============================================================================
testVariadic();

const sum = add(10, 20);
console.log("Sum:", sum);

const greeting = greet("World");
console.log(greeting);

const fib5 = fib(5);
console.log("Fib(5):", fib5);

const fact5 = factorial(5);
console.log("Factorial(5):", fact5);

console.log("Mutability test:", testMutability());
