// Comprehensive Demo: MetaScript → Erlang Backend
// Demonstrates: variable shadowing, functions, console.log, control flow

// ============================================================================
// Part 1: Simple Functions with Logging
// ============================================================================

function simpleAdd(a: number, b: number): number {
  console.log("=== simpleAdd called ===");
  console.log("Input a:", a);
  console.log("Input b:", b);

  const result = a + b;
  console.log("Result:", result);

  return result;
}

function simpleMultiply(x: number, y: number): number {
  console.log("=== simpleMultiply called ===");
  console.log("Multiplying", x, "by", y);

  const product = x * y;
  console.log("Product:", product);

  return product;
}

// ============================================================================
// Part 2: Variable Shadowing Demo
// ============================================================================

function shadowingDemo(): number {
  console.log("=== Variable Shadowing Demo ===");

  let counter = 0;
  console.log("Initial counter:", counter);

  counter = counter + 10;
  console.log("After +10:", counter);

  counter = counter * 2;
  console.log("After *2:", counter);

  counter = counter - 5;
  console.log("After -5:", counter);

  console.log("Final result:", counter);
  return counter;
}

// ============================================================================
// Part 3: Conditional Logic
// ============================================================================

function isPositive(n: number): boolean {
  console.log("=== Checking if positive ===");

  if (n > 0) {
    console.log("Result: TRUE");
    return true;
  }

  console.log("Result: FALSE");
  return false;
}

function absoluteValue(n: number): number {
  console.log("=== Computing absolute value ===");

  if (n < 0) {
    console.log("Number is negative");
    return -n;
  }

  console.log("Number is non-negative");
  return n;
}

// ============================================================================
// Part 4: Nested Scopes
// ============================================================================

function nestedScopes(x: number): number {
  console.log("=== Nested Scopes Demo ===");

  const outer = x;
  console.log("Outer:", outer);

  if (outer > 0) {
    console.log("Positive branch");
    const inner = outer * 2;
    console.log("Inner:", inner);

    inner = inner + 5;
    console.log("Modified inner:", inner);
  }

  outer = outer + 100;
  console.log("Modified outer:", outer);

  return outer;
}

// ============================================================================
// Part 5: Factorial (Recursive)
// ============================================================================

function factorial(n: number): number {
  console.log("factorial called with:", n);

  if (n <= 1) {
    console.log("Base case reached");
    return 1;
  }

  const result = n * factorial(n - 1);
  console.log("factorial result:", result);
  return result;
}

// ============================================================================
// Part 6: Main Demo Runner
// ============================================================================

function main(): number {
  console.log("=== MetaScript Erlang Demo ===");

  const sum = simpleAdd(5, 3);
  console.log("Sum:", sum);

  const product = simpleMultiply(4, 7);
  console.log("Product:", product);

  const shadowed = shadowingDemo();
  console.log("Shadowed:", shadowed);

  const isPos = isPositive(42);
  console.log("Is positive:", isPos);

  const abs1 = absoluteValue(-15);
  console.log("Absolute:", abs1);

  const nested = nestedScopes(10);
  console.log("Nested result:", nested);

  const fact5 = factorial(5);
  console.log("Factorial:", fact5);

  console.log("=== All Tests Complete ===");
  return 0;
}
