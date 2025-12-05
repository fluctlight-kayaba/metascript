// Test method calls on arrays

function testArrayMethods(): number {
  console.log("=== Testing Array Methods ===");

  const numbers = [1, 2, 3, 4, 5];
  console.log("Original array:", numbers);

  const len = numbers.length;
  console.log("Array length:", len);

  const doubled = [2, 4, 6, 8, 10];
  console.log("Doubled array:", doubled);

  const withSix = numbers.push(6);
  console.log("After push(6):", withSix);

  return len;
}

function main(): number {
  console.log("================================");
  console.log("  Method Calls Test");
  console.log("================================");

  const result = testArrayMethods();
  console.log("Final result:", result);

  return 0;
}
