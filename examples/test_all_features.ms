// Comprehensive test of all implemented Erlang backend features

// Test 1: Variable Shadowing
function testShadowing(): number {
  console.log("=== Test 1: Variable Shadowing ===");
  const x = 10;
  console.log("x =", x);

  x = x + 5;
  console.log("x =", x);

  x = x * 2;
  console.log("x =", x);

  return x;
}

// Test 2: While Loop
function testWhile(): number {
  console.log("=== Test 2: While Loop ===");
  const count = 5;

  while (count > 0) {
    console.log("count =", count);
    count = count - 1;
  }

  console.log("While loop complete");
  return 0;
}

// Test 3: For Loop
function testFor(): number {
  console.log("=== Test 3: For Loop ===");

  for (const i = 0; i < 5; i = i + 1) {
    console.log("i =", i);
  }

  console.log("For loop complete");
  return 0;
}

// Test 4: Object Member Access
function testMemberAccess(): number {
  console.log("=== Test 4: Member Access ===");

  const person = {
    name: "Alice",
    age: 30,
    city: "Portland"
  };

  const userName = person.name;
  const userAge = person.age;
  const userCity = person.city;

  console.log("Name:", userName);
  console.log("Age:", userAge);
  console.log("City:", userCity);

  return userAge;
}

// Test 5: Array Methods
function testArrayMethods(): number {
  console.log("=== Test 5: Array Methods ===");

  const numbers = [1, 2, 3, 4, 5];
  console.log("Original:", numbers);

  const len = numbers.length;
  console.log("Length:", len);

  const extended = numbers.push(6);
  console.log("After push(6):", extended);

  const extendedMore = extended.push(7);
  console.log("After push(7):", extendedMore);

  return len;
}

// Test 6: Multiple Variables
function testMultipleVars(): number {
  console.log("=== Test 6: Multiple Variables ===");

  const a = 1;
  const b = 2;
  const c = 3;

  console.log("Initial: a =", a, "b =", b, "c =", c);

  a = a + 10;
  b = b + 20;
  c = c + 30;

  console.log("Updated: a =", a, "b =", b, "c =", c);

  const sum = a + b + c;
  return sum;
}

// Test 7: Nested Scopes
function testNestedScopes(): number {
  console.log("=== Test 7: Nested Scopes ===");

  const outer = 100;
  console.log("Outer:", outer);

  const inner = 10;
  console.log("Inner:", inner);

  const result = outer + inner;
  console.log("Result:", result);

  return result;
}

// Test 8: Arithmetic
function testArithmetic(): number {
  console.log("=== Test 8: Arithmetic ===");

  const a = 10;
  const b = 3;

  const sum = a + b;
  const diff = a - b;
  const prod = a * b;
  const quot = a / b;

  console.log("10 + 3 =", sum);
  console.log("10 - 3 =", diff);
  console.log("10 * 3 =", prod);
  console.log("10 / 3 =", quot);

  return sum;
}

// Main test runner
function main(): number {
  console.log("================================");
  console.log("  MetaScript Erlang Backend");
  console.log("  Feature Test Suite");
  console.log("================================");
  console.log("");

  const r1 = testShadowing();
  console.log("Result:", r1);
  console.log("");

  const r2 = testWhile();
  console.log("Result:", r2);
  console.log("");

  const r3 = testFor();
  console.log("Result:", r3);
  console.log("");

  const r4 = testMemberAccess();
  console.log("Result:", r4);
  console.log("");

  const r5 = testArrayMethods();
  console.log("Result:", r5);
  console.log("");

  const r6 = testMultipleVars();
  console.log("Result:", r6);
  console.log("");

  const r7 = testNestedScopes();
  console.log("Result:", r7);
  console.log("");

  const r8 = testArithmetic();
  console.log("Result:", r8);
  console.log("");

  console.log("================================");
  console.log("  All Tests Complete!");
  console.log("================================");

  return 0;
}
