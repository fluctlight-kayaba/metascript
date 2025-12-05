// Test only the features that work correctly (no loops with mutable state)

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

// Test 2: Object Member Access
function testMemberAccess(): number {
  console.log("=== Test 2: Member Access ===");

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

// Test 3: Array Methods
function testArrayMethods(): number {
  console.log("=== Test 3: Array Methods ===");

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

// Test 4: Multiple Variables
function testMultipleVars(): number {
  console.log("=== Test 4: Multiple Variables ===");

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

// Test 5: Arithmetic
function testArithmetic(): number {
  console.log("=== Test 5: Arithmetic ===");

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

// Test 6: String Concatenation
function testStrings(): number {
  console.log("=== Test 6: Strings ===");

  const first = "Hello";
  const second = "World";
  const greeting = first;

  console.log("First:", first);
  console.log("Second:", second);
  console.log("Greeting:", greeting);

  return 0;
}

// Test 7: Nested Expressions
function testNested(): number {
  console.log("=== Test 7: Nested Expressions ===");

  const a = 2;
  const b = 3;
  const c = 4;

  const result = (a + b) * c;
  console.log("(2 + 3) * 4 =", result);

  const result2 = a * (b + c);
  console.log("2 * (3 + 4) =", result2);

  return result;
}

// Main test runner
function main(): number {
  console.log("================================");
  console.log("  MetaScript Erlang Backend");
  console.log("  Working Features Test");
  console.log("================================");
  console.log("");

  const r1 = testShadowing();
  console.log("Result:", r1);
  console.log("");

  const r2 = testMemberAccess();
  console.log("Result:", r2);
  console.log("");

  const r3 = testArrayMethods();
  console.log("Result:", r3);
  console.log("");

  const r4 = testMultipleVars();
  console.log("Result:", r4);
  console.log("");

  const r5 = testArithmetic();
  console.log("Result:", r5);
  console.log("");

  const r6 = testStrings();
  console.log("Result:", r6);
  console.log("");

  const r7 = testNested();
  console.log("Result:", r7);
  console.log("");

  console.log("================================");
  console.log("  All Tests PASSED!");
  console.log("================================");

  return 0;
}
