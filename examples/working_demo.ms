// Working Demo: What's Working in Erlang Backend

function testAdd(): number {
  console.log("=== Testing Addition ===");
  const a = 5;
  const b = 3;
  const sum = a + b;
  console.log("5 + 3 =", sum);
  return sum;
}

function testShadowing(): number {
  console.log("=== Testing Variable Shadowing ===");
  const x = 10;
  console.log("Initial x:", x);

  x = x + 5;
  console.log("After +5:", x);

  x = x * 2;
  console.log("After *2:", x);

  x = x - 10;
  console.log("After -10:", x);

  console.log("Final x:", x);
  return x;
}

function testMultipleVars(): number {
  console.log("=== Testing Multiple Variables ===");
  const a = 1;
  const b = 2;
  const c = 3;

  console.log("a:", a, "b:", b, "c:", c);

  a = a + 10;
  b = b + 20;
  c = c + 30;

  console.log("a:", a, "b:", b, "c:", c);

  const result = a + b + c;
  console.log("Sum:", result);
  return result;
}

function main(): number {
  console.log("================================");
  console.log("  MetaScript → Erlang Demo");
  console.log("  Variable Shadowing Showcase");
  console.log("================================");

  const sum = testAdd();
  console.log("");

  const shadowed = testShadowing();
  console.log("");

  const multi = testMultipleVars();
  console.log("");

  console.log("================================");
  console.log("  All Tests Complete!");
  console.log("================================");
  console.log("Results: sum=", sum, "shadowed=", shadowed, "multi=", multi);

  return 0;
}
