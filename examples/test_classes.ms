// Test class support with tagged tuples

class Point {
  x: number;
  y: number;

  distance(): number {
    return this.x * this.x + this.y * this.y;
  }
}

function testClasses(): number {
  console.log("=== Testing Classes ===");

  const p1 = new Point(3, 4);
  console.log("Created point p1");

  const p2 = new Point(5, 12);
  console.log("Created point p2");

  console.log("Points created successfully");
  return 0;
}

function main(): number {
  console.log("================================");
  console.log("  Class Support Test");
  console.log("================================");

  const result = testClasses();
  console.log("Test complete");

  return result;
}
