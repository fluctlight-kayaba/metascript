// Test member access with object literals

function testObjectAccess(): number {
  console.log("=== Testing Object Member Access ===");

  const person = { name: "Alice", age: 30 };
  const userName = person.name;
  const userAge = person.age;

  console.log("Person name:", userName);
  console.log("Person age:", userAge);

  return userAge;
}

function main(): number {
  console.log("================================");
  console.log("  Member Access Test");
  console.log("================================");

  const age = testObjectAccess();
  console.log("Final result:", age);

  return 0;
}
