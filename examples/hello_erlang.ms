// Test variable shadowing in Erlang backend
function greet(name: string): string {
  const message = "Hello";
  message = message + " " + name;
  message = message + "!";
  return message;
}

function counter(): number {
  const x = 0;
  x = x + 1;
  x = x + 1;
  x = x + 1;
  return x;
}

function testScopes(): number {
  const y = 10;
  if (true) {
    const z = y + 5;
    z = z * 2;
  }
  y = y + 100;
  return y;
}
