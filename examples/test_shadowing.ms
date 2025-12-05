// Test variable shadowing compilation to Erlang
function testShadowing() {
  const x = 10;
  x = x + 1;
  x = x * 2;
  return x;
}

function testScopes() {
  const y = 5;
  if (y > 0) {
    const z = y + 1;
    z = z * 2;
  }
  y = y + 10;
  return y;
}
