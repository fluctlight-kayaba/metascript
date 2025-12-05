// Property override test - later properties should override earlier ones

const base = { x: 1, y: 2 };
const overridden = { ...base, x: 99 };  // x should be 99, not 1

console.log(overridden.x);  // Should print 99
