// Multiple spreads test - merge multiple objects

const obj1 = { a: 1, b: 2 };
const obj2 = { c: 3, d: 4 };
const obj3 = { e: 5 };

const merged = { ...obj1, ...obj2, ...obj3 };

console.log(merged);  // Should have a, b, c, d, e
