// Spread with explicit types - should enable normalization

type Point = {
  x: number;
  y: number;
};

const base: Point = { x: 1, y: 2 };
const extended = { ...base, z: 3 };

// Expected normalization:
// const extended = { x: base.x, y: base.y, z: 3 };
