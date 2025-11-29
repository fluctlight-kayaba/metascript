# Performance Guide

This guide covers optimization techniques, benchmarking, and performance best practices for Metascript.

## Performance Targets

Metascript aims for **90-95% of C performance** on compute-bound workloads. Here's what that means in practice:

| Scenario | Target vs C | Real-World Impact |
|----------|------------|-------------------|
| Tight loops | 90-95% | Math-heavy algorithms, data processing |
| Mixed workloads | 80-90% | Typical applications with some allocation |
| Allocation-heavy | 70-85% | Object-intensive code, frequent GC |
| Lambda cold start | <50ms | 10x faster than Node.js |
| Memory usage | +10-20% | GC overhead, predictable |

## Optimization Principles

### 1. Stack Over Heap

**Problem:** Heap allocation triggers GC, adding overhead.

**Solution:** Use stack allocation for value types.

```typescript
// ❌ Heap allocated (slower)
class Point {
    x: number;
    y: number;
}

const p = new Point(1, 2);  // Heap allocation + GC

// ✅ Stack allocated (faster)
interface Point {
    x: number;
    y: number;
}

const p: Point = { x: 1, y: 2 };  // Stack allocation, no GC
```

**Impact:** ~20-30% faster for small objects in hot paths.

### 2. Avoid Unnecessary Allocations

```typescript
// ❌ Allocates new array on every call
function getValues(): number[] {
    return [1, 2, 3, 4, 5];
}

// ✅ Reuse static array
const VALUES: number[] = [1, 2, 3, 4, 5];
function getValues(): readonly number[] {
    return VALUES;
}

// ✅ Or use @comptime for compile-time constants
const VALUES = @comptime [1, 2, 3, 4, 5];
```

### 3. Monomorphization for Generics

Generics are specialized at compile-time (zero cost):

```typescript
function process<T>(items: T[]): void {
    for (const item of items) {
        // Process item
    }
}

// Compiler generates:
// - process_number(items: number[])
// - process_string(items: string[])
// No runtime type dispatch!
```

### 4. Use `@inline` for Hot Paths

```typescript
// ❌ Function call overhead
function add(a: number, b: number): number {
    return a + b;
}

// ✅ Inlined (no call overhead)
@inline
function add(a: number, b: number): number {
    return a + b;
}

// Compiler replaces calls with direct code:
const result = a + b;  // No function call
```

**Impact:** Eliminates ~2-5ns per call (significant in tight loops).

### 5. Struct-Based Objects

```typescript
// ❌ Dynamic property access (hash table lookup)
const obj: any = {};
obj.x = 1;  // Runtime property addition
const val = obj.x;  // Hash lookup

// ✅ Fixed struct (direct field access)
interface Point {
    x: number;
    y: number;
}

const point: Point = { x: 1, y: 2 };
const val = point.x;  // Direct memory access
```

**Impact:** ~10-20x faster property access.

## Memory Management

### Understanding GC

Metascript uses a **generational garbage collector**:

```
Young Generation (fast, frequent)
  ├─ Most objects die young
  └─ Minor GC: <1ms pause

Old Generation (slow, rare)
  ├─ Long-lived objects
  └─ Major GC: ~10ms pause
```

**Optimize allocation patterns:**

```typescript
// ❌ Creates many short-lived objects
function processData(items: Item[]): Result[] {
    return items.map(item => {
        const temp = new TempObject(item);  // Allocates
        return temp.process();  // Dies immediately
    });
}

// ✅ Reuse objects or avoid allocation
function processData(items: Item[]): Result[] {
    const temp = new TempObject();  // Allocate once
    return items.map(item => {
        temp.reset(item);  // Reuse
        return temp.process();
    });
}
```

### ARC Mode (Optional)

For predictable latency, use reference counting:

```typescript
@memory(strategy: "arc")
class Node {
    value: number;
    next: Node | null;
}

// No GC pauses, deterministic deallocation
// Trade-off: Slightly higher overhead, can't handle cycles
```

**When to use ARC:**
- Real-time systems
- Latency-sensitive applications
- Known acyclic data structures

## Benchmarking

### Built-in Profiling

```bash
# Time compilation
metascript compile --time myapp.mts

# Profile execution
metascript profile myapp.mts

# Memory profiling
metascript profile --memory myapp.mts
```

### Micro-benchmarks

```typescript
import { benchmark } from "@metascript/bench";

benchmark("Array.map", () => {
    const arr = [1, 2, 3, 4, 5];
    arr.map(x => x * 2);
});

benchmark("for loop", () => {
    const arr = [1, 2, 3, 4, 5];
    const result: number[] = [];
    for (const x of arr) {
        result.push(x * 2);
    }
});

// Run: metascript bench mybench.mts
// Output:
// Array.map: 125ns/iter
// for loop: 98ns/iter (21% faster)
```

### Real-World Benchmarks

Compare against C, Rust, Go, Node.js:

```bash
# Fibonacci benchmark
metascript compile fibonacci.mts
time ./fibonacci

# Compare with Node.js
time node fibonacci.js

# Compare with C
gcc -O3 fibonacci.c -o fibonacci-c
time ./fibonacci-c
```

**Example results:**
```
Metascript: 342ms (90% of C)
C (gcc -O3): 301ms (baseline)
Node.js: 682ms (44% of C)
```

## Common Performance Patterns

### Pattern 1: Loop Optimization

```typescript
// ❌ Slow: Dynamic dispatch in loop
interface Shape {
    area(): number;
}

function totalArea(shapes: Shape[]): number {
    let sum = 0;
    for (const shape of shapes) {
        sum += shape.area();  // Virtual call every iteration
    }
    return sum;
}

// ✅ Fast: Monomorphic loop
function totalCircleArea(circles: Circle[]): number {
    let sum = 0;
    for (const circle of circles) {
        sum += circle.area();  // Direct call, can inline
    }
    return sum;
}
```

### Pattern 2: Data-Oriented Design

```typescript
// ❌ Array of objects (poor cache locality)
class Particle {
    x: number;
    y: number;
    vx: number;
    vy: number;
}

const particles: Particle[] = [];

function update(particles: Particle[]): void {
    for (const p of particles) {
        p.x += p.vx;  // Cache miss every access
        p.y += p.vy;
    }
}

// ✅ Structure of arrays (cache-friendly)
interface Particles {
    x: number[];
    y: number[];
    vx: number[];
    vy: number[];
}

function update(particles: Particles, count: number): void {
    for (let i = 0; i < count; i++) {
        particles.x[i] += particles.vx[i];  // Sequential memory access
        particles.y[i] += particles.vy[i];
    }
}
```

**Impact:** ~2-3x faster for large datasets.

### Pattern 3: Compile-Time Computation

```typescript
// ❌ Runtime computation
function fibonacci(n: number): number {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

const fib10 = fibonacci(10);  // Computed every run

// ✅ Compile-time computation
const fib10 = @comptime fibonacci(10);  // Computed at compile-time
// Embedded as constant: const fib10 = 55;
```

### Pattern 4: String Building

```typescript
// ❌ String concatenation (many allocations)
let result = "";
for (const item of items) {
    result += item.toString() + "\n";  // New string each iteration
}

// ✅ StringBuilder (fewer allocations)
const builder = new StringBuilder();
for (const item of items) {
    builder.append(item.toString());
    builder.append("\n");
}
const result = builder.toString();
```

## Lambda/Edge Optimization

### Cold Start Optimization

```typescript
// ❌ Lazy initialization (adds cold start time)
let db: Database | null = null;

export function handler(event: Event): Response {
    if (db === null) {
        db = connectDatabase();  // Slow!
    }
    return db.query(event);
}

// ✅ Ahead-of-time initialization
const db = @comptime {
    // Connection string embedded at compile-time
    return buildConnectionString();
};

export function handler(event: Event): Response {
    const conn = fastConnect(db);  // Fast reconnect
    return conn.query(event);
}
```

**Impact:** <50ms cold starts (vs ~200ms for Node.js).

### Binary Size Optimization

```bash
# Compile with size optimization
metascript compile --optimize=size lambda.mts

# Strip debug info
metascript compile --strip lambda.mts

# Tree-shake unused code
metascript compile --tree-shake lambda.mts
```

**Results:**
- Hello World: ~500KB (vs Node.js ~50MB)
- Typical Lambda: ~1-2MB (vs Node.js ~5-10MB)

## Profiling Tools

### CPU Profiling

```bash
# Generate flamegraph
metascript profile --flamegraph myapp.mts

# Output: flamegraph.svg (open in browser)
```

### Memory Profiling

```bash
# Track allocations
metascript profile --memory --track-allocations myapp.mts

# Output:
# Top allocations:
# 1. String.concat: 45% (234 MB)
# 2. Array.push: 32% (165 MB)
# 3. HashMap.insert: 15% (78 MB)
```

### Compilation Profiling

```bash
# Show compilation breakdown
metascript compile --profile myapp.mts

# Output:
# Parsing: 50ms (10%)
# Macro expansion: 75ms (15%)
# Type checking: 150ms (30%)
# Code generation: 200ms (40%)
# LLVM optimization: 25ms (5%)
# Total: 500ms
```

## Performance Anti-Patterns

### Anti-Pattern 1: Excessive Abstraction

```typescript
// ❌ Too many layers
interface Repository<T> {
    find(id: number): T | null;
}

class BaseRepository<T> implements Repository<T> {
    find(id: number): T | null { ... }
}

class CachedRepository<T> extends BaseRepository<T> {
    find(id: number): T | null {
        return this.cache.get(id) ?? super.find(id);
    }
}

// ✅ Direct and simple
function findUser(id: number): User | null {
    return cache.get(id) ?? db.query(id);
}
```

### Anti-Pattern 2: Premature Allocation

```typescript
// ❌ Allocates even when not needed
function process(data: Data): Result | null {
    const result = new Result();  // Always allocated
    if (!data.valid) {
        return null;  // Wasted allocation!
    }
    result.fill(data);
    return result;
}

// ✅ Allocate only when needed
function process(data: Data): Result | null {
    if (!data.valid) {
        return null;
    }
    const result = new Result();
    result.fill(data);
    return result;
}
```

### Anti-Pattern 3: Boxing Primitives

```typescript
// ❌ Boxed numbers (heap allocated)
const numbers: Number[] = [new Number(1), new Number(2)];

// ✅ Primitive numbers (stack or packed array)
const numbers: number[] = [1, 2, 3];
```

## Optimization Checklist

Performance optimization workflow:

1. **Measure first**
   ```bash
   metascript profile myapp.mts
   ```

2. **Identify hotspots**
   - Where is time spent?
   - What allocates most?

3. **Apply targeted optimizations**
   - [ ] Use stack allocation for small objects
   - [ ] Add `@inline` to hot functions
   - [ ] Replace dynamic dispatch with static
   - [ ] Reduce allocations in loops
   - [ ] Use `@comptime` for constants

4. **Measure again**
   - Verify improvements
   - Check for regressions

5. **Document trade-offs**
   - Note any complexity added
   - Explain why optimization is needed

## When NOT to Optimize

- **Premature optimization is evil**: Profile first
- **Readability matters**: Don't sacrifice clarity for 1% gains
- **Maintainability matters**: Complex code is expensive
- **Focus on algorithms first**: O(n²) → O(n log n) beats micro-optimizations

## Resources

- [Benchmarks Repository](https://github.com/metascript/benchmarks)
- [Performance Blog Posts](https://blog.metascript.dev/tags/performance)
- [Architecture Guide](./architecture.md) - Understand internals
- [Macro System](./macro-system.md) - Compile-time optimization

---

**Key Takeaways:**
- Metascript achieves 90-95% of C performance on compute workloads
- Stack allocation and monomorphization are key to performance
- Measure before optimizing
- Use `@inline`, `@comptime`, and struct-based objects
- Lambda cold starts <50ms (10x faster than Node.js)
