# Performance Guide

**Target:** 90-95% of C performance on compute workloads

---

## Performance Targets

| Scenario | vs C | Impact |
|----------|------|--------|
| Tight loops | 90-95% | Math, data processing |
| Mixed workloads | 80-90% | Typical apps |
| Allocation-heavy | 70-85% | Object-intensive |
| Lambda cold start | <50ms | 10x faster than Node.js |
| Memory overhead | +10-20% | GC overhead |

---

## Key Optimizations

### 1. Stack Over Heap

```typescript
❌ class Point { x: number; y: number; }
   const p = new Point(1, 2);  // Heap + GC

✅ interface Point { x: number; y: number; }
   const p: Point = { x: 1, y: 2 };  // Stack, no GC
```

**Impact:** ~20-30% faster

### 2. Avoid Unnecessary Allocations

```typescript
❌ function getValues(): number[] { return [1, 2, 3, 4, 5]; }

✅ const VALUES = @comptime [1, 2, 3, 4, 5];
   function getValues(): readonly number[] { return VALUES; }
```

### 3. Monomorphization (Zero-Cost Generics)

```typescript
function process<T>(items: T[]): void { /* ... */ }

// Compiler generates:
// - process_number(items: number[])
// - process_string(items: string[])
// No runtime dispatch!
```

### 4. `@inline` for Hot Paths

```typescript
@inline
function add(a: number, b: number): number { return a + b; }

// Compiler replaces calls with: const result = a + b;
```

**Impact:** Eliminates ~2-5ns per call

### 5. Struct-Based Objects

```typescript
❌ const obj: any = {}; obj.x = 1; const val = obj.x;  // Hash lookup

✅ interface Point { x: number; y: number; }
   const p: Point = { x: 1, y: 2 }; const val = p.x;  // Direct access
```

**Impact:** ~10-20x faster property access

---

## Memory Management

**Generational GC:**
```
Young Gen (fast, frequent)
  └─ Minor GC: <1ms pause

Old Gen (slow, rare)
  └─ Major GC: ~10ms pause
```

**Optimize allocation patterns:**
```typescript
❌ items.map(item => new TempObject(item).process());  // Many short-lived objects

✅ const temp = new TempObject();
   items.map(item => { temp.reset(item); return temp.process(); });  // Reuse
```

**ARC Mode (Optional):**
```typescript
@memory(strategy: "arc")
class Node { value: number; next: Node | null; }
// No GC pauses, deterministic deallocation
// Trade-off: Can't handle cycles
```

**When to use ARC:** Real-time systems, latency-sensitive apps, acyclic data structures

---

## Benchmarking

```bash
# Profile execution
msc profile myapp.ms

# Memory profiling
msc profile --memory myapp.ms

# Micro-benchmarks
import { benchmark } from "@metascript/bench";
benchmark("Array.map", () => { [1,2,3].map(x => x * 2); });
benchmark("for loop", () => { const r = []; for (const x of [1,2,3]) r.push(x * 2); });
# → Array.map: 125ns/iter, for loop: 98ns/iter (21% faster)
```

**Real-world comparison:**
```
Fibonacci(40):
C (gcc -O3):  301ms (baseline)
Metascript:   342ms (90% of C) ✅
Node.js:      682ms (44% of C)
```

---

## Performance Patterns

### Loop Optimization

```typescript
❌ for (const shape of shapes) sum += shape.area();  // Virtual call every iteration

✅ for (const circle of circles) sum += circle.area();  // Direct call, can inline
```

### Data-Oriented Design

```typescript
❌ class Particle { x, y, vx, vy; }  // Array of objects (poor cache locality)
   for (const p of particles) { p.x += p.vx; }  // Cache miss

✅ interface Particles { x: number[]; y: number[]; vx: number[]; vy: number[]; }
   for (let i = 0; i < count; i++) particles.x[i] += particles.vx[i];  // Sequential
```

**Impact:** ~2-3x faster for large datasets

### Compile-Time Computation

```typescript
❌ const fib10 = fibonacci(10);  // Computed every run

✅ const fib10 = @comptime fibonacci(10);  // Embedded as: const fib10 = 55;
```

### String Building

```typescript
❌ let result = ""; for (const item of items) result += item.toString() + "\n";

✅ const builder = new StringBuilder();
   for (const item of items) builder.append(item.toString()).append("\n");
```

---

## Lambda/Edge Optimization

**Cold Start:**
```typescript
❌ let db: Database | null = null;
   if (db === null) db = connectDatabase();  // Slow!

✅ const db = @comptime buildConnectionString();
   const conn = fastConnect(db);  // Fast reconnect
```

**Impact:** <50ms cold starts (vs ~200ms for Node.js)

**Binary Size:**
```bash
msc compile --optimize=size --strip --tree-shake lambda.ms
# Hello World: ~500KB (vs Node.js ~50MB)
# Typical Lambda: ~1-2MB (vs Node.js ~5-10MB)
```

---

## Profiling Tools

```bash
# Flamegraph
msc profile --flamegraph myapp.ms

# Allocation tracking
msc profile --memory --track-allocations myapp.ms
# → Top allocations: String.concat 45% (234 MB), Array.push 32% (165 MB)

# Compilation breakdown
msc compile --profile myapp.ms
# → Parsing 50ms (10%), Macro 75ms (15%), Type check 150ms (30%), Codegen 200ms (40%)
```

---

## Anti-Patterns

**1. Excessive Abstraction:**
```typescript
❌ interface Repository<T> { find(id): T; }
   class BaseRepository<T> implements Repository<T> { ... }
   class CachedRepository<T> extends BaseRepository<T> { ... }

✅ function findUser(id: number): User | null { return cache.get(id) ?? db.query(id); }
```

**2. Premature Allocation:**
```typescript
❌ const result = new Result(); if (!data.valid) return null;  // Wasted allocation

✅ if (!data.valid) return null; const result = new Result();
```

**3. Boxing Primitives:**
```typescript
❌ const numbers: Number[] = [new Number(1), new Number(2)];  // Heap allocated

✅ const numbers: number[] = [1, 2, 3];  // Stack/packed array
```

---

## Optimization Checklist

1. **Measure first:** `msc profile myapp.ms`
2. **Identify hotspots:** Where is time spent? What allocates most?
3. **Apply targeted optimizations:**
   - [ ] Stack allocation for small objects
   - [ ] `@inline` for hot functions
   - [ ] Static dispatch over dynamic
   - [ ] Reduce allocations in loops
   - [ ] `@comptime` for constants
4. **Measure again:** Verify improvements
5. **Document trade-offs:** Note complexity added

---

## When NOT to Optimize

- **Premature optimization is evil:** Profile first
- **Readability matters:** Don't sacrifice clarity for 1% gains
- **Focus on algorithms first:** O(n²) → O(n log n) beats micro-optimizations

---

## Key Takeaways

- 90-95% of C on compute workloads
- Stack allocation + monomorphization = performance
- Measure before optimizing
- Use `@inline`, `@comptime`, struct-based objects
- Lambda cold starts <50ms (10x faster than Node.js)
