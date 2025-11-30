# Migration from TypeScript

**TL;DR:** ~70% of strict TypeScript works unchanged. Main changes: no `any`, no dynamic property access, add compile-time macros.

---

## Compatibility Matrix

| TypeScript Feature | Metascript Support | Notes |
|-------------------|-------------------|-------|
| Interfaces | ✅ Full | Compile to structs (fixed layout) |
| Classes | ✅ Full | Nominal typing (not structural) |
| Generics | ✅ Full | Monomorphized at compile-time |
| Arrow functions | ✅ Full | Same syntax |
| Async/await | ⚠️ Partial | Supported but runtime overhead |
| Type inference | ✅ Full | Same behavior |
| Union types | ✅ Full | Discriminated unions preferred |
| Intersection types | ✅ Full | Must be statically resolvable |
| Mapped types | ✅ Mostly | Compile-time only |
| `any` type | ❌ None | Use `unknown` instead |
| `eval()` | ❌ None | Security + performance |
| Dynamic `obj[key]` | ❌ Limited | Use `Map<K, V>` or macros |
| Prototype manipulation | ❌ None | Classes use vtables |
| `typeof`/`instanceof` | ⚠️ Limited | Compile-time known types only |

---

## Key Changes

| What Changes | Before (TypeScript) | After (Metascript) |
|--------------|---------------------|-------------------|
| **No `any` type** | `data: any` | `data: unknown` (with type guards) |
| **No dynamic property access** | `obj[key]` | `Map<K, V>` or compile-time macros |
| **Add macros for boilerplate** | Manual `equals()`, `hashCode()`, `toJSON()` | `@derive(Eq, Hash, Serialize)` |
| **Compile-time config** | `JSON.parse(fs.readFileSync("config.json"))` | `@comptime { readFile("config.json") }` |

---

## Migration Examples

### Example 1: Basic Code (No Changes)
```typescript
// ✅ Works unchanged in Metascript
interface User { name: string; age: number; }
function greet(user: User): string { return `Hello, ${user.name}!`; }
const alice: User = { name: "Alice", age: 30 };
```

### Example 2: Remove `any` → Use `unknown` or Discriminated Unions
```typescript
// ❌ TypeScript with `any`
function processData(data: any): void {
    if (typeof data === "string") console.log(data.toUpperCase());
}

// ✅ Metascript Option 1: unknown with type guards
function processData(data: unknown): void {
    if (typeof data === "string") console.log(data.toUpperCase());
    else throw new Error("Unexpected type");
}

// ✅ Metascript Option 2: Discriminated unions (better)
type Data = { type: "string"; value: string } | { type: "number"; value: number };
function processData(data: Data): void {
    switch (data.type) {
        case "string": console.log(data.value.toUpperCase()); break;
        case "number": console.log(data.value * 2); break;
    }
}
```

### Example 3: Dynamic Property Access → Map or Macros
```typescript
// ❌ TypeScript dynamic access
function getValue(obj: any, key: string): any { return obj[key]; }

// ✅ Metascript Option 1: Map
function getValue<V>(map: Map<string, V>, key: string): V | undefined {
    return map.get(key);
}

// ✅ Metascript Option 2: Macro (compile-time dispatch)
function getValue<T>(obj: T, key: keyof T): T[keyof T] {
    return @comptime { generatePropertyAccessor<T>(key); };
}
```

### Example 4: Eliminate Boilerplate with Macros
```typescript
// ❌ TypeScript (100 lines of boilerplate)
class User {
    name: string; email: string; age: number;
    equals(other: User): boolean { return this.name === other.name && /*...*/ }
    hashCode(): number { return hash(this.name) ^ hash(this.email) ^ /*...*/ }
    clone(): User { return new User(this.name, this.email, this.age); }
    toString(): string { return `User(name=${this.name}, ...)`; }
}

// ✅ Metascript (4 lines + auto-generated methods)
@derive(Eq, Hash, Clone, Debug)
class User {
    name: string; email: string; age: number;
}
```

### Example 5: Compile-Time Configuration
```typescript
// ❌ TypeScript (runtime overhead)
const config: any = JSON.parse(fs.readFileSync("config.json", "utf8"));

// ✅ Metascript (embedded at compile-time, zero runtime cost)
const config = @comptime {
    const data = readFile("config.json");
    return parseJSON<Config>(data);
};
```

---

## Migration Strategy

### Phase 1: Enable Strict TypeScript
```json
// tsconfig.json
{
  "compilerOptions": {
    "strict": true,
    "noImplicitAny": true,
    "strictNullChecks": true
  }
}
```
Run `tsc --noEmit` and fix errors. This brings you **70% of the way** to Metascript compatibility.

### Phase 2: Remove `any` and `eval`
```bash
# Find problematic patterns
grep -r ": any" src/
grep -r "eval\\|new Function" src/
```

**Replace:**
- `any` → `unknown` (with type guards) or discriminated unions
- `eval()` → Compile-time macros or refactor
- Dynamic property access → `Map<K, V>` or compile-time generation

### Phase 3: Test with Metascript
```bash
msc check src/**/*.ts  # Type-check your code
# Fix errors (Metascript points to specific issues)
```

### Phase 4: Add Macros for Boilerplate
- `@derive(Eq, Hash)` for equality/hashing
- `@derive(Serialize)` for JSON serialization
- `@derive(Builder)` for builder patterns
- `@derive(Validate)` for runtime validation

### Phase 5: Optimize Performance
- `@inline` for hot path functions
- `@comptime` for compile-time computation
- Profile and optimize (see [Performance Guide](./performance-guide.md))

---

## Performance Benefits After Migration

| Metric | TypeScript/Node.js | Metascript | Improvement |
|--------|-------------------|------------|-------------|
| Cold start | ~200ms | ~20ms | **10x faster** |
| Memory usage | ~50MB | ~10MB | **5x smaller** |
| Execution speed | Baseline | 2-3x faster | **2-3x faster** |
| Binary size | Node.js (~50MB) | ~1MB | **50x smaller** |

---

## Gradual Migration

**Strategy:** Keep TypeScript for frontend, use Metascript for backend.

```
project/
├── frontend/         # TypeScript + React
│   └── tsconfig.json
├── backend/          # Metascript + Lambda
│   └── metascript.json
└── shared/           # Shared type definitions
    └── types.d.ts
```

**Benefits:**
1. Frontend stays on TypeScript (browser ecosystem)
2. Backend gets native performance (Lambda/APIs)
3. Share types via `.d.ts` files
4. Serialize at boundaries (JSON communication)

---

## Migration Checklist

- [ ] Enable strict TypeScript mode
- [ ] Remove all `any` types (replace with `unknown` or specific types)
- [ ] Remove `eval()` and `new Function()`
- [ ] Replace dynamic property access with `Map` or macros
- [ ] Test with `msc check`
- [ ] Add `@derive` macros for boilerplate
- [ ] Profile and optimize
- [ ] Deploy and measure improvements

---

**See:** `macro-system.md` (compile-time metaprogramming), `performance-guide.md` (optimization)

**Help:**
- GitHub Discussions: [metascript/discussions](https://github.com/fluctlight-kayaba/metascript/discussions)
- Discord: [#migration channel](https://discord.gg/metascript)
- Examples: [metascript-examples repo](https://github.com/metascript/examples)
