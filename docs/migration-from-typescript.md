# Migration from TypeScript

This guide helps TypeScript developers understand what stays the same and what changes when moving to Metascript.

## TL;DR

**What stays the same:**
- ✅ TypeScript syntax (interfaces, classes, generics, arrow functions)
- ✅ Type annotations and inference
- ✅ Strict type checking
- ✅ Modern language features (destructuring, spread, async/await)

**What changes:**
- ❌ No `any` type (use `unknown` with type guards)
- ❌ No dynamic property addition
- ❌ No `eval` or dynamic code execution
- ❌ Stricter type requirements (everything must be statically resolvable)
- ➕ Compile-time macros (new superpower!)
- ➕ Native compilation (5-10x faster cold starts)

## Compatibility Matrix

| TypeScript Feature | Metascript Support | Notes |
|-------------------|-------------------|-------|
| Interfaces | ✅ Full | Compile to structs with fixed layout |
| Classes | ✅ Full | Nominal typing (not structural) |
| Generics | ✅ Full | Monomorphized at compile-time |
| Arrow functions | ✅ Full | Same syntax |
| Async/await | ⚠️ Partial | Supported but with runtime overhead |
| Type inference | ✅ Full | Same behavior |
| Union types | ✅ Full | Discriminated unions preferred |
| Intersection types | ✅ Full | Must be statically resolvable |
| Mapped types | ✅ Mostly | Compile-time only |
| `any` type | ❌ None | Use `unknown` instead |
| `eval()` | ❌ None | Security and performance reasons |
| Dynamic `obj[key]` | ❌ Limited | Use `Map<K, V>` or macros |
| Prototype manipulation | ❌ None | Classes use vtables |
| `typeof`/`instanceof` | ⚠️ Limited | Only for compile-time known types |

## Code Migration Examples

### Example 1: Basic TypeScript

**TypeScript (before):**
```typescript
interface User {
    name: string;
    age: number;
}

function greet(user: User): string {
    return `Hello, ${user.name}!`;
}

const alice: User = { name: "Alice", age: 30 };
console.log(greet(alice));
```

**Metascript (after):**
```typescript
// ✅ Identical! No changes needed
interface User {
    name: string;
    age: number;
}

function greet(user: User): string {
    return `Hello, ${user.name}!`;
}

const alice: User = { name: "Alice", age: 30 };
console.log(greet(alice));
```

### Example 2: Removing `any`

**TypeScript (before):**
```typescript
function processData(data: any): void {
    if (typeof data === "string") {
        console.log(data.toUpperCase());
    } else if (typeof data === "number") {
        console.log(data * 2);
    }
}
```

**Metascript (after):**
```typescript
// ✅ Use 'unknown' with type guards
function processData(data: unknown): void {
    if (typeof data === "string") {
        console.log(data.toUpperCase());
    } else if (typeof data === "number") {
        console.log(data * 2);
    } else {
        throw new Error("Unexpected type");
    }
}

// ✅ Better: Use discriminated unions
type Data =
    | { type: "string"; value: string }
    | { type: "number"; value: number };

function processData(data: Data): void {
    switch (data.type) {
        case "string":
            console.log(data.value.toUpperCase());
            break;
        case "number":
            console.log(data.value * 2);
            break;
    }
}
```

### Example 3: Dynamic Property Access

**TypeScript (before):**
```typescript
function getValue(obj: any, key: string): any {
    return obj[key];  // Dynamic access
}

const user = { name: "Alice", age: 30 };
const name = getValue(user, "name");
```

**Metascript (after - Option 1: Map):**
```typescript
// ✅ Use Map for truly dynamic keys
function getValue<V>(map: Map<string, V>, key: string): V | undefined {
    return map.get(key);
}

const user = new Map<string, string | number>();
user.set("name", "Alice");
user.set("age", 30);

const name = getValue(user, "name");
```

**Metascript (after - Option 2: Macro):**
```typescript
// ✅ Use macro for static dispatch
function getValue<T>(obj: T, key: keyof T): T[keyof T] {
    return @comptime {
        // Macro generates type-safe switch at compile-time
        generatePropertyAccessor<T>(key);
    };
}

interface User {
    name: string;
    age: number;
}

const user: User = { name: "Alice", age: 30 };
const name = getValue(user, "name");  // Type-safe, compiled to direct access
```

### Example 4: Boilerplate Elimination with Macros

**TypeScript (before):**
```typescript
class User {
    name: string;
    email: string;
    age: number;

    equals(other: User): boolean {
        return this.name === other.name &&
               this.email === other.email &&
               this.age === other.age;
    }

    hashCode(): number {
        return hash(this.name) ^ hash(this.email) ^ hash(this.age);
    }

    clone(): User {
        return new User(this.name, this.email, this.age);
    }

    toString(): string {
        return `User(name=${this.name}, email=${this.email}, age=${this.age})`;
    }
}
```

**Metascript (after):**
```typescript
// ✅ Use macros to auto-generate boilerplate
@derive(Eq, Hash, Clone, Debug)
class User {
    name: string;
    email: string;
    age: number;
}

// Compiler generates equals(), hashCode(), clone(), toString() automatically
```

### Example 5: Serialization

**TypeScript (before):**
```typescript
class User {
    name: string;
    email: string;
    age: number;

    toJSON(): object {
        return {
            name: this.name,
            email: this.email,
            age: this.age
        };
    }

    static fromJSON(json: any): User {
        return new User(json.name, json.email, json.age);
    }
}
```

**Metascript (after):**
```typescript
// ✅ Use @derive macro
@derive(Serialize, Deserialize)
class User {
    name: string;
    email: string;
    age: number;
}

// Automatic JSON serialization
const user = new User("Alice", "alice@example.com", 30);
const json = JSON.stringify(user);  // Auto-generated
const restored = JSON.parse<User>(json);  // Type-safe parsing
```

## Migration Strategy

### Phase 1: Enable Strict Mode

If you haven't already, enable strict TypeScript:

```json
// tsconfig.json
{
  "compilerOptions": {
    "strict": true,
    "noImplicitAny": true,
    "strictNullChecks": true,
    "strictFunctionTypes": true,
    "strictPropertyInitialization": true,
    "noImplicitThis": true,
    "alwaysStrict": true
  }
}
```

Run `tsc --noEmit` and fix all errors. This brings you 70% of the way to Metascript compatibility.

### Phase 2: Remove `any` and `eval`

Search and replace:
```bash
# Find all 'any' usages
grep -r ": any" src/

# Find eval and Function constructor
grep -r "eval\\|new Function" src/
```

Replace with:
- `any` → `unknown` (with type guards)
- `eval()` → Compile-time macros or refactor
- Dynamic property access → `Map` or compile-time generation

### Phase 3: Test with Metascript

```bash
# Type-check your code
metascript check src/**/*.ts

# Fix errors (likely dynamic patterns)
# Metascript will point to specific issues
```

### Phase 4: Add Macros for Boilerplate

Identify repetitive patterns:
- Equality/hashing implementations → `@derive(Eq, Hash)`
- Serialization code → `@derive(Serialize)`
- Builder patterns → `@derive(Builder)`
- Error handling boilerplate → `@derive(Result)`

### Phase 5: Optimize for Performance

Use Metascript-specific features:
- `@inline` for hot path functions
- `@stack` for value types
- `@comptime` for compile-time computation
- Profile and optimize (see [Performance Guide](./performance-guide.md))

## Common Migration Patterns

### Pattern 1: Configuration Loading

**Before (TypeScript):**
```typescript
const config: any = JSON.parse(fs.readFileSync("config.json", "utf8"));
```

**After (Metascript):**
```typescript
// ✅ Compile-time configuration
const config = @comptime {
    const data = readFile("config.json");
    return parseJSON<Config>(data);
};
// Config is embedded in binary at compile-time
```

### Pattern 2: Validation

**Before (TypeScript):**
```typescript
function validateUser(data: any): User {
    if (typeof data.name !== "string") throw new Error("Invalid name");
    if (typeof data.age !== "number") throw new Error("Invalid age");
    return data as User;
}
```

**After (Metascript):**
```typescript
// ✅ Use @derive for automatic validation
@derive(Validate)
class User {
    name: string;
    age: number;
}

const user = User.fromUntrusted(data);  // Throws on validation failure
```

### Pattern 3: Mocking and Testing

**Before (TypeScript):**
```typescript
const mockDb: any = {
    query: jest.fn().mockResolvedValue([{ id: 1 }])
};
```

**After (Metascript):**
```typescript
// ✅ Use interfaces for dependency injection
interface Database {
    query<T>(sql: string): Promise<T[]>;
}

class MockDatabase implements Database {
    query<T>(sql: string): Promise<T[]> {
        return Promise.resolve([{ id: 1 } as T]);
    }
}

const mockDb: Database = new MockDatabase();
```

## Gradual Migration

You don't have to migrate everything at once:

1. **Keep TypeScript for frontend** - Use for web apps, browser code
2. **Use Metascript for backend** - Lambda functions, APIs, CLI tools
3. **Share type definitions** - Export interfaces from `.d.ts` files
4. **Communicate via JSON** - Serialize at boundaries

Example project structure:
```
project/
├── frontend/         # TypeScript + React
│   └── tsconfig.json
├── backend/          # Metascript + Lambda
│   └── metascript.json
└── shared/           # Shared type definitions
    └── types.d.ts
```

## Performance Benefits

After migration, you'll see:

| Metric | TypeScript/Node.js | Metascript | Improvement |
|--------|-------------------|------------|-------------|
| Cold start | ~200ms | ~20ms | **10x faster** |
| Memory usage | ~50MB | ~10MB | **5x smaller** |
| Execution speed | Baseline | 2-3x faster | **2-3x faster** |
| Binary size | Node.js (~50MB) | ~1MB | **50x smaller** |

## Migration Checklist

- [ ] Enable strict TypeScript mode
- [ ] Remove all `any` types (replace with `unknown` or specific types)
- [ ] Remove `eval()` and `new Function()`
- [ ] Replace dynamic property access with `Map` or macros
- [ ] Test with `metascript check`
- [ ] Add `@derive` macros for boilerplate
- [ ] Profile and optimize (see Performance Guide)
- [ ] Deploy and measure improvements

## Getting Help

- **Migration Issues**: [GitHub Discussions](https://github.com/metascript/metascript/discussions)
- **Discord**: [#migration channel](https://discord.gg/metascript)
- **Examples**: [metascript-examples repo](https://github.com/metascript/examples)

---

**Next Steps:**
- [Type System](./type-system.md) - Deep dive into Metascript types
- [Macro System](./macro-system.md) - Master compile-time metaprogramming
- [Performance Guide](./performance-guide.md) - Optimization techniques
