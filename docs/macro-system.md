# Macro System

**Purpose:** Compile-time metaprogramming - eliminate boilerplate at zero runtime cost

---

## Core Concept

Macros run at **compile-time** to generate code:

**Input:**
```typescript
@derive(Eq, Hash, Clone, Debug)
class User {
    name: string;
    age: number;
}
```

**Output (auto-generated):**
```typescript
class User {
    name: string;
    age: number;
    equals(other: User): boolean { return this.name === other.name && this.age === other.age; }
    hashCode(): number { return hash(this.name) ^ hash(this.age); }
    clone(): User { return new User(this.name, this.age); }
    toString(): string { return `User { name: ${this.name}, age: ${this.age} }`; }
}
```

**Benefits:**
- Zero runtime cost (generated before compilation)
- Type-safe (works with type system)
- Transparent (view with `msc expand`)
- Composable (combine multiple macros)

---

## `@comptime` - Compile-Time Execution

```typescript
const version = @comptime {
    const gitHash = exec("git rev-parse --short HEAD");
    const buildDate = new Date().toISOString();
    return `${gitHash}-${buildDate}`;
};
// version = "a3f5d2c-2025-11-30T10:23:45.000Z" (embedded constant)
```

**Key:** Runs during compilation, result embedded as constant, no runtime overhead.

---

## `@derive` - Trait Auto-Generation

| Trait | Generated Method | Example |
|-------|------------------|---------|
| `Eq` | `equals(other): boolean` | Structural equality |
| `Hash` | `hashCode(): number` | Hash function |
| `Clone` | `clone(): T` | Deep copy |
| `Debug` | `toString(): string` | String representation |
| `Serialize` | `toJSON(): object` | JSON encoding |
| `Deserialize` | `static fromJSON(json): T` | JSON decoding |
| `Builder` | `static builder(): Builder<T>` | Builder pattern |
| `Validate` | `static validate(data): T` | Runtime validation |

**Example:**
```typescript
@derive(Eq)
class Point {
    x: number;
    y: number;
}
// Generates: equals(other: Point): boolean { return this.x === other.x && this.y === other.y; }
```

---

## Custom Serialization

```typescript
@serialize({
    rename: { createdAt: "created_at" },
    skip: ["password"],
    transform: { createdAt: (d: Date) => d.getTime() }
})
class User {
    id: number;
    name: string;
    password: string;
    createdAt: Date;
}

JSON.stringify(user);
// → { "id": 1, "name": "Alice", "created_at": 1732963425000 }
```

---

## FFI Macros

```typescript
const libc = @comptime bindC("./libc.h");

const fd = libc.open("/tmp/test.txt", libc.O_RDWR);
const buf = new Uint8Array(1024);
const bytesRead = libc.read(fd, buf, 1024);
libc.close(fd);
```

**Custom FFI:**
```typescript
@ffi({
    library: "libcurl",
    functions: ["curl_easy_init", "curl_easy_setopt", "curl_easy_perform", "curl_easy_cleanup"]
})
class Curl { /* Auto-generated bindings */ }
```

---

## Advanced: AST Manipulation

**Type-Driven Code Generation:**
```typescript
function generateValidator<T>(): (value: unknown) => T {
    return @comptime {
        const type = getTypeInfo<T>();
        const validators = type.fields.map(field =>
            `if (typeof value.${field.name} !== "${field.type}") throw new Error("Invalid ${field.name}");`
        );
        return eval(`(value) => { ${validators.join("\n")} return value as T; }`);
    };
}

const validateUser = generateValidator<User>();
const user = validateUser({ name: "Alice", age: 30 });  // Type-safe
```

**Builder Pattern:**
```typescript
@derive(Builder)
class HttpRequest {
    method: string;
    url: string;
    headers: Map<string, string>;
    body: string | null;
}

const request = HttpRequest.builder()
    .method("GET")
    .url("https://api.example.com/users")
    .header("Authorization", "Bearer token")
    .build();
```

---

## Compile-Time Configuration

```typescript
export const config = @comptime {
    const env = readEnv("NODE_ENV");
    return {
        apiUrl: env === "production" ? "https://api.prod.com" : "http://localhost:3000",
        apiKey: parseEnv(readFile(".env")).API_KEY,
        debug: env === "development"
    };
};
// Config embedded in binary - no runtime config loading
```

---

## Standard Macro Library

```typescript
import { derive, comptime, inline, bindC, buildInfo, embedFile } from "@metascript/macros";

const schema = embedFile("./schema.sql");
const logo = embedFile("./logo.png");

const build = buildInfo();
console.log(build.version);    // Git hash
console.log(build.timestamp);  // Build time
console.log(build.target);     // Platform target

@inline
function add(a: number, b: number): number { return a + b; }
// Compiler inlines all calls - no function call overhead
```

---

## Custom Macro Development

```typescript
// macros/my-macro.ts
export function myMacro(target: ClassDeclaration): void {
    const methods = target.fields.map(field => `
        get${capitalize(field.name)}() { return this.${field.name}; }
        set${capitalize(field.name)}(value: ${field.type}) { this.${field.name} = value; }
    `);
    target.addMethods(methods);
}

@myMacro
class User {
    name: string;
    age: number;
}
// Auto-generates getName(), setName(), getAge(), setAge()
```

**Macro API:**
```typescript
interface TypeInfo {
    name: string;
    fields: Field[];
    methods: Method[];
    parent: TypeInfo | null;
}

function readFile(path: string): string;
function exec(command: string): string;
function getTypeInfo<T>(): TypeInfo;
function readEnv(key: string): string;
```

---

## Best Practices

**1. Keep Simple:**
```typescript
✅ @derive(Eq)              // Focused
❌ @derive(Eq, Hash, Clone, Serialize, Validate, Builder, Debug)  // Too many
```

**2. Readable Generated Code:**
```typescript
✅ equals(other: User): boolean { return this.id === other.id && this.name === other.name; }
❌ equals(o: any): boolean { return Object.keys(this).every(k => this[k] === o[k]); }
```

**3. Clear Errors:**
```typescript
✅ Error: @derive(Eq) cannot handle field 'metadata' of type Map<string, any>.
   Suggestion: Implement equals() manually or use @derive(Eq, ignore: ["metadata"])
❌ Error: Type 'Map' is not equatable
```

**4. Document Behavior:**
```typescript
/**
 * @derive(Eq) - Generates structural equality comparison.
 * Compares all fields using === operator.
 * For deep comparison, use @derive(Eq, deep: true).
 */
```

---

## Debugging

```bash
# View expanded code
msc expand --macro=derive user.ms

# Debug macro execution
msc compile --debug-macros user.ms

# Profile macro overhead
msc compile --profile user.ms
# → Parsing: 5ms, Macro expansion: 15ms, Type checking: 20ms, Total: 70ms
```

**Target:** Macro expansion <10% of total build time.

---

## Performance

**Macro Overhead:**
```
Parsing:         5ms
Macro expansion: 15ms  (target <10% of total)
Type checking:   20ms
Code generation: 30ms
Total:           70ms
```

**Generated Code Size:**
```
Original lines:  50
Generated lines: 250
Expansion ratio: 5x (acceptable)
```

---

## Standard Macro Reference

| Macro | Purpose | Generated |
|-------|---------|-----------|
| `@derive(Eq)` | Equality | `equals(other): boolean` |
| `@derive(Hash)` | Hashing | `hashCode(): number` |
| `@derive(Clone)` | Cloning | `clone(): T` |
| `@derive(Debug)` | String repr | `toString(): string` |
| `@derive(Serialize)` | JSON encoding | `toJSON(): object` |
| `@derive(Deserialize)` | JSON decoding | `static fromJSON(json): T` |
| `@derive(Builder)` | Builder pattern | `static builder(): Builder<T>` |
| `@comptime` | Compile-time exec | Embedded constant |
| `@inline` | Force inline | No function call overhead |
| `@bindC(path)` | FFI bindings | Type-safe C bindings |

---

## Key Takeaways

- Macros eliminate boilerplate at zero runtime cost
- `@comptime` runs code during compilation
- `@derive` auto-generates trait implementations
- Macros are type-safe and transparent
- Custom macros enable domain-specific optimizations

**See:**
- `source-defined-macros.md` - Nim-style macros defined in `.ms` source files
- `architecture.md` - Macro expander design
- `lsp-implementation.md` - Trans-Am engine and macro expansion in LSP
