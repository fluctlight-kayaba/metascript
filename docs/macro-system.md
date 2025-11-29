# Macro System

Metascript's macro system enables compile-time metaprogramming, bridging dynamic patterns to static native code. This guide covers everything from basic `@comptime` functions to advanced AST manipulation.

## What Are Macros?

Macros are functions that run at **compile-time** to generate code. They turn this:

```typescript
@derive(Eq, Hash, Clone, Debug)
class User {
    name: string;
    age: number;
}
```

Into this (automatically generated):

```typescript
class User {
    name: string;
    age: number;

    equals(other: User): boolean {
        return this.name === other.name && this.age === other.age;
    }

    hashCode(): number {
        return hash(this.name) ^ hash(this.age);
    }

    clone(): User {
        return new User(this.name, this.age);
    }

    toString(): string {
        return `User { name: ${this.name}, age: ${this.age} }`;
    }
}
```

## Why Macros?

**Problem**: TypeScript requires lots of boilerplate for common patterns (equality, serialization, validation, etc.).

**Solution**: Macros eliminate boilerplate by generating code at compile-time.

**Benefits**:
- **Zero runtime cost** - Code is generated before compilation
- **Type-safe** - Macros work with the type system
- **Transparent** - See generated code with `metascript expand`
- **Composable** - Combine multiple macros

## Basic Macros: `@comptime`

The simplest macro is a compile-time function:

```typescript
function buildVersion(): string {
    return @comptime {
        const gitHash = exec("git rev-parse --short HEAD");
        const buildDate = new Date().toISOString();
        return `${gitHash}-${buildDate}`;
    };
}

// buildVersion() returns a constant string embedded in the binary
const version = buildVersion();  // "a3f5d2c-2025-11-30T10:23:45.000Z"
```

**Key points**:
- `@comptime { ... }` runs during compilation
- Result is embedded as a constant
- No runtime overhead

## Derive Macros

`@derive` auto-generates trait implementations:

### Eq (Equality)

```typescript
@derive(Eq)
class Point {
    x: number;
    y: number;
}

// Generates:
equals(other: Point): boolean {
    return this.x === other.x && this.y === other.y;
}
```

### Hash

```typescript
@derive(Hash)
class Point {
    x: number;
    y: number;
}

// Generates:
hashCode(): number {
    return hash(this.x) ^ hash(this.y);
}
```

### Clone

```typescript
@derive(Clone)
class Point {
    x: number;
    y: number;
}

// Generates:
clone(): Point {
    return new Point(this.x, this.y);
}
```

### Debug

```typescript
@derive(Debug)
class Point {
    x: number;
    y: number;
}

// Generates:
toString(): string {
    return `Point { x: ${this.x}, y: ${this.y} }`;
}
```

### Combine Multiple Traits

```typescript
@derive(Eq, Hash, Clone, Debug)
class User {
    id: number;
    name: string;
    email: string;
}

// Generates all four methods automatically
```

## Serialization Macros

### JSON Serialization

```typescript
@derive(Serialize, Deserialize)
class User {
    id: number;
    name: string;
    createdAt: Date;
}

// Auto-generated methods:
const user = new User(1, "Alice", new Date());
const json = JSON.stringify(user);  // Serialize
const restored = JSON.parse<User>(json);  // Deserialize with type safety
```

### Custom Serialization

```typescript
@serialize({
    rename: { createdAt: "created_at" },
    skip: ["password"],
    transform: {
        createdAt: (d: Date) => d.getTime()
    }
})
class User {
    id: number;
    name: string;
    password: string;
    createdAt: Date;
}

const json = JSON.stringify(user);
// { "id": 1, "name": "Alice", "created_at": 1732963425000 }
// password is omitted
```

## FFI Macros

Generate TypeScript bindings from C headers:

```typescript
// Compile-time FFI binding generation
const libc = @comptime bindC("./libc.h");

// Use C functions with type safety
const fd = libc.open("/tmp/test.txt", libc.O_RDWR);
const buf = new Uint8Array(1024);
const bytesRead = libc.read(fd, buf, 1024);
libc.close(fd);
```

### Custom FFI Wrapper

```typescript
@ffi({
    library: "libcurl",
    functions: [
        "curl_easy_init",
        "curl_easy_setopt",
        "curl_easy_perform",
        "curl_easy_cleanup"
    ]
})
class Curl {
    // Auto-generated bindings
}

const curl = Curl.curl_easy_init();
Curl.curl_easy_setopt(curl, Curl.CURLOPT_URL, "https://example.com");
Curl.curl_easy_perform(curl);
Curl.curl_easy_cleanup(curl);
```

## AST Manipulation

Advanced macros can inspect and generate code:

### Type-Driven Code Generation

```typescript
function generateValidator<T>(): (value: unknown) => T {
    return @comptime {
        const type = getTypeInfo<T>();
        const validators: string[] = [];

        for (const field of type.fields) {
            const check = `
                if (typeof value.${field.name} !== "${field.type}") {
                    throw new Error("Invalid ${field.name}");
                }
            `;
            validators.push(check);
        }

        return eval(`
            (value) => {
                ${validators.join("\n")}
                return value as T;
            }
        `);
    };
}

interface User {
    name: string;
    age: number;
}

const validateUser = generateValidator<User>();
const user = validateUser({ name: "Alice", age: 30 });  // Type-safe
```

### Builder Pattern Generation

```typescript
@derive(Builder)
class HttpRequest {
    method: string;
    url: string;
    headers: Map<string, string>;
    body: string | null;
}

// Auto-generated builder:
const request = HttpRequest.builder()
    .method("GET")
    .url("https://api.example.com/users")
    .header("Authorization", "Bearer token")
    .build();
```

## Compile-Time Configuration

```typescript
// config.mts
export const config = @comptime {
    const env = readEnv("NODE_ENV");
    const secrets = readFile(".env");

    return {
        environment: env,
        apiUrl: env === "production"
            ? "https://api.prod.com"
            : "http://localhost:3000",
        apiKey: parseEnv(secrets).API_KEY,
        debug: env === "development",
        features: {
            analytics: env === "production",
            logging: true
        }
    };
};

// Config is embedded in binary at compile-time
// No runtime config loading overhead
```

## Macro Utilities

### Standard Macro Library

```typescript
import {
    derive,
    comptime,
    inline,
    bindC,
    buildInfo,
    embedFile
} from "@metascript/macros";

// Embed files at compile-time
const schema = embedFile("./schema.sql");
const logo = embedFile("./logo.png");

// Build information
const build = buildInfo();
console.log(build.version);    // Git hash
console.log(build.timestamp);  // Build time
console.log(build.target);     // Platform target
```

### Inline Macro

```typescript
// Force inline for performance-critical code
@inline
function add(a: number, b: number): number {
    return a + b;
}

// Compiler inlines all calls to add()
// No function call overhead
```

## Macro Development

### Creating Custom Macros

```typescript
// macros/my-macro.mts
export function myMacro(target: ClassDeclaration): void {
    // Inspect AST
    const className = target.name;
    const fields = target.fields;

    // Generate code
    const methods = fields.map(field => `
        get${capitalize(field.name)}() {
            return this.${field.name};
        }
        set${capitalize(field.name)}(value: ${field.type}) {
            this.${field.name} = value;
        }
    `);

    // Inject into class
    target.addMethods(methods);
}

// Usage:
@myMacro
class User {
    name: string;
    age: number;
}

// Auto-generates getName(), setName(), getAge(), setAge()
```

### Macro API Reference

```typescript
// Type information
interface TypeInfo {
    name: string;
    fields: Field[];
    methods: Method[];
    parent: TypeInfo | null;
}

interface Field {
    name: string;
    type: string;
    optional: boolean;
    visibility: "public" | "private" | "protected";
}

// AST manipulation
class ClassDeclaration {
    name: string;
    fields: Field[];
    methods: Method[];

    addField(field: Field): void;
    addMethod(method: Method): void;
    extends(parent: ClassDeclaration): void;
}

// Compile-time utilities
function readFile(path: string): string;
function exec(command: string): string;
function getTypeInfo<T>(): TypeInfo;
function readEnv(key: string): string;
```

## Macro Best Practices

### 1. Keep Macros Simple

```typescript
// ✅ Good: Simple, focused macro
@derive(Eq)
class Point {
    x: number;
    y: number;
}

// ❌ Bad: Complex macro doing too much
@derive(Eq, Hash, Clone, Serialize, Validate, Builder, Debug)
class Point {
    x: number;
    y: number;
}
```

### 2. Make Generated Code Readable

```typescript
// ✅ Good: Clear generated code
equals(other: User): boolean {
    return this.id === other.id &&
           this.name === other.name;
}

// ❌ Bad: Obfuscated generated code
equals(o: any): boolean {
    return Object.keys(this).every(k => this[k] === o[k]);
}
```

### 3. Provide Clear Error Messages

```typescript
// ✅ Good: Helpful error
@derive(Eq)
class User {
    id: number;
    metadata: Map<string, any>;
}
// Error: @derive(Eq) cannot handle field 'metadata' of type Map<string, any>.
// Suggestion: Implement equals() manually or use @derive(Eq, ignore: ["metadata"])

// ❌ Bad: Cryptic error
// Error: Type 'Map' is not equatable
```

### 4. Document Macro Behavior

```typescript
/**
 * @derive(Eq)
 *
 * Generates structural equality comparison.
 *
 * Compares all fields using === operator.
 * For reference types, use @derive(Eq, deep: true) for deep comparison.
 *
 * Example:
 *   @derive(Eq)
 *   class Point { x: number; y: number; }
 *
 *   const p1 = new Point(1, 2);
 *   const p2 = new Point(1, 2);
 *   p1.equals(p2);  // true
 */
```

## Debugging Macros

### View Expanded Code

```bash
# See what code macros generate
metascript expand --macro=derive user.mts
```

### Step Through Macro Execution

```bash
# Debug macro expansion
metascript compile --debug-macros user.mts
```

### Macro Compilation Errors

```typescript
@derive(Eq)
class User {
    callback: () => void;  // Error: Cannot derive Eq for function types
}

// Error message shows:
// Error at user.mts:2:5
//   @derive(Eq) cannot handle field 'callback' of type '() => void'
//   Suggestion: Implement equals() manually or exclude this field
```

## Performance Considerations

### Macro Overhead

```bash
# Measure macro compilation time
metascript compile --profile user.mts

# Output:
# Parsing: 5ms
# Macro expansion: 15ms
# Type checking: 20ms
# Code generation: 30ms
# Total: 70ms
```

**Target**: Macro expansion should be <10% of total build time.

### Generated Code Size

```bash
# Check generated code size
metascript emit-c --stats user.mts

# Output:
# Original lines: 50
# Generated lines: 250
# Expansion ratio: 5x
```

## Standard Macro Reference

| Macro | Purpose | Generated Code |
|-------|---------|----------------|
| `@derive(Eq)` | Equality | `equals(other): boolean` |
| `@derive(Hash)` | Hashing | `hashCode(): number` |
| `@derive(Clone)` | Cloning | `clone(): T` |
| `@derive(Debug)` | String repr | `toString(): string` |
| `@derive(Serialize)` | JSON encoding | `toJSON(): object` |
| `@derive(Deserialize)` | JSON decoding | `static fromJSON(json): T` |
| `@derive(Builder)` | Builder pattern | `static builder(): Builder<T>` |
| `@derive(Validate)` | Validation | `static validate(data): T` |
| `@comptime` | Compile-time exec | Embedded constant |
| `@inline` | Force inline | No function call |
| `@bindC(path)` | FFI bindings | Type-safe C bindings |

## Next Steps

- [Type System](./type-system.md) - How macros interact with types
- [Standard Macros](./standard-macros.md) - Complete macro library reference
- [Performance Guide](./performance-guide.md) - Optimizing macro usage
- [Architecture](./architecture.md) - How the macro system works internally

---

**Key Takeaways**:
- Macros eliminate boilerplate at zero runtime cost
- `@comptime` runs code during compilation
- `@derive` auto-generates trait implementations
- Macros are type-safe and transparent
- Custom macros enable domain-specific optimizations
