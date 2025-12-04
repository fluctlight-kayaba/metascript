# Metaprogramming & Macro System

**Purpose:** Compile-time code generation with zero runtime cost

**Design:** Nim-inspired syntax, TypeScript-familiar usage

---

## Syntax Convention

| Action | Syntax | Example |
|--------|--------|---------|
| **Define** macro | Keyword only | `macro derive(...)` |
| **Use** macro | Always `@` prefix | `@derive(Eq)`, `@comptime { }` |

---

## Core Macros

### @derive - Trait Auto-Generation

```typescript
@derive(Eq, Hash, Clone, Debug)
class User {
    name: string;
    age: number;
}

// Generates:
// equals(other: User): boolean
// hashCode(): number
// clone(): User
// toString(): string
```

**Supported Traits:**

| Trait | Generated Method |
|-------|------------------|
| Eq | `equals(other): boolean` |
| Hash | `hashCode(): number` |
| Clone | `clone(): T` |
| Debug | `toString(): string` |
| Serialize | `toJSON(): object` |
| Deserialize | `static fromJSON(json): T` |
| Builder | `static builder(): Builder<T>` |
| Validate | `static validate(data): T` |

### @comptime - Compile-Time Execution

```typescript
const version = @comptime {
    const gitHash = exec("git rev-parse --short HEAD");
    const buildDate = new Date().toISOString();
    return `${gitHash}-${buildDate}`;
};
// → "a3f5d2c-2025-11-30T10:23:45.000Z" (embedded constant)

const config = @comptime {
    return {
        apiUrl: readEnv("NODE_ENV") === "production"
            ? "https://api.prod.com"
            : "http://localhost:3000"
    };
};
// Config embedded in binary - no runtime loading
```

### @ffi - Foreign Function Interface

```typescript
const libc = @comptime bindC("./libc.h");

const fd = libc.open("/tmp/test.txt", libc.O_RDWR);
const buf = new Uint8Array(1024);
libc.read(fd, buf, 1024);
libc.close(fd);
```

---

## Macro Architecture

**Key Decision:** ALL macros are `.ms` source files - no built-in macros in compiler.

```
std/
├── macros/
│   ├── index.ms      # Re-exports
│   ├── derive.ms     # @derive implementation
│   ├── serialize.ms  # @serialize (TODO)
│   └── ffi.ms        # @ffi (TODO)
```

**Why:**
- **Dogfooding:** Macros written IN Metascript
- **Versioning:** Updates ship with releases
- **Transparency:** Users can read macro source
- **Extensibility:** Same mechanism for all macros

---

## Execution Flow

```
@derive(Eq) class User {}
       │
       ↓
┌──────────────────┐    ┌──────────────────┐    ┌──────────────────┐
│ Find derive.ms   │ ─▶ │ Execute via      │ ─▶ │ Insert generated │
│ in std/macros/   │    │ Hermes VM        │    │ AST into class   │
└──────────────────┘    └──────────────────┘    └──────────────────┘
```

---

## Custom Macro Development

### Defining a Macro

```typescript
// macros/my-macro.ms
macro myMacro(ctx: MacroContext) {
    for field in ctx.target.fields {
        quote {
            get${capitalize(field.name)}(): ${field.type} {
                return this.${field.name};
            }
            set${capitalize(field.name)}(value: ${field.type}) {
                this.${field.name} = value;
            }
        }
    }
}
```

### Using the Macro

```typescript
@myMacro
class User {
    name: string;
    age: number;
}
// Auto-generates getName(), setName(), getAge(), setAge()
```

### The `quote` Block

- Code inside `quote { }` becomes AST at compile-time
- `${expr}` interpolates values into the quoted AST
- No manual `ast.createMethod()` calls needed

---

## Macro API

```typescript
interface MacroContext {
    target: TargetInfo;      // The decorated class/function
    args: any[];             // Arguments passed to @macro(...)
}

interface TargetInfo {
    name: string;
    kind: "class" | "function" | "property";
    fields: Field[];
    methods: Method[];
}

// Compile-time I/O
function readFile(path: string): string;
function exec(command: string): string;
function fetch(url: string): Response;
function readEnv(key: string): string;
```

---

## Hygiene

Macros are **hygienic** - no variable capture:

```typescript
macro addHelper() {
    let helper = 42;  // Local to macro expansion
    quote {
        // `helper` here is fresh, doesn't capture outer scope
    }
}
```

---

## Cross-Backend Compatibility

Macros are **backend-agnostic** - they generate AST before backend selection:

```typescript
@derive(Eq, Hash)
class User { name: string; }

// C backend: struct + vtable + equals/hash functions
// JS backend: ES2020 class with equals/hash methods
// Erlang backend: record + module functions
```

---

## Performance

| Metric | Target |
|--------|--------|
| Expansion time | <10% of total build |
| Generated code ratio | 5x acceptable |
| Cache hit rate | >99% |

```bash
# Profile macro overhead
msc compile --profile user.ms
# → Parsing: 5ms, Macro expansion: 15ms, Type checking: 20ms

# View expanded code
msc expand --macro=derive user.ms
```

---

## Best Practices

**1. Keep Focused:**
```typescript
// Good
@derive(Eq)

// Avoid
@derive(Eq, Hash, Clone, Serialize, Validate, Builder, Debug)
```

**2. Readable Generated Code:**
```typescript
// Good
equals(other: User): boolean {
    return this.id === other.id && this.name === other.name;
}

// Avoid
equals(o: any): boolean {
    return Object.keys(this).every(k => this[k] === o[k]);
}
```

**3. Clear Error Messages:**
```
// Good
Error: @derive(Eq) cannot handle field 'metadata' of type Map<string, any>.
Suggestion: Implement equals() manually or use @derive(Eq, ignore: ["metadata"])

// Avoid
Error: Type 'Map' is not equatable
```

---

## Testing Strategy

```zig
test "macro expands @derive(Eq)" {
    const source = "@derive(Eq) class Point { x: number; y: number; }";
    const expanded = expandMacros(parse(source));

    // Check equals method exists
    const point = expanded.classes[0];
    try testing.expect(point.hasMethod("equals"));

    // Check method signature
    const equals = point.getMethod("equals");
    try testing.expectEqual(.boolean, equals.returnType.kind);
}

test "macro firewall: independent expansions" {
    // Editing one macro shouldn't re-expand others
    const db = TransAmDatabase.init();
    db.setFileText("test.ms", "@derive(Eq) class A {}\n@derive(Hash) class B {}");

    const v1 = db.macroExpand("A");
    const v2 = db.macroExpand("B");

    // Edit class A
    db.setFileText("test.ms", "@derive(Eq) class A { x: number; }\n@derive(Hash) class B {}");

    // B should still be cached
    try testing.expect(db.isCacheHit("B"));
}
```

---

## File Location

```
src/macro/
  expander.zig        # Main expansion engine
  builtin_macros.zig  # Temporary Zig fallback
  cache.zig           # Expansion caching

std/macros/
  derive.ms           # @derive implementation
  serialize.ms        # @serialize (TODO)
  ffi.ms              # @ffi (TODO)
```

---

## References

- **Macro system design:** `../macro-system.md`
- **Source-defined macros:** `../source-defined-macros.md`
- **Trans-Am caching:** `./trans-am.md`
- **LSP macro support:** `./lsp.md`
