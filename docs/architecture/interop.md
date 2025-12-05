# Interop & Extern System

**Purpose:** Type-safe foreign function interface and multi-backend code emission

---

## Core Concept: `extern`

`extern` = "implementation provided externally (not in Metascript)"

| Declaration | Meaning | Provider |
|-------------|---------|----------|
| `extern macro` | Compiler intrinsic | Compiler |
| `extern function` | FFI function | C library / OS / Runtime |
| `extern class` | Foreign type binding | C struct / JS class / Erlang module |
| `extern const` | Build-time constant | Build system / Environment |

---

## 1. Extern Macro (Compiler Intrinsics)

Compiler-implemented macros with no Metascript body:

```typescript
// std/macros/compiler.ms

/**
 * Conditional compilation for target backends
 * Code inside block only compiled for matching target
 *
 * @target("c") { ... }           // C backend only
 * @target("js") { ... }          // JavaScript backend only
 * @target("erlang") { ... }      // Erlang backend only
 * @target("c", "js") { ... }     // C or JS (multiple targets)
 * @target("c") { ... } else { }  // With fallback
 */
extern macro target(
    ...platforms: ("c" | "js" | "erlang")[]
): void;

/**
 * Raw code emission with $var interpolation
 * Bypasses AST, emits literal string to backend
 *
 * @emit("printf($msg)")              // C code
 * @emit("console.log($x)")           // JS code
 * @emit("io:format(\"~p~n\", [$x])") // Erlang code
 */
extern macro emit(code: string): void;

/**
 * Typed emit - returns a value of specified type
 *
 * const result = @emit<boolean>("confirm($msg)");
 */
extern macro emit<T>(code: string): T;

/**
 * Compile-time execution block
 * Code runs during compilation, result embedded as constant
 */
extern macro comptime<T>(block: () => T): T;

/**
 * Force inline expansion at call sites
 */
extern macro inline(): void;

/**
 * Get size of type in bytes (backend-specific)
 */
extern macro sizeof<T>(): usize;

/**
 * Get alignment of type in bytes (backend-specific)
 */
extern macro alignof<T>(): usize;
```

---

## 2. `@target` with `else` (Conditional Compilation)

**Basic usage:**
```typescript
@target("c") {
    // Only compiled for C backend
}
```

**With else fallback:**
```typescript
@target("c") {
    @emit("printf(\"%s\\n\", $msg)")
} else {
    // Fallback for all other backends (js, erlang)
    console.log(msg);
}
```

**Multiple targets:**
```typescript
@target("c", "js") {
    // Compiled for C or JS
} else {
    // Erlang only
}
```

**Chained conditions:**
```typescript
@target("c") {
    @emit("clock_gettime(CLOCK_MONOTONIC, &ts)")
} else @target("js") {
    @emit("performance.now()")
} else @target("erlang") {
    @emit("erlang:monotonic_time()")
} else {
    // Compile error: unhandled target (safety check)
}
```

**Real-world example:**
```typescript
export function readFileSync(path: string): string {
    @target("c") {
        @emit<string>("ms_read_file_sync($path)")
    } else @target("js") {
        @emit<string>("require('fs').readFileSync($path, 'utf8')")
    } else @target("erlang") {
        @emit<string>("binary_to_list(element(2, file:read_file($path)))")
    }
}
```

**Platform detection at compile-time:**
```typescript
export function getPlatformName(): string {
    @target("c") {
        return "native";
    } else @target("js") {
        return "browser";
    } else {
        return "beam";
    }
}
```

---

## 3. Extern Function (FFI)

### Basic FFI
```typescript
// Linker resolves symbol
extern function printf(format: string, ...args: any[]): i32;
extern function malloc(size: usize): *void;
extern function free(ptr: *void): void;
```

### With Library Hint
```typescript
@library("libcurl")
extern function curl_easy_init(): *CurlHandle;

@library("libssl")
extern function SSL_new(ctx: *SSL_CTX): *SSL;
```

### With Native Name Mapping
```typescript
// When Metascript name differs from native name
@native("__builtin_popcount")
extern function popcount(x: u32): u32;

@native("GetTickCount64")
extern function getTickCount(): u64;
```

### Target-Specific Externs
```typescript
// Only available on C backend
@target("c")
extern function mmap(
    addr: *void,
    length: usize,
    prot: i32,
    flags: i32,
    fd: i32,
    offset: i64
): *void;

// Only available on JS backend
@target("js")
@native("fetch")
extern function jsFetch(url: string, options?: RequestInit): Promise<Response>;

// Only available on Erlang backend
@target("erlang")
@native("gen_server:call")
extern function genServerCall(server: pid, request: any): any;
```

---

## 4. Extern Class (Foreign Types)

### Opaque Types
```typescript
// Layout unknown - treat as pointer
extern class FILE;
extern class SSL_CTX;
extern class CurlHandle;
```

### With Known Fields
```typescript
@include("sys/stat.h")
extern class stat_t {
    st_size: i64;
    st_mode: u32;
    st_mtime: i64;
    st_dev: u64;
    st_ino: u64;
}
```

### JavaScript Externs
```typescript
@target("js")
@native("Promise")
extern class JSPromise<T> {
    constructor(executor: (resolve: (T) => void, reject: (Error) => void) => void);
    then<U>(onFulfilled: (T) => U): JSPromise<U>;
    catch(onRejected: (Error) => void): JSPromise<T>;
    static resolve<T>(value: T): JSPromise<T>;
    static reject(reason: any): JSPromise<never>;
}

@target("js")
@native("Map")
extern class JSMap<K, V> {
    constructor();
    set(key: K, value: V): this;
    get(key: K): V | undefined;
    has(key: K): boolean;
    delete(key: K): boolean;
    clear(): void;
    readonly size: number;
}
```

### Erlang Externs
```typescript
@target("erlang")
@native("gen_server")
extern class GenServer {
    static start_link(module: atom, args: any[], options?: any[]): {ok: pid} | {error: any};
    static call(server: pid | atom, request: any, timeout?: number): any;
    static cast(server: pid | atom, request: any): void;
    static reply(from: any, reply: any): void;
}

@target("erlang")
@native("ets")
extern class ETS {
    static new(name: atom, options: any[]): ets_table;
    static insert(table: ets_table, object: any): boolean;
    static lookup(table: ets_table, key: any): any[];
    static delete(table: ets_table): boolean;
}
```

### C Struct Mapping
```typescript
@target("c")
@include("time.h")
extern class timespec {
    tv_sec: i64;   // seconds
    tv_nsec: i64;  // nanoseconds
}

@target("c")
@include("netinet/in.h")
extern class sockaddr_in {
    sin_family: u16;
    sin_port: u16;
    sin_addr: in_addr;
}
```

---

## 5. Extern Const (Build-Time Constants)

```typescript
// Injected by build system
extern const BUILD_VERSION: string;
extern const BUILD_TIMESTAMP: string;
extern const GIT_COMMIT: string;

// Compiler flags (-D debug, -D release)
extern const DEBUG: boolean;
extern const RELEASE: boolean;

// Target information
extern const TARGET: "c" | "js" | "erlang";
extern const TARGET_OS: "linux" | "macos" | "windows" | "browser" | "beam";
extern const TARGET_ARCH: "x86_64" | "aarch64" | "wasm32";

// Usage
if (DEBUG) {
    console.log("Debug build:", BUILD_VERSION);
}
```

---

## 6. `@emit` Interpolation Syntax

### Basic Interpolation
```typescript
function greet(name: string): void {
    @target("c") {
        @emit("printf(\"Hello, %s!\\n\", $name);")
    }
    @target("js") {
        @emit("console.log(`Hello, ${$name}!`);")
    }
}
```

### Multiple Variables
```typescript
function add(a: i32, b: i32): i32 {
    @target("c") {
        return @emit<i32>("($a + $b)");
    }
}
```

### Complex Expressions
```typescript
function processArray(arr: number[], len: i32): void {
    @target("c") {
        @emit("for (int i = 0; i < $len; i++) { process($arr[i]); }")
    }
}
```

### Type-Safe Returns
```typescript
function confirm(msg: string): boolean {
    @target("js") {
        return @emit<boolean>("window.confirm($msg)");
    } else {
        // Non-browser fallback
        console.log(msg, "[y/n]");
        return true;
    }
}
```

---

## 7. Complete FFI Example

```typescript
// bindings/sqlite.ms - SQLite FFI bindings

@library("libsqlite3")
@include("sqlite3.h")
module sqlite;

// Opaque handle types
extern class sqlite3;
extern class sqlite3_stmt;

// Constants
export const SQLITE_OK = 0;
export const SQLITE_ROW = 100;
export const SQLITE_DONE = 101;

// Core functions
@native("sqlite3_open")
extern function open(filename: string, db: **sqlite3): i32;

@native("sqlite3_close")
extern function close(db: *sqlite3): i32;

@native("sqlite3_exec")
extern function exec(
    db: *sqlite3,
    sql: string,
    callback: ?(*void, i32, **i8, **i8) => i32,
    arg: *void,
    errmsg: **i8
): i32;

@native("sqlite3_prepare_v2")
extern function prepare(
    db: *sqlite3,
    sql: string,
    nBytes: i32,
    stmt: **sqlite3_stmt,
    tail: **i8
): i32;

@native("sqlite3_step")
extern function step(stmt: *sqlite3_stmt): i32;

@native("sqlite3_finalize")
extern function finalize(stmt: *sqlite3_stmt): i32;

@native("sqlite3_column_text")
extern function columnText(stmt: *sqlite3_stmt, col: i32): string;

@native("sqlite3_column_int")
extern function columnInt(stmt: *sqlite3_stmt, col: i32): i32;

// High-level wrapper
export class Database {
    private db: *sqlite3;

    constructor(path: string) {
        let dbPtr: *sqlite3 = null;
        const result = sqlite.open(path, &dbPtr);
        if (result != SQLITE_OK) {
            throw new Error("Failed to open database");
        }
        this.db = dbPtr;
    }

    execute(sql: string): void {
        let errmsg: *i8 = null;
        const result = sqlite.exec(this.db, sql, null, null, &errmsg);
        if (result != SQLITE_OK) {
            throw new Error(`SQL error: ${errmsg}`);
        }
    }

    close(): void {
        sqlite.close(this.db);
    }
}
```

---

## 8. Design Principles

### Why `extern` over `declare`?
- **`extern`** implies "defined elsewhere, linked later" (C heritage)
- **`declare`** implies "just type info" (TypeScript heritage)
- We chose `extern` because it covers FFI + compiler intrinsics uniformly

### Why `@target` macro over `#if` preprocessor?
- **Macro approach:** AST-level, IDE-aware, type-checked
- **Preprocessor:** Text-level, invisible to IDE, no type checking
- Macros integrate with LSP (hover, completion, go-to-definition)

### Why `$var` over `{0}` placeholders?
- **`$var`:** Readable, matches template string familiarity
- **`{0}`:** Positional, error-prone, harder to read
- Named interpolation is self-documenting

### Comparison with Haxe

| Aspect | Haxe | Metascript |
|--------|------|------------|
| Conditional | `#if js ... #end` | `@target("js") { } else { }` |
| Code injection | `js.Syntax.code("{0}", x)` | `@emit("code $x")` |
| Extern keyword | `extern class Math` | `extern class`, `extern function` |
| Native rename | `@:native("name")` | `@native("name")` |
| Multi-target | Separate packages per target | Single file with `@target` blocks |

---

## 9. Implementation Status

| Feature | Status | Notes |
|---------|--------|-------|
| `extern` keyword | ✓ Done | Lexer token added |
| `extern macro` | ✓ Grammar | Tree-sitter parses, codegen TODO |
| `extern function` | ✓ Grammar | Tree-sitter parses, codegen TODO |
| `extern class` | ✓ Grammar | Tree-sitter parses, codegen TODO |
| `extern const` | ✓ Grammar | Tree-sitter parses, codegen TODO |
| `@target` | ✓ Grammar | Tree-sitter parses, LSP hover works |
| `@target else` | ✓ Grammar | Chained else supported |
| `@emit` | ✓ Grammar | Tree-sitter parses, LSP hover works |
| `@emit<T>` | ✓ Grammar | Type parameters supported |
| LSP hover | ✓ Done | All built-in macros documented |
| `@native` | TODO | Name mapping decorator |
| `@library` | TODO | Library hints decorator |
| `@include` | TODO | C header includes decorator |
| Codegen (C) | TODO | Actual code generation |
| Codegen (JS) | TODO | Actual code generation |
| Codegen (Erlang) | TODO | Actual code generation |

---

## 10. Tree-sitter & LSP Integration

### Tree-sitter Grammar
```javascript
// No special keywords for @target, @emit - just macro invocations
macro_invocation: $ => seq(
    '@',
    $.identifier,  // target, emit, native, etc.
    optional($.type_arguments),
    optional($.arguments),
    optional($.block),
    optional(seq('else', choice($.block, $.macro_invocation)))
),
```

### LSP Hover
All extern macros defined in `std/macros/compiler.ms` with JSDoc comments.
LSP reads these for hover info - no special cases needed.

---

## References

- **Haxe externs:** https://haxe.org/manual/lf-externs.html
- **Haxe conditional compilation:** https://haxe.org/manual/lf-condition-compilation.html
- **Haxe target syntax:** https://haxe.org/manual/target-syntax.html
- **Nim FFI:** https://nim-lang.org/docs/manual.html#foreign-function-interface
