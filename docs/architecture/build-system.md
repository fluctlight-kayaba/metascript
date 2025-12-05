# Metascript Build System

Declarative build configuration via `build.ms`, inspired by **Vite** and **webpack**.

---

## Implementation Status

> **Last Updated:** 2025-12-06

| Component | Status | Notes |
|-----------|--------|-------|
| **Core Infrastructure** | | |
| `msc build` CLI | ✅ Implemented | `src/cli/build.zig` |
| `build.ms` loader | ✅ Implemented | `src/build/loader.zig` - JS object → JSON parsing |
| Config schema | ✅ Implemented | `src/build/config.zig` - Full type definitions |
| CLI argument parsing | ✅ Implemented | `--target`, `--mode`, `--watch`, `--minify`, `--sourcemap` |
| **Transforms** | | |
| Transform pipeline | ✅ Implemented | `src/transform/pipeline.zig` - 900 lines |
| Topological sort | ✅ Implemented | Kahn's algorithm with cycle detection |
| `varHoist()` | ✅ Implemented | `src/transform/builtin/var_hoist.zig` - 8 tests |
| `nullishCoalesce()` | ✅ Implemented | `src/transform/builtin/nullish_coalesce.zig` - 9 tests |
| `optionalChain()` | 🚧 Placeholder | Parser needs `?.` support first |
| `decoratorDesugar()` | ❌ Not started | |
| `asyncTransform()` | ❌ Not started | |
| Custom transforms | 🚧 Partial | Infrastructure ready, external loading pending |
| **Module Resolution** | | |
| `resolve.alias` | ✅ Implemented | Config parsing ready |
| `resolve.extensions` | ✅ Implemented | `.ms`, `.ts`, `.js` defaults |
| `resolve.nodeCompat` | ✅ Implemented | Config flag ready |
| **Define** | | |
| Compile-time constants | ✅ Implemented | Config parsing ready |
| `.env` file loading | ❌ Not started | `loadEnv()` function |
| **Dev Server** | | |
| `msc dev` command | ❌ Not started | |
| File watching | 🚧 Skeleton | `--watch` flag parsed, logic pending |
| HMR | ❌ Not started | |
| **Testing** | | |
| `msc test` command | ❌ Not started | |
| Coverage | ❌ Not started | |
| **Multi-target** | | |
| `build.targets[]` | ✅ Implemented | Config + iteration logic |

### Test Coverage

| Component | Tests | Coverage |
|-----------|-------|----------|
| `pipeline.zig` | 8 | ~80% |
| `var_hoist.zig` | 8 | ~85% |
| `nullish_coalesce.zig` | 9 | ~85% |
| `config.zig` | 4 | ~70% |
| **Total** | **29** | **~80%** |

---

## Philosophy

Metascript targets TypeScript/JavaScript developers. The build system should feel **instantly familiar** - like Vite, webpack, or esbuild configs they already know and love.

| Approach | Example | Target Audience |
|----------|---------|--------------------|
| Zig-style | `b.addExecutable({...})` | Systems programmers |
| **Vite-style** | `export default { build: {...} }` | **Web developers** ✓ |

---

## Quick Start

```javascript
// build.ms - Feels like vite.config.ts!

export default {
    root: "src/main.ms",

    build: {
        target: "native",
        outDir: "dist",
    },
};
```

That's it. Run `msc build` and you're done.

---

## Full Configuration

```typescript
// build.ms
import { defineConfig } from "std/build";
import { varHoist, optionalChain, nullishCoalesce, decoratorDesugar } from "std/transforms";

export default defineConfig({
    // =========================================================================
    // Entry & Output
    // =========================================================================

    /** Entry point (required) */
    root: "src/main.ms",

    /** Build configuration */
    build: {
        /** Target backend: "native" | "js" | "erlang" | "wasm" */
        target: "native",

        /** Output directory */
        outDir: "dist",

        /** Output filename (without extension) */
        outFile: "myapp",

        /** Minify output (JS target only) */
        minify: true,

        /** Generate source maps */
        sourcemap: true,

        /** Optimization level: "debug" | "release" | "release-small" */
        optimize: "release",

        /** Multi-format output (JS target) */
        rollupOptions: {
            output: [
                { format: "esm", dir: "dist/esm" },
                { format: "cjs", dir: "dist/cjs" },
                { format: "iife", dir: "dist/browser", name: "MyApp" },
            ],
        },
    },

    // =========================================================================
    // Transforms (like Vite plugins!)
    // =========================================================================

    transforms: [
        // Built-in transforms (✅ = implemented, 🚧 = in progress)
        varHoist(),                          // ✅ JS-compatible var hoisting
        nullishCoalesce(),                   // ✅ a ?? b → null check
        optionalChain(),                     // 🚧 a?.b → safe access (placeholder)
        decoratorDesugar({ legacy: true }),  // ❌ @decorator → function call

        // Custom inline transform
        {
            name: "my-transform",
            visitor: {
                FunctionDeclaration(node, ctx) {
                    // Transform logic
                    return node;
                },
            },
        },

        // External transform
        myCustomTransform({ option: true }),
    ],

    // =========================================================================
    // Module Resolution (like webpack!)
    // =========================================================================

    resolve: {
        /** Path aliases */
        alias: {
            "@": "./src",
            "@components": "./src/components",
            "@utils": "./src/utils",
            "@lib": "./src/lib",
            "lodash": "lodash-es",            // Swap modules
        },

        /** File extensions to try */
        extensions: [".ms", ".ts", ".js"],

        /** Package.json fields to check */
        mainFields: ["module", "main"],

        /** Node.js built-in aliases */
        nodeCompat: true,                     // fs→std/fs, path→std/path, etc.
    },

    // =========================================================================
    // Define (compile-time constants, like Vite/esbuild!)
    // =========================================================================

    define: {
        __DEV__: "true",
        __VERSION__: '"1.0.0"',
        "process.env.NODE_ENV": '"production"',
        __BUILD_TIME__: Date.now().toString(),
    },

    // =========================================================================
    // Dev Server (like Vite!) - NOT YET IMPLEMENTED
    // =========================================================================

    server: {
        /** Port number */
        port: 3000,

        /** Open browser on start */
        open: true,

        /** Host to bind */
        host: "localhost",

        /** File watching */
        watch: {
            include: ["src/**/*.ms"],
            exclude: ["node_modules", "dist"],
        },

        /** Hot Module Replacement (JS target) */
        hmr: true,
    },

    // =========================================================================
    // Test (like Vitest!) - NOT YET IMPLEMENTED
    // =========================================================================

    test: {
        /** Test file patterns */
        include: ["tests/**/*.test.ms", "src/**/*.test.ms"],

        /** Files to exclude */
        exclude: ["tests/fixtures/**"],

        /** Run tests in parallel */
        parallel: true,

        /** Timeout per test (ms) */
        timeout: 5000,

        /** Setup files to run before tests */
        setupFiles: ["tests/setup.ms"],

        /** Coverage configuration */
        coverage: {
            enabled: true,
            include: ["src/**/*.ms"],
            exclude: ["src/**/*.test.ms"],
            reporter: ["text", "html", "lcov"],
            thresholds: {
                lines: 80,
                functions: 80,
                branches: 70,
            },
        },
    },
});
```

---

## CLI Commands

```bash
# Build (✅ IMPLEMENTED)
msc build                        # Production build
msc build --target=js            # Override target
msc build --mode=production      # Set mode
msc build --watch                # Watch mode (skeleton only)
msc build --minify               # Enable minification
msc build --no-minify            # Disable minification
msc build --sourcemap            # Enable source maps
msc build --verbose              # Verbose output

# Compile (✅ IMPLEMENTED - legacy command)
msc compile main.ms              # Direct compilation
msc compile --target=c main.ms   # C backend
msc compile --target=js main.ms  # JS backend

# Development (❌ NOT IMPLEMENTED)
msc dev                          # Start dev server
msc dev --port 3000              # Custom port
msc dev --host 0.0.0.0           # Expose to network
msc dev --open                   # Open browser

# Preview (❌ NOT IMPLEMENTED)
msc preview                      # Preview at localhost:4173
msc preview --port 8080

# Test (❌ NOT IMPLEMENTED)
msc test                         # Run tests once
msc test --watch                 # Watch mode
msc test --coverage              # With coverage
msc test --filter="auth"         # Filter by name
msc test --parallel=false        # Sequential

# Other (✅ IMPLEMENTED)
msc check                        # Type check only
msc lsp                          # Start LSP server
```

---

## Built-in Transforms

### Import and Use

```typescript
import {
    varHoist,
    nullishCoalesce,
    optionalChain,      // 🚧 placeholder
    decoratorDesugar,   // ❌ not implemented
    asyncTransform,     // ❌ not implemented
} from "std/transforms";

export default {
    transforms: [
        varHoist(),
        nullishCoalesce(),
    ],
};
```

### Transform Details

#### `varHoist()` - ✅ IMPLEMENTED

JavaScript-compatible `var` hoisting for TypeScript compatibility.

**Implementation:** `src/transform/builtin/var_hoist.zig` (784 lines, 8 tests)

**Features:**
- Hoists `var` declarations to function scope top
- Deduplicates multiple declarations of same variable
- Preserves `let`/`const` declarations unchanged
- Handles nested blocks, if/while/for statements
- Respects function scope boundaries (nested functions have own scope)
- Converts `for (var i = 0; ...)` initializers correctly

```typescript
// Input
function example() {
    console.log(x);  // undefined, not error
    var x = 5;
    if (true) {
        var y = 10;  // Hoisted to function scope
    }
    console.log(y);  // Works
}

// Output (after transform)
function example() {
    var x;           // hoisted declaration (deduplicated)
    var y;           // hoisted declaration
    console.log(x);
    x = 5;           // assignment stays in place
    if (true) {
        y = 10;
    }
    console.log(y);
}
```

#### `nullishCoalesce()` - ✅ IMPLEMENTED

Transform nullish coalescing operator for backends that don't support it natively.

**Implementation:** `src/transform/builtin/nullish_coalesce.zig` (412 lines, 9 tests)

**Features:**
- Transforms `a ?? b` to conditional expression
- Detects "simple" expressions (identifiers, literals, member access)
- Avoids triple evaluation for simple expressions
- Deep clones AST nodes to prevent aliasing bugs

**Known Limitation:** Complex expressions (function calls) may be evaluated multiple times. IIFE wrapper not yet implemented.

```typescript
// Input
const value = input ?? defaultValue;

// Output (simple case)
const value = input !== null && input !== undefined ? input : defaultValue;

// For complex expressions (TODO: implement IIFE wrapper)
// getValue() ?? "default"
// Currently evaluates getValue() up to 3 times!
```

#### `optionalChain()` - 🚧 PLACEHOLDER

Transform optional chaining to safe null checks.

**Implementation:** `src/transform/builtin/optional_chain.zig` (73 lines, placeholder)

**Status:** Parser needs to support `?.` syntax first. Transform infrastructure is ready.

```typescript
// Input (when parser supports ?.)
const name = user?.profile?.name;
const first = arr?.[0];
const result = obj?.method?.();

// Output (planned)
const name = user != null
    ? (user.profile != null ? user.profile.name : undefined)
    : undefined;
```

#### `decoratorDesugar(options?)` - ❌ NOT IMPLEMENTED

Transform decorators to function calls.

#### `asyncTransform()` - ❌ NOT IMPLEMENTED

Transform async/await to state machine (for C backend).

---

## Transform Pipeline Architecture

**Implementation:** `src/transform/pipeline.zig` (900 lines, 8 tests)

### Pipeline Structure

```
┌─────────────────────────────────────────────────────────────────┐
│                      Transform Pipeline                          │
├─────────────────────────────────────────────────────────────────┤
│  Pipeline                                                        │
│  ├── transforms: ArrayList(Transform)                           │
│  ├── builtin_registry: StringHashMap(TransformFn)               │
│  └── sortTransforms() → Kahn's algorithm (topological sort)     │
├─────────────────────────────────────────────────────────────────┤
│  TransformContext                                                │
│  ├── arena: *ASTArena (for creating new nodes)                  │
│  ├── allocator: Allocator (for temp work)                       │
│  ├── file_path: []const u8                                      │
│  ├── options: ?std.json.Value (from build.ms)                   │
│  ├── stats: Stats { nodes_visited, nodes_transformed, ... }     │
│  └── cloneNode() → deep clone for safe transformations          │
├─────────────────────────────────────────────────────────────────┤
│  walkAndTransform()                                              │
│  └── Visits 30+ node types:                                      │
│      program, block_stmt, function_decl, function_expr,         │
│      if_stmt, while_stmt, for_stmt, binary_expr, unary_expr,    │
│      call_expr, member_expr, return_stmt, expression_stmt,      │
│      variable_stmt, array_expr, object_expr, conditional_expr,  │
│      class_decl, method_decl, constructor_decl, new_expr,       │
│      spread_element, move_expr, import_decl, export_decl,       │
│      property_decl, macro_decl, macro_invocation,               │
│      comptime_block, quote_expr, and leaf nodes                 │
└─────────────────────────────────────────────────────────────────┘
```

### Dependency Resolution

Transforms can specify ordering constraints:

```typescript
// build.ms
transforms: [
    {
        name: "my-transform",
        runAfter: ["var_hoist"],      // Run after var_hoist
        runBefore: ["nullish_coalesce"], // Run before nullish_coalesce
    },
]
```

**Algorithm:** Kahn's topological sort with O(V+E) complexity and cycle detection.

```zig
// From pipeline.zig:227-334
pub fn sortTransforms(self: *Pipeline) !void {
    // Build adjacency list from run_after/run_before constraints
    // Run Kahn's algorithm (BFS from nodes with in_degree = 0)
    // Detect cycles: if sorted_indices.len != n, cycle exists
}
```

### Deep Clone Implementation

The `cloneNode()` function provides safe deep copying:

```zig
// Supported node types for deep clone:
.number_literal, .string_literal, .boolean_literal,
.null_literal, .identifier,
.binary_expr, .unary_expr, .member_expr,
.call_expr, .conditional_expr

// Other types fall back to shallow copy (TODO: expand)
```

---

## Build Configuration Schema

**Implementation:** `src/build/config.zig` (468 lines, 4 tests)

```zig
pub const BuildConfig = struct {
    root: []const u8,                    // Entry point
    build: BuildSection,                 // Build options
    transforms: []const TransformConfig, // Transform plugins
    resolve: ResolveSection,             // Module resolution
    define: StringHashMap([]const u8),   // Compile-time constants
    server: ServerSection,               // Dev server (not implemented)
    test_config: TestSection,            // Testing (not implemented)
    workspace: []const []const u8,       // Monorepo packages
};

pub const BuildSection = struct {
    target: Target = .native,            // native | js | erlang | wasm
    out_dir: []const u8 = "dist",
    out_file: ?[]const u8 = null,
    optimize: Optimize = .debug,         // debug | release | release-small | release-safe
    minify: bool = false,
    sourcemap: bool = false,
    targets: []const TargetConfig = &.{}, // Multi-target support
    rollup_outputs: []const RollupOutput = &.{},
};

pub const TransformConfig = struct {
    name: []const u8,
    builtin: bool = true,
    path: ?[]const u8 = null,            // For external transforms
    run_after: []const []const u8 = &.{},
    run_before: []const []const u8 = &.{},
    options: ?std.json.Value = null,
};
```

---

## Bootstrap Strategy

`build.ms` runs on Hermes VM. For now, write JavaScript directly:

```
Phase 0 (Current):  build.ms is JavaScript → Simple parser extracts config
Phase 1 (Soon):     build.ms is JavaScript → Hermes runs directly
Phase 2 (Future):   build.ms is Metascript → msc compiles to .js → Hermes runs
Phase 3 (Later):    build.ms → .js → .hbc (bytecode) → Hermes (fastest)
```

### Current Loader Implementation

**File:** `src/build/loader.zig`

```zig
pub fn loadBuildMs(allocator, build_ms_path, cli_options) !BuildConfig {
    // 1. Read build.ms file
    // 2. Try simple JS object literal parsing (no Hermes needed)
    // 3. Fall back to Hermes execution if complex
    // 4. Parse JSON output into BuildConfig
}

fn tryParseSimpleConfig(allocator, source, options) !BuildConfig {
    // Find "export default { ... }"
    // Convert JS object literal to JSON (handle unquoted keys, single quotes)
    // Parse JSON into BuildConfig
}
```

---

## Implementation Plan

### Phase 1: Core - ✅ COMPLETE
- [x] `msc build` CLI command
- [x] `build.ms` loader (simple parser)
- [x] Basic config parsing (root, build.target, build.outDir)
- [x] Integration with compile pipeline

### Phase 2: Transforms - ✅ MOSTLY COMPLETE
- [x] Transform pipeline infrastructure
- [x] Topological sort with cycle detection
- [x] Deep clone for AST nodes
- [x] `varHoist()` transform (8 tests)
- [x] `nullishCoalesce()` transform (9 tests)
- [ ] `optionalChain()` transform (needs parser support)
- [ ] Custom external transform loading

### Phase 3: Resolution & Define - 🚧 PARTIAL
- [x] `resolve.alias` config parsing
- [x] `resolve.extensions` config parsing
- [x] `define` config parsing
- [ ] Actually apply aliases during module resolution
- [ ] Actually replace defined constants during compilation
- [ ] `loadEnv()` function

### Phase 4: Dev Experience - ❌ NOT STARTED
- [ ] `msc dev` command
- [ ] File watching implementation
- [ ] `msc test` command
- [ ] Coverage reporting
- [ ] HMR

---

## File Structure

```
src/
├── build/
│   ├── config.zig      # BuildConfig schema + JSON parsing (468 lines)
│   └── loader.zig      # build.ms loading + JS→JSON conversion (200+ lines)
├── transform/
│   ├── pipeline.zig    # Transform pipeline + walker (900 lines)
│   └── builtin/
│       ├── var_hoist.zig        # Var hoisting (784 lines)
│       ├── nullish_coalesce.zig # ?? operator (412 lines)
│       └── optional_chain.zig   # ?. operator (73 lines, placeholder)
└── cli/
    ├── build.zig       # `msc build` command (150+ lines)
    └── compile.zig     # `msc compile` command (748 lines)
```

---

## Comparison with Existing Tools

| Feature | Vite | webpack | esbuild | **Metascript** |
|---------|------|---------|---------|----------------|
| Config style | Declarative | Declarative | Declarative | **Declarative** ✅ |
| HMR | ✓ | ✓ | ✗ | ❌ Not yet |
| Transforms | Plugins | Loaders | Plugins | **Transforms** ✅ |
| TypeScript | ✓ | Via loader | ✓ | **Native** ✅ |
| Multi-target | ✗ | ✗ | ✗ | **✓ (C/JS/Erlang)** ✅ |
| Zero-config | ✓ | ✗ | ✓ | **✓** ✅ |
| Transform dependencies | ✗ | ✗ | ✗ | **✓ (topological sort)** ✅ |

---

## Known Issues & TODOs

### High Priority
1. **IIFE wrapper for nullish_coalesce** - Complex expressions evaluated multiple times
2. **Optional chain parser support** - Need `?.` token and AST node

### Medium Priority
3. **Deep clone expansion** - Add array_expr, object_expr, function_expr
4. **External transform loading** - Load transforms from file paths
5. **Actually apply resolve.alias** - Currently only parsed, not used
6. **Actually replace define constants** - Currently only parsed, not used

### Low Priority
7. **Full Hermes integration** - Currently using simple JS parser
8. **Dev server** - `msc dev` with file watching and HMR
9. **Test runner** - `msc test` with coverage

---

## Migration from Other Tools

### From Vite

```typescript
// vite.config.ts
export default {
    resolve: { alias: { "@": "./src" } },
    define: { __DEV__: "true" },
    build: { minify: true },
};

// build.ms (almost identical!)
export default {
    root: "src/main.ms",  // Add entry point
    resolve: { alias: { "@": "./src" } },
    define: { __DEV__: "true" },
    build: { target: "js", minify: true },
};
```

### From webpack

```javascript
// webpack.config.js
module.exports = {
    entry: "./src/index.js",
    resolve: { alias: { "@": path.resolve("./src") } },
};

// build.ms
export default {
    root: "src/index.ms",
    resolve: { alias: { "@": "./src" } },
};
```
