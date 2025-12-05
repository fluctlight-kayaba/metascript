# Metascript Build System

Declarative build configuration via `build.ms`, inspired by **Vite** and **webpack**.

**Status: Design Document** - Implementation pending.

---

## Philosophy

Metascript targets TypeScript/JavaScript developers. The build system should feel **instantly familiar** - like Vite, webpack, or esbuild configs they already know and love.

| Approach | Example | Target Audience |
|----------|---------|-----------------|
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
        // Built-in transforms
        varHoist(),                          // JS-compatible var hoisting
        optionalChain(),                     // a?.b → safe access
        nullishCoalesce(),                   // a ?? b → null check
        decoratorDesugar({ legacy: true }),  // @decorator → function call

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
    // Dev Server (like Vite!)
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
    // Test (like Vitest!)
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

## Environment-aware Config (like Vite!)

```typescript
// build.ms
import { defineConfig, loadEnv } from "std/build";

export default defineConfig(({ command, mode }) => {
    // Load .env files based on mode
    const env = loadEnv(mode, process.cwd());

    const isDev = mode === "development";
    const isProd = mode === "production";

    return {
        root: "src/main.ms",

        define: {
            __DEV__: isDev.toString(),
            __API_URL__: JSON.stringify(env.API_URL),
            __VERSION__: JSON.stringify(env.npm_package_version),
        },

        build: {
            target: isProd ? "native" : "js",
            minify: isProd,
            sourcemap: isDev,
            optimize: isProd ? "release" : "debug",
        },

        server: isDev ? {
            port: parseInt(env.PORT) || 3000,
            open: true,
        } : undefined,
    };
});
```

### Environment Files

```bash
.env                # Loaded in all cases
.env.local          # Loaded in all cases, ignored by git
.env.development    # Loaded in development mode
.env.production     # Loaded in production mode
.env.[mode].local   # Local overrides for specific mode
```

---

## CLI Commands (Vite-style!)

```bash
# Development
msc dev                          # Start dev server
msc dev --port 3000              # Custom port
msc dev --host 0.0.0.0           # Expose to network
msc dev --open                   # Open browser

# Build
msc build                        # Production build
msc build --target=js            # Override target
msc build --mode=staging         # Use .env.staging
msc build --watch                # Watch mode
msc build --minify=false         # Disable minification

# Preview (serve production build)
msc preview                      # Preview at localhost:4173
msc preview --port 8080

# Test (like Vitest!)
msc test                         # Run tests once
msc test --watch                 # Watch mode
msc test --coverage              # With coverage
msc test --filter="auth"         # Filter by name
msc test --parallel=false        # Sequential

# Other
msc check                        # Type check only
msc lint                         # Lint files
msc format                       # Format files
```

---

## Built-in Transforms

### Import and Use

```typescript
import {
    varHoist,
    optionalChain,
    nullishCoalesce,
    decoratorDesugar,
    asyncTransform,
} from "std/transforms";

export default {
    transforms: [
        varHoist(),
        optionalChain(),
        nullishCoalesce(),
    ],
};
```

### Transform Details

#### `varHoist()`

JavaScript-compatible `var` hoisting for TypeScript compatibility.

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
    var x = undefined;
    var y = undefined;
    console.log(x);
    x = 5;
    if (true) {
        y = 10;
    }
    console.log(y);
}
```

#### `optionalChain()`

Transform optional chaining to safe access.

```typescript
// Input
const name = user?.profile?.name;
const first = arr?.[0];
const result = obj?.method?.();

// Output
const name = user != null
    ? (user.profile != null ? user.profile.name : undefined)
    : undefined;
```

#### `nullishCoalesce()`

Transform nullish coalescing operator.

```typescript
// Input
const value = input ?? defaultValue;

// Output
const value = input != null ? input : defaultValue;
```

#### `decoratorDesugar(options?)`

Transform decorators to function calls.

```typescript
// Options
decoratorDesugar({
    legacy: true,    // Use legacy decorator semantics
})

// Input
@logged
@memoize({ maxSize: 100 })
class Calculator {
    @bound
    add(a: number, b: number) {
        return a + b;
    }
}

// Output
class Calculator {
    add(a: number, b: number) {
        return a + b;
    }
}
Calculator.prototype.add = bound(Calculator.prototype.add);
Calculator = memoize({ maxSize: 100 })(logged(Calculator));
```

#### `asyncTransform()`

Transform async/await to state machine (for C backend).

```typescript
// Input
async function fetchData() {
    const response = await fetch(url);
    const data = await response.json();
    return data;
}

// Output (state machine for C backend)
// ... generates continuation-passing style code
```

---

## Custom Transforms

### Inline Transform

```typescript
export default {
    transforms: [
        {
            name: "add-debug-logs",

            visitor: {
                FunctionDeclaration(node, ctx) {
                    // Add console.log at function entry
                    const logStatement = ctx.createNode("ExpressionStatement", {
                        expression: ctx.createNode("CallExpression", {
                            callee: ctx.createNode("MemberExpression", {
                                object: ctx.createNode("Identifier", { name: "console" }),
                                property: ctx.createNode("Identifier", { name: "log" }),
                            }),
                            arguments: [
                                ctx.createNode("StringLiteral", {
                                    value: `Entering ${node.name}`
                                }),
                            ],
                        }),
                    });

                    node.body.statements.unshift(logStatement);
                    return node;
                },
            },
        },
    ],
};
```

### External Transform Module

```typescript
// transforms/strip-console.ms
import { defineTransform } from "std/transforms";

export default defineTransform({
    name: "strip-console",

    // Optional: run ordering
    runAfter: ["var_hoist"],
    runBefore: ["minify"],

    visitor: {
        CallExpression(node, ctx) {
            // Remove console.* calls
            if (node.callee.type === "MemberExpression" &&
                node.callee.object.name === "console") {
                return ctx.remove();
            }
            return node;
        },
    },
});

// build.ms
import stripConsole from "./transforms/strip-console.ms";

export default {
    transforms: [
        stripConsole(),
    ],
};
```

### Transform Context API

```typescript
interface TransformContext {
    /** Current file path */
    readonly filePath: string;

    /** Create a new AST node */
    createNode(type: string, props: object): Node;

    /** Clone existing node */
    clone(node: Node): Node;

    /** Mark node for removal */
    remove(): RemoveResult;

    /** Replace with multiple nodes */
    expand(nodes: Node[]): ExpandResult;

    /** Report warning (continues) */
    warn(message: string): void;

    /** Report error (stops file) */
    error(message: string): void;

    /** Get parent node */
    parent(): Node | null;

    /** Check if inside specific node type */
    isInside(type: string): boolean;

    /** Store metadata for other transforms */
    setMeta(key: string, value: any): void;

    /** Get metadata from other transforms */
    getMeta(key: string): any;
}
```

---

## Multi-target Builds

```typescript
// build.ms
import { defineConfig } from "std/build";

export default defineConfig({
    root: "src/main.ms",

    build: {
        // Build for multiple targets
        targets: [
            {
                target: "native",
                outDir: "dist/native",
                optimize: "release",
            },
            {
                target: "js",
                outDir: "dist/js",
                minify: true,
                rollupOptions: {
                    output: [
                        { format: "esm", dir: "dist/js/esm" },
                        { format: "cjs", dir: "dist/js/cjs" },
                    ],
                },
            },
            {
                target: "erlang",
                outDir: "dist/beam",
                otpApp: "myapp",
            },
        ],
    },
});
```

---

## Monorepo Support

```typescript
// build.ms (workspace root)
import { defineConfig } from "std/build";

export default defineConfig({
    // Workspace packages
    workspace: [
        "packages/*",
        "apps/*",
    ],

    // Shared configuration for all packages
    shared: {
        resolve: {
            alias: {
                "@myorg/shared": "./packages/shared/src",
                "@myorg/utils": "./packages/utils/src",
            },
        },
        transforms: [
            varHoist(),
            optionalChain(),
        ],
    },
});

// packages/web/build.ms
import { defineConfig } from "std/build";

export default defineConfig({
    root: "src/index.ms",

    build: {
        target: "js",
        outDir: "dist",
    },

    // Package-specific overrides
    resolve: {
        alias: {
            "@": "./src",
        },
    },
});
```

---

## API Reference

### `defineConfig(config)`

Type-safe configuration helper with IntelliSense support.

```typescript
import { defineConfig } from "std/build";

// Static config
export default defineConfig({
    root: "src/main.ms",
});

// Dynamic config
export default defineConfig(({ command, mode }) => ({
    root: "src/main.ms",
    build: {
        minify: mode === "production",
    },
}));
```

### `loadEnv(mode, root, prefix?)`

Load environment variables from `.env` files.

```typescript
import { loadEnv } from "std/build";

const env = loadEnv("production", process.cwd());
// env.API_URL, env.DATABASE_URL, etc.

// With prefix filter (like Vite's VITE_ prefix)
const publicEnv = loadEnv("production", process.cwd(), "PUBLIC_");
// Only variables starting with PUBLIC_
```

### `mergeConfig(base, override)`

Deep merge two configurations.

```typescript
import { defineConfig, mergeConfig } from "std/build";
import baseConfig from "./build.base.ms";

export default mergeConfig(baseConfig, {
    build: {
        minify: true,
    },
});
```

---

## Bootstrap Strategy

`build.ms` runs on Hermes VM. For now, write JavaScript directly:

```
Phase 0 (Now):     build.ms is JavaScript → Hermes runs directly
Phase 1 (Soon):    build.ms is Metascript → msc compiles to .js → Hermes runs
Phase 2 (Future):  build.ms → .js → .hbc (bytecode) → Hermes (fastest)
```

### How It Works

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│  build.ms   │ ──► │   Hermes    │ ──► │ Build Config│
│ (JS for now)│     │   Runtime   │     │   Object    │
└─────────────┘     └─────────────┘     └─────────────┘
                                               │
                                               ▼
                                        ┌─────────────┐
                                        │ msc compile │
                                        │  pipeline   │
                                        └─────────────┘
```

---

## Configuration Schema

```typescript
interface UserConfig {
    /** Entry point file */
    root: string;

    /** Build options */
    build?: {
        target?: "native" | "js" | "erlang" | "wasm";
        outDir?: string;
        outFile?: string;
        minify?: boolean;
        sourcemap?: boolean | "inline" | "hidden";
        optimize?: "debug" | "release" | "release-small";
        rollupOptions?: RollupOptions;
        targets?: BuildTarget[];
    };

    /** Transform plugins */
    transforms?: Transform[];

    /** Module resolution */
    resolve?: {
        alias?: Record<string, string>;
        extensions?: string[];
        mainFields?: string[];
        nodeCompat?: boolean;
    };

    /** Compile-time constants */
    define?: Record<string, string>;

    /** Dev server options */
    server?: {
        port?: number;
        host?: string;
        open?: boolean;
        watch?: WatchOptions;
        hmr?: boolean | HmrOptions;
    };

    /** Test options */
    test?: {
        include?: string[];
        exclude?: string[];
        parallel?: boolean;
        timeout?: number;
        setupFiles?: string[];
        coverage?: CoverageOptions;
    };

    /** Workspace packages (monorepo) */
    workspace?: string[];

    /** Shared config for workspace */
    shared?: Partial<UserConfig>;
}
```

---

## Implementation Plan

### Phase 1: Core (Week 1-2)
- [ ] `msc build` CLI command
- [ ] `build.ms` loader via Hermes
- [ ] Basic config parsing (root, build.target, build.outDir)
- [ ] Integration with existing compile pipeline

### Phase 2: Transforms (Week 3-4)
- [ ] Transform pipeline infrastructure
- [ ] `varHoist()` transform
- [ ] `optionalChain()` transform
- [ ] `nullishCoalesce()` transform
- [ ] Custom transform support

### Phase 3: Resolution & Define (Week 5-6)
- [ ] `resolve.alias` implementation
- [ ] `resolve.extensions` implementation
- [ ] `define` compile-time constants
- [ ] `loadEnv()` function

### Phase 4: Dev Experience (Week 7-8)
- [ ] `msc dev` command
- [ ] File watching
- [ ] `msc test` command
- [ ] Coverage reporting

---

## Comparison with Existing Tools

| Feature | Vite | webpack | esbuild | **Metascript** |
|---------|------|---------|---------|----------------|
| Config style | Declarative | Declarative | Declarative | **Declarative** |
| HMR | ✓ | ✓ | ✗ | Planned |
| Transforms | Plugins | Loaders | Plugins | **Transforms** |
| TypeScript | ✓ | Via loader | ✓ | **Native** |
| Multi-target | ✗ | ✗ | ✗ | **✓ (C/JS/Erlang)** |
| Zero-config | ✓ | ✗ | ✓ | **✓** |

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
