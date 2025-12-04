# Module System

**Purpose:** Import/export resolution and module loading

**Features:** Resolution caching, macro collection, cross-module imports

---

## Overview

| Component | Purpose |
|-----------|---------|
| **ModuleResolver** | Map specifiers to file paths |
| **ModuleLoader** | Parse and cache modules |
| **Module** | Loaded module with exports |

---

## Architecture

```
import { derive } from "std/macros"
         ↓
┌──────────────────────┐
│   ModuleResolver     │
│   "std/macros" →     │
│   /path/std/macros/  │
│   index.ms           │
└──────────────────────┘
         ↓
┌──────────────────────┐
│   ModuleLoader       │
│   - Read file        │
│   - Lexer → Parser   │
│   - Cache module     │
│   - Collect macros   │
└──────────────────────┘
         ↓
┌──────────────────────┐
│   Module             │
│   - path             │
│   - ast              │
│   - macros           │
│   - exports          │
└──────────────────────┘
```

---

## ModuleResolver

Resolves import specifiers to absolute file paths:

```zig
pub const ModuleResolver = struct {
    allocator: std.mem.Allocator,
    std_lib_path: []const u8,
    resolved_cache: std.StringHashMap([]const u8),

    pub fn resolve(
        self: *ModuleResolver,
        specifier: []const u8,  // "std/macros", "./foo"
        from_file: []const u8,  // Importing file path
    ) !?[]const u8;
};
```

### Resolution Rules

| Specifier | Resolution | Example |
|-----------|------------|---------|
| `std/*` | Standard library | `std/macros` → `<std>/macros/index.ms` |
| `./` | Relative to file | `./foo` → `<dir>/foo.ms` |
| `../` | Parent directory | `../bar` → `<parent>/bar.ms` |
| `package` | node_modules (future) | `lodash` → `node_modules/lodash/index.ms` |

### Standard Library Path

```zig
pub fn getStdLibPath(allocator: Allocator) ![]const u8 {
    // Priority:
    // 1. MSC_STD_PATH environment variable
    // 2. Relative to executable: ../std/
    // 3. Current working directory: ./std/
}
```

### Caching

```zig
// Cache key: "from_file:specifier"
// Example: "/src/app.ms:std/macros"

if (self.resolved_cache.get(cache_key)) |cached| {
    return cached;  // Fast path
}
```

---

## ModuleLoader

Loads, parses, and caches modules:

```zig
pub const ModuleLoader = struct {
    allocator: std.mem.Allocator,
    arena: *ast.ASTArena,
    resolver: ModuleResolver,
    modules: std.StringHashMap(*Module),
    macro_registry: SourceMacroRegistry,

    pub fn loadModule(self: *ModuleLoader, path: []const u8) !*Module;
    pub fn loadStdMacros(self: *ModuleLoader) !void;
    pub fn findMacro(self: *ModuleLoader, name: []const u8) ?*ast.Node;
};
```

### Load Flow

```
1. Check cache → return if hit
2. Read file contents
3. Lexer.init(source)
4. Parser.parse() → AST
5. Collect macros from AST
6. Collect exports
7. Cache module
8. Return module
```

### Usage

```zig
var loader = try ModuleLoader.init(allocator, arena);
defer loader.deinit();

// Load standard library macros
try loader.loadStdMacros();

// Load user module
const module = try loader.loadModule("./src/app.ms");

// Find macro for expansion
if (loader.findMacro("derive")) |macro| {
    // Use macro node
}
```

---

## Module

Represents a loaded module:

```zig
pub const Module = struct {
    path: []const u8,
    ast: *ast.Node,
    macros: std.StringHashMap(*ast.Node),
    exports: std.StringHashMap(ExportedSymbol),
    imports: std.ArrayList(ImportDecl),

    pub const ExportedSymbol = struct {
        name: []const u8,
        node: *ast.Node,
        kind: SymbolKind,
    };

    pub const SymbolKind = enum {
        macro,
        function,
        class,
        type_alias,
        variable,
    };

    pub const ImportDecl = struct {
        specifier: []const u8,
        resolved_path: ?[]const u8,
        symbols: []const []const u8,
    };
};
```

---

## Import/Export Syntax

### Named Imports

```typescript
import { derive, comptime } from "std/macros";
import { User, Admin } from "./models";
```

### Default Import

```typescript
import config from "./config";
```

### Namespace Import

```typescript
import * as utils from "./utils";
```

### Named Exports

```typescript
export { User, Admin };
export function helper() { }
export class Service { }
export type ID = number;
```

### Default Export

```typescript
export default class App { }
```

### Re-exports

```typescript
export { derive } from "std/macros";
export * from "./utils";
```

---

## Macro Collection

When loading a module, macros are collected into a registry:

```zig
fn collectMacros(self: *ModuleLoader, module: *Module) !void {
    for (module.ast.data.program.statements) |stmt| {
        if (stmt.kind == .macro_decl) {
            const name = stmt.data.macro_decl.name;
            try module.macros.put(name, stmt);
            try self.macro_registry.register(name, stmt);
        }
    }
}
```

### Source-Defined Macros

```typescript
// std/macros/index.ms
macro derive(ctx) {
    // Macro implementation
}

export { derive };
```

---

## Integration Points

### Parser

```
import { derive } from "std/macros";
         ↓
ImportDecl {
    specifier: "std/macros",
    symbols: ["derive"],
}
```

### Macro Expander

```zig
// Find macro before expansion
if (loader.findMacro("derive")) |macro_node| {
    vm.executeMacro(macro_node, target);
}
```

### LSP

```
Hover on "std/macros" → Show resolved path
Go-to-definition → Jump to macro source
Autocomplete → List exported symbols
```

---

## Dependency Graph

```
app.ms
  ├── import "std/macros"
  │     └── derive, comptime
  ├── import "./models"
  │     ├── import "./user"
  │     └── import "./admin"
  └── import "./utils"
```

### Cycle Detection

```zig
fn loadModuleRecursive(self: *ModuleLoader, path: []const u8, loading: *Set) !*Module {
    if (loading.contains(path)) {
        return error.CircularDependency;
    }
    try loading.put(path, {});
    defer _ = loading.remove(path);

    // Load and process imports
}
```

---

## File Structure

```
src/module/
  module.zig       # Re-exports, public API
  resolver.zig     # Path resolution
  loader.zig       # Module loading, caching
```

---

## Testing

```bash
zig build test -- --test-filter="module"
```

```zig
test "resolve std library path" {
    var resolver = ModuleResolver.init(allocator, "/opt/metascript/std");
    defer resolver.deinit();

    const path = try resolver.resolve("std/macros", "/src/app.ms");

    try testing.expectEqualStrings("/opt/metascript/std/macros/index.ms", path.?);
}

test "resolve relative import" {
    var resolver = ModuleResolver.init(allocator, "/opt/metascript/std");
    defer resolver.deinit();

    const path = try resolver.resolve("./models", "/src/app.ms");

    try testing.expectEqualStrings("/src/models.ms", path.?);
}

test "module loader caches modules" {
    var loader = try ModuleLoader.init(allocator, arena);
    defer loader.deinit();

    const mod1 = try loader.loadModule("/src/app.ms");
    const mod2 = try loader.loadModule("/src/app.ms");

    try testing.expectEqual(mod1, mod2);  // Same pointer (cached)
}
```

---

## Future Work

- **Package resolution:** node_modules-style lookup
- **Package.json:** Read "main" field for entry points
- **Version resolution:** Semantic versioning support
- **Bundling:** Combine modules into single output
- **Tree shaking:** Remove unused exports

---

## References

- **Parser imports:** `./parser.md`
- **Macro system:** `./macros.md`
- **LSP integration:** `./lsp.md`
