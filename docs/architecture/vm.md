# Comptime VM (MacroVM)

**Purpose:** Execute macros and `@comptime` blocks during compilation

**Runtime:** Hermes JavaScript engine (Meta's React Native JS engine)

---

## Overview

| Aspect | Implementation |
|--------|----------------|
| **Engine** | Hermes (AOT bytecode, fast startup) |
| **Language** | JavaScript/TypeScript |
| **AST Access** | Zero-copy via JSI HostObjects |
| **Caching** | Bytecode cache for ~225x speedup |

---

## Architecture

```
Macro Source (TypeScript)
         ↓
  ┌──────────────────┐
  │   Hermes Runtime │ ← MacroVM
  │   ┌────────────┐ │
  │   │ JSI Bridge │─┼─→ ast.* (AST manipulation)
  │   └────────────┘ │
  │   ┌────────────┐ │
  │   │  Bytecode  │ │ ← Optional cache
  │   │   Cache    │ │
  │   └────────────┘ │
  └──────────────────┘
         ↓
  Modified AST (Zig)
```

---

## MacroVM API

```zig
pub const MacroVM = struct {
    allocator: std.mem.Allocator,
    runtime: *c.MSHermesRuntime,
    ast_ctx: *ASTContext,
    bytecode_cache: ?*BytecodeCache,
    net_cache: ?*NetworkCache,

    pub fn init(allocator: Allocator, arena: *ASTArena) !MacroVM;
    pub fn deinit(self: *MacroVM) void;

    // Execute @derive(Eq, Hash) on class node
    pub fn executeMacro(
        self: *MacroVM,
        name: []const u8,      // "derive"
        args: []const []const u8,  // ["Eq", "Hash"]
        target: *ast.Node,
    ) !void;

    // Execute arbitrary JS source
    pub fn executeSource(self: *MacroVM, source: []const u8, target: *ast.Node) !void;

    // Execute pre-compiled bytecode (fast path)
    pub fn executeBytecode(self: *MacroVM, bytecode: []const u8, target: *ast.Node) !void;
};
```

### Usage Example

```zig
var vm = try MacroVM.init(allocator, arena);
defer vm.deinit();

// Execute @derive(Eq) on class node
try vm.executeMacro("derive", &[_][]const u8{"Eq"}, class_node);
```

---

## JSI Bridge: AST API

The `ast` global object exposes AST manipulation to JavaScript:

```javascript
// Available in macro code
const ast = {
    createMethod(name, body, params): MethodNode,
    createParam(name, type?): ParamNode,
    createBinaryExpr(op, left, right): BinaryExprNode,
    createIdentifier(name): IdentifierNode,
    createMemberExpr(object, property): MemberExprNode,
    createReturnStmt(expr): ReturnStmtNode,
    createBlock(statements): BlockStmtNode,
};
```

### Target Object

The `target` global provides access to the decorated node:

```javascript
// For @derive on class:
target.name        // "User"
target.properties  // ["id", "name", "email"]
target.addMethod(method)  // Mutate class
```

---

## Built-in Macros

### @derive(Eq)

Generates `equals()` method:

```javascript
// Implementation (in Zig as string constant)
(function() {
    const props = target.properties;

    // Build: this.prop === other.prop && ...
    let expr = null;
    for (const prop of props) {
        const thisAccess = ast.createMemberExpr(
            ast.createIdentifier("this"),
            ast.createIdentifier(prop)
        );
        const otherAccess = ast.createMemberExpr(
            ast.createIdentifier("other"),
            ast.createIdentifier(prop)
        );
        const cmp = ast.createBinaryExpr("===", thisAccess, otherAccess);

        expr = expr ? ast.createBinaryExpr("&&", expr, cmp) : cmp;
    }

    const returnStmt = ast.createReturnStmt(expr);
    const body = ast.createBlock([returnStmt]);
    const method = ast.createMethod("equals", body, [ast.createParam("other")]);

    target.addMethod(method);
})();
```

### @derive(Hash)

Generates `hash()` method using property values.

---

## Bytecode Compilation

### Performance

| Mode | Execution Time | Speedup |
|------|----------------|---------|
| Source eval | ~45ms | 1x |
| Bytecode | ~0.2ms | **225x** |

### Bytecode Flow

```
1. Source Hash → Cache Lookup
   ↓ (miss)
2. hermesc → Compile to HBC
   ↓
3. Store in BytecodeCache
   ↓
4. ms_hermes_eval_bytecode()
```

### Cache Key

```zig
fn hashMacroSource(trait: []const u8, source: []const u8) u64 {
    // Hash trait name + source code
    return std.hash.Wyhash.hash(0, trait ++ source);
}
```

---

## Network Cache (@comptime fetch)

For `@comptime` blocks that fetch external data:

```typescript
@comptime {
    const config = fetch("https://api.example.com/config").json();
    // config is compile-time constant
}
```

### Synchronous Fetch

```javascript
// Wrapped in VM setup
function fetch(url) {
    const result = __ms_fetch(url);  // Native function
    return {
        ok: result.status >= 200 && result.status < 300,
        status: result.status,
        body: result.body,
        text: () => result.body,
        json: () => JSON.parse(result.body),
    };
}
```

### NetworkCache

```zig
pub const NetworkCache = struct {
    pub fn get(self: *NetworkCache, url: []const u8) ?CachedResponse;
    pub fn store(self: *NetworkCache, url: []const u8, response: Response) !void;
};
```

---

## ASTContext

Bridge context passed to all JSI functions:

```zig
pub const ASTContext = struct {
    arena: *ast.ASTArena,
    allocator: std.mem.Allocator,
    current_node: ?*ast.Node,  // Target of current macro
};
```

### HostObject Registration

```zig
pub fn register(runtime: *c.MSHermesRuntime, ctx: *ASTContext) void {
    // Register 'ast' global
    c.ms_hermes_register_host_object(runtime, "ast", astGetter, null, ctx);

    // Register 'target' global
    c.ms_hermes_register_host_object(runtime, "target", targetGetter, targetSetter, ctx);
}
```

---

## Error Handling

```zig
fn executeMacro(...) !void {
    const result = c.ms_hermes_eval(runtime, source.ptr, source.len, "macro.js");

    if (c.ms_hermes_has_exception(runtime)) {
        const msg = c.ms_hermes_get_exception(runtime);
        std.log.err("Macro error: {s}", .{std.mem.span(msg)});
        c.ms_hermes_clear_exception(runtime);
        return error.MacroExecutionFailed;
    }

    if (result) |r| {
        c.ms_value_destroy(r);
    }
}
```

---

## Why Hermes?

| Feature | Benefit |
|---------|---------|
| **AOT Bytecode** | ~225x faster than source eval |
| **Small footprint** | ~2MB for runtime |
| **JSI (C API)** | Zero-copy AST manipulation |
| **Battle-tested** | Powers React Native |
| **No JIT** | Deterministic performance |

### Alternative Considered

- **V8:** Too heavy (50MB+), JIT unpredictable
- **QuickJS:** Slower, less mature
- **Custom interpreter:** High development cost

---

## File Structure

```
src/vm/
  macro_vm.zig        # Main VM implementation
  ast_api.zig         # JSI bridge for AST manipulation
  c_api.zig           # Hermes C bindings
  bytecode_compiler.zig  # hermesc integration

src/transam/
  disk_cache.zig      # Bytecode cache
  network_cache.zig   # @comptime fetch cache
```

---

## Integration with Pipeline

```
Parser → AST → MacroVM → Expanded AST → Type Checker
                 ↑
            This component

MacroVM receives:
  - MacroInvocation nodes (@derive, @comptime)
  - Target nodes (class, function)

MacroVM produces:
  - Modified AST (new methods, expanded code)
```

---

## Testing

```bash
zig build test -- --test-filter="macro_vm"
```

```zig
test "MacroVM: derive(Eq) adds equals method" {
    var arena = ast.ASTArena.init(testing.allocator);
    defer arena.deinit();

    // Create class with properties
    const class_node = try createTestClass(&arena, "User", &.{"id", "name"});

    var vm = try MacroVM.init(testing.allocator, &arena);
    defer vm.deinit();

    try vm.executeMacro("derive", &.{"Eq"}, class_node);

    // Verify equals method was added
    const methods = class_node.data.class_decl.methods;
    try testing.expectEqual(1, methods.len);
    try testing.expectEqualStrings("equals", methods[0].data.method_decl.name);
}
```

---

## References

- **Hermes:** https://hermesengine.dev
- **Macro system:** `./macros.md`
- **Trans-Am cache:** `./trans-am.md`
- **AST structure:** `./ast.md`
