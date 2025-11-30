# Metascript Compiler Source Code

This directory contains the Metascript compiler implementation in Zig.

## Architecture Overview

```
TypeScript Source (.ms)
    ↓
[Parser] src/parser/
    ↓
AST (Abstract Syntax Tree)
    ↓
[Macro Expander] src/macro/  ← THE MAGIC HAPPENS HERE
    ↓
Expanded AST (with generated code)
    ↓
[Type Checker] src/checker/
    ↓
Typed AST
    ↓
[IR Lowering] src/ir/
    ↓
Unified IR (platform-agnostic)
    ↓
[Backend Selection]
    ├─→ C Backend → GCC/Clang → Native Binary
    ├─→ JavaScript Backend → Modern JS (Browser/Node/Deno/Bun/Hermes)
    └─→ Erlang Backend → BEAM Bytecode (OTP)
```

## Directory Structure

### `ast/` - AST Foundation (CORE)

The foundation of the compiler. All other components operate on AST nodes.

- **`ast.zig`** - Main module, exports all AST types
- **`node.zig`** - AST node definitions (37 node kinds)
  - Expression nodes: literals, binary/unary ops, calls, members
  - Statement nodes: blocks, if/while/for, returns, variables
  - Declaration nodes: functions, classes, interfaces, imports
  - **Macro nodes**: `MacroInvocation`, `ComptimeBlock` (critical!)
- **`types.zig`** - Type representation
  - Primitives: number, string, boolean, void, unknown, never
  - Compound: object, array, tuple, function
  - Generics: type parameters, instantiations
  - Advanced: union, intersection, type references
- **`location.zig`** - Source location tracking
  - File registry (path → file ID)
  - Position tracking (line + column)
  - Error reporting

**Key Design:**
- AST must be designed first because macros operate on it
- All backends consume the same AST (after macro expansion)
- Memory-efficient arena allocation

### `macro/` - Macro System (DIFFERENTIATOR)

**This is what makes Metascript special!**

- **`expander.zig`** - Macro expansion engine
  - **AST → AST transformations** (the core of metaprogramming)
  - 3-pass algorithm:
    1. Find all `@macro` invocations
    2. Execute each macro function (generate new AST)
    3. Recursively expand (macros can generate macros!)
  - Macro registry + error reporting
  - AST utilities (clone, create nodes)

- **`builtin_macros.zig`** - Standard macros
  - **`@derive(Eq, Hash, Clone, Debug, Serialize)`** - Auto-generate methods
  - `@comptime { ... }` - Compile-time evaluation
  - `@compileError("msg")` - Emit errors
  - `@serialize` - Convenience alias

**Example: @derive Implementation**

```zig
fn deriveMacro(ctx: *MacroContext, invocation: *Node) !?*Node {
    // 1. Get target class
    const class = invocation.target;

    // 2. Parse traits (Eq, Hash, etc.)
    const traits = parseTraits(invocation.arguments);

    // 3. Generate methods for each trait
    for (traits) |trait| {
        if (trait == "Eq") {
            const equals_method = generateEqualsMethod(class);
            class.members.append(equals_method);
        }
        // ... Hash, Clone, Debug, Serialize
    }

    // 4. Return modified class (macro expanded!)
    return class;
}
```

**Why Macros Come Before Type Checking:**
- Macros generate code at compile-time
- Type checker validates the **generated** code
- Ensures macro output is type-safe
- Same macros work on ALL three backends (C, JS, Erlang)

### `parser/` - TypeScript Parser

**Status:** Stub (Week 5-6 implementation)

Will include:
- `lexer.zig` - Tokenization
- `parser.zig` - Strict TypeScript subset parser
- `grammar.zig` - Grammar definitions

**Design:**
- Recursive descent parser
- Builds AST directly (no intermediate parse tree)
- Strict mode only (no `any`, no `eval`, no dynamics)

### `checker/` - Type Checker

**Status:** Stub (Week 5-6 implementation)

Will include:
- `typechecker.zig` - Type inference and checking
- `symbols.zig` - Symbol table management
- `scope.zig` - Scope resolution

**Design:**
- Validates **post-macro** AST
- Hindley-Milner style type inference
- Nominal typing for classes, structural for interfaces

### `ir/` - Unified IR

**Status:** Stub (Week 7-8 implementation)

Will include:
- `ir.zig` - IR definition (platform-agnostic)
- `lower.zig` - AST → IR lowering
- `optimize.zig` - IR optimization passes

**Design:**
- SSA form (Static Single Assignment)
- Same IR for all three backends
- Optimizations before backend selection

### `main.zig` - CLI Entry Point

Compiler driver that orchestrates all phases:

```zig
fn compile(input_file: string) !void {
    // 1. Parse TypeScript → AST
    const ast = try parser.parse(input_file);

    // 2. Expand macros (AST → AST) ← CRITICAL STEP
    const expanded_ast = try macro_expander.expand(ast);

    // 3. Type check
    try type_checker.check(expanded_ast);

    // 4. Lower to IR
    const ir = try ir_lowering.lower(expanded_ast);

    // 5. Generate code (backend-specific)
    try backend.generate(ir);
}
```

## Why AST + Macros First?

**Traditional Compiler Order:**
Parser → Type Checker → IR → Codegen

**Metascript Order:**
Parser → **Macros (AST → AST)** → Type Checker → IR → Codegen

**Rationale:**
1. **Macros define the language** - They're the differentiator
2. **Can't design macros without AST** - Macros manipulate AST nodes
3. **Type checker validates macro output** - Ensures generated code is safe
4. **Proven by references:**
   - **Nim:** Macros operate on AST (`PNode`), expand before semantic analysis
   - **Haxe:** `macroApi.ml` provides AST manipulation, runs before typing
   - **Bun:** `Macro.zig` transforms JavaScript AST at compile-time

## Memory Management

**ASTArena:**
- Arena allocator for all AST nodes
- Fast O(1) allocation, no individual frees
- Automatic cleanup on deinit()
- File registry for source tracking

**Typical Memory Usage:**
- AST node: ~200 bytes
- Small program (100 LOC): ~50KB AST
- Large program (10K LOC): ~5MB AST

## Error Handling

All compiler phases use Zig's error unions:

```zig
pub fn expandMacro(node: *Node) !?*Node {
    // Returns:
    // - Error (compilation failed)
    // - null (macro reported error via ctx.reportError)
    // - *Node (success, expanded AST)
}
```

**Error Reporting:**
- Source locations tracked throughout compilation
- Errors point to original source (not generated code!)
- Macro errors show macro invocation location

## Testing

Run all tests:
```bash
zig build test --summary all
```

Test structure:
- Unit tests in each module (`test "..."`)
- Integration tests via examples/
- Validates: AST creation, macro expansion, memory management

## Building

```bash
# Debug build (fast compilation)
zig build

# Release build (optimized)
zig build -Doptimize=ReleaseFast

# Run tests
zig build test

# Run compiler
zig build run -- compile examples/macro_test.ms
```

## Contributing

See [../docs/contributing.md](../docs/contributing.md) for:
- Code style guidelines
- Testing requirements
- Areas needing help

**Good first issues:**
- Implement lexer (src/parser/lexer.zig)
- Add more built-in macros (@derive variants)
- Improve error messages

---

**Next Steps:** Weeks 5-12 - Parser, Type Checker, IR, and Three Backends in parallel.
