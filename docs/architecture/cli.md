# Command Line Interface (CLI)

**Purpose:** User-facing compiler commands and debugging tools

**Binary:** `msc` (Metascript Compiler)

---

## Overview

```
msc [COMMAND] <file.ms> [OPTIONS]
```

| Category | Commands |
|----------|----------|
| **Compilation** | `compile`, `run`, `check` |
| **Debugging** | `dump-tokens`, `dump-ast`, `pipeline`, `expand` |
| **Info** | `--help`, `--version` |

---

## Compilation Commands

### compile

Compile to target backend:

```bash
msc compile main.ms                      # Default: C backend
msc compile main.ms --target=c           # C backend (native binary)
msc compile main.ms --target=js          # JavaScript backend
msc compile main.ms --target=erlang      # Erlang backend
msc compile main.ms --output=out.c       # Custom output path
msc compile main.ms --no-normalize       # Disable AST normalization
```

**Pipeline Stages:**
```
[1/5] Parsing main.ms...
[2/5] Analyzing types...
[3/5] Expanding macros...
[4/5] Running DRC analysis...
[5/5] Generating C code...
```

### run

Compile and execute:

```bash
msc run main.ms              # Compile to temp, execute
msc run main.ms --target=js  # Run with Node.js
```

### check

Type check only (no codegen):

```bash
msc check main.ms
```

Outputs type errors without generating code.

---

## Debugging Commands

### dump-tokens

Show lexer output:

```bash
msc dump-tokens main.ms
```

Output:
```
Token(keyword_class, "class", 1:1)
Token(identifier, "User", 1:7)
Token(left_brace, "{", 1:12)
Token(identifier, "name", 2:5)
Token(colon, ":", 2:9)
Token(identifier, "string", 2:11)
...
```

### dump-ast

Show parsed AST:

```bash
msc dump-ast main.ms
```

Output:
```
Program
└─ ClassDecl "User"
   ├─ PropertyDecl "name": string
   └─ MethodDecl "greet"
      └─ ReturnStmt
         └─ StringLiteral "Hello"
```

### pipeline

Show full compilation pipeline:

```bash
msc pipeline main.ms
```

Output:
```
=== LEXER ===
169 tokens produced

=== PARSER ===
AST with 3 top-level declarations

=== MACRO EXPANSION ===
Expanded: @derive(Eq) on User

=== TYPE CHECKER ===
All types resolved

=== CODE GENERATION ===
Target: C
Output: 142 lines
```

### expand

Show macro expansion results:

```bash
msc expand main.ms
```

Output:
```
Before:
@derive(Eq)
class User { name: string; }

After:
class User {
    name: string;
    equals(other: User): boolean {
        return this.name === other.name;
    }
}
```

---

## Options

| Option | Description |
|--------|-------------|
| `--target=<backend>` | Target backend: `c`, `js`, `erlang` |
| `--output=<path>` | Output file path |
| `--no-normalize` | Disable AST normalization (benchmarking) |
| `-h`, `--help` | Show help |
| `-v`, `--version` | Show version |

---

## Architecture

```
src/main.zig          # Entry point, command dispatch
src/cli/
  compile.zig         # compile command
  run.zig             # run command
  check.zig           # check command
  dump_tokens.zig     # dump-tokens command
  dump_ast.zig        # dump-ast command
  pipeline.zig        # pipeline command
  expand.zig          # expand command
  expand_macro.zig    # Macro expansion helper
  colors.zig          # Terminal color codes
```

### Command Dispatch

```zig
pub fn main() !void {
    const args = try std.process.argsAlloc(allocator);
    const command = args[1];

    if (std.mem.eql(u8, command, "compile")) {
        try cli_compile.runWithArgs(allocator, input, target, output, normalize);
    } else if (std.mem.eql(u8, command, "run")) {
        try cli_run.run(allocator, input);
    } else if (std.mem.eql(u8, command, "check")) {
        try cli_check.run(allocator, input);
    }
    // ... etc
}
```

---

## Compile Pipeline

The `compile` command uses Trans-Am for incremental analysis:

```zig
pub fn runWithArgs(allocator: Allocator, input: []const u8, target: Backend, ...) !void {
    // Initialize Trans-Am (same engine as LSP)
    var db = try transam.TransAmDatabase.init(allocator);
    defer db.deinit();

    // Phase 1: Parse
    _ = try db.setFileText(input, source);

    // Phase 2: Check diagnostics
    const diagnostics = db.getDiagnostics(input);
    if (diagnostics.len > 0) {
        printDiagnostics(diagnostics);
        return;
    }

    // Phase 3: Get typed AST
    const typed_ast = try db.getTypedAst(input);

    // Phase 4: Macro expansion
    if (enable_normalize) {
        try vm_expander.expandMacros(allocator, typed_ast);
    }

    // Phase 5: DRC analysis (optional)
    var drc = try Drc.init(allocator, typed_ast);
    try drc.analyze();

    // Phase 6: Code generation
    switch (target) {
        .c => try cgen.generate(allocator, typed_ast, output),
        .js => try jsgen.generate(allocator, typed_ast, output),
        .erlang => try erlgen.generate(allocator, typed_ast, output),
    }
}
```

---

## Colored Output

Terminal colors for better UX:

```zig
pub const colors = struct {
    pub const error_color = Color.bright_red;
    pub const warning = Color.bright_yellow;
    pub const success = Color.bright_green;
    pub const info = Color.bright_blue;
    pub const header = Color.bright_cyan;
    pub const dim_text = Color.dim;
};
```

Example output:
```
[1/5] Parsing main.ms...
[2/5] Analyzing types...
error: Type mismatch at line 15
  Expected: number
  Got: string
```

---

## Error Reporting

Diagnostics with source context:

```zig
fn printDiagnostics(file: []const u8, source: []const u8, diags: []Diagnostic) void {
    for (diags) |diag| {
        // Print location
        std.debug.print("{s}:{}:{}: ", .{file, diag.line, diag.column});

        // Print severity
        switch (diag.severity) {
            .@"error" => std.debug.print("error: ", .{}),
            .warning => std.debug.print("warning: ", .{}),
        }

        // Print message
        std.debug.print("{s}\n", .{diag.message});

        // Print source line with caret
        printSourceContext(source, diag.line, diag.column);
    }
}
```

Output:
```
main.ms:15:10: error: Type mismatch
    const x: number = "hello";
             ^^^^^^^
  Expected: number
  Got: string
```

---

## Backend Selection

| Backend | Extension | Compiler | Runtime |
|---------|-----------|----------|---------|
| C | `.c` | `zig cc` | Native binary |
| JavaScript | `.js` | - | Node.js / Browser |
| Erlang | `.erl` | `erlc` | BEAM VM |

### Auto-detection (Future)

```typescript
// Imports trigger backend selection
import { fetch } from "@metascript/web";     // → JavaScript
import { ffi } from "@metascript/ffi";       // → C
import { GenServer } from "@metascript/otp"; // → Erlang
```

---

## Examples

```bash
# Basic compilation
msc compile hello.ms --target=c
zig cc hello.c -o hello
./hello

# JavaScript compilation
msc compile app.ms --target=js
node app.js

# Erlang compilation
msc compile server.ms --target=erlang
erlc server.erl
erl -noshell -s server main -s init stop

# Full pipeline debug
msc pipeline main.ms

# Quick type check
msc check main.ms
```

---

## Testing

```bash
# Build CLI
zig build

# Run help
./zig-out/bin/msc --help

# Test compile
./zig-out/bin/msc compile examples/hello.ms --target=c
```

---

## References

- **Trans-Am integration:** `./trans-am.md`
- **Code generation:** `./backends.md`
- **Type checking:** `./ast-analyzer.md`
- **Macro expansion:** `./macros.md`
