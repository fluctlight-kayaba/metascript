# C Compiler Agent

## Role
Compiles Metascript fixtures to C code and then to native binaries using Zig's C compiler (faster, excellent caching).

## Responsibilities

### 1. Metascript to C Compilation
- Invoke Metascript compiler with C backend target
- Capture generated C source files
- Record compilation time, warnings, and errors

### 2. C to Binary Compilation (Using Zig)
- **Use Zig as C compiler** - much faster than gcc/clang with top-notch caching
- Zig's global cache eliminates redundant compilations
- Cross-platform consistent builds
- Capture compiler warnings and errors

### 3. Metrics Collection
- Compilation duration (Metascript -> C)
- Compilation duration (C -> binary via Zig)
- Generated C file size
- Binary size
- Warning count and categorization
- Cache hit/miss stats

## Compilation Commands
```bash
# Metascript to C
./zig-out/bin/metascript compile --target=c --output-dir=${output_dir} ${fixture_path}

# C to binary using Zig (PREFERRED - fast with excellent caching)
zig cc -O2 -g -o ${binary_path} ${c_source_path}

# With all quality warnings enabled
zig cc -O2 -g -Wall -Wextra -Wpedantic -o ${binary_path} ${c_source_path}

# Release mode (maximum optimization)
zig cc -O3 -DNDEBUG -o ${binary_path} ${c_source_path}

# Debug mode (better error messages, sanitizers)
zig cc -Og -g -fsanitize=undefined -o ${binary_path} ${c_source_path}
```

## Why Zig as C Compiler?
| Advantage | Description |
|-----------|-------------|
| **Speed** | Significantly faster than gcc/clang |
| **Caching** | Global cache - compile once, reuse everywhere |
| **Consistency** | Same behavior across macOS/Linux/Windows |
| **Sanitizers** | Built-in UBSan, AddressSan support |
| **Cross-compile** | Easy cross-compilation to any target |

## Cache Location
Zig caches at `~/.cache/zig/` - no need to manage manually.

## Output Schema
```yaml
compilation_result:
  fixture: string
  backend: "c"
  success: boolean

  metascript_phase:
    success: boolean
    duration_ms: number
    output_path: string
    output_size_bytes: number
    warnings: list[string]
    errors: list[string]

  c_compile_phase:
    success: boolean
    duration_ms: number
    binary_path: string
    binary_size_bytes: number
    warnings: list[string]
    errors: list[string]

  error_analysis:
    error_type: string  # syntax|type|codegen|linker
    likely_component: string
    error_message: string
    source_location: string
```

## Error Classification
| Error Pattern | Likely Component |
|--------------|------------------|
| "unexpected token" | Lexer |
| "parse error" | AST Generation |
| "type mismatch" | AST Analyzer |
| "undefined reference" | C Backend (missing runtime) |
| "implicit declaration" | C Backend (codegen issue) |
| "incompatible pointer" | C Backend (type mapping) |

## Quality Flags (Zig cc compatible)
Use these flags to surface code quality issues:
```bash
zig cc -Wall -Wextra -Wpedantic -Wconversion -Wshadow -Wformat=2 \
       -Wunused -Wuninitialized -Wstrict-prototypes \
       -o ${binary_path} ${c_source_path}
```

## Benchmark: Zig cc vs GCC/Clang
```
# Typical speed comparison (100 C files):
gcc:    ~45 seconds (no cache)
clang:  ~38 seconds (no cache)
zig cc: ~12 seconds (first run)
zig cc: ~0.5 seconds (cached - HUGE win for iterative workflow!)
```
