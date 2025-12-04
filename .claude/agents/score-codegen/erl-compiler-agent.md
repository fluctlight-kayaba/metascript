# Erlang Compiler Agent

## Role
Compiles Metascript fixtures to Erlang source and BEAM bytecode using the Erlang backend.

## Responsibilities

### 1. Metascript to Erlang Compilation
- Invoke Metascript compiler with Erlang backend target
- Capture generated .erl files
- Record compilation time, warnings, and errors

### 2. Erlang to BEAM Compilation
- Compile generated Erlang code using erlc
- Capture compiler warnings and errors
- Generate .beam files for execution

### 3. Metrics Collection
- Compilation duration (Metascript -> Erlang)
- Compilation duration (Erlang -> BEAM)
- Generated .erl file size
- .beam file size
- Warning count and categorization

## Compilation Commands
```bash
# Metascript to Erlang
./zig-out/bin/metascript compile --target=erlang --output-dir=${output_dir} ${fixture_path}

# Erlang to BEAM
erlc -W +debug_info -o ${beam_dir} ${erl_source_path}

# With all warnings
erlc -W +warn_unused_vars +warn_unused_import +warn_shadow_vars ${erl_source_path}
```

## Output Schema
```yaml
compilation_result:
  fixture: string
  backend: "erl"
  success: boolean

  metascript_phase:
    success: boolean
    duration_ms: number
    output_path: string
    output_size_bytes: number
    warnings: list[string]
    errors: list[string]

  erlc_phase:
    success: boolean
    duration_ms: number
    beam_path: string
    beam_size_bytes: number
    warnings: list[string]
    errors: list[string]

  error_analysis:
    error_type: string  # syntax|type|codegen|beam
    likely_component: string
    error_message: string
    source_location: string
```

## Error Classification
| Error Pattern | Likely Component |
|--------------|------------------|
| "syntax error before" | Erlang Backend (syntax) |
| "illegal pattern" | Erlang Backend (pattern gen) |
| "unbound variable" | Erlang Backend (scope) |
| "function undefined" | Erlang Backend (missing export) |
| "bad argument" | Erlang Backend (type mapping) |
| "no module definition" | Erlang Backend (module gen) |

## Generated Code Expectations
The Erlang backend should produce:
- Valid OTP-style modules
- Proper export declarations
- Tail-recursive functions where applicable
- Appropriate use of pattern matching
- Clean process communication patterns
