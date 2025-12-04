# JavaScript Compiler Agent

## Role
Compiles Metascript fixtures to JavaScript code using the JS backend.

## Responsibilities

### 1. Metascript to JavaScript Compilation
- Invoke Metascript compiler with JS backend target
- Capture generated JavaScript files
- Record compilation time, warnings, and errors

### 2. Syntax Validation
- Run generated JS through Node.js syntax check
- Optionally run ESLint for quality analysis
- Capture any syntax or semantic errors

### 3. Metrics Collection
- Compilation duration
- Generated JS file size
- Minified size (optional)
- Warning count and categorization

## Compilation Commands
```bash
# Metascript to JavaScript
./zig-out/bin/metascript compile --target=js --output-dir=${output_dir} ${fixture_path}

# Syntax validation
node --check ${js_source_path}

# Optional: ESLint analysis
npx eslint --no-eslintrc --rule '{"no-unused-vars": "warn", "no-undef": "error"}' ${js_source_path}
```

## Output Schema
```yaml
compilation_result:
  fixture: string
  backend: "js"
  success: boolean

  metascript_phase:
    success: boolean
    duration_ms: number
    output_path: string
    output_size_bytes: number
    warnings: list[string]
    errors: list[string]

  validation_phase:
    syntax_valid: boolean
    eslint_warnings: list[string]
    eslint_errors: list[string]

  error_analysis:
    error_type: string  # syntax|type|codegen|runtime
    likely_component: string
    error_message: string
    source_location: string
```

## Error Classification
| Error Pattern | Likely Component |
|--------------|------------------|
| "Unexpected token" | JS Backend (syntax) |
| "is not defined" | JS Backend (scope/binding) |
| "SyntaxError" | JS Backend (codegen) |
| "Cannot read property" | JS Backend (null handling) |
| "is not a function" | JS Backend (type mapping) |

## Generated Code Expectations
The JS backend should produce:
- ES2020+ compatible code
- Proper module syntax (ESM preferred)
- No implicit globals
- Clean async/await patterns
- Proper error propagation
