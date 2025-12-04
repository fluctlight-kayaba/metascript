# JavaScript Executor Agent

## Role
Executes generated JavaScript code in Node.js and validates output.

## Responsibilities

### 1. JavaScript Execution
- Run generated JS files with Node.js
- Capture stdout, stderr, and exit code
- Enforce timeout limits
- Monitor memory usage via V8 flags

### 2. Output Validation
- Compare actual output to expected output
- Handle async output properly
- Support JSON output comparison

### 3. Test Case Execution
- Run fixture-specific test cases
- Support common test frameworks (if used)
- Aggregate results

## Execution Commands
```bash
# Basic execution with timeout
timeout 30s node ${js_path}

# With memory tracking
node --max-old-space-size=512 --expose-gc ${js_path}

# With performance timing
node --perf-basic-prof ${js_path}

# ESM module execution
node --experimental-vm-modules ${js_path}
```

## Output Schema
```yaml
execution_result:
  fixture: string
  backend: "js"
  success: boolean

  execution:
    exit_code: number
    stdout: string
    stderr: string
    duration_ms: number
    memory_peak_kb: number
    timed_out: boolean

  validation:
    output_matches: boolean
    expected: string
    actual: string
    diff: string

  test_cases:
    total: number
    passed: number
    failed: number
    failures: list[object]

  error_analysis:
    error_type: string  # runtime|type|reference|timeout
    likely_component: string
    error_message: string
    stack_trace: string
```

## Error Classification
| Error Pattern | Likely Component |
|--------------|------------------|
| ReferenceError | JS Backend (undefined var) |
| TypeError | JS Backend (type issue) |
| RangeError | JS Backend (recursion/bounds) |
| SyntaxError | JS Backend (codegen) |
| Timeout | JS Backend (infinite loop) or Lobster |
| Wrong output | Multiple (logic error) |

## Node.js Requirements
- Version: 18.0.0+
- ESM support enabled
- No external dependencies required for basic tests
