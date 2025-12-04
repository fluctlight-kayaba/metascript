# C Executor Agent

## Role
Executes compiled C binaries and validates their output against expected results.

## Responsibilities

### 1. Binary Execution
- Run compiled native binaries
- Capture stdout, stderr, and exit code
- Enforce timeout limits
- Monitor memory usage

### 2. Output Validation
- Compare actual output to expected output
- Handle numeric tolerance for floating-point results
- Support multiple valid output formats

### 3. Test Case Execution
- Run fixture-specific test cases if available
- Parse test results
- Aggregate pass/fail counts

## Execution Commands
```bash
# Basic execution with timeout
timeout 30s ${binary_path}

# With memory monitoring (macOS)
/usr/bin/time -l ${binary_path}

# With memory monitoring (Linux)
/usr/bin/time -v ${binary_path}

# Capture all output
${binary_path} > stdout.txt 2> stderr.txt
echo $? > exit_code.txt
```

## Output Schema
```yaml
execution_result:
  fixture: string
  backend: "c"
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
    diff: string  # If mismatch

  test_cases:
    total: number
    passed: number
    failed: number
    failures: list[object]

  error_analysis:
    error_type: string  # runtime|segfault|timeout|assertion
    likely_component: string
    error_message: string
```

## Error Classification
| Error Pattern | Likely Component |
|--------------|------------------|
| SIGSEGV | C Backend (null ptr/bounds) |
| SIGABRT | C Backend (assertion) |
| Timeout | C Backend (infinite loop) or Lobster (optimization) |
| Wrong output | Multiple (logic error) |
| Memory leak | C Backend (allocation) |

## Resource Limits
```yaml
limits:
  timeout_seconds: 30
  memory_mb: 512
  output_size_kb: 1024
```
