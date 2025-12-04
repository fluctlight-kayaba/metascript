# Erlang Executor Agent

## Role
Executes compiled BEAM bytecode and validates output against expected results.

## Responsibilities

### 1. BEAM Execution
- Start Erlang VM and load compiled modules
- Execute main function
- Capture output and exit status
- Enforce timeout limits

### 2. Output Validation
- Compare actual output to expected
- Handle Erlang term formatting
- Support pattern-based matching for process output

### 3. Test Case Execution
- Run EUnit tests if present
- Support Common Test format
- Aggregate test results

## Execution Commands
```bash
# Basic execution
erl -noshell -pa ${beam_dir} -s ${module_name} main -s init stop

# With output capture
erl -noshell -pa ${beam_dir} -eval "${module_name}:main(), init:stop()."

# With timeout (using timeout command)
timeout 30s erl -noshell -pa ${beam_dir} -s ${module_name} main -s init stop

# Run EUnit tests
erl -noshell -pa ${beam_dir} -eval "eunit:test(${module_name}), init:stop()."
```

## Output Schema
```yaml
execution_result:
  fixture: string
  backend: "erl"
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
    error_type: string  # runtime|badarg|badfun|timeout|crash
    likely_component: string
    error_message: string
    stack_trace: string
```

## Error Classification
| Error Pattern | Likely Component |
|--------------|------------------|
| badarg | Erlang Backend (type mapping) |
| badfun | Erlang Backend (function ref) |
| badmatch | Erlang Backend (pattern) |
| function_clause | Erlang Backend (incomplete match) |
| undef | Erlang Backend (missing function) |
| timeout | Erlang Backend or Lobster |
| process crash | Erlang Backend (supervision) |

## OTP Considerations
- Support for supervision tree testing
- Process message passing validation
- Hot code reload testing (if applicable)
