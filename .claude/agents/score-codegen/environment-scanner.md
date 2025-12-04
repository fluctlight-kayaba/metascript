# Environment Scanner Agent

## Role
Discovers test fixtures, validates backend toolchains, and loads baseline data for comparison.

## Responsibilities

### 1. Fixture Discovery
- Scan `/Users/le/projects/metascript/tests/fixtures/` recursively
- Categorize fixtures by directory (algorithms, basic, bugs, classes, control_flow, executable, patterns)
- Identify fixture metadata (expected output, test cases, complexity level)
- Filter fixtures based on user-specified categories

### 2. Backend Validation
- Verify required tools are installed and accessible:
  - **C Backend**: `zig cc` (Zig as C compiler - fast with excellent caching)
  - **JS Backend**: node (v18+), npm (optional)
  - **Erlang Backend**: erl, erlc, rebar3 (optional)
- Check tool versions for compatibility
- Report missing tools with installation instructions

### Validation Commands
```bash
# C Backend (using Zig)
zig version          # Zig compiler (also used for Metascript)
zig cc --version     # Zig as C compiler

# JavaScript Backend
node --version       # Node.js runtime

# Erlang Backend
erl -version         # Erlang runtime
erlc -v              # Erlang compiler
```

### 3. Baseline Loading
- Load previous run results from `.score-codegen/baseline.json`
- Parse historical scores for trend analysis
- Handle missing baseline gracefully (first run scenario)

## Input Schema
```yaml
args:
  backends: string  # "c,js,erl" or subset
  fixtures: string  # "all" or comma-separated categories
```

## Output Schema
```yaml
available_fixtures:
  - path: string
    category: string
    name: string
    has_expected_output: boolean
    has_test_cases: boolean
    complexity: low|medium|high

backend_configs:
  c:
    available: boolean
    compiler: "zig cc"  # Zig as C compiler (fast + cached)
    version: string
    cache_dir: "~/.cache/zig/"
  js:
    available: boolean
    runtime: string  # node
    version: string
  erl:
    available: boolean
    version: string

previous_run_baseline:
  timestamp: string
  scores: object
  issue_counts: object
```

## Error Handling
- Missing fixture directory: ABORT with clear message
- Missing backend tools: WARN and exclude backend from run
- Corrupted baseline: WARN and proceed without baseline comparison
