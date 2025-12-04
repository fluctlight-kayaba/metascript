# Score Codegen

Comprehensive code generation quality scoring for Metascript compiler.

## Native Zig Tool (Preferred)

The score-codegen workflow is implemented as a **native Zig tool** for:
- Type safety and cross-platform consistency
- Integration with existing test infrastructure
- Zig's excellent caching for fast iteration
- No shell script dependencies

### Run via Build System

```bash
# Full scoring (all backends)
zig build test-score-codegen

# Individual backend tests
zig build test-score-codegen -- --test-filter="C backend"
zig build test-score-codegen -- --test-filter="JavaScript"
zig build test-score-codegen -- --test-filter="Erlang"

# Quick smoke test
zig build test-analytics  # Existing success rate analytics
```

### Source Location
- **Main tool:** `tests/backends/score_codegen.zig`
- **Build config:** `build/tests.zig` (test-score-codegen step)

---

## Claude Code Slash Command Usage

```
/score-codegen [backends] [options]
```

## Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `backends` | `c,js,erl` | Comma-separated backends to test |
| `--fixtures` | `all` | Fixture categories (algorithms,basic,bugs,classes,control_flow,executable,patterns) |
| `--verbose` | `false` | Include full code snippets in output |
| `--no-baseline` | `false` | Skip baseline comparison |

## Examples

```bash
# Score all backends with all fixtures
/score-codegen

# Score only C backend
/score-codegen c

# Score C and JavaScript backends
/score-codegen c,js

# Score Erlang backend with verbose output
/score-codegen erl --verbose

# Score specific fixture categories
/score-codegen c --fixtures=algorithms,classes

# Fresh run without baseline comparison
/score-codegen --no-baseline
```

## Workflow Stages

### Stage 1: Environment Setup
- Discover all test fixtures in `/Users/le/projects/metascript/tests/fixtures/`
- Validate backend toolchains (gcc/clang, node, erlc)
- Load previous baseline for comparison

### Stage 2: Compile & Execute (Parallel by Backend)
For each backend:
1. Compile all fixtures through Metascript compiler
2. Compile generated code (C->binary, JS->validate, Erlang->BEAM)
3. Execute compiled outputs
4. Validate against expected results
5. Run test cases if available

### Stage 3: Code Quality Analysis
Evaluate generated code against principal engineer standards:
- Readability (naming, structure, comments)
- Efficiency (allocations, redundant ops, loops)
- Idiomaticity (C/JS/Erlang best practices)
- Safety (null handling, bounds, errors)
- Size efficiency (bloat, dead code)

### Stage 4: Component Attribution
Map issues to compiler infrastructure:
- Lexer
- AST Generation (Parser)
- AST Analyzer (Type Checker)
- Trans-Am Cache
- DRC/ORC (Dependency Resolution)
- Lobster (Optimization)
- Macro System
- Backend Codegen (C/JS/Erlang specific)

### Stage 5: Report Generation
- Summary report with scores
- Detailed report with all results
- Trend analysis vs baseline
- Prioritized action items

## Output

### Terminal Summary
```
Score-Codegen Results
=====================

Backend Results:
  C:      [################----] 80% compile | 75% execute | 72/100 quality
  JS:     [##################--] 90% compile | 85% execute | 78/100 quality
  Erlang: [##############------] 70% compile | 65% execute | 68/100 quality

Overall Health: 73/100 (+5 from baseline)

Top Issues:
  CRITICAL: C Backend - Null pointer in array codegen (3 fixtures)
  HIGH:     Erlang Backend - Missing tail recursion (5 fixtures)
```

### Generated Files
```
.score-codegen/
  baseline.json              # Latest scores for comparison
  LATEST_SCORE.md           # Quick reference summary
  reports/
    summary_TIMESTAMP.md     # Executive summary
    detailed_TIMESTAMP.md    # Full analysis
    data_TIMESTAMP.json      # Raw data
```

## Scoring Methodology

### Compilation Rate
```
successful_compilations / total_fixtures * 100
```

### Execution Rate
```
passed_executions / successful_compilations * 100
```

### Quality Score (0-100)
```
Weighted average of:
  - Readability:      20%
  - Efficiency:       25%
  - Idiomaticity:     20%
  - Safety:           25%
  - Size Efficiency:  10%
```

### Overall Backend Score
```
compilation_rate * 0.3 + execution_rate * 0.3 + quality_score * 0.4
```

### Health Score
```
Average of all backend overall scores
```

## Quality Gates

| Gate | Condition | Action |
|------|-----------|--------|
| Minimum Compile | At least 1 fixture compiles | Abort if failed |
| Baseline Regression | Score drop > 5 points | Warning |
| Critical Failures | Any critical issues | Warning |

## Component Mapping

When issues are found, they're mapped to specific compiler components:

| Component | Indicators |
|-----------|------------|
| Lexer | "unexpected token", "invalid character" |
| Parser | "parse error", "syntax error", "unexpected EOF" |
| Analyzer | "type mismatch", "undefined variable" |
| Cache | "cache inconsistency", "stale compilation" |
| DRC | "circular dependency", "unresolved import" |
| Lobster | "optimization failed", code bloat |
| Macro | "@derive error", "hygiene violation" |
| C Backend | gcc/clang errors, non-idiomatic C |
| JS Backend | node errors, async issues |
| Erlang Backend | erlc errors, missing tail recursion |

## Continuous Improvement

This workflow is designed for repeated execution:

1. **Run regularly**: After significant changes
2. **Track trends**: Compare against baseline
3. **Fix prioritized issues**: Focus on highest-impact fixes
4. **Re-run**: Verify improvements, update baseline

## Related Commands

- `/test-backend <backend>` - Run backend-specific tests only
- `/compile <file> --target=<backend>` - Compile single file
- `/quality-check <file>` - Analyze single generated file

## Implementation Notes

This workflow uses parallel execution for:
- All 3 backends compile simultaneously
- All fixtures within a backend compile simultaneously
- Quality analysis runs parallel to execution

Estimated runtime:
- Small fixture set (~20): 1-2 minutes
- Full fixture set (~100): 5-10 minutes
- With verbose analysis: +2-3 minutes

---

## Execution Instructions

When this command is invoked, execute the following workflow:

### Step 1: Parse Arguments
```
backends = parse_backends(args) || ["c", "js", "erl"]
fixtures_filter = parse_fixtures(args) || "all"
verbose = "--verbose" in args
use_baseline = "--no-baseline" not in args
```

### Step 2: Environment Validation
Run these checks for selected backends:

**C Backend (using Zig as C compiler - fast + cached):**
```bash
zig version      # Metascript compiler AND C compiler
zig cc --version # Verify zig cc works
```

**JavaScript Backend:**
```bash
zig version      # Metascript compiler
node --version   # JS runtime
```

**Erlang Backend:**
```bash
zig version      # Metascript compiler
erl -version     # Erlang runtime
erlc -v          # Erlang compiler
```

> **Why Zig for C?** Zig's C compiler is significantly faster than gcc/clang
> and has a global cache (`~/.cache/zig/`) that eliminates redundant compilations.
> This is crucial for iterative score-codegen runs!

### Step 3: Discover Fixtures
```bash
find tests/fixtures -name "*.ms" -type f
```

Categorize by directory: algorithms, basic, bugs, classes, control_flow, executable, patterns

### Step 4: Compile Phase (Parallel)

Use the Task tool to spawn parallel agents for each backend:

```
For each backend in selected_backends (PARALLEL):
  For each fixture in fixtures (PARALLEL, max 5 concurrent):
    1. Run: zig build run -- compile --target={backend} {fixture}
    2. Record: success/failure, time, output size, warnings
    3. If backend == "c": Run `zig cc` on generated .c file (FAST + CACHED!)
    4. If backend == "erl": Run erlc on generated .erl file
```

**C compilation using Zig:**
```bash
# Standard build
zig cc -O2 -g -o {binary} {generated.c}

# With warnings for quality analysis
zig cc -O2 -g -Wall -Wextra -o {binary} {generated.c}
```

### Step 5: Execute Phase (Parallel)

For successfully compiled fixtures:

**C:**
```bash
./{compiled_binary}
# Check exit code, capture output
```

**JavaScript:**
```bash
node {generated.js}
# Check exit code, capture output
```

**Erlang:**
```bash
erl -noshell -pa {output_dir} -eval '{module}:main()' -s init stop
# Check exit code, capture output
```

### Step 6: Quality Analysis

Read each generated file and evaluate (use Opus model for this):

For C files - check:
- Memory management patterns
- Null pointer handling
- Buffer overflow potential
- Proper use of const
- Struct layout efficiency

For JS files - check:
- Modern JS patterns (const/let vs var)
- Async/await usage
- Error handling
- Module structure

For Erlang files - check:
- Tail recursion usage
- Pattern matching efficiency
- OTP compliance
- Message passing patterns

Score each dimension 1-10, calculate weighted average.

### Step 7: Component Attribution

For each issue found, classify by examining:
- Error message patterns
- Where in pipeline failure occurred
- Code patterns in generated output

### Step 8: Generate Report

Output format:
```
═══════════════════════════════════════════════════════════════
                 METASCRIPT CODEGEN SCORE REPORT
                 {timestamp}
═══════════════════════════════════════════════════════════════

SUMMARY
───────────────────────────────────────────────────────────────
Backends tested: {backends}
Fixtures processed: {count}
Total time: {duration}

BACKEND SCORES
┌──────────┬──────────┬──────────┬──────────┬──────────┐
│ Backend  │ Compile% │ Execute% │ Quality  │ Overall  │
├──────────┼──────────┼──────────┼──────────┼──────────┤
│ C        │   {c_c}% │   {c_e}% │ {c_q}/10 │ {c_o}/100│
│ JS       │  {js_c}% │  {js_e}% │{js_q}/10 │{js_o}/100│
│ Erlang   │ {erl_c}% │ {erl_e}% │{erl_q}/10│{erl_o}/100│
└──────────┴──────────┴──────────┴──────────┴──────────┘

COMPONENT HEALTH
───────────────────────────────────────────────────────────────
{component_table}

TOP ISSUES (Prioritized)
───────────────────────────────────────────────────────────────
1. [{severity}] {component}: {description}
   Fixture: {fixture_path}
   Recommendation: {action}

...

TREND (vs baseline)
───────────────────────────────────────────────────────────────
Overall: {delta} ({direction})
Regressions: {count}
Improvements: {count}

═══════════════════════════════════════════════════════════════
```

### Step 9: Save Artifacts

Save to `.score-codegen/`:
- `baselines/latest.json` - Current scores
- `reports/{timestamp}.md` - Detailed report
- `LATEST_SCORE.md` - Quick summary

---

## Agent Delegation

When executing, delegate to these specialized agents:

| Stage | Agent | Model |
|-------|-------|-------|
| Compile/Execute | Use Task with haiku | Fast, mechanical |
| Code Quality | Use main thread with sonnet | Needs judgment |
| Component Attribution | Use main thread with opus | Complex reasoning |
| Report Generation | Use main thread | Formatting |

---

**BEGIN EXECUTION NOW** with the parsed arguments.
