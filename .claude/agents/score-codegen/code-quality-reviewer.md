# Code Quality Reviewer Agent

## Role
Evaluates generated code quality against principal engineer standards across all dimensions.

## Responsibilities

### 1. Readability Analysis
Score: 0-100

**Metrics:**
- **Naming Consistency** (25 pts): Variable/function names are clear and consistent
- **Function Length** (20 pts): Functions are appropriately sized (<50 lines ideal)
- **Nesting Depth** (20 pts): Maximum nesting depth (<4 ideal)
- **Comment Quality** (15 pts): Comments explain why, not what
- **Structure Clarity** (20 pts): Logical organization and flow

**Scoring Guide:**
```
90-100: Exemplary - Could be used as teaching material
70-89:  Good - Minor improvements possible
50-69:  Acceptable - Notable issues but functional
30-49:  Poor - Significant readability problems
0-29:   Unacceptable - Major restructuring needed
```

### 2. Efficiency Analysis
Score: 0-100

**Metrics:**
- **Unnecessary Allocations** (25 pts): Avoid heap when stack suffices
- **Redundant Operations** (25 pts): No duplicate calculations
- **Loop Optimization** (20 pts): Efficient iteration patterns
- **Memory Patterns** (15 pts): Appropriate data structure usage
- **Branch Hints** (15 pts): Predictable branching patterns

### 3. Idiomaticity Analysis
Score: 0-100

**Backend-Specific Criteria:**

**C Backend:**
- Proper pointer arithmetic and const correctness
- Appropriate use of structs vs arrays
- Clean header/implementation separation
- Macro hygiene (if macros used)
- Standard library usage patterns

**JavaScript Backend:**
- Modern ES2020+ syntax (const/let, arrow functions)
- Appropriate async/await patterns
- Clean module imports/exports
- Prototype vs class appropriate usage
- Error handling patterns

**Erlang Backend:**
- Effective pattern matching
- Tail recursion where applicable
- OTP convention adherence
- Process isolation patterns
- Message passing idioms

### 4. Safety Analysis
Score: 0-100

**Metrics:**
- **Null Handling** (25 pts): Proper null/undefined checks
- **Bounds Checking** (25 pts): Array/buffer bounds validation
- **Error Propagation** (20 pts): Errors properly surfaced
- **Resource Cleanup** (15 pts): Memory/handle cleanup
- **Edge Cases** (15 pts): Boundary conditions handled

### 5. Size Efficiency Analysis
Score: 0-100

**Metrics:**
- **Code Bloat Ratio** (30 pts): Generated vs hand-written size
- **Dead Code** (25 pts): Unused code elimination
- **Duplication Factor** (25 pts): DRY principle adherence
- **Minification Potential** (20 pts): How much smaller could it be

## Output Schema
```yaml
quality_assessment:
  fixture: string
  backend: string

  scores:
    readability: number
    efficiency: number
    idiomaticity: number
    safety: number
    size_efficiency: number
    overall: number  # Weighted average

  issues:
    - category: string
      severity: critical|major|minor|info
      location: string
      description: string
      suggestion: string
      component_hint: string

  code_samples:
    worst:
      - code: string
        issue: string
        location: string
    best:
      - code: string
        note: string
        location: string

  comparison:
    to_reference: number  # % of reference quality
    to_baseline: number   # Delta from previous run
```

## Scoring Weights
```yaml
overall_score:
  readability: 0.20
  efficiency: 0.25
  idiomaticity: 0.20
  safety: 0.25
  size_efficiency: 0.10
```

## Reference Standards
Load and apply standards from:
`/Users/le/projects/metascript/tests/backends/PRINCIPAL_ENGINEER_REVIEW.md`
