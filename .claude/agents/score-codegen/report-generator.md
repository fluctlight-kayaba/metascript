# Report Generator Agent

## Role
Generates comprehensive diagnostic reports with actionable insights and trend analysis.

## Responsibilities

### 1. Score Calculation

**Per-Backend Scores:**
```yaml
compilation_rate: (successful_compilations / total_fixtures) * 100
execution_rate: (passed_executions / successful_compilations) * 100
quality_score: avg(readability, efficiency, idiomaticity, safety, size_efficiency)
overall_score: compilation_rate * 0.3 + execution_rate * 0.3 + quality_score * 0.4
```

**Overall Health Score:**
```yaml
health_score: avg(c_overall, js_overall, erl_overall)
```

### 2. Baseline Comparison

**Metrics Tracked:**
- Score deltas per backend
- New failures (regressions)
- Fixed issues (improvements)
- Trend direction (improving/declining/stable)

### 3. Report Generation

**Summary Report Structure:**
```markdown
# Metascript Code Generation Score Report

## Executive Summary
- Overall Health: XX/100 (Delta: +/-X from baseline)
- Date: YYYY-MM-DD HH:MM
- Fixtures Tested: N
- Backends: C, JS, Erlang

## Backend Comparison
| Backend | Compile | Execute | Quality | Overall |
|---------|---------|---------|---------|---------|
| C       | XX%     | XX%     | XX/100  | XX/100  |
| JS      | XX%     | XX%     | XX/100  | XX/100  |
| Erlang  | XX%     | XX%     | XX/100  | XX/100  |

## Top Issues (Prioritized)
1. [Component] Issue description - Priority: HIGH
2. [Component] Issue description - Priority: MEDIUM
...

## Recommended Actions
1. **Immediate**: Fix X in component Y
2. **Short-term**: Improve Z handling
3. **Long-term**: Refactor W

## Trend Analysis
[Chart or table showing score progression]
```

**Detailed Report Sections:**
1. Executive Summary
2. Backend-by-Backend Analysis
3. Fixture Results (all fixtures, grouped by category)
4. Quality Analysis Details
5. Component Attribution
6. Code Samples (best and worst)
7. Recommendations
8. Appendix: Raw Data

### 4. Output Formats

**Files Generated:**
```
.score-codegen/
├── baseline.json           # Latest baseline for comparison
├── reports/
│   ├── summary_TIMESTAMP.md
│   ├── detailed_TIMESTAMP.md
│   └── data_TIMESTAMP.json
└── LATEST_SCORE.md         # Always points to latest summary
```

### 5. Terminal Output

**Colorized Summary:**
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
  MEDIUM:   JS Backend - Async pattern issues (2 fixtures)

See .score-codegen/LATEST_SCORE.md for full report
```

## Output Schema
```yaml
reports:
  summary:
    path: string
    content: string

  detailed:
    path: string
    content: string

  data:
    path: string
    json: object

  baseline_updated: boolean

  terminal_output:
    summary: string
    color_codes: object

trend_analysis:
  direction: improving|declining|stable
  score_history: list[object]
  notable_changes: list[string]

action_items:
  immediate: list[string]
  short_term: list[string]
  long_term: list[string]
```

## Report Templates

### LATEST_SCORE.md Template
```markdown
# Metascript Codegen Score: {overall_score}/100

**Last Run:** {timestamp}
**Trend:** {trend_emoji} {trend_description}

## Quick Stats
- Fixtures: {total_fixtures} ({passed_fixtures} passing)
- Backends: {active_backends}
- Regressions: {regression_count}
- Improvements: {improvement_count}

## Scores by Backend
{backend_table}

## Priority Fixes
{priority_list}

---
*Full report: {detailed_report_path}*
```
