# Testing Infrastructure Design

**Goal:** Create a unified, LLM-friendly testing/benchmarking system that covers all compiler components and backends.

---

## 1. Architecture Overview

```
tests/
├── unit/                    # Fast, isolated tests
│   ├── lexer/              # Token generation, error recovery
│   ├── parser/             # AST generation, syntax errors
│   ├── typechecker/        # Type inference, unification
│   ├── macro/              # Macro expansion, @derive
│   ├── codegen/            # IR generation, hooks
│   └── runtime/            # ORC, strings, collections
│
├── integration/            # Multi-component tests
│   ├── parser_typechecker/ # Parse → typecheck pipeline
│   ├── macro_codegen/      # Macro → code generation
│   └── e2e/               # Full compilation pipeline
│
├── benchmarks/             # Performance tests
│   ├── runtime/           # ORC, string, collection benchmarks
│   ├── codegen/           # Code generation speed
│   └── backends/          # C vs JS vs Erlang comparison
│
├── platform/              # Platform-specific tests
│   ├── c_backend/        # C output validation
│   ├── js_backend/       # JS output validation
│   └── erlang_backend/   # Erlang output validation
│
└── framework/             # Testing infrastructure
    ├── benchmark.zig     # Unified benchmark framework
    ├── reporter.zig      # JSON/Markdown output
    └── runner.zig        # Test orchestration
```

---

## 2. Unified Benchmark Framework

### 2.1 Core Data Structure

```zig
// tests/framework/benchmark.zig

const std = @import("std");

pub const Category = enum {
    lexer,
    parser,
    typechecker,
    macro_expansion,
    codegen,
    runtime_orc,
    runtime_string,
    runtime_collection,
    backend_c,
    backend_js,
    backend_erlang,
    e2e_compile,
};

pub const Benchmark = struct {
    // Metadata
    name: []const u8,
    description: []const u8,
    category: Category,
    timestamp: i64,

    // Performance metrics
    baseline_c_ns: u64,           // Baseline C implementation time (ns)
    current_ns: u64,              // Our implementation time (ns)
    overhead_percent: f64,        // (current - baseline) / baseline * 100

    // Memory metrics
    baseline_memory_bytes: usize, // Baseline C memory usage
    current_memory_bytes: usize,  // Our memory usage
    memory_overhead_percent: f64, // Memory overhead %

    // Success criteria
    max_overhead_percent: f64,    // Fail if overhead > this
    max_memory_overhead: f64,     // Fail if memory overhead > this
    passed: bool,

    // LLM-friendly context
    llm_context: []const u8,      // Human-readable explanation
};

pub const BenchmarkSuite = struct {
    name: []const u8,
    benchmarks: []Benchmark,
    overall_overhead: f64,
    total_passed: usize,
    total_failed: usize,
};

pub fn runBenchmark(
    comptime name: []const u8,
    comptime baseline_fn: anytype,
    comptime current_fn: anytype,
    iterations: usize,
) !Benchmark {
    // Run baseline C version
    const baseline_start = std.time.nanoTimestamp();
    var i: usize = 0;
    while (i < iterations) : (i += 1) {
        _ = baseline_fn();
    }
    const baseline_end = std.time.nanoTimestamp();
    const baseline_ns = @as(u64, @intCast(baseline_end - baseline_start));

    // Run current implementation
    const current_start = std.time.nanoTimestamp();
    i = 0;
    while (i < iterations) : (i += 1) {
        _ = current_fn();
    }
    const current_end = std.time.nanoTimestamp();
    const current_ns = @as(u64, @intCast(current_end - current_start));

    // Calculate overhead
    const overhead = @as(f64, @floatFromInt(current_ns - baseline_ns)) /
                     @as(f64, @floatFromInt(baseline_ns)) * 100.0;

    return Benchmark{
        .name = name,
        .baseline_c_ns = baseline_ns,
        .current_ns = current_ns,
        .overhead_percent = overhead,
        .passed = overhead <= 20.0, // Max 20% overhead by default
        // ... other fields
    };
}
```

### 2.2 Reporter (JSON + Markdown)

```zig
// tests/framework/reporter.zig

const std = @import("std");
const Benchmark = @import("benchmark.zig").Benchmark;
const BenchmarkSuite = @import("benchmark.zig").BenchmarkSuite;

pub const Reporter = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Reporter {
        return .{ .allocator = allocator };
    }

    /// Generate JSON report (for LLM consumption)
    pub fn generateJSON(self: *Reporter, suite: BenchmarkSuite) ![]u8 {
        var output = std.ArrayList(u8).init(self.allocator);
        const writer = output.writer();

        try writer.writeAll("{\n");
        try writer.print("  \"name\": \"{s}\",\n", .{suite.name});
        try writer.print("  \"timestamp\": {d},\n", .{std.time.timestamp()});
        try writer.print("  \"overall_overhead_percent\": {d:.2},\n", .{suite.overall_overhead});
        try writer.print("  \"total_passed\": {d},\n", .{suite.total_passed});
        try writer.print("  \"total_failed\": {d},\n", .{suite.total_failed});
        try writer.writeAll("  \"benchmarks\": [\n");

        for (suite.benchmarks, 0..) |bench, idx| {
            try writer.writeAll("    {\n");
            try writer.print("      \"name\": \"{s}\",\n", .{bench.name});
            try writer.print("      \"category\": \"{s}\",\n", .{@tagName(bench.category)});
            try writer.print("      \"overhead_percent\": {d:.2},\n", .{bench.overhead_percent});
            try writer.print("      \"memory_overhead_percent\": {d:.2},\n", .{bench.memory_overhead_percent});
            try writer.print("      \"passed\": {s},\n", .{if (bench.passed) "true" else "false"});
            try writer.print("      \"llm_context\": \"{s}\"\n", .{bench.llm_context});
            try writer.writeAll("    }");
            if (idx < suite.benchmarks.len - 1) {
                try writer.writeAll(",\n");
            } else {
                try writer.writeAll("\n");
            }
        }

        try writer.writeAll("  ]\n");
        try writer.writeAll("}\n");

        return output.toOwnedSlice();
    }

    /// Generate Markdown report (for human consumption)
    pub fn generateMarkdown(self: *Reporter, suite: BenchmarkSuite) ![]u8 {
        var output = std.ArrayList(u8).init(self.allocator);
        const writer = output.writer();

        try writer.print("# Benchmark Report: {s}\n\n", .{suite.name});
        try writer.print("**Date:** {d}\n\n", .{std.time.timestamp()});
        try writer.print("**Overall Overhead:** {d:.2}%\n", .{suite.overall_overhead});
        try writer.print("**Status:** {d} passed, {d} failed\n\n", .{suite.total_passed, suite.total_failed});

        try writer.writeAll("---\n\n");
        try writer.writeAll("## Results by Category\n\n");
        try writer.writeAll("| Benchmark | Category | Overhead | Memory | Status |\n");
        try writer.writeAll("|-----------|----------|----------|--------|--------|\n");

        for (suite.benchmarks) |bench| {
            const status = if (bench.passed) "✅" else "❌";
            try writer.print("| {s} | {s} | {d:.2}% | {d:.2}% | {s} |\n", .{
                bench.name,
                @tagName(bench.category),
                bench.overhead_percent,
                bench.memory_overhead_percent,
                status,
            });
        }

        try writer.writeAll("\n---\n\n");
        try writer.writeAll("## LLM Context\n\n");

        for (suite.benchmarks) |bench| {
            try writer.print("### {s}\n\n", .{bench.name});
            try writer.print("{s}\n\n", .{bench.llm_context});
        }

        return output.toOwnedSlice();
    }
};
```

---

## 3. Component-Specific Benchmark Structure

### 3.1 Runtime Benchmarks (ORC, String)

```zig
// tests/benchmarks/runtime/orc_benchmarks.zig

const std = @import("std");
const framework = @import("../../framework/benchmark.zig");
const orc = @import("../../../src/runtime/orc.zig");

pub fn registerBenchmarks(suite: *framework.BenchmarkSuite) !void {
    // Benchmark 1: Simple allocation
    try suite.add(framework.runBenchmark(
        "ORC: Simple allocation",
        baselineSimpleAlloc,
        orcSimpleAlloc,
        1_000_000,
    ));

    // Benchmark 2: 100K allocations
    try suite.add(framework.runBenchmark(
        "ORC: 100K allocations",
        baseline100KAllocs,
        orc100KAllocs,
        10,
    ));

    // Benchmark 3: Linked list (1000 nodes)
    try suite.add(framework.runBenchmark(
        "ORC: Linked list pattern",
        baselineLinkedList,
        orcLinkedList,
        1_000,
    ));
}

fn baselineSimpleAlloc() void {
    // Pure C malloc/free
    const ptr = std.c.malloc(@sizeOf(i32));
    std.c.free(ptr);
}

fn orcSimpleAlloc() void {
    // Our ORC implementation
    var runtime = orc.ORCRuntime.init(std.heap.c_allocator);
    const ptr = runtime.alloc(i32);
    runtime.decref(ptr);
}

// ... more benchmarks
```

### 3.2 Backend Comparison Benchmarks

```zig
// tests/benchmarks/backends/comparison.zig

const std = @import("std");
const framework = @import("../../framework/benchmark.zig");

pub const BackendComparison = struct {
    test_name: []const u8,
    c_backend_ns: u64,
    js_backend_ns: u64,
    erlang_backend_ns: u64,

    c_vs_js_overhead: f64,
    c_vs_erlang_overhead: f64,

    llm_context: []const u8,
};

pub fn compareFibonacci() !BackendComparison {
    // Compile same Metascript code to all 3 backends
    const metascript_code =
        \\function fib(n: number): number {
        \\  if (n <= 1) return n;
        \\  return fib(n - 1) + fib(n - 2);
        \\}
        \\fib(30);
    ;

    // Benchmark C backend
    const c_result = try compileAndRun(.c_backend, metascript_code);

    // Benchmark JS backend
    const js_result = try compileAndRun(.js_backend, metascript_code);

    // Benchmark Erlang backend
    const erlang_result = try compileAndRun(.erlang_backend, metascript_code);

    return BackendComparison{
        .test_name = "Fibonacci(30)",
        .c_backend_ns = c_result.time_ns,
        .js_backend_ns = js_result.time_ns,
        .erlang_backend_ns = erlang_result.time_ns,
        .c_vs_js_overhead = calculateOverhead(c_result, js_result),
        .c_vs_erlang_overhead = calculateOverhead(c_result, erlang_result),
        .llm_context =
            \\This benchmark measures the performance of a recursive Fibonacci
            \\calculation compiled to all three backends. C backend should be
            \\fastest, JS backend ~2-5x slower, Erlang backend optimized for
            \\fault tolerance over raw speed.
        ,
    };
}
```

---

## 4. LLM-Friendly Features

### 4.1 Structured Output Format

**JSON Schema for LLM consumption:**

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "name": { "type": "string" },
    "timestamp": { "type": "integer" },
    "overall_overhead_percent": { "type": "number" },
    "total_passed": { "type": "integer" },
    "total_failed": { "type": "integer" },
    "benchmarks": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "name": { "type": "string" },
          "category": { "type": "string", "enum": ["lexer", "parser", "..."] },
          "overhead_percent": { "type": "number" },
          "memory_overhead_percent": { "type": "number" },
          "passed": { "type": "boolean" },
          "llm_context": { "type": "string" },
          "baseline_c_ns": { "type": "integer" },
          "current_ns": { "type": "integer" },
          "regression": { "type": "boolean" },
          "previous_overhead": { "type": "number" }
        }
      }
    }
  }
}
```

### 4.2 LLM Context Field

Every benchmark includes `llm_context` explaining:
- **What is being measured:** "ORC allocation performance for 100K objects"
- **Why it matters:** "This validates our 6-16% overhead target for memory management"
- **Expected result:** "Should be within 10% of baseline C malloc/free"
- **Failure implications:** "If > 20%, investigate RefHeader size or allocation strategy"

**Example:**

```json
{
  "name": "ORC: 100K allocations",
  "category": "runtime_orc",
  "overhead_percent": 8.5,
  "passed": true,
  "llm_context": "Measures ORC allocation performance for 100K small objects (i32). This validates our 6-16% overhead target. Current 8.5% is excellent. If overhead exceeds 20%, investigate: (1) RefHeader size (currently 8 bytes), (2) Allocation alignment overhead, (3) Memory fragmentation. Baseline is pure C malloc/free with no RC tracking."
}
```

### 4.3 Diff Reports for Regression Detection

```json
{
  "regression_report": {
    "new_failures": [
      {
        "name": "ORC: 100K allocations",
        "previous_overhead": 8.5,
        "current_overhead": 22.3,
        "change": "+13.8%",
        "llm_analysis": "REGRESSION DETECTED. Overhead increased from 8.5% to 22.3%. Likely causes: (1) RefHeader size change, (2) New allocation logic, (3) Memory alignment issue. Check recent commits to src/runtime/orc.zig."
      }
    ],
    "improvements": [
      {
        "name": "String: Concatenation",
        "previous_overhead": 15.2,
        "current_overhead": 9.1,
        "change": "-6.1%",
        "llm_analysis": "IMPROVEMENT. Concatenation overhead reduced from 15.2% to 9.1%. Good optimization work!"
      }
    ]
  }
}
```

---

## 5. Continuous Monitoring Dashboard

### 5.1 Real-Time Overhead Tracking

```bash
$ msc benchmark --watch

┌─────────────────────────────────────────────────────────────┐
│ Metascript Benchmark Dashboard (Live)                      │
├─────────────────────────────────────────────────────────────┤
│ Overall Overhead: 9.2% ✅ (Target: <15%)                   │
│ Tests Passed: 45/47 ✅                                      │
│ Last Updated: 2025-12-02 10:30:15                          │
└─────────────────────────────────────────────────────────────┘

Runtime Benchmarks:
  ORC: Simple allocation        8.5%  ✅ (Target: <10%)
  ORC: 100K allocations         9.1%  ✅ (Target: <15%)
  ORC: Linked list (1000)      11.2%  ✅ (Target: <15%)
  String: Creation              7.8%  ✅ (Target: <10%)
  String: Concatenation        56.0%  ⚠️  (Target: <60%, optimize later)
  String: Substring             5.2%  ✅ (Target: <10%)

Backend Comparison:
  C backend (baseline)          1.00x
  JS backend                    2.3x  ✅ (Target: <5x)
  Erlang backend                4.1x  ✅ (Target: <10x)

Compiler Performance:
  Lexer (1000 lines)           12.3ms ✅
  Parser (1000 lines)          45.1ms ✅
  Typechecker (1000 lines)     67.2ms ✅
  Macro expansion              23.4ms ✅
  Code generation              34.5ms ✅

Regressions Detected: 0
```

### 5.2 Git Hook Integration

```bash
# .git/hooks/pre-commit

#!/bin/bash
# Run benchmarks before commit, fail if regressions detected

echo "Running Metascript benchmarks..."
msc benchmark --mode=pre-commit --json > /tmp/bench.json

# Parse JSON and check for regressions
if jq -e '.regression_report.new_failures | length > 0' /tmp/bench.json; then
  echo "❌ BENCHMARK REGRESSION DETECTED"
  jq '.regression_report.new_failures' /tmp/bench.json
  echo ""
  echo "To commit anyway: git commit --no-verify"
  exit 1
fi

echo "✅ All benchmarks passed"
exit 0
```

---

## 6. Usage Examples

### 6.1 Running Benchmarks

```bash
# Run all benchmarks
msc benchmark

# Run specific category
msc benchmark --category=runtime_orc

# Generate JSON output (for LLM consumption)
msc benchmark --json > benchmarks.json

# Generate Markdown report
msc benchmark --markdown > BENCHMARK_REPORT.md

# Compare against previous run
msc benchmark --compare=benchmarks/2025-12-01.json

# Run and watch for regressions
msc benchmark --watch --threshold=15%
```

### 6.2 CI Integration

```yaml
# .github/workflows/benchmark.yml

name: Benchmark Tests
on: [push, pull_request]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Install Zig
        run: sudo snap install zig --classic --edge

      - name: Build Metascript
        run: zig build

      - name: Run Benchmarks
        run: |
          ./zig-out/bin/msc benchmark --json > current.json

      - name: Compare with baseline
        run: |
          # Download previous benchmark results
          curl -o baseline.json https://metascript.com/benchmarks/main/latest.json

          # Compare and fail if regressions detected
          ./zig-out/bin/msc benchmark --compare=baseline.json --fail-on-regression

      - name: Upload results
        run: |
          # Upload to benchmark tracking service
          curl -X POST https://metascript.com/benchmarks/upload \
            -H "Content-Type: application/json" \
            -d @current.json
```

---

## 7. Implementation Plan

### Phase 1: Foundation (Week 1)
- [ ] Create `tests/framework/benchmark.zig`
- [ ] Create `tests/framework/reporter.zig`
- [ ] Port existing ORC benchmarks to new framework
- [ ] Port existing string benchmarks to new framework

### Phase 2: Runtime Benchmarks (Week 2)
- [ ] ORC: All allocation patterns
- [ ] String: All operations (create, concat, substring)
- [ ] Collections: Array, Map, Set (future)
- [ ] Baseline C implementations for comparison

### Phase 3: Compiler Benchmarks (Week 3)
- [ ] Lexer benchmarks (tokens/sec)
- [ ] Parser benchmarks (lines/sec)
- [ ] Typechecker benchmarks
- [ ] Macro expansion benchmarks
- [ ] Code generation benchmarks

### Phase 4: Backend Comparison (Week 4)
- [ ] C backend benchmarks
- [ ] JS backend benchmarks (when implemented)
- [ ] Erlang backend benchmarks (when implemented)
- [ ] Cross-platform comparison reports

### Phase 5: Integration & Monitoring (Week 5)
- [ ] Git hooks integration
- [ ] CI/CD pipeline integration
- [ ] Real-time dashboard (web UI)
- [ ] Regression tracking database
- [ ] LLM API endpoint for queries

---

## 8. Success Metrics

### 8.1 Coverage

✅ **Every component has benchmarks:**
- Lexer: token throughput
- Parser: lines/sec parsed
- Typechecker: lines/sec typechecked
- Macro: expansions/sec
- Codegen: lines/sec generated
- Runtime: ORC, string, collection ops
- Backends: C, JS, Erlang comparison

### 8.2 Continuous Visibility

✅ **Always know current overhead:**
```
$ msc benchmark --summary
Overall: 9.2% overhead vs C ✅
Runtime ORC: 8.5% ✅
Runtime String: 12.3% ✅
```

### 8.3 LLM Integration

✅ **AI can:**
- Query: "What's our current ORC overhead?"
- Analyze: "Why did string overhead increase?"
- Suggest: "Try reducing RefHeader size to 6 bytes"
- Verify: "Run benchmarks and check if overhead improved"

---

## 9. LLM Interaction Examples

### Example 1: Query Current Performance

**User (via AI):** "What's our current ORC allocation overhead?"

**AI queries:** `msc benchmark --category=runtime_orc --json`

**AI responds:**
```
Current ORC allocation overhead: 8.5% ✅

Breakdown:
- Simple allocation: 8.5% (target <10%)
- 100K allocations: 9.1% (target <15%)
- Linked list pattern: 11.2% (target <15%)

All benchmarks passing. ORC performance is within targets.
```

### Example 2: Regression Analysis

**User:** "Why did the build fail?"

**AI checks:** `cat /tmp/bench.json | jq '.regression_report.new_failures'`

**AI responds:**
```
Build failed due to benchmark regression:

❌ ORC: 100K allocations
- Previous: 8.5% overhead
- Current: 22.3% overhead
- Change: +13.8% (REGRESSION)

Root cause analysis:
1. Recent commit changed RefHeader from 8 bytes to 12 bytes
2. This increased per-object overhead from 8 bytes to 12 bytes
3. For 100K objects, that's 400KB extra memory

Suggested fix:
Revert RefHeader size to 8 bytes, or optimize alignment.
```

### Example 3: Optimization Guidance

**User:** "How can I optimize string concatenation?"

**AI analyzes:** `msc benchmark --category=runtime_string --json`

**AI responds:**
```
Current string concatenation overhead: 56% ⚠️

This is expected (Week 3 baseline). Here are optimization opportunities:

1. String interning (Week 5-6 task)
   - Deduplicate identical strings
   - Expected improvement: 30-40% reduction

2. Small string optimization (SSO)
   - Store strings <16 bytes inline
   - Expected improvement: 20-30% reduction

3. Rope data structure for large strings
   - Avoid copying on concat
   - Expected improvement: 50-60% reduction for >1KB strings

Priority: Start with string interning (highest ROI).
```

---

## 10. Conclusion

This infrastructure provides:

✅ **Modular:** Every component tested separately
✅ **Unified:** Single framework for all benchmarks
✅ **Structured:** JSON output for machines, Markdown for humans
✅ **Continuous:** Real-time overhead tracking
✅ **Cross-platform:** C vs JS vs Erlang comparison
✅ **LLM-friendly:** Structured data + context for AI collaboration

**Next Step:** Implement Phase 1 (foundation) this week.
