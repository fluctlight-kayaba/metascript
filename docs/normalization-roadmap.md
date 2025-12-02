# AST Normalization: Revised Implementation Roadmap

**Created:** 2025-12-02
**Status:** Phase 1 Complete, Phases 2-5 Pending
**Based on:** Principal Engineer Review

---

## Current Reality Check

**What We Have (Phase 1):**
- ✅ Framework infrastructure (normalize.zig - 329 lines)
- ✅ Pipeline integration (Phase 2.5 in compile.zig)
- ✅ Stats tracking and observability
- ✅ 5 framework tests (272/272 tests passing)
- ✅ Architecture documentation (600+ lines)

**What We DON'T Have:**
- ❌ Actual transformation implementations (all are TODOs)
- ❌ Semantic equivalence testing
- ❌ RC elimination benchmarks
- ❌ Parser support for spread elements
- ❌ Type checker integration

**Current RC Elimination Rate:** **0%** (transformations are NOPs)

---

## Revised Phase Breakdown

### Phase 1: Infrastructure ✅ COMPLETE (Week 5-6 Day 1-2)

**Status:** Done
**Deliverables:**
- [x] Normalization framework (NormalizeContext, 3 passes)
- [x] Pipeline integration (Phase 2.5)
- [x] Stats tracking
- [x] Framework tests (5 tests)
- [x] Architecture documentation

**Result:** Compilation pipeline includes normalization, but it does nothing yet.

---

### Phase 2: Immediate Fixes 🔴 HIGH PRIORITY (Week 5-6 Day 3)

**Goal:** Address critical gaps from PE review

**Tasks:**

1. **Update Documentation Status** (30 min)
   - Add "STATUS: FRAMEWORK ONLY - TRANSFORMATIONS NOT IMPLEMENTED" to normalization-architecture.md
   - Update README with honest status
   - Clarify expected vs. actual RC elimination

2. **Add Safety Feature Flag** (1 hour)
   - Implement `--no-normalize` CLI flag
   - Allow disabling normalization for debugging
   - Add to help text and documentation

3. **Measure Current Overhead** (2 hours)
   - Benchmark compilation time with/without normalization
   - Document overhead (target: <5% for NOPs)
   - Add to CI to track regressions

**Success Criteria:**
- ✅ Docs reflect implementation reality
- ✅ Escape hatch exists (--no-normalize)
- ✅ Overhead measured and acceptable

---

### Phase 3: Type Checker Integration 🟡 MEDIUM PRIORITY (Week 5-6 Day 4-5)

**Goal:** Enable normalization to query type information

**Blocker for:** Pass 1 (object spread normalization)

**Tasks:**

1. **Add TypeChecker Reference** (1 hour)
   ```zig
   pub const NormalizeContext = struct {
       arena: *ast.ASTArena,
       allocator: std.mem.Allocator,
       type_checker: ?*checker.TypeChecker,  // NEW
       stats: Stats,
   };
   ```

2. **Implement getTypeOf() Helper** (2 hours)
   ```zig
   pub fn getTypeOf(self: *NormalizeContext, node: *ast.Node) ?*types.Type {
       return if (self.type_checker) |tc| node.type else null;
   }
   ```

3. **Add getFields() Helper** (2 hours)
   ```zig
   pub fn getFields(self: *NormalizeContext, typ: *types.Type) ?[]FieldInfo {
       // Query type's fields for spread expansion
   }
   ```

4. **Update Pipeline Integration** (1 hour)
   - Pass TypeChecker to NormalizeContext in compile.zig
   - Ensure type checking runs before normalization (or interleaved)

**Success Criteria:**
- ✅ NormalizeContext can query node types
- ✅ NormalizeContext can get object fields
- ✅ Tests validate type queries work

---

### Phase 4: Parser Extensions 🟡 MEDIUM PRIORITY (Week 5-6 Day 6-7)

**Goal:** Add AST support for spread elements

**Blocker for:** Pass 1 implementation

**Tasks:**

1. **Add SpreadElement Node Kind** (2 hours)
   ```zig
   // src/ast/node.zig
   pub const NodeKind = enum {
       // ...
       spread_element,  // ...expr
   };

   pub const SpreadElement = struct {
       argument: *Node,  // The expression being spread
   };
   ```

2. **Parse Spread Syntax** (4 hours)
   ```typescript
   // Parser must handle:
   const obj = { ...source };           // Object spread
   const arr = [...source];             // Array spread (future)
   func(...args);                       // Function args (future)
   ```

3. **Update ObjectExpr to Support Spreads** (2 hours)
   ```zig
   pub const ObjectProperty = union(enum) {
       property: struct {
           key: *Node,
           value: *Node,
           shorthand: bool,
       },
       spread: *Node,  // NEW: spread element
   };
   ```

4. **Add Parser Tests** (2 hours)
   - Test: `{ ...obj }` parses correctly
   - Test: `{ a: 1, ...obj, b: 2 }` (ordering)
   - Test: Multiple spreads

**Success Criteria:**
- ✅ Parser recognizes `...expr` syntax
- ✅ AST contains SpreadElement nodes
- ✅ 10+ parser tests for spread syntax

---

### Phase 5: Pass 1 Implementation 🟢 CORE FEATURE (Week 6-7 Day 1-3)

**Goal:** Implement object spread normalization

**Dependencies:** Phase 3 (type integration), Phase 4 (parser)

**Tasks:**

1. **Implement Spread Detection** (2 hours)
   ```zig
   fn normalizeObjectSpreads(...) !*ast.Node {
       switch (node.kind) {
           .object_expr => {
               const obj_expr = &node.data.object_expr;

               // Check for spread properties
               var has_spread = false;
               for (obj_expr.properties) |prop| {
                   if (prop == .spread) {
                       has_spread = true;
                       break;
                   }
               }

               if (!has_spread) return node;

               // Expand spreads...
           },
       }
   }
   ```

2. **Implement Type-Driven Field Expansion** (6 hours)
   ```zig
   // For each spread:
   // 1. Get type of spread expression
   // 2. Query fields from type
   // 3. Generate explicit field accesses
   // 4. Handle field ordering and overwrites
   ```

3. **Handle Edge Cases** (4 hours)
   - Computed properties: `{ ...obj, [key]: value }`
   - Multiple spreads: `{ ...a, ...b, ...c }`
   - Overwrites: `{ ...obj, x: 2 }` (x should be 2, not obj.x)
   - Unknown types: Fallback to keeping spread

4. **Update Stats Tracking** (1 hour)
   - Uncomment `ctx.stats.object_spreads_normalized += 1`
   - Track success vs. fallback rate

**Success Criteria:**
- ✅ Spreads expand to explicit field accesses
- ✅ Field ordering correct (later fields overwrite earlier)
- ✅ Stats show normalization count
- ✅ 20+ unit tests for spread normalization

---

### Phase 6: Semantic Equivalence Testing 🔴 CRITICAL (Week 6-7 Day 4-5)

**Goal:** Prove normalization preserves program semantics

**Validates:** All transformations produce correct output

**Tasks:**

1. **Create Test Execution Framework** (4 hours)
   ```zig
   fn executeAndCompare(source: []const u8, test_env: TestEnv) !void {
       const original = parse(source);
       const normalized = normalize(original);

       const orig_result = execute(original, test_env);
       const norm_result = execute(normalized, test_env);

       try testing.expectEqual(orig_result, norm_result);
   }
   ```

2. **Add Interpreter or Codegen Testing** (6 hours)
   - Option A: Simple AST interpreter for testing
   - Option B: Compile both to JS, execute, compare output
   - Recommendation: Start with Option B (use existing JS backend)

3. **Write Semantic Tests** (4 hours)
   ```zig
   test "semantic: object spread with overwrite" {
       const source =
           \\const a = { x: 1, y: 2 };
           \\const b = { y: 3, z: 4 };
           \\const c = { ...a, ...b };
           \\console.log(c.y);  // Should be 3, not 2
       ;
       try executeAndCompare(source, test_env);
   }
   ```

4. **Property-Based Testing** (4 hours)
   - Generate random object spreads
   - Normalize and validate equivalence
   - Catch edge cases

**Success Criteria:**
- ✅ 50+ semantic equivalence tests
- ✅ All tests pass (normalized = original behavior)
- ✅ Property-based tests catch edge cases

---

### Phase 7: RC Elimination Benchmarks 🟢 VALIDATION (Week 6-7 Day 6-7)

**Goal:** Measure actual RC elimination rate

**Validates:** 85-90% target is achievable

**Tasks:**

1. **Create RC Counting Infrastructure** (4 hours)
   ```zig
   fn countRCOps(ast: *ast.Node) usize {
       var count: usize = 0;
       // Walk AST, count:
       // - incref operations
       // - decref operations
       // - clone operations
       return count;
   }
   ```

2. **Add Spread Benchmark** (2 hours)
   ```zig
   test "benchmark: object spread RC elimination" {
       const source = "const merged = { ...obj1, ...obj2 };";

       const baseline = countRCOps(compile(source, .no_normalize));
       const optimized = countRCOps(compile(source, .with_normalize));

       const reduction = (baseline - optimized) / baseline;

       // Target: 90-100% elimination for spreads
       try testing.expect(reduction >= 0.90);
   }
   ```

3. **Add Integration Benchmarks** (4 hours)
   - Real-world code samples
   - Mix of spreads, chains, closures
   - Measure overall reduction

4. **Add to CI** (2 hours)
   - Run benchmarks on every PR
   - Fail if reduction drops below 80%
   - Track metrics over time

**Success Criteria:**
- ✅ Infrastructure counts RC ops accurately
- ✅ Object spreads achieve 90%+ elimination
- ✅ CI tracks RC elimination rate
- ✅ Metrics dashboard shows progress

---

### Phase 8: Pass 2 & 3 Implementation 🟡 MEDIUM PRIORITY (Week 7 Day 1-5)

**Goal:** Implement array chain fusion and closure inlining

**Similar process to Phase 5:**
1. Implement transformation
2. Add semantic tests
3. Add RC benchmarks
4. Validate targets

**Array Chain Fusion Target:** 100% (no intermediate arrays)
**Closure Inlining Target:** 50-80% (some closures must stay)

---

### Phase 9: Production Readiness 🟢 POLISH (Week 7 Day 6-7)

**Goal:** Make normalization production-ready

**Tasks:**

1. **Refine Error Handling** (2 hours)
   - Distinguish fatal (OOM) vs. recoverable errors
   - Better error messages

2. **Add Detailed Logging** (3 hours)
   - Debug mode: log every transformation
   - Track why normalizations fail

3. **Performance Optimization** (4 hours)
   - Profile normalization overhead
   - Optimize hot paths
   - Target: <2% compile time increase

4. **User Documentation** (3 hours)
   - When normalization helps
   - How to debug normalization issues
   - Performance characteristics

**Success Criteria:**
- ✅ Production-quality error handling
- ✅ Debug logging helps troubleshoot
- ✅ Overhead <2% measured
- ✅ User docs complete

---

## Success Metrics

### Phase 1 (Infrastructure) ✅
- [x] Framework compiles
- [x] Tests pass (272/272)
- [x] Integrated into pipeline
- **RC Elimination:** 0% (expected)

### Phase 5 (Pass 1 Complete)
- [ ] Object spreads normalize correctly
- [ ] Semantic tests pass (50+)
- [ ] RC benchmarks pass (>90%)
- **RC Elimination:** 30-50% (spreads only)

### Phase 8 (All Passes Complete)
- [ ] All 3 passes implemented
- [ ] All semantic tests pass (100+)
- [ ] All benchmarks pass
- **RC Elimination:** 85-90% (target)

### Phase 9 (Production Ready)
- [ ] Overhead <2%
- [ ] Error handling robust
- [ ] Documentation complete
- **RC Elimination:** 85-90% (validated)

---

## Dependency Graph

```
Phase 1: Infrastructure ✅
         ↓
    ┌────┴────┐
    ↓         ↓
Phase 2:   Phase 3:
Immediate  TypeChecker
Fixes      Integration
    ↓         ↓
    └────┬────┘
         ↓
    Phase 4:
    Parser
    Extensions
         ↓
    Phase 5:
    Pass 1
    Implementation
         ↓
    ┌────┴────┐
    ↓         ↓
Phase 6:   Phase 7:
Semantic   RC
Testing    Benchmarks
    ↓         ↓
    └────┬────┘
         ↓
    Phase 8:
    Pass 2 & 3
         ↓
    Phase 9:
    Production
    Readiness
```

---

## Timeline Estimate

| Phase | Days | Status |
|-------|------|--------|
| Phase 1: Infrastructure | 2 | ✅ Complete |
| Phase 2: Immediate Fixes | 1 | 🔴 Not Started |
| Phase 3: Type Integration | 2 | 🔴 Not Started |
| Phase 4: Parser Extensions | 2 | 🔴 Not Started |
| Phase 5: Pass 1 Implementation | 3 | 🔴 Blocked |
| Phase 6: Semantic Testing | 2 | 🔴 Blocked |
| Phase 7: RC Benchmarks | 2 | 🔴 Blocked |
| Phase 8: Pass 2 & 3 | 5 | 🔴 Blocked |
| Phase 9: Production Readiness | 2 | 🔴 Blocked |

**Total:** ~21 days (4 weeks)

**Realistic Timeline:**
- Week 5-6 (Current): Phases 1-2 ✅
- Week 6-7: Phases 3-5 (Pass 1 complete)
- Week 7-8: Phases 6-7 (Validation)
- Week 8-9: Phases 8-9 (Full implementation)

---

## Risk Mitigation

### Risk 1: Semantic Bugs in Transformations
**Likelihood:** High
**Impact:** Critical (silent code corruption)
**Mitigation:** Semantic equivalence testing (Phase 6) before shipping

### Risk 2: RC Elimination Below Target
**Likelihood:** Medium
**Impact:** High (doesn't justify complexity)
**Mitigation:** Benchmarks (Phase 7) validate before claiming success

### Risk 3: Parser Changes Break Existing Code
**Likelihood:** Low
**Impact:** High
**Mitigation:** Extensive parser tests, backward compatibility

### Risk 4: Type Integration Complexity
**Likelihood:** Medium
**Impact:** Medium (delays Pass 1)
**Mitigation:** Phase 3 addresses early, simple API design

---

## Next Actions (This Week)

**Immediate:**
1. ✅ Complete Phase 1 (Done!)
2. Start Phase 2: Update docs, add --no-normalize flag
3. Measure current overhead

**This Week:**
4. Phase 3: Type checker integration design
5. Phase 4: Start parser extensions (SpreadElement)

**Next Week:**
6. Phase 5: Implement Pass 1 (object spreads)
7. Phase 6: Start semantic testing

---

**Status:** Honest assessment complete. Clear path forward defined. 🚀

**Grade:** Infrastructure A+, Implementation 0%, Roadmap A
