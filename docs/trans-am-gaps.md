# Trans-Am Implementation Gaps

**Created:** December 2024
**Status:** Tracking document for Principal Engineer review findings

---

## Summary

Trans-Am Phases 4-5 deliver real value (fast LSP responses, caching framework). Phases 2-3 are partial implementations waiting for parser/macro VM integration.

| What Works | What's Scaffolding |
|------------|-------------------|
| LSP semantic tokens (0ms) | Macro firewall behavior |
| Diagnostics caching | Async expansion |
| Revision tracking | Parser integration |
| Red-green state machine | Content-addressed macro caching |
| tryMarkGreen() + cycle detection | Incremental re-expansion |

---

## Critical Issues

### Issue #1: Macro Firewall Incomplete

**Location:** `src/transam/transam.zig:1632`

**Problem:**
```zig
// getMacroCallSites returns stub arguments
.arguments = &[_][]const u8{}, // TODO: parse arguments
```

**Architecture says:**
```
macroArg() → macroExpander() → macroExpand()
```

**Implementation has:**
- `getMacroCallSites()` finds macro tokens but doesn't parse arguments
- `macroArg()` returns stub `MacroArgs` (line 1112)
- `macroExpander()` looks up but nothing to expand
- No actual macro expansion execution

**Required:**
1. Parse macro arguments from AST (needs parser integration)
2. Wire up to macro VM for actual expansion
3. Implement content-addressed output caching

**Note:** Parser ALREADY parses decorators! (`parseDecorator()` at line 137)
- `@derive(Eq, Hash)` → `MacroInvocation` node with arguments
- Arguments extracted into `MacroArgument` structs
- **Blocked by:** Wiring parser output to Trans-Am queries

---

### Issue #2: No Parser Integration

**Location:** `src/transam/transam.zig:1160-1310`

**Problem:**
- `getSyntaxTokens()` uses lexer directly, not parser
- `getDiagnostics()` only catches lexer errors, not parse/type errors
- Query dependency graph from docs not wired up:
  ```
  file_text → parse → macro_call_sites → macro_expand → ...
  ```

**Current flow:**
```
file_text → lexer → tokens (AST bypassed!)
```

**Required flow:**
```
file_text → lexer → parser → AST → semantic tokens
                         ↓
                  macro_call_sites → macro_expand
                         ↓
                  type_checker → diagnostics
```

**Required:**
1. Add `parse(file_id) → AST` query
2. Wire `getSyntaxTokens()` to use parsed AST for semantic info
3. Wire `getDiagnostics()` to include parse errors
4. Wire `getMacroCallSites()` to extract from AST nodes

**Blocked by:** Parser completion (separate workstream)

---

### Issue #3: Async Expansion is Synchronous

**Location:** `src/transam/transam.zig:1354-1355`

**Problem:**
```zig
// In startAsyncMacroExpansion:
if (self.async_tasks.getPtr(handle.id)) |task_ptr| {
    task_ptr.status = .completed;  // Immediately marked complete!
}
```

**Impact:**
- `waitForExpansion()` is a no-op
- Progress callbacks never fire with real data
- No actual threading

**Required:**
1. Spawn actual `std.Thread` for macro expansion
2. Report progress via callbacks
3. Support cancellation properly

**Blocked by:** Issue #1 (no macros to expand)

**Priority:** Low until Issues #1-2 resolved

---

## Moderate Concerns

### Issue #4: Memory Management Gaps

**Location:** `src/transam/transam.zig` (storeQueryResult)

**Problem:**
```zig
// Old value memory leak - would need type-aware cleanup
// CachedSyntaxTokens tokens are duped but originals may leak
```

**Required:**
1. Track allocations per QueryValue
2. Add type-erased cleanup function
3. Free old values when overwriting cache entries

**Priority:** Medium (production concern)

---

### Issue #5: Content-Addressing Not Used

**Location:** `src/transam/transam.zig:700-750`

**Problem:**
- `hashAstNode()` exists but nothing calls it
- `MacroOutputCache.store()` requires AST nodes we don't have
- Key insight never triggers: "output unchanged → dependents stay GREEN"

**Required:**
1. Call `hashAstNode()` on macro expansion output
2. Compare hashes before storing
3. Skip dependent invalidation when output unchanged

**Blocked by:** Issue #1 (no macro output to hash)

---

### Issue #6: Tests Don't Validate Core Claims

**Location:** `src/transam/transam.zig` (test section)

**Missing tests:**
- [ ] "Edit one macro → others stay GREEN"
- [ ] Incremental re-expansion < 20ms
- [ ] Cascading invalidation prevention
- [ ] Content-addressed deduplication

**Existing tests verify:**
- [x] Revision tracking
- [x] Red-green state machine
- [x] tryMarkGreen() with cycle detection
- [x] Semantic token generation
- [x] Diagnostics caching

---

## Dependency Graph

```
┌─────────────────────────────────────────────────────────┐
│                    Parser Completion                     │
│                  (external workstream)                   │
└─────────────────────────┬───────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│              Issue #2: Parser Integration                │
│     - Add parse(file_id) → AST query                    │
│     - Wire getSyntaxTokens to use AST                   │
│     - Wire getDiagnostics to include parse errors       │
└─────────────────────────┬───────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│           Issue #1: Macro Firewall Behavior             │
│     - Parse macro arguments from AST                    │
│     - Connect to macro VM                               │
│     - Implement actual expansion                         │
└─────────────────────────┬───────────────────────────────┘
                          │
          ┌───────────────┼───────────────┐
          ▼               ▼               ▼
┌─────────────┐   ┌─────────────┐   ┌─────────────┐
│  Issue #5   │   │  Issue #3   │   │  Issue #6   │
│  Content    │   │   Async     │   │   Tests     │
│  Addressing │   │  Expansion  │   │             │
└─────────────┘   └─────────────┘   └─────────────┘
          │               │               │
          └───────────────┼───────────────┘
                          ▼
          ┌─────────────────────────────────┐
          │       Issue #4: Memory          │
          │     (can be done in parallel)   │
          └─────────────────────────────────┘
```

---

## Quick Wins (Parser Already Exists!)

The parser at `src/parser/parser.zig` already handles:
- `parseDecorator()` - parses `@derive(Eq, Hash)` with arguments
- `parseClassDeclaration()` - attaches decorators to classes
- `parseMacroDeclaration()` - parses `@macro function` definitions
- Full TypeScript subset parsing

**This unblocks much of the work!**

```zig
// Parser already produces this:
MacroInvocation {
    .name = "derive",
    .arguments = [
        MacroArgument{ .identifier_value = "Eq" },
        MacroArgument{ .identifier_value = "Hash" },
    ],
}
```

**Next step:** Add `parse(file_id)` query to Trans-Am that caches parsed AST.

---

## Implementation Plan

### Phase 6: Parser Integration (Next)

**Goal:** Wire parser to Trans-Am queries

**Tasks:**
1. Add `parse(file_id) → *ast.Node` query with caching
   ```zig
   pub fn parse(self: *Self, file_id: []const u8) !*ast.Node {
       // Check cache first (revision-based)
       // Parse via Parser.parse()
       // Store in cache, track dependencies
   }
   ```
2. Update `getMacroCallSites()` to extract from AST:
   ```zig
   // Walk AST, find MacroInvocation nodes
   // Extract arguments (already parsed!)
   // Return proper MacroCallSite with arguments
   ```
3. Update `getDiagnostics()` to include parse errors
4. Add parse error tests

**Acceptance:**
- [ ] `parse("test.ms")` returns cached AST
- [ ] `getMacroCallSites()` returns arguments (not empty)
- [ ] Parser errors show in LSP diagnostics

### Phase 6.5: CLI Integration (Next)

**Goal:** Wire CLI to Trans-Am, validate shared architecture

**Tasks:**
1. Create `src/cli/compiler.zig` using Trans-Am queries
2. Wire `./msc compile file.ms` to use `db.parse()`, `db.getDiagnostics()`
3. Add `--watch` mode leveraging incremental caching
4. Test: same file through CLI and LSP produces identical diagnostics

**Acceptance:**
- [ ] `./msc compile test.ms` uses Trans-Am (not direct parser calls)
- [ ] Broken code shows errors but doesn't crash
- [ ] Watch mode reuses cached AST on unchanged files

---

### Phase 7: Macro Firewall Implementation ✅

**Goal:** Real macro expansion with firewall pattern

**Completed:**
1. ✅ `expandMacroCallSite()` - expands @derive(Eq), @derive(Hash)
2. ✅ `generateMacroExpansion()` - generates method AST nodes
3. ✅ `hashGeneratedNodes()` - content-addressed output hashing
4. ✅ `expandAllMacros()` - high-level file expansion API
5. ✅ Content-addressed caching via MacroOutputCache

**Tests (5 new):**
- [x] "expandMacroCallSite generates method for @derive(Eq)"
- [x] "expandMacroCallSite caches result"
- [x] "firewall - editing one macro doesn't invalidate others"
- [x] "expandAllMacros returns stats"
- [x] "content-addressed caching - same output reused"

### Phase 8: Async + Polish

**Goal:** Real async expansion, memory safety

**Tasks:**
1. Spawn `std.Thread` in `startAsyncMacroExpansion()`
2. Implement progress callbacks
3. Fix memory leaks in cache

**Acceptance:**
- [ ] Benchmark: incremental re-expansion < 20ms
- [ ] Benchmark: full file expansion < 200ms
- [ ] No memory leaks (valgrind/zig test)

---

## Architecture Decision: Shared Engine (Zig-Style)

**Decision:** CLI and LSP share Trans-Am engine (not separate implementations).

**Alternatives Considered:**

| Approach | Example | Pros | Cons |
|----------|---------|------|------|
| **Separate** | rust-analyzer vs rustc | Optimized for each use case | Duplicate parser/type-checker |
| **Shared** | Zig compiler + ZLS | Single implementation | Must handle broken code gracefully |

**Why Shared:**
1. We're building both together (not retrofitting LSP to existing compiler)
2. Limited resources - can't maintain two implementations
3. Trans-Am already designed for IDE requirements (incremental, cancellable)

**Key Requirement Met:** "IDE must work on broken code"
- Parser returns `(AST, Vec<Error>)` not `Result<AST, Error>`
- `synchronize()` recovers after errors, continues parsing
- Partial AST returned with accumulated errors
- Same behavior for CLI (show errors) and LSP (show errors + features)

---

## Current Status

| Phase | Status | Notes |
|-------|--------|-------|
| Phase 1: Core Infrastructure | ✅ Complete | Revision, QueryKey, DependencyStack |
| Phase 2: Red-Green Algorithm | ✅ Complete | tryMarkGreen, cycle detection |
| Phase 3: Macro Optimization | ⚠️ Scaffolding | Types exist, no behavior |
| Phase 4: Parallel Highlighting | ✅ Complete | Fast LSP responses |
| Phase 5: LSP Integration | ✅ Complete | didChange, didOpen, semanticTokens |
| Phase 6: Parser Integration | ✅ Complete | parse() wired up, getMacroCallSites with args, getDiagnostics with parse errors |
| Phase 6.5: CLI Integration | ✅ Complete | check/compile/pipeline use Trans-Am, same diagnostics as LSP |
| Phase 7: Macro Firewall | ✅ Complete | expandMacroCallSite with content-addressed caching, 5 new tests |
| Phase 8: Async + Polish | 🔜 Next | Real async threads, memory optimization |

---

## Performance Baseline

Current measurements (Phase 5):
- Semantic tokens: **0ms** (lexer-based, fast but incomplete)
- Diagnostics: **cached** (lexer errors only)

Target measurements (Phase 8):
- Semantic tokens: **<5ms** (with AST + type info)
- Incremental macro expansion: **<20ms**
- Full file macro expansion: **<200ms**
- Type hover: **<100ms**

---

## References

- Architecture: `/docs/trans-am-architecture.md`
- Implementation: `/src/transam/transam.zig`
- LSP Server: `/src/lsp/server.zig`
- Parser: `/src/parser/parser.zig`
- Macro VM: `/src/vm/macro_vm.zig`
