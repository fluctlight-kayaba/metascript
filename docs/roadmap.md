# Metascript Roadmap

**Timeline:** 2-3 years to production-ready
**Philosophy:** Quality over speed, validation over hype
**Architecture:** AST is the IR - no separate intermediate representation
**Last Updated:** December 2024 (ORC/DRC Complete)

---

## Current Status (December 2024) - HONEST ASSESSMENT

**Overall:** 65-70% toward Year 1 completion. Frontend complete, ORC/DRC runtime complete, backends need integration.

### ✅ DONE - Frontend (100%)
| Component | Status | LOC | Notes |
|-----------|--------|-----|-------|
| **Trans-Am** | ✅ Complete | 7K | Incremental query engine, Salsa-style caching |
| **Lexer** | ✅ Complete | 500 | All TS tokens + macro tokens |
| **Parser** | ✅ Complete | 76KB | Full TypeScript subset parsing |
| **AST** | ✅ Complete | - | Nodes, location tracking, pretty printer |
| **Macro System** | ✅ Complete | 8KB | @derive(Eq,Hash,Clone,Debug) working |
| **CLI** | ✅ Complete | - | compile, dump-tokens, dump-ast, pipeline, expand |
| **Module System** | ✅ Working | - | Loader, resolver for imports |

### ✅ Type System - COMPLETE
| Component | Status | LOC | Notes |
|-----------|--------|-----|-------|
| **Type Checker** | ✅ Complete | 5K | 4-phase: collect → resolve → infer → check |
| **Type Compatibility** | ✅ Complete | 247 | Extracted to type_compat.zig |
| **Type Factory** | ✅ Complete | 266 | Extracted to type_factory.zig |
| **Symbol Table** | ✅ Complete | 465 | Scoped symbol management |
| **Type Inference** | ✅ Complete | 978 | Expression type inference |
| **Type Resolver** | ✅ Complete | 617 | Type reference resolution |

### 🚧 IN PROGRESS - Backends (60%)

| Backend | Codegen | Runtime | Completeness | Notes |
|---------|---------|---------|--------------|-------|
| **JavaScript** | 27KB | ✅ Native | 60% | Most complete, generates valid ES2020 |
| **C** | 37KB | ✅ ORC/DRC Complete | 60% | ORC runtime done (6-8% overhead), needs codegen integration |
| **Erlang** | 40KB | ✅ BEAM | 40% | Generates valid Erlang, limited features |

### ✅ DONE - Memory Management Runtime (100%)
| Component | Status | Notes |
|-----------|--------|-------|
| **ORC Runtime (C)** | ✅ Complete | `orc.h` - 720 LOC, 8-byte RefHeader, Bacon-Rajan cycle detection |
| **ORC Runtime (Zig)** | ✅ Complete | `orc.zig` - 760 LOC, full test suite |
| **Type Registry** | ✅ Complete | Caller-provides-type pattern, 24-bit type IDs |
| **Cycle Detection** | ✅ Complete | BLACK/PURPLE/GRAY/WHITE marking, recursive collection |
| **Real-World Tests** | ✅ Complete | Self-cycles, doubly-linked lists, trees, complex graphs |
| **Benchmarks** | ✅ Complete | 6-8% avg overhead, 16% worst-case (allocation-heavy) |

### ⚠️ NEEDS WORK - Developer Experience (40%)

| Component | Status | Notes |
|-----------|--------|-------|
| **LSP Server** | 🚧 Skeleton (20%) | Architecture exists, basic handlers only |
| **Macro-Aware LSP** | ❌ CRITICAL | LSP must query EXPANDED AST for completions |
| **Error Messages** | ❌ Poor | Generic messages, no source context |
| **Editor Integration** | ❌ Missing | No VSCode extension, no Neovim plugin |
| **Syntax Highlighting** | ❌ Missing | Tree-sitter grammar exists but not packaged |
| **Debugger** | ❌ Not built | No debug info generation |

**Macro-Aware LSP (Holy Grail):** Completions/hover must show macro-generated members.
Example: `user.█` after `@derive(Eq) class User {}` must autocomplete `equals()`.

**Trans-Am Caching (5-Level, Automatic):**
- L1: Bytecode (disk) - ✅ Done
- L2: Macro output - ✅ Done, needs LRU + timeout
- L3: Types - 🚧 Partial
- L4: Completions - ❌ Planned
- L5: Network - ❌ Planned

**Self-Optimizing (No Config):** Slow macros (>100ms) warned + excluded from LSP cache.
Users fix their macros, not tune settings. LRU eviction (2000 entries) prevents OOM.
See: `docs/lsp-architecture.md` "Timeout Bailout" section.

### ❌ CRITICAL GAP - Market Adoption (15%)

| Component | Status | Notes |
|-----------|--------|-------|
| **Real Examples** | ❌ Missing | No working CLI/Lambda/OTP examples |
| **Benchmarks** | ❌ Missing | No comparative performance data |
| **Community** | ❌ None | No forums, Discord, or presence |
| **Distribution** | ❌ None | No homebrew, npm, or releases |

---

## Four Pillars Status

| Pillar | Status | Priority |
|--------|--------|----------|
| **1. Developer Experience** | 40% ❌ | HIGH - Blocking adoption |
| **2. Macros & Metaprogramming** | 65% ✅ | LOW - Core works |
| **3. Multi-Backend Architecture** | 60% ⚠️ | HIGH - ORC done, codegen integration needed |
| **4. Market Adoption** | 15% ❌ | MEDIUM - After backends work |

---

## WHAT TO INVEST IN NOW (Priority Order)

### Tier 1: MUST HAVE (Blocking Year 1 Demo)

| Task | Effort | Impact | Status |
|------|--------|--------|--------|
| **Generated Code Verification** | 1-2 weeks | HIGH | ✅ DONE - 51/63 tests passing |
| **ORC/DRC Runtime** | 2-3 weeks | HIGH | ✅ DONE - 6-8% overhead, cycle detection working |
| **C Backend + ORC Codegen Integration** | 1-2 weeks | HIGH | 🚧 IN PROGRESS - Runtime ready, emit RC calls |
| **LSP Basic Functionality** | 1-2 weeks | HIGH | ❌ Pending |

**Test Infrastructure (December 2024):**
- `tests/backends/c_codegen_test.zig` - 26 tests for C backend
- `tests/backends/erlang_codegen_test.zig` - 18 tests for Erlang backend
- `tests/backends/cross_backend_test.zig` - Cross-backend parity tests
- `tests/backends/backend_test_helpers.zig` - Compiles with GCC/erlc/Node
- Run with: `zig build test-backends`

### Tier 2: IMPORTANT (For Market Entry)

| Task | Effort | Impact | Why Important |
|------|--------|--------|---------------|
| **Error Message Quality** | 1 week | MEDIUM | Poor errors = poor DX |
| **VSCode Extension** | 1-2 weeks | MEDIUM | Editor integration drives adoption |
| **Real-World Examples** | 2-3 weeks | HIGH | Prove it works for real use cases |
| **Performance Benchmarks** | 1 week | MEDIUM | Validate performance claims |

### Tier 3: NICE TO HAVE (Ecosystem)

| Task | Effort | Impact |
|------|--------|--------|
| Package Manager | 2-4 weeks | MEDIUM |
| Debugger Support | 3-4 weeks | LOW |
| Source Maps | 1-2 weeks | LOW |

---

## Architectural Decision: AST is the IR

**No separate IR layer.** Codegen backends work directly from AST.

**Why:**
- AST already contains all semantic information needed
- Typed AST (after type checking) is sufficient for all backends
- Eliminates translation layer (AST→IR→Backend becomes AST→Backend)
- Faster compilation, simpler architecture
- Macros operate on AST, codegen operates on AST (unified representation)

**Pipeline:**
```
                     Source Files (*.ms)
                            ↓
        ┌───────────────────────────────────────────────┐
        │  TRANS-AM QUERY ENGINE (incremental caching)  │
        │  - ✅ parse(file) → AST                       │
        │  - ✅ expand_macros(ast) → Expanded AST       │
        │  - ✅ type_check(ast) → Typed AST             │
        │  - Content-addressed cache + disk persistence │
        └───────────────────────────────────────────────┘
                            ↓
                  ✅ Typed AST (AST is the IR)
                            ↓
        ┌───────────────────┼───────────────────┐
        ↓                   ↓                   ↓
   🚧 C Codegen      🚧 JS Codegen      🚧 Erlang Codegen
   + ✅ ORC/DRC       (infrastructure)   (40% complete)
        ↓                   ↓                   ↓
  Native Binary    JavaScript (ES2020)   Erlang (BEAM)
```

**Implementation:** See `/src/codegen/{c,js}/` for backend implementations working directly from AST.

---

## Next Priority: Complete Codegen Integration

**Status:** Frontend complete. ORC/DRC runtime complete. **Now focus: Codegen → Runtime integration.**

**Goal:** `@derive(Eq)` class compiles to runnable code on all backends

```
✅ Source → Trans-Am → Typed AST → 🚧 Backend Codegen → ✅ Runs!
   (frontend complete)              (ORC ready!)
```

### Critical Path

**The only thing blocking end-to-end compilation:**
1. ✅ ~~Runtime/memory management~~ - ORC/DRC complete (6-8% overhead)
2. 🚧 Codegen emits ORC calls (ms_alloc, ms_incref, ms_decref, ms_decref_typed)
3. 🚧 Wire up type registry for cycle detection

### Backend 1: JavaScript (Fastest Path to Demo)

**Status:** 60% complete (27KB), most feature-complete backend
- [x] Codegen infrastructure (jsgen.zig, declarations, expressions, statements)
- [x] Basic AST node types → JS
- [x] Function declarations, expressions
- [ ] Complete class/OOP support
- [ ] Source maps
- [ ] Verify: Generated JS actually runs correctly

**Runtime:** None needed (JS runtime provides GC, stdlib)

---

### Backend 2: C (Performance Target)

**Status:** 60% complete (37KB + ORC runtime), generates valid C, runtime ready
- [x] Codegen infrastructure (cgen.zig)
- [x] Function declarations, basic expressions
- [x] Class → struct mapping
- [x] ✅ **ORC Runtime Complete** - `orc.h` (720 LOC), 6-8% overhead
- [x] ✅ **Cycle Detection** - Bacon-Rajan algorithm, real-world tests passing
- [x] ✅ **Type Registry** - Caller-provides-type pattern, 24-bit type IDs
- [ ] 🚧 **Codegen → ORC Integration** - Emit ms_alloc/ms_incref/ms_decref calls
- [ ] String handling (ms_string.h exists)
- [ ] Array bounds checking
- [ ] Verify: Generated C actually compiles AND runs with ORC

**Runtime:** ✅ ORC/DRC Complete!
- `src/runtime/orc.h` - 720 LOC (C header, production-ready)
- `src/runtime/orc.zig` - 760 LOC (Zig implementation, full tests)
- `src/runtime/ms_string.h` - String with RC
- Benchmarks: 6-8% avg overhead, 16% worst-case
- Real-world tests: self-cycles, doubly-linked lists, trees, complex graphs

---

### Backend 3: Erlang (Distributed Target)

**Status:** 40% complete (40KB), generates valid but limited Erlang
- [x] Codegen infrastructure (erlgen.zig)
- [x] Module declaration, exports
- [x] Basic function definitions
- [ ] Class → record mapping
- [ ] Pattern matching
- [ ] Gen_server integration
- [ ] Verify: Generated Erlang compiles AND runs on BEAM

**Runtime:** None needed (BEAM provides GC, OTP)

---

## Year 1: Prove Multi-Backend Concept

### Weeks 1-4: Foundation & Architecture
- [x] TypeScript parser (strict subset)
- [x] Type checker (strict mode) - symbol table, resolver, inference
- [x] **AST as IR** (architectural decision - no separate IR layer)
- [x] Core stdlib (primitives, collections)
- [x] Codegen infrastructure (C + JS backends started)
- [x] Trans-Am query engine (7K LOC, incremental computation)
- [x] ✅ **Memory management (ORC/DRC)** - Complete! 6-8% overhead, cycle detection working
- [ ] "Hello World" compiles to any backend ← **NEXT**

**Success:** AST maps cleanly to C, JavaScript, Erlang backends

### Weeks 5-12: Three Backends in Parallel (3-5 person team)

| Backend | Team | Deliverables |
|---------|------|--------------|
| **C** | 2 engineers | AST→C codegen, ORC/Lobster memory mgmt, stdlib, <500KB binary, 80%+ C perf |
| **JavaScript** | 1-2 engineers | AST→JS codegen (ES2020+), source maps, npm/ESM/CJS, <10KB bundled |
| **Erlang** | 1 engineer | AST→Erlang codegen, BEAM bytecode, OTP basics, process model |

**Success:** Same program compiles to all 3 backends, cross-backend tests pass

### Weeks 13-24: Macros + Production Validation
- [ ] `@comptime` execution (all backends)
- [ ] `@derive` macros (Eq, Hash, Clone, Debug)
- [ ] Lambda runtime optimization (C backend)
- [ ] Performance benchmarks vs Node.js/Bun/Go/Elixir
- [ ] 3-5 production pilot projects (one per backend)

**Success:** C <50ms cold start, 90%+ C perf; JS output ~hand-written; Erlang OTP working

**Milestones:**
- **M1 (Week 4):** ✅ Type checker complete, ✅ Memory management (ORC/DRC) complete
- **M2 (Week 8):** All 3 backends compile "Hello World" ← **NEXT TARGET**
- **M3 (Week 12):** Cross-backend test suite passing
- **M4 (Week 16):** Basic macros working (all backends)
- **M5 (Week 24):** First production deployment (each backend)

---

## Year 2: Build Ecosystem

### Q1-Q2: Tooling & DX
- [ ] LSP server (<200ms responsiveness)
- [ ] VS Code extension
- [ ] Source maps & debugging
- [ ] Package manager integration
- [ ] Comprehensive docs + tutorials
- [ ] Error message quality pass

**Success:** 30-min onboarding, 1k+ weekly active developers

### Q3-Q4: Ecosystem Growth
- [ ] CLI framework
- [ ] HTTP/web server libraries
- [ ] Database drivers (Postgres, Redis, SQLite)
- [ ] JSON/serialization libraries
- [ ] Testing framework
- [ ] 10+ community libraries
- [ ] Package registry

**Success:** 70% of strict TS compiles unchanged, 10k+ developers, 50+ prod deployments, 3-5 corporate pilots

**Milestones:**
- **M6 (Q1):** LSP feature-complete
- **M7 (Q2):** VS Code extension released
- **M8 (Q3):** Core ecosystem libraries ready
- **M9 (Q4):** 10k developer milestone

---

## Year 3: Mainstream Adoption

### Q1-Q2: Maturity
- [ ] Advanced macros (custom syntax, DSLs)
- [ ] Optimization passes (devirtualization, inlining)
- [ ] Self-hosting compiler (written in Metascript)
- [ ] Security audits & hardening
- [ ] Performance profiling tools
- [ ] 1.0 release

**Success:** 90%+ C perf validated, self-hosting works, <50ms cold start consistent

### Q3-Q4: Scale
- [ ] Conference talks & case studies
- [ ] Corporate support/sponsorship
- [ ] Training materials & certification
- [ ] Multi-platform support (mobile, embedded)
- [ ] Enterprise support packages

**Success:** 50k-200k developers, 500+ prod apps, sustainable funding, 5+ companies (>10 eng)

**Milestones:**
- **M10 (Q1):** Self-hosting complete
- **M11 (Q2):** 1.0 release
- **M12 (Q3):** 50k developer milestone
- **M13 (Q4):** Corporate adoption validated

---

## Success Metrics

| Metric | Year 1 | Year 2 | Year 3 |
|--------|--------|--------|--------|
| **Developers** | 1k-10k | 10k-50k | 50k-200k |
| **GitHub Stars** | 100-1k | 1k-10k | 10k-20k |
| **Discord Members** | 10-100 | 100-1k | 1k-5k |
| **Production Apps** | 3-10 | 10-50 | 50-500 |
| **Companies (>10 eng)** | 0-1 | 1-5 | 5-10 |
| **Lambda invocations/day** | 10k-100k | 100k-10M | 10M-1B |

### Technical Quality Targets

| Metric | Year 1 | Year 2 | Year 3 |
|--------|--------|--------|--------|
| **TS Compatibility** | 50% | 70% | 80% |
| **C Backend:** | | | |
| C performance | 80% | 85% | 90% |
| Cold start | <100ms | <75ms | <50ms |
| Binary size | <1MB | <800KB | <500KB |
| **JavaScript Backend:** | | | |
| npm compatibility | 30% | 60% | 80% |
| Bundle size | 50KB | 30KB | 20KB |
| **Erlang Backend:** | | | |
| OTP compatibility | Basic | Full | Production |
| Process startup | <10ms | <5ms | <2ms |
| **Shared:** | | | |
| LSP response | N/A | <200ms | <100ms |
| Compile time | <10s | <5s | <2s |

---

## Team & Resources

| Year | Engineers | DevRel | Funding |
|------|-----------|--------|---------|
| **1** | 3-5 | 0 | $300k-500k |
| **2** | 5-8 | 1 | $600k-1M |
| **3** | 8-12 | 2 | $1M-2M |

---

## Library Priorities

### Year 1: Core (Backend-Specific)

**C Backend:** Primitives/collections, file I/O, Lambda runtime, GC tuning, FFI (`@bindC`)
**JavaScript Backend:** Browser APIs, npm compat layer, bundling, source maps, testing
**Erlang Backend:** OTP GenServer/Supervisor, process comms, distributed nodes, hot reload
**Shared:** JSON serialization, testing utilities, core primitives

### Year 2: Ecosystem

**C Backend:** HTTP server, database drivers (Postgres/Redis/SQLite), CLI framework, crypto (OpenSSL FFI)
**JavaScript Backend:** React/Vue bindings, npm publishing, browser testing, bundler plugins
**Erlang Backend:** Phoenix-like web framework, distributed DB drivers, clustering, Mnesia
**Shared:** HTTP client, logging/monitoring, authentication

### Year 3: Expansion

**C Backend:** Advanced async runtime, SIMD optimizations, GPU computing (CUDA/Metal)
**JavaScript Backend:** SSR, PWAs, WebAssembly interop
**Erlang Backend:** Distributed consensus (Raft/Paxos), real-time streaming, multi-DC replication
**Shared:** GraphQL/gRPC, cloud integrations (AWS/GCP/Azure), ML libraries

---

## Risk Mitigation

| Risk | Mitigation |
|------|-----------|
| **Perf claims don't hold** | Early benchmarking, Haxe validation, conservative targets |
| **Macro system too complex** | Start simple, iterate based on feedback, excellent docs |
| **Compilation too slow** | Incremental compilation, caching, performance monitoring |
| **Competing languages improve** | Focus unique value (TS syntax + macros), niche first |
| **Ecosystem doesn't materialize** | Core team builds critical libs, corporate partnerships |
| **Slow developer adoption** | Excellent DX, clear migration path, killer demos |
| **Funding dries up** | Multiple revenue streams, sustainable pace, corporate backing |
| **Team burnout** | Realistic timeline, no death marches, healthy culture |

---

## Critical Success Factors

**Must Prove Early (Year 1):**
- Performance claims via benchmarks
- Lambda cold start <50ms
- Real-world case studies
- Macro system usability

**Cannot Defer:**
- LSP quality (critical for adoption)
- Error message clarity
- Debugging experience
- Build system reliability

---

## Non-Goals by Phase

**Year 1:** Browser compatibility, npm package compat, full TS compat, Windows native, mobile/embedded
**Year 2:** Async runtime (defer Year 3), GUI frameworks, games/graphics, academic type features
**Year 3:** Everything - focus on core use cases, quality over quantity

---

## Beyond Year 3

**Long-term Vision (Year 4-5):**
- Mainstream alternative for systems programming
- Standard toolchain for Lambda/Edge
- 500+ ecosystem packages
- Self-sustaining community
- Multiple production success stories

**Sustainability:**
- Corporate sponsorship model
- Paid support offerings
- Training/certification revenue
- Foundation donations
- Open-core model (optional)

---

**This roadmap is a living document.** Core vision constant; path evolves based on learnings and market feedback.

**See:** `positioning.md` (adoption strategy), `architecture.md` (technical implementation)
