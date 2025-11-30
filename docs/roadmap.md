# Metascript Roadmap

**Timeline:** 2-3 years to production-ready
**Philosophy:** Quality over speed, validation over hype

---

## Year 1: Prove Multi-Backend Concept

### Weeks 1-4: Foundation & Unified IR
- [x] TypeScript parser (strict subset)
- [x] Type checker (strict mode)
- [x] Unified IR design (backend-agnostic)
- [x] Core stdlib (primitives, collections)
- [x] "Hello World" compiles

**Success:** IR maps cleanly to C, JavaScript, Erlang

### Weeks 5-12: Three Backends in Parallel (3-5 person team)

| Backend | Team | Deliverables |
|---------|------|--------------|
| **C** | 2 engineers | IR→C, generational GC, stdlib, <500KB binary, 80%+ C perf |
| **JavaScript** | 1-2 engineers | IR→JS (ES2020+), source maps, npm/ESM/CJS, <10KB bundled |
| **Erlang** | 1 engineer | IR→Erlang, BEAM bytecode, OTP basics, process model |

**Success:** Same program compiles to all 3 backends, cross-backend tests pass

### Weeks 13-24: Macros + Production Validation
- [ ] `@comptime` execution (all backends)
- [ ] `@derive` macros (Eq, Hash, Clone, Debug)
- [ ] Lambda runtime optimization (C backend)
- [ ] Performance benchmarks vs Node.js/Bun/Go/Elixir
- [ ] 3-5 production pilot projects (one per backend)

**Success:** C <50ms cold start, 90%+ C perf; JS output ~hand-written; Erlang OTP working

**Milestones:**
- **M1 (Week 4):** Unified IR design complete
- **M2 (Week 8):** All 3 backends compile "Hello World"
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
