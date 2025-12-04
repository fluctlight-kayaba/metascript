# Metascript Philosophy

Core principles guiding development.

---

## Developer Experience First

Every technical decision considers developer impact. Performance means nothing if developers won't use the language.

**Implementation:** Clear error messages, quality tooling (LSP, debugger), familiar syntax, progressive adoption.

---

## One Language, Three Runtimes

| Backend | Best For | Key Metrics |
|---------|----------|-------------|
| **C** | Lambda/Edge, CLI, perf-critical | <50ms cold start, 90%+ C perf |
| **JavaScript** | Browser, npm ecosystem, TS migration | Universal reach, millions of developers |
| **Erlang** | Distributed, fault-tolerant | OTP supervision, hot reload, production reliability |

**Why Three Backends:**
- Typed AST validates design (if it maps to all three, abstraction is sound)
- Different strengths (performance, reach, reliability)
- One codebase, multiple targets (write once, deploy anywhere)
- De-risks the project (not betting on single runtime)

**Universal Promise:** Same TypeScript syntax. Same macros. Three strategic runtimes.

---

## Macros Make Restrictions Palatable

Instead of "we removed features," it's "we added compile-time superpowers."

**Reframing:**
- No `any` → "Type-safe by design"
- No dynamic properties → "Zero-cost abstractions via macros"
- No runtime reflection → "Compile-time introspection"
- Static restrictions → Enable native performance

**Example:**
```typescript
// Dynamic (TypeScript): obj[key]  // Runtime lookup
// Metascript: obj[key]  // Macro generates type-safe compile-time switch
```

---

## Proven, Not Novel

Combine proven techniques from production languages. Lower risk, faster development, easier adoption.

**Reference Models:**

| What | Proven By |
|------|-----------|
| **Multi-backend IR** | Haxe (20 years production, C++/JS/JVM) |
| **C backend + macros** | Nim (90%+ C perf), Haxe (85-95% C++ perf) |
| **TS→JS in Zig** | Bun (millions of users, 10-20x faster than tsc) |
| **Erlang backend** | Elixir (Discord, WhatsApp), Gleam (type-safe BEAM) |

**Local Reference:** All in `~/projects/{haxe,nim,bun,elixir,gleam,typescript-go}`

---

## Incremental Adoption

Start with familiar TypeScript, add metaprogramming as needed. No all-or-nothing migration.

**Adoption Path:**
1. Start with strict TypeScript subset (minimal changes)
2. Add standard macros (`@derive`, `@serialize`)
3. Explore compile-time features (`@comptime`)
4. Build custom macros (advanced users)

**Migration:** 70% of strict TS works unchanged, 90% works with <10% modifications.

---

## Performance as Feature

85-95% of C performance isn't just a metric—it's the core value proposition.

**Targets:**
- Compute-bound: 90-95% of C
- Mixed workloads: 80-90% of C
- Allocation-heavy: 70-85% of C (GC overhead)
- Lambda cold start: <50ms (10x faster than Node.js)

**Why:** Enables new use cases (Lambda, Edge), direct cost savings (serverless), better UX (faster response).

---

## The Magic Formula

**TypeScript syntax + Compile-time macros + Three strategic backends = Metascript**

Each component is essential:

**TypeScript Syntax:** Familiar to millions, copy-paste friendly, excellent tooling
**Compile-Time Macros:** Bridge dynamic→static, eliminate boilerplate, zero-cost abstractions
**Three Backends:** C (performance), JavaScript (reach), Erlang (reliability)

**Why This Works:** Typed AST ensures same semantics, developer chooses backend for deployment, macros expand consistently.

---

## Design Principles

**Type System:** Static-first (all types resolvable at compile-time), no `any` (explicit `unknown`), nominal typing for classes, structural for interfaces

**Macro System:** Transparent (bridge dynamic→static), hygienic (no variable capture), type-driven (full type info access), fail-fast (clear compile-time errors)

**Compilation:** Optimize runtime (not compile-time), clear errors (point to source), debuggability (source maps), LLVM optimization

**Memory Model:**
- **C backend:** Stack allocation (preferred), generational GC, optional ARC
- **JavaScript backend:** V8/engine-managed
- **Erlang backend:** Per-process heaps, immutable data

**Backend Selection:** Auto-inference from imports (e.g., `@metascript/web` → JS), explicit via `--target=c|js|erlang`, same semantics across all

---

## Success Philosophy

**Year 1:** "This actually works!" - Performance claims validated, clear TS migration path
**Year 2:** "Macros are a superpower" - Ecosystem forming, production deployments growing
**Year 3:** "Worth learning over Rust for our use case" - Sustainable community, corporate adoption

**Avoid:** "Yet another language" without differentiator, performance claims that fail, DX sacrificed for purity, community burnout

---

## Non-Goals

**NOT:**
- TypeScript drop-in replacement (strict subset only, breaking changes for perf)
- Single-purpose language (universal with backend choice)
- New syntax invention (TypeScript syntax non-negotiable)
- Dynamic code prioritization (no `eval`, runtime type manipulation)
- Quick project (2-3 year timeline, quality over speed)

---

## Core Beliefs

1. **Syntax familiarity lowers adoption barriers** (millions know TypeScript)
2. **Compile-time metaprogramming is underutilized** (most languages lack it)
3. **Multi-backend compilation validates design** (if IR maps to C/JS/Erlang, abstraction is sound)
4. **Native performance is achievable** (Haxe proved 85-95% of C possible)
5. **Developer experience matters most** (best tech loses if DX is poor)
6. **Incremental adoption wins** (all-or-nothing migrations fail)
7. **One codebase, multiple runtimes** (write once, deploy to native/browser/BEAM)

---

**See:** `design-references.md` (proven reference models), `positioning.md` (target markets)
