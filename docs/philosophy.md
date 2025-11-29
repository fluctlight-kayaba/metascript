# Metascript Philosophy

Core principles and design philosophy guiding Metascript development.

## Developer Experience First

Every technical decision considers developer impact. Performance means nothing if developers won't use the language.

**Implementation:**
- Clear, helpful error messages pointing to user code
- Quality tooling (LSP, debugger, profiler)
- Familiar syntax requiring minimal learning
- Progressive adoption path

## Macros Make Restrictions Palatable

Instead of "we removed features," it's "we added compile-time superpowers." Macros shift the narrative from limitation to empowerment.

**Why This Matters:**
- No `any` type becomes "type-safe by design"
- No dynamic properties becomes "zero-cost abstractions via macros"
- No runtime reflection becomes "compile-time introspection"
- Static restrictions enable native performance

**Example:**
```typescript
// Dynamic pattern in TypeScript
const value = obj[key];  // Runtime lookup

// Metascript with macros
const value = obj[key];  // Macro generates type-safe compile-time switch
```

## Proven, Not Novel

We combine proven techniques (Haxe performance, Nim macros, TS syntax) rather than inventing new paradigms.

**Reference Models:**
- **Haxe**: Performance architecture (85-95% of C proven achievable)
- **Nim**: Compile-time macro system design
- **TypeScript**: Syntax and tooling patterns
- **Deno**: Modern tooling integration

**Why Not Novel:**
- Lower risk (proven approaches)
- Clear precedents for performance claims
- Faster development (learn from others)
- Easier adoption (familiar patterns)

## Incremental Adoption

Developers start with familiar TypeScript, add metaprogramming as needed. No all-or-nothing migration.

**Adoption Path:**
1. Start with strict TypeScript subset (minimal changes)
2. Add standard macros (`@derive`, `@serialize`)
3. Explore compile-time features (`@comptime`)
4. Build custom macros (advanced users)

**Migration Strategy:**
- 70% of strict TS compiles unchanged (goal)
- 90% works with < 10% modifications (goal)
- Clear migration guide from TypeScript
- Compatibility layer where beneficial

## Performance as Feature

85-95% of C performance isn't just a metric—it's the core value proposition that justifies the learning curve.

**Performance Targets:**
- Compute-bound: 90-95% of C (validated via benchmarks)
- Mixed workloads: 80-90% of C
- Allocation-heavy: 70-85% of C (GC overhead)
- Lambda cold start: < 50ms (10x faster than Node.js)

**Why Performance Matters:**
- Enables new use cases (Lambda, Edge, real-time)
- Direct cost savings (serverless pricing)
- Better user experience (faster response times)
- Differentiator vs high-level languages

## The Magic Formula

**TypeScript syntax + Compile-time macros + Native performance = Metascript**

Each component is essential:

**TypeScript Syntax:**
- Familiar to millions of developers
- Copy-paste friendly
- Excellent tooling ecosystem
- Gradual typing mindset

**Compile-Time Macros:**
- Bridge dynamic patterns to static code
- Eliminate boilerplate
- Enable DSLs and custom syntax
- Zero-cost abstractions

**Native Performance:**
- Competitive with C/Rust/Go
- Small binaries, fast startup
- Predictable resource usage
- Production-grade performance

## Design Principles

### Type System
- Static-first: All types resolvable at compile-time
- No `any`: Explicit `unknown` with type guards
- Nominal typing for classes (identity matters)
- Structural typing for interfaces (shape matters)

### Macro System
- Transparent: Bridge dynamic → static without friction
- Hygienic: No variable capture issues
- Type-driven: Access full type information
- Fail-fast: Clear errors at compile-time

### Compilation
- Optimize for runtime, not compile-time
- Clear errors pointing to source (not generated code)
- Debuggability via source maps
- LLVM optimization passes

### Memory Model
- Stack allocation for value types (preferred)
- Generational GC for heap objects
- Optional ARC mode for predictable deallocation
- Explicit annotations (`@stack`, `@heap`) where needed

## Success Philosophy

### What Success Looks Like

**Year 1:**
- Developers say "this actually works!"
- Performance claims validated
- Clear migration path from TypeScript

**Year 2:**
- Developers say "macros are a superpower"
- Ecosystem forming organically
- Production deployments growing

**Year 3:**
- Developers say "worth learning over Rust for our use case"
- Sustainable community and funding
- Corporate adoption beginning

### What Failure Looks Like

**Avoid:**
- "Yet another language" without clear differentiator
- Performance claims that don't hold up
- Developer experience sacrificed for purity
- Community fragmentation and burnout

## Non-Goals

### What We're NOT Building

**Not a JavaScript runtime:**
- No browser compatibility
- No Node.js API compatibility
- Not targeting dynamic JavaScript code

**Not TypeScript-compatible:**
- Not a drop-in `tsc` replacement
- Breaking changes for performance
- Subset only (no `any`, no eval, restricted dynamics)

**Not replacing high-level languages:**
- Not targeting web frontend development
- Not competing with Node.js/Deno/Bun for I/O-heavy apps
- Not prioritizing npm package compatibility

**Not a new syntax:**
- Not inventing new language paradigms
- Not chasing academic type system features
- TypeScript syntax is non-negotiable

**Not a quick project:**
- Not aiming for 6-month MVP
- Not sacrificing quality for speed
- 2-3 year timeline to production-ready

## Core Beliefs

1. **Syntax familiarity lowers adoption barriers** - Millions know TypeScript
2. **Compile-time metaprogramming is underutilized** - Most languages lack it
3. **Native performance is achievable** - Haxe proved it's possible
4. **Developer experience matters most** - Best tech loses if DX is poor
5. **Incremental adoption wins** - All-or-nothing migrations fail

---

**See Also:**
- [CLAUDE.md](../CLAUDE.md) - Project overview
- [Design References](./design-references.md) - Proven reference models
- [Market Strategy](./market-strategy.md) - Target markets and adoption
