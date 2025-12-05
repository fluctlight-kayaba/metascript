# Metascript Standard Library

Design and roadmap for `std/*` - Metascript's standard library providing cross-platform APIs.

---

## Philosophy

1. **Zero-cost abstractions** - Direct Zig stdlib mapping, no runtime overhead
2. **Node.js API** - Familiar to millions of developers, battle-tested for 15+ years
3. **One way to do things** - No redundant abstractions, no confusion
4. **Cross-platform** - Same API on macOS, Linux, Windows

---

## Architecture

```
std/
├── fs.ms           # File system operations
├── path.ms         # Path manipulation
├── process.ms      # Process info and control
├── child_process.ms # Spawn subprocesses
├── events.ms       # EventEmitter pattern
├── util.ms         # Utility functions
├── os.ms           # OS information
├── url.ms          # URL parsing
├── buffer.ms       # Binary data handling
├── stream.ms       # Readable/Writable streams
├── crypto.ms       # Cryptography
└── assert.ms       # Assertions
```

Simple. Familiar. Just `std/*`.

---

## Implementation Strategy

### Zig Backend Mapping

| std/* API | Zig stdlib | Notes |
|-----------|------------|-------|
| `fs.readFileSync` | `std.fs.readFileAlloc` | Direct map |
| `fs.writeFileSync` | `std.fs.writeFile` | Direct map |
| `fs.existsSync` | `std.fs.access` | Check access |
| `fs.statSync` | `std.fs.stat` | File metadata |
| `fs.readdirSync` | `std.fs.Dir.iterate` | Directory listing |
| `path.join` | String concatenation | Pure, no syscall |
| `path.resolve` | `std.fs.realpath` | Resolve symlinks |
| `path.dirname` | String slice | Pure |
| `path.basename` | String slice | Pure |
| `process.cwd()` | `std.process.getCwd` | Direct map |
| `process.argv` | Passed at startup | Compile-time |
| `process.env` | `std.process.getEnvMap` | Direct map |
| `child_process.spawn` | `std.process.Child` | Direct map |
| `EventEmitter` | ArrayList + fn ptrs | Simple impl |

### No libuv Dependency

We deliberately avoid libuv:
- **Overhead**: Event loop not needed for sync operations
- **Complexity**: Additional C dependency
- **Performance**: Direct syscalls are faster

Zig's stdlib provides cross-platform abstractions at zero cost.

---

## Module Specifications

### std/fs.ms

```typescript
// Sync file operations
function readFileSync(path: string): string;
function readFileSync(path: string, encoding: null): Buffer;
function writeFileSync(path: string, data: string): void;
function appendFileSync(path: string, data: string): void;
function existsSync(path: string): boolean;
function statSync(path: string): Stats;
function mkdirSync(path: string, options?: { recursive?: boolean }): void;
function rmdirSync(path: string): void;
function unlinkSync(path: string): void;
function readdirSync(path: string): string[];
function renameSync(oldPath: string, newPath: string): void;
function copyFileSync(src: string, dest: string): void;

interface Stats {
    isFile(): boolean;
    isDirectory(): boolean;
    isSymbolicLink(): boolean;
    size: number;
    mtime: Date;
    atime: Date;
    ctime: Date;
}
```

**Implementation**: ~500 lines, maps to `std.fs`

### std/path.ms

```typescript
// Pure string operations - no syscalls
function join(...paths: string[]): string;
function resolve(...paths: string[]): string;
function dirname(path: string): string;
function basename(path: string, ext?: string): string;
function extname(path: string): string;
function normalize(path: string): string;
function isAbsolute(path: string): boolean;
function relative(from: string, to: string): string;
function parse(path: string): ParsedPath;
function format(pathObject: ParsedPath): string;

const sep: string;      // '/' on POSIX, '\\' on Windows
const delimiter: string; // ':' on POSIX, ';' on Windows

interface ParsedPath {
    root: string;
    dir: string;
    base: string;
    ext: string;
    name: string;
}
```

**Implementation**: ~200 lines, pure string manipulation

### std/process.ms

```typescript
const argv: string[];
const env: Record<string, string>;
function cwd(): string;
function chdir(directory: string): void;
function exit(code?: number): never;

const platform: 'darwin' | 'linux' | 'win32';
const arch: 'x64' | 'arm64';

function memoryUsage(): MemoryUsage;
function hrtime(): [number, number];
function uptime(): number;

interface MemoryUsage {
    heapTotal: number;
    heapUsed: number;
    external: number;
    rss: number;
}
```

**Implementation**: ~100 lines, maps to `std.process`

### std/child_process.ms

```typescript
function spawn(
    command: string,
    args?: string[],
    options?: SpawnOptions
): ChildProcess;

function execSync(command: string, options?: ExecOptions): string;
function spawnSync(command: string, args?: string[]): SpawnResult;

interface SpawnOptions {
    cwd?: string;
    env?: Record<string, string>;
    stdio?: 'pipe' | 'inherit' | 'ignore';
}

interface ChildProcess {
    pid: number;
    stdin: Writable;
    stdout: Readable;
    stderr: Readable;
    kill(signal?: string): boolean;
    wait(): number;
}

interface SpawnResult {
    status: number;
    stdout: string;
    stderr: string;
}
```

**Implementation**: ~300 lines, maps to `std.process.Child`

### std/events.ms

```typescript
class EventEmitter {
    on(event: string, listener: Function): this;
    once(event: string, listener: Function): this;
    off(event: string, listener: Function): this;
    emit(event: string, ...args: any[]): boolean;
    removeAllListeners(event?: string): this;
    listenerCount(event: string): number;
}
```

**Implementation**: ~150 lines, ArrayList of function pointers

### std/util.ms

```typescript
function format(format: string, ...args: any[]): string;
function inspect(object: any, options?: InspectOptions): string;
function promisify<T>(fn: Function): (...args: any[]) => Promise<T>;
function isDeepStrictEqual(val1: any, val2: any): boolean;

interface InspectOptions {
    depth?: number;
    colors?: boolean;
    maxArrayLength?: number;
}
```

**Implementation**: ~200 lines

---

## Roadmap

### Phase 1: typescript-language-server Support (Week 1-2)

| Module | Status | Functions | Effort |
|--------|--------|-----------|--------|
| std/fs.ms | TODO | readFileSync, writeFileSync, existsSync, statSync, readdirSync, mkdirSync | 2 days |
| std/path.ms | TODO | join, resolve, dirname, basename, extname, isAbsolute | 1 day |
| std/process.ms | TODO | cwd, argv, env, exit, platform | 1 day |
| std/child_process.ms | TODO | spawn, spawnSync | 1-2 days |
| std/events.ms | TODO | EventEmitter class | 1 day |
| std/util.ms | TODO | format, promisify (stub) | 0.5 day |

**Phase 1 Total: ~800-1000 lines, 6-8 days**

### Phase 2: Complete Standard Library (Week 3-4)

| Module | Status | Priority |
|--------|--------|----------|
| std/os.ms | TODO | Medium |
| std/url.ms | TODO | Medium |
| std/buffer.ms | TODO | Medium |
| std/stream.ms | TODO | Low |
| std/crypto.ms | TODO | Low |
| std/assert.ms | TODO | Low |

**Phase 2 Total: ~1500 lines, 2 weeks**

---

## Implementation Checklist

### std/fs.ms
- [ ] readFileSync(path: string): string
- [ ] readFileSync(path: string, encoding: null): Buffer
- [ ] writeFileSync(path: string, data: string): void
- [ ] appendFileSync(path: string, data: string): void
- [ ] existsSync(path: string): boolean
- [ ] statSync(path: string): Stats
- [ ] mkdirSync(path: string, options?): void
- [ ] rmdirSync(path: string): void
- [ ] unlinkSync(path: string): void
- [ ] readdirSync(path: string): string[]
- [ ] renameSync(oldPath, newPath): void
- [ ] copyFileSync(src, dest): void

### std/path.ms
- [ ] join(...paths): string
- [ ] resolve(...paths): string
- [ ] dirname(path): string
- [ ] basename(path, ext?): string
- [ ] extname(path): string
- [ ] normalize(path): string
- [ ] isAbsolute(path): boolean
- [ ] relative(from, to): string
- [ ] parse(path): ParsedPath
- [ ] format(obj): string
- [ ] sep constant
- [ ] delimiter constant

### std/process.ms
- [ ] argv: string[]
- [ ] env: Record<string, string>
- [ ] cwd(): string
- [ ] chdir(dir): void
- [ ] exit(code?): never
- [ ] platform: string
- [ ] arch: string
- [ ] memoryUsage(): MemoryUsage
- [ ] hrtime(): [number, number]

### std/child_process.ms
- [ ] spawn(cmd, args?, opts?): ChildProcess
- [ ] spawnSync(cmd, args?): SpawnResult
- [ ] execSync(cmd, opts?): string
- [ ] ChildProcess.kill()
- [ ] ChildProcess.wait()
- [ ] ChildProcess.stdin/stdout/stderr

### std/events.ms
- [ ] EventEmitter class
- [ ] on(event, listener)
- [ ] once(event, listener)
- [ ] off(event, listener)
- [ ] emit(event, ...args)
- [ ] removeAllListeners(event?)
- [ ] listenerCount(event)

---

## Testing Strategy

Each module should have:
1. **Unit tests** - Test each function in isolation
2. **Compatibility tests** - Compare output with Node.js
3. **Edge case tests** - Empty paths, missing files, permissions

```typescript
// Example test
test "fs.readFileSync reads file content" {
    writeFileSync("/tmp/test.txt", "hello");
    const content = readFileSync("/tmp/test.txt");
    assert(content === "hello");
}

test "fs.existsSync returns false for missing file" {
    assert(!existsSync("/nonexistent/path"));
}
```

---

## Performance Targets

| Operation | Node.js | Metascript Target |
|-----------|---------|-------------------|
| readFileSync (1KB) | ~0.1ms | ~0.05ms |
| readFileSync (1MB) | ~5ms | ~3ms |
| path.join (5 parts) | ~0.001ms | ~0.0005ms |
| spawn + wait | ~10ms | ~5ms |
| EventEmitter.emit | ~0.01ms | ~0.005ms |

Target: **2x faster than Node.js** (no event loop overhead).

---

## Usage

```typescript
import * as fs from "std/fs";
import * as path from "std/path";
import { spawn } from "std/child_process";

const configPath = path.join(process.cwd(), "config.json");
const config = JSON.parse(fs.readFileSync(configPath));

const child = spawn("ls", ["-la"]);
const output = child.stdout.read();
```

Same API you already know. No learning curve.

---

## References

- [Node.js Documentation](https://nodejs.org/docs/latest/api/)
- [Zig Standard Library](https://ziglang.org/documentation/master/std/)
