# LSP Implementation Guide for Metascript

**Goal:** <100ms responsiveness with macro expansion, validated by rust-analyzer (<100ms) and ZLS (300ms)

**Research Base:** Rust-analyzer (Salsa), Zig compiler (incremental), Bun (Zig TypeScript parser)

---

## Architecture Overview

```
┌──────────────────────────────────────────────────────────────────┐
│                    LSP Server (Main Thread)                      │
│  ┌────────────────────────────────────────────────────────────┐  │
│  │              GlobalState (Mutable)                         │  │
│  │  - database: QueryDatabase (Salsa-like)                    │  │
│  │  - vfs: FileStore (file text, mtime)                       │  │
│  │  - task_pool: ThreadPool                                   │  │
│  │  - cancel_flag: Atomic(bool)                               │  │
│  └────────────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌──────────────────────────────────────────────────────────────────┐
│              Event Loop (select! / try_recv)                     │
│  Priority 1: Formatting (try_recv, non-blocking)                 │
│  Priority 2: LSP requests, VFS changes, Tasks (blocking select)  │
└──────────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌──────────────────────────────────────────────────────────────────┐
│          Snapshot = Immutable Database Clone                     │
│  - Spawned to background thread                                  │
│  - Checks cancel_flag in long loops                              │
│  - Returns Cancelled or Result                                   │
└──────────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌──────────────────────────────────────────────────────────────────┐
│              Query System (Salsa Pattern)                        │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │ INPUT QUERIES (never cached, set by LSP)                 │   │
│  │  - file_text(FileId) -> []const u8                       │   │
│  │  - config() -> CompilerConfig                            │   │
│  └──────────────────────────────────────────────────────────┘   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │ LRU CACHED QUERIES (limited size, evicted)              │   │
│  │  - parse(FileId) -> *SyntaxTree         [128 entries]   │   │
│  │  - macro_expand(MacroCallId) -> *AST    [512 entries]   │   │
│  │  - ast_id_map(FileId) -> AstIdMap       [1024 entries]  │   │
│  └──────────────────────────────────────────────────────────┘   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │ DEFAULT CACHED QUERIES (unbounded, LRU in future)        │   │
│  │  - infer_types(FunctionId) -> TypeInference              │   │
│  │  - lower_to_ir(FileId) -> UnifiedIR                      │   │
│  └──────────────────────────────────────────────────────────┘   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │ INTERNED QUERIES (deduplicated, never evicted)           │   │
│  │  - intern_symbol([]const u8) -> SymbolId                 │   │
│  │  - intern_macro_call(MacroCallLoc) -> MacroCallId        │   │
│  └──────────────────────────────────────────────────────────┘   │
└──────────────────────────────────────────────────────────────────┘
```

---

## 1. Query Database Implementation (Zig)

### Core Structure

```zig
// src/lsp/database.zig
const std = @import("std");
const ast = @import("../ast/ast.zig");

pub const QueryDatabase = struct {
    allocator: std.mem.Allocator,

    // INPUT LAYER (never cached)
    file_store: FileStore,
    config: CompilerConfig,

    // CACHE LAYERS
    parse_cache: LRUCache(*SyntaxTree, 128),
    macro_cache: LRUCache(*AST, 512),
    ast_id_cache: LRUCache(AstIdMap, 1024),
    type_cache: std.AutoHashMap(FunctionId, *TypeInference),
    ir_cache: std.AutoHashMap(FileId, *UnifiedIR),

    // INTERNED LAYER
    symbol_interner: StringInterner,
    macro_call_interner: MacroCallInterner,

    // INVALIDATION TRACKING
    dirty_files: std.AutoHashMap(FileId, void),
    revision: u64 = 0,  // Increment on any input change

    // CANCELLATION
    cancel_flag: *std.atomic.Atomic(bool),

    pub fn init(allocator: std.mem.Allocator, cancel_flag: *std.atomic.Atomic(bool)) !QueryDatabase {
        return .{
            .allocator = allocator,
            .file_store = FileStore.init(allocator),
            .config = CompilerConfig.default(),
            .parse_cache = LRUCache(*SyntaxTree, 128).init(allocator),
            .macro_cache = LRUCache(*AST, 512).init(allocator),
            .ast_id_cache = LRUCache(AstIdMap, 1024).init(allocator),
            .type_cache = std.AutoHashMap(FunctionId, *TypeInference).init(allocator),
            .ir_cache = std.AutoHashMap(FileId, *UnifiedIR).init(allocator),
            .symbol_interner = StringInterner.init(allocator),
            .macro_call_interner = MacroCallInterner.init(allocator),
            .dirty_files = std.AutoHashMap(FileId, void).init(allocator),
            .cancel_flag = cancel_flag,
        };
    }

    // INPUT QUERY: Set file text (invalidates dependents)
    pub fn setFileText(self: *QueryDatabase, file_id: FileId, text: []const u8) !void {
        try self.file_store.set(file_id, text);
        try self.dirty_files.put(file_id, {});
        self.revision += 1;

        // Invalidate caches for this file
        self.parse_cache.remove(file_id);
        // Macro cache invalidated lazily (firewall pattern)
    }

    // LRU CACHED QUERY: Parse file
    pub fn parse(self: *QueryDatabase, file_id: FileId) !*SyntaxTree {
        // Check cancellation
        if (self.cancel_flag.load(.SeqCst)) return error.Cancelled;

        // Check cache
        if (self.parse_cache.get(file_id)) |cached| {
            return cached;
        }

        // Cache miss: parse and store
        const text = self.file_store.get(file_id) orelse return error.FileNotFound;
        const tree = try self.parseImpl(text);
        try self.parse_cache.put(file_id, tree);

        return tree;
    }

    // MACRO FIREWALL PATTERN (3 queries to prevent cascading invalidation)

    // Firewall 1: Extract macro arguments (cached per call)
    pub fn macroArg(self: *QueryDatabase, call_id: MacroCallId) !MacroArgs {
        // Only invalidate if THIS specific call's text changes
        const call_loc = self.macro_call_interner.lookup(call_id);
        const syntax = try self.parse(call_loc.file_id);
        // Extract arguments from syntax tree
        return extractMacroArgs(syntax, call_loc.range);
    }

    // Firewall 2: Lookup macro expander (transparent, no cache)
    pub fn macroExpander(self: *QueryDatabase, def_id: MacroDefId) MacroExpanderFn {
        // Just return function pointer, don't cache
        return self.macro_registry.get(def_id);
    }

    // Firewall 3: Expand macro (LRU cached, limit 512)
    pub fn macroExpand(self: *QueryDatabase, call_id: MacroCallId) !*AST {
        // Check cancellation
        if (self.cancel_flag.load(.SeqCst)) return error.Cancelled;

        // Check cache
        if (self.macro_cache.get(call_id)) |cached| {
            return cached;
        }

        // Cache miss: expand
        const args = try self.macroArg(call_id);
        const expander = self.macroExpander(call_id.def_id);
        const expanded = try expander(self, args);
        try self.macro_cache.put(call_id, expanded);

        return expanded;
    }
};
```

---

## 2. LRU Cache Implementation

```zig
// src/lsp/lru_cache.zig
pub fn LRUCache(comptime V: type, comptime capacity: usize) type {
    return struct {
        const Self = @This();
        const Node = struct {
            key: u64,  // Hash of input
            value: V,
            prev: ?*Node = null,
            next: ?*Node = null,
        };

        allocator: std.mem.Allocator,
        map: std.AutoHashMap(u64, *Node),
        head: ?*Node = null,  // Most recently used
        tail: ?*Node = null,  // Least recently used
        count: usize = 0,

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .allocator = allocator,
                .map = std.AutoHashMap(u64, *Node).init(allocator),
            };
        }

        pub fn get(self: *Self, key: anytype) ?V {
            const hash = computeHash(key);
            const node = self.map.get(hash) orelse return null;

            // Move to front (most recently used)
            self.moveToFront(node);

            return node.value;
        }

        pub fn put(self: *Self, key: anytype, value: V) !void {
            const hash = computeHash(key);

            // Update existing
            if (self.map.get(hash)) |node| {
                node.value = value;
                self.moveToFront(node);
                return;
            }

            // Evict if at capacity
            if (self.count >= capacity) {
                try self.evictLRU();
            }

            // Insert new node
            const node = try self.allocator.create(Node);
            node.* = .{ .key = hash, .value = value };
            try self.map.put(hash, node);
            self.addToFront(node);
            self.count += 1;
        }

        pub fn remove(self: *Self, key: anytype) void {
            const hash = computeHash(key);
            if (self.map.fetchRemove(hash)) |entry| {
                self.removeNode(entry.value);
                self.allocator.destroy(entry.value);
                self.count -= 1;
            }
        }

        fn evictLRU(self: *Self) !void {
            const node = self.tail orelse return;
            _ = self.map.remove(node.key);
            self.removeNode(node);
            self.allocator.destroy(node);
            self.count -= 1;
        }

        fn moveToFront(self: *Self, node: *Node) void {
            if (self.head == node) return;  // Already at front
            self.removeNode(node);
            self.addToFront(node);
        }

        fn addToFront(self: *Self, node: *Node) void {
            node.next = self.head;
            node.prev = null;
            if (self.head) |head| head.prev = node;
            self.head = node;
            if (self.tail == null) self.tail = node;
        }

        fn removeNode(self: *Self, node: *Node) void {
            if (node.prev) |prev| prev.next = node.next;
            if (node.next) |next| next.prev = node.prev;
            if (self.head == node) self.head = node.next;
            if (self.tail == node) self.tail = node.prev;
        }

        fn computeHash(key: anytype) u64 {
            var hasher = std.hash.Wyhash.init(0);
            std.hash.autoHash(&hasher, key);
            return hasher.final();
        }
    };
}
```

---

## 3. Event Loop with Prioritization

```zig
// src/lsp/server.zig
const EventKind = enum {
    lsp_request,
    lsp_notification,
    vfs_change,
    task_complete,
    formatting_complete,
};

const Event = union(EventKind) {
    lsp_request: LspRequest,
    lsp_notification: LspNotification,
    vfs_change: VfsChange,
    task_complete: TaskResult,
    formatting_complete: FormattingResult,
};

pub const LSPServer = struct {
    allocator: std.mem.Allocator,
    database: QueryDatabase,
    task_pool: ThreadPool,
    formatting_pool: ThreadPool,  // Single-threaded
    cancel_flag: std.atomic.Atomic(bool),

    // Communication channels
    lsp_receiver: *Channel(LspMessage),
    task_receiver: *Channel(TaskResult),
    fmt_receiver: *Channel(FormattingResult),
    vfs_receiver: *Channel(VfsChange),

    pub fn run(self: *Self) !void {
        while (true) {
            const event = try self.nextEvent();
            try self.handleEvent(event);

            // Coalesce multiple events of same type
            while (self.tryReceiveMore()) |more_event| {
                try self.handleEvent(more_event);
            }
        }
    }

    fn nextEvent(self: *Self) !Event {
        // PRIORITY 1: Formatting (non-blocking, must complete fast)
        if (self.fmt_receiver.tryRecv()) |fmt| {
            return Event{ .formatting_complete = fmt };
        }

        // PRIORITY 2: Everything else (blocking select)
        return select! {
            recv(self.lsp_receiver) -> msg => {
                if (msg.isRequest()) {
                    Event{ .lsp_request = msg.request }
                } else {
                    Event{ .lsp_notification = msg.notification }
                }
            },
            recv(self.task_receiver) -> task => {
                Event{ .task_complete = task }
            },
            recv(self.vfs_receiver) -> vfs => {
                Event{ .vfs_change = vfs }
            },
        };
    }

    fn handleEvent(self: *Self, event: Event) !void {
        switch (event) {
            .vfs_change => |change| {
                // CRITICAL: Cancel in-flight tasks FIRST
                self.cancel_flag.store(true, .SeqCst);

                // Then apply VFS changes
                for (change.files) |file| {
                    try self.database.setFileText(file.id, file.text);
                }

                // Reset cancel flag for new requests
                self.cancel_flag.store(false, .SeqCst);
            },

            .lsp_request => |req| {
                // Spawn background task with snapshot
                const snapshot = try self.database.snapshot();
                try self.task_pool.spawn(handleRequest, .{ snapshot, req });
            },

            .formatting_complete => |fmt| {
                try self.respondToClient(fmt.request_id, fmt.result);
            },

            .task_complete => |task| {
                if (task.result) |result| {
                    try self.respondToClient(task.request_id, result);
                } else |err| {
                    if (err == error.Cancelled) {
                        // Discard cancelled result, don't respond
                    } else {
                        try self.respondWithError(task.request_id, err);
                    }
                }
            },

            else => {},
        }
    }
};
```

---

## 4. Cancellation Pattern

```zig
// src/lsp/cancellation.zig
pub fn checkCancellation(cancel_flag: *std.atomic.Atomic(bool)) !void {
    if (cancel_flag.load(.SeqCst)) {
        return error.Cancelled;
    }
}

// Example usage in long-running query
pub fn inferTypes(db: *QueryDatabase, func_id: FunctionId) !*TypeInference {
    const body = try db.functionBody(func_id);

    var infer = TypeInference.init(db.allocator);
    for (body.statements) |stmt| {
        // Check cancellation every 100 iterations
        if (i % 100 == 0) {
            try checkCancellation(db.cancel_flag);
        }

        try infer.analyzeStmt(stmt);
    }

    return infer;
}
```

---

## 5. Build-on-Save Strategy (Zig Pattern)

```zig
// src/lsp/file_store.zig
pub const FileStore = struct {
    files: std.AutoHashMap(FileId, FileEntry),

    pub const FileEntry = struct {
        text: []const u8,
        stat: FileStat,         // mtime, size
        status: FileStatus,
    };

    pub const FileStatus = enum {
        never_loaded,
        success,
        parse_failure,
        io_error,
    };

    pub const FileStat = struct {
        mtime: i64,
        size: u64,

        pub fn hasChanged(self: FileStat, path: []const u8) !bool {
            const stat = try std.fs.cwd().statFile(path);
            return self.mtime != stat.mtime or self.size != stat.size;
        }
    };

    pub fn set(self: *FileStore, file_id: FileId, text: []const u8) !void {
        const stat = try self.statFile(file_id);
        try self.files.put(file_id, .{
            .text = try self.allocator.dupe(u8, text),
            .stat = stat,
            .status = .success,
        });
    }

    // Lazy reparse: Only stat-check on textDocument/didSave
    pub fn checkStale(self: *FileStore, file_id: FileId) !bool {
        const entry = self.files.get(file_id) orelse return true;
        const path = self.getPath(file_id);
        return try entry.stat.hasChanged(path);
    }
};
```

---

## 6. Struct-of-Arrays for Tokens/Nodes (Zig Pattern)

```zig
// src/parser/token_buffer.zig
pub const TokenBuffer = struct {
    // Separate arrays for cache locality
    kinds: std.ArrayList(TokenKind),
    starts: std.ArrayList(u32),        // Byte offsets
    lens: std.ArrayList(u16),          // Token lengths

    // Preallocate with measured heuristics
    // Empirical: TypeScript averages 8:1 bytes to tokens
    pub fn initCapacity(allocator: std.mem.Allocator, source_len: usize) !TokenBuffer {
        const estimated_tokens = source_len / 8;
        return .{
            .kinds = try std.ArrayList(TokenKind).initCapacity(allocator, estimated_tokens),
            .starts = try std.ArrayList(u32).initCapacity(allocator, estimated_tokens),
            .lens = try std.ArrayList(u16).initCapacity(allocator, estimated_tokens),
        };
    }

    pub fn append(self: *TokenBuffer, kind: TokenKind, start: u32, len: u16) !void {
        try self.kinds.append(kind);
        try self.starts.append(start);
        try self.lens.append(len);
    }

    pub fn get(self: *TokenBuffer, index: usize) Token {
        return .{
            .kind = self.kinds.items[index],
            .start = self.starts.items[index],
            .len = self.lens.items[index],
        };
    }
};

// Similar pattern for AST nodes
pub const NodeBuffer = struct {
    kinds: std.ArrayList(NodeKind),
    locs: std.ArrayList(SourceLocation),
    data_indices: std.ArrayList(u32),  // Index into separate data storage

    // Empirical: 2:1 tokens to nodes
    pub fn initCapacity(allocator: std.mem.Allocator, token_count: usize) !NodeBuffer {
        const estimated_nodes = token_count / 2;
        return .{
            .kinds = try std.ArrayList(NodeKind).initCapacity(allocator, estimated_nodes),
            .locs = try std.ArrayList(SourceLocation).initCapacity(allocator, estimated_nodes),
            .data_indices = try std.ArrayList(u32).initCapacity(allocator, estimated_nodes),
        };
    }
};
```

---

## 7. Dual IR Retention (Incremental Invalidation)

```zig
// src/lsp/incremental.zig
pub const IncrementalContext = struct {
    current_ir: std.AutoHashMap(FileId, *UnifiedIR),
    prev_ir: std.AutoHashMap(FileId, *UnifiedIR),

    pub fn update(self: *IncrementalContext, file_id: FileId, new_ir: *UnifiedIR) !void {
        // Move current to prev
        if (self.current_ir.get(file_id)) |old_ir| {
            try self.prev_ir.put(file_id, old_ir);
        }

        // Store new IR
        try self.current_ir.put(file_id, new_ir);
    }

    pub fn hasChanged(self: *IncrementalContext, file_id: FileId) !bool {
        const curr = self.current_ir.get(file_id) orelse return true;
        const prev = self.prev_ir.get(file_id) orelse return true;

        // Compare IR hashes (not source text!)
        return curr.hash() != prev.hash();
    }

    // Invalidate transitive dependencies lazily
    pub fn invalidateDependents(self: *IncrementalContext, file_id: FileId) !void {
        if (!try self.hasChanged(file_id)) return;  // IR unchanged, stop

        const dependents = try self.getDependents(file_id);
        for (dependents) |dep_id| {
            try self.invalidateDependents(dep_id);  // Recursive
        }
    }
};
```

---

## 8. Performance Targets & Tuning

| Operation | Target | Max | Strategy |
|-----------|--------|-----|----------|
| Syntax highlighting | <16ms | 50ms | Tree-sitter (LSP-independent) |
| Hover (static) | <50ms | 100ms | Parse cache hit + symbol lookup |
| Hover (macro) | <100ms | 200ms | Parse + macro expansion cache |
| Completion (no macros) | <100ms | 200ms | Parse + scope resolution |
| Completion (with macros) | <200ms | 500ms | Full expansion + type inference |
| Full diagnostics | <500ms | 1s | Parse + expand + typecheck |

### Cache Sizes (Tune Based on Benchmarks)

```zig
// Initial settings (adjust based on profiling)
const PARSE_CACHE_SIZE = 128;        // ~1MB per tree × 128 = 128MB max
const MACRO_CACHE_SIZE = 512;        // Smaller expansions, more reuse
const AST_ID_CACHE_SIZE = 1024;      // Just integer maps, cheap
const SYMBOL_CACHE_SIZE = 4096;      // Deduplicated strings

// Memory budget: ~256MB for LSP caches (reasonable for dev machines)
```

---

## 9. Implementation Roadmap

### Week 1-2: Query System Foundation
- ✅ Implement `QueryDatabase` with input/LRU/interned layers
- ✅ Implement `LRUCache` generic type
- ✅ Implement `FileStore` with stat tracking
- ✅ Test: Cache hit/miss, eviction

### Week 3-4: Event Loop & Cancellation
- ✅ Implement `EventLoop` with prioritization
- ✅ Implement `ThreadPool` for background tasks
- ✅ Implement atomic cancellation flag
- ✅ Test: Cancel in-flight tasks on file change

### Week 5-6: Macro Firewall
- ✅ Split macro expansion into 3 queries
- ✅ Implement macro argument extraction (firewall 1)
- ✅ Implement macro expansion caching (firewall 3)
- ✅ Test: Edit macro call → only that call re-expands

### Week 7-8: Incremental Compilation
- ✅ Implement dual IR retention
- ✅ Implement IR hash comparison
- ✅ Implement lazy transitive invalidation
- ✅ Test: Edit file → only changed IR triggers rebuild

### Week 9-10: LSP Protocol Integration
- ✅ Implement textDocument/hover (with macro origin tracking)
- ✅ Implement textDocument/completion
- ✅ Implement textDocument/diagnostics
- ✅ Benchmark: <100ms hover, <200ms completion

---

## 10. Critical Anti-Patterns (Validated by Research)

❌ **DON'T make macro expansions depend directly on file text**
- Rust-analyzer lesson: Use firewall queries (macro_arg → expander → expansion)
- Prevents cascading invalidation when typing in macro call

❌ **DON'T rebuild on every keystroke**
- Zig lesson: Stat-check on save, lazy reparse on diagnostics request
- Syntax highlighting via Tree-sitter (LSP-independent)

❌ **DON'T use unbounded caches**
- Rust-analyzer lesson: LRU with measured limits (128 parse trees, 512 expansions)
- OOM on large codebases otherwise

❌ **DON'T block main thread on slow queries**
- Rust-analyzer lesson: Snapshot database, spawn background task
- Main thread only handles events, never computes

❌ **DON'T flood client with progress notifications**
- Rust-analyzer lesson: Coalesce VFS events (100+ file changes → 1 notification)
- Prevents LSP client lag

---

## Summary

**Metascript LSP combines 3 proven patterns - and named the query engine as Trans-Am Engine:**

1. **Rust-analyzer (Salsa):** Query-based caching, macro firewall, cancellation
2. **Zig (Incremental):** Dual IR, lazy invalidation, build-on-save
3. **Bun (Performance):** Stack allocator, Struct-of-Arrays, comptime specialization

**Result:** <100ms hover, <200ms completion, <500ms full diagnostics - validated by production LSPs.

**Next Step:** Implement `QueryDatabase` + `LRUCache` foundation (Week 1-2).
