// Trans-am Query Engine
// Metascript's incremental computation engine (inspired by Salsa)
//
// The Trans-am engine provides:
// - Input queries (file text, config) - never cached, set by LSP
// - LRU cached queries (parse trees, macro expansions) - limited size
// - Default cached queries (type inference, IR) - unbounded (for now)
// - Interned queries (symbols, IDs) - deduplicated, never evicted
//
// Named "Trans-am" for speed and power - the muscle car of query engines.

const std = @import("std");
const ast = @import("../ast/ast.zig");

/// Trans-am Query Database
/// The core of incremental compilation for Metascript
pub const TransAmDatabase = struct {
    allocator: std.mem.Allocator,

    // INPUT LAYER - Set by LSP, never cached
    file_texts: std.StringHashMap([]const u8),  // FileId -> source text
    config: CompilerConfig,

    // CACHE LAYER - LRU caching with size limits
    parse_cache: LRUCache(ParseResult),
    macro_cache: LRUCache(MacroExpansion),
    ast_id_cache: LRUCache(AstIdMap),

    // DEFAULT CACHE - Unbounded for now (will add LRU later)
    type_cache: std.AutoHashMap(u64, TypeInference),
    ir_cache: std.AutoHashMap(u64, UnifiedIR),

    // INTERNED LAYER - Deduplicated, never evicted
    symbol_interner: StringInterner,
    macro_call_interner: MacroCallInterner,

    // INVALIDATION TRACKING
    dirty_files: std.AutoHashMap(u64, void),
    revision: u64 = 0,  // Increment on any input change

    // CANCELLATION SUPPORT
    cancel_flag: *std.atomic.Value(bool),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) !Self {
        const cancel_flag = try allocator.create(std.atomic.Value(bool));
        cancel_flag.* = std.atomic.Value(bool).init(false);

        return .{
            .allocator = allocator,
            .file_texts = std.StringHashMap([]const u8).init(allocator),
            .config = CompilerConfig.default(),
            .parse_cache = try LRUCache(ParseResult).init(allocator, 128),
            .macro_cache = try LRUCache(MacroExpansion).init(allocator, 512),
            .ast_id_cache = try LRUCache(AstIdMap).init(allocator, 1024),
            .type_cache = std.AutoHashMap(u64, TypeInference).init(allocator),
            .ir_cache = std.AutoHashMap(u64, UnifiedIR).init(allocator),
            .symbol_interner = StringInterner.init(allocator),
            .macro_call_interner = MacroCallInterner.init(allocator),
            .dirty_files = std.AutoHashMap(u64, void).init(allocator),
            .cancel_flag = cancel_flag,
        };
    }

    pub fn deinit(self: *Self) void {
        self.file_texts.deinit();
        self.parse_cache.deinit();
        self.macro_cache.deinit();
        self.ast_id_cache.deinit();
        self.type_cache.deinit();
        self.ir_cache.deinit();
        self.symbol_interner.deinit();
        self.macro_call_interner.deinit();
        self.dirty_files.deinit();
        self.allocator.destroy(self.cancel_flag);
    }

    // ===== INPUT QUERIES (never cached, set by LSP) =====

    /// Set file text - invalidates all dependent queries
    pub fn setFileText(self: *Self, file_id: []const u8, text: []const u8) !void {
        const owned_text = try self.allocator.dupe(u8, text);
        try self.file_texts.put(file_id, owned_text);

        // Mark file as dirty
        const file_hash = hashString(file_id);
        try self.dirty_files.put(file_hash, {});

        // Increment revision (triggers invalidation)
        self.revision += 1;

        // Invalidate parse cache for this file
        self.parse_cache.remove(file_hash);

        // Macro cache invalidated lazily (firewall pattern)
    }

    /// Get file text (input query)
    pub fn getFileText(self: *Self, file_id: []const u8) ?[]const u8 {
        return self.file_texts.get(file_id);
    }

    // ===== LRU CACHED QUERIES =====

    /// Parse file - LRU cached (limit 128 entries)
    pub fn parse(self: *Self, file_id: []const u8) !ParseResult {
        // Check cancellation
        try self.checkCancellation();

        const file_hash = hashString(file_id);

        // Check cache
        if (self.parse_cache.get(file_hash)) |cached| {
            return cached;
        }

        // Cache miss: parse and store
        const text = self.getFileText(file_id) orelse return error.FileNotFound;
        const result = try self.parseImpl(file_id, text);
        try self.parse_cache.put(file_hash, result);

        return result;
    }

    // ===== MACRO FIREWALL PATTERN =====
    // Three separate queries to prevent cascading invalidation

    /// Firewall 1: Extract macro arguments
    /// Only invalidates when THIS specific call's text changes
    pub fn macroArg(self: *Self, call_id: MacroCallId) !MacroArgs {
        try self.checkCancellation();

        const call_loc = self.macro_call_interner.lookup(call_id);
        const parse_result = try self.parse(call_loc.file_id);

        // Extract arguments from syntax tree
        return self.extractMacroArgs(parse_result.tree, call_loc.range);
    }

    /// Firewall 2: Lookup macro expander (transparent, no cache)
    pub fn macroExpander(self: *Self, def_id: MacroDefId) !MacroExpanderFn {
        // Just return function pointer, don't cache
        return self.builtin_macros.get(def_id) orelse error.UnknownMacro;
    }

    /// Firewall 3: Expand macro - LRU cached (limit 512 entries)
    pub fn macroExpand(self: *Self, call_id: MacroCallId) !MacroExpansion {
        try self.checkCancellation();

        const call_hash = hashMacroCallId(call_id);

        // Check cache
        if (self.macro_cache.get(call_hash)) |cached| {
            return cached;
        }

        // Cache miss: expand
        const args = try self.macroArg(call_id);
        const call_loc = self.macro_call_interner.lookup(call_id);
        const expander = try self.macroExpander(call_loc.def_id);
        const expanded = try expander(self, args);

        const result = MacroExpansion{
            .ast = expanded,
            .origin = call_loc,
        };

        try self.macro_cache.put(call_hash, result);

        return result;
    }

    // ===== INTERNED QUERIES =====

    /// Intern a symbol (deduplicated, never evicted)
    pub fn internSymbol(self: *Self, name: []const u8) !SymbolId {
        return try self.symbol_interner.intern(name);
    }

    /// Intern a macro call location
    pub fn internMacroCall(self: *Self, loc: MacroCallLoc) !MacroCallId {
        return try self.macro_call_interner.intern(loc);
    }

    // ===== CANCELLATION SUPPORT =====

    /// Check if query execution should be cancelled
    pub fn checkCancellation(self: *Self) !void {
        if (self.cancel_flag.load(.seq_cst)) {
            return error.Cancelled;
        }
    }

    /// Request cancellation of all in-flight queries
    pub fn requestCancellation(self: *Self) void {
        self.cancel_flag.store(true, .seq_cst);
    }

    /// Reset cancellation flag (after handling cancellation)
    pub fn resetCancellation(self: *Self) void {
        self.cancel_flag.store(false, .seq_cst);
    }

    // ===== IMPLEMENTATION DETAILS =====

    fn parseImpl(self: *Self, file_id: []const u8, text: []const u8) !ParseResult {
        // TODO: Implement actual parser
        // For now, return placeholder
        _ = self;
        _ = file_id;
        _ = text;
        return ParseResult{
            .tree = undefined,
            .errors = &[_]ParseError{},
        };
    }

    fn extractMacroArgs(self: *Self, tree: *ast.Node, range: SourceRange) !MacroArgs {
        // TODO: Implement macro argument extraction
        _ = self;
        _ = tree;
        _ = range;
        return MacroArgs{};
    }
};

// ===== TYPE DEFINITIONS =====

pub const ParseResult = struct {
    tree: *ast.Node,
    errors: []ParseError,
};

pub const ParseError = struct {
    message: []const u8,
    location: ast.SourceLocation,
};

pub const MacroExpansion = struct {
    ast: *ast.Node,
    origin: MacroCallLoc,
};

pub const MacroArgs = struct {
    // TODO: Define macro argument structure
};

pub const MacroCallId = u64;
pub const MacroDefId = u64;
pub const SymbolId = u64;

pub const MacroCallLoc = struct {
    file_id: []const u8,
    def_id: MacroDefId,
    range: SourceRange,
};

pub const SourceRange = struct {
    start: u32,
    end: u32,
};

pub const MacroExpanderFn = *const fn (db: *TransAmDatabase, args: MacroArgs) anyerror!*ast.Node;

pub const AstIdMap = struct {
    // TODO: Implement AST ID mapping
};

pub const TypeInference = struct {
    // TODO: Implement type inference results
};

pub const UnifiedIR = struct {
    // TODO: Implement unified IR
};

pub const CompilerConfig = struct {
    pub fn default() CompilerConfig {
        return .{};
    }
};

// ===== LRU CACHE =====

pub fn LRUCache(comptime V: type) type {
    return struct {
        const Self = @This();
        const Node = struct {
            key: u64,
            value: V,
            prev: ?*Node = null,
            next: ?*Node = null,
        };

        allocator: std.mem.Allocator,
        capacity: usize,
        map: std.AutoHashMap(u64, *Node),
        head: ?*Node = null,  // Most recently used
        tail: ?*Node = null,  // Least recently used
        count: usize = 0,

        pub fn init(allocator: std.mem.Allocator, capacity: usize) !Self {
            return .{
                .allocator = allocator,
                .capacity = capacity,
                .map = std.AutoHashMap(u64, *Node).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            var it = self.map.valueIterator();
            while (it.next()) |node_ptr| {
                self.allocator.destroy(node_ptr.*);
            }
            self.map.deinit();
        }

        pub fn get(self: *Self, key: u64) ?V {
            const node = self.map.get(key) orelse return null;

            // Move to front (most recently used)
            self.moveToFront(node);

            return node.value;
        }

        pub fn put(self: *Self, key: u64, value: V) !void {
            // Update existing
            if (self.map.get(key)) |node| {
                node.value = value;
                self.moveToFront(node);
                return;
            }

            // Evict if at capacity
            if (self.count >= self.capacity) {
                try self.evictLRU();
            }

            // Insert new node
            const node = try self.allocator.create(Node);
            node.* = .{ .key = key, .value = value };
            try self.map.put(key, node);
            self.addToFront(node);
            self.count += 1;
        }

        pub fn remove(self: *Self, key: u64) void {
            if (self.map.fetchRemove(key)) |entry| {
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
    };
}

// ===== STRING INTERNER =====

pub const StringInterner = struct {
    allocator: std.mem.Allocator,
    map: std.StringHashMap(SymbolId),
    strings: std.ArrayList([]const u8),
    next_id: SymbolId = 0,

    pub fn init(allocator: std.mem.Allocator) StringInterner {
        return .{
            .allocator = allocator,
            .map = std.StringHashMap(SymbolId).init(allocator),
            .strings = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *StringInterner) void {
        for (self.strings.items) |str| {
            self.allocator.free(str);
        }
        self.strings.deinit();
        self.map.deinit();
    }

    pub fn intern(self: *StringInterner, name: []const u8) !SymbolId {
        if (self.map.get(name)) |id| {
            return id;
        }

        const owned_name = try self.allocator.dupe(u8, name);
        const id = self.next_id;
        self.next_id += 1;

        try self.map.put(owned_name, id);
        try self.strings.append(owned_name);

        return id;
    }

    pub fn lookup(self: *StringInterner, id: SymbolId) ?[]const u8 {
        if (id < self.strings.items.len) {
            return self.strings.items[@intCast(id)];
        }
        return null;
    }
};

// ===== MACRO CALL INTERNER =====

pub const MacroCallInterner = struct {
    allocator: std.mem.Allocator,
    map: std.AutoHashMap(u64, MacroCallId),
    locations: std.ArrayList(MacroCallLoc),
    next_id: MacroCallId = 0,

    pub fn init(allocator: std.mem.Allocator) MacroCallInterner {
        return .{
            .allocator = allocator,
            .map = std.AutoHashMap(u64, MacroCallId).init(allocator),
            .locations = std.ArrayList(MacroCallLoc).init(allocator),
        };
    }

    pub fn deinit(self: *MacroCallInterner) void {
        for (self.locations.items) |loc| {
            self.allocator.free(loc.file_id);
        }
        self.locations.deinit();
        self.map.deinit();
    }

    pub fn intern(self: *MacroCallInterner, loc: MacroCallLoc) !MacroCallId {
        const hash = hashMacroCallLoc(&loc);

        if (self.map.get(hash)) |id| {
            return id;
        }

        const owned_file_id = try self.allocator.dupe(u8, loc.file_id);
        const owned_loc = MacroCallLoc{
            .file_id = owned_file_id,
            .def_id = loc.def_id,
            .range = loc.range,
        };

        const id = self.next_id;
        self.next_id += 1;

        try self.map.put(hash, id);
        try self.locations.append(owned_loc);

        return id;
    }

    pub fn lookup(self: *MacroCallInterner, id: MacroCallId) MacroCallLoc {
        return self.locations.items[@intCast(id)];
    }
};

// ===== HASH FUNCTIONS =====

fn hashString(s: []const u8) u64 {
    var hasher = std.hash.Wyhash.init(0);
    hasher.update(s);
    return hasher.final();
}

fn hashMacroCallId(id: MacroCallId) u64 {
    var hasher = std.hash.Wyhash.init(0);
    std.hash.autoHash(&hasher, id);
    return hasher.final();
}

fn hashMacroCallLoc(loc: *const MacroCallLoc) u64 {
    var hasher = std.hash.Wyhash.init(0);
    hasher.update(loc.file_id);
    std.hash.autoHash(&hasher, loc.def_id);
    std.hash.autoHash(&hasher, loc.range.start);
    std.hash.autoHash(&hasher, loc.range.end);
    return hasher.final();
}

// ===== TESTS =====

test "Trans-am: basic init" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    try std.testing.expect(db.revision == 0);
}

test "Trans-am: set and get file text" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    try db.setFileText("test.mts", "const x = 1;");

    const text = db.getFileText("test.mts").?;
    try std.testing.expectEqualStrings("const x = 1;", text);
    try std.testing.expect(db.revision == 1);
}

test "Trans-am: LRU cache eviction" {
    var cache = try LRUCache(u32).init(std.testing.allocator, 2);
    defer cache.deinit();

    try cache.put(1, 100);
    try cache.put(2, 200);

    try std.testing.expect(cache.get(1).? == 100);
    try std.testing.expect(cache.get(2).? == 200);

    // Evict oldest (1)
    try cache.put(3, 300);

    try std.testing.expect(cache.get(1) == null);
    try std.testing.expect(cache.get(2).? == 200);
    try std.testing.expect(cache.get(3).? == 300);
}

test "Trans-am: string interner" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    const id1 = try interner.intern("foo");
    const id2 = try interner.intern("bar");
    const id3 = try interner.intern("foo");  // Should return same ID

    try std.testing.expect(id1 == id3);
    try std.testing.expect(id1 != id2);

    try std.testing.expectEqualStrings("foo", interner.lookup(id1).?);
    try std.testing.expectEqualStrings("bar", interner.lookup(id2).?);
}

test "Trans-am: cancellation" {
    var db = try TransAmDatabase.init(std.testing.allocator);
    defer db.deinit();

    // Initially not cancelled
    try db.checkCancellation();

    // Request cancellation
    db.requestCancellation();

    // Should now fail
    const result = db.checkCancellation();
    try std.testing.expectError(error.Cancelled, result);

    // Reset
    db.resetCancellation();
    try db.checkCancellation();
}
