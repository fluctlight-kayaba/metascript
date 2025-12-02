// Trans-Am Interners
// String and macro call interning for deduplication

const std = @import("std");
const types = @import("types.zig");
const hash = @import("hash.zig");

// ===== STRING INTERNER =====

/// StringInterner provides deduplication for symbol names.
/// Interned strings are never evicted (unlike LRU caches).
pub const StringInterner = struct {
    allocator: std.mem.Allocator,
    map: std.StringHashMap(types.SymbolId),
    strings: std.ArrayList([]const u8),
    next_id: types.SymbolId = 0,

    pub fn init(allocator: std.mem.Allocator) StringInterner {
        return .{
            .allocator = allocator,
            .map = std.StringHashMap(types.SymbolId).init(allocator),
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

    /// Intern a string, returning its unique ID.
    /// If the string was already interned, returns the existing ID.
    pub fn intern(self: *StringInterner, name: []const u8) !types.SymbolId {
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

    /// Look up the string for a given ID.
    pub fn lookup(self: *StringInterner, id: types.SymbolId) ?[]const u8 {
        if (id < self.strings.items.len) {
            return self.strings.items[@intCast(id)];
        }
        return null;
    }
};

// ===== MACRO CALL INTERNER =====

/// MacroCallInterner provides deduplication for macro call locations.
/// Each unique macro call site gets a unique ID for efficient tracking.
pub const MacroCallInterner = struct {
    allocator: std.mem.Allocator,
    map: std.AutoHashMap(u64, types.MacroCallId),
    locations: std.ArrayList(types.MacroCallLoc),
    next_id: types.MacroCallId = 0,

    pub fn init(allocator: std.mem.Allocator) MacroCallInterner {
        return .{
            .allocator = allocator,
            .map = std.AutoHashMap(u64, types.MacroCallId).init(allocator),
            .locations = std.ArrayList(types.MacroCallLoc).init(allocator),
        };
    }

    pub fn deinit(self: *MacroCallInterner) void {
        for (self.locations.items) |loc| {
            self.allocator.free(loc.file_id);
        }
        self.locations.deinit();
        self.map.deinit();
    }

    /// Intern a macro call location, returning its unique ID.
    /// If this location was already interned, returns the existing ID.
    pub fn intern(self: *MacroCallInterner, loc: types.MacroCallLoc) !types.MacroCallId {
        const loc_hash = hash.hashMacroCallLoc(&loc);

        if (self.map.get(loc_hash)) |id| {
            return id;
        }

        const owned_file_id = try self.allocator.dupe(u8, loc.file_id);
        const owned_loc = types.MacroCallLoc{
            .file_id = owned_file_id,
            .def_id = loc.def_id,
            .range = loc.range,
        };

        const id = self.next_id;
        self.next_id += 1;

        try self.map.put(loc_hash, id);
        try self.locations.append(owned_loc);

        return id;
    }

    /// Look up the location for a given ID.
    pub fn lookup(self: *MacroCallInterner, id: types.MacroCallId) types.MacroCallLoc {
        return self.locations.items[@intCast(id)];
    }
};

// ===== TESTS =====

test "StringInterner: basic interning" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    const id1 = try interner.intern("hello");
    const id2 = try interner.intern("world");
    const id3 = try interner.intern("hello"); // Same as id1

    try std.testing.expectEqual(id1, id3);
    try std.testing.expect(id1 != id2);
}

test "StringInterner: lookup" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    const id = try interner.intern("test_string");
    const result = interner.lookup(id);

    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("test_string", result.?);
}

test "StringInterner: lookup invalid id" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    const result = interner.lookup(999);
    try std.testing.expect(result == null);
}

test "MacroCallInterner: basic interning" {
    var interner = MacroCallInterner.init(std.testing.allocator);
    defer interner.deinit();

    const loc1 = types.MacroCallLoc{
        .file_id = "test.ms",
        .def_id = 1,
        .range = .{ .start = 0, .end = 10 },
    };

    const loc2 = types.MacroCallLoc{
        .file_id = "other.ms",
        .def_id = 2,
        .range = .{ .start = 5, .end = 15 },
    };

    const id1 = try interner.intern(loc1);
    const id2 = try interner.intern(loc2);
    const id3 = try interner.intern(loc1); // Same as id1

    try std.testing.expectEqual(id1, id3);
    try std.testing.expect(id1 != id2);
}

test "MacroCallInterner: lookup" {
    var interner = MacroCallInterner.init(std.testing.allocator);
    defer interner.deinit();

    const loc = types.MacroCallLoc{
        .file_id = "test.ms",
        .def_id = 42,
        .range = .{ .start = 100, .end = 200 },
    };

    const id = try interner.intern(loc);
    const result = interner.lookup(id);

    try std.testing.expectEqual(@as(u64, 42), result.def_id);
    try std.testing.expectEqual(@as(u32, 100), result.range.start);
    try std.testing.expectEqual(@as(u32, 200), result.range.end);
}
