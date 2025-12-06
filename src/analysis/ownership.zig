///! Lobster-style Ownership Analysis
///!
///! This module provides AST-level ownership annotations that are generic
///! across all backends. Each backend decides how to use this information:
///!
///! - C Backend: Uses ownership info to insert/elide RC operations
///! - JS Backend: Mostly ignores (GC handles memory)
///! - Erlang Backend: Ignores (immutable data + per-process GC)
///!
///! Based on "Ownership-Based Memory Management" from Lobster:
///! Every AST node has `wants` (from children) and `provides` (to parent).

const std = @import("std");

// Note: For AST integration, import via: const ownership = @import("analysis/ownership.zig");
// AST-dependent functions are in a separate section at the bottom

/// Ownership classification for values
pub const Ownership = enum {
    /// Caller owns the value and is responsible for freeing it
    owned,

    /// Caller borrows the value and must not free it
    borrowed,

    /// Reference counted - caller gets a reference (incref on receive)
    shared,

    /// Unknown ownership (requires runtime check or conservative handling)
    unknown,
};

/// Escape reason - why a value escapes its defining scope
pub const EscapeReason = enum {
    /// Does not escape - can be stack allocated
    none,

    /// Returned from function
    returned,

    /// Stored in object field (outlives current scope)
    stored_in_field,

    /// Captured by closure
    captured_by_closure,

    /// Passed to function that may store it
    passed_to_unknown_sink,

    /// Stored in global/module-level variable
    stored_in_global,
};

/// Ownership information for an AST node
pub const OwnershipInfo = struct {
    /// What ownership this node wants from its children
    wants: Ownership = .unknown,

    /// What ownership this node provides to its parent
    provides: Ownership = .unknown,

    /// Is this the last use of the value? (enables decref elision)
    is_last_use: bool = false,

    /// Does this value escape its defining scope?
    escape_reason: EscapeReason = .none,

    /// Should RC operations be inserted for this node?
    needs_rc: bool = true,

    pub fn escapes(self: OwnershipInfo) bool {
        return self.escape_reason != .none;
    }

    /// Create info for an owned value (common case for `new` expressions)
    pub fn owned() OwnershipInfo {
        return .{
            .wants = .owned,
            .provides = .owned,
            .needs_rc = true,
        };
    }

    /// Create info for a borrowed reference (parameter, temporary)
    pub fn borrowed() OwnershipInfo {
        return .{
            .wants = .borrowed,
            .provides = .borrowed,
            .needs_rc = false, // No RC ops for borrows
        };
    }

    /// Create info for a shared reference (stored in field, etc.)
    pub fn shared() OwnershipInfo {
        return .{
            .wants = .shared,
            .provides = .shared,
            .needs_rc = true,
        };
    }

    /// Create info for last use (enables decref elision)
    pub fn lastUse() OwnershipInfo {
        return .{
            .wants = .owned,
            .provides = .owned,
            .is_last_use = true,
            .needs_rc = false, // Elide decref on last use (move semantics)
        };
    }
};

/// Variable use site information
pub const UseInfo = struct {
    /// Source location of the use
    line: u32,
    column: u32,

    /// Context of the use
    context: UseContext,
};

/// Context in which a variable is used
pub const UseContext = enum {
    /// Read (e.g., `x + 1`)
    read,

    /// Passed as argument (e.g., `foo(x)`)
    argument,

    /// Returned from function (e.g., `return x`)
    returned,

    /// Stored in field (e.g., `obj.field = x`)
    stored,

    /// Captured by closure
    captured,
};

/// Definition site information
pub const DefInfo = struct {
    line: u32,
    column: u32,

    /// Is this a parameter?
    is_parameter: bool,

    /// Type of the value (for RC classification)
    type_kind: TypeKind,
};

/// Type classification for ownership purposes
pub const TypeKind = enum {
    /// Primitive value type (number, boolean) - no RC needed
    value,

    /// String - needs RC (dynamically allocated)
    string,

    /// Interned string literal - NO RC needed
    /// These are compile-time constants with static lifetime.
    /// ms_intern_get() returns borrowed reference to static storage.
    interned_string,

    /// Object/class instance - needs RC
    object,

    /// Array - needs RC
    array,

    /// Function/closure - needs RC if captures
    function,

    /// Unknown - treat conservatively
    unknown,

    /// Returns true if this type requires reference counting
    pub fn needsRc(self: TypeKind) bool {
        return switch (self) {
            .value, .interned_string => false,
            .string, .object, .array, .function, .unknown => true,
        };
    }
};

/// Ownership analyzer - computes ownership info for AST nodes
pub const OwnershipAnalyzer = struct {
    allocator: std.mem.Allocator,

    /// Per-variable: list of use sites
    uses: std.StringHashMap(std.ArrayList(UseInfo)),

    /// Per-variable: definition site
    definitions: std.StringHashMap(DefInfo),

    /// Per-variable: computed ownership info
    ownership: std.StringHashMap(OwnershipInfo),

    /// Variables that escape their defining scope
    escaping: std.StringHashMap(EscapeReason),

    pub fn init(allocator: std.mem.Allocator) OwnershipAnalyzer {
        return .{
            .allocator = allocator,
            .uses = std.StringHashMap(std.ArrayList(UseInfo)).init(allocator),
            .definitions = std.StringHashMap(DefInfo).init(allocator),
            .ownership = std.StringHashMap(OwnershipInfo).init(allocator),
            .escaping = std.StringHashMap(EscapeReason).init(allocator),
        };
    }

    pub fn deinit(self: *OwnershipAnalyzer) void {
        var use_iter = self.uses.valueIterator();
        while (use_iter.next()) |list| {
            list.deinit();
        }
        self.uses.deinit();
        self.definitions.deinit();
        self.ownership.deinit();
        self.escaping.deinit();
    }

    /// Check if a variable use is the last use
    pub fn isLastUse(self: *const OwnershipAnalyzer, var_name: []const u8, line: u32, column: u32) bool {
        const uses_list = self.uses.get(var_name) orelse return true; // No uses = last use
        if (uses_list.items.len == 0) return true;

        // Find the last use
        var last_line: u32 = 0;
        var last_col: u32 = 0;
        for (uses_list.items) |use| {
            if (use.line > last_line or (use.line == last_line and use.column > last_col)) {
                last_line = use.line;
                last_col = use.column;
            }
        }

        return line == last_line and column == last_col;
    }

    /// Check if a variable escapes its defining scope
    pub fn escapesScope(self: *const OwnershipAnalyzer, var_name: []const u8) bool {
        return self.escaping.contains(var_name);
    }

    /// Get escape reason for a variable
    pub fn getEscapeReason(self: *const OwnershipAnalyzer, var_name: []const u8) EscapeReason {
        return self.escaping.get(var_name) orelse .none;
    }

    /// Get ownership info for a variable
    pub fn getOwnership(self: *const OwnershipAnalyzer, var_name: []const u8) OwnershipInfo {
        return self.ownership.get(var_name) orelse OwnershipInfo{};
    }

    /// Record a variable definition
    pub fn recordDefinition(
        self: *OwnershipAnalyzer,
        name: []const u8,
        line: u32,
        column: u32,
        is_param: bool,
        type_kind: TypeKind,
    ) !void {
        try self.definitions.put(name, .{
            .line = line,
            .column = column,
            .is_parameter = is_param,
            .type_kind = type_kind,
        });
        // Use getOrPut to avoid leaking if the same name is registered twice
        // (e.g., same variable name in different function scopes)
        const gop = try self.uses.getOrPut(name);
        if (gop.found_existing) {
            // Clear the existing list instead of creating a new one
            gop.value_ptr.clearRetainingCapacity();
        } else {
            // New entry - create the list
            gop.value_ptr.* = std.ArrayList(UseInfo).init(self.allocator);
        }
    }

    /// Record a variable use
    pub fn recordUse(
        self: *OwnershipAnalyzer,
        name: []const u8,
        line: u32,
        column: u32,
        context: UseContext,
    ) !void {
        if (self.uses.getPtr(name)) |list| {
            try list.append(.{
                .line = line,
                .column = column,
                .context = context,
            });

            // Track escapes
            switch (context) {
                .returned => try self.escaping.put(name, .returned),
                .stored => {
                    if (!self.escaping.contains(name)) {
                        try self.escaping.put(name, .stored_in_field);
                    }
                },
                .captured => try self.escaping.put(name, .captured_by_closure),
                else => {},
            }
        }
    }

    /// Compute ownership info after all uses are recorded
    pub fn computeOwnership(self: *OwnershipAnalyzer) !void {
        var def_iter = self.definitions.iterator();
        while (def_iter.next()) |entry| {
            const name = entry.key_ptr.*;
            const def = entry.value_ptr.*;

            var info = OwnershipInfo{};

            // Check if this type needs RC (value and interned_string don't)
            if (!def.type_kind.needsRc()) {
                info.needs_rc = false;
                info.wants = .borrowed;
                info.provides = .borrowed;
            } else {
                // Reference types need RC
                info.needs_rc = true;

                // Check escape status
                if (self.escaping.get(name)) |reason| {
                    info.escape_reason = reason;
                    info.wants = .shared; // Escaping values need shared ownership
                    info.provides = .shared;
                } else {
                    // Non-escaping values can use owned semantics
                    info.wants = .owned;
                    info.provides = .owned;
                }
            }

            // Parameters are borrowed by default
            if (def.is_parameter) {
                info.wants = .borrowed;
            }

            try self.ownership.put(name, info);
        }
    }

    // ========================================================================
    // AST Integration
    // ========================================================================
    // The analyze() function is provided in a separate file that can import
    // both this module and the AST module. See: src/analysis/ast_ownership.zig
    //
    // Usage from C backend:
    //   const ownership = @import("../../analysis/ownership.zig");
    //   var analyzer = ownership.OwnershipAnalyzer.init(allocator);
    //   // Record definitions and uses as you walk the AST
    //   analyzer.recordDefinition("x", 1, 1, false, .object);
    //   analyzer.recordUse("x", 5, 1, .returned);
    //   analyzer.computeOwnership();
    //   const info = analyzer.getOwnership("x");
    //   // Use info.is_last_use, info.needs_rc, etc.
};

/// Infer TypeKind from a type name string
pub fn inferTypeKindFromName(type_name: []const u8) TypeKind {
    if (std.mem.eql(u8, type_name, "number") or
        std.mem.eql(u8, type_name, "boolean") or
        std.mem.eql(u8, type_name, "void") or
        std.mem.eql(u8, type_name, "i32") or
        std.mem.eql(u8, type_name, "i64") or
        std.mem.eql(u8, type_name, "f32") or
        std.mem.eql(u8, type_name, "f64"))
    {
        return .value;
    }

    if (std.mem.eql(u8, type_name, "string")) {
        return .string;
    }

    // Arrays
    if (std.mem.startsWith(u8, type_name, "Array") or
        std.mem.endsWith(u8, type_name, "[]"))
    {
        return .array;
    }

    // Function types
    if (std.mem.indexOf(u8, type_name, "=>") != null) {
        return .function;
    }

    // Assume user-defined types are objects
    return .object;
}

// ============================================================================
// Tests
// ============================================================================

test "OwnershipInfo defaults" {
    const info = OwnershipInfo{};
    try std.testing.expect(!info.is_last_use);
    try std.testing.expect(!info.escapes());
    try std.testing.expectEqual(Ownership.unknown, info.wants);
}

test "OwnershipInfo factory methods" {
    const owned = OwnershipInfo.owned();
    try std.testing.expectEqual(Ownership.owned, owned.wants);
    try std.testing.expect(owned.needs_rc);

    const borrowed = OwnershipInfo.borrowed();
    try std.testing.expectEqual(Ownership.borrowed, borrowed.wants);
    try std.testing.expect(!borrowed.needs_rc);

    const last = OwnershipInfo.lastUse();
    try std.testing.expect(last.is_last_use);
    try std.testing.expect(!last.needs_rc);
}

test "OwnershipAnalyzer basic" {
    var analyzer = OwnershipAnalyzer.init(std.testing.allocator);
    defer analyzer.deinit();

    try analyzer.recordDefinition("x", 1, 1, false, .value);
    try analyzer.recordUse("x", 2, 1, .read);
    try analyzer.recordUse("x", 3, 1, .read);

    try std.testing.expect(!analyzer.isLastUse("x", 2, 1));
    try std.testing.expect(analyzer.isLastUse("x", 3, 1));
}

test "OwnershipAnalyzer escape tracking" {
    var analyzer = OwnershipAnalyzer.init(std.testing.allocator);
    defer analyzer.deinit();

    try analyzer.recordDefinition("obj", 1, 1, false, .object);
    try analyzer.recordUse("obj", 2, 1, .returned);

    try std.testing.expect(analyzer.escapesScope("obj"));
    try std.testing.expectEqual(EscapeReason.returned, analyzer.getEscapeReason("obj"));
}

test "TypeKind inference from name" {
    try std.testing.expectEqual(TypeKind.value, inferTypeKindFromName("number"));
    try std.testing.expectEqual(TypeKind.value, inferTypeKindFromName("boolean"));
    try std.testing.expectEqual(TypeKind.string, inferTypeKindFromName("string"));
    try std.testing.expectEqual(TypeKind.array, inferTypeKindFromName("Array<number>"));
    try std.testing.expectEqual(TypeKind.array, inferTypeKindFromName("number[]"));
    try std.testing.expectEqual(TypeKind.object, inferTypeKindFromName("User"));
    try std.testing.expectEqual(TypeKind.object, inferTypeKindFromName("Point"));
}
