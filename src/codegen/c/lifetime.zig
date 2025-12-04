///! C Backend Lifetime Management
///!
///! This module uses the shared ownership analysis to determine where to insert
///! RC operations (ms_incref, ms_decref) in generated C code.
///!
///! Part of the DRC (Dynamic/Dual Reference Counting) system:
///! - Uses Lobster-style ownership analysis (shared: src/analysis/ownership.zig)
///! - Inserts RC ops based on ownership semantics
///! - Elides RC ops where safe (last-use, value types)
///!
///! This is C-SPECIFIC. JS/Erlang backends don't need this.

const std = @import("std");
const ownership = @import("../../analysis/ownership.zig");

/// Lifetime information for a variable in C code generation
pub const LifetimeInfo = struct {
    /// Variable name
    name: []const u8,

    /// Ownership info from shared analysis
    ownership_info: ownership.OwnershipInfo,

    /// Should we emit ms_incref when this variable is assigned?
    needs_incref: bool,

    /// Should we emit ms_decref when this variable goes out of scope?
    needs_decref: bool,

    /// Is this a stack-allocated value (no heap, no RC)?
    is_stack_allocated: bool,

    /// Scope depth where this variable was defined
    scope_depth: u32,
};

/// RC operation to insert in generated code
pub const RcOp = enum {
    /// ms_incref(ptr)
    incref,

    /// ms_decref(ptr)
    decref,

    /// No RC operation needed
    none,
};

/// Lifetime manager for C code generation
pub const LifetimeManager = struct {
    allocator: std.mem.Allocator,

    /// Ownership analyzer (from shared infrastructure)
    analyzer: ownership.OwnershipAnalyzer,

    /// Per-variable lifetime info
    lifetimes: std.StringHashMap(LifetimeInfo),

    /// Current scope depth
    current_scope: u32,

    /// Variables to decref when exiting current scope
    scope_cleanup: std.ArrayList([]const u8),

    pub fn init(allocator: std.mem.Allocator) LifetimeManager {
        return .{
            .allocator = allocator,
            .analyzer = ownership.OwnershipAnalyzer.init(allocator),
            .lifetimes = std.StringHashMap(LifetimeInfo).init(allocator),
            .current_scope = 0,
            .scope_cleanup = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *LifetimeManager) void {
        self.analyzer.deinit();
        self.lifetimes.deinit();
        self.scope_cleanup.deinit();
    }

    /// Enter a new scope (function, block, etc.)
    pub fn enterScope(self: *LifetimeManager) void {
        self.current_scope += 1;
    }

    /// Exit current scope and return variables that need decref
    pub fn exitScope(self: *LifetimeManager) []const []const u8 {
        if (self.current_scope > 0) {
            self.current_scope -= 1;
        }

        // Find all variables defined in the scope we're leaving
        self.scope_cleanup.clearRetainingCapacity();

        var iter = self.lifetimes.iterator();
        while (iter.next()) |entry| {
            const info = entry.value_ptr.*;
            if (info.scope_depth > self.current_scope and info.needs_decref) {
                self.scope_cleanup.append(info.name) catch {};
            }
        }

        return self.scope_cleanup.items;
    }

    /// Register a variable definition
    pub fn registerVariable(
        self: *LifetimeManager,
        name: []const u8,
        type_kind: ownership.TypeKind,
        line: u32,
        column: u32,
    ) !void {
        // Record in ownership analyzer
        try self.analyzer.recordDefinition(name, line, column, false, type_kind);

        // Determine lifetime characteristics
        const is_value = type_kind == .value;

        try self.lifetimes.put(name, .{
            .name = name,
            .ownership_info = ownership.OwnershipInfo{},
            .needs_incref = false, // Will be computed later
            .needs_decref = !is_value, // Value types don't need decref
            .is_stack_allocated = is_value,
            .scope_depth = self.current_scope,
        });
    }

    /// Register a variable use (for last-use detection)
    pub fn registerUse(
        self: *LifetimeManager,
        name: []const u8,
        line: u32,
        column: u32,
        context: ownership.UseContext,
    ) !void {
        try self.analyzer.recordUse(name, line, column, context);
    }

    /// Compute final ownership info after all definitions and uses are recorded
    pub fn computeLifetimes(self: *LifetimeManager) !void {
        try self.analyzer.computeOwnership();

        // Update lifetime info with computed ownership
        var iter = self.lifetimes.iterator();
        while (iter.next()) |entry| {
            const name = entry.key_ptr.*;
            const info = entry.value_ptr;

            info.ownership_info = self.analyzer.getOwnership(name);

            // Update needs_incref based on ownership
            if (info.ownership_info.wants == .shared) {
                info.needs_incref = true;
            }

            // Check for escaping values
            if (self.analyzer.escapesScope(name)) {
                info.needs_decref = false; // Don't decref escaping values
            }
        }
    }

    /// Get the RC operation needed when assigning to a variable
    pub fn getAssignOp(self: *const LifetimeManager, name: []const u8) RcOp {
        const info = self.lifetimes.get(name) orelse return .none;

        if (info.is_stack_allocated) return .none;
        if (info.needs_incref) return .incref;

        return .none;
    }

    /// Get the RC operation needed when a variable is used
    pub fn getUseOp(
        self: *const LifetimeManager,
        name: []const u8,
        line: u32,
        column: u32,
    ) RcOp {
        const info = self.lifetimes.get(name) orelse return .none;

        if (info.is_stack_allocated) return .none;

        // Check if this is the last use
        if (self.analyzer.isLastUse(name, line, column)) {
            // Last use: we can transfer ownership instead of incref
            return .none;
        }

        // Not last use: need incref if passing to function that takes ownership
        return .none;
    }

    /// Check if a variable needs cleanup at scope exit
    pub fn needsCleanup(self: *const LifetimeManager, name: []const u8) bool {
        const info = self.lifetimes.get(name) orelse return false;
        return info.needs_decref and !info.is_stack_allocated;
    }

    /// Generate cleanup code for scope exit
    pub fn generateScopeCleanup(
        self: *LifetimeManager,
        writer: anytype,
    ) !void {
        const vars_to_cleanup = self.exitScope();

        for (vars_to_cleanup) |name| {
            try writer.print("    ms_decref({s});\n", .{name});
        }
    }
};

/// Check if a C type name represents a reference-counted type
pub fn isRcType(c_type: []const u8) bool {
    // Pointer types to user-defined structs need RC
    if (std.mem.endsWith(u8, c_type, "*")) {
        const base = c_type[0 .. c_type.len - 1];
        // Primitive pointers don't need RC
        if (std.mem.eql(u8, base, "char") or
            std.mem.eql(u8, base, "int") or
            std.mem.eql(u8, base, "double") or
            std.mem.eql(u8, base, "float") or
            std.mem.eql(u8, base, "void"))
        {
            return false;
        }
        return true;
    }

    // msString always needs RC
    if (std.mem.eql(u8, c_type, "msString*")) {
        return true;
    }

    return false;
}

/// Check if a variable should use move semantics (no incref on last use)
pub fn shouldMove(analyzer: *const ownership.OwnershipAnalyzer, name: []const u8, line: u32, column: u32) bool {
    // If this is the last use and value doesn't escape, we can move
    if (!analyzer.isLastUse(name, line, column)) {
        return false;
    }

    // Check if value escapes (returned, stored in field, etc.)
    if (analyzer.escapesScope(name)) {
        return false;
    }

    return true;
}

// ============================================================================
// Tests
// ============================================================================

test "LifetimeManager basic" {
    var manager = LifetimeManager.init(std.testing.allocator);
    defer manager.deinit();

    try manager.registerVariable("x", .value, 1, 1);
    try manager.registerVariable("obj", .object, 2, 1);

    try std.testing.expect(!manager.needsCleanup("x")); // Value type
    try std.testing.expect(manager.needsCleanup("obj")); // Object type
}

test "LifetimeManager scope tracking" {
    var manager = LifetimeManager.init(std.testing.allocator);
    defer manager.deinit();

    manager.enterScope();
    try manager.registerVariable("x", .object, 1, 1);

    const cleanup = manager.exitScope();
    try std.testing.expectEqual(@as(usize, 1), cleanup.len);
    try std.testing.expectEqualStrings("x", cleanup[0]);
}

test "LifetimeManager escape detection" {
    var manager = LifetimeManager.init(std.testing.allocator);
    defer manager.deinit();

    try manager.registerVariable("result", .object, 1, 1);
    try manager.registerUse("result", 5, 1, .returned);
    try manager.computeLifetimes();

    // Returned values should not be decreffed in the function
    try std.testing.expect(!manager.needsCleanup("result"));
}

test "isRcType" {
    try std.testing.expect(isRcType("Point*"));
    try std.testing.expect(isRcType("User*"));
    try std.testing.expect(isRcType("msString*"));
    try std.testing.expect(!isRcType("int"));
    try std.testing.expect(!isRcType("double"));
    try std.testing.expect(!isRcType("char*")); // Primitive pointer
    try std.testing.expect(!isRcType("void*")); // Void pointer
}

test "shouldMove" {
    var analyzer = ownership.OwnershipAnalyzer.init(std.testing.allocator);
    defer analyzer.deinit();

    try analyzer.recordDefinition("local", 1, 1, false, .object);
    try analyzer.recordUse("local", 2, 1, .read);

    // Last use of non-escaping variable = can move
    try std.testing.expect(shouldMove(&analyzer, "local", 2, 1));

    // Not last use = can't move
    try analyzer.recordUse("local", 3, 1, .read);
    try std.testing.expect(!shouldMove(&analyzer, "local", 2, 1));
}
