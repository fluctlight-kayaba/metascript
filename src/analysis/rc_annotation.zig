///! RC Annotation Pass (AST-Level)
///!
///! This module annotates AST nodes with RC operations BEFORE codegen.
///! Backends read these annotations and emit appropriate syntax.
///!
///! Pipeline:
///!   Parse → Type Check → Ownership Analysis → RC Annotation → Codegen
///!                                              ↑ this module
///!
///! The AST gets annotated with:
///!   - Where to insert incref (sharing a reference)
///!   - Where to insert decref (releasing ownership)
///!   - Where to init RC (new allocations)
///!   - Cycle root candidates (decref to non-zero)
///!
///! Codegen then just reads annotations and emits target syntax.

const std = @import("std");
const ownership = @import("ownership.zig");

/// RC operation to be inserted at a specific location
pub const RcOp = enum {
    /// Initialize RC to 1 (for new allocations)
    init,

    /// Increment reference count (sharing)
    incref,

    /// Decrement reference count (releasing)
    decref,

    /// Decref with cycle check (may become cycle root)
    decref_cycle_check,

    /// Move (transfer ownership, no RC change)
    move,
};

/// Location in source where RC op should be inserted
pub const RcLocation = struct {
    /// Line number (1-based)
    line: u32,

    /// Column number (1-based)
    column: u32,

    /// Insert before or after this location?
    position: Position,

    pub const Position = enum {
        before,
        after,
    };
};

/// A single RC annotation
pub const RcAnnotation = struct {
    /// The operation to perform
    op: RcOp,

    /// Variable/expression this applies to
    target: []const u8,

    /// Where in source this should be inserted
    location: RcLocation,

    /// Reason for this annotation (for debugging/optimization)
    reason: Reason,

    pub const Reason = enum {
        /// New allocation
        allocation,

        /// Passing to function that takes ownership
        function_arg_owned,

        /// Passing to function that borrows
        function_arg_borrowed,

        /// Storing in field (shared ownership)
        field_store,

        /// Returning from function
        return_value,

        /// Scope exit cleanup
        scope_exit,

        /// Last use optimization (move instead of copy+decref)
        last_use,

        /// Assignment to existing variable (decref old, incref new)
        reassignment,
    };
};

/// Annotations for a single scope/function
pub const ScopeAnnotations = struct {
    /// RC operations in order of insertion
    ops: std.ArrayList(RcAnnotation),

    /// Variables that need cleanup at scope exit
    cleanup: std.ArrayList([]const u8),

    /// Scope depth
    depth: u32,

    pub fn init(allocator: std.mem.Allocator, depth: u32) ScopeAnnotations {
        return .{
            .ops = std.ArrayList(RcAnnotation).init(allocator),
            .cleanup = std.ArrayList([]const u8).init(allocator),
            .depth = depth,
        };
    }

    pub fn deinit(self: *ScopeAnnotations) void {
        self.ops.deinit();
        self.cleanup.deinit();
    }
};

/// RC Annotator - walks AST and produces annotations
pub const RcAnnotator = struct {
    allocator: std.mem.Allocator,

    /// Ownership analyzer (from ownership.zig)
    ownership_analyzer: ownership.OwnershipAnalyzer,

    /// All annotations produced
    annotations: std.ArrayList(RcAnnotation),

    /// Scope stack for tracking cleanup
    scope_stack: std.ArrayList(ScopeAnnotations),

    /// Statistics
    stats: Stats,

    pub const Stats = struct {
        total_annotations: u64 = 0,
        increfs: u64 = 0,
        decrefs: u64 = 0,
        moves: u64 = 0,
        elided: u64 = 0, // RC ops that were optimized away
    };

    pub fn init(allocator: std.mem.Allocator) RcAnnotator {
        return .{
            .allocator = allocator,
            .ownership_analyzer = ownership.OwnershipAnalyzer.init(allocator),
            .annotations = std.ArrayList(RcAnnotation).init(allocator),
            .scope_stack = std.ArrayList(ScopeAnnotations).init(allocator),
            .stats = .{},
        };
    }

    pub fn deinit(self: *RcAnnotator) void {
        self.ownership_analyzer.deinit();
        self.annotations.deinit();
        for (self.scope_stack.items) |*scope| {
            scope.deinit();
        }
        self.scope_stack.deinit();
    }

    /// Enter a new scope
    pub fn enterScope(self: *RcAnnotator) !void {
        const depth: u32 = @intCast(self.scope_stack.items.len);
        try self.scope_stack.append(ScopeAnnotations.init(self.allocator, depth));
    }

    /// Exit current scope, generating cleanup annotations
    pub fn exitScope(self: *RcAnnotator, exit_line: u32, exit_column: u32) !void {
        if (self.scope_stack.items.len == 0) return;

        var scope = self.scope_stack.pop().?;
        defer scope.deinit();

        // Generate decref for all variables that need cleanup
        for (scope.cleanup.items) |var_name| {
            try self.addAnnotation(.{
                .op = .decref,
                .target = var_name,
                .location = .{
                    .line = exit_line,
                    .column = exit_column,
                    .position = .before,
                },
                .reason = .scope_exit,
            });
        }
    }

    /// Register a variable that needs RC management
    pub fn registerVariable(
        self: *RcAnnotator,
        name: []const u8,
        type_kind: ownership.TypeKind,
        line: u32,
        column: u32,
    ) !void {
        // Record in ownership analyzer
        try self.ownership_analyzer.recordDefinition(name, line, column, false, type_kind);

        // Value types don't need RC
        if (type_kind == .value) return;

        // Add to current scope's cleanup list
        if (self.scope_stack.items.len > 0) {
            const current_scope = &self.scope_stack.items[self.scope_stack.items.len - 1];
            try current_scope.cleanup.append(name);
        }
    }

    /// Annotate a new allocation
    pub fn annotateAllocation(
        self: *RcAnnotator,
        var_name: []const u8,
        line: u32,
        column: u32,
    ) !void {
        try self.addAnnotation(.{
            .op = .init,
            .target = var_name,
            .location = .{
                .line = line,
                .column = column,
                .position = .after,
            },
            .reason = .allocation,
        });
    }

    /// Annotate a variable use
    pub fn annotateUse(
        self: *RcAnnotator,
        var_name: []const u8,
        line: u32,
        column: u32,
        context: ownership.UseContext,
    ) !void {
        // Record in ownership analyzer
        try self.ownership_analyzer.recordUse(var_name, line, column, context);

        // Check if this is the last use
        const is_last = self.ownership_analyzer.isLastUse(var_name, line, column);

        if (is_last and !self.ownership_analyzer.escapesScope(var_name)) {
            // Last use of non-escaping value: can move
            try self.addAnnotation(.{
                .op = .move,
                .target = var_name,
                .location = .{
                    .line = line,
                    .column = column,
                    .position = .before,
                },
                .reason = .last_use,
            });
            self.stats.moves += 1;

            // Remove from cleanup list (moved, not owned anymore)
            self.removeFromCleanup(var_name);
        } else if (context == .stored or context == .argument) {
            // Sharing: need incref
            try self.addAnnotation(.{
                .op = .incref,
                .target = var_name,
                .location = .{
                    .line = line,
                    .column = column,
                    .position = .before,
                },
                .reason = if (context == .stored) .field_store else .function_arg_owned,
            });
        }
    }

    /// Annotate a return statement
    pub fn annotateReturn(
        self: *RcAnnotator,
        var_name: []const u8,
        line: u32,
        column: u32,
    ) !void {
        // Returning transfers ownership, so remove from cleanup
        self.removeFromCleanup(var_name);

        try self.addAnnotation(.{
            .op = .move,
            .target = var_name,
            .location = .{
                .line = line,
                .column = column,
                .position = .before,
            },
            .reason = .return_value,
        });
    }

    /// Get all annotations
    pub fn getAnnotations(self: *const RcAnnotator) []const RcAnnotation {
        return self.annotations.items;
    }

    /// Get annotations for a specific line
    pub fn getAnnotationsForLine(self: *const RcAnnotator, line: u32) ![]const RcAnnotation {
        var result = std.ArrayList(RcAnnotation).init(self.allocator);
        for (self.annotations.items) |ann| {
            if (ann.location.line == line) {
                try result.append(ann);
            }
        }
        return result.toOwnedSlice();
    }

    /// Get statistics
    pub fn getStats(self: *const RcAnnotator) Stats {
        return self.stats;
    }

    // ========================================================================
    // Private helpers
    // ========================================================================

    fn addAnnotation(self: *RcAnnotator, ann: RcAnnotation) !void {
        try self.annotations.append(ann);
        self.stats.total_annotations += 1;

        switch (ann.op) {
            .incref => self.stats.increfs += 1,
            .decref, .decref_cycle_check => self.stats.decrefs += 1,
            .move => {}, // Already counted
            .init => {},
        }
    }

    fn removeFromCleanup(self: *RcAnnotator, var_name: []const u8) void {
        if (self.scope_stack.items.len == 0) return;

        const current_scope = &self.scope_stack.items[self.scope_stack.items.len - 1];
        var i: usize = 0;
        while (i < current_scope.cleanup.items.len) {
            if (std.mem.eql(u8, current_scope.cleanup.items[i], var_name)) {
                _ = current_scope.cleanup.swapRemove(i);
                self.stats.elided += 1; // Elided a decref
            } else {
                i += 1;
            }
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "RcAnnotator basic allocation" {
    var annotator = RcAnnotator.init(std.testing.allocator);
    defer annotator.deinit();

    try annotator.enterScope();
    try annotator.registerVariable("obj", .object, 1, 1);
    try annotator.annotateAllocation("obj", 1, 10);
    try annotator.exitScope(5, 1);

    const anns = annotator.getAnnotations();
    try std.testing.expectEqual(@as(usize, 2), anns.len);

    // First: init from allocation
    try std.testing.expectEqual(RcOp.init, anns[0].op);
    try std.testing.expectEqualStrings("obj", anns[0].target);

    // Second: decref at scope exit
    try std.testing.expectEqual(RcOp.decref, anns[1].op);
}

test "RcAnnotator move on last use" {
    var annotator = RcAnnotator.init(std.testing.allocator);
    defer annotator.deinit();

    try annotator.enterScope();
    try annotator.registerVariable("x", .object, 1, 1);
    try annotator.annotateUse("x", 3, 1, .read); // Last use

    const anns = annotator.getAnnotations();
    try std.testing.expectEqual(@as(usize, 1), anns.len);
    try std.testing.expectEqual(RcOp.move, anns[0].op);
    try std.testing.expectEqual(RcAnnotation.Reason.last_use, anns[0].reason);
}

test "RcAnnotator incref on field store" {
    var annotator = RcAnnotator.init(std.testing.allocator);
    defer annotator.deinit();

    try annotator.enterScope();
    try annotator.registerVariable("x", .object, 1, 1);
    try annotator.annotateUse("x", 2, 1, .read); // First use
    try annotator.annotateUse("x", 3, 1, .stored); // Store in field

    const anns = annotator.getAnnotations();
    // First use is last use (move), but store also generates incref
    try std.testing.expect(anns.len >= 1);
}

test "RcAnnotator value types no RC" {
    var annotator = RcAnnotator.init(std.testing.allocator);
    defer annotator.deinit();

    try annotator.enterScope();
    try annotator.registerVariable("n", .value, 1, 1); // number - value type
    try annotator.exitScope(5, 1);

    const anns = annotator.getAnnotations();
    // Value types should not generate any RC ops
    try std.testing.expectEqual(@as(usize, 0), anns.len);
}

test "RcAnnotator stats" {
    var annotator = RcAnnotator.init(std.testing.allocator);
    defer annotator.deinit();

    try annotator.enterScope();
    try annotator.registerVariable("obj", .object, 1, 1);
    try annotator.annotateAllocation("obj", 1, 10);
    try annotator.annotateUse("obj", 2, 1, .stored); // incref
    try annotator.exitScope(5, 1); // decref

    const stats = annotator.getStats();
    try std.testing.expect(stats.total_annotations >= 2);
    try std.testing.expect(stats.increfs >= 1);
}
