///! Dynamic Reference Counter (DRC)
///!
///! Unified module that orchestrates the complete DRC pipeline:
///!
///!   1. Ownership Analysis (Lobster-style)
///!   2. RC Annotation (where to insert ops)
///!   3. Cycle Detection (Bacon-Rajan)
///!   4. Backend Integration (via rc_trait)
///!
///! Usage:
///!   var drc = Drc.init(allocator);
///!   defer drc.deinit();
///!
///!   // Annotate an AST
///!   try drc.annotate(ast);
///!
///!   // Get annotations for codegen
///!   const ops = drc.getOpsForLine(10);
///!   for (ops) |op| {
///!       try backend.emit(op);
///!   }
///!
///! This module is backend-agnostic. Backends use rc_trait.zig for emission.

const std = @import("std");
const ownership = @import("ownership.zig");
const rc_annotation = @import("rc_annotation.zig");
const cycle_detection = @import("cycle_detection.zig");

/// DRC Configuration
pub const Config = struct {
    /// Enable cycle detection (disable for performance if no cycles possible)
    enable_cycle_detection: bool = true,

    /// Threshold for triggering cycle collection
    cycle_collection_threshold: usize = 100,

    /// Enable move optimization (elide RC ops on last use)
    enable_move_optimization: bool = true,

    /// Enable escape analysis (track values that escape scope)
    enable_escape_analysis: bool = true,

    /// Debug mode - emit comments explaining RC decisions
    debug_mode: bool = false,
};

/// DRC Statistics
pub const Stats = struct {
    /// Total variables analyzed
    variables_analyzed: u64 = 0,

    /// Variables requiring RC (non-value types)
    rc_variables: u64 = 0,

    /// Total RC operations generated
    total_ops: u64 = 0,

    /// Operations elided due to optimization
    ops_elided: u64 = 0,

    /// Incref operations
    increfs: u64 = 0,

    /// Decref operations
    decrefs: u64 = 0,

    /// Move operations (ownership transfer, no RC change)
    moves: u64 = 0,

    /// Potential cycle roots detected
    cycle_roots: u64 = 0,

    /// Cycles collected (if cycle detection ran)
    cycles_collected: u64 = 0,

    pub fn elisionRate(self: Stats) f64 {
        if (self.total_ops + self.ops_elided == 0) return 0;
        return @as(f64, @floatFromInt(self.ops_elided)) /
            @as(f64, @floatFromInt(self.total_ops + self.ops_elided));
    }
};

/// RC Operation with full context for codegen
pub const RcOp = struct {
    /// The operation type
    kind: Kind,

    /// Target variable/expression
    target: []const u8,

    /// Source location
    line: u32,
    column: u32,

    /// Insert before or after the line?
    position: Position,

    /// Why this op was generated (for debugging)
    reason: Reason,

    /// Optional comment for debug mode
    comment: ?[]const u8 = null,

    pub const Kind = enum {
        /// RC = 1 (new allocation)
        init,

        /// RC += 1 (sharing reference)
        incref,

        /// RC -= 1, free if zero
        decref,

        /// RC -= 1, check for cycle root if non-zero
        decref_cycle_check,

        /// Transfer ownership (no RC change)
        move,

        /// Scope cleanup marker (multiple decrefs)
        scope_cleanup_start,
        scope_cleanup_end,
    };

    pub const Position = enum {
        before,
        after,
    };

    pub const Reason = enum {
        allocation,
        assignment,
        reassignment,
        field_store,
        function_arg,
        return_value,
        scope_exit,
        last_use,
        cycle_candidate,
    };
};

/// Variable tracking for DRC
pub const Variable = struct {
    name: []const u8,
    type_kind: ownership.TypeKind,
    scope_depth: u32,
    line: u32,
    column: u32,

    /// Does this variable need RC management?
    needs_rc: bool,

    /// Has this variable escaped its scope?
    escaped: bool = false,

    /// Is this a parameter (borrowed by default)?
    is_parameter: bool = false,

    /// Current ownership state
    ownership_state: OwnershipState = .owned,

    pub const OwnershipState = enum {
        owned,      // We own it, must decref
        borrowed,   // We borrowed it, don't decref
        moved,      // Ownership transferred, don't decref
        unknown,
    };
};

/// The main DRC orchestrator
pub const Drc = struct {
    allocator: std.mem.Allocator,
    config: Config,

    /// Ownership analyzer
    ownership_analyzer: ownership.OwnershipAnalyzer,

    /// RC annotator
    annotator: rc_annotation.RcAnnotator,

    /// Cycle detector
    cycle_detector: cycle_detection.CycleDetector,

    /// All generated operations (sorted by line)
    operations: std.ArrayList(RcOp),

    /// Operations indexed by line for fast lookup
    ops_by_line: std.AutoHashMap(u32, std.ArrayList(RcOp)),

    /// All tracked variables
    variables: std.StringHashMap(Variable),

    /// Scope stack
    scope_stack: std.ArrayList(Scope),

    /// Statistics
    stats: Stats,

    pub const Scope = struct {
        depth: u32,
        variables: std.ArrayList([]const u8),
        start_line: u32,

        pub fn init(allocator: std.mem.Allocator, depth: u32, start_line: u32) Scope {
            return .{
                .depth = depth,
                .variables = std.ArrayList([]const u8).init(allocator),
                .start_line = start_line,
            };
        }

        pub fn deinit(self: *Scope) void {
            self.variables.deinit();
        }
    };

    pub fn init(allocator: std.mem.Allocator) Drc {
        return initWithConfig(allocator, .{});
    }

    pub fn initWithConfig(allocator: std.mem.Allocator, config: Config) Drc {
        return .{
            .allocator = allocator,
            .config = config,
            .ownership_analyzer = ownership.OwnershipAnalyzer.init(allocator),
            .annotator = rc_annotation.RcAnnotator.init(allocator),
            .cycle_detector = cycle_detection.CycleDetector.init(allocator),
            .operations = std.ArrayList(RcOp).init(allocator),
            .ops_by_line = std.AutoHashMap(u32, std.ArrayList(RcOp)).init(allocator),
            .variables = std.StringHashMap(Variable).init(allocator),
            .scope_stack = std.ArrayList(Scope).init(allocator),
            .stats = .{},
        };
    }

    pub fn deinit(self: *Drc) void {
        self.ownership_analyzer.deinit();
        self.annotator.deinit();
        self.cycle_detector.deinit();
        self.operations.deinit();

        var iter = self.ops_by_line.valueIterator();
        while (iter.next()) |list| {
            list.deinit();
        }
        self.ops_by_line.deinit();

        self.variables.deinit();

        for (self.scope_stack.items) |*scope| {
            scope.deinit();
        }
        self.scope_stack.deinit();
    }

    // ========================================================================
    // Scope Management
    // ========================================================================

    pub fn enterScope(self: *Drc, start_line: u32) !void {
        const depth: u32 = @intCast(self.scope_stack.items.len);
        try self.scope_stack.append(Scope.init(self.allocator, depth, start_line));
    }

    pub fn exitScope(self: *Drc, end_line: u32, end_column: u32) !void {
        if (self.scope_stack.items.len == 0) return;

        var scope = self.scope_stack.pop().?;
        defer scope.deinit();

        // Generate cleanup for all variables in this scope
        if (scope.variables.items.len > 0) {
            try self.addOp(.{
                .kind = .scope_cleanup_start,
                .target = "",
                .line = end_line,
                .column = end_column,
                .position = .before,
                .reason = .scope_exit,
                .comment = if (self.config.debug_mode) "// DRC: scope cleanup" else null,
            });

            for (scope.variables.items) |var_name| {
                if (self.variables.get(var_name)) |v| {
                    // Only decref if we still own it and it needs RC
                    if (v.needs_rc and v.ownership_state == .owned and !v.escaped) {
                        try self.addOp(.{
                            .kind = if (self.config.enable_cycle_detection) .decref_cycle_check else .decref,
                            .target = var_name,
                            .line = end_line,
                            .column = end_column,
                            .position = .before,
                            .reason = .scope_exit,
                        });
                        self.stats.decrefs += 1;
                    }
                }
            }

            try self.addOp(.{
                .kind = .scope_cleanup_end,
                .target = "",
                .line = end_line,
                .column = end_column,
                .position = .before,
                .reason = .scope_exit,
            });
        }
    }

    // ========================================================================
    // Variable Registration
    // ========================================================================

    /// Register a new variable
    pub fn registerVariable(
        self: *Drc,
        name: []const u8,
        type_kind: ownership.TypeKind,
        line: u32,
        column: u32,
        is_parameter: bool,
    ) !void {
        const needs_rc = type_kind != .value;

        const variable = Variable{
            .name = name,
            .type_kind = type_kind,
            .scope_depth = @intCast(self.scope_stack.items.len),
            .line = line,
            .column = column,
            .needs_rc = needs_rc,
            .is_parameter = is_parameter,
            .ownership_state = if (is_parameter) .borrowed else .owned,
        };

        try self.variables.put(name, variable);
        self.stats.variables_analyzed += 1;

        if (needs_rc) {
            self.stats.rc_variables += 1;

            // Add to current scope for cleanup tracking
            if (self.scope_stack.items.len > 0) {
                const current = &self.scope_stack.items[self.scope_stack.items.len - 1];
                try current.variables.append(name);
            }
        }

        // Record in ownership analyzer
        try self.ownership_analyzer.recordDefinition(name, line, column, is_parameter, type_kind);
    }

    /// Register an allocation (new expression)
    pub fn registerAllocation(
        self: *Drc,
        var_name: []const u8,
        line: u32,
        column: u32,
    ) !void {
        // RC is initialized to 1 by allocator, but we track it
        try self.addOp(.{
            .kind = .init,
            .target = var_name,
            .line = line,
            .column = column,
            .position = .after,
            .reason = .allocation,
            .comment = if (self.config.debug_mode) "// DRC: RC=1 from allocation" else null,
        });
    }

    // ========================================================================
    // Use Tracking
    // ========================================================================

    /// Track a variable use
    pub fn trackUse(
        self: *Drc,
        var_name: []const u8,
        line: u32,
        column: u32,
        context: UseContext,
    ) !void {
        const v = self.variables.get(var_name) orelse return;
        if (!v.needs_rc) return;

        try self.ownership_analyzer.recordUse(var_name, line, column, toOwnershipContext(context));

        // Check for last use optimization
        if (self.config.enable_move_optimization) {
            const is_last = self.ownership_analyzer.isLastUse(var_name, line, column);
            const escapes = self.ownership_analyzer.escapesScope(var_name);

            if (is_last and !escapes) {
                // Last use - can move instead of copy
                try self.addOp(.{
                    .kind = .move,
                    .target = var_name,
                    .line = line,
                    .column = column,
                    .position = .before,
                    .reason = .last_use,
                    .comment = if (self.config.debug_mode) "// DRC: move (last use)" else null,
                });
                self.stats.moves += 1;
                self.stats.ops_elided += 1; // Elided a decref

                // Mark as moved
                if (self.variables.getPtr(var_name)) |vptr| {
                    vptr.ownership_state = .moved;
                }
                return;
            }
        }

        // Handle based on context
        switch (context) {
            .field_store => {
                // Storing in field = sharing reference
                try self.addOp(.{
                    .kind = .incref,
                    .target = var_name,
                    .line = line,
                    .column = column,
                    .position = .before,
                    .reason = .field_store,
                    .comment = if (self.config.debug_mode) "// DRC: incref for field store" else null,
                });
                self.stats.increfs += 1;
            },
            .function_arg_owned => {
                // Passing to function that takes ownership
                try self.addOp(.{
                    .kind = .incref,
                    .target = var_name,
                    .line = line,
                    .column = column,
                    .position = .before,
                    .reason = .function_arg,
                });
                self.stats.increfs += 1;
            },
            .returned => {
                // Returning transfers ownership out
                if (self.variables.getPtr(var_name)) |vptr| {
                    vptr.escaped = true;
                    vptr.ownership_state = .moved;
                }
                try self.addOp(.{
                    .kind = .move,
                    .target = var_name,
                    .line = line,
                    .column = column,
                    .position = .before,
                    .reason = .return_value,
                    .comment = if (self.config.debug_mode) "// DRC: move to caller" else null,
                });
                self.stats.moves += 1;
            },
            .variable_copy => {
                // Variable-to-variable copy is handled by trackVariableCopy()
                // This case should not be reached via trackUse()
            },
            .read, .function_arg_borrowed => {
                // Just reading or borrowing - no RC change
            },
        }
    }

    pub const UseContext = enum {
        read,
        field_store,
        function_arg_owned,
        function_arg_borrowed,
        returned,
        /// Variable-to-variable copy: let x = y where y is reference type
        /// This creates a shared reference, needs incref on the NEW variable
        variable_copy,
    };

    fn toOwnershipContext(ctx: UseContext) ownership.UseContext {
        return switch (ctx) {
            .read, .variable_copy => .read,
            .field_store => .stored,
            .function_arg_owned, .function_arg_borrowed => .argument,
            .returned => .returned,
        };
    }

    // ========================================================================
    // Variable Copy Handling
    // ========================================================================

    /// Track a variable-to-variable copy: let target = source
    /// This creates a shared reference - target needs incref after assignment
    pub fn trackVariableCopy(
        self: *Drc,
        target_var: []const u8,
        source_var: []const u8,
        line: u32,
        column: u32,
    ) !void {
        _ = source_var; // Source is used for documentation/debugging

        // Check if target is a reference type that needs RC
        const v = self.variables.get(target_var) orelse return;
        if (!v.needs_rc) return;

        // Emit incref for the target variable after the assignment
        try self.addOp(.{
            .kind = .incref,
            .target = target_var,
            .line = line,
            .column = column,
            .position = .after, // After the assignment line
            .reason = .assignment,
            .comment = if (self.config.debug_mode) "// DRC: incref for variable copy" else null,
        });
        self.stats.increfs += 1;
    }

    // ========================================================================
    // Reassignment Handling
    // ========================================================================

    /// Handle variable reassignment (decref old, incref new)
    pub fn trackReassignment(
        self: *Drc,
        var_name: []const u8,
        line: u32,
        column: u32,
    ) !void {
        const v = self.variables.get(var_name) orelse return;
        if (!v.needs_rc) return;

        // Decref old value
        try self.addOp(.{
            .kind = .decref,
            .target = var_name,
            .line = line,
            .column = column,
            .position = .before,
            .reason = .reassignment,
            .comment = if (self.config.debug_mode) "// DRC: decref old value" else null,
        });
        self.stats.decrefs += 1;

        // New value will be increffed by assignment handling
    }

    // ========================================================================
    // Query Interface (for Codegen)
    // ========================================================================

    /// Get all RC operations for a specific line
    pub fn getOpsForLine(self: *Drc, line: u32) []const RcOp {
        if (self.ops_by_line.get(line)) |list| {
            return list.items;
        }
        return &[_]RcOp{};
    }

    /// Get all RC operations
    pub fn getAllOps(self: *Drc) []const RcOp {
        return self.operations.items;
    }

    /// Get operations before a specific position
    pub fn getOpsBefore(self: *Drc, line: u32) []const RcOp {
        var result = std.ArrayList(RcOp).init(self.allocator);
        for (self.getOpsForLine(line)) |op| {
            if (op.position == .before) {
                result.append(op) catch {};
            }
        }
        return result.toOwnedSlice() catch &[_]RcOp{};
    }

    /// Get operations after a specific position
    pub fn getOpsAfter(self: *Drc, line: u32) []const RcOp {
        var result = std.ArrayList(RcOp).init(self.allocator);
        for (self.getOpsForLine(line)) |op| {
            if (op.position == .after) {
                result.append(op) catch {};
            }
        }
        return result.toOwnedSlice() catch &[_]RcOp{};
    }

    /// Get statistics
    pub fn getStats(self: *const Drc) Stats {
        return self.stats;
    }

    /// Check if a variable needs RC management
    pub fn needsRc(self: *const Drc, var_name: []const u8) bool {
        if (self.variables.get(var_name)) |v| {
            return v.needs_rc;
        }
        return false;
    }

    // ========================================================================
    // Cycle Detection Integration
    // ========================================================================

    /// Register a potential cycle root (decref to non-zero)
    pub fn registerCycleRoot(self: *Drc, ptr: usize) !void {
        if (!self.config.enable_cycle_detection) return;
        try self.cycle_detector.addRoot(ptr);
        self.stats.cycle_roots += 1;
    }

    /// Should we run cycle collection?
    pub fn shouldCollectCycles(self: *const Drc) bool {
        if (!self.config.enable_cycle_detection) return false;
        return self.cycle_detector.shouldCollect(self.config.cycle_collection_threshold);
    }

    /// Get cycle detector for runtime integration
    pub fn getCycleDetector(self: *Drc) *cycle_detection.CycleDetector {
        return &self.cycle_detector;
    }

    // ========================================================================
    // Finalization
    // ========================================================================

    /// Finalize analysis and sort operations
    pub fn finalize(self: *Drc) !void {
        // Compute final ownership info
        try self.ownership_analyzer.computeOwnership();

        // Sort operations by line number for efficient codegen
        std.mem.sort(RcOp, self.operations.items, {}, struct {
            fn lessThan(_: void, a: RcOp, b: RcOp) bool {
                if (a.line != b.line) return a.line < b.line;
                if (a.position != b.position) return a.position == .before;
                return a.column < b.column;
            }
        }.lessThan);

        // Update stats
        self.stats.total_ops = @intCast(self.operations.items.len);
    }

    // ========================================================================
    // Private Helpers
    // ========================================================================

    fn addOp(self: *Drc, op: RcOp) !void {
        try self.operations.append(op);

        // Index by line
        const entry = try self.ops_by_line.getOrPut(op.line);
        if (!entry.found_existing) {
            entry.value_ptr.* = std.ArrayList(RcOp).init(self.allocator);
        }
        try entry.value_ptr.append(op);
    }
};

// ============================================================================
// Code Generation Helpers
// ============================================================================

/// Generate C code for an RC operation
pub fn emitCCode(op: RcOp, writer: anytype) !void {
    switch (op.kind) {
        .init => {
            // RC is set by ms_alloc, just emit comment if debug
            if (op.comment) |c| try writer.print("{s}\n", .{c});
        },
        .incref => {
            if (op.comment) |c| try writer.print("    {s}\n", .{c});
            try writer.print("    ms_incref({s});\n", .{op.target});
        },
        .decref => {
            if (op.comment) |c| try writer.print("    {s}\n", .{c});
            try writer.print("    ms_decref({s});\n", .{op.target});
        },
        .decref_cycle_check => {
            if (op.comment) |c| try writer.print("    {s}\n", .{c});
            try writer.print("    ms_decref_cycle_check({s});\n", .{op.target});
        },
        .move => {
            // Move is a no-op in terms of RC, but we might want to track it
            if (op.comment) |c| try writer.print("    {s}\n", .{c});
        },
        .scope_cleanup_start => {
            if (op.comment) |c| try writer.print("    {s}\n", .{c});
        },
        .scope_cleanup_end => {},
    }
}

/// Generate Zig code for an RC operation
pub fn emitZigCode(op: RcOp, writer: anytype) !void {
    switch (op.kind) {
        .init => {
            if (op.comment) |c| try writer.print("    {s}\n", .{c});
        },
        .incref => {
            if (op.comment) |c| try writer.print("    {s}\n", .{c});
            try writer.print("    _ = @atomicAdd(&{s}.rc, 1, .seq_cst);\n", .{op.target});
        },
        .decref => {
            if (op.comment) |c| try writer.print("    {s}\n", .{c});
            try writer.print("    if (@atomicSub(&{s}.rc, 1, .seq_cst) == 1) {s}.deinit();\n", .{ op.target, op.target });
        },
        .decref_cycle_check => {
            try writer.print("    const prev = @atomicSub(&{s}.rc, 1, .seq_cst);\n", .{op.target});
            try writer.print("    if (prev == 1) {s}.deinit() else cycle_detector.addRoot(@intFromPtr({s}));\n", .{ op.target, op.target });
        },
        .move, .scope_cleanup_start, .scope_cleanup_end => {
            if (op.comment) |c| try writer.print("    {s}\n", .{c});
        },
    }
}

// ============================================================================
// Tests
// ============================================================================

test "Drc basic allocation and cleanup" {
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    try drc.enterScope(1);
    try drc.registerVariable("obj", .object, 1, 5, false);
    try drc.registerAllocation("obj", 1, 10);
    try drc.exitScope(5, 1);
    try drc.finalize();

    const ops = drc.getAllOps();
    try std.testing.expect(ops.len >= 2); // init + decref

    // Check stats
    const stats = drc.getStats();
    try std.testing.expectEqual(@as(u64, 1), stats.variables_analyzed);
    try std.testing.expectEqual(@as(u64, 1), stats.rc_variables);
}

test "Drc value types no RC" {
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    try drc.enterScope(1);
    try drc.registerVariable("n", .value, 1, 5, false);
    try drc.exitScope(5, 1);
    try drc.finalize();

    const ops = drc.getAllOps();
    // Value types should not generate RC ops (except maybe scope markers)
    var rc_ops: usize = 0;
    for (ops) |op| {
        if (op.kind == .incref or op.kind == .decref) rc_ops += 1;
    }
    try std.testing.expectEqual(@as(usize, 0), rc_ops);
}

test "Drc move optimization" {
    var drc = Drc.initWithConfig(std.testing.allocator, .{ .enable_move_optimization = true });
    defer drc.deinit();

    try drc.enterScope(1);
    try drc.registerVariable("x", .object, 1, 5, false);
    try drc.registerAllocation("x", 1, 10);
    try drc.trackUse("x", 3, 1, .read); // Single use = last use
    try drc.exitScope(5, 1);
    try drc.finalize();

    const stats = drc.getStats();
    try std.testing.expect(stats.moves >= 1);
    try std.testing.expect(stats.ops_elided >= 1);
}

test "Drc field store requires incref" {
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    try drc.enterScope(1);
    try drc.registerVariable("x", .object, 1, 5, false);
    try drc.trackUse("x", 2, 1, .read); // First use
    try drc.trackUse("x", 3, 1, .field_store); // Store in field
    try drc.exitScope(5, 1);
    try drc.finalize();

    const stats = drc.getStats();
    try std.testing.expect(stats.increfs >= 1);
}

test "Drc return transfers ownership" {
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    try drc.enterScope(1);
    try drc.registerVariable("result", .object, 1, 5, false);
    try drc.registerAllocation("result", 1, 10);
    try drc.trackUse("result", 3, 1, .returned);
    try drc.exitScope(5, 1);
    try drc.finalize();

    const stats = drc.getStats();
    try std.testing.expect(stats.moves >= 1);

    // Returned value should not be decreffed at scope exit
    var scope_decrefs: usize = 0;
    for (drc.getAllOps()) |op| {
        if (op.kind == .decref and op.reason == .scope_exit) scope_decrefs += 1;
    }
    try std.testing.expectEqual(@as(usize, 0), scope_decrefs);
}

test "Drc parameters are borrowed" {
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    try drc.enterScope(1);
    try drc.registerVariable("param", .object, 1, 5, true); // is_parameter = true
    try drc.exitScope(5, 1);
    try drc.finalize();

    // Parameters should not be decreffed (we don't own them)
    var decrefs: usize = 0;
    for (drc.getAllOps()) |op| {
        if (op.kind == .decref and std.mem.eql(u8, op.target, "param")) decrefs += 1;
    }
    try std.testing.expectEqual(@as(usize, 0), decrefs);
}

test "Drc code generation C" {
    var drc = Drc.initWithConfig(std.testing.allocator, .{ .debug_mode = true });
    defer drc.deinit();

    try drc.enterScope(1);
    try drc.registerVariable("obj", .object, 1, 5, false);
    try drc.trackUse("obj", 2, 1, .field_store);
    try drc.finalize();

    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    for (drc.getAllOps()) |op| {
        try emitCCode(op, buf.writer());
    }

    try std.testing.expect(std.mem.indexOf(u8, buf.items, "ms_incref(obj)") != null);
}

test "Drc statistics" {
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    try drc.enterScope(1);
    try drc.registerVariable("a", .object, 1, 1, false);
    try drc.registerVariable("b", .string, 2, 1, false);
    try drc.registerVariable("c", .value, 3, 1, false);
    try drc.finalize();

    const stats = drc.getStats();
    try std.testing.expectEqual(@as(u64, 3), stats.variables_analyzed);
    try std.testing.expectEqual(@as(u64, 2), stats.rc_variables); // object + string, not value
}
