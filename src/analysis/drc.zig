///! Dynamic Reference Counter (DRC)
///!
///! Unified module that orchestrates the complete DRC pipeline:
///!
///!   1. Ownership Analysis (Lobster-style)
///!   2. RC Annotation (where to insert ops)
///!   3. Cycle Detection (Bacon-Rajan)
///!   4. Backend Integration (via rc_trait)
///!
///! Lifetime System (stolen from Lobster):
///!   - LT_BORROW: Value is borrowed, don't hold onto it
///!   - LT_KEEP: You own this value, must delete or store
///!   - LT_ANY: Non-reference type, lifetime doesn't matter
///!
///! Key optimizations:
///!   - consumes_vars_on_return: Skip decref when returning owned var
///!   - AdjustLifetime: Convert between borrow/keep with incref/decref
///!   - Last-use move: Transfer ownership on last use
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
const cycle_detection = @import("cycle_detection.zig");

// ============================================================================
// Lifetime System (Lobster-style)
// ============================================================================

/// Lifetime annotation for values (stolen from Lobster)
/// Positive values (>= 0) are borrow indices into the borrow stack
pub const Lifetime = enum(i32) {
    /// Value: you are receiving a value stored elsewhere, do not hold on.
    /// Recipient: I do not want to be responsible for managing this value.
    borrow = -1,

    /// Value: you are responsible for this value, you must delete or store.
    /// Recipient: I want to hold on to this value (inc ref, or be sole owner).
    keep = -2,

    /// Value: lifetime shouldn't matter, because type is non-reference.
    /// Recipient: I'm cool with any lifetime.
    any = -3,

    /// Value: there are multiple lifetimes, stored elsewhere.
    multiple = -4,

    /// Lifetime is not valid/undefined.
    undef = -5,

    pub fn isBorrow(self: Lifetime) bool {
        return @intFromEnum(self) >= @intFromEnum(Lifetime.borrow);
    }

    pub fn lifetimeType(self: Lifetime) Lifetime {
        return if (self.isBorrow()) .borrow else self;
    }

    pub fn isRef(self: Lifetime) bool {
        return self != .any and self != .undef;
    }
};

/// Borrow tracking entry (like Lobster's Borrow struct)
pub const BorrowEntry = struct {
    /// The variable being borrowed from
    var_name: []const u8,
    /// Reference count of outstanding borrows
    refc: u32 = 1,
    /// Line where borrow started
    line: u32,
    /// Column where borrow started
    column: u32,

    pub fn init(var_name: []const u8, line: u32, column: u32) BorrowEntry {
        return .{ .var_name = var_name, .refc = 1, .line = line, .column = column };
    }
};

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

/// DRC Diagnostic for LSP integration
pub const DrcDiagnostic = struct {
    line: u32,
    column: u32,
    end_line: u32,
    end_column: u32,
    severity: Severity,
    message: []const u8,
    code: Code,

    pub const Severity = enum {
        @"error",
        warning,
        hint,
    };

    pub const Code = enum {
        use_after_move,
        potential_cycle,
        uninitialized_use,
        double_free_risk,
    };
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

        /// Decref for reassignment - must save old value first, then assign, then decref
        /// This prevents use-after-free when RHS references LHS (e.g., obj = obj.clone())
        decref_reassign,

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

    /// Is this a module-level (global) variable?
    /// Module-level variables have static lifetime and are never freed.
    is_module_level: bool = false,

    /// Has this variable been initialized (assigned a value)?
    initialized: bool = false,

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
///
/// Architecture:
/// - `ownership_analyzer`: Computes semantic ownership (wants/provides) for last-use detection
/// - `cycle_detector`: Tracks potential cycle roots for Bacon-Rajan collection
/// - `variables`: Runtime state tracking (owned→moved→borrowed transitions)
/// - `operations`: Final RC operations for codegen
///
/// Note on ownership concepts:
/// - `ownership.Ownership` = semantic ownership (who should own the value)
/// - `Variable.OwnershipState` = runtime state (has ownership been transferred?)
/// These are complementary: semantic ownership guides decisions, state tracks execution.
pub const Drc = struct {
    allocator: std.mem.Allocator,
    config: Config,

    /// Ownership analyzer - computes semantic ownership for Lobster-style last-use optimization.
    /// Used for: isLastUse(), escapesScope(), computeOwnership()
    ownership_analyzer: ownership.OwnershipAnalyzer,

    /// Cycle detector - tracks potential cycle roots for deferred collection.
    /// Used when decref leaves RC > 0 (potential cycle member).
    cycle_detector: cycle_detection.CycleDetector,

    /// All generated operations (sorted by line)
    operations: std.ArrayList(RcOp),

    /// Operations indexed by line for fast lookup
    ops_by_line: std.AutoHashMap(u32, std.ArrayList(RcOp)),

    /// All tracked variables
    variables: std.StringHashMap(Variable),

    /// Scope stack
    scope_stack: std.ArrayList(Scope),

    /// Borrow stack (Lobster-style) - tracks outstanding borrows
    borrow_stack: std.ArrayList(BorrowEntry),

    /// Statement temporaries - call results that need cleanup after statement
    /// Used for nested calls like f(g(x)) where g(x) returns owned value
    statement_temporaries: std.ArrayList(Temporary),

    /// Current line being processed (for error messages)
    current_line: u32 = 0,

    /// Loop scope stack - tracks scope depths where loops start
    /// Used for break/continue to know which scopes to clean up
    loop_scope_stack: std.ArrayList(u32),

    /// Statistics
    stats: Stats,

    /// Diagnostics for LSP integration (use-after-move, cycle candidates, etc.)
    diagnostics: std.ArrayList(DrcDiagnostic),

    /// Counter for generating unique temporary names (instance-level, not global)
    temp_counter: u32 = 0,

    /// A temporary value from a call expression that needs cleanup
    pub const Temporary = struct {
        /// Synthetic name for the temporary (e.g., "_tmp_0")
        name: []const u8,
        /// Line where the call occurs
        line: u32,
        /// Column where the call occurs
        column: u32,
        /// Whether this temp is consumed (stored, returned, etc.)
        consumed: bool = false,
    };

    pub const Scope = struct {
        depth: u32,
        variables: std.ArrayList([]const u8),
        start_line: u32,
        /// Variables from outer scopes that were shadowed by this scope.
        /// These are restored when this scope exits.
        shadowed_variables: std.StringHashMap(Variable),

        pub fn init(allocator: std.mem.Allocator, depth: u32, start_line: u32) Scope {
            return .{
                .depth = depth,
                .variables = std.ArrayList([]const u8).init(allocator),
                .start_line = start_line,
                .shadowed_variables = std.StringHashMap(Variable).init(allocator),
            };
        }

        pub fn deinit(self: *Scope) void {
            self.variables.deinit();
            self.shadowed_variables.deinit();
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
            .cycle_detector = cycle_detection.CycleDetector.init(allocator),
            .operations = std.ArrayList(RcOp).init(allocator),
            .ops_by_line = std.AutoHashMap(u32, std.ArrayList(RcOp)).init(allocator),
            .variables = std.StringHashMap(Variable).init(allocator),
            .scope_stack = std.ArrayList(Scope).init(allocator),
            .borrow_stack = std.ArrayList(BorrowEntry).init(allocator),
            .statement_temporaries = std.ArrayList(Temporary).init(allocator),
            .loop_scope_stack = std.ArrayList(u32).init(allocator),
            .stats = .{},
            .diagnostics = std.ArrayList(DrcDiagnostic).init(allocator),
        };
    }

    pub fn deinit(self: *Drc) void {
        self.ownership_analyzer.deinit();
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
        self.borrow_stack.deinit();

        // Free temporary names (allocated via allocPrint in registerTemporary)
        for (self.statement_temporaries.items) |temp| {
            self.allocator.free(temp.name);
        }
        self.statement_temporaries.deinit();
        self.loop_scope_stack.deinit();

        // Free diagnostic messages (allocated via allocPrint in emitUseAfterMove/emitPotentialCycle)
        for (self.diagnostics.items) |diag| {
            self.allocator.free(diag.message);
        }
        self.diagnostics.deinit();
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
                    // Only decref if:
                    // - It needs RC management
                    // - We still own it (not moved/borrowed)
                    // - It hasn't escaped
                    // - It's NOT a module-level variable (static lifetime, never freed)
                    if (v.needs_rc and v.ownership_state == .owned and !v.escaped and !v.is_module_level) {
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

        // Restore any variables that were shadowed by this scope.
        // This ensures outer scope variables are visible again after inner scope exits.
        var iter = scope.shadowed_variables.iterator();
        while (iter.next()) |entry| {
            try self.variables.put(entry.key_ptr.*, entry.value_ptr.*);
        }
    }

    // ========================================================================
    // Early Return Cleanup
    // ========================================================================

    /// Emit cleanup for early returns - decref all owned vars except the returned one
    /// This is critical for functions that return early and would otherwise leak
    ///
    /// IMPORTANT: We DON'T check ownership_state here because:
    /// - ownership_state may be .moved due to analysis of a DIFFERENT control-flow path
    /// - At runtime, only ONE return executes, so each return must clean up its own scope
    /// - Example: if (cond) { return x; } return y; -- both returns need independent cleanup
    pub fn emitEarlyReturnCleanup(
        self: *Drc,
        line: u32,
        column: u32,
        returned_var: ?[]const u8,
    ) !void {
        // Iterate through all scopes and emit cleanup for owned variables
        // We need to clean up ALL scopes we're exiting, not just the current one
        for (self.scope_stack.items) |scope| {
            for (scope.variables.items) |var_name| {
                // Skip the returned variable - it's being moved to caller
                if (returned_var) |ret_var| {
                    if (std.mem.eql(u8, var_name, ret_var)) continue;
                }

                if (self.variables.getPtr(var_name)) |vptr| {
                    // Only decref non-parameter variables that need RC
                    // NOTE: We ONLY check is_parameter here, NOT ownership_state or escaped.
                    // - Parameters are borrowed (caller owns them) - never decref
                    // - Local variables are owned and must be cleaned up
                    // - ownership_state might be .moved from a DIFFERENT branch
                    // - escaped might be true from a DIFFERENT branch where this var was returned
                    // Each return statement must independently clean up all vars it doesn't return.
                    if (vptr.needs_rc and !vptr.is_parameter) {
                        try self.addOp(.{
                            .kind = if (self.config.enable_cycle_detection) .decref_cycle_check else .decref,
                            .target = var_name,
                            .line = line,
                            .column = column,
                            .position = .before,
                            .reason = .scope_exit,
                            .comment = if (self.config.debug_mode) "// DRC: early return cleanup" else null,
                        });
                        self.stats.decrefs += 1;

                        // Mark as moved so exitScope doesn't emit duplicate decref
                        // This is safe because return statements fully handle cleanup
                        vptr.ownership_state = .moved;
                    }
                }
            }
        }
    }

    // ========================================================================
    // Loop Control Flow (break/continue)
    // ========================================================================

    /// Enter a loop context - records current scope depth for break/continue cleanup
    pub fn enterLoop(self: *Drc) !void {
        const current_depth: u32 = @intCast(self.scope_stack.items.len);
        try self.loop_scope_stack.append(current_depth);
    }

    /// Exit a loop context
    pub fn exitLoop(self: *Drc) void {
        _ = self.loop_scope_stack.pop();
    }

    /// Emit cleanup for break statement
    /// Cleans up variables from current scope down to (but not including) the loop scope
    pub fn emitBreakCleanup(self: *Drc, line: u32, column: u32) !void {
        if (self.loop_scope_stack.items.len == 0) return; // Not in a loop

        const loop_scope_depth = self.loop_scope_stack.items[self.loop_scope_stack.items.len - 1];
        const current_depth = self.scope_stack.items.len;

        // Clean up scopes from current down to loop scope (exclusive)
        // Iterate in reverse (innermost first)
        var i: usize = current_depth;
        while (i > loop_scope_depth) {
            i -= 1;
            const scope = self.scope_stack.items[i];
            for (scope.variables.items) |var_name| {
                if (self.variables.getPtr(var_name)) |vptr| {
                    // Only decref if we still own it (not already moved/returned)
                    if (vptr.needs_rc and !vptr.is_parameter and vptr.ownership_state == .owned) {
                        try self.addOp(.{
                            .kind = if (self.config.enable_cycle_detection) .decref_cycle_check else .decref,
                            .target = var_name,
                            .line = line,
                            .column = column,
                            .position = .before,
                            .reason = .scope_exit,
                            .comment = if (self.config.debug_mode) "// DRC: break cleanup" else null,
                        });
                        self.stats.decrefs += 1;
                        // Mark as moved to prevent duplicate cleanup at scope exit
                        vptr.ownership_state = .moved;
                    }
                }
            }
        }
    }

    /// Emit cleanup for continue statement
    /// Similar to break but only cleans the current iteration's scope
    pub fn emitContinueCleanup(self: *Drc, line: u32, column: u32) !void {
        if (self.loop_scope_stack.items.len == 0) return; // Not in a loop

        const loop_scope_depth = self.loop_scope_stack.items[self.loop_scope_stack.items.len - 1];
        const current_depth = self.scope_stack.items.len;

        // Clean up scopes from current down to loop scope (exclusive)
        // Same as break - both exit to loop header
        var i: usize = current_depth;
        while (i > loop_scope_depth) {
            i -= 1;
            const scope = self.scope_stack.items[i];
            for (scope.variables.items) |var_name| {
                if (self.variables.getPtr(var_name)) |vptr| {
                    // Only decref if we still own it (not already moved/returned)
                    if (vptr.needs_rc and !vptr.is_parameter and vptr.ownership_state == .owned) {
                        try self.addOp(.{
                            .kind = if (self.config.enable_cycle_detection) .decref_cycle_check else .decref,
                            .target = var_name,
                            .line = line,
                            .column = column,
                            .position = .before,
                            .reason = .scope_exit,
                            .comment = if (self.config.debug_mode) "// DRC: continue cleanup" else null,
                        });
                        self.stats.decrefs += 1;
                        // Mark as moved to prevent duplicate cleanup at scope exit
                        vptr.ownership_state = .moved;
                    }
                }
            }
        }
    }

    // ========================================================================
    // Statement Temporary Management (for nested calls)
    // ========================================================================

    /// Register a temporary from a nested call expression
    /// Returns the temporary name for codegen to use
    pub fn registerTemporary(self: *Drc, line: u32, column: u32) ![]const u8 {
        const name = try std.fmt.allocPrint(self.allocator, "_tmp_{d}", .{self.temp_counter});
        self.temp_counter += 1;

        try self.statement_temporaries.append(.{
            .name = name,
            .line = line,
            .column = column,
            .consumed = false,
        });

        return name;
    }

    /// Mark a temporary as consumed (e.g., assigned to a variable or returned)
    pub fn markTemporaryConsumed(self: *Drc, name: []const u8) void {
        for (self.statement_temporaries.items) |*temp| {
            if (std.mem.eql(u8, temp.name, name)) {
                temp.consumed = true;
                return;
            }
        }
    }

    /// Emit decrefs for all unconsumed temporaries and clear the list
    /// Called at the end of each expression statement
    pub fn flushStatementTemporaries(self: *Drc, line: u32, column: u32) !void {
        for (self.statement_temporaries.items) |temp| {
            if (!temp.consumed) {
                try self.addOp(.{
                    .kind = if (self.config.enable_cycle_detection) .decref_cycle_check else .decref,
                    .target = temp.name,
                    .line = line,
                    .column = column,
                    .position = .after,
                    .reason = .scope_exit,
                    .comment = if (self.config.debug_mode) "// DRC: cleanup nested call temp" else null,
                });
                self.stats.decrefs += 1;
            }
            // Free the allocated temp name (allocated in registerTemporary via allocPrint)
            self.allocator.free(temp.name);
        }
        self.statement_temporaries.clearRetainingCapacity();
    }

    /// Check if there are pending temporaries
    pub fn hasPendingTemporaries(self: *const Drc) bool {
        return self.statement_temporaries.items.len > 0;
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
        const needs_rc = type_kind.needsRc();
        const current_scope_depth: u32 = @intCast(self.scope_stack.items.len);

        // Check if a variable with this name exists in an outer scope.
        // If so, save it to the current scope's shadowed_variables so we can
        // restore it when this scope exits.
        if (self.variables.get(name)) |existing| {
            // Only save if it's from an outer scope (lower depth)
            if (existing.scope_depth < current_scope_depth) {
                if (self.scope_stack.items.len > 0) {
                    const current = &self.scope_stack.items[self.scope_stack.items.len - 1];
                    try current.shadowed_variables.put(name, existing);
                }
            }
        }

        // Module-level variables are at scope depth 1 (the global scope)
        // They have static lifetime and should never be freed
        const is_module_level = (current_scope_depth == 1) and !is_parameter;

        const variable = Variable{
            .name = name,
            .type_kind = type_kind,
            .scope_depth = current_scope_depth,
            .line = line,
            .column = column,
            .needs_rc = needs_rc,
            .is_parameter = is_parameter,
            .is_module_level = is_module_level,
            .initialized = is_parameter, // Parameters are initialized by caller
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

        // Mark variable as initialized
        if (self.variables.getPtr(var_name)) |vptr| {
            vptr.initialized = true;
        }
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

        // NOTE: Use-after-move and uninitialized-use are now handled by the typechecker.
        // DRC is purely an optimization layer - typechecker handles safety diagnostics.
        // With "Shared by Default" + explicit move keyword, there's no uncertain ownership.

        try self.ownership_analyzer.recordUse(var_name, line, column, toOwnershipContext(context));

        // Check for last use optimization
        // IMPORTANT: Only apply move optimization for contexts that actually transfer ownership.
        // Borrowed contexts (read, function_arg_borrowed) don't transfer ownership, so the
        // caller still needs to clean up the value even if this is the last use in source code.
        const can_move = switch (context) {
            .function_arg_owned, .returned, .field_store => true,
            .read, .function_arg_borrowed => false, // Caller still owns it!
            else => false,
        };

        if (self.config.enable_move_optimization and can_move) {
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

                // DIAGNOSTIC: Field stores to object types can form cycles
                // Emit hint so user knows cycle detection is active
                if (v.type_kind == .object and self.config.enable_cycle_detection) {
                    try self.emitPotentialCycle(var_name, line, column);
                }
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
                // Returning transfers ownership to caller
                // (Lobster calls this "consumes_vars_on_return")
                if (self.variables.getPtr(var_name)) |vptr| {
                    if (vptr.ownership_state == .borrowed) {
                        // CRITICAL: Returning a borrowed parameter!
                        // Must incref to give caller ownership, otherwise they get
                        // a dangling reference when our scope ends.
                        //
                        // IMPORTANT: We do NOT change ownership_state here!
                        // The parameter remains borrowed - we're just giving the caller
                        // a new reference. Other branches may also return this same
                        // borrowed parameter and need to emit their own incref.
                        try self.addOp(.{
                            .kind = .incref,
                            .target = var_name,
                            .line = line,
                            .column = column,
                            .position = .before,
                            .reason = .return_value,
                            .comment = if (self.config.debug_mode) "// DRC: incref borrowed return" else null,
                        });
                        self.stats.increfs += 1;
                        vptr.escaped = true;
                        // Note: Do NOT set ownership_state = .moved for borrowed params!
                        // They remain borrowed so other branches can also return them.
                    } else if (vptr.ownership_state == .owned) {
                        // Returning owned value - elide scope cleanup decref
                        // For owned values, we DO mark as moved since we're transferring ownership
                        self.stats.ops_elided += 1;
                        vptr.escaped = true;
                        vptr.ownership_state = .moved;
                    }
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
    // Lifetime System (Lobster-style)
    // ========================================================================

    /// Push a borrow onto the borrow stack (like Lobster's PushBorrow)
    /// Returns a lifetime that can be used to track this borrow
    pub fn pushBorrow(self: *Drc, var_name: []const u8, line: u32, column: u32) !Lifetime {
        // Check if this variable is a reference type
        const v = self.variables.get(var_name) orelse return .any;
        if (!v.needs_rc) return .any;

        // Check if we already have a borrow for this variable
        for (self.borrow_stack.items, 0..) |*entry, i| {
            if (std.mem.eql(u8, entry.var_name, var_name)) {
                entry.refc += 1;
                // Return the borrow index as a positive lifetime value
                return @enumFromInt(@as(i32, @intCast(i)));
            }
        }

        // Create new borrow entry
        const borrow_idx = self.borrow_stack.items.len;
        try self.borrow_stack.append(BorrowEntry.init(var_name, line, column));

        // Return the borrow index as lifetime
        return @enumFromInt(@as(i32, @intCast(borrow_idx)));
    }

    /// Increment borrow count (like Lobster's IncBorrowers)
    pub fn incBorrowers(self: *Drc, lt: Lifetime) void {
        const idx = @intFromEnum(lt);
        if (idx < 0) return; // Not a borrow index
        if (idx >= self.borrow_stack.items.len) return;
        self.borrow_stack.items[@intCast(idx)].refc += 1;
    }

    /// Decrement borrow count (like Lobster's DecBorrowers)
    pub fn decBorrowers(self: *Drc, lt: Lifetime) void {
        const idx = @intFromEnum(lt);
        if (idx < 0) return; // Not a borrow index
        if (idx >= self.borrow_stack.items.len) return;
        const entry = &self.borrow_stack.items[@intCast(idx)];
        if (entry.refc > 0) {
            entry.refc -= 1;
        }
    }

    /// Context for lifetime adjustment - different contexts have different semantics
    pub const AdjustContext = enum {
        /// Function argument passing: caller lends, callee borrows temporarily
        call,
        /// Storage: assigning to variable/field, creates shared ownership
        store,
        /// Return: transferring ownership to caller
        return_value,
    };

    /// Adjust lifetime from given to recipient (like Lobster's AdjustLifetime)
    /// Returns true if an RC operation was emitted
    ///
    /// CRITICAL: The context matters!
    /// - call: KEEP→BORROW = no-op (lending), KEEP→KEEP at last use = move
    /// - store: KEEP→KEEP = incref (sharing), BORROW→KEEP = incref
    /// - return: KEEP→KEEP = move (transfer), BORROW→KEEP = incref
    pub fn adjustLifetime(
        self: *Drc,
        given: Lifetime,
        recipient: Lifetime,
        var_name: []const u8,
        line: u32,
        column: u32,
    ) !bool {
        // Default to call context for backwards compatibility
        return self.adjustLifetimeWithContext(given, recipient, var_name, line, column, .call);
    }

    /// Adjust lifetime with explicit context
    pub fn adjustLifetimeWithContext(
        self: *Drc,
        given: Lifetime,
        recipient: Lifetime,
        var_name: []const u8,
        line: u32,
        column: u32,
        context: AdjustContext,
    ) !bool {
        const given_type = given.lifetimeType();
        const recip_type = recipient.lifetimeType();

        // Check if variable needs RC
        const v = self.variables.get(var_name) orelse return false;
        if (!v.needs_rc) return false;

        // Handle based on context
        switch (context) {
            .call => {
                // Function calls: lending semantics
                if (given_type == .borrow and recip_type == .keep) {
                    // Callee wants ownership of borrowed value: incref
                    try self.emitIncref(var_name, line, column, .function_arg);
                    self.decBorrowers(given);
                    return true;
                }
                // KEEP→BORROW = no-op (lending temporarily)
                // KEEP→KEEP handled by move optimization in analyzer
                return false;
            },
            .store => {
                // Storage: sharing semantics
                if (recip_type == .keep) {
                    // Storing creates a new reference: incref needed
                    // (except for fresh allocations, handled elsewhere)
                    if (given_type == .borrow or given_type == .keep) {
                        try self.emitIncref(var_name, line, column, .assignment);
                        if (given_type == .borrow) self.decBorrowers(given);
                        return true;
                    }
                }
                return false;
            },
            .return_value => {
                // Return: transfer semantics
                if (given_type == .borrow and recip_type == .keep) {
                    // Returning borrowed value: must incref to give caller ownership
                    try self.emitIncref(var_name, line, column, .return_value);
                    self.decBorrowers(given);
                    return true;
                }
                // KEEP→KEEP = move (ownership transfer, no RC change)
                return false;
            },
        }
    }

    /// Helper to emit incref
    fn emitIncref(self: *Drc, var_name: []const u8, line: u32, column: u32, reason: RcOp.Reason) !void {
        try self.addOp(.{
            .kind = .incref,
            .target = var_name,
            .line = line,
            .column = column,
            .position = .before,
            .reason = reason,
            .comment = if (self.config.debug_mode) "// DRC: incref" else null,
        });
        self.stats.increfs += 1;
    }

    /// Get the lifetime for a variable based on its current state
    pub fn getVariableLifetime(self: *Drc, var_name: []const u8) Lifetime {
        const v = self.variables.get(var_name) orelse return .undef;

        if (!v.needs_rc) return .any;

        return switch (v.ownership_state) {
            .owned => .keep,
            .borrowed => .borrow,
            .moved => .any, // Already moved, no longer matters
            .unknown => .undef,
        };
    }

    /// Infer function argument lifetime (like Lobster's ArgLifetime)
    /// Returns LT_KEEP if arg takes ownership, LT_BORROW if it borrows
    pub fn inferArgLifetime(self: *Drc, var_name: []const u8, is_single_assignment: bool) Lifetime {
        const v = self.variables.get(var_name) orelse return .any;

        if (!v.needs_rc) return .any;

        // Parameters that are reassigned must be LT_KEEP
        if (!is_single_assignment) return .keep;

        // Default: borrow (caller retains ownership)
        return .borrow;
    }

    // ========================================================================
    // Variable Copy Handling
    // ========================================================================

    /// Track a variable-to-variable copy: let target = source
    /// This creates a shared reference - we incref the value after assignment
    ///
    /// The incref is emitted for target_var because AFTER the assignment,
    /// target_var points to the same object as source_var. Incrementing
    /// target_var's refcount is equivalent to incrementing source_var's.
    pub fn trackVariableCopy(
        self: *Drc,
        target_var: []const u8,
        _: []const u8, // source_var - use-after-move is handled by typechecker
        line: u32,
        column: u32,
    ) !void {
        // Check if target is a reference type that needs RC
        const target = self.variables.get(target_var) orelse return;
        if (!target.needs_rc) return;

        // NOTE: Use-after-move is now handled by the typechecker.
        // DRC is purely an optimization layer - typechecker handles safety diagnostics.

        // Emit incref for the target variable after the assignment
        // After `target = source`, target and source point to same object
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
    /// IMPORTANT: Only decref if we still own the old value
    /// If the variable was moved (e.g., returned in a different branch),
    /// we must NOT decref it again.
    pub fn trackReassignment(
        self: *Drc,
        var_name: []const u8,
        line: u32,
        column: u32,
    ) !void {
        if (self.variables.getPtr(var_name)) |vptr| {
            if (!vptr.needs_rc) return;

            // NOTE: Reassignment after move is valid in the "Shared by Default" + move model.
            // Typechecker handles this - we just skip the decref (no old value to free).
            // No diagnostic needed here anymore.

            // Only decref if we still own the old value
            // If it was moved (returned/passed to ownership-taking function),
            // we don't own it anymore and must not decref
            if (vptr.ownership_state == .owned) {
                // Use decref_reassign to handle self-referential assignments like:
                //   obj = obj.clone()  -- RHS uses LHS, must evaluate RHS before decref
                // The cgen will: save old ptr → emit assignment → decref saved ptr
                try self.addOp(.{
                    .kind = .decref_reassign,
                    .target = var_name,
                    .line = line,
                    .column = column,
                    .position = .before,
                    .reason = .reassignment,
                    .comment = if (self.config.debug_mode) "// DRC: decref old value (reassign)" else null,
                });
                self.stats.decrefs += 1;
            }

            // After reassignment, we own the new value
            vptr.ownership_state = .owned;
            vptr.initialized = true; // Reassignment initializes
        }
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
        return result.toOwnedSlice() catch {
            result.deinit(); // Free ArrayList buffer on error
            return &[_]RcOp{};
        };
    }

    /// Get operations after a specific position
    pub fn getOpsAfter(self: *Drc, line: u32) []const RcOp {
        var result = std.ArrayList(RcOp).init(self.allocator);
        for (self.getOpsForLine(line)) |op| {
            if (op.position == .after) {
                result.append(op) catch {};
            }
        }
        return result.toOwnedSlice() catch {
            result.deinit(); // Free ArrayList buffer on error
            return &[_]RcOp{};
        };
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

    /// Get diagnostics for LSP integration
    pub fn getDiagnostics(self: *const Drc) []const DrcDiagnostic {
        return self.diagnostics.items;
    }

    /// Emit a diagnostic (for use by DrcAnalyzer and internal methods)
    pub fn emitDiagnostic(self: *Drc, diag: DrcDiagnostic) !void {
        try self.diagnostics.append(diag);
    }

    /// Emit a use-after-move warning with rich explanation
    pub fn emitUseAfterMove(self: *Drc, var_name: []const u8, line: u32, column: u32) !void {
        const msg = try std.fmt.allocPrint(
            self.allocator,
            \\Use of moved value '{s}'
            \\
            \\The value was transferred to another owner (returned or passed to a function).
            \\After a move, the original variable is no longer valid.
            \\
            \\Options:
            \\  • Clone before move: use `{s}.clone()` if you need the value twice
            \\  • Restructure: use '{s}' before the move happens
            \\  • Change ownership: make the function borrow instead of take ownership
        ,
            .{ var_name, var_name, var_name },
        );
        try self.emitDiagnostic(.{
            .line = line,
            .column = column,
            .end_line = line,
            .end_column = column + @as(u32, @intCast(var_name.len)),
            .severity = .warning,
            .message = msg,
            .code = .use_after_move,
        });
    }

    /// Emit a potential cycle hint with explanation
    pub fn emitPotentialCycle(self: *Drc, var_name: []const u8, line: u32, column: u32) !void {
        const msg = try std.fmt.allocPrint(
            self.allocator,
            \\Potential reference cycle involving '{s}'
            \\
            \\Storing an object in a field can create cycles (A→B→A).
            \\Cycles prevent automatic cleanup via reference counting.
            \\
            \\Options:
            \\  • Use WeakRef for back-references: `parent: WeakRef<Node>`
            \\  • Break cycle manually: set reference to null when done
            \\  • Accept cycle: cycle collector will handle it (slight perf cost)
        ,
            .{var_name},
        );
        try self.emitDiagnostic(.{
            .line = line,
            .column = column,
            .end_line = line,
            .end_column = column + @as(u32, @intCast(var_name.len)),
            .severity = .hint,
            .message = msg,
            .code = .potential_cycle,
        });
    }

    /// Emit an uninitialized use warning with explanation
    pub fn emitUninitializedUse(self: *Drc, var_name: []const u8, line: u32, column: u32) !void {
        const msg = try std.fmt.allocPrint(
            self.allocator,
            \\Use of possibly uninitialized variable '{s}'
            \\
            \\This variable was declared but may not have been assigned a value
            \\on all code paths before this use.
            \\
            \\Options:
            \\  • Initialize at declaration: `let {s} = initialValue;`
            \\  • Ensure all branches assign: check if/else paths
            \\  • Use optional type: `let {s}: Type? = null;` if absence is valid
        ,
            .{ var_name, var_name, var_name },
        );
        try self.emitDiagnostic(.{
            .line = line,
            .column = column,
            .end_line = line,
            .end_column = column + @as(u32, @intCast(var_name.len)),
            .severity = .warning,
            .message = msg,
            .code = .uninitialized_use,
        });
    }

    /// Emit a double-free risk warning with explanation
    pub fn emitDoubleFreeRisk(self: *Drc, var_name: []const u8, line: u32, column: u32) !void {
        const msg = try std.fmt.allocPrint(
            self.allocator,
            \\Reassigning previously moved variable '{s}'
            \\
            \\This variable was moved earlier, so its old value is gone.
            \\Reassigning is safe, but this pattern can indicate confusion
            \\about ownership flow.
            \\
            \\This is usually fine if:
            \\  • You're intentionally reusing the variable name
            \\  • The move was conditional and you're on the non-moved path
            \\
            \\Review if:
            \\  • You expected the old value to still exist
            \\  • The move was unintentional
        ,
            .{var_name},
        );
        try self.emitDiagnostic(.{
            .line = line,
            .column = column,
            .end_line = line,
            .end_column = column + @as(u32, @intCast(var_name.len)),
            .severity = .hint, // Downgrade to hint - this is usually safe
            .message = msg,
            .code = .double_free_risk,
        });
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
        // Debug: print each op as it's added
        if (self.config.debug_mode) {
            std.debug.print("[DRC] Adding op: {s} {s} at line {d} reason={s}\n", .{
                @tagName(op.kind),
                op.target,
                op.line,
                @tagName(op.reason),
            });
        }

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
        .decref_reassign => {
            // Reassignment decref: save old, assign new, decref old
            // This is handled specially by cgen, here we just show the intent
            if (op.comment) |c| try writer.print("    {s}\n", .{c});
            try writer.print("    void* _old_{s} = {s}; /* save old */\n", .{ op.target, op.target });
            try writer.print("    /* assignment happens here */\n", .{});
            try writer.print("    ms_decref(_old_{s}); /* decref old */\n", .{op.target});
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
        .decref_reassign => {
            // Reassignment decref: save old, assign new, decref old
            // This prevents use-after-free when RHS references LHS
            if (op.comment) |c| try writer.print("    {s}\n", .{c});
            try writer.print("    const _old_{s} = {s}; // save old\n", .{ op.target, op.target });
            try writer.print("    // assignment happens here\n", .{});
            try writer.print("    if (@atomicSub(&_old_{s}.rc, 1, .seq_cst) == 1) _old_{s}.deinit(); // decref old\n", .{ op.target, op.target });
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

test "Drc move optimization requires ownership-transfer context" {
    // Move optimization only applies to ownership-transfer contexts (return, field_store, function_arg_owned)
    // NOT to read-only contexts (read, function_arg_borrowed)
    var drc = Drc.initWithConfig(std.testing.allocator, .{ .enable_move_optimization = true });
    defer drc.deinit();

    try drc.enterScope(1);
    try drc.registerVariable("x", .object, 1, 5, false);
    try drc.registerAllocation("x", 1, 10);
    try drc.trackUse("x", 3, 1, .returned); // Last use with ownership transfer
    try drc.exitScope(5, 1);
    try drc.finalize();

    const stats = drc.getStats();
    try std.testing.expect(stats.moves >= 1);
    // Note: returned values don't go through the last-use path in trackUse,
    // they're handled specially. Check that no scope cleanup decref was emitted.
    var scope_decrefs: usize = 0;
    for (drc.getAllOps()) |op| {
        if ((op.kind == .decref or op.kind == .decref_cycle_check) and op.reason == .scope_exit) {
            scope_decrefs += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 0), scope_decrefs);
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

// ============================================================================
// Tests for Bug Fixes (Dec 5, 2025)
// ============================================================================

test "Drc borrowed args do NOT trigger move optimization" {
    // Bug fix: function_arg_borrowed should NOT use move optimization
    // even if it's the last use, because the caller still owns the value
    var drc = Drc.initWithConfig(std.testing.allocator, .{ .enable_move_optimization = true });
    defer drc.deinit();

    // Module scope (global) - required to prevent variables being marked as module-level
    try drc.enterScope(0);
    // Function scope (local)
    try drc.enterScope(1);

    // Create a heap-allocated variable
    try drc.registerVariable("result", .string, 2, 5, false);
    try drc.registerAllocation("result", 2, 10);

    // Pass it to a function as borrowed (this is the ONLY use)
    try drc.trackUse("result", 3, 1, .function_arg_borrowed);

    try drc.exitScope(5, 1);
    try drc.exitScope(6, 1); // Exit module scope
    try drc.finalize();

    // Should NOT have a move op (borrowed doesn't transfer ownership)
    var move_count: usize = 0;
    for (drc.getAllOps()) |op| {
        if (op.kind == .move and std.mem.eql(u8, op.target, "result")) {
            move_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 0), move_count);

    // Should have a decref at scope exit (caller must clean up)
    var scope_decref_count: usize = 0;
    for (drc.getAllOps()) |op| {
        if ((op.kind == .decref or op.kind == .decref_cycle_check) and
            op.reason == .scope_exit and
            std.mem.eql(u8, op.target, "result"))
        {
            scope_decref_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 1), scope_decref_count);
}

test "Drc read context does NOT trigger move optimization" {
    // Similar to borrowed args: read-only uses don't transfer ownership
    var drc = Drc.initWithConfig(std.testing.allocator, .{ .enable_move_optimization = true });
    defer drc.deinit();

    // Module scope (global) - required to prevent variables being marked as module-level
    try drc.enterScope(0);
    // Function scope (local)
    try drc.enterScope(1);
    try drc.registerVariable("obj", .object, 2, 5, false);
    try drc.registerAllocation("obj", 2, 10);

    // Just read the variable (this is the ONLY use)
    try drc.trackUse("obj", 3, 1, .read);

    try drc.exitScope(5, 1);
    try drc.exitScope(6, 1); // Exit module scope
    try drc.finalize();

    // Should NOT have a move op
    var move_count: usize = 0;
    for (drc.getAllOps()) |op| {
        if (op.kind == .move and std.mem.eql(u8, op.target, "obj")) {
            move_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 0), move_count);

    // Should have scope cleanup decref
    var scope_decref_count: usize = 0;
    for (drc.getAllOps()) |op| {
        if ((op.kind == .decref or op.kind == .decref_cycle_check) and
            op.reason == .scope_exit and
            std.mem.eql(u8, op.target, "obj"))
        {
            scope_decref_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 1), scope_decref_count);
}

test "Drc ownership-transfer contexts CAN use move optimization" {
    // Contexts that transfer ownership (return, field_store, function_arg_owned)
    // should use move optimization when it's the last use
    var drc = Drc.initWithConfig(std.testing.allocator, .{ .enable_move_optimization = true });
    defer drc.deinit();

    try drc.enterScope(1);
    try drc.registerVariable("obj", .object, 2, 5, false);
    try drc.registerAllocation("obj", 2, 10);

    // Return the variable (ownership transfer, last use)
    try drc.trackUse("obj", 3, 1, .returned);

    try drc.exitScope(5, 1);
    try drc.finalize();

    // Should have a move op (ownership transferred to caller)
    var move_count: usize = 0;
    for (drc.getAllOps()) |op| {
        if (op.kind == .move and std.mem.eql(u8, op.target, "obj")) {
            move_count += 1;
        }
    }
    try std.testing.expect(move_count >= 1);

    // Should NOT have scope cleanup decref (ownership was transferred)
    var scope_decref_count: usize = 0;
    for (drc.getAllOps()) |op| {
        if ((op.kind == .decref or op.kind == .decref_cycle_check) and
            op.reason == .scope_exit and
            std.mem.eql(u8, op.target, "obj"))
        {
            scope_decref_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 0), scope_decref_count);
}

test "Drc value type variables skip all RC ops" {
    // Value types (integers, booleans, etc.) should never generate RC ops
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    try drc.enterScope(1);
    try drc.registerVariable("count", .value, 2, 5, false);
    try drc.trackUse("count", 3, 1, .read);
    try drc.trackUse("count", 4, 1, .returned);
    try drc.exitScope(5, 1);
    try drc.finalize();

    // Should have NO RC operations for value type
    var rc_ops: usize = 0;
    for (drc.getAllOps()) |op| {
        if (std.mem.eql(u8, op.target, "count")) {
            rc_ops += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 0), rc_ops);
}

test "Drc scope cleanup only for owned variables" {
    // Variables that are borrowed (parameters) or moved should not get scope cleanup
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    // Module scope (global) - required to prevent variables being marked as module-level
    try drc.enterScope(0);
    // Function scope (local)
    try drc.enterScope(1);

    // Borrowed parameter - should not be decreffed
    try drc.registerVariable("param", .object, 1, 5, true);

    // Owned local - should be decreffed
    try drc.registerVariable("local", .object, 2, 5, false);
    try drc.registerAllocation("local", 2, 10);

    try drc.exitScope(5, 1);
    try drc.exitScope(6, 1); // Exit module scope
    try drc.finalize();

    // Count scope cleanup decrefs per variable
    var param_decrefs: usize = 0;
    var local_decrefs: usize = 0;
    for (drc.getAllOps()) |op| {
        if ((op.kind == .decref or op.kind == .decref_cycle_check) and op.reason == .scope_exit) {
            if (std.mem.eql(u8, op.target, "param")) param_decrefs += 1;
            if (std.mem.eql(u8, op.target, "local")) local_decrefs += 1;
        }
    }

    try std.testing.expectEqual(@as(usize, 0), param_decrefs); // Borrowed, not decreffed
    try std.testing.expectEqual(@as(usize, 1), local_decrefs); // Owned, decreffed
}

test "Drc multiple variables in scope cleanup" {
    // Multiple heap-allocated variables should all get cleanup
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    // Module scope (global) - required to prevent variables being marked as module-level
    try drc.enterScope(0);
    // Function scope (local)
    try drc.enterScope(1);
    try drc.registerVariable("a", .string, 2, 5, false);
    try drc.registerAllocation("a", 2, 10);
    try drc.registerVariable("b", .string, 3, 5, false);
    try drc.registerAllocation("b", 3, 10);
    try drc.registerVariable("c", .object, 4, 5, false);
    try drc.registerAllocation("c", 4, 10);
    try drc.exitScope(10, 1);
    try drc.exitScope(11, 1); // Exit module scope
    try drc.finalize();

    // Count scope cleanup decrefs
    var cleanup_count: usize = 0;
    var has_cleanup_start = false;
    var has_cleanup_end = false;
    for (drc.getAllOps()) |op| {
        if (op.kind == .scope_cleanup_start) has_cleanup_start = true;
        if (op.kind == .scope_cleanup_end) has_cleanup_end = true;
        if ((op.kind == .decref or op.kind == .decref_cycle_check) and op.reason == .scope_exit) {
            cleanup_count += 1;
        }
    }

    try std.testing.expect(has_cleanup_start);
    try std.testing.expect(has_cleanup_end);
    try std.testing.expectEqual(@as(usize, 3), cleanup_count); // a, b, c all decreffed
}

test "Drc nested scopes cleanup correctly" {
    // Variables should be cleaned up at their own scope exit, not outer scope
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    // Module scope (global) - required to prevent variables being marked as module-level
    try drc.enterScope(0);

    // Outer scope (function level)
    try drc.enterScope(1);
    try drc.registerVariable("outer", .object, 2, 5, false);
    try drc.registerAllocation("outer", 2, 10);

    // Inner scope (block level)
    try drc.enterScope(3);
    try drc.registerVariable("inner", .object, 4, 5, false);
    try drc.registerAllocation("inner", 4, 10);
    try drc.exitScope(6, 1); // Inner scope exits at line 6

    try drc.exitScope(10, 1); // Outer scope exits at line 10
    try drc.exitScope(11, 1); // Module scope exits
    try drc.finalize();

    // Check that inner is cleaned up at line 6, outer at line 10
    var inner_cleanup_line: ?u32 = null;
    var outer_cleanup_line: ?u32 = null;
    for (drc.getAllOps()) |op| {
        if ((op.kind == .decref or op.kind == .decref_cycle_check) and op.reason == .scope_exit) {
            if (std.mem.eql(u8, op.target, "inner")) inner_cleanup_line = op.line;
            if (std.mem.eql(u8, op.target, "outer")) outer_cleanup_line = op.line;
        }
    }

    try std.testing.expectEqual(@as(?u32, 6), inner_cleanup_line);
    try std.testing.expectEqual(@as(?u32, 10), outer_cleanup_line);
}

test "Drc variable copy generates incref" {
    // let x = y where y is reference type should incref x
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    try drc.enterScope(1);
    try drc.registerVariable("original", .object, 2, 5, false);
    try drc.registerAllocation("original", 2, 10);

    try drc.registerVariable("copy", .object, 3, 5, false);
    try drc.trackVariableCopy("copy", "original", 3, 5);

    try drc.exitScope(5, 1);
    try drc.finalize();

    // Should have incref for copy
    var incref_count: usize = 0;
    for (drc.getAllOps()) |op| {
        if (op.kind == .incref and std.mem.eql(u8, op.target, "copy")) {
            incref_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 1), incref_count);
}

test "Drc elision rate calculation" {
    var drc = Drc.initWithConfig(std.testing.allocator, .{ .enable_move_optimization = true });
    defer drc.deinit();

    try drc.enterScope(1);
    try drc.registerVariable("obj", .object, 2, 5, false);
    try drc.registerAllocation("obj", 2, 10);
    try drc.trackUse("obj", 3, 1, .returned); // Last use with ownership transfer, will be moved
    try drc.exitScope(5, 1);
    try drc.finalize();

    const stats = drc.getStats();
    // Move optimization should elide the scope cleanup decref (ownership transferred to caller)
    // The move op itself counts as an "elided" decref since we skip the scope cleanup
    try std.testing.expect(stats.moves >= 1);
    // elisionRate depends on implementation details, just check it's calculable
    _ = stats.elisionRate();
}

test "Drc variable shadowing restores outer variable" {
    // Test that shadowing a variable in inner scope doesn't corrupt outer scope
    // Bug: HashMap.put(name) would overwrite outer scope's variable info
    // After inner scope exits, outer variable should still be tracked correctly
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    // Module scope (global) - required to prevent variables being marked as module-level
    try drc.enterScope(0);

    // Outer scope (function level)
    try drc.enterScope(1);
    try drc.registerVariable("x", .object, 2, 5, false);
    try drc.registerAllocation("x", 2, 10);

    // Verify outer x exists with scope_depth 2 (module + function)
    const outer_x = drc.variables.get("x").?;
    try std.testing.expectEqual(@as(u32, 2), outer_x.scope_depth);
    try std.testing.expectEqual(Variable.OwnershipState.owned, outer_x.ownership_state);

    // Inner scope with shadowing
    try drc.enterScope(3);
    try drc.registerVariable("x", .object, 4, 5, false); // Shadows outer x
    try drc.registerAllocation("x", 4, 10);

    // Verify inner x has scope_depth 3 (module + function + block)
    const inner_x = drc.variables.get("x").?;
    try std.testing.expectEqual(@as(u32, 3), inner_x.scope_depth);

    // Exit inner scope - should restore outer x
    try drc.exitScope(6, 1);

    // Verify outer x is restored
    const restored_x = drc.variables.get("x").?;
    try std.testing.expectEqual(@as(u32, 2), restored_x.scope_depth);
    try std.testing.expectEqual(Variable.OwnershipState.owned, restored_x.ownership_state);

    // Exit outer scope (function)
    try drc.exitScope(10, 1);
    // Exit module scope
    try drc.exitScope(11, 1);
    try drc.finalize();

    // Count decrefs for "x" - should have 2 (one for each scope exit)
    var decref_count: usize = 0;
    for (drc.getAllOps()) |op| {
        if ((op.kind == .decref or op.kind == .decref_cycle_check) and
            std.mem.eql(u8, op.target, "x"))
        {
            decref_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 2), decref_count);
}

// ============================================================================
// Diagnostic Tests
// NOTE: Use-after-move, uninitialized-use, and double-free-risk are now
// handled by the typechecker. DRC is purely an optimization layer.
// Only potential_cycle hints remain in DRC.
// ============================================================================

test "Drc does not emit use-after-move diagnostic (handled by typechecker)" {
    var drc = Drc.initWithConfig(std.testing.allocator, .{ .enable_move_optimization = true });
    defer drc.deinit();

    try drc.enterScope(0); // Module
    try drc.enterScope(1); // Function

    try drc.registerVariable("obj", .object, 2, 5, false);
    try drc.registerAllocation("obj", 2, 10);

    // Move the variable (ownership transfer context)
    try drc.trackUse("obj", 3, 1, .returned);

    // Use after move - DRC no longer emits diagnostic (typechecker handles it)
    try drc.trackUse("obj", 4, 1, .read);

    try drc.exitScope(5, 1);
    try drc.exitScope(6, 1);
    try drc.finalize();

    // No diagnostics from DRC - typechecker handles safety
    const diags = drc.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 0), diags.len);
}

test "Drc does not emit uninitialized-use diagnostic (handled by typechecker)" {
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    try drc.enterScope(0); // Module
    try drc.enterScope(1); // Function

    // Register variable but DON'T allocate/initialize it
    try drc.registerVariable("uninit", .object, 2, 5, false);

    // Use uninitialized - DRC no longer emits diagnostic (typechecker handles it)
    try drc.trackUse("uninit", 3, 1, .read);

    try drc.exitScope(5, 1);
    try drc.exitScope(6, 1);
    try drc.finalize();

    // No diagnostics from DRC - typechecker handles safety
    const diags = drc.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 0), diags.len);
}

test "Drc emits potential-cycle hint for field stores" {
    var drc = Drc.initWithConfig(std.testing.allocator, .{ .enable_cycle_detection = true });
    defer drc.deinit();

    try drc.enterScope(0); // Module
    try drc.enterScope(1); // Function

    try drc.registerVariable("node", .object, 2, 5, false);
    try drc.registerAllocation("node", 2, 10);

    // Store in field (potential cycle if self-referential)
    try drc.trackUse("node", 3, 1, .field_store);

    try drc.exitScope(5, 1);
    try drc.exitScope(6, 1);
    try drc.finalize();

    const diags = drc.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    try std.testing.expectEqual(DrcDiagnostic.Code.potential_cycle, diags[0].code);
    try std.testing.expectEqual(DrcDiagnostic.Severity.hint, diags[0].severity);
}

test "Drc does not emit double-free-risk for reassigning moved variable (valid operation)" {
    var drc = Drc.initWithConfig(std.testing.allocator, .{ .enable_move_optimization = true });
    defer drc.deinit();

    try drc.enterScope(0); // Module
    try drc.enterScope(1); // Function

    try drc.registerVariable("x", .object, 2, 5, false);
    try drc.registerAllocation("x", 2, 10);

    // Move the variable
    try drc.trackUse("x", 3, 1, .returned);

    // Reassign - this is valid in "Shared by Default" + move model
    // Typechecker clears moved state on reassignment
    try drc.trackReassignment("x", 4, 1);

    try drc.exitScope(5, 1);
    try drc.exitScope(6, 1);
    try drc.finalize();

    // No diagnostics - reassignment after move is valid
    const diags = drc.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 0), diags.len);
}

test "Drc parameters don't emit uninitialized-use" {
    var drc = Drc.init(std.testing.allocator);
    defer drc.deinit();

    try drc.enterScope(0); // Module
    try drc.enterScope(1); // Function

    // Parameters are initialized by caller
    try drc.registerVariable("param", .object, 1, 5, true); // is_parameter = true

    // Use without explicit allocation - should NOT emit diagnostic
    try drc.trackUse("param", 3, 1, .read);

    try drc.exitScope(5, 1);
    try drc.exitScope(6, 1);
    try drc.finalize();

    const diags = drc.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 0), diags.len); // No diagnostics
}

// ============================================================================
// Error Path Tests
// ============================================================================

test "Drc deinit cleans up temporary names on success" {
    // Verify that temporary names allocated via registerTemporary are freed
    var drc = Drc.init(std.testing.allocator);

    try drc.enterScope(1);
    _ = try drc.registerTemporary(1, 1);
    _ = try drc.registerTemporary(2, 1);
    _ = try drc.registerTemporary(3, 1);
    try drc.exitScope(5, 1);

    // deinit should free all temporary names without leaking
    drc.deinit();

    // If we get here without the testing allocator complaining, no leaks!
}

test "Drc deinit cleans up diagnostics on success" {
    // Verify that diagnostic messages are freed (using potential_cycle hint)
    var drc = Drc.initWithConfig(std.testing.allocator, .{
        .enable_move_optimization = true,
        .enable_cycle_detection = true,
    });

    try drc.enterScope(1);
    try drc.registerVariable("node", .object, 1, 5, false);
    try drc.registerAllocation("node", 1, 10);

    // Field store triggers potential_cycle hint (the only diagnostic still emitted by DRC)
    try drc.trackUse("node", 2, 1, .field_store);

    try drc.exitScope(5, 1);
    try drc.finalize();

    // Verify we got a diagnostic (potential_cycle hint)
    const diags = drc.getDiagnostics();
    try std.testing.expect(diags.len >= 1);

    // deinit should free diagnostic messages without leaking
    drc.deinit();
}

test "Drc multiple scopes cleanup correctly" {
    // Verify nested scopes are all cleaned up without leaking
    var drc = Drc.init(std.testing.allocator);

    try drc.enterScope(1);
    try drc.registerVariable("a", .object, 1, 1, false);
    _ = try drc.registerTemporary(1, 5);

    try drc.enterScope(2);
    try drc.registerVariable("b", .string, 2, 1, false);
    _ = try drc.registerTemporary(2, 5);

    try drc.enterScope(3);
    try drc.registerVariable("c", .array, 3, 1, false);
    _ = try drc.registerTemporary(3, 5);

    // Exit all scopes
    try drc.exitScope(4, 1);
    try drc.exitScope(5, 1);
    try drc.exitScope(6, 1);

    try drc.finalize();

    // deinit should free everything without leaking
    drc.deinit();
    // std.testing.allocator will panic if anything leaked
}
