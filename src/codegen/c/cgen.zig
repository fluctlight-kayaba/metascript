/// C Code Generator - Main Module
/// Direct emission from Typed AST to C (C99+)
///
/// Generates native C code with structs, functions, and basic runtime support.
/// Type mappings:
///   int8/16/32/64 → int8_t/int16_t/int32_t/int64_t
///   uint8/16/32/64 → uint8_t/uint16_t/uint32_t/uint64_t
///   float32 → float
///   float64 → double
///   number → double (default)
///   string → char*
///   boolean → bool
///
/// Module Structure:
///   cgen.zig        - Main generator, orchestration
///   emit.zig        - Low-level output helpers
///   types.zig       - Type mapping helpers
///   declarations.zig - Struct/function declarations
///
/// Usage:
///   var gen = CGenerator.init(allocator);
///   defer gen.deinit();
///   const c_code = try gen.generate(typed_ast);

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const types_mod = @import("../../ast/types.zig");
const node_mod = @import("../../ast/node.zig");

// DRC (Deferred Reference Counting) analysis - REQUIRED for C backend
// DRC provides ownership analysis and RC operation placement
const drc_mod = @import("../../analysis/drc.zig");
const Drc = drc_mod.Drc;
const RcOp = drc_mod.RcOp;

pub const CGenerator = struct {
    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),
    indent_level: usize,
    /// DRC analysis results - REQUIRED for C backend
    /// Provides ownership analysis and RC operation placement
    drc: *Drc,
    /// Current line being generated (for DRC queries)
    current_line: u32,

    // ========================================================================
    // String Interning - Compile-time literal deduplication
    // ========================================================================
    /// Map of string literal -> intern ID for deduplication
    string_literals: std.StringHashMap(u32),
    /// Next available intern ID
    next_intern_id: u32,
    /// Enable string interning optimization (default: true)
    enable_interning: bool,
    /// Literals that are ONLY used in StringBuilder appends (can skip from pool)
    stringbuilder_only_literals: std.StringHashMap(void),

    // ========================================================================
    // StringBuilder Optimization - Concat-in-loop detection
    // ========================================================================
    /// Variables that should use StringBuilder (detected via concat-in-loop pattern)
    /// Scoped per-function - cleared at function entry
    stringbuilder_vars: std.StringHashMap(void),
    /// StringBuilder variables that have been "finalized" (built to msString*)
    /// After finalization, identifier emission uses the plain name instead of ms_sb_build()
    stringbuilder_finalized: std.StringHashMap(void),
    /// Enable StringBuilder optimization (default: true)
    enable_stringbuilder: bool,
    /// Current function name (for scoping StringBuilder vars)
    current_function: ?[]const u8,

    // ========================================================================
    // Nested Call Temporary Tracking - for f(g(x)) pattern
    // ========================================================================
    /// Counter for generating unique temporary names
    nested_call_temp_counter: u32,
    /// Pending temporaries that need decref after current statement
    pending_temps: std.ArrayList([]const u8),
    /// Map of hoisted call nodes to their temporary names
    /// Used during expression emission to substitute temps for nested calls
    hoisted_calls: std.AutoHashMap(*ast.Node, []const u8),

    const Self = @This();

    /// Initialize C generator with DRC analysis results (required)
    pub fn init(allocator: std.mem.Allocator, drc: *Drc) Self {
        return .{
            .allocator = allocator,
            .output = std.ArrayList(u8).init(allocator),
            .indent_level = 0,
            .drc = drc,
            .current_line = 0,
            .string_literals = std.StringHashMap(u32).init(allocator),
            .next_intern_id = 0,
            .enable_interning = true,
            .stringbuilder_only_literals = std.StringHashMap(void).init(allocator),
            // StringBuilder optimization
            .stringbuilder_vars = std.StringHashMap(void).init(allocator),
            .stringbuilder_finalized = std.StringHashMap(void).init(allocator),
            .enable_stringbuilder = true,
            .current_function = null,
            // Nested call temporaries
            .nested_call_temp_counter = 0,
            .pending_temps = std.ArrayList([]const u8).init(allocator),
            .hoisted_calls = std.AutoHashMap(*ast.Node, []const u8).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        // Free duplicated string keys from the intern pool
        var key_iter = self.string_literals.keyIterator();
        while (key_iter.next()) |key_ptr| {
            self.allocator.free(key_ptr.*);
        }
        self.string_literals.deinit();
        // Free StringBuilder-only literal keys
        var sb_lit_iter = self.stringbuilder_only_literals.keyIterator();
        while (sb_lit_iter.next()) |key_ptr| {
            self.allocator.free(key_ptr.*);
        }
        self.stringbuilder_only_literals.deinit();
        // Free StringBuilder candidate keys
        var sb_key_iter = self.stringbuilder_vars.keyIterator();
        while (sb_key_iter.next()) |key_ptr| {
            self.allocator.free(key_ptr.*);
        }
        self.stringbuilder_vars.deinit();
        // Free StringBuilder finalized keys
        var sb_fin_iter = self.stringbuilder_finalized.keyIterator();
        while (sb_fin_iter.next()) |key_ptr| {
            self.allocator.free(key_ptr.*);
        }
        self.stringbuilder_finalized.deinit();
        // Free nested call temporary names
        for (self.pending_temps.items) |temp_name| {
            self.allocator.free(temp_name);
        }
        self.pending_temps.deinit();
        self.hoisted_calls.deinit();
        self.output.deinit();
    }

    // ========================================================================
    // DRC Integration - Emit RC operations from analysis
    // ========================================================================

    /// Emit RC operations from DRC analysis for a specific line
    /// Call this before emitting the statement at `line` (for .before ops)
    /// or after emitting the statement (for .after ops)
    fn emitDrcOpsForLine(self: *Self, line: u32, position: RcOp.Position) !void {
        try self.emitDrcOpsForLineFiltered(line, position, false);
    }

    /// Emit RC operations with optional scope_exit filter
    /// When skip_scope_exit is true, scope cleanup ops are skipped (for per-statement emission)
    fn emitDrcOpsForLineFiltered(self: *Self, line: u32, position: RcOp.Position, skip_scope_exit: bool) !void {
        const ops = self.drc.getOpsForLine(line);
        for (ops) |op| {
            if (op.position == position) {
                // Skip scope_exit ops if requested (they'll be emitted at block end)
                if (skip_scope_exit and op.reason == .scope_exit) {
                    continue;
                }
                try self.emitRcOp(op);
            }
        }
    }

    /// Emit all RC operations for a line (both before and after)
    fn emitAllDrcOpsForLine(self: *Self, line: u32) !void {
        const ops = self.drc.getOpsForLine(line);
        for (ops) |op| {
            try self.emitRcOp(op);
        }
    }

    /// Emit a single RC operation from DRC
    fn emitRcOp(self: *Self, op: RcOp) !void {
        // Skip RC ops for StringBuilder variables - they manage their own memory
        if (self.isStringBuilderVar(op.target)) {
            return;
        }

        switch (op.kind) {
            .init => {
                // RC = 1 is handled by ms_alloc, no explicit op needed
            },
            .incref => {
                try self.emitIndent();
                try self.emit("ms_incref(");
                try self.emit(op.target);
                try self.emit(");\n");
            },
            .decref => {
                try self.emitIndent();
                try self.emit("ms_decref(");
                try self.emit(op.target);
                try self.emit(");\n");
            },
            .decref_cycle_check => {
                // Use ms_decref which handles cycle detection internally
                try self.emitIndent();
                try self.emit("ms_decref(");
                try self.emit(op.target);
                try self.emit(");\n");
            },
            .move => {
                // Transfer ownership - no RC change needed
                // Emit comment for debugging
                if (op.comment) |comment| {
                    try self.emitIndent();
                    try self.emit(comment);
                    try self.emit("\n");
                }
            },
            .scope_cleanup_start => {
                try self.emitIndent();
                try self.emit("// scope cleanup start\n");
            },
            .scope_cleanup_end => {
                try self.emitIndent();
                try self.emit("// scope cleanup end\n");
            },
        }
    }

    /// Convert BinaryOp enum to C operator string
    fn binaryOpToString(op: node_mod.BinaryOp) []const u8 {
        return switch (op) {
            .assign => "=",
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "%",
            .eq => "==",
            .ne => "!=",
            .lt => "<",
            .le => "<=",
            .gt => ">",
            .ge => ">=",
            .@"and" => "&&",
            .@"or" => "||",
            .bit_and => "&",
            .bit_or => "|",
            .bit_xor => "^",
            .shl => "<<",
            .shr => ">>",
        };
    }

    /// Convert UnaryOp enum to C operator string
    fn unaryOpToString(op: node_mod.UnaryOp) []const u8 {
        return switch (op) {
            .neg => "-",
            .not => "!",
            .bit_not => "~",
            .typeof => "typeof",  // Note: typeof not standard C
            .void => "(void)",
        };
    }

    /// Generate C from a typed AST
    pub fn generate(self: *Self, program: *ast.Node) ![]const u8 {
        std.debug.assert(program.kind == .program);

        // Pass 1: Pre-scan for StringBuilder candidates (just to know if header is needed)
        // Per-function detection happens in emitFunctionDecl, this is just for include decision
        const uses_stringbuilder = if (self.enable_stringbuilder) self.hasStringBuilderCandidates(program) else false;

        // Pass 2: Collect string literals for interning (if enabled)
        // Also marks literals used only in StringBuilder contexts
        if (self.enable_interning) {
            try self.collectStringLiterals(program);
            // Mark StringBuilder-only literals for exclusion from pool
            if (uses_stringbuilder) {
                try self.markStringBuilderOnlyLiterals(program);
            }
        }

        // Emit includes
        try self.emitIncludesWithStringBuilder(uses_stringbuilder);
        try self.emitNewline();

        // Emit string literal pool (if interning enabled and has literals)
        // Excludes literals that are only used in StringBuilder appends
        try self.emitStringLiteralPool();

        // Check if user defined a main function
        var has_user_main = false;
        for (program.data.program.statements) |stmt| {
            if (stmt.kind == .function_decl) {
                const func = &stmt.data.function_decl;
                if (std.mem.eql(u8, func.name, "main")) {
                    has_user_main = true;
                    break;
                }
            }
        }

        // Emit top-level variable declarations as globals
        for (program.data.program.statements) |stmt| {
            if (stmt.kind == .variable_stmt) {
                try self.emitGlobalVariables(stmt);
            }
        }

        // Emit forward declarations for classes/functions
        for (program.data.program.statements) |stmt| {
            if (stmt.kind == .class_decl or stmt.kind == .function_decl) {
                try self.emitNode(stmt);
            }
        }

        // Only emit wrapper main if user didn't define one
        if (!has_user_main) {
            // Emit main function containing all code
            try self.emit("int main(void) {\n");
            self.indent_level += 1;

            // Emit program statements inside main
            for (program.data.program.statements) |stmt| {
                // Skip class/function declarations (already emitted)
                if (stmt.kind != .class_decl and stmt.kind != .function_decl) {
                    try self.emitIndent();
                    try self.emitNode(stmt);
                }
            }

            // Return 0
            try self.emitIndent();
            try self.emit("return 0;\n");

            self.indent_level -= 1;
            try self.emit("}\n");
        }

        return try self.output.toOwnedSlice();
    }

    /// Pass 1: Collect all string literals for interning
    fn collectStringLiterals(self: *Self, node: *ast.Node) !void {
        switch (node.kind) {
            .string_literal => {
                _ = try self.registerStringLiteral(node.data.string_literal);
            },
            .program => {
                for (node.data.program.statements) |stmt| {
                    try self.collectStringLiterals(stmt);
                }
            },
            .function_decl => {
                const func = &node.data.function_decl;
                if (func.body) |body| {
                    try self.collectStringLiterals(body);
                }
            },
            .class_decl => {
                for (node.data.class_decl.members) |member| {
                    try self.collectStringLiterals(member);
                }
            },
            .block_stmt => {
                for (node.data.block_stmt.statements) |stmt| {
                    try self.collectStringLiterals(stmt);
                }
            },
            .variable_stmt => {
                const var_stmt = &node.data.variable_stmt;
                for (var_stmt.declarations) |decl| {
                    if (decl.init) |var_init| {
                        try self.collectStringLiterals(var_init);
                    }
                }
            },
            .expression_stmt => {
                try self.collectStringLiterals(node.data.expression_stmt);
            },
            .if_stmt => {
                const if_stmt = &node.data.if_stmt;
                try self.collectStringLiterals(if_stmt.condition);
                try self.collectStringLiterals(if_stmt.consequent);
                if (if_stmt.alternate) |else_br| {
                    try self.collectStringLiterals(else_br);
                }
            },
            .while_stmt => {
                const while_stmt = &node.data.while_stmt;
                try self.collectStringLiterals(while_stmt.condition);
                try self.collectStringLiterals(while_stmt.body);
            },
            .for_stmt => {
                const for_stmt = &node.data.for_stmt;
                if (for_stmt.init) |for_init| try self.collectStringLiterals(for_init);
                if (for_stmt.condition) |cond| try self.collectStringLiterals(cond);
                if (for_stmt.update) |upd| try self.collectStringLiterals(upd);
                try self.collectStringLiterals(for_stmt.body);
            },
            .return_stmt => {
                if (node.data.return_stmt.argument) |arg| {
                    try self.collectStringLiterals(arg);
                }
            },
            .binary_expr => {
                const binary = &node.data.binary_expr;
                try self.collectStringLiterals(binary.left);
                try self.collectStringLiterals(binary.right);
            },
            .unary_expr => {
                try self.collectStringLiterals(node.data.unary_expr.argument);
            },
            .call_expr => {
                const call = &node.data.call_expr;
                try self.collectStringLiterals(call.callee);
                for (call.arguments) |arg| {
                    try self.collectStringLiterals(arg);
                }
            },
            .member_expr => {
                const member = &node.data.member_expr;
                try self.collectStringLiterals(member.object);
                // For computed access (obj[key]), the key could contain string literals
                if (member.computed) {
                    try self.collectStringLiterals(member.property);
                }
            },
            .array_expr => {
                for (node.data.array_expr.elements) |elem| {
                    try self.collectStringLiterals(elem);
                }
            },
            .object_expr => {
                for (node.data.object_expr.properties) |prop| {
                    switch (prop) {
                        .property => |p| {
                            try self.collectStringLiterals(p.key);
                            try self.collectStringLiterals(p.value);
                        },
                        .spread => |s| try self.collectStringLiterals(s),
                    }
                }
            },
            .property_decl => {
                const prop = &node.data.property_decl;
                if (prop.init) |prop_init| {
                    try self.collectStringLiterals(prop_init);
                }
            },
            .method_decl => {
                const method = &node.data.method_decl;
                if (method.body) |body| {
                    try self.collectStringLiterals(body);
                }
            },
            // Terminals that don't contain string literals
            .number_literal, .boolean_literal, .null_literal, .identifier,
            .break_stmt, .continue_stmt, .interface_decl, .type_alias_decl => {},
            // Other nodes - skip
            else => {},
        }
    }

    /// Emit standard C includes
    fn emitIncludesWithStringBuilder(self: *Self, uses_stringbuilder: bool) !void {
        try self.emit("#include <stdio.h>\n");
        try self.emit("#include <stdint.h>\n");
        try self.emit("#include <stdbool.h>\n");
        try self.emit("#include <string.h>\n");
        try self.emit("#include <stdlib.h>\n");
        try self.emit("#include <math.h>\n");
        try self.emit("\n// Metascript ORC Runtime (compile with: -I<metascript>/src/runtime)\n");
        try self.emit("#define MS_ORC_IMPLEMENTATION\n");
        try self.emit("#include \"orc.h\"\n");
        try self.emit("#include \"ms_string.h\"\n");
        if (self.enable_interning) {
            try self.emit("#include \"ms_string_intern.h\"\n");
        }
        // Include StringBuilder header if any candidates detected
        if (uses_stringbuilder) {
            try self.emit("#include \"ms_string_builder.h\"\n");
        }
    }

    /// Check if the program contains any StringBuilder candidates (concat-in-loop patterns)
    /// This is a lightweight pre-scan used only to decide if StringBuilder header is needed
    fn hasStringBuilderCandidates(self: *Self, node: *ast.Node) bool {
        _ = self;
        return hasStringBuilderCandidatesRecursive(node, false);
    }

    fn hasStringBuilderCandidatesRecursive(node: *ast.Node, in_loop: bool) bool {
        switch (node.kind) {
            .program => {
                for (node.data.program.statements) |stmt| {
                    if (hasStringBuilderCandidatesRecursive(stmt, false)) return true;
                }
            },
            .function_decl => {
                const func = &node.data.function_decl;
                if (func.body) |body| {
                    if (hasStringBuilderCandidatesRecursive(body, false)) return true;
                }
            },
            .block_stmt => {
                for (node.data.block_stmt.statements) |stmt| {
                    if (hasStringBuilderCandidatesRecursive(stmt, in_loop)) return true;
                }
            },
            .while_stmt => {
                if (hasStringBuilderCandidatesRecursive(node.data.while_stmt.body, true)) return true;
            },
            .for_stmt => {
                if (hasStringBuilderCandidatesRecursive(node.data.for_stmt.body, true)) return true;
            },
            .expression_stmt => {
                if (hasStringBuilderCandidatesRecursive(node.data.expression_stmt, in_loop)) return true;
            },
            .binary_expr => {
                const binary = &node.data.binary_expr;
                // Check for concat-in-loop pattern: str = str + x
                if (in_loop and binary.op == .assign) {
                    if (binary.right.kind == .binary_expr) {
                        const rhs = &binary.right.data.binary_expr;
                        if (rhs.op == .add) {
                            if (binary.left.kind == .identifier and rhs.left.kind == .identifier) {
                                const lhs_name = binary.left.data.identifier;
                                const rhs_left_name = rhs.left.data.identifier;
                                if (std.mem.eql(u8, lhs_name, rhs_left_name)) {
                                    const is_string = if (binary.left.type) |t| t.kind == .string else false;
                                    if (is_string) return true;
                                }
                            }
                        }
                    }
                }
            },
            else => {},
        }
        return false;
    }

    /// Mark string literals that are ONLY used in StringBuilder concat patterns
    /// These can be excluded from the interned string pool
    fn markStringBuilderOnlyLiterals(self: *Self, node: *ast.Node) !void {
        try self.markStringBuilderOnlyLiteralsRecursive(node, false);
    }

    fn markStringBuilderOnlyLiteralsRecursive(self: *Self, node: *ast.Node, in_loop: bool) !void {
        switch (node.kind) {
            .program => {
                for (node.data.program.statements) |stmt| {
                    try self.markStringBuilderOnlyLiteralsRecursive(stmt, false);
                }
            },
            .function_decl => {
                const func = &node.data.function_decl;
                if (func.body) |body| {
                    try self.markStringBuilderOnlyLiteralsRecursive(body, false);
                }
            },
            .block_stmt => {
                for (node.data.block_stmt.statements) |stmt| {
                    try self.markStringBuilderOnlyLiteralsRecursive(stmt, in_loop);
                }
            },
            .while_stmt => {
                try self.markStringBuilderOnlyLiteralsRecursive(node.data.while_stmt.body, true);
            },
            .for_stmt => {
                try self.markStringBuilderOnlyLiteralsRecursive(node.data.for_stmt.body, true);
            },
            .expression_stmt => {
                try self.markStringBuilderOnlyLiteralsRecursive(node.data.expression_stmt, in_loop);
            },
            .variable_stmt => {
                // Check for StringBuilder init pattern: let result: string = ""
                const var_stmt = &node.data.variable_stmt;
                for (var_stmt.declarations) |decl| {
                    if (decl.init) |init_expr| {
                        // Check if this is a string variable with empty string init
                        // that will later be used in a concat-in-loop pattern
                        if (init_expr.kind == .string_literal) {
                            const is_string_type = if (decl.type) |t| t.kind == .string else false;
                            if (is_string_type) {
                                // Mark the init literal as StringBuilder-only
                                // It will be used in ms_sb_init or ms_sb_append_cstr
                                const literal = init_expr.data.string_literal;
                                if (!self.stringbuilder_only_literals.contains(literal)) {
                                    const key = try self.allocator.dupe(u8, literal);
                                    try self.stringbuilder_only_literals.put(key, {});
                                }
                            }
                        }
                    }
                }
            },
            .binary_expr => {
                const binary = &node.data.binary_expr;
                // Check for concat-in-loop pattern: str = str + "literal"
                if (in_loop and binary.op == .assign) {
                    if (binary.right.kind == .binary_expr) {
                        const rhs = &binary.right.data.binary_expr;
                        if (rhs.op == .add) {
                            if (binary.left.kind == .identifier and rhs.left.kind == .identifier) {
                                const lhs_name = binary.left.data.identifier;
                                const rhs_left_name = rhs.left.data.identifier;
                                if (std.mem.eql(u8, lhs_name, rhs_left_name)) {
                                    const is_string = if (binary.left.type) |t| t.kind == .string else false;
                                    if (is_string) {
                                        // Mark the appended literal as StringBuilder-only
                                        if (rhs.right.kind == .string_literal) {
                                            const literal = rhs.right.data.string_literal;
                                            if (!self.stringbuilder_only_literals.contains(literal)) {
                                                const key = try self.allocator.dupe(u8, literal);
                                                try self.stringbuilder_only_literals.put(key, {});
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
            else => {},
        }
    }

    // ========================================================================
    // String Interning Helpers
    // ========================================================================

    /// Register a string literal and return its intern ID
    /// If already registered, returns existing ID (deduplication)
    fn registerStringLiteral(self: *Self, literal: []const u8) !u32 {
        if (self.string_literals.get(literal)) |id| {
            return id;
        }
        const id = self.next_intern_id;
        // Make a copy of the literal for the hash map
        const key = try self.allocator.dupe(u8, literal);
        try self.string_literals.put(key, id);
        self.next_intern_id += 1;
        return id;
    }

    /// Emit the string literal pool (static interned strings)
    /// Call this after collecting all literals but before main code
    fn emitStringLiteralPool(self: *Self) !void {
        if (!self.enable_interning or self.string_literals.count() == 0) return;

        // Collect entries and sort by ID for deterministic output
        // Skip literals that are only used in StringBuilder contexts
        var entries = std.ArrayList(struct { key: []const u8, id: u32 }).init(self.allocator);
        defer entries.deinit();

        var iter = self.string_literals.iterator();
        while (iter.next()) |entry| {
            // Skip StringBuilder-only literals - they're passed as raw C strings
            if (self.stringbuilder_only_literals.contains(entry.key_ptr.*)) {
                continue;
            }
            try entries.append(.{ .key = entry.key_ptr.*, .id = entry.value_ptr.* });
        }

        // If all literals are StringBuilder-only, skip the pool entirely
        if (entries.items.len == 0) return;

        try self.emit("\n// ========================================================================\n");
        try self.emit("// String Literal Pool (compile-time interning)\n");
        try self.emit("// ========================================================================\n\n");

        // Sort by ID
        std.mem.sort(@TypeOf(entries.items[0]), entries.items, {}, struct {
            fn lessThan(_: void, a: @TypeOf(entries.items[0]), b: @TypeOf(entries.items[0])) bool {
                return a.id < b.id;
            }
        }.lessThan);

        // Emit static interned string declarations
        for (entries.items) |entry| {
            try self.emit("static msInternedString _str_");
            var id_buf: [16]u8 = undefined;
            const id_str = std.fmt.bufPrint(&id_buf, "{d}", .{entry.id}) catch "0";
            try self.emit(id_str);
            try self.emit(" = MS_INTERN_INIT(\"");
            try self.emitEscapedString(entry.key);
            try self.emit("\", ");
            var len_buf: [16]u8 = undefined;
            const len_str = std.fmt.bufPrint(&len_buf, "{d}", .{entry.key.len}) catch "0";
            try self.emit(len_str);
            try self.emit(");\n");
        }
        try self.emit("\n");
    }

    /// Emit a string with C escape sequences
    fn emitEscapedString(self: *Self, s: []const u8) !void {
        for (s) |c| {
            switch (c) {
                0 => try self.emit("\\0"),
                '\n' => try self.emit("\\n"),
                '\r' => try self.emit("\\r"),
                '\t' => try self.emit("\\t"),
                '\\' => try self.emit("\\\\"),
                '"' => try self.emit("\\\""),
                '\x07' => try self.emit("\\a"), // bell
                '\x08' => try self.emit("\\b"), // backspace
                '\x0C' => try self.emit("\\f"), // form feed
                '\x0B' => try self.emit("\\v"), // vertical tab
                else => {
                    if (c >= 32 and c < 127) {
                        try self.output.append(c);
                    } else {
                        // Emit as hex escape for non-printable chars
                        var buf: [8]u8 = undefined;
                        const escaped = std.fmt.bufPrint(&buf, "\\x{x:0>2}", .{c}) catch "\\x00";
                        try self.emit(escaped);
                    }
                },
            }
        }
    }

    // ========================================================================
    // StringBuilder Optimization - Concat-in-loop detection
    // ========================================================================

    /// Detect variables that are string-concatenated inside loops
    /// Pattern: `str = str + x` or `str += x` where str is string type inside loop
    /// Returns true if the node modifies a string variable via concatenation
    fn detectStringBuilderCandidates(self: *Self, node: *ast.Node, in_loop: bool) !void {
        switch (node.kind) {
            .program => {
                for (node.data.program.statements) |stmt| {
                    try self.detectStringBuilderCandidates(stmt, false);
                }
            },
            .function_decl => {
                const func = &node.data.function_decl;
                if (func.body) |body| {
                    try self.detectStringBuilderCandidates(body, false);
                }
            },
            .class_decl => {
                for (node.data.class_decl.members) |member| {
                    try self.detectStringBuilderCandidates(member, false);
                }
            },
            .method_decl => {
                const method = &node.data.method_decl;
                if (method.body) |body| {
                    try self.detectStringBuilderCandidates(body, false);
                }
            },
            .block_stmt => {
                for (node.data.block_stmt.statements) |stmt| {
                    try self.detectStringBuilderCandidates(stmt, in_loop);
                }
            },
            .if_stmt => {
                const if_stmt = &node.data.if_stmt;
                try self.detectStringBuilderCandidates(if_stmt.consequent, in_loop);
                if (if_stmt.alternate) |else_br| {
                    try self.detectStringBuilderCandidates(else_br, in_loop);
                }
            },
            .while_stmt => {
                const while_stmt = &node.data.while_stmt;
                // Recursively scan body with in_loop=true
                try self.detectStringBuilderCandidates(while_stmt.body, true);
            },
            .for_stmt => {
                const for_stmt = &node.data.for_stmt;
                // Recursively scan body with in_loop=true
                try self.detectStringBuilderCandidates(for_stmt.body, true);
            },
            .expression_stmt => {
                try self.detectStringBuilderCandidates(node.data.expression_stmt, in_loop);
            },
            .binary_expr => {
                const binary = &node.data.binary_expr;
                // Check for concat-in-loop pattern: str = str + x
                if (in_loop and binary.op == .assign) {
                    // Check if RHS is a string concatenation
                    if (binary.right.kind == .binary_expr) {
                        const rhs = &binary.right.data.binary_expr;
                        if (rhs.op == .add) {
                            // Check if LHS and RHS.left refer to the same variable
                            if (binary.left.kind == .identifier and rhs.left.kind == .identifier) {
                                const lhs_name = binary.left.data.identifier;
                                const rhs_left_name = rhs.left.data.identifier;
                                if (std.mem.eql(u8, lhs_name, rhs_left_name)) {
                                    // Check if it's a string type
                                    const is_string = if (binary.left.type) |t| t.kind == .string else false;
                                    if (is_string) {
                                        // Register as StringBuilder candidate
                                        if (!self.stringbuilder_vars.contains(lhs_name)) {
                                            const key = try self.allocator.dupe(u8, lhs_name);
                                            try self.stringbuilder_vars.put(key, {});
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
            else => {},
        }
    }

    /// Check if a variable is a StringBuilder candidate
    fn isStringBuilderVar(self: *Self, name: []const u8) bool {
        return self.enable_stringbuilder and self.stringbuilder_vars.contains(name);
    }

    /// Emit StringBuilder variable declaration
    /// Transforms: let result: string = "";
    /// Into: msStringBuilder _sb_result; ms_sb_init_default(&_sb_result);
    fn emitStringBuilderDecl(self: *Self, decl: node_mod.VariableStmt.VariableDeclarator) !void {
        // Emit StringBuilder declaration
        try self.emit("msStringBuilder _sb_");
        try self.emit(decl.name);
        try self.emit(";\n");

        // Initialize StringBuilder
        try self.emitIndent();
        try self.emit("ms_sb_init_default(&_sb_");
        try self.emit(decl.name);
        try self.emit(");\n");

        // If there's an initial value, append it
        if (decl.init) |init_expr| {
            // Only append if not empty string
            if (init_expr.kind != .string_literal or init_expr.data.string_literal.len > 0) {
                try self.emitIndent();
                try self.emitStringBuilderAppend(decl.name, init_expr);
            }
        }
    }

    /// Emit ms_sb_append_* call for a value
    fn emitStringBuilderAppend(self: *Self, var_name: []const u8, value: *ast.Node) !void {
        // Determine the type and use appropriate append function
        const value_type = value.type;

        if (value.kind == .string_literal) {
            // String literal - use ms_sb_append_cstr
            try self.emit("ms_sb_append_cstr(&_sb_");
            try self.emit(var_name);
            try self.emit(", \"");
            try self.emitEscapedString(value.data.string_literal);
            try self.emit("\");\n");
        } else if (value_type) |typ| {
            if (typ.kind == .string) {
                // msString* - use ms_sb_append_string
                try self.emit("ms_sb_append_string(&_sb_");
                try self.emit(var_name);
                try self.emit(", ");
                try self.emitExpression(value);
                try self.emit(");\n");
            } else if (typ.kind == .number or typ.kind == .float64 or typ.kind == .float32) {
                // double - use ms_sb_append_double
                try self.emit("ms_sb_append_double(&_sb_");
                try self.emit(var_name);
                try self.emit(", ");
                try self.emitExpression(value);
                try self.emit(");\n");
            } else if (typ.kind == .int32 or typ.kind == .int64) {
                // int - use ms_sb_append_int
                try self.emit("ms_sb_append_int(&_sb_");
                try self.emit(var_name);
                try self.emit(", ");
                try self.emitExpression(value);
                try self.emit(");\n");
            } else if (typ.kind == .boolean) {
                // bool - use ms_sb_append_bool
                try self.emit("ms_sb_append_bool(&_sb_");
                try self.emit(var_name);
                try self.emit(", ");
                try self.emitExpression(value);
                try self.emit(");\n");
            } else {
                // Fallback: convert to string with ms_sb_append_string
                try self.emit("ms_sb_append_string(&_sb_");
                try self.emit(var_name);
                try self.emit(", ");
                try self.emitExpression(value);
                try self.emit(");\n");
            }
        } else {
            // Unknown type - use generic string append
            try self.emit("ms_sb_append_string(&_sb_");
            try self.emit(var_name);
            try self.emit(", ");
            try self.emitExpression(value);
            try self.emit(");\n");
        }
    }

    /// Emit StringBuilder concat assignment: result = result + x -> ms_sb_append_*(...)
    fn emitStringBuilderConcat(self: *Self, var_name: []const u8, rhs_value: *ast.Node) !void {
        // Emit the append operation
        try self.emitStringBuilderAppend(var_name, rhs_value);
    }

    /// Check if a binary expression is a StringBuilder concat pattern
    /// Pattern: str = str + value (where str is a StringBuilder var)
    fn isStringBuilderConcatExpr(self: *Self, node: *ast.Node) ?struct { var_name: []const u8, append_value: *ast.Node } {
        if (node.kind != .binary_expr) return null;

        const binary = &node.data.binary_expr;
        if (binary.op != .assign) return null;

        // LHS must be identifier that's a StringBuilder var
        if (binary.left.kind != .identifier) return null;
        const lhs_name = binary.left.data.identifier;
        if (!self.isStringBuilderVar(lhs_name)) return null;

        // RHS must be binary add with LHS as first operand
        if (binary.right.kind != .binary_expr) return null;
        const rhs_binary = &binary.right.data.binary_expr;
        if (rhs_binary.op != .add) return null;

        // RHS.left must be same identifier
        if (rhs_binary.left.kind != .identifier) return null;
        if (!std.mem.eql(u8, rhs_binary.left.data.identifier, lhs_name)) return null;

        return .{ .var_name = lhs_name, .append_value = rhs_binary.right };
    }

    /// Main dispatch for node emission
    pub fn emitNode(self: *Self, node: *ast.Node) anyerror!void {
        // Track current line for DRC queries
        self.current_line = node.location.start.line;

        switch (node.kind) {
            // Declarations
            .class_decl => try self.emitClassDecl(node),
            .function_decl => try self.emitFunctionDecl(node),

            // Statements
            .variable_stmt => try self.emitVariableStmt(node),
            .expression_stmt => try self.emitExpressionStmt(node),
            .block_stmt => try self.emitBlockStmt(node),
            .if_stmt => try self.emitIfStmt(node),
            .while_stmt => try self.emitWhileStmt(node),
            .for_stmt => try self.emitForStmt(node),
            .return_stmt => try self.emitReturnStmt(node),
            .break_stmt => try self.emitBreakStmt(),
            .continue_stmt => try self.emitContinueStmt(),

            // Skip type-only declarations
            .interface_decl, .type_alias_decl => {},

            // Others not implemented yet
            else => {
                // Placeholder comment for unsupported nodes
                try self.emit("// TODO: ");
                try self.emit(@tagName(node.kind));
                try self.emitNewline();
            },
        }
    }

    /// Emit a class as a C struct
    fn emitClassDecl(self: *Self, node: *ast.Node) !void {
        const class = &node.data.class_decl;

        // typedef struct ClassName {
        try self.emit("typedef struct ");
        try self.emit(class.name);
        try self.emit(" {\n");
        self.indent_level += 1;

        // Emit properties as struct fields
        for (class.members) |member| {
            if (member.kind == .property_decl) {
                const prop = &member.data.property_decl;
                try self.emitIndent();
                try self.emitType(prop.type);
                try self.emit(" ");
                try self.emit(prop.name);
                try self.emit(";\n");
            }
        }

        self.indent_level -= 1;
        try self.emit("} ");
        try self.emit(class.name);
        try self.emit(";\n\n");

        // Emit method prototypes
        for (class.members) |member| {
            if (member.kind == .method_decl) {
                try self.emitMethodPrototype(class.name, member);
            }
        }

        // ORC Integration: Emit trace function and TypeInfo for cycle detection
        try self.emitClassOrcSupport(class.name, class.members);

        // Emit method implementations (full function bodies)
        for (class.members) |member| {
            if (member.kind == .method_decl) {
                try self.emitMethodImplementation(class.name, member);
            }
        }
    }

    /// Check if a type is a reference type (needs ORC tracing)
    fn isOrcReferenceType(typ: ?*types_mod.Type) bool {
        if (typ == null) return false;
        const t = typ.?;
        return switch (t.kind) {
            // Reference types that need ORC tracing
            // - object: class instances (heap allocated with ORC header)
            // - array: arrays (heap allocated with ORC header)
            // - string: msString* (heap allocated with ORC header)
            // - type_reference: may resolve to a reference type
            .object, .array, .string, .type_reference => true,
            // Primitives - no tracing needed
            else => false,
        };
    }

    // ========================================================================
    // Nested Call Temporary Handling - for f(g(x)) pattern
    // ========================================================================

    /// Check if a call expression returns an RC type that needs cleanup
    fn callReturnsRcType(node: *ast.Node) bool {
        if (node.kind != .call_expr) return false;
        return isOrcReferenceType(node.type);
    }

    /// Collect nested call expressions from an expression that need to be hoisted
    /// These are call expressions used as arguments to other calls that return RC types
    fn collectNestedRcCalls(self: *Self, node: *ast.Node, calls: *std.ArrayList(*ast.Node)) !void {
        switch (node.kind) {
            .call_expr => {
                const call = &node.data.call_expr;
                // Check arguments for nested calls that return RC types
                for (call.arguments) |arg| {
                    if (arg.kind == .call_expr and callReturnsRcType(arg)) {
                        // This is a nested call returning RC type - needs hoisting
                        try calls.append(arg);
                    }
                    // Recursively check for deeper nesting
                    try self.collectNestedRcCalls(arg, calls);
                }
            },
            .binary_expr => {
                const bin = &node.data.binary_expr;
                try self.collectNestedRcCalls(bin.left, calls);
                try self.collectNestedRcCalls(bin.right, calls);
            },
            .unary_expr => {
                try self.collectNestedRcCalls(node.data.unary_expr.argument, calls);
            },
            .member_expr => {
                try self.collectNestedRcCalls(node.data.member_expr.object, calls);
            },
            else => {},
        }
    }

    /// Hoist nested RC-returning calls into temporaries before the statement
    /// Populates self.hoisted_calls map for use during expression emission
    /// Note: Caller (block) already emitted indent for first item, so we skip first indent
    fn hoistNestedCalls(self: *Self, calls: []const *ast.Node) !void {
        for (calls, 0..) |call_node, i| {
            // Generate unique temporary name
            const temp_name = try std.fmt.allocPrint(self.allocator, "_nested_tmp_{d}", .{self.nested_call_temp_counter});
            self.nested_call_temp_counter += 1;

            // Emit temporary declaration and initialization
            // Skip indent for first item (block already emitted it)
            if (i > 0) {
                try self.emitIndent();
            }
            try self.emitType(call_node.type);
            try self.emit(" ");
            try self.emit(temp_name);
            try self.emit(" = ");
            // Recursively emit the call - nested calls within this one
            // will already be in hoisted_calls from earlier iterations
            try self.emitExpression(call_node);
            try self.emit(";\n");

            // Track for cleanup after statement (use original, will be freed in cleanup)
            try self.pending_temps.append(temp_name);

            // Add to map so emitExpression will use temp name
            try self.hoisted_calls.put(call_node, temp_name);
        }
    }

    /// Emit cleanup for pending temporaries after a statement
    fn emitTempCleanup(self: *Self) !void {
        for (self.pending_temps.items) |temp_name| {
            try self.emitIndent();
            try self.emit("ms_decref(");
            try self.emit(temp_name);
            try self.emit(");\n");
            self.allocator.free(temp_name);
        }
        self.pending_temps.clearRetainingCapacity();
        self.hoisted_calls.clearRetainingCapacity();
    }

    /// Emit ORC support (trace function + TypeInfo) for a class
    fn emitClassOrcSupport(self: *Self, class_name: []const u8, members: []*ast.Node) !void {
        // Check if class has any reference-type fields (determines is_cyclic)
        var has_ref_fields = false;
        for (members) |member| {
            if (member.kind == .property_decl) {
                const prop = &member.data.property_decl;
                if (isOrcReferenceType(prop.type)) {
                    has_ref_fields = true;
                    break;
                }
            }
        }

        // Emit trace function (traces reference fields for cycle detection)
        try self.emit("static void ");
        try self.emit(class_name);
        try self.emit("_trace(void* obj, msTraceCallback cb) {\n");
        self.indent_level += 1;

        if (has_ref_fields) {
            try self.emitIndent();
            try self.emit(class_name);
            try self.emit("* self = (");
            try self.emit(class_name);
            try self.emit("*)obj;\n");

            // Trace each reference-type field
            for (members) |member| {
                if (member.kind == .property_decl) {
                    const prop = &member.data.property_decl;
                    if (isOrcReferenceType(prop.type)) {
                        try self.emitIndent();
                        try self.emit("if (self->");
                        try self.emit(prop.name);
                        try self.emit(") cb(self->");
                        try self.emit(prop.name);
                        try self.emit(");\n");
                    }
                }
            }
        } else {
            try self.emitIndent();
            try self.emit("(void)obj; (void)cb; // No reference fields to trace\n");
        }

        self.indent_level -= 1;
        try self.emit("}\n\n");

        // Emit TypeInfo struct
        try self.emit("static const msTypeInfo ");
        try self.emit(class_name);
        try self.emit("_type = {\n");
        self.indent_level += 1;

        try self.emitIndent();
        try self.emit(".name = \"");
        try self.emit(class_name);
        try self.emit("\",\n");

        try self.emitIndent();
        try self.emit(".size = sizeof(");
        try self.emit(class_name);
        try self.emit("),\n");

        try self.emitIndent();
        if (has_ref_fields) {
            try self.emit(".is_cyclic = true,\n");
        } else {
            try self.emit(".is_cyclic = false,\n");
        }

        try self.emitIndent();
        try self.emit(".trace_fn = ");
        try self.emit(class_name);
        try self.emit("_trace,\n");

        try self.emitIndent();
        try self.emit(".destroy_fn = NULL\n");

        self.indent_level -= 1;
        try self.emit("};\n\n");

        // Emit portable _new() constructor function
        // This replaces GCC statement expressions with standard C
        try self.emit("static inline ");
        try self.emit(class_name);
        try self.emit("* ");
        try self.emit(class_name);
        try self.emit("_new(void) {\n");
        self.indent_level += 1;

        try self.emitIndent();
        try self.emit(class_name);
        try self.emit("* _t = (");
        try self.emit(class_name);
        try self.emit("*)ms_alloc(sizeof(");
        try self.emit(class_name);
        try self.emit("));\n");

        try self.emitIndent();
        try self.emit("ms_set_type(_t, &");
        try self.emit(class_name);
        try self.emit("_type);\n");

        try self.emitIndent();
        try self.emit("return _t;\n");

        self.indent_level -= 1;
        try self.emit("}\n\n");
    }

    /// Emit method prototype: returnType ClassName_methodName(ClassName* this, params...)
    fn emitMethodPrototype(self: *Self, class_name: []const u8, node: *ast.Node) !void {
        const method = &node.data.method_decl;

        // Return type
        if (method.return_type) |ret| {
            try self.emitType(ret);
        } else {
            try self.emit("void");
        }
        try self.emit(" ");

        // ClassName_methodName
        try self.emit(class_name);
        try self.emit("_");
        try self.emit(method.name);
        try self.emit("(");

        // First param: ClassName* this
        try self.emit(class_name);
        try self.emit("* this");

        // Other params
        for (method.params) |param| {
            try self.emit(", ");
            if (param.type) |ptype| {
                try self.emitType(ptype);
            } else {
                try self.emit("void*");
            }
            try self.emit(" ");
            try self.emit(param.name);
        }

        try self.emit(")");

        // For now, just semicolon (no body)
        try self.emit(";\n");
    }

    /// Emit method implementation: full function body
    fn emitMethodImplementation(self: *Self, class_name: []const u8, node: *ast.Node) !void {
        const method = &node.data.method_decl;

        // Return type
        if (method.return_type) |ret| {
            try self.emitType(ret);
        } else {
            try self.emit("void");
        }
        try self.emit(" ");

        // ClassName_methodName
        try self.emit(class_name);
        try self.emit("_");
        try self.emit(method.name);
        try self.emit("(");

        // First param: ClassName* this
        try self.emit(class_name);
        try self.emit("* this");

        // Other params
        for (method.params) |param| {
            try self.emit(", ");
            if (param.type) |ptype| {
                try self.emitType(ptype);
            } else {
                try self.emit("void*");
            }
            try self.emit(" ");
            try self.emit(param.name);
        }

        try self.emit(") ");

        // Emit method body
        if (method.body) |body| {
            if (body.kind == .block_stmt) {
                try self.emitBlockStmt(body);
            } else {
                // Single expression body - wrap in block
                try self.emit("{\n");
                self.indent_level += 1;
                try self.emitIndent();
                try self.emitNode(body);
                self.indent_level -= 1;
                try self.emit("}\n");
            }
        } else {
            try self.emit("{ }\n");
        }
        try self.emit("\n");
    }

    /// Emit function declaration (placeholder)
    fn emitFunctionDecl(self: *Self, node: *ast.Node) !void {
        const func = &node.data.function_decl;

        // Clear StringBuilder candidates from previous function (per-function scoping)
        try self.clearStringBuilderVars();
        self.current_function = func.name;

        // Detect StringBuilder candidates for THIS function only
        if (self.enable_stringbuilder) {
            if (func.body) |body| {
                try self.detectStringBuilderCandidates(body, false);
            }
        }

        // Special case: main always returns int in C
        const is_main = std.mem.eql(u8, func.name, "main");

        // Emit return type
        if (is_main) {
            try self.emit("int");
        } else if (func.return_type) |ret_type| {
            try self.emitType(ret_type);
        } else {
            try self.emit("void");
        }
        try self.emit(" ");

        // Emit function name
        try self.emit(func.name);

        // Emit parameters
        try self.emit("(");
        for (func.params, 0..) |param, i| {
            if (i > 0) try self.emit(", ");

            // Emit parameter type
            if (param.type) |param_type| {
                try self.emitType(param_type);
            } else {
                try self.emit("void*"); // default to void* for untyped params
            }
            try self.emit(" ");

            // Emit parameter name
            try self.emit(param.name);
        }
        try self.emit(")");

        // Emit body if present
        if (func.body) |body| {
            try self.emit(" ");
            if (body.kind == .block_stmt) {
                try self.emitFunctionBlockStmt(body);
            } else {
                // Single expression body - wrap in block with return
                try self.emit("{\n");
                self.indent_level += 1;
                try self.emitIndent();
                try self.emit("return ");
                try self.emitExpression(body);
                try self.emit(";\n");
                // StringBuilder cleanup before function exit
                try self.emitStringBuilderCleanup();
                self.indent_level -= 1;
                try self.emit("}\n");
            }
        } else {
            // Forward declaration
            try self.emit(";\n");
        }
    }

    /// Emit finalization and cleanup for all StringBuilder variables
    /// 1. Build each StringBuilder into a final msString* variable
    /// 2. Free the StringBuilder buffer
    /// 3. Decref the built string (ORC managed)
    fn emitStringBuilderCleanup(self: *Self) !void {
        if (self.stringbuilder_vars.count() == 0) return;

        // Step 1: Finalize StringBuilders that haven't been finalized yet
        // This happens when a StringBuilder is never accessed before function exit
        var iter = self.stringbuilder_vars.keyIterator();
        while (iter.next()) |key_ptr| {
            const name = key_ptr.*;
            if (!self.stringbuilder_finalized.contains(name)) {
                // Emit: msString* varname = ms_sb_build(&_sb_varname);
                try self.emitIndent();
                try self.emit("msString* ");
                try self.emit(name);
                try self.emit(" = ms_sb_build(&_sb_");
                try self.emit(name);
                try self.emit(");\n");
                // Mark as finalized
                const key = try self.allocator.dupe(u8, name);
                try self.stringbuilder_finalized.put(key, {});
            }
        }

        // Step 2: Free StringBuilder buffers
        iter = self.stringbuilder_vars.keyIterator();
        while (iter.next()) |key_ptr| {
            try self.emitIndent();
            try self.emit("ms_sb_free(&_sb_");
            try self.emit(key_ptr.*);
            try self.emit(");\n");
        }

        // Step 3: Decref the built strings (ORC cleanup)
        iter = self.stringbuilder_vars.keyIterator();
        while (iter.next()) |key_ptr| {
            try self.emitIndent();
            try self.emit("ms_decref(");
            try self.emit(key_ptr.*);
            try self.emit(");\n");
        }
    }

    /// Clear StringBuilder variables (called at function entry for per-function scoping)
    fn clearStringBuilderVars(self: *Self) !void {
        // Free all keys from stringbuilder_vars
        var key_iter = self.stringbuilder_vars.keyIterator();
        while (key_iter.next()) |key_ptr| {
            self.allocator.free(key_ptr.*);
        }
        self.stringbuilder_vars.clearRetainingCapacity();
        // Free all keys from stringbuilder_finalized
        var fin_iter = self.stringbuilder_finalized.keyIterator();
        while (fin_iter.next()) |key_ptr| {
            self.allocator.free(key_ptr.*);
        }
        self.stringbuilder_finalized.clearRetainingCapacity();
    }

    /// Finalize any StringBuilder variables used in an expression BEFORE emitting the expression
    /// This ensures the msString* is created once and properly tracked for cleanup
    /// Returns true if any finalization was emitted (caller may need to re-emit indent)
    fn finalizeStringBuildersInExprCheck(self: *Self, node: *ast.Node) !bool {
        var finalized_any = false;
        switch (node.kind) {
            .identifier => {
                const name = node.data.identifier;
                // If this is a StringBuilder var and not yet finalized, finalize it now
                if (self.isStringBuilderVar(name) and !self.stringbuilder_finalized.contains(name)) {
                    // Emit: msString* varname = ms_sb_build(&_sb_varname);
                    try self.emitIndent();
                    try self.emit("msString* ");
                    try self.emit(name);
                    try self.emit(" = ms_sb_build(&_sb_");
                    try self.emit(name);
                    try self.emit(");\n");
                    // Mark as finalized
                    const key = try self.allocator.dupe(u8, name);
                    try self.stringbuilder_finalized.put(key, {});
                    finalized_any = true;
                }
            },
            .binary_expr => {
                const binary = &node.data.binary_expr;
                // Skip LHS of assignment - it's the target, not a usage
                if (binary.op != .assign) {
                    if (try self.finalizeStringBuildersInExprCheck(binary.left)) {
                        finalized_any = true;
                    }
                }
                if (try self.finalizeStringBuildersInExprCheck(binary.right)) {
                    finalized_any = true;
                }
            },
            .call_expr => {
                const call = &node.data.call_expr;
                if (try self.finalizeStringBuildersInExprCheck(call.callee)) {
                    finalized_any = true;
                }
                for (call.arguments) |arg| {
                    if (try self.finalizeStringBuildersInExprCheck(arg)) {
                        finalized_any = true;
                    }
                }
            },
            .member_expr => {
                const member = &node.data.member_expr;
                if (try self.finalizeStringBuildersInExprCheck(member.object)) {
                    finalized_any = true;
                }
            },
            .unary_expr => {
                if (try self.finalizeStringBuildersInExprCheck(node.data.unary_expr.argument)) {
                    finalized_any = true;
                }
            },
            .conditional_expr => {
                const cond = &node.data.conditional_expr;
                if (try self.finalizeStringBuildersInExprCheck(cond.condition)) {
                    finalized_any = true;
                }
                if (try self.finalizeStringBuildersInExprCheck(cond.consequent)) {
                    finalized_any = true;
                }
                if (try self.finalizeStringBuildersInExprCheck(cond.alternate)) {
                    finalized_any = true;
                }
            },
            else => {},
        }
        return finalized_any;
    }

    /// Check if a statement uses any StringBuilder vars and finalize them if needed
    /// Called from block emission BEFORE emitting the statement's indent
    /// Returns true if any finalization occurred (caller should skip indent since we already emitted it)
    fn checkAndFinalizeStringBuildersForStmt(self: *Self, stmt: *ast.Node) !bool {
        // Only check expression statements and return statements
        switch (stmt.kind) {
            .expression_stmt => {
                const expr = stmt.data.expression_stmt;
                // Skip StringBuilder concat assignments (result = result + x)
                if (self.isStringBuilderConcatExpr(expr) != null) {
                    return false;
                }
                // Check if expression uses any StringBuilder vars
                const finalized = try self.finalizeStringBuildersInExprCheck(expr);
                if (finalized) {
                    // Re-emit indent for the actual statement
                    try self.emitIndent();
                }
                return finalized;
            },
            .return_stmt => {
                const ret = &stmt.data.return_stmt;
                if (ret.argument) |arg| {
                    const finalized = try self.finalizeStringBuildersInExprCheck(arg);
                    if (finalized) {
                        // Re-emit indent for the return statement
                        try self.emitIndent();
                    }
                    return finalized;
                }
                return false;
            },
            else => return false,
        }
    }

    /// Emit function body block statement (with StringBuilder cleanup before exit)
    fn emitFunctionBlockStmt(self: *Self, node: *ast.Node) !void {
        const block = &node.data.block_stmt;
        const block_start_line = node.location.start.line;

        try self.emit("{\n");
        self.indent_level += 1;

        // Emit statements in the block
        for (block.statements) |stmt| {
            const stmt_line = stmt.location.start.line;
            self.current_line = stmt_line;

            // Emit DRC ops that should come BEFORE this statement
            // This includes decref for reassignment old values
            // Skip scope_exit ops - they're emitted at block end
            try self.emitDrcOpsForLineFiltered(stmt_line, .before, true);

            // Check if this statement uses any StringBuilder vars - finalize them first
            // This must happen BEFORE emitIndent to avoid double-indentation
            const finalized_any = try self.checkAndFinalizeStringBuildersForStmt(stmt);

            // Emit indent (unless finalization already happened - it handles its own indent+reindent)
            if (!finalized_any) {
                try self.emitIndent();
            }
            try self.emitNode(stmt);

            // Emit DRC ops that should come AFTER this statement
            try self.emitDrcOpsForLine(stmt_line, .after);
        }

        // Update current_line to block's end for scope exit ops
        const block_end_line = if (block.statements.len > 0)
            block.statements[block.statements.len - 1].location.start.line
        else
            block_start_line;
        self.current_line = block_end_line;

        // Check if block ends with return/break/continue (control flow exit)
        const ends_with_control_flow = if (block.statements.len > 0)
            switch (block.statements[block.statements.len - 1].kind) {
                .return_stmt, .break_stmt, .continue_stmt => true,
                else => false,
            }
        else
            false;

        // Emit scope cleanup from DRC (if not already handled by control flow)
        if (!ends_with_control_flow) {
            try self.emitDrcOpsForLine(block_end_line, .before);
            // StringBuilder finalization and cleanup at function exit
            // 1. Build final strings (already done inline via ms_sb_build)
            // 2. Free StringBuilder buffers
            try self.emitStringBuilderCleanup();
        }

        self.indent_level -= 1;
        try self.emitIndent();
        try self.emit("}\n");
    }

    /// Emit global variable declarations (top-level const/let)
    fn emitGlobalVariables(self: *Self, node: *ast.Node) !void {
        const var_stmt = &node.data.variable_stmt;

        for (var_stmt.declarations) |decl| {
            // Determine type
            const is_string = if (decl.init) |init_expr|
                init_expr.kind == .string_literal
            else
                false;

            if (is_string and self.enable_interning) {
                // Emit as macro that lazily gets interned string
                // #define greeting ms_intern_get(&_str_0)
                const literal = decl.init.?.data.string_literal;
                if (self.string_literals.get(literal)) |intern_id| {
                    try self.emit("#define ");
                    try self.emit(decl.name);
                    try self.emit(" ms_intern_get(&_str_");
                    const id_str = try std.fmt.allocPrint(self.allocator, "{d}", .{intern_id});
                    defer self.allocator.free(id_str);
                    try self.emit(id_str);
                    try self.emit(")\n");
                }
            } else if (decl.type) |typ| {
                try self.emitType(typ);
                try self.emit(" ");
                try self.emit(decl.name);
                if (decl.init) |init_expr| {
                    try self.emit(" = ");
                    try self.emitExpression(init_expr);
                }
                try self.emit(";\n");
            } else if (decl.init) |init_expr| {
                // Infer type from initializer
                if (init_expr.kind == .number_literal) {
                    const num = init_expr.data.number_literal;
                    if (@floor(num) == num and num >= -2147483648 and num <= 2147483647) {
                        try self.emit("int ");
                    } else {
                        try self.emit("double ");
                    }
                } else if (init_expr.kind == .boolean_literal) {
                    try self.emit("bool ");
                } else if (is_string) {
                    // String without interning
                    try self.emit("msString* ");
                    try self.emit(decl.name);
                    try self.emit(" = ms_string_from_cstr(\"");
                    try self.emitEscapedString(init_expr.data.string_literal);
                    try self.emit("\");\n");
                    continue;
                } else {
                    try self.emit("void* ");
                }
                try self.emit(decl.name);
                try self.emit(" = ");
                try self.emitExpression(init_expr);
                try self.emit(";\n");
            }
        }
    }

    /// Emit variable statement: const x = ...; or let x = ...;
    fn emitVariableStmt(self: *Self, node: *ast.Node) !void {
        const var_stmt = &node.data.variable_stmt;

        for (var_stmt.declarations) |decl| {
            // Check if this is a StringBuilder candidate
            if (self.isStringBuilderVar(decl.name)) {
                try self.emitStringBuilderDecl(decl);
                continue;
            }

            // Hoist nested RC-returning calls from initializer
            // For: let x = f(g(h(y))), hoist h, then g, before emitting declaration
            var had_hoisted_temps = false;
            if (decl.init) |init_expr| {
                var nested_calls = std.ArrayList(*ast.Node).init(self.allocator);
                defer nested_calls.deinit();
                try self.collectNestedRcCalls(init_expr, &nested_calls);

                if (nested_calls.items.len > 0) {
                    // Reverse to process innermost calls first
                    std.mem.reverse(*ast.Node, nested_calls.items);
                    try self.hoistNestedCalls(nested_calls.items);
                    had_hoisted_temps = true;
                }
            }
            // If we hoisted temps, need indent for the actual declaration
            if (had_hoisted_temps) {
                try self.emitIndent();
            }

            // Check if initializer is a non-constant object literal
            const needs_split = if (decl.init) |init_expr|
                init_expr.kind == .object_expr and self.objectHasVariableReferences(init_expr)
            else
                false;

            if (needs_split) {
                // Declare variable, then assign fields separately
                if (decl.type) |typ| {
                    try self.emitType(typ);
                } else if (decl.init) |init_expr| {
                    // Try to use the inferred type from the initializer
                    if (init_expr.type) |inferred_type| {
                        try self.emitType(inferred_type);
                    } else if (init_expr.kind == .array_expr) {
                        try self.emit("double");
                    } else if (init_expr.kind == .number_literal) {
                        // Heuristic: if it's an integer literal, use int; otherwise double
                        const num = init_expr.data.number_literal;
                        if (@floor(num) == num and num >= -2147483648 and num <= 2147483647) {
                            try self.emit("int");
                        } else {
                            try self.emit("double");
                        }
                    } else if (init_expr.kind == .member_expr) {
                        // Member access or array indexing - default to double
                        try self.emit("double");
                    } else {
                        try self.emit("void*");
                    }
                } else {
                    try self.emit("void*");
                }
                try self.emit(" ");
                try self.emit(decl.name);

                // Emit array brackets if type is array
                if (decl.type) |typ| {
                    if (typ.kind == .array) {
                        if (decl.init) |init_expr| {
                            if (init_expr.kind == .array_expr) {
                                const array = &init_expr.data.array_expr;
                                const len_str = try std.fmt.allocPrint(self.allocator, "{d}", .{array.elements.len});
                                defer self.allocator.free(len_str);
                                try self.emit("[");
                                try self.emit(len_str);
                                try self.emit("]");
                            }
                        } else {
                            try self.emit("[]");
                        }
                    }
                } else if (decl.init) |init_expr| {
                    if (init_expr.kind == .array_expr) {
                        const array = &init_expr.data.array_expr;
                        const len_str = try std.fmt.allocPrint(self.allocator, "{d}", .{array.elements.len});
                        defer self.allocator.free(len_str);
                        try self.emit("[");
                        try self.emit(len_str);
                        try self.emit("]");
                    }
                }
                try self.emit(";\n");

                // Emit field assignments
                const obj = &decl.init.?.data.object_expr;
                for (obj.properties) |prop| {
                    switch (prop) {
                        .property => |p| {
                            try self.emitIndent();
                            try self.emit(decl.name);
                            try self.emit(".");
                            if (p.key.kind == .identifier) {
                                try self.emit(p.key.data.identifier);
                            }
                            try self.emit(" = ");
                            try self.emitExpression(p.value);
                            try self.emit(";\n");
                        },
                        .spread => {
                            // Spreads should be normalized away
                        },
                    }
                }
            } else {
                // Normal initialization (constants only)
                // Special case: new_expr gets ClassName* type (must check before decl.type)
                if (decl.init) |init_expr| {
                    if (init_expr.kind == .new_expr) {
                        const new_e = &init_expr.data.new_expr;
                        if (new_e.callee.kind == .identifier) {
                            try self.emit(new_e.callee.data.identifier);
                            try self.emit("*");
                        } else {
                            try self.emit("void*");
                        }
                    } else if (decl.type) |typ| {
                        try self.emitType(typ);
                    } else if (init_expr.type) |inferred_type| {
                        // Try to use the inferred type from the initializer
                        try self.emitType(inferred_type);
                    } else if (init_expr.kind == .array_expr) {
                        try self.emit("double");
                    } else if (init_expr.kind == .number_literal) {
                        // Heuristic: if it's an integer literal, use int; otherwise double
                        const num = init_expr.data.number_literal;
                        if (@floor(num) == num and num >= -2147483648 and num <= 2147483647) {
                            try self.emit("int");
                        } else {
                            try self.emit("double");
                        }
                    } else if (init_expr.kind == .member_expr) {
                        // Member access or array indexing - default to double
                        try self.emit("double");
                    } else {
                        try self.emit("void*");
                    }
                } else if (decl.type) |typ| {
                    // No initializer, but have explicit type
                    try self.emitType(typ);
                } else {
                    try self.emit("void*");
                }

                try self.emit(" ");
                try self.emit(decl.name);

                // Emit array brackets if type is array
                if (decl.type) |typ| {
                    if (typ.kind == .array) {
                        if (decl.init) |init_expr2| {
                            if (init_expr2.kind == .array_expr) {
                                const array = &init_expr2.data.array_expr;
                                const len_str = try std.fmt.allocPrint(self.allocator, "{d}", .{array.elements.len});
                                defer self.allocator.free(len_str);
                                try self.emit("[");
                                try self.emit(len_str);
                                try self.emit("]");
                            }
                        } else {
                            try self.emit("[]");
                        }
                    }
                } else if (decl.init) |init_expr| {
                    if (init_expr.kind == .array_expr) {
                        const array = &init_expr.data.array_expr;
                        const len_str = try std.fmt.allocPrint(self.allocator, "{d}", .{array.elements.len});
                        defer self.allocator.free(len_str);
                        try self.emit("[");
                        try self.emit(len_str);
                        try self.emit("]");
                    }
                }

                // Emit initializer if present
                if (decl.init) |init_expr| {
                    try self.emit(" = ");
                    try self.emitExpression(init_expr);
                    // Note: incref for variable-to-variable copy (let x = y) is handled
                    // by DRC and emitted via emitDrcOpsForLine(.after) after this statement
                }

                try self.emit(";\n");

                // Cleanup any hoisted temporaries after this declaration
                try self.emitTempCleanup();
            }
        }
    }

    /// Check if an object literal contains variable references
    fn objectHasVariableReferences(self: *Self, node: *ast.Node) bool {
        _ = self;
        if (node.kind != .object_expr) return false;

        const obj = &node.data.object_expr;
        for (obj.properties) |prop| {
            switch (prop) {
                .property => |p| {
                    if (p.value.kind == .identifier) return true;
                    if (p.value.kind == .member_expr) return true;
                },
                .spread => return true,
            }
        }
        return false;
    }

    /// Emit expression statement: expr;
    fn emitExpressionStmt(self: *Self, node: *ast.Node) !void {
        const expr = node.data.expression_stmt;

        // Collect and hoist nested RC-returning calls before the main expression
        // This handles patterns like: printMessage(makeGreeting("World"))
        // where makeGreeting returns an owned string that needs cleanup
        var nested_calls = std.ArrayList(*ast.Node).init(self.allocator);
        defer nested_calls.deinit();
        try self.collectNestedRcCalls(expr, &nested_calls);

        if (nested_calls.items.len > 0) {
            // CRITICAL: Reverse to process innermost calls first
            // For f(g(h(x))), collector produces [g, h] (outer-first)
            // We need [h, g] so h is hoisted before g references it
            std.mem.reverse(*ast.Node, nested_calls.items);
            try self.hoistNestedCalls(nested_calls.items);
        }

        // Map console.log to printf
        if (expr.kind == .call_expr) {
            const call = &expr.data.call_expr;
            if (call.callee.kind == .member_expr) {
                const member = &call.callee.data.member_expr;
                if (member.object.kind == .identifier and
                    std.mem.eql(u8, member.object.data.identifier, "console"))
                {
                    // Note: StringBuilder finalization is handled by checkAndFinalizeStringBuildersForStmt
                    // before this function is called, so we can emit directly
                    try self.emitConsoleCall(call);
                    // Emit cleanup for any hoisted temporaries
                    try self.emitTempCleanup();
                    return;
                }
            }
        }

        // Check for StringBuilder concat pattern: result = result + x
        if (self.isStringBuilderConcatExpr(expr)) |concat_info| {
            try self.emitStringBuilderConcat(concat_info.var_name, concat_info.append_value);
            // Emit cleanup for any hoisted temporaries
            try self.emitTempCleanup();
            return;
        }

        // Note: StringBuilder finalization is handled by checkAndFinalizeStringBuildersForStmt
        // before this function is called
        // If we hoisted temps, we need indent for main expression (previous line was temp decl)
        // If no temps, block already emitted indent
        if (nested_calls.items.len > 0) {
            try self.emitIndent();
        }
        try self.emitExpression(expr);
        try self.emit(";\n");

        // Emit cleanup for any hoisted temporaries
        try self.emitTempCleanup();
    }

    /// Emit a block statement
    fn emitBlockStmt(self: *Self, node: *ast.Node) !void {
        const block = &node.data.block_stmt;
        const block_start_line = node.location.start.line;

        try self.emit("{\n");
        self.indent_level += 1;

        // Emit statements in the block
        for (block.statements) |stmt| {
            const stmt_line = stmt.location.start.line;
            self.current_line = stmt_line;

            // Emit DRC ops that should come BEFORE this statement
            // This includes decref for reassignment old values
            // Skip scope_exit ops - they're emitted at block end
            try self.emitDrcOpsForLineFiltered(stmt_line, .before, true);

            try self.emitIndent();
            try self.emitNode(stmt);

            // Emit DRC ops that should come AFTER this statement
            // This includes incref for variable copies (let x = y)
            try self.emitDrcOpsForLine(stmt_line, .after);
        }

        // Update current_line to block's end for scope exit ops
        const block_end_line = if (block.statements.len > 0)
            block.statements[block.statements.len - 1].location.start.line
        else
            block_start_line;
        self.current_line = block_end_line;

        // Check if block ends with return/break/continue (control flow exit)
        // If so, skip scope cleanup - return statement handles its own cleanup
        const ends_with_control_flow = if (block.statements.len > 0)
            switch (block.statements[block.statements.len - 1].kind) {
                .return_stmt, .break_stmt, .continue_stmt => true,
                else => false,
            }
        else
            false;

        // Emit scope cleanup from DRC (if not already handled by control flow)
        // Note: Scope cleanup ops have position=.before, so they won't duplicate
        // the per-statement .after ops
        if (!ends_with_control_flow) {
            try self.emitDrcOpsForLine(block_end_line, .before);
        }

        self.indent_level -= 1;
        try self.emitIndent();
        try self.emit("}\n");
    }

    /// Emit an if statement
    fn emitIfStmt(self: *Self, node: *ast.Node) !void {
        const if_stmt = &node.data.if_stmt;

        try self.emit("if (");
        try self.emitExpression(if_stmt.condition);
        try self.emit(") ");

        // Emit consequent (always present)
        if (if_stmt.consequent.kind == .block_stmt) {
            try self.emitBlockStmt(if_stmt.consequent);
        } else {
            try self.emit("{\n");
            self.indent_level += 1;
            try self.emitIndent();
            try self.emitNode(if_stmt.consequent);
            self.indent_level -= 1;
            try self.emitIndent();
            try self.emit("}\n");
        }

        // Emit alternate (optional else)
        if (if_stmt.alternate) |alternate| {
            try self.emitIndent();
            try self.emit("else ");

            if (alternate.kind == .if_stmt) {
                // else if - don't add extra braces
                try self.emitIfStmt(alternate);
            } else if (alternate.kind == .block_stmt) {
                try self.emitBlockStmt(alternate);
            } else {
                try self.emit("{\n");
                self.indent_level += 1;
                try self.emitIndent();
                try self.emitNode(alternate);
                self.indent_level -= 1;
                try self.emitIndent();
                try self.emit("}\n");
            }
        }
    }

    /// Emit a while loop
    fn emitWhileStmt(self: *Self, node: *ast.Node) !void {
        const while_stmt = &node.data.while_stmt;

        try self.emit("while (");
        try self.emitExpression(while_stmt.condition);
        try self.emit(") ");

        if (while_stmt.body.kind == .block_stmt) {
            try self.emitBlockStmt(while_stmt.body);
        } else {
            try self.emit("{\n");
            self.indent_level += 1;
            try self.emitIndent();
            try self.emitNode(while_stmt.body);
            self.indent_level -= 1;
            try self.emitIndent();
            try self.emit("}\n");
        }
    }

    /// Emit a for loop
    fn emitForStmt(self: *Self, node: *ast.Node) !void {
        const for_stmt = &node.data.for_stmt;

        try self.emit("for (");

        // Init clause
        if (for_stmt.init) |init_node| {
            // For init, we emit the variable declaration or expression without trailing semicolon/newline
            if (init_node.kind == .variable_stmt) {
                const var_stmt = &init_node.data.variable_stmt;
                for (var_stmt.declarations, 0..) |decl, i| {
                    if (i > 0) try self.emit(", ");

                    // Emit type - refine generic 'number' type for integer literals
                    if (decl.type) |typ| {
                        // If type is generic 'number' and init is integer literal, use int
                        if (typ.kind == .number and decl.init != null) {
                            const init_expr = decl.init.?;
                            if (init_expr.kind == .number_literal) {
                                const num = init_expr.data.number_literal;
                                if (@floor(num) == num and num >= -2147483648 and num <= 2147483647) {
                                    try self.emit("int");
                                } else {
                                    try self.emit("double");
                                }
                            } else {
                                try self.emitType(typ);
                            }
                        } else {
                            try self.emitType(typ);
                        }
                    } else if (decl.init) |init_expr| {
                        // No explicit type - try to infer from initializer
                        if (init_expr.type) |inferred_type| {
                            try self.emitType(inferred_type);
                        } else if (init_expr.kind == .number_literal) {
                            const num = init_expr.data.number_literal;
                            if (@floor(num) == num and num >= -2147483648 and num <= 2147483647) {
                                try self.emit("int");
                            } else {
                                try self.emit("double");
                            }
                        } else {
                            try self.emit("double");
                        }
                    } else {
                        try self.emit("double");
                    }

                    try self.emit(" ");
                    try self.emit(decl.name);
                    if (decl.init) |init_expr| {
                        try self.emit(" = ");
                        try self.emitExpression(init_expr);
                    }
                }
            } else {
                try self.emitExpression(init_node);
            }
        }
        try self.emit("; ");

        // Condition clause
        if (for_stmt.condition) |cond| {
            try self.emitExpression(cond);
        }
        try self.emit("; ");

        // Update clause
        if (for_stmt.update) |update| {
            try self.emitExpression(update);
        }
        try self.emit(") ");

        // Body
        if (for_stmt.body.kind == .block_stmt) {
            try self.emitBlockStmt(for_stmt.body);
        } else {
            try self.emit("{\n");
            self.indent_level += 1;
            try self.emitIndent();
            try self.emitNode(for_stmt.body);
            self.indent_level -= 1;
            try self.emitIndent();
            try self.emit("}\n");
        }
    }

    /// Emit a break statement
    fn emitBreakStmt(self: *Self) !void {
        try self.emit("break;\n");
    }

    /// Emit a continue statement
    fn emitContinueStmt(self: *Self) !void {
        try self.emit("continue;\n");
    }

    /// Emit a return statement
    fn emitReturnStmt(self: *Self, node: *ast.Node) !void {
        const ret = &node.data.return_stmt;

        // Note: StringBuilder finalization is handled by checkAndFinalizeStringBuildersForStmt
        // before this function is called

        // Determine if return argument is safe to evaluate after cleanup
        // Safe: identifiers (cleanup skips returned var), literals (no dependencies)
        // Unsafe: expressions using variables that may be freed during cleanup
        const is_safe_after_cleanup = if (ret.argument) |arg|
            arg.kind == .identifier or
                arg.kind == .number_literal or
                arg.kind == .string_literal or
                arg.kind == .boolean_literal or
                arg.kind == .null_literal
        else
            true; // No argument is safe

        // Get the returned variable name (if returning a simple identifier)
        const returned_var: ?[]const u8 = if (ret.argument) |arg|
            if (arg.kind == .identifier) arg.data.identifier else null
        else
            null;

        // Check if we have any cleanup to emit (DRC or StringBuilder)
        const has_drc_cleanup = self.hasAnyCleanup();
        const has_sb_cleanup = self.stringbuilder_vars.count() > 0;
        const needs_cleanup = has_drc_cleanup or has_sb_cleanup;

        // CRITICAL: For complex expressions that use variables (not identifiers/literals),
        // we must evaluate the expression BEFORE cleanup to avoid use-after-free.
        // Example: `return part1 + part2` - we need part1/part2 valid during concat
        if (needs_cleanup and !is_safe_after_cleanup) {
            // Complex expression: evaluate to temp, cleanup, return temp
            try self.emit("\n"); // End current line

            // Evaluate expression into a temporary variable
            try self.emitIndent();
            // Infer return type from the expression or use generic pointer
            try self.emit("void* _return_value = (void*)(");
            if (ret.argument) |arg| {
                try self.emitExpression(arg);
            }
            try self.emit(");\n");

            // Now emit cleanup (variables are still valid, but we have our copy)
            try self.emitAllScopeCleanup();
            try self.emitStringBuilderCleanupForReturn(returned_var);

            // Return the saved value
            try self.emitIndent();
            try self.emit("return _return_value;\n");
        } else if (needs_cleanup) {
            // Simple identifier or no argument: existing logic is safe
            // The scope cleanup already skips the returned variable
            try self.emit("\n"); // End the current (empty) line
            try self.emitAllScopeCleanup();
            try self.emitStringBuilderCleanupForReturn(returned_var);
            try self.emitIndent(); // Indent for the return statement

            try self.emit("return");
            if (ret.argument) |arg| {
                try self.emit(" ");
                try self.emitExpression(arg);
            }
            try self.emit(";\n");
        } else {
            // No cleanup needed - simple return
            try self.emit("return");
            if (ret.argument) |arg| {
                try self.emit(" ");
                try self.emitExpression(arg);
            }
            try self.emit(";\n");
        }
    }

    /// Emit StringBuilder cleanup for return statements
    /// Frees buffers but doesn't decref the returned variable
    fn emitStringBuilderCleanupForReturn(self: *Self, returned_var: ?[]const u8) !void {
        if (self.stringbuilder_vars.count() == 0) return;

        // Step 1: Free StringBuilder buffers
        var iter = self.stringbuilder_vars.keyIterator();
        while (iter.next()) |key_ptr| {
            try self.emitIndent();
            try self.emit("ms_sb_free(&_sb_");
            try self.emit(key_ptr.*);
            try self.emit(");\n");
        }

        // Step 2: Decref all built strings EXCEPT the one being returned
        iter = self.stringbuilder_vars.keyIterator();
        while (iter.next()) |key_ptr| {
            const name = key_ptr.*;
            // Skip the returned variable - caller takes ownership
            if (returned_var) |rv| {
                if (std.mem.eql(u8, name, rv)) continue;
            }
            // Only decref if it was finalized
            if (self.stringbuilder_finalized.contains(name)) {
                try self.emitIndent();
                try self.emit("ms_decref(");
                try self.emit(name);
                try self.emit(");\n");
            }
        }
    }

    /// Check if any scope has variables that need cleanup
    fn hasAnyCleanup(self: *Self) bool {
        // Check DRC for ops at current line
        const ops = self.drc.getOpsForLine(self.current_line);
        return ops.len > 0;
    }

    /// Emit cleanup for all scopes (used for early return)
    fn emitAllScopeCleanup(self: *Self) !void {
        // Emit all DRC cleanup ops for current line
        try self.emitAllDrcOpsForLine(self.current_line);
    }

    /// Emit console.log/error/warn as printf
    fn emitConsoleCall(self: *Self, call: *const node_mod.CallExpr) !void {
        // console.log(...) → printf("...\n")

        // Build format string
        var format_parts = std.ArrayList(u8).init(self.allocator);
        defer format_parts.deinit();

        try format_parts.appendSlice("\"");
        for (call.arguments, 0..) |arg, i| {
            if (i > 0) try format_parts.appendSlice(" ");

            // Determine format specifier based on argument type
            if (arg.type) |arg_type| {
                switch (arg_type.kind) {
                    .number, .float32, .float64 => try format_parts.appendSlice("%g"),
                    .int32 => try format_parts.appendSlice("%d"),
                    .int64 => try format_parts.appendSlice("%lld"),
                    .string => try format_parts.appendSlice("%s"),
                    .boolean => try format_parts.appendSlice("%d"),
                    .object => {
                        // For objects, we'll print field by field
                        const obj_type = arg_type.data.object;
                        try format_parts.appendSlice("{");
                        for (obj_type.properties, 0..) |prop, j| {
                            if (j > 0) try format_parts.appendSlice(", ");
                            try format_parts.appendSlice(prop.name);
                            try format_parts.appendSlice(": ");
                            // Add format specifier based on property type
                            switch (prop.type.kind) {
                                .number, .float32, .float64 => try format_parts.appendSlice("%g"),
                                .int32 => try format_parts.appendSlice("%d"),
                                .int64 => try format_parts.appendSlice("%lld"),
                                .string => try format_parts.appendSlice("%s"),
                                .boolean => try format_parts.appendSlice("%d"),
                                else => try format_parts.appendSlice("%g"),
                            }
                        }
                        try format_parts.appendSlice("}");
                    },
                    else => {
                        // For unknown types, default to %g (number) as most common case
                        // %p is rarely what we want for user-facing output
                        try format_parts.appendSlice("%g");
                    },
                }
            } else {
                // Default to number format (common case)
                try format_parts.appendSlice("%g");
            }
        }
        try format_parts.appendSlice("\\n\"");

        // Emit printf call
        try self.emit("printf(");
        try self.emit(format_parts.items);

        // Emit arguments
        for (call.arguments) |arg| {
            if (arg.type) |arg_type| {
                if (arg_type.kind == .object) {
                    // For objects, emit each property value
                    const obj_type = arg_type.data.object;
                    for (obj_type.properties) |prop| {
                        try self.emit(", ");
                        try self.emitExpression(arg);
                        try self.emit(".");
                        try self.emit(prop.name);
                    }
                    continue;
                }
                // Strings: wrap with ms_string_cstr() to get char* for printf
                if (arg_type.kind == .string) {
                    try self.emit(", ms_string_cstr(");
                    try self.emitExpression(arg);
                    try self.emit(")");
                    continue;
                }
                // Booleans: emit directly without cast
                if (arg_type.kind == .boolean) {
                    try self.emit(", ");
                    try self.emitExpression(arg);
                    continue;
                }
                // Integers: emit directly (format string uses %d/%lld)
                if (arg_type.kind == .int32 or arg_type.kind == .int64) {
                    try self.emit(", ");
                    try self.emitExpression(arg);
                    continue;
                }
                // Floats/number: cast to double for %g format
                if (arg_type.kind == .number or arg_type.kind == .float32 or arg_type.kind == .float64) {
                    try self.emit(", (double)(");
                    try self.emitExpression(arg);
                    try self.emit(")");
                    continue;
                }
            }
            // For unknown types, cast identifiers to double for safety with %g format
            // (most unknown types are numbers in practice)
            if (arg.kind == .identifier) {
                try self.emit(", (double)(");
                try self.emitExpression(arg);
                try self.emit(")");
                continue;
            }
            try self.emit(", ");
            try self.emitExpression(arg);
        }

        try self.emit(");\n");
    }

    /// Emit Math.* builtin calls mapped to C <math.h> functions
    /// Math.floor(x) → floor(x), Math.sqrt(x) → sqrt(x), etc.
    fn emitMathCall(self: *Self, method: []const u8, args: []const *ast.Node) !void {
        // Map TypeScript Math methods to C <math.h> functions
        const c_func: ?[]const u8 = if (std.mem.eql(u8, method, "floor")) "floor"
            else if (std.mem.eql(u8, method, "ceil")) "ceil"
            else if (std.mem.eql(u8, method, "round")) "round"
            else if (std.mem.eql(u8, method, "trunc")) "trunc"
            else if (std.mem.eql(u8, method, "sqrt")) "sqrt"
            else if (std.mem.eql(u8, method, "cbrt")) "cbrt"
            else if (std.mem.eql(u8, method, "abs")) "fabs"
            else if (std.mem.eql(u8, method, "sin")) "sin"
            else if (std.mem.eql(u8, method, "cos")) "cos"
            else if (std.mem.eql(u8, method, "tan")) "tan"
            else if (std.mem.eql(u8, method, "asin")) "asin"
            else if (std.mem.eql(u8, method, "acos")) "acos"
            else if (std.mem.eql(u8, method, "atan")) "atan"
            else if (std.mem.eql(u8, method, "atan2")) "atan2"
            else if (std.mem.eql(u8, method, "sinh")) "sinh"
            else if (std.mem.eql(u8, method, "cosh")) "cosh"
            else if (std.mem.eql(u8, method, "tanh")) "tanh"
            else if (std.mem.eql(u8, method, "exp")) "exp"
            else if (std.mem.eql(u8, method, "expm1")) "expm1"
            else if (std.mem.eql(u8, method, "log")) "log"
            else if (std.mem.eql(u8, method, "log10")) "log10"
            else if (std.mem.eql(u8, method, "log2")) "log2"
            else if (std.mem.eql(u8, method, "log1p")) "log1p"
            else if (std.mem.eql(u8, method, "pow")) "pow"
            else if (std.mem.eql(u8, method, "hypot")) "hypot"
            else if (std.mem.eql(u8, method, "sign")) "copysign"  // sign(x) → copysign(1.0, x)
            else if (std.mem.eql(u8, method, "min")) "fmin"
            else if (std.mem.eql(u8, method, "max")) "fmax"
            else if (std.mem.eql(u8, method, "random")) "((double)rand() / RAND_MAX)"  // Simple random [0,1)
            else null;

        if (c_func) |func| {
            // Special case: Math.sign(x) → copysign(1.0, x)
            if (std.mem.eql(u8, method, "sign")) {
                try self.emit("copysign(1.0, ");
                if (args.len > 0) {
                    try self.emitExpression(args[0]);
                }
                try self.emit(")");
                return;
            }

            // Special case: Math.random() has no arguments
            if (std.mem.eql(u8, method, "random")) {
                try self.emit("((double)rand() / RAND_MAX)");
                return;
            }

            // Standard case: func(arg1, arg2, ...)
            try self.emit(func);
            try self.emit("(");
            for (args, 0..) |arg, i| {
                if (i > 0) try self.emit(", ");
                try self.emitExpression(arg);
            }
            try self.emit(")");
        } else {
            // Unknown Math method - emit as-is (will cause C compiler error)
            try self.emit("/* unsupported: Math.");
            try self.emit(method);
            try self.emit(" */ 0");
        }
    }

    /// Emit an expression (recursive)
    fn emitExpression(self: *Self, node: *ast.Node) anyerror!void {
        switch (node.kind) {
            .number_literal => {
                const value = node.data.number_literal;
                const num_str = try std.fmt.allocPrint(self.allocator, "{d}", .{value});
                defer self.allocator.free(num_str);
                try self.emit(num_str);
            },
            .string_literal => {
                const literal = node.data.string_literal;
                if (self.enable_interning) {
                    // Use interned string pool for deduplication and performance
                    const intern_id = try self.registerStringLiteral(literal);
                    try self.emit("ms_intern_get(&_str_");
                    var id_buf: [16]u8 = undefined;
                    const id_str = std.fmt.bufPrint(&id_buf, "{d}", .{intern_id}) catch "0";
                    try self.emit(id_str);
                    try self.emit(")");
                } else {
                    // Fallback: wrap string literals in ms_string_from_cstr for ORC management
                    try self.emit("ms_string_from_cstr(\"");
                    try self.emit(literal);
                    try self.emit("\")");
                }
            },
            .boolean_literal => {
                if (node.data.boolean_literal) {
                    try self.emit("true");
                } else {
                    try self.emit("false");
                }
            },
            .null_literal => {
                try self.emit("NULL");
            },
            .identifier => {
                const name = node.data.identifier;
                // For StringBuilder variables that have been finalized, use the plain name
                // (finalization creates: msString* varname = ms_sb_build(&_sb_varname))
                // For non-finalized StringBuilder vars, this is an error - should have been
                // finalized by finalizeStringBuildersInExpr before this expression was emitted
                try self.emit(name);
            },
            .binary_expr => {
                const binary = &node.data.binary_expr;

                // Try constant folding: if both operands are number literals, evaluate at compile time
                if (binary.left.kind == .number_literal and binary.right.kind == .number_literal) {
                    const left_val = binary.left.data.number_literal;
                    const right_val = binary.right.data.number_literal;
                    const result: ?f64 = switch (binary.op) {
                        .add => left_val + right_val,
                        .sub => left_val - right_val,
                        .mul => left_val * right_val,
                        .div => if (right_val != 0) left_val / right_val else null,
                        .mod => if (right_val != 0) @mod(left_val, right_val) else null,
                        else => null, // Comparison ops, etc. - not folded
                    };
                    if (result) |val| {
                        // Emit the constant value directly
                        var buf: [64]u8 = undefined;
                        // Check if result is integer
                        if (@floor(val) == val and val >= -2147483648 and val <= 2147483647) {
                            const int_val: i64 = @intFromFloat(val);
                            const s = std.fmt.bufPrint(&buf, "{d}", .{int_val}) catch "0";
                            try self.emit(s);
                        } else {
                            const s = std.fmt.bufPrint(&buf, "{d}", .{val}) catch "0";
                            try self.emit(s);
                        }
                        return;
                    }
                }

                // Check for string concatenation (+ with string operands)
                if (binary.op == .add) {
                    const left_is_string = if (binary.left.type) |t| t.kind == .string else false;
                    const right_is_string = if (binary.right.type) |t| t.kind == .string else false;
                    const left_is_number = if (binary.left.type) |t|
                        (t.kind == .number or t.kind == .float64 or t.kind == .float32 or
                            t.kind == .int32 or t.kind == .int64)
                    else
                        false;
                    const right_is_number = if (binary.right.type) |t|
                        (t.kind == .number or t.kind == .float64 or t.kind == .float32 or
                            t.kind == .int32 or t.kind == .int64)
                    else
                        false;

                    if (left_is_string and right_is_string) {
                        // string + string → ms_string_concat (ORC-managed)
                        try self.emit("ms_string_concat(");
                        try self.emitExpression(binary.left);
                        try self.emit(", ");
                        try self.emitExpression(binary.right);
                        try self.emit(")");
                        return;
                    } else if (left_is_string and right_is_number) {
                        // string + number → ms_string_concat_num (ORC-managed)
                        try self.emit("ms_string_concat_num(");
                        try self.emitExpression(binary.left);
                        try self.emit(", ");
                        try self.emitExpression(binary.right);
                        try self.emit(")");
                        return;
                    } else if (left_is_number and right_is_string) {
                        // number + string → ms_num_concat_string (ORC-managed)
                        try self.emit("ms_num_concat_string(");
                        try self.emitExpression(binary.left);
                        try self.emit(", ");
                        try self.emitExpression(binary.right);
                        try self.emit(")");
                        return;
                    }
                }

                // Special case: modulo with floating-point types needs fmod()
                if (binary.op == .mod) {
                    const left_is_float = if (binary.left.type) |t|
                        (t.kind == .number or t.kind == .float64 or t.kind == .float32)
                    else
                        true; // Default to float for number type in MetaScript

                    if (left_is_float) {
                        try self.emit("fmod(");
                        try self.emitExpression(binary.left);
                        try self.emit(", ");
                        try self.emitExpression(binary.right);
                        try self.emit(")");
                        return;
                    }
                }

                // Default: regular binary expression
                // Skip outer parens for assignment operators (already lowest precedence)
                const is_assign = binary.op == .assign;

                // ORC: For reference-type assignments to member expressions,
                // emit ms_incref on the right-hand side to prevent use-after-free
                // Example: team.leader = user  ->  ms_incref(user); team->leader = user;
                //
                // CRITICAL: Only incref EXISTING references (identifiers, member expressions).
                // Fresh allocations (string literals, new expressions, call expressions)
                // already have rc=1 and don't need incref - calling emitExpression twice
                // would create TWO separate allocations, leaking the first one.
                if (is_assign and binary.left.kind == .member_expr) {
                    const rhs_is_ref = isOrcReferenceType(binary.right.type);
                    const rhs_is_existing_ref = binary.right.kind == .identifier or
                        binary.right.kind == .member_expr;
                    if (rhs_is_ref and rhs_is_existing_ref) {
                        // Emit incref for existing reference being assigned
                        try self.emit("ms_incref(");
                        try self.emitExpression(binary.right);
                        try self.emit(");\n");
                        try self.emitIndent();
                    }
                }

                if (!is_assign) try self.emit("(");
                try self.emitExpression(binary.left);
                try self.emit(" ");
                try self.emit(binaryOpToString(binary.op));
                try self.emit(" ");
                try self.emitExpression(binary.right);
                if (!is_assign) try self.emit(")");
            },
            .unary_expr => {
                const unary = &node.data.unary_expr;
                try self.emit(unaryOpToString(unary.op));
                try self.emitExpression(unary.argument);
            },
            .object_expr => {
                try self.emitObjectLiteral(node);
            },
            .member_expr => {
                const member = &node.data.member_expr;

                // Check for Math.* constants: Math.PI → M_PI, Math.E → M_E
                if (member.object.kind == .identifier and
                    std.mem.eql(u8, member.object.data.identifier, "Math") and
                    member.property.kind == .identifier)
                {
                    const prop = member.property.data.identifier;
                    const c_const: ?[]const u8 = if (std.mem.eql(u8, prop, "PI")) "M_PI"
                        else if (std.mem.eql(u8, prop, "E")) "M_E"
                        else if (std.mem.eql(u8, prop, "LN2")) "M_LN2"
                        else if (std.mem.eql(u8, prop, "LN10")) "M_LN10"
                        else if (std.mem.eql(u8, prop, "LOG2E")) "M_LOG2E"
                        else if (std.mem.eql(u8, prop, "LOG10E")) "M_LOG10E"
                        else if (std.mem.eql(u8, prop, "SQRT2")) "M_SQRT2"
                        else if (std.mem.eql(u8, prop, "SQRT1_2")) "M_SQRT1_2"
                        else null;

                    if (c_const) |c| {
                        try self.emit(c);
                        return;
                    }
                    // Fall through for unknown Math properties (might be method without call)
                }

                try self.emitExpression(member.object);
                if (member.computed) {
                    try self.emit("[");
                    try self.emitExpression(member.property);
                    try self.emit("]");
                } else {
                    // Use -> for pointer types (class instances allocated with ms_alloc)
                    // Use . for stack-allocated value types (object literals)
                    const is_pointer = if (member.object.type) |obj_type| blk: {
                        // type_reference means it's a named type (like Point)
                        if (obj_type.kind == .type_reference) {
                            break :blk true;
                        }
                        // ref type (explicit ref annotation)
                        if (obj_type.kind == .ref) {
                            break :blk true;
                        }
                        // object type with a name = class instance (heap pointer)
                        // object type without name = object literal (stack value)
                        if (obj_type.kind == .object) {
                            if (obj_type.data.object.name != null) {
                                break :blk true; // Class instance -> pointer
                            }
                        }
                        break :blk false;
                    } else false;
                    if (is_pointer) {
                        try self.emit("->");
                    } else {
                        try self.emit(".");
                    }
                    try self.emitExpression(member.property);
                }
            },
            .call_expr => {
                // Check if this call was hoisted to a temporary
                // (for nested calls like f(g(x)) where g(x) returns RC type)
                if (self.hoisted_calls.get(node)) |temp_name| {
                    try self.emit(temp_name);
                    return;
                }

                const call = &node.data.call_expr;

                // Check for Math.* builtin calls: Math.floor(x) → floor(x)
                if (call.callee.kind == .member_expr) {
                    const member = &call.callee.data.member_expr;
                    if (member.object.kind == .identifier and
                        std.mem.eql(u8, member.object.data.identifier, "Math"))
                    {
                        if (member.property.kind == .identifier) {
                            try self.emitMathCall(member.property.data.identifier, call.arguments);
                            return;
                        }
                    }
                }

                // Check for method call on class instance: obj.method(args) → ClassName_method(obj, args)
                if (call.callee.kind == .member_expr) {
                    const member = &call.callee.data.member_expr;
                    // Check if the object has a named object type (class instance)
                    if (member.object.type) |obj_type| {
                        if (obj_type.kind == .object) {
                            if (obj_type.data.object.name) |class_name| {
                                // This is a method call on a class instance
                                // Emit: ClassName_methodName(instance, args...)
                                try self.emit(class_name);
                                try self.emit("_");
                                try self.emitExpression(member.property);
                                try self.emit("(");
                                try self.emitExpression(member.object);
                                for (call.arguments) |arg| {
                                    try self.emit(", ");
                                    try self.emitExpression(arg);
                                }
                                try self.emit(")");
                                return;
                            }
                        }
                    }
                }

                // Default: regular function call
                try self.emitExpression(call.callee);
                try self.emit("(");
                for (call.arguments, 0..) |arg, i| {
                    if (i > 0) try self.emit(", ");
                    try self.emitExpression(arg);
                }
                try self.emit(")");
            },
            .array_expr => {
                const array = &node.data.array_expr;
                try self.emit("{");
                for (array.elements, 0..) |elem, i| {
                    if (i > 0) try self.emit(", ");
                    try self.emitExpression(elem);
                }
                try self.emit("}");
            },
            .new_expr => {
                // Emit: ClassName_new() - calls the generated constructor function
                // Uses standard C99 instead of GCC statement expressions
                const new_e = &node.data.new_expr;
                if (new_e.callee.kind == .identifier) {
                    const class_name = new_e.callee.data.identifier;
                    try self.emit(class_name);
                    try self.emit("_new()");
                } else {
                    try self.emit("/* unsupported: new with non-identifier */");
                }
            },
            else => {
                // Unsupported expression type
                try self.emit("/* unsupported: ");
                try self.emit(@tagName(node.kind));
                try self.emit(" */");
            },
        }
    }

    /// Emit object literal as C compound literal
    fn emitObjectLiteral(self: *Self, node: *ast.Node) !void {
        const obj = &node.data.object_expr;

        // For now, emit as struct initializer
        // Type annotation would help here, but we'll use generic struct syntax
        try self.emit("{ ");

        for (obj.properties, 0..) |prop, i| {
            if (i > 0) try self.emit(", ");

            switch (prop) {
                .property => |p| {
                    // .field_name = value
                    try self.emit(".");
                    if (p.key.kind == .identifier) {
                        try self.emit(p.key.data.identifier);
                    } else {
                        try self.emit("field");
                    }
                    try self.emit(" = ");
                    try self.emitExpression(p.value);
                },
                .spread => {
                    // Spreads should have been normalized away by now
                    try self.emit("/* spread should be normalized */");
                },
            }
        }

        try self.emit(" }");
    }

    /// Emit a type as C type
    fn emitType(self: *Self, typ: ?*types_mod.Type) !void {
        if (typ == null) {
            try self.emit("void");
            return;
        }

        const t = typ.?;
        switch (t.kind) {
            .int8 => try self.emit("int8_t"),
            .int16 => try self.emit("int16_t"),
            .int32 => try self.emit("int32_t"),
            .int64 => try self.emit("int64_t"),
            .uint8 => try self.emit("uint8_t"),
            .uint16 => try self.emit("uint16_t"),
            .uint32 => try self.emit("uint32_t"),
            .uint64 => try self.emit("uint64_t"),
            .float32 => try self.emit("float"),
            .float64 => try self.emit("double"),
            .number => try self.emit("double"),
            .string => try self.emit("msString*"),
            .boolean => try self.emit("bool"),
            .void => try self.emit("void"),
            .type_reference => {
                // Type references (e.g., string, number, User)
                const type_ref = t.data.type_reference;

                // Check if resolved to actual type
                if (type_ref.resolved) |resolved| {
                    return try self.emitType(resolved);
                }

                // Map common type names to C types
                const name = type_ref.name;
                if (std.mem.eql(u8, name, "string")) {
                    try self.emit("msString*");
                } else if (std.mem.eql(u8, name, "number")) {
                    try self.emit("double");
                } else if (std.mem.eql(u8, name, "boolean")) {
                    try self.emit("bool");
                } else if (std.mem.eql(u8, name, "void")) {
                    try self.emit("void");
                } else if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "int32")) {
                    try self.emit("int32_t");
                } else if (std.mem.eql(u8, name, "int8")) {
                    try self.emit("int8_t");
                } else if (std.mem.eql(u8, name, "int16")) {
                    try self.emit("int16_t");
                } else if (std.mem.eql(u8, name, "int64")) {
                    try self.emit("int64_t");
                } else if (std.mem.eql(u8, name, "uint8")) {
                    try self.emit("uint8_t");
                } else if (std.mem.eql(u8, name, "uint16")) {
                    try self.emit("uint16_t");
                } else if (std.mem.eql(u8, name, "uint32")) {
                    try self.emit("uint32_t");
                } else if (std.mem.eql(u8, name, "uint64")) {
                    try self.emit("uint64_t");
                } else if (std.mem.eql(u8, name, "float") or std.mem.eql(u8, name, "float32")) {
                    try self.emit("float");
                } else if (std.mem.eql(u8, name, "double") or std.mem.eql(u8, name, "float64")) {
                    try self.emit("double");
                } else {
                    // Assume it's a custom type (struct pointer)
                    // Use 'struct Name*' for forward-compatibility in struct definitions
                    try self.emit("struct ");
                    try self.emit(name);
                    try self.emit("*");
                }
            },
            .object => {
                const obj = t.data.object;

                // If object has a name, it's a class instance (emit as ClassName*)
                if (obj.name) |class_name| {
                    try self.emit(class_name);
                    try self.emit("*");
                    return;
                }

                // Anonymous object: emit as anonymous struct
                try self.emit("struct { ");

                // Emit properties as struct fields
                for (obj.properties, 0..) |prop, i| {
                    if (i > 0) try self.emit("; ");
                    try self.emitType(prop.type);
                    try self.emit(" ");
                    try self.emit(prop.name);
                }

                if (obj.properties.len > 0) {
                    try self.emit("; ");
                }

                try self.emit("}");
            },
            .array => {
                // For arrays, emit just the element type
                // The array brackets will be emitted in variable declaration
                const elem_type = t.data.array;
                try self.emitType(elem_type);
            },
            else => try self.emit("void*"), // Fallback
        }
    }

    /// Low-level emit helpers
    fn emit(self: *Self, text: []const u8) !void {
        try self.output.appendSlice(text);
    }

    fn emitIndent(self: *Self) !void {
        for (0..self.indent_level) |_| {
            try self.emit("    ");
        }
    }

    fn emitNewline(self: *Self) !void {
        try self.emit("\n");
    }
};
