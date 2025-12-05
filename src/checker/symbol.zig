/// Symbol Table for Metascript Type Checker
///
/// Tracks scopes and bindings for variables, functions, classes, interfaces.
/// Used by the type checker to resolve identifiers and check types.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const types = @import("../ast/types.zig");
const location = @import("../ast/location.zig");

/// Kinds of symbols we track
pub const SymbolKind = enum {
    variable,
    function,
    class,
    interface,
    type_alias,
    parameter,
    property,
    method,
    macro, // Compile-time macro function
};

/// A symbol represents a named entity in the program
pub const Symbol = struct {
    name: []const u8,
    kind: SymbolKind,
    type: ?*types.Type = null,
    location: location.SourceLocation,
    /// For variables: is it const or let?
    mutable: bool = false,
    /// For class members: visibility
    visibility: Visibility = .public,
    /// The AST node that declared this symbol (if any)
    decl_node: ?*ast.Node = null,
    /// JSDoc comment preceding this declaration (if any)
    doc_comment: ?[]const u8 = null,

    pub const Visibility = enum {
        public,
        private,
        protected,
    };

    pub fn init(
        name: []const u8,
        kind: SymbolKind,
        loc: location.SourceLocation,
    ) Symbol {
        return .{
            .name = name,
            .kind = kind,
            .type = null,
            .location = loc,
            .mutable = false,
            .visibility = .public,
            .decl_node = null,
            .doc_comment = null,
        };
    }
};

/// A scope contains symbols and has a parent scope
pub const Scope = struct {
    symbols: std.StringHashMap(Symbol),
    parent: ?*Scope,
    kind: ScopeKind,
    /// For function scopes: the return type
    return_type: ?*types.Type,
    /// Source location of this scope (start = open brace, end = close brace)
    /// Used by lookupAtPosition to determine if cursor is within this scope
    start_location: ?location.SourceLocation,

    pub const ScopeKind = enum {
        global,
        function,
        block,
        class,
        loop, // for break/continue validation
    };

    pub fn init(allocator: std.mem.Allocator, parent: ?*Scope, kind: ScopeKind) Scope {
        return .{
            .symbols = std.StringHashMap(Symbol).init(allocator),
            .parent = parent,
            .kind = kind,
            .return_type = null,
            .start_location = null,
        };
    }

    /// Check if a position (line, column) is within this scope's boundaries
    /// Returns true if:
    /// - Scope has no location (backward compatibility - assume position is within)
    /// - Position is between scope's start and end locations
    pub fn containsPosition(self: *const Scope, line: u32, column: u32) bool {
        const loc = self.start_location orelse return true; // No boundary = assume within

        const start_line = loc.start.line;
        const start_col = loc.start.column;
        const end_line = loc.end.line;
        const end_col = loc.end.column;

        // Check if position is after start
        const after_start = (line > start_line) or (line == start_line and column >= start_col);
        // Check if position is before end
        const before_end = (line < end_line) or (line == end_line and column <= end_col);

        return after_start and before_end;
    }

    pub fn deinit(self: *Scope) void {
        self.symbols.deinit();
    }

    /// Define a new symbol in this scope
    pub fn define(self: *Scope, symbol: Symbol) !void {
        if (self.symbols.contains(symbol.name)) {
            return error.SymbolAlreadyDefined;
        }
        try self.symbols.put(symbol.name, symbol);
    }

    /// Look up a symbol in this scope only (not parent)
    pub fn lookupLocal(self: *Scope, name: []const u8) ?Symbol {
        return self.symbols.get(name);
    }

    /// Look up a symbol in this scope and all parent scopes
    pub fn lookup(self: *Scope, name: []const u8) ?Symbol {
        if (self.symbols.get(name)) |symbol| {
            return symbol;
        }
        if (self.parent) |parent| {
            return parent.lookup(name);
        }
        return null;
    }

    /// Update the type of an existing symbol
    pub fn updateType(self: *Scope, name: []const u8, new_type: *types.Type) !void {
        if (self.symbols.getPtr(name)) |symbol_ptr| {
            symbol_ptr.type = new_type;
        } else if (self.parent) |parent| {
            try parent.updateType(name, new_type);
        } else {
            return error.SymbolNotFound;
        }
    }

    /// Check if we're inside a loop (for break/continue validation)
    /// Note: Function boundaries stop the search - a break inside a nested
    /// function cannot target a loop in the enclosing function.
    pub fn isInLoop(self: *Scope) bool {
        if (self.kind == .loop) return true;
        // Stop at function boundaries - break/continue cannot cross functions
        if (self.kind == .function) return false;
        if (self.parent) |parent| {
            return parent.isInLoop();
        }
        return false;
    }

    /// Get the enclosing function's return type (for return statement validation)
    pub fn getFunctionReturnType(self: *Scope) ?*types.Type {
        if (self.kind == .function) {
            return self.return_type;
        }
        if (self.parent) |parent| {
            return parent.getFunctionReturnType();
        }
        return null;
    }
};

/// Symbol Table manages all scopes during type checking
pub const SymbolTable = struct {
    allocator: std.mem.Allocator,
    scopes: std.ArrayList(*Scope),
    current: *Scope,
    global: *Scope,
    type_arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) !SymbolTable {
        var scopes = std.ArrayList(*Scope).init(allocator);
        var type_arena = std.heap.ArenaAllocator.init(allocator);

        // Create global scope
        const global = try allocator.create(Scope);
        global.* = Scope.init(allocator, null, .global);
        try scopes.append(global);

        // Pre-populate with built-in types
        try defineBuiltins(global, &type_arena);

        return .{
            .allocator = allocator,
            .scopes = scopes,
            .current = global,
            .global = global,
            .type_arena = type_arena,
        };
    }

    pub fn deinit(self: *SymbolTable) void {
        for (self.scopes.items) |scope| {
            scope.deinit();
            self.allocator.destroy(scope);
        }
        self.scopes.deinit();
        self.type_arena.deinit();
    }

    /// Enter a new scope
    pub fn enterScope(self: *SymbolTable, kind: Scope.ScopeKind) !void {
        const scope = try self.allocator.create(Scope);
        scope.* = Scope.init(self.allocator, self.current, kind);
        try self.scopes.append(scope);
        self.current = scope;
    }

    /// Enter a new scope with source location boundaries
    /// The location's start marks where the scope begins, end marks where it ends
    /// This enables position-aware symbol lookup to correctly handle shadowing
    pub fn enterScopeWithLocation(self: *SymbolTable, kind: Scope.ScopeKind, loc: location.SourceLocation) !void {
        const scope = try self.allocator.create(Scope);
        scope.* = Scope.init(self.allocator, self.current, kind);
        scope.start_location = loc;
        try self.scopes.append(scope);
        self.current = scope;
    }

    /// Set the current scope's location (for cases where location is known after entering)
    pub fn setCurrentScopeLocation(self: *SymbolTable, loc: location.SourceLocation) void {
        self.current.start_location = loc;
    }

    /// Exit the current scope
    pub fn exitScope(self: *SymbolTable) void {
        if (self.current.parent) |parent| {
            self.current = parent;
        }
    }

    /// Define a symbol in the current scope
    pub fn define(self: *SymbolTable, symbol: Symbol) !void {
        try self.current.define(symbol);
    }

    /// Look up a symbol (searches all parent scopes)
    pub fn lookup(self: *SymbolTable, name: []const u8) ?Symbol {
        return self.current.lookup(name);
    }

    /// Look up a symbol in current scope only
    pub fn lookupLocal(self: *SymbolTable, name: []const u8) ?Symbol {
        return self.current.lookupLocal(name);
    }

    /// Set the return type for the current function scope
    pub fn setReturnType(self: *SymbolTable, return_type: *types.Type) void {
        self.current.return_type = return_type;
    }

    /// Get the return type of the enclosing function
    pub fn getReturnType(self: *SymbolTable) ?*types.Type {
        return self.current.getFunctionReturnType();
    }

    /// Check if we're in a loop
    pub fn isInLoop(self: *SymbolTable) bool {
        return self.current.isInLoop();
    }

    /// Update the type of an existing symbol
    pub fn updateType(self: *SymbolTable, name: []const u8, new_type: *types.Type) !void {
        try self.current.updateType(name, new_type);
    }

    /// Update the type of an existing symbol in ALL scopes
    /// This is needed because type inference (Phase 3) creates new scopes,
    /// but we need to update symbols defined in Phase 1's scopes.
    pub fn updateTypeAll(self: *SymbolTable, name: []const u8, new_type: *types.Type) !void {
        // Search all scopes in reverse order (most recent first = inner scopes)
        var i: usize = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.scopes.items[i].symbols.getPtr(name)) |symbol_ptr| {
                symbol_ptr.type = new_type;
                return;
            }
        }
        return error.SymbolNotFound;
    }

    /// Look up a symbol in ALL scopes (not just current and parents)
    /// This is used by LSP for hover - after type checking, current scope
    /// is back to global, but we need to find symbols in child scopes.
    pub fn lookupAll(self: *SymbolTable, name: []const u8) ?Symbol {
        // Search all scopes in reverse order (most recent first = inner scopes)
        var i: usize = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.scopes.items[i].lookupLocal(name)) |sym| {
                return sym;
            }
        }
        return null;
    }

    /// Look up a symbol at a specific position (line, column)
    /// This is position-aware: it finds the symbol whose declaration is
    /// visible at the given position (declared before, in enclosing scope)
    ///
    /// IMPORTANT: This function respects scope boundaries. A symbol is only
    /// visible if:
    /// 1. It was declared BEFORE the cursor position
    /// 2. The cursor is WITHIN the scope that contains the symbol
    ///
    /// This correctly handles shadowing across nested scopes:
    /// ```
    /// function outer() {
    ///     let x = 1;           // visible at line 10
    ///     function inner() {
    ///         let x = 2;       // NOT visible at line 10 (scope ended)
    ///     }
    ///     return x;            // line 10: finds outer's x, not inner's x
    /// }
    /// ```
    pub fn lookupAtPosition(self: *SymbolTable, name: []const u8, line: u32, column: u32) ?Symbol {
        var best_match: ?Symbol = null;
        var best_line: u32 = 0;

        // Search all scopes for symbols with matching name
        for (self.scopes.items) |scope| {
            // CRITICAL: Only consider symbols in scopes that contain the cursor position
            // This prevents finding variables from inner scopes that have already ended
            if (!scope.containsPosition(line, column)) {
                continue;
            }

            if (scope.lookupLocal(name)) |sym| {
                const sym_line = sym.location.start.line;
                const sym_col = sym.location.start.column;

                // Symbol must be declared BEFORE the cursor position
                // (on an earlier line, or same line but earlier column)
                const is_before = (sym_line < line) or (sym_line == line and sym_col <= column);

                if (is_before) {
                    // Prefer the symbol declared closest to (but before) the cursor
                    // This handles shadowing: inner scope's variable shadows outer
                    if (best_match == null or sym_line > best_line) {
                        best_match = sym;
                        best_line = sym_line;
                    }
                }
            }
        }

        return best_match;
    }
};

/// Define built-in types and functions
fn defineBuiltins(scope: *Scope, type_arena: *std.heap.ArenaAllocator) !void {
    const allocator = type_arena.allocator();

    // Create primitive types (reused across built-ins)
    const void_type = try allocator.create(types.Type);
    void_type.* = .{
        .kind = .void,
        .location = location.SourceLocation.dummy(),
        .data = .{ .void = {} },
    };

    const unknown_type = try allocator.create(types.Type);
    unknown_type.* = .{
        .kind = .unknown,
        .location = location.SourceLocation.dummy(),
        .data = .{ .unknown = {} },
    };

    // console type: { log: (msg: any) => void, error: (msg: any) => void, warn: (msg: any) => void }
    const console_type = try createConsoleType(allocator, void_type, unknown_type);

    // this (class instance reference)
    try scope.define(Symbol{
        .name = "this",
        .kind = .variable,
        .type = null, // Type depends on enclosing class
        .location = location.SourceLocation.dummy(),
        .mutable = false,
        .visibility = .public,
        .decl_node = null,
    });

    // console (as an object with methods)
    try scope.define(Symbol{
        .name = "console",
        .kind = .variable,
        .type = console_type,
        .location = location.SourceLocation.dummy(),
        .mutable = false,
        .visibility = .public,
        .decl_node = null,
    });

    // parseInt, parseFloat - simple stubs for now
    try scope.define(Symbol{
        .name = "parseInt",
        .kind = .function,
        .type = null, // TODO: Add function type
        .location = location.SourceLocation.dummy(),
        .mutable = false,
        .visibility = .public,
        .decl_node = null,
    });

    try scope.define(Symbol{
        .name = "parseFloat",
        .kind = .function,
        .type = null, // TODO: Add function type
        .location = location.SourceLocation.dummy(),
        .mutable = false,
        .visibility = .public,
        .decl_node = null,
    });

    // Math global object with methods and constants
    const number_type = try allocator.create(types.Type);
    number_type.* = .{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    const math_type = try createMathType(allocator, number_type);
    try scope.define(Symbol{
        .name = "Math",
        .kind = .variable,
        .type = math_type,
        .location = location.SourceLocation.dummy(),
        .mutable = false,
        .visibility = .public,
        .decl_node = null,
    });
}

/// Create console type: { log: (...args: any[]) => void, ... }
/// console.log is variadic - accepts any number of arguments of any type
fn createConsoleType(allocator: std.mem.Allocator, void_type: *types.Type, any_type: *types.Type) !*types.Type {
    // Create rest parameter for variadic log/error/warn: (...args: any[]) => void
    // No required params, just a rest param that accepts any type
    const rest_param = try allocator.create(types.FunctionType.FunctionParam);
    rest_param.* = .{
        .name = "args",
        .type = any_type, // Element type is 'any' (unknown)
        .optional = false,
    };

    const log_func_data = try allocator.create(types.FunctionType);
    log_func_data.* = .{
        .type_params = &[_]types.GenericParam{},
        .params = &[_]types.FunctionType.FunctionParam{}, // No required params
        .return_type = void_type,
        .rest_param = rest_param, // Variadic!
    };

    const log_func_type = try allocator.create(types.Type);
    log_func_type.* = .{
        .kind = .function,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function = log_func_data },
    };

    // Create console object type with methods
    const methods_slice = try allocator.alloc(types.ObjectType.Property, 3);
    methods_slice[0] = .{ .name = "log", .type = log_func_type, .optional = false };
    methods_slice[1] = .{ .name = "error", .type = log_func_type, .optional = false };
    methods_slice[2] = .{ .name = "warn", .type = log_func_type, .optional = false };

    const console_obj_data = try allocator.create(types.ObjectType);
    console_obj_data.* = .{
        .properties = &[_]types.ObjectType.Property{},
        .methods = methods_slice,
    };

    const console_type = try allocator.create(types.Type);
    console_type.* = .{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = console_obj_data },
    };

    return console_type;
}

/// Create Math type: { floor: (x: number) => number, PI: number, ... }
/// TypeScript-compatible Math global object
fn createMathType(allocator: std.mem.Allocator, number_type: *types.Type) !*types.Type {
    // Create single-arg function type: (x: number) => number
    const single_param = try allocator.alloc(types.FunctionType.FunctionParam, 1);
    single_param[0] = .{ .name = "x", .type = number_type, .optional = false };

    const single_arg_func_data = try allocator.create(types.FunctionType);
    single_arg_func_data.* = .{
        .type_params = &[_]types.GenericParam{},
        .params = single_param,
        .return_type = number_type,
        .rest_param = null,
    };

    const single_arg_func = try allocator.create(types.Type);
    single_arg_func.* = .{
        .kind = .function,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function = single_arg_func_data },
    };

    // Create two-arg function type: (x: number, y: number) => number
    const two_params = try allocator.alloc(types.FunctionType.FunctionParam, 2);
    two_params[0] = .{ .name = "x", .type = number_type, .optional = false };
    two_params[1] = .{ .name = "y", .type = number_type, .optional = false };

    const two_arg_func_data = try allocator.create(types.FunctionType);
    two_arg_func_data.* = .{
        .type_params = &[_]types.GenericParam{},
        .params = two_params,
        .return_type = number_type,
        .rest_param = null,
    };

    const two_arg_func = try allocator.create(types.Type);
    two_arg_func.* = .{
        .kind = .function,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function = two_arg_func_data },
    };

    // Create zero-arg function type: () => number (for random)
    const zero_arg_func_data = try allocator.create(types.FunctionType);
    zero_arg_func_data.* = .{
        .type_params = &[_]types.GenericParam{},
        .params = &[_]types.FunctionType.FunctionParam{},
        .return_type = number_type,
        .rest_param = null,
    };

    const zero_arg_func = try allocator.create(types.Type);
    zero_arg_func.* = .{
        .kind = .function,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function = zero_arg_func_data },
    };

    // Math methods (as object properties with function types)
    // Single-arg: floor, ceil, round, sqrt, abs, sin, cos, tan, asin, acos, atan,
    //             sinh, cosh, tanh, exp, log, log10, log2, trunc, sign, cbrt, expm1, log1p
    // Two-arg: pow, atan2, min, max, hypot
    // Zero-arg: random
    const methods_slice = try allocator.alloc(types.ObjectType.Property, 30);

    // Single-argument methods
    methods_slice[0] = .{ .name = "floor", .type = single_arg_func, .optional = false };
    methods_slice[1] = .{ .name = "ceil", .type = single_arg_func, .optional = false };
    methods_slice[2] = .{ .name = "round", .type = single_arg_func, .optional = false };
    methods_slice[3] = .{ .name = "sqrt", .type = single_arg_func, .optional = false };
    methods_slice[4] = .{ .name = "abs", .type = single_arg_func, .optional = false };
    methods_slice[5] = .{ .name = "sin", .type = single_arg_func, .optional = false };
    methods_slice[6] = .{ .name = "cos", .type = single_arg_func, .optional = false };
    methods_slice[7] = .{ .name = "tan", .type = single_arg_func, .optional = false };
    methods_slice[8] = .{ .name = "asin", .type = single_arg_func, .optional = false };
    methods_slice[9] = .{ .name = "acos", .type = single_arg_func, .optional = false };
    methods_slice[10] = .{ .name = "atan", .type = single_arg_func, .optional = false };
    methods_slice[11] = .{ .name = "sinh", .type = single_arg_func, .optional = false };
    methods_slice[12] = .{ .name = "cosh", .type = single_arg_func, .optional = false };
    methods_slice[13] = .{ .name = "tanh", .type = single_arg_func, .optional = false };
    methods_slice[14] = .{ .name = "exp", .type = single_arg_func, .optional = false };
    methods_slice[15] = .{ .name = "log", .type = single_arg_func, .optional = false };
    methods_slice[16] = .{ .name = "log10", .type = single_arg_func, .optional = false };
    methods_slice[17] = .{ .name = "log2", .type = single_arg_func, .optional = false };
    methods_slice[18] = .{ .name = "trunc", .type = single_arg_func, .optional = false };
    methods_slice[19] = .{ .name = "sign", .type = single_arg_func, .optional = false };
    methods_slice[20] = .{ .name = "cbrt", .type = single_arg_func, .optional = false };
    methods_slice[21] = .{ .name = "expm1", .type = single_arg_func, .optional = false };
    methods_slice[22] = .{ .name = "log1p", .type = single_arg_func, .optional = false };

    // Two-argument methods
    methods_slice[23] = .{ .name = "pow", .type = two_arg_func, .optional = false };
    methods_slice[24] = .{ .name = "atan2", .type = two_arg_func, .optional = false };
    methods_slice[25] = .{ .name = "min", .type = two_arg_func, .optional = false };
    methods_slice[26] = .{ .name = "max", .type = two_arg_func, .optional = false };
    methods_slice[27] = .{ .name = "hypot", .type = two_arg_func, .optional = false };

    // Zero-argument methods
    methods_slice[28] = .{ .name = "random", .type = zero_arg_func, .optional = false };

    // Variadic min/max would need rest params - using two-arg for now
    methods_slice[29] = .{ .name = "imul", .type = two_arg_func, .optional = false };

    // Math constants (as properties)
    const props_slice = try allocator.alloc(types.ObjectType.Property, 8);
    props_slice[0] = .{ .name = "PI", .type = number_type, .optional = false };
    props_slice[1] = .{ .name = "E", .type = number_type, .optional = false };
    props_slice[2] = .{ .name = "LN2", .type = number_type, .optional = false };
    props_slice[3] = .{ .name = "LN10", .type = number_type, .optional = false };
    props_slice[4] = .{ .name = "LOG2E", .type = number_type, .optional = false };
    props_slice[5] = .{ .name = "LOG10E", .type = number_type, .optional = false };
    props_slice[6] = .{ .name = "SQRT2", .type = number_type, .optional = false };
    props_slice[7] = .{ .name = "SQRT1_2", .type = number_type, .optional = false };

    const math_obj_data = try allocator.create(types.ObjectType);
    math_obj_data.* = .{
        .properties = props_slice,
        .methods = methods_slice,
    };

    const math_type = try allocator.create(types.Type);
    math_type.* = .{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = math_obj_data },
    };

    return math_type;
}

// ============================================================================
// Tests
// ============================================================================

test "symbol table: define and lookup" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Define a variable
    try table.define(Symbol.init("x", .variable, location.SourceLocation.dummy()));

    // Should find it
    const found = table.lookup("x");
    try std.testing.expect(found != null);
    try std.testing.expectEqualStrings("x", found.?.name);

    // Unknown should return null
    const not_found = table.lookup("unknown");
    try std.testing.expect(not_found == null);
}

test "symbol table: nested scopes" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Define in global scope
    try table.define(Symbol.init("global_var", .variable, location.SourceLocation.dummy()));

    // Enter function scope
    try table.enterScope(.function);
    try table.define(Symbol.init("local_var", .variable, location.SourceLocation.dummy()));

    // Should find both
    try std.testing.expect(table.lookup("global_var") != null);
    try std.testing.expect(table.lookup("local_var") != null);

    // Exit function scope
    table.exitScope();

    // Global still visible, local not
    try std.testing.expect(table.lookup("global_var") != null);
    try std.testing.expect(table.lookup("local_var") == null);
}

test "symbol table: duplicate definition error" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    try table.define(Symbol.init("x", .variable, location.SourceLocation.dummy()));

    // Duplicate should fail
    const result = table.define(Symbol.init("x", .variable, location.SourceLocation.dummy()));
    try std.testing.expectError(error.SymbolAlreadyDefined, result);
}

test "symbol table: shadowing in nested scope" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Define x in global
    var global_x = Symbol.init("x", .variable, location.SourceLocation.dummy());
    global_x.mutable = false;
    try table.define(global_x);

    // Enter block scope and shadow x
    try table.enterScope(.block);
    var local_x = Symbol.init("x", .variable, location.SourceLocation.dummy());
    local_x.mutable = true;
    try table.define(local_x);

    // Should find the local (mutable) one
    const found = table.lookup("x");
    try std.testing.expect(found != null);
    try std.testing.expect(found.?.mutable == true);

    // Exit scope, should find global (immutable)
    table.exitScope();
    const found_global = table.lookup("x");
    try std.testing.expect(found_global != null);
    try std.testing.expect(found_global.?.mutable == false);
}

test "symbol table: loop detection" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Not in loop initially
    try std.testing.expect(!table.isInLoop());

    // Enter function
    try table.enterScope(.function);
    try std.testing.expect(!table.isInLoop());

    // Enter loop
    try table.enterScope(.loop);
    try std.testing.expect(table.isInLoop());

    // Enter block inside loop
    try table.enterScope(.block);
    try std.testing.expect(table.isInLoop()); // Still in loop

    table.exitScope();
    table.exitScope();
    try std.testing.expect(!table.isInLoop()); // Out of loop
}

test "symbol table: loop detection stops at function boundary" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Structure: function outer() { while(true) { function inner() { /* here */ } } }
    try table.enterScope(.function); // outer
    try table.enterScope(.loop); // while
    try std.testing.expect(table.isInLoop()); // In loop

    // Enter nested function - this should "reset" loop context
    try table.enterScope(.function); // inner
    try std.testing.expect(!table.isInLoop()); // NOT in loop from inner's perspective

    // Even entering a block inside the nested function shouldn't find the outer loop
    try table.enterScope(.block);
    try std.testing.expect(!table.isInLoop()); // Still not in loop

    table.exitScope(); // exit block
    table.exitScope(); // exit inner function

    // Back in the loop scope
    try std.testing.expect(table.isInLoop()); // In loop again
}

test "symbol table: builtins are defined" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Built-in functions should be available
    try std.testing.expect(table.lookup("console") != null);
    try std.testing.expect(table.lookup("parseInt") != null);
    try std.testing.expect(table.lookup("parseFloat") != null);
}

// ============================================================================
// Tests for lookupAtPosition (position-aware symbol lookup for LSP)
// ============================================================================

test "lookupAtPosition: finds symbol declared before cursor" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Simulate: let x = 1; (declared at line 5)
    const sym_x = Symbol.init("x", .variable, location.SourceLocation.init(
        1,
        .{ .line = 5, .column = 4 },
        .{ .line = 5, .column = 5 },
    ));
    try table.define(sym_x);

    // Cursor at line 10 - should find x (declared before)
    const found = table.lookupAtPosition("x", 10, 0);
    try std.testing.expect(found != null);
    try std.testing.expectEqualStrings("x", found.?.name);

    // Cursor at line 3 - should NOT find x (cursor is before declaration)
    const not_found = table.lookupAtPosition("x", 3, 0);
    try std.testing.expect(not_found == null);
}

test "lookupAtPosition: handles variable shadowing correctly" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Outer scope: let x = "outer" at line 1
    var outer_x = Symbol.init("x", .variable, location.SourceLocation.init(
        1,
        .{ .line = 1, .column = 4 },
        .{ .line = 1, .column = 5 },
    ));
    outer_x.mutable = false; // const
    try table.define(outer_x);

    // Enter inner scope
    try table.enterScope(.block);

    // Inner scope: let x = "inner" at line 5
    var inner_x = Symbol.init("x", .variable, location.SourceLocation.init(
        1,
        .{ .line = 5, .column = 8 },
        .{ .line = 5, .column = 9 },
    ));
    inner_x.mutable = true; // let
    try table.define(inner_x);

    table.exitScope();

    // At line 3 (between outer and inner declarations): should find outer x
    const at_line_3 = table.lookupAtPosition("x", 3, 0);
    try std.testing.expect(at_line_3 != null);
    try std.testing.expect(at_line_3.?.mutable == false); // outer is const

    // At line 7 (after inner declaration): should find inner x (shadows outer)
    const at_line_7 = table.lookupAtPosition("x", 7, 0);
    try std.testing.expect(at_line_7 != null);
    try std.testing.expect(at_line_7.?.mutable == true); // inner is let
}

test "lookupAtPosition: same line uses column for ordering" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // let a = 1, b = a; (both on line 5, a at col 4, b at col 12)
    const sym_a = Symbol.init("a", .variable, location.SourceLocation.init(
        1,
        .{ .line = 5, .column = 4 },
        .{ .line = 5, .column = 5 },
    ));
    try table.define(sym_a);

    const sym_b = Symbol.init("b", .variable, location.SourceLocation.init(
        1,
        .{ .line = 5, .column = 12 },
        .{ .line = 5, .column = 13 },
    ));
    try table.define(sym_b);

    // At column 8 (after a, before b): should find a, not b
    const at_col_8 = table.lookupAtPosition("a", 5, 8);
    try std.testing.expect(at_col_8 != null);

    const b_at_col_8 = table.lookupAtPosition("b", 5, 8);
    try std.testing.expect(b_at_col_8 == null); // b not visible yet

    // At column 15 (after both): should find both
    const a_at_col_15 = table.lookupAtPosition("a", 5, 15);
    try std.testing.expect(a_at_col_15 != null);

    const b_at_col_15 = table.lookupAtPosition("b", 5, 15);
    try std.testing.expect(b_at_col_15 != null);
}

test "lookupAtPosition: nested function scopes" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // function outer() at line 1
    try table.enterScope(.function);

    // let x = 1 at line 2 (inside outer)
    const outer_x = Symbol.init("x", .variable, location.SourceLocation.init(
        1,
        .{ .line = 2, .column = 8 },
        .{ .line = 2, .column = 9 },
    ));
    try table.define(outer_x);

    // function inner() at line 4
    try table.enterScope(.function);

    // let x = 2 at line 5 (inside inner, shadows outer's x)
    const inner_x = Symbol.init("x", .variable, location.SourceLocation.init(
        1,
        .{ .line = 5, .column = 12 },
        .{ .line = 5, .column = 13 },
    ));
    try table.define(inner_x);

    table.exitScope(); // exit inner
    table.exitScope(); // exit outer

    // At line 3 (inside outer, before inner): find outer's x
    const at_line_3 = table.lookupAtPosition("x", 3, 0);
    try std.testing.expect(at_line_3 != null);
    try std.testing.expectEqual(@as(u32, 2), at_line_3.?.location.start.line);

    // At line 6 (inside inner): find inner's x
    const at_line_6 = table.lookupAtPosition("x", 6, 0);
    try std.testing.expect(at_line_6 != null);
    try std.testing.expectEqual(@as(u32, 5), at_line_6.?.location.start.line);
}

test "lookupAtPosition: returns null for unknown symbol" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    const sym = Symbol.init("x", .variable, location.SourceLocation.init(
        1,
        .{ .line = 5, .column = 4 },
        .{ .line = 5, .column = 5 },
    ));
    try table.define(sym);

    // Looking for "y" which doesn't exist
    const not_found = table.lookupAtPosition("y", 10, 0);
    try std.testing.expect(not_found == null);
}

// ============================================================================
// Tests for updateTypeAll (cross-phase type updates)
// ============================================================================

test "updateTypeAll: updates symbol in any scope" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Create a type to use
    var int_type = types.Type{
        .kind = .int32,
        .location = location.SourceLocation.dummy(),
        .data = .{ .int32 = {} },
    };

    // Phase 1: Define symbol with null type in function scope
    try table.enterScope(.function);
    var sym = Symbol.init("x", .variable, location.SourceLocation.dummy());
    sym.type = null;
    try table.define(sym);
    table.exitScope();

    // Verify symbol has null type
    const before = table.lookupAll("x");
    try std.testing.expect(before != null);
    try std.testing.expect(before.?.type == null);

    // Phase 3: Update type (from global scope - simulates inference phase)
    try table.updateTypeAll("x", &int_type);

    // Verify symbol now has int32 type
    const after = table.lookupAll("x");
    try std.testing.expect(after != null);
    try std.testing.expect(after.?.type != null);
    try std.testing.expectEqual(types.TypeKind.int32, after.?.type.?.kind);
}

test "updateTypeAll: updates correct symbol when shadowed" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    var int_type = types.Type{
        .kind = .int32,
        .location = location.SourceLocation.dummy(),
        .data = .{ .int32 = {} },
    };

    // Global x
    var global_x = Symbol.init("x", .variable, location.SourceLocation.dummy());
    global_x.type = null;
    try table.define(global_x);

    // Function scope with shadowed x
    try table.enterScope(.function);
    var local_x = Symbol.init("x", .variable, location.SourceLocation.dummy());
    local_x.type = null;
    try table.define(local_x);
    table.exitScope();

    // updateTypeAll finds most recent (inner scope) first
    try table.updateTypeAll("x", &int_type);

    // The inner x should be updated (searched in reverse order)
    // Note: This tests the current behavior - inner scope is found first
    const found = table.lookupAll("x");
    try std.testing.expect(found != null);
    try std.testing.expect(found.?.type != null);
    try std.testing.expectEqual(types.TypeKind.int32, found.?.type.?.kind);
}

test "updateTypeAll: returns error for unknown symbol" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    var int_type = types.Type{
        .kind = .int32,
        .location = location.SourceLocation.dummy(),
        .data = .{ .int32 = {} },
    };

    // Try to update non-existent symbol
    const result = table.updateTypeAll("nonexistent", &int_type);
    try std.testing.expectError(error.SymbolNotFound, result);
}

// ============================================================================
// Tests for scope boundary bugs - lookupAtPosition must respect scope end
// ============================================================================

test "lookupAtPosition: CRITICAL - cursor outside inner scope must find outer variable" {
    // This is the critical bug test case:
    //
    // function outer(): number {
    //     let x = 1;           // Line 3, outer scope
    //     function inner() {
    //         let x = 2;       // Line 6, inner scope
    //     }                    // Line 7, inner scope ENDS here
    //     return x;            // Line 10, cursor here should find OUTER x (line 3)
    // }
    //
    // Bug: lookupAtPosition was returning inner x (line 6) because it only
    // checked if declaration was BEFORE cursor, not if cursor was WITHIN scope.

    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Enter outer function scope (lines 2-11)
    try table.enterScope(.function);
    table.scopes.items[table.scopes.items.len - 1].start_location = location.SourceLocation.init(
        1,
        .{ .line = 2, .column = 0 },
        .{ .line = 11, .column = 1 },
    );

    // Outer x at line 3
    const outer_x = Symbol.init("x", .variable, location.SourceLocation.init(
        1,
        .{ .line = 3, .column = 8 },
        .{ .line = 3, .column = 9 },
    ));
    try table.define(outer_x);

    // Enter inner function scope (lines 5-7)
    try table.enterScope(.function);
    table.scopes.items[table.scopes.items.len - 1].start_location = location.SourceLocation.init(
        1,
        .{ .line = 5, .column = 4 },
        .{ .line = 7, .column = 5 },
    );

    // Inner x at line 6
    const inner_x = Symbol.init("x", .variable, location.SourceLocation.init(
        1,
        .{ .line = 6, .column = 12 },
        .{ .line = 6, .column = 13 },
    ));
    try table.define(inner_x);

    table.exitScope(); // exit inner
    table.exitScope(); // exit outer

    // At line 10 (OUTSIDE inner scope, but INSIDE outer scope):
    // Should find OUTER x (line 3), NOT inner x (line 6)
    const at_line_10 = table.lookupAtPosition("x", 10, 0);
    try std.testing.expect(at_line_10 != null);
    try std.testing.expectEqual(@as(u32, 3), at_line_10.?.location.start.line);

    // At line 6 (INSIDE inner scope): should find inner x
    const at_line_6 = table.lookupAtPosition("x", 6, 15);
    try std.testing.expect(at_line_6 != null);
    try std.testing.expectEqual(@as(u32, 6), at_line_6.?.location.start.line);
}

test "lookupAtPosition: block scope shadowing - cursor after block must find outer" {
    // {
    //     let y = 10;      // Line 2, outer block
    //     {
    //         let y = 20;  // Line 4, inner block
    //     }                // Line 5, inner block ENDS
    //     return y;        // Line 6, should find outer y (line 2)
    // }

    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Outer block (lines 1-7)
    try table.enterScope(.block);
    table.scopes.items[table.scopes.items.len - 1].start_location = location.SourceLocation.init(
        1,
        .{ .line = 1, .column = 0 },
        .{ .line = 7, .column = 1 },
    );

    // Outer y at line 2
    const outer_y = Symbol.init("y", .variable, location.SourceLocation.init(
        1,
        .{ .line = 2, .column = 8 },
        .{ .line = 2, .column = 9 },
    ));
    try table.define(outer_y);

    // Inner block (lines 3-5)
    try table.enterScope(.block);
    table.scopes.items[table.scopes.items.len - 1].start_location = location.SourceLocation.init(
        1,
        .{ .line = 3, .column = 4 },
        .{ .line = 5, .column = 5 },
    );

    // Inner y at line 4
    const inner_y = Symbol.init("y", .variable, location.SourceLocation.init(
        1,
        .{ .line = 4, .column = 12 },
        .{ .line = 4, .column = 13 },
    ));
    try table.define(inner_y);

    table.exitScope(); // exit inner
    table.exitScope(); // exit outer

    // At line 6 (after inner block ends): should find outer y (line 2)
    const at_line_6 = table.lookupAtPosition("y", 6, 0);
    try std.testing.expect(at_line_6 != null);
    try std.testing.expectEqual(@as(u32, 2), at_line_6.?.location.start.line);

    // At line 4 col 15 (inside inner block): should find inner y
    const at_line_4 = table.lookupAtPosition("y", 4, 15);
    try std.testing.expect(at_line_4 != null);
    try std.testing.expectEqual(@as(u32, 4), at_line_4.?.location.start.line);
}

test "lookupAtPosition: loop scope - cursor after loop must find outer variable" {
    // let i = 100;         // Line 1, outer
    // for (let i = 0; ...) // Line 2, loop scope
    // {                    // Line 3
    // }                    // Line 4, loop ends
    // console.log(i);      // Line 5, should find outer i (line 1)

    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Outer i at line 1 (global scope)
    const outer_i = Symbol.init("i", .variable, location.SourceLocation.init(
        1,
        .{ .line = 1, .column = 4 },
        .{ .line = 1, .column = 5 },
    ));
    try table.define(outer_i);

    // Loop scope (lines 2-4)
    try table.enterScope(.loop);
    table.scopes.items[table.scopes.items.len - 1].start_location = location.SourceLocation.init(
        1,
        .{ .line = 2, .column = 0 },
        .{ .line = 4, .column = 1 },
    );

    // Loop i at line 2
    const loop_i = Symbol.init("i", .variable, location.SourceLocation.init(
        1,
        .{ .line = 2, .column = 9 },
        .{ .line = 2, .column = 10 },
    ));
    try table.define(loop_i);

    table.exitScope();

    // At line 5 (after loop): should find outer i (line 1)
    const at_line_5 = table.lookupAtPosition("i", 5, 0);
    try std.testing.expect(at_line_5 != null);
    try std.testing.expectEqual(@as(u32, 1), at_line_5.?.location.start.line);

    // At line 3 (inside loop): should find loop i (line 2)
    const at_line_3 = table.lookupAtPosition("i", 3, 0);
    try std.testing.expect(at_line_3 != null);
    try std.testing.expectEqual(@as(u32, 2), at_line_3.?.location.start.line);
}

test "lookupAtPosition: deeply nested scopes - must respect all boundaries" {
    // function a() {           // Line 1, scope A
    //     let x = 1;           // Line 2
    //     function b() {       // Line 3, scope B
    //         let x = 2;       // Line 4
    //         function c() {   // Line 5, scope C
    //             let x = 3;   // Line 6
    //         }                // Line 7, scope C ends
    //         return x;        // Line 8, should find B's x (line 4)
    //     }                    // Line 9, scope B ends
    //     return x;            // Line 10, should find A's x (line 2)
    // }

    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Scope A (lines 1-11)
    try table.enterScope(.function);
    table.scopes.items[table.scopes.items.len - 1].start_location = location.SourceLocation.init(
        1,
        .{ .line = 1, .column = 0 },
        .{ .line = 11, .column = 1 },
    );
    const x_a = Symbol.init("x", .variable, location.SourceLocation.init(
        1,
        .{ .line = 2, .column = 8 },
        .{ .line = 2, .column = 9 },
    ));
    try table.define(x_a);

    // Scope B (lines 3-9)
    try table.enterScope(.function);
    table.scopes.items[table.scopes.items.len - 1].start_location = location.SourceLocation.init(
        1,
        .{ .line = 3, .column = 4 },
        .{ .line = 9, .column = 5 },
    );
    const x_b = Symbol.init("x", .variable, location.SourceLocation.init(
        1,
        .{ .line = 4, .column = 12 },
        .{ .line = 4, .column = 13 },
    ));
    try table.define(x_b);

    // Scope C (lines 5-7)
    try table.enterScope(.function);
    table.scopes.items[table.scopes.items.len - 1].start_location = location.SourceLocation.init(
        1,
        .{ .line = 5, .column = 8 },
        .{ .line = 7, .column = 9 },
    );
    const x_c = Symbol.init("x", .variable, location.SourceLocation.init(
        1,
        .{ .line = 6, .column = 16 },
        .{ .line = 6, .column = 17 },
    ));
    try table.define(x_c);

    table.exitScope(); // exit C
    table.exitScope(); // exit B
    table.exitScope(); // exit A

    // Line 10 (inside A, outside B): should find A's x (line 2)
    const at_line_10 = table.lookupAtPosition("x", 10, 0);
    try std.testing.expect(at_line_10 != null);
    try std.testing.expectEqual(@as(u32, 2), at_line_10.?.location.start.line);

    // Line 8 (inside B, outside C): should find B's x (line 4)
    const at_line_8 = table.lookupAtPosition("x", 8, 0);
    try std.testing.expect(at_line_8 != null);
    try std.testing.expectEqual(@as(u32, 4), at_line_8.?.location.start.line);

    // Line 6 col 20 (inside C): should find C's x (line 6)
    const at_line_6 = table.lookupAtPosition("x", 6, 20);
    try std.testing.expect(at_line_6 != null);
    try std.testing.expectEqual(@as(u32, 6), at_line_6.?.location.start.line);
}

test "lookupAtPosition: no scope boundary - falls back to line-based lookup" {
    // Test backward compatibility: if scope has no start_location, use line-based logic
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Define x without scope boundaries (old behavior)
    const sym_x = Symbol.init("x", .variable, location.SourceLocation.init(
        1,
        .{ .line = 5, .column = 4 },
        .{ .line = 5, .column = 5 },
    ));
    try table.define(sym_x);

    // Should still find x at line 10 (line-based fallback)
    const found = table.lookupAtPosition("x", 10, 0);
    try std.testing.expect(found != null);
    try std.testing.expectEqualStrings("x", found.?.name);
}
