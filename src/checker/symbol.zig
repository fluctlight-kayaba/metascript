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
};

/// A symbol represents a named entity in the program
pub const Symbol = struct {
    name: []const u8,
    kind: SymbolKind,
    type: ?*types.Type,
    location: location.SourceLocation,
    /// For variables: is it const or let?
    mutable: bool,
    /// For class members: visibility
    visibility: Visibility,
    /// The AST node that declared this symbol (if any)
    decl_node: ?*ast.Node,

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
        };
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
    pub fn isInLoop(self: *Scope) bool {
        if (self.kind == .loop) return true;
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
}

/// Create console type: { log: (msg: any) => void, ... }
fn createConsoleType(allocator: std.mem.Allocator, void_type: *types.Type, any_type: *types.Type) !*types.Type {
    // Create function type for log/error/warn: (msg: any) => void
    const params_slice = try allocator.alloc(types.FunctionType.FunctionParam, 1);
    params_slice[0] = .{
        .name = "msg",
        .type = any_type,
        .optional = false,
    };

    const log_func_data = try allocator.create(types.FunctionType);
    log_func_data.* = .{
        .type_params = &[_]types.GenericParam{},
        .params = params_slice,
        .return_type = void_type,
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

test "symbol table: builtins are defined" {
    const allocator = std.testing.allocator;
    var table = try SymbolTable.init(allocator);
    defer table.deinit();

    // Built-in functions should be available
    try std.testing.expect(table.lookup("console") != null);
    try std.testing.expect(table.lookup("parseInt") != null);
    try std.testing.expect(table.lookup("parseFloat") != null);
}
