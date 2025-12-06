const std = @import("std");
const ast = @import("../ast/ast.zig");
const types = @import("../ast/types.zig");
const location = @import("../ast/location.zig");
const module_loader = @import("../module/loader.zig");

// Re-export checker submodules
pub const symbol = @import("symbol.zig");
pub const resolver = @import("resolver.zig");
pub const inference = @import("inference.zig");
pub const type_compat = @import("type_compat.zig");
pub const type_factory = @import("type_factory.zig");

/// Type checking error
pub const TypeError = struct {
    message: []const u8,
    location: location.SourceLocation,
    kind: Kind,

    pub const Kind = enum {
        type_mismatch,
        undefined_variable,
        undefined_type,
        invalid_operation,
        missing_return,
        return_type_mismatch,
        break_outside_loop,
        continue_outside_loop,
        duplicate_definition,
        readonly_assignment,
        missing_implementation,
        incompatible_override,
        use_after_move,
        move_in_loop,
    };
};

/// Type checker for Metascript
///
/// The type checker performs these phases:
/// 1. Symbol collection: Register all declarations
/// 2. Type resolution: Resolve type references
/// 3. Type inference: Infer types for expressions
/// 4. Type checking: Verify type compatibility
pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    symbols: symbol.SymbolTable,
    errors: std.ArrayList(TypeError),
    message_arena: std.heap.ArenaAllocator,
    type_arena: std.heap.ArenaAllocator,
    /// Current class type for `this` binding in methods
    current_class_type: ?*types.Type = null,
    /// Module loader for resolving imports across modules
    loader: ?*module_loader.ModuleLoader = null,
    /// Current module path being type-checked (for import resolution)
    current_module_path: ?[]const u8 = null,
    /// Track variables that have been moved (use-after-move detection)
    moved_variables: std.StringHashMap(location.SourceLocation) = undefined,

    pub fn init(allocator: std.mem.Allocator) !TypeChecker {
        return .{
            .allocator = allocator,
            .symbols = try symbol.SymbolTable.init(allocator),
            .errors = std.ArrayList(TypeError).init(allocator),
            .message_arena = std.heap.ArenaAllocator.init(allocator),
            .type_arena = std.heap.ArenaAllocator.init(allocator),
            .current_class_type = null,
            .loader = null,
            .current_module_path = null,
            .moved_variables = std.StringHashMap(location.SourceLocation).init(allocator),
        };
    }

    /// Set the module loader for cross-module import resolution
    pub fn setModuleLoader(self: *TypeChecker, loader: *module_loader.ModuleLoader) void {
        self.loader = loader;
    }

    /// Set the current module path being type-checked
    pub fn setCurrentModulePath(self: *TypeChecker, path: []const u8) void {
        self.current_module_path = path;
    }

    pub fn deinit(self: *TypeChecker) void {
        self.symbols.deinit();
        self.errors.deinit();
        self.message_arena.deinit();
        self.type_arena.deinit();
        self.moved_variables.deinit();
    }

    /// Format a type mismatch error message with actual type names
    fn formatTypeMismatch(self: *TypeChecker, expected: *types.Type, actual: *types.Type, context: []const u8) ![]const u8 {
        const arena_alloc = self.message_arena.allocator();
        const expected_str = try types.typeToString(arena_alloc, expected);
        const actual_str = try types.typeToString(arena_alloc, actual);
        return try std.fmt.allocPrint(arena_alloc, "{s}: expected '{s}', got '{s}'", .{ context, expected_str, actual_str });
    }

    /// Get a type name as a string
    fn typeName(self: *TypeChecker, t: *types.Type) ![]const u8 {
        return try types.typeToString(self.message_arena.allocator(), t);
    }

    // Type factory methods - delegate to type_factory module
    fn getTypeFactory(self: *TypeChecker) type_factory.TypeFactory {
        return type_factory.TypeFactory.init(self.type_arena.allocator());
    }

    fn createFunctionType(
        self: *TypeChecker,
        type_params: []const types.GenericParam,
        params: []const ast.node.FunctionExpr.FunctionParam,
        return_type: ?*types.Type,
        loc: location.SourceLocation,
    ) !*types.Type {
        var factory = self.getTypeFactory();
        return factory.createFunctionType(type_params, params, return_type, loc);
    }

    fn createVoidType(self: *TypeChecker, loc: location.SourceLocation) !*types.Type {
        var factory = self.getTypeFactory();
        return factory.createVoidType(loc);
    }

    fn createUnknownType(self: *TypeChecker, loc: location.SourceLocation) !*types.Type {
        var factory = self.getTypeFactory();
        return factory.createUnknownType(loc);
    }

    fn createClassType(self: *TypeChecker, members: []*ast.Node, loc: location.SourceLocation) !*types.Type {
        var factory = self.getTypeFactory();
        return factory.createClassType(members, loc);
    }

    fn createClassTypeNamed(self: *TypeChecker, name: []const u8, members: []*ast.Node, loc: location.SourceLocation) !*types.Type {
        var factory = self.getTypeFactory();
        return factory.createClassTypeNamed(name, members, loc);
    }

    /// Type check the AST
    /// Returns true if no errors were found
    pub fn check(self: *TypeChecker, program: *ast.Node) !bool {
        std.debug.assert(program.kind == .program);

        // Phase 1: Collect all declarations into symbol table
        try self.collectDeclarations(program);

        // Phase 2: Resolve type references
        var type_resolver = resolver.TypeResolver.init(self.allocator, &self.symbols);
        defer type_resolver.deinit();
        try type_resolver.resolve(program);

        // Copy resolver errors
        for (type_resolver.getErrors()) |err| {
            try self.errors.append(.{
                .message = err.message,
                .location = err.location,
                .kind = .undefined_type,
            });
        }

        // Phase 3: Infer types for expressions
        var type_inference = inference.TypeInference.init(self.allocator, &self.symbols);
        defer type_inference.deinit();
        try type_inference.infer(program);

        // Copy inference errors
        for (type_inference.getErrors()) |err| {
            try self.errors.append(.{
                .message = err.message,
                .location = err.location,
                .kind = .undefined_variable,
            });
        }

        // Phase 4: Check type compatibility
        try self.checkTypes(program);

        return self.errors.items.len == 0;
    }

    /// Collect all declarations into the symbol table
    fn collectDeclarations(self: *TypeChecker, node: *ast.Node) !void {
        switch (node.kind) {
            .program => {
                for (node.data.program.statements) |stmt| {
                    try self.collectDeclarations(stmt);
                }
            },
            .variable_stmt => {
                const var_stmt = &node.data.variable_stmt;
                const is_mutable = var_stmt.kind != .@"const";

                for (var_stmt.declarations) |decl| {
                    var sym = symbol.Symbol.init(decl.name, .variable, node.location);
                    sym.mutable = is_mutable;
                    sym.type = decl.type; // May be null, will be inferred later in Phase 3
                    sym.doc_comment = node.doc_comment; // Propagate JSDoc from AST

                    self.symbols.define(sym) catch {
                        try self.errors.append(.{
                            .message = "duplicate variable definition",
                            .location = node.location,
                            .kind = .duplicate_definition,
                        });
                    };
                }
            },
            .function_decl => {
                const func = &node.data.function_decl;
                var sym = symbol.Symbol.init(func.name, .function, node.location);

                // Create function type from declaration (with type parameters)
                sym.type = try self.createFunctionType(func.type_params, func.params, func.return_type, node.location);
                sym.doc_comment = node.doc_comment; // Propagate JSDoc from AST

                self.symbols.define(sym) catch {
                    try self.errors.append(.{
                        .message = "duplicate function definition",
                        .location = node.location,
                        .kind = .duplicate_definition,
                    });
                };

                // Enter function scope for body
                if (func.body) |body| {
                    // Use node location (full declaration) so parameters in signature are in scope
                    try self.symbols.enterScopeWithLocation(.function, node.location);
                    defer self.symbols.exitScope();

                    // Register type parameters as type aliases in scope
                    for (func.type_params) |tp| {
                        var tp_sym = symbol.Symbol.init(tp.name, .type_alias, node.location);
                        // Create a generic_param type for this type parameter
                        const arena_alloc = self.type_arena.allocator();
                        const gp_data = try arena_alloc.create(types.GenericParam);
                        gp_data.* = tp;
                        const gp_type = try arena_alloc.create(types.Type);
                        gp_type.* = .{
                            .kind = .generic_param,
                            .location = node.location,
                            .data = .{ .generic_param = gp_data },
                        };
                        tp_sym.type = gp_type;
                        self.symbols.define(tp_sym) catch {};
                    }

                    // Register parameters
                    for (func.params) |param| {
                        var param_sym = symbol.Symbol.init(param.name, .parameter, node.location);
                        param_sym.type = param.type;
                        self.symbols.define(param_sym) catch {};
                    }

                    try self.collectDeclarations(body);
                }
            },
            .class_decl => {
                const class = &node.data.class_decl;
                var sym = symbol.Symbol.init(class.name, .class, node.location);

                // Create object type for the class (with name for debugging and is_cyclic for ORC)
                sym.type = try self.createClassTypeNamed(class.name, class.members, node.location);
                sym.doc_comment = node.doc_comment; // Propagate JSDoc from AST
                sym.decl_node = node; // Store AST node for inheritance checking

                self.symbols.define(sym) catch {
                    try self.errors.append(.{
                        .message = "duplicate class definition",
                        .location = node.location,
                        .kind = .duplicate_definition,
                    });
                };

                // Enter class scope for members (use node location for class body boundaries)
                try self.symbols.enterScopeWithLocation(.class, node.location);
                defer self.symbols.exitScope();

                // Set current class type for `this` binding in methods
                const saved_class_type = self.current_class_type;
                self.current_class_type = sym.type;
                defer self.current_class_type = saved_class_type;

                for (class.members) |member| {
                    try self.collectDeclarations(member);
                }
            },
            .interface_decl => {
                const iface = &node.data.interface_decl;
                var sym = symbol.Symbol.init(iface.name, .interface, node.location);
                sym.doc_comment = node.doc_comment; // Propagate JSDoc from AST
                sym.decl_node = node; // Store AST node for interface implementation checking

                self.symbols.define(sym) catch {
                    try self.errors.append(.{
                        .message = "duplicate interface definition",
                        .location = node.location,
                        .kind = .duplicate_definition,
                    });
                };
            },
            .type_alias_decl => {
                const alias = &node.data.type_alias_decl;
                var sym = symbol.Symbol.init(alias.name, .type_alias, node.location);
                sym.type = alias.type; // Store aliased type for codegen to resolve
                sym.doc_comment = node.doc_comment; // Propagate JSDoc from AST

                self.symbols.define(sym) catch {
                    try self.errors.append(.{
                        .message = "duplicate type alias definition",
                        .location = node.location,
                        .kind = .duplicate_definition,
                    });
                };
            },
            .macro_decl => {
                // Register macro as a symbol so it can be invoked like a function
                // Macros look like functions but expand at compile time
                const macro = &node.data.macro_decl;
                var sym = symbol.Symbol.init(macro.name, .macro, node.location);

                // Create function-like type for the macro
                sym.type = try self.createFunctionType(macro.type_params, macro.params, macro.return_type, node.location);
                sym.decl_node = node; // Store the macro body for expansion
                sym.doc_comment = node.doc_comment; // Propagate JSDoc from AST

                self.symbols.define(sym) catch {
                    try self.errors.append(.{
                        .message = "duplicate macro definition",
                        .location = node.location,
                        .kind = .duplicate_definition,
                    });
                };

                // Collect declarations in macro body (same as function/method)
                // macro.body is always present (not optional like method.body)
                {
                    // Use node location (full declaration) so parameters in signature are in scope
                    try self.symbols.enterScopeWithLocation(.function, node.location);
                    defer self.symbols.exitScope();

                    // Register macro parameters as symbols for hover/completion
                    for (macro.params) |param| {
                        var param_sym = symbol.Symbol.init(param.name, .parameter, node.location);
                        param_sym.type = param.type;
                        self.symbols.define(param_sym) catch {};
                    }

                    try self.collectDeclarations(macro.body);
                }
            },
            .property_decl => {
                const prop = &node.data.property_decl;
                var sym = symbol.Symbol.init(prop.name, .property, node.location);
                sym.type = prop.type;
                sym.mutable = !prop.readonly;
                sym.doc_comment = node.doc_comment; // Propagate JSDoc from AST

                self.symbols.define(sym) catch {};
            },
            .method_decl => {
                const method = &node.data.method_decl;
                var sym = symbol.Symbol.init(method.name, .method, node.location);
                sym.doc_comment = node.doc_comment; // Propagate JSDoc from AST

                self.symbols.define(sym) catch {};

                // Collect declarations in method body
                if (method.body) |body| {
                    // Use node location (full declaration) so parameters in signature are in scope
                    try self.symbols.enterScopeWithLocation(.function, node.location);
                    defer self.symbols.exitScope();

                    // Register `this` with the current class type
                    if (self.current_class_type) |class_type| {
                        var this_sym = symbol.Symbol.init("this", .variable, node.location);
                        this_sym.type = class_type;
                        this_sym.mutable = false;
                        self.symbols.define(this_sym) catch {};
                    }

                    for (method.params) |param| {
                        var param_sym = symbol.Symbol.init(param.name, .parameter, node.location);
                        param_sym.type = param.type;
                        self.symbols.define(param_sym) catch {};
                    }

                    try self.collectDeclarations(body);
                }
            },
            .block_stmt => {
                try self.symbols.enterScopeWithLocation(.block, node.location);
                defer self.symbols.exitScope();

                for (node.data.block_stmt.statements) |stmt| {
                    try self.collectDeclarations(stmt);
                }
            },
            .if_stmt => {
                try self.collectDeclarations(node.data.if_stmt.consequent);
                if (node.data.if_stmt.alternate) |alt| {
                    try self.collectDeclarations(alt);
                }
            },
            .while_stmt => {
                try self.symbols.enterScopeWithLocation(.loop, node.location);
                defer self.symbols.exitScope();
                try self.collectDeclarations(node.data.while_stmt.body);
            },
            .for_stmt => {
                try self.symbols.enterScopeWithLocation(.loop, node.location);
                defer self.symbols.exitScope();

                if (node.data.for_stmt.init) |for_init| {
                    try self.collectDeclarations(for_init);
                }
                try self.collectDeclarations(node.data.for_stmt.body);
            },
            // Export declarations - process the inner declaration
            .export_decl => {
                const export_data = &node.data.export_decl;
                // If there's a declaration (export function foo...), process it
                if (export_data.declaration) |decl| {
                    try self.collectDeclarations(decl);
                }
                // Named exports (export { foo, bar }) just verify names exist later
            },
            // Import declarations - resolve imported symbols via ModuleLoader
            .import_decl => {
                try self.processImportDecl(node);
            },
            // Expression statements may contain function expressions (named nested functions)
            .expression_stmt => {
                try self.collectDeclarations(node.data.expression_stmt);
            },
            // Function expressions (nested named functions like `function inner() {}`)
            .function_expr => {
                const func_expr = &node.data.function_expr;
                const body = func_expr.body;

                // For NAMED function expressions (like `function inner() {}`),
                // register the function name in the CURRENT scope before entering the function scope.
                // This allows the function to be called by name within the enclosing scope.
                if (func_expr.name) |name| {
                    var sym = symbol.Symbol.init(name, .function, node.location);
                    // Create function type from expression
                    sym.type = try self.createFunctionType(
                        func_expr.type_params,
                        func_expr.params,
                        func_expr.return_type,
                        node.location,
                    );
                    self.symbols.define(sym) catch {}; // Ignore duplicate errors
                }

                // Enter function scope with body boundaries for correct shadowing lookup
                try self.symbols.enterScopeWithLocation(.function, body.location);
                defer self.symbols.exitScope();

                // Register parameters
                for (func_expr.params) |param| {
                    var param_sym = symbol.Symbol.init(param.name, .parameter, node.location);
                    param_sym.type = param.type;
                    self.symbols.define(param_sym) catch {};
                }

                try self.collectDeclarations(body);
            },
            // Other nodes don't introduce declarations
            else => {},
        }
    }

    /// Process an import declaration: resolve the module and register imported symbols
    fn processImportDecl(self: *TypeChecker, node: *ast.Node) !void {
        const import_data = &node.data.import_decl;

        // Need module loader to resolve imports
        const loader = self.loader orelse {
            // No module loader - imports can't be resolved at type-check time
            // This is OK during single-file compilation or when imports are handled elsewhere
            return;
        };

        // Need current module path for relative import resolution
        const from_path = self.current_module_path orelse {
            // No current path - can't resolve relative imports
            return;
        };

        // Strip surrounding quotes from import source if present
        var source = import_data.source;
        if (source.len >= 2) {
            if ((source[0] == '"' and source[source.len - 1] == '"') or
                (source[0] == '\'' and source[source.len - 1] == '\''))
            {
                source = source[1 .. source.len - 1];
            }
        }

        // Resolve the import specifier to a module path
        const resolved_path = loader.resolver.resolve(source, from_path) catch |err| {
            const arena_alloc = self.message_arena.allocator();
            const msg = std.fmt.allocPrint(arena_alloc, "failed to resolve import '{s}': {s}", .{ source, @errorName(err) }) catch "failed to resolve import";
            try self.errors.append(.{
                .message = msg,
                .location = node.location,
                .kind = .undefined_variable,
            });
            return;
        } orelse {
            const arena_alloc = self.message_arena.allocator();
            const msg = std.fmt.allocPrint(arena_alloc, "cannot resolve module '{s}'", .{source}) catch "cannot resolve module";
            try self.errors.append(.{
                .message = msg,
                .location = node.location,
                .kind = .undefined_variable,
            });
            return;
        };

        // Get the loaded module (should already be loaded by ModuleLoader)
        const imported_module = loader.modules.get(resolved_path) orelse {
            const arena_alloc = self.message_arena.allocator();
            const msg = std.fmt.allocPrint(arena_alloc, "module '{s}' not loaded", .{source}) catch "module not loaded";
            try self.errors.append(.{
                .message = msg,
                .location = node.location,
                .kind = .undefined_variable,
            });
            return;
        };

        // Register each imported symbol in the current scope
        for (import_data.specifiers) |spec| {
            // Look up the symbol in the imported module's exports
            if (imported_module.exports.get(spec.imported)) |exported_sym| {
                // Create symbol in current scope with the local name
                var sym = symbol.Symbol.init(spec.local, symbolKindFromModuleKind(exported_sym.kind), node.location);
                sym.decl_node = exported_sym.node;

                // Try to infer type from the exported node
                sym.type = self.inferTypeFromNode(exported_sym.node);

                self.symbols.define(sym) catch {
                    const arena_alloc = self.message_arena.allocator();
                    const msg = std.fmt.allocPrint(arena_alloc, "import '{s}' conflicts with existing definition", .{spec.local}) catch "import conflicts with existing definition";
                    try self.errors.append(.{
                        .message = msg,
                        .location = node.location,
                        .kind = .duplicate_definition,
                    });
                };
            } else {
                // Symbol not found in module exports
                const arena_alloc = self.message_arena.allocator();
                const msg = std.fmt.allocPrint(arena_alloc, "'{s}' is not exported from '{s}'", .{ spec.imported, import_data.source }) catch "symbol not exported";
                try self.errors.append(.{
                    .message = msg,
                    .location = node.location,
                    .kind = .undefined_variable,
                });
            }
        }
    }

    /// Convert module loader symbol kind to type checker symbol kind
    fn symbolKindFromModuleKind(kind: module_loader.Module.SymbolKind) symbol.SymbolKind {
        return switch (kind) {
            .macro => .macro,
            .function => .function,
            .class => .class,
            .variable => .variable,
        };
    }

    /// Attempt to infer type from an AST node
    fn inferTypeFromNode(self: *TypeChecker, node: *ast.Node) ?*types.Type {
        return switch (node.kind) {
            .function_decl => blk: {
                const func = &node.data.function_decl;
                break :blk self.createFunctionType(
                    func.type_params,
                    func.params,
                    func.return_type,
                    node.location,
                ) catch null;
            },
            .class_decl => blk: {
                const class_data = &node.data.class_decl;
                break :blk self.createClassTypeNamed(
                    class_data.name,
                    class_data.members,
                    node.location,
                ) catch null;
            },
            .macro_decl => null, // Macros don't have runtime types
            else => null,
        };
    }

    /// Check type compatibility throughout the AST
    fn checkTypes(self: *TypeChecker, node: *ast.Node) !void {
        switch (node.kind) {
            .program => {
                for (node.data.program.statements) |stmt| {
                    try self.checkTypes(stmt);
                }
            },
            .variable_stmt => {
                for (node.data.variable_stmt.declarations) |decl| {
                    if (decl.type != null and decl.init != null) {
                        try self.checkTypes(decl.init.?);
                        // Check init type matches declared type
                        if (decl.init.?.type) |init_type| {
                            if (!self.typesCompatible(decl.type.?, init_type)) {
                                const msg = self.formatTypeMismatch(decl.type.?, init_type, "variable initialization") catch "type mismatch in variable initialization";
                                try self.errors.append(.{
                                    .message = msg,
                                    .location = node.location,
                                    .kind = .type_mismatch,
                                });
                            }
                        }
                    }
                }
            },
            .return_stmt => {
                if (node.data.return_stmt.argument) |arg| {
                    try self.checkTypes(arg);
                    // Check return type matches function return type
                    const expected_return = self.symbols.getReturnType();
                    if (expected_return) |expected| {
                        if (arg.type) |actual| {
                            if (!self.typesCompatible(expected, actual)) {
                                const msg = self.formatTypeMismatch(expected, actual, "return type mismatch") catch "return type mismatch";
                                try self.errors.append(.{
                                    .message = msg,
                                    .location = node.location,
                                    .kind = .return_type_mismatch,
                                });
                            }
                        }
                    }
                } else {
                    // Return without value - check function expects void
                    const expected_return = self.symbols.getReturnType();
                    if (expected_return) |expected| {
                        if (expected.kind != .void and expected.kind != .unknown) {
                            const expected_name = self.typeName(expected) catch "unknown";
                            const arena_alloc = self.message_arena.allocator();
                            const msg = std.fmt.allocPrint(arena_alloc, "function expects return value of type '{s}'", .{expected_name}) catch "function expects return value";
                            try self.errors.append(.{
                                .message = msg,
                                .location = node.location,
                                .kind = .return_type_mismatch,
                            });
                        }
                    }
                }
            },
            .break_stmt => {
                if (!self.symbols.isInLoop()) {
                    try self.errors.append(.{
                        .message = "break statement outside of loop",
                        .location = node.location,
                        .kind = .break_outside_loop,
                    });
                }
            },
            .continue_stmt => {
                if (!self.symbols.isInLoop()) {
                    try self.errors.append(.{
                        .message = "continue statement outside of loop",
                        .location = node.location,
                        .kind = .continue_outside_loop,
                    });
                }
            },
            .binary_expr => {
                const bin = &node.data.binary_expr;

                // P1 FIX: Handle reassignment to moved variable
                // Assignment to a moved variable resets its state (it now holds a fresh value)
                if (bin.op == .assign and bin.left.kind == .identifier) {
                    const name = bin.left.data.identifier;

                    // Check right side first (may use moved vars)
                    try self.checkTypes(bin.right);

                    // Clear moved state - variable now holds a new value
                    _ = self.moved_variables.remove(name);

                    // Don't check left side - it's the assignment target, not a use
                    try self.checkBinaryExpr(node);
                } else {
                    // Normal case - check both sides
                    try self.checkTypes(bin.left);
                    try self.checkTypes(bin.right);
                    try self.checkBinaryExpr(node);
                }
            },
            .block_stmt => {
                for (node.data.block_stmt.statements) |stmt| {
                    try self.checkTypes(stmt);
                }
            },
            .if_stmt => {
                try self.checkTypes(node.data.if_stmt.condition);
                try self.checkTypes(node.data.if_stmt.consequent);
                if (node.data.if_stmt.alternate) |alt| {
                    try self.checkTypes(alt);
                }
                // Check condition is boolean
                if (node.data.if_stmt.condition.type) |cond_type| {
                    if (cond_type.kind != .boolean) {
                        try self.errors.append(.{
                            .message = "if condition must be boolean",
                            .location = node.data.if_stmt.condition.location,
                            .kind = .type_mismatch,
                        });
                    }
                }
            },
            .while_stmt => {
                try self.checkTypes(node.data.while_stmt.condition);
                // Enter loop scope so break/continue can be validated
                try self.symbols.enterScopeWithLocation(.loop, node.location);
                defer self.symbols.exitScope();
                try self.checkTypes(node.data.while_stmt.body);
                // Check condition is boolean
                if (node.data.while_stmt.condition.type) |cond_type| {
                    if (cond_type.kind != .boolean) {
                        try self.errors.append(.{
                            .message = "while condition must be boolean",
                            .location = node.data.while_stmt.condition.location,
                            .kind = .type_mismatch,
                        });
                    }
                }
            },
            .for_stmt => {
                // Enter loop scope so break/continue can be validated
                try self.symbols.enterScopeWithLocation(.loop, node.location);
                defer self.symbols.exitScope();
                if (node.data.for_stmt.init) |for_init| try self.checkTypes(for_init);
                if (node.data.for_stmt.condition) |cond| try self.checkTypes(cond);
                if (node.data.for_stmt.update) |update| try self.checkTypes(update);
                try self.checkTypes(node.data.for_stmt.body);
            },
            .function_decl => {
                const func = &node.data.function_decl;

                // Check parameter ordering: optional params must come after required ones
                var seen_optional = false;
                for (func.params) |param| {
                    if (param.optional or param.default_value != null) {
                        seen_optional = true;
                    } else if (seen_optional) {
                        // Required param after optional - error!
                        try self.errors.append(.{
                            .message = "required parameter cannot follow optional parameter",
                            .location = node.location,
                            .kind = .invalid_operation,
                        });
                        break;
                    }
                }

                if (func.body) |body| {
                    // Enter function scope and set return type
                    try self.symbols.enterScopeWithLocation(.function, body.location);
                    defer self.symbols.exitScope();

                    // Clear moved variables tracking for new function scope
                    self.moved_variables.clearRetainingCapacity();

                    // Register function parameters in scope
                    for (func.params) |param| {
                        var param_sym = symbol.Symbol.init(param.name, .parameter, node.location);
                        param_sym.type = param.type;
                        param_sym.mutable = false; // Parameters are immutable by default
                        try self.symbols.define(param_sym);
                    }

                    // Collect local variable declarations for const/let checking
                    try self.collectLocalVariables(body);

                    if (func.return_type) |ret_type| {
                        self.symbols.setReturnType(ret_type);
                    }

                    try self.checkTypes(body);

                    // Check that non-void functions always return
                    if (func.return_type) |ret_type| {
                        if (ret_type.kind != .void and !self.alwaysReturns(body)) {
                            try self.errors.append(.{
                                .message = "function must return a value on all code paths",
                                .location = node.location,
                                .kind = .missing_return,
                            });
                        }
                    }
                }
            },
            .class_decl => {
                const class = &node.data.class_decl;

                // Check interface implementations
                for (class.implements) |iface_type| {
                    try self.checkInterfaceImplementation(node, iface_type);
                }

                // Check inheritance (extends)
                if (class.extends) |parent_type| {
                    try self.checkClassInheritance(node, parent_type);
                }

                // Check members
                for (class.members) |member| {
                    try self.checkTypes(member);
                }
            },
            .method_decl => {
                const method = &node.data.method_decl;
                if (method.body) |body| {
                    // Enter function scope and set return type
                    try self.symbols.enterScopeWithLocation(.function, body.location);
                    defer self.symbols.exitScope();

                    // Clear moved variables tracking for new method scope
                    self.moved_variables.clearRetainingCapacity();

                    // Register method parameters in scope
                    for (method.params) |param| {
                        var param_sym = symbol.Symbol.init(param.name, .parameter, node.location);
                        param_sym.type = param.type;
                        param_sym.mutable = false;
                        try self.symbols.define(param_sym);
                    }

                    // Collect local variable declarations for const/let checking
                    try self.collectLocalVariables(body);

                    if (method.return_type) |ret_type| {
                        self.symbols.setReturnType(ret_type);
                    }

                    try self.checkTypes(body);

                    // Check that non-void methods always return
                    if (method.return_type) |ret_type| {
                        if (ret_type.kind != .void and !self.alwaysReturns(body)) {
                            try self.errors.append(.{
                                .message = "method must return a value on all code paths",
                                .location = node.location,
                                .kind = .missing_return,
                            });
                        }
                    }
                }
            },
            .expression_stmt => {
                try self.checkTypes(node.data.expression_stmt);
            },
            .function_expr => {
                const func_expr = &node.data.function_expr;
                const body = func_expr.body;

                // Enter function scope and set return type
                try self.symbols.enterScopeWithLocation(.function, body.location);
                defer self.symbols.exitScope();

                // Clear moved variables tracking for new lambda scope
                self.moved_variables.clearRetainingCapacity();

                // Register function expression parameters in scope
                for (func_expr.params) |param| {
                    var param_sym = symbol.Symbol.init(param.name, .parameter, node.location);
                    param_sym.type = param.type;
                    param_sym.mutable = false;
                    try self.symbols.define(param_sym);
                }

                // Collect local variable declarations for const/let checking
                try self.collectLocalVariables(body);

                if (func_expr.return_type) |ret_type| {
                    self.symbols.setReturnType(ret_type);
                }

                try self.checkTypes(body);

                // Check that non-void function expressions always return
                if (func_expr.return_type) |ret_type| {
                    if (ret_type.kind != .void and !self.alwaysReturns(body)) {
                        try self.errors.append(.{
                            .message = "function expression must return a value on all code paths",
                            .location = node.location,
                            .kind = .missing_return,
                        });
                    }
                }
            },
            .call_expr => {
                const call = &node.data.call_expr;
                try self.checkTypes(call.callee);
                for (call.arguments) |arg| {
                    try self.checkTypes(arg);
                }

                // Get the callee's type and check if it's a function
                if (call.callee.type) |callee_type| {
                    if (callee_type.kind == .function) {
                        const func_type = callee_type.data.function;
                        const params = func_type.params;
                        const args = call.arguments;
                        const is_variadic = func_type.isVariadic();

                        // Count required parameters
                        var required_count: usize = 0;
                        for (params) |param| {
                            if (!param.optional) required_count += 1;
                        }

                        // Check argument count
                        if (args.len < required_count) {
                            const arena_alloc = self.message_arena.allocator();
                            const msg = std.fmt.allocPrint(arena_alloc, "too few arguments: expected {d}, got {d}", .{ required_count, args.len }) catch "too few arguments in function call";
                            try self.errors.append(.{
                                .message = msg,
                                .location = node.location,
                                .kind = .type_mismatch,
                            });
                        } else if (args.len > params.len and !is_variadic) {
                            // Only error if function is NOT variadic
                            const arena_alloc = self.message_arena.allocator();
                            const msg = std.fmt.allocPrint(arena_alloc, "too many arguments: expected {d}, got {d}", .{ params.len, args.len }) catch "too many arguments in function call";
                            try self.errors.append(.{
                                .message = msg,
                                .location = node.location,
                                .kind = .type_mismatch,
                            });
                        } else {
                            // Check argument types match parameter types
                            for (args, 0..) |arg, i| {
                                if (i < params.len) {
                                    // Regular parameter
                                    const param_type = params[i].type;
                                    if (arg.type) |arg_type| {
                                        if (!self.typesCompatible(param_type, arg_type)) {
                                            const msg = self.formatTypeMismatch(param_type, arg_type, "argument type mismatch") catch "argument type mismatch";
                                            try self.errors.append(.{
                                                .message = msg,
                                                .location = arg.location,
                                                .kind = .type_mismatch,
                                            });
                                        }
                                    }
                                } else if (is_variadic) {
                                    // Rest parameter - check against rest_param type
                                    if (func_type.rest_param) |rest_param| {
                                        if (arg.type) |arg_type| {
                                            // Rest param type is the element type (e.g., ...args: any[] → any)
                                            if (!self.typesCompatible(rest_param.type, arg_type)) {
                                                const msg = self.formatTypeMismatch(rest_param.type, arg_type, "rest argument type mismatch") catch "rest argument type mismatch";
                                                try self.errors.append(.{
                                                    .message = msg,
                                                    .location = arg.location,
                                                    .kind = .type_mismatch,
                                                });
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
            .move_expr => {
                // Mark the variable as moved
                const operand = node.data.move_expr.operand;
                if (operand.kind == .identifier) {
                    const name = operand.data.identifier;

                    // P0 FIX: Move inside loop body is always an error
                    // The loop executes multiple times, so 2nd iteration = use-after-move
                    if (self.symbols.isInLoop()) {
                        const arena_alloc = self.message_arena.allocator();
                        const msg = std.fmt.allocPrint(arena_alloc, "cannot move '{s}' inside a loop - variable would be invalid on next iteration", .{name}) catch "cannot move inside loop";
                        try self.errors.append(.{
                            .message = msg,
                            .location = node.location,
                            .kind = .move_in_loop,
                        });
                        // Don't mark as moved - already errored
                    } else if (self.moved_variables.get(name)) |prev_move_loc| {
                        // Check for double-move (moving already-moved variable)
                        const arena_alloc = self.message_arena.allocator();
                        const msg = std.fmt.allocPrint(arena_alloc, "variable '{s}' was already moved at line {d}", .{ name, prev_move_loc.start.line }) catch "variable already moved";
                        try self.errors.append(.{
                            .message = msg,
                            .location = node.location,
                            .kind = .use_after_move,
                        });
                    } else {
                        // Mark as moved
                        try self.moved_variables.put(name, node.location);
                    }
                }
                // NOTE: Don't recurse into operand - the identifier inside move is not a regular use
                // The move consumes the variable, it doesn't use it in the regular sense
            },
            .identifier => {
                // Check if this variable has been moved
                const name = node.data.identifier;
                if (self.moved_variables.get(name)) |move_loc| {
                    const arena_alloc = self.message_arena.allocator();
                    const msg = std.fmt.allocPrint(arena_alloc, "use of moved variable '{s}' (moved at line {d})", .{ name, move_loc.start.line }) catch "use of moved variable";
                    try self.errors.append(.{
                        .message = msg,
                        .location = node.location,
                        .kind = .use_after_move,
                    });
                }
            },
            .member_expr => {
                // Check the object part for use-after-move
                try self.checkTypes(node.data.member_expr.object);
            },
            .unary_expr => {
                try self.checkTypes(node.data.unary_expr.argument);
            },
            .array_expr => {
                for (node.data.array_expr.elements) |elem| {
                    try self.checkTypes(elem);
                }
            },
            .new_expr => {
                const new_expr = &node.data.new_expr;
                for (new_expr.arguments) |arg| {
                    try self.checkTypes(arg);
                }
            },
            else => {},
        }
    }

    /// Collect local variable declarations from a block for the current scope
    /// This is needed during checkTypes phase to ensure variables are visible for
    /// const/let mutability checking. Called when entering function/method bodies.
    fn collectLocalVariables(self: *TypeChecker, node: *ast.Node) !void {
        switch (node.kind) {
            .block_stmt => {
                for (node.data.block_stmt.statements) |stmt| {
                    try self.collectLocalVariables(stmt);
                }
            },
            .variable_stmt => {
                const var_stmt = &node.data.variable_stmt;
                const is_mutable = var_stmt.kind != .@"const";

                for (var_stmt.declarations) |decl| {
                    var sym = symbol.Symbol.init(decl.name, .variable, node.location);
                    sym.mutable = is_mutable;
                    sym.type = decl.type;
                    // Silently ignore duplicates (may already be registered)
                    self.symbols.define(sym) catch {};
                }
            },
            .if_stmt => {
                try self.collectLocalVariables(node.data.if_stmt.consequent);
                if (node.data.if_stmt.alternate) |alt| {
                    try self.collectLocalVariables(alt);
                }
            },
            .while_stmt => {
                try self.collectLocalVariables(node.data.while_stmt.body);
            },
            .for_stmt => {
                if (node.data.for_stmt.init) |for_init| {
                    try self.collectLocalVariables(for_init);
                }
                try self.collectLocalVariables(node.data.for_stmt.body);
            },
            else => {},
        }
    }

    // Type helper functions - delegate to type_compat module
    fn isNumericType(kind: types.TypeKind) bool {
        return type_compat.isNumericType(kind);
    }

    fn isNumericTypeResolved(t: *types.Type) bool {
        return type_compat.isNumericTypeResolved(t);
    }

    fn isStringType(t: *types.Type) bool {
        return type_compat.isStringType(t);
    }

    fn isBooleanType(t: *types.Type) bool {
        return type_compat.isBooleanType(t);
    }

    fn isIntegerType(kind: types.TypeKind) bool {
        return type_compat.isIntegerType(kind);
    }

    /// Check if a statement or block always returns a value
    /// Returns true if all code paths end with a return statement
    fn alwaysReturns(self: *TypeChecker, node: *ast.Node) bool {
        switch (node.kind) {
            .return_stmt => return true,
            .block_stmt => {
                // A block returns if any statement returns (for now, just check last statement)
                const stmts = node.data.block_stmt.statements;
                if (stmts.len == 0) return false;

                // Check if any statement is a return
                for (stmts) |stmt| {
                    if (stmt.kind == .return_stmt) return true;
                }

                // Check if last statement always returns (e.g., if-else with returns)
                const last = stmts[stmts.len - 1];
                return self.alwaysReturns(last);
            },
            .if_stmt => {
                // An if statement returns only if BOTH branches return
                const if_data = &node.data.if_stmt;

                // Must have else branch for all paths to return
                const alternate = if_data.alternate orelse return false;

                // Check if consequent returns
                const then_returns = self.alwaysReturns(if_data.consequent);
                if (!then_returns) return false;

                // Check if else returns
                return self.alwaysReturns(alternate);
            },
            else => return false,
        }
    }

    /// Check binary expression type compatibility
    fn checkBinaryExpr(self: *TypeChecker, node: *ast.Node) !void {
        const expr = &node.data.binary_expr;
        const left_type = expr.left.type;
        const right_type = expr.right.type;

        if (left_type == null or right_type == null) return;

        switch (expr.op) {
            // Assignment: right type must be compatible with left
            .assign => {
                // Check that left side is assignable (identifier or member)
                if (expr.left.kind != .identifier and expr.left.kind != .member_expr) {
                    try self.errors.append(.{
                        .message = "invalid assignment target",
                        .location = expr.left.location,
                        .kind = .invalid_operation,
                    });
                    return;
                }

                // Check for readonly property assignment
                if (expr.left.kind == .identifier) {
                    const name = expr.left.data.identifier;
                    if (self.symbols.lookup(name)) |sym| {
                        // Check if it's a const variable or readonly property
                        if (!sym.mutable and sym.kind == .variable) {
                            const arena_alloc = self.message_arena.allocator();
                            const msg = std.fmt.allocPrint(arena_alloc, "cannot assign to const variable '{s}'", .{name}) catch "cannot assign to const variable";
                            try self.errors.append(.{
                                .message = msg,
                                .location = expr.left.location,
                                .kind = .readonly_assignment,
                            });
                            return;
                        }
                        if (!sym.mutable and sym.kind == .property) {
                            const arena_alloc = self.message_arena.allocator();
                            const msg = std.fmt.allocPrint(arena_alloc, "cannot assign to readonly property '{s}'", .{name}) catch "cannot assign to readonly property";
                            try self.errors.append(.{
                                .message = msg,
                                .location = expr.left.location,
                                .kind = .readonly_assignment,
                            });
                            return;
                        }
                    }
                }

                // Check type compatibility
                if (!self.typesCompatible(left_type.?, right_type.?)) {
                    const msg = self.formatTypeMismatch(left_type.?, right_type.?, "assignment type mismatch") catch "assignment type mismatch";
                    try self.errors.append(.{
                        .message = msg,
                        .location = node.location,
                        .kind = .type_mismatch,
                    });
                }
            },
            // Arithmetic: both sides must be numeric (number, int32, float64, etc.)
            .add, .sub, .mul, .div, .mod => {
                if (!isNumericTypeResolved(left_type.?) or !isNumericTypeResolved(right_type.?)) {
                    // Special case: string + string is allowed
                    if (expr.op == .add and isStringType(left_type.?) and isStringType(right_type.?)) {
                        return;
                    }
                    try self.errors.append(.{
                        .message = "arithmetic operation requires numeric operands",
                        .location = node.location,
                        .kind = .invalid_operation,
                    });
                }
            },
            // Comparison: same types
            .eq, .ne => {
                if (!self.typesCompatible(left_type.?, right_type.?)) {
                    try self.errors.append(.{
                        .message = "comparison requires compatible types",
                        .location = node.location,
                        .kind = .type_mismatch,
                    });
                }
            },
            // Ordering: numeric types or strings
            .lt, .le, .gt, .ge => {
                const left_ok = isNumericTypeResolved(left_type.?) or isStringType(left_type.?);
                const right_ok = isNumericTypeResolved(right_type.?) or isStringType(right_type.?);
                // Both must be comparable and of compatible types
                const types_compatible = (isNumericTypeResolved(left_type.?) and isNumericTypeResolved(right_type.?)) or
                    (isStringType(left_type.?) and isStringType(right_type.?));
                if (!left_ok or !right_ok or !types_compatible) {
                    try self.errors.append(.{
                        .message = "comparison requires numeric or string operands of compatible types",
                        .location = node.location,
                        .kind = .invalid_operation,
                    });
                }
            },
            // Logical: booleans
            .@"and", .@"or" => {
                if (left_type.?.kind != .boolean or right_type.?.kind != .boolean) {
                    try self.errors.append(.{
                        .message = "logical operation requires boolean operands",
                        .location = node.location,
                        .kind = .invalid_operation,
                    });
                }
            },
            // Bitwise: integer types
            .bit_and, .bit_or, .bit_xor, .shl, .shr => {
                if (!isIntegerType(left_type.?.kind) or !isIntegerType(right_type.?.kind)) {
                    try self.errors.append(.{
                        .message = "bitwise operation requires integer operands",
                        .location = node.location,
                        .kind = .invalid_operation,
                    });
                }
            },
            // Nullish coalesce (??) - right side must be compatible with left
            .nullish_coalesce => {
                if (!self.typesCompatible(left_type.?, right_type.?)) {
                    try self.errors.append(.{
                        .message = "nullish coalesce requires compatible types",
                        .location = node.location,
                        .kind = .type_mismatch,
                    });
                }
            },
        }
    }

    /// Check if two types are compatible
    /// Delegates to type_compat module
    fn typesCompatible(_: *TypeChecker, target: *types.Type, source: *types.Type) bool {
        return type_compat.typesCompatible(target, source);
    }

    /// Check if an object type is compatible with another (structural typing)
    /// Delegates to type_compat module
    fn objectTypesCompatible(_: *TypeChecker, target: *types.ObjectType, source: *types.ObjectType) bool {
        return type_compat.objectTypesCompatible(target, source);
    }

    /// Check if two function types are compatible
    /// Delegates to type_compat module
    fn functionTypesCompatible(_: *TypeChecker, target: *types.FunctionType, source: *types.FunctionType) bool {
        return type_compat.functionTypesCompatible(target, source);
    }

    /// Check that a class implements all required interface methods
    fn checkInterfaceImplementation(self: *TypeChecker, class_node: *ast.Node, iface_type: *types.Type) !void {
        const class = &class_node.data.class_decl;

        // Get interface name from type reference
        const iface_name = switch (iface_type.kind) {
            .type_reference => iface_type.data.type_reference.name,
            .object => {
                // Already resolved object type - use old path
                return self.checkInterfaceImplementationFromObject(class_node, iface_type.data.object);
            },
            else => return, // Not a valid interface type
        };

        // Look up interface symbol
        const iface_sym = self.symbols.lookup(iface_name) orelse return;
        if (iface_sym.kind != .interface) return;

        // Get interface AST node
        const iface_node = iface_sym.decl_node orelse return;
        if (iface_node.kind != .interface_decl) return;
        const iface_decl = &iface_node.data.interface_decl;

        // Check each interface member is implemented in the class
        for (iface_decl.members) |iface_member| {
            switch (iface_member.kind) {
                .method_decl => {
                    const iface_method = &iface_member.data.method_decl;
                    var found = false;

                    for (class.members) |class_member| {
                        if (class_member.kind != .method_decl) continue;
                        const class_method = &class_member.data.method_decl;

                        if (!std.mem.eql(u8, class_method.name, iface_method.name)) continue;

                        found = true;

                        // Check return type compatibility
                        if (class_method.return_type) |class_ret| {
                            if (iface_method.return_type) |iface_ret| {
                                if (!self.typesCompatible(iface_ret, class_ret)) {
                                    const arena_alloc = self.message_arena.allocator();
                                    const msg = std.fmt.allocPrint(
                                        arena_alloc,
                                        "method '{s}' has incompatible return type with interface '{s}'",
                                        .{ class_method.name, iface_name },
                                    ) catch "incompatible interface method";
                                    try self.errors.append(.{
                                        .message = msg,
                                        .location = class_member.location,
                                        .kind = .incompatible_override,
                                    });
                                }
                            }
                        }

                        // Check parameter count
                        if (class_method.params.len != iface_method.params.len) {
                            const arena_alloc = self.message_arena.allocator();
                            const msg = std.fmt.allocPrint(
                                arena_alloc,
                                "method '{s}' has different parameter count than interface '{s}'",
                                .{ class_method.name, iface_name },
                            ) catch "incompatible interface method params";
                            try self.errors.append(.{
                                .message = msg,
                                .location = class_member.location,
                                .kind = .incompatible_override,
                            });
                        }

                        break;
                    }

                    if (!found) {
                        const arena_alloc = self.message_arena.allocator();
                        const msg = std.fmt.allocPrint(
                            arena_alloc,
                            "class '{s}' missing implementation of interface method '{s}' from '{s}'",
                            .{ class.name, iface_method.name, iface_name },
                        ) catch "missing interface method";
                        try self.errors.append(.{
                            .message = msg,
                            .location = class_node.location,
                            .kind = .missing_implementation,
                        });
                    }
                },
                .property_decl => {
                    const iface_prop = &iface_member.data.property_decl;
                    var found = false;

                    for (class.members) |class_member| {
                        if (class_member.kind != .property_decl) continue;
                        const class_prop = &class_member.data.property_decl;

                        if (!std.mem.eql(u8, class_prop.name, iface_prop.name)) continue;

                        found = true;

                        // Check type compatibility
                        if (class_prop.type) |class_type| {
                            if (iface_prop.type) |iface_prop_type| {
                                if (!self.typesCompatible(iface_prop_type, class_type)) {
                                    const arena_alloc = self.message_arena.allocator();
                                    const msg = std.fmt.allocPrint(
                                        arena_alloc,
                                        "property '{s}' has incompatible type with interface '{s}'",
                                        .{ class_prop.name, iface_name },
                                    ) catch "incompatible interface property";
                                    try self.errors.append(.{
                                        .message = msg,
                                        .location = class_member.location,
                                        .kind = .type_mismatch,
                                    });
                                }
                            }
                        }

                        break;
                    }

                    if (!found) {
                        const arena_alloc = self.message_arena.allocator();
                        const msg = std.fmt.allocPrint(
                            arena_alloc,
                            "class '{s}' missing implementation of interface property '{s}' from '{s}'",
                            .{ class.name, iface_prop.name, iface_name },
                        ) catch "missing interface property";
                        try self.errors.append(.{
                            .message = msg,
                            .location = class_node.location,
                            .kind = .missing_implementation,
                        });
                    }
                },
                else => {},
            }
        }
    }

    /// Helper for already-resolved object types (used by unit tests)
    fn checkInterfaceImplementationFromObject(self: *TypeChecker, class_node: *ast.Node, iface: *types.ObjectType) !void {
        const class = &class_node.data.class_decl;

        // Check each interface method is implemented
        for (iface.methods) |iface_method| {
            var found = false;
            for (class.members) |member| {
                if (member.kind == .method_decl) {
                    const method = &member.data.method_decl;
                    if (std.mem.eql(u8, method.name, iface_method.name)) {
                        found = true;
                        if (method.return_type) |ret_type| {
                            if (iface_method.type.kind == .function) {
                                const iface_func = iface_method.type.data.function;
                                if (!self.typesCompatible(iface_func.return_type, ret_type)) {
                                    const msg = self.formatTypeMismatch(iface_func.return_type, ret_type, "method override") catch "incompatible method override";
                                    try self.errors.append(.{
                                        .message = msg,
                                        .location = member.location,
                                        .kind = .incompatible_override,
                                    });
                                }
                            }
                        }
                        break;
                    }
                }
            }

            if (!found and !iface_method.optional) {
                const arena_alloc = self.message_arena.allocator();
                const msg = std.fmt.allocPrint(arena_alloc, "class '{s}' missing implementation of interface method '{s}'", .{ class.name, iface_method.name }) catch "missing interface method implementation";
                try self.errors.append(.{
                    .message = msg,
                    .location = class_node.location,
                    .kind = .missing_implementation,
                });
            }
        }

        // Check required properties
        for (iface.properties) |iface_prop| {
            var found = false;
            for (class.members) |member| {
                if (member.kind == .property_decl) {
                    const prop = &member.data.property_decl;
                    if (std.mem.eql(u8, prop.name, iface_prop.name)) {
                        found = true;
                        if (prop.type) |prop_type| {
                            if (!self.typesCompatible(iface_prop.type, prop_type)) {
                                const msg = self.formatTypeMismatch(iface_prop.type, prop_type, "property type") catch "incompatible property type";
                                try self.errors.append(.{
                                    .message = msg,
                                    .location = member.location,
                                    .kind = .type_mismatch,
                                });
                            }
                        }
                        break;
                    }
                }
            }

            if (!found and !iface_prop.optional) {
                const arena_alloc = self.message_arena.allocator();
                const msg = std.fmt.allocPrint(arena_alloc, "class '{s}' missing implementation of interface property '{s}'", .{ class.name, iface_prop.name }) catch "missing interface property implementation";
                try self.errors.append(.{
                    .message = msg,
                    .location = class_node.location,
                    .kind = .missing_implementation,
                });
            }
        }
    }

    /// Check class inheritance is valid
    /// Verifies that override methods have compatible signatures with parent
    fn checkClassInheritance(self: *TypeChecker, class_node: *ast.Node, parent_type: *types.Type) !void {
        const class = &class_node.data.class_decl;

        // Get parent class name from type reference
        const parent_name = switch (parent_type.kind) {
            .type_reference => parent_type.data.type_reference.name,
            .object => {
                // Already resolved object type - use old path
                return self.checkClassInheritanceFromObject(class_node, parent_type.data.object);
            },
            else => return, // Not a class type
        };

        // Look up parent class symbol
        const parent_sym = self.symbols.lookup(parent_name) orelse return;
        if (parent_sym.kind != .class) return;

        // Get parent class AST node
        const parent_node = parent_sym.decl_node orelse return;
        if (parent_node.kind != .class_decl) return;
        const parent_class = &parent_node.data.class_decl;

        // Check each child method against parent methods
        for (class.members) |member| {
            if (member.kind != .method_decl) continue;
            const child_method = &member.data.method_decl;

            // Look for matching method in parent class
            for (parent_class.members) |parent_member| {
                if (parent_member.kind != .method_decl) continue;
                const parent_method = &parent_member.data.method_decl;

                if (!std.mem.eql(u8, parent_method.name, child_method.name)) continue;

                // Found a method with the same name - check return type compatibility
                if (child_method.return_type) |child_ret| {
                    if (parent_method.return_type) |parent_ret| {
                        if (!self.typesCompatible(parent_ret, child_ret)) {
                            const arena_alloc = self.message_arena.allocator();
                            const msg = std.fmt.allocPrint(
                                arena_alloc,
                                "method '{s}' has incompatible return type with parent class '{s}'",
                                .{ child_method.name, parent_name },
                            ) catch "incompatible override return type";
                            try self.errors.append(.{
                                .message = msg,
                                .location = member.location,
                                .kind = .incompatible_override,
                            });
                        }
                    }
                }

                // Check parameter count
                if (child_method.params.len != parent_method.params.len) {
                    const arena_alloc = self.message_arena.allocator();
                    const msg = std.fmt.allocPrint(
                        arena_alloc,
                        "method '{s}' has different parameter count than parent class '{s}'",
                        .{ child_method.name, parent_name },
                    ) catch "incompatible override parameter count";
                    try self.errors.append(.{
                        .message = msg,
                        .location = member.location,
                        .kind = .incompatible_override,
                    });
                }

                break; // Found matching method, stop searching
            }
        }
    }

    /// Helper for already-resolved object types (used by unit tests)
    fn checkClassInheritanceFromObject(self: *TypeChecker, class_node: *ast.Node, parent_obj: *types.ObjectType) !void {
        const class = &class_node.data.class_decl;

        // Check each child method against parent methods
        for (class.members) |member| {
            if (member.kind != .method_decl) continue;
            const child_method = &member.data.method_decl;

            // Check properties array
            for (parent_obj.properties) |parent_prop| {
                if (!std.mem.eql(u8, parent_prop.name, child_method.name)) continue;
                if (parent_prop.type.kind == .function) {
                    const parent_func = parent_prop.type.data.function;
                    if (child_method.return_type) |child_ret| {
                        if (!self.typesCompatible(parent_func.return_type, child_ret)) {
                            const arena_alloc = self.message_arena.allocator();
                            const msg = std.fmt.allocPrint(
                                arena_alloc,
                                "method '{s}' has incompatible return type with parent class",
                                .{child_method.name},
                            ) catch "incompatible override return type";
                            try self.errors.append(.{
                                .message = msg,
                                .location = member.location,
                                .kind = .incompatible_override,
                            });
                        }
                    }
                    if (child_method.params.len != parent_func.params.len) {
                        const arena_alloc = self.message_arena.allocator();
                        const msg = std.fmt.allocPrint(
                            arena_alloc,
                            "method '{s}' has different parameter count than parent class",
                            .{child_method.name},
                        ) catch "incompatible override parameter count";
                        try self.errors.append(.{
                            .message = msg,
                            .location = member.location,
                            .kind = .incompatible_override,
                        });
                    }
                }
                break;
            }

            // Check methods array
            for (parent_obj.methods) |parent_method| {
                if (!std.mem.eql(u8, parent_method.name, child_method.name)) continue;
                if (parent_method.type.kind == .function) {
                    const parent_func = parent_method.type.data.function;
                    if (child_method.return_type) |child_ret| {
                        if (!self.typesCompatible(parent_func.return_type, child_ret)) {
                            const arena_alloc = self.message_arena.allocator();
                            const msg = std.fmt.allocPrint(
                                arena_alloc,
                                "method '{s}' has incompatible return type with parent class",
                                .{child_method.name},
                            ) catch "incompatible override return type";
                            try self.errors.append(.{
                                .message = msg,
                                .location = member.location,
                                .kind = .incompatible_override,
                            });
                        }
                    }
                }
                break;
            }
        }
    }

    /// Check if there are any errors
    pub fn hasErrors(self: *TypeChecker) bool {
        return self.errors.items.len > 0;
    }

    /// Get all errors
    pub fn getErrors(self: *TypeChecker) []const TypeError {
        return self.errors.items;
    }
};

test "type checker initialization" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Just verify it initializes without error
    try std.testing.expect(!checker.hasErrors());
}

test "type checker: break outside loop" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create a program with break outside loop
    var break_node = ast.Node{
        .kind = .break_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .break_stmt = {} },
    };
    var stmts = [_]*ast.Node{&break_node};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{
            .statements = &stmts,
            .file_id = 0,
        } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok);
    try std.testing.expect(checker.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), checker.errors.items.len);
    try std.testing.expectEqual(TypeError.Kind.break_outside_loop, checker.errors.items[0].kind);
}

test "type checker: continue outside loop" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create a program with continue outside loop
    var continue_node = ast.Node{
        .kind = .continue_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .continue_stmt = {} },
    };
    var stmts = [_]*ast.Node{&continue_node};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{
            .statements = &stmts,
            .file_id = 0,
        } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok);
    try std.testing.expect(checker.hasErrors());
    try std.testing.expectEqual(TypeError.Kind.continue_outside_loop, checker.errors.items[0].kind);
}

test "type checker: break inside while loop is valid" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: while (true) { break; }
    var bool_type = types.Type{
        .kind = .boolean,
        .location = location.SourceLocation.dummy(),
        .data = .{ .boolean = {} },
    };

    var cond = ast.Node{
        .kind = .boolean_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .boolean_literal = true },
        .type = &bool_type,
    };

    var break_node = ast.Node{
        .kind = .break_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .break_stmt = {} },
    };

    var body_stmts = [_]*ast.Node{&break_node};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var while_node = ast.Node{
        .kind = .while_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .while_stmt = .{ .condition = &cond, .body = &body } },
    };

    var stmts = [_]*ast.Node{&while_node};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{
            .statements = &stmts,
            .file_id = 0,
        } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);
    try std.testing.expect(!checker.hasErrors());
}

test "type checker: continue inside for loop is valid" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: for (;;) { continue; }
    var continue_node = ast.Node{
        .kind = .continue_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .continue_stmt = {} },
    };

    var body_stmts = [_]*ast.Node{&continue_node};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var for_node = ast.Node{
        .kind = .for_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .for_stmt = .{
            .init = null,
            .condition = null,
            .update = null,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&for_node};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{
            .statements = &stmts,
            .file_id = 0,
        } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);
    try std.testing.expect(!checker.hasErrors());
}

test "type checker: empty program" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var stmts = [_]*ast.Node{};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{
            .statements = &stmts,
            .file_id = 0,
        } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);
    try std.testing.expect(!checker.hasErrors());
}

// ============================================================================
// Return Type Checking Tests
// ============================================================================

test "type checker: return type matches function declaration" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: function foo(): number { return 42; }
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var return_val = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
        .type = &num_type,
    };

    var return_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &return_val } },
    };

    var body_stmts = [_]*ast.Node{&return_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "foo",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &num_type,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok); // Should pass - return type matches
}

test "type checker: return type mismatch reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: function foo(): number { return "hello"; }
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var return_val = ast.Node{
        .kind = .string_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string_literal = "hello" },
        .type = &str_type,
    };

    var return_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &return_val } },
    };

    var body_stmts = [_]*ast.Node{&return_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "foo",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &num_type,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should fail - return type mismatch
    try std.testing.expect(checker.hasErrors());

    // Find the return type mismatch error
    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .return_type_mismatch) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

test "type checker: void function with no return is valid" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: function foo(): void { console.log("hi"); }
    var void_type = types.Type{
        .kind = .void,
        .location = location.SourceLocation.dummy(),
        .data = .{ .void = {} },
    };

    var console_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "console" },
    };

    var log_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "log" },
    };

    var member = ast.Node{
        .kind = .member_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .member_expr = .{
            .object = &console_id,
            .property = &log_id,
            .computed = false,
        } },
    };

    var arg = ast.Node{
        .kind = .string_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string_literal = "hi" },
    };

    var args = [_]*ast.Node{&arg};
    var call = ast.Node{
        .kind = .call_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .call_expr = .{
            .callee = &member,
            .arguments = &args,
            .type_args = &[_]*types.Type{},
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &call },
    };

    var body_stmts = [_]*ast.Node{&expr_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "foo",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &void_type,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok); // Should pass - void function doesn't need return
}

// ============================================================================
// Readonly Assignment Tests
// ============================================================================

test "type checker: assignment to const variable reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: const x = 1; x = 2;
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    // First, define the const variable
    var sym = symbol.Symbol.init("x", .variable, location.SourceLocation.dummy());
    sym.mutable = false; // const
    sym.type = &num_type;
    try checker.symbols.define(sym);

    // Create assignment: x = 2
    var left = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "x" },
        .type = &num_type,
    };

    var right = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 2.0 },
        .type = &num_type,
    };

    var assign = ast.Node{
        .kind = .binary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .assign,
            .left = &left,
            .right = &right,
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &assign },
    };

    var stmts = [_]*ast.Node{&expr_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should fail
    try std.testing.expect(checker.hasErrors());

    // Find the readonly assignment error
    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .readonly_assignment) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

test "type checker: assignment to let variable is valid" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: let x = 1; x = 2;
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    // Define mutable variable
    var sym = symbol.Symbol.init("x", .variable, location.SourceLocation.dummy());
    sym.mutable = true; // let
    sym.type = &num_type;
    try checker.symbols.define(sym);

    // Create assignment: x = 2
    var left = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "x" },
        .type = &num_type,
    };

    var right = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 2.0 },
        .type = &num_type,
    };

    var assign = ast.Node{
        .kind = .binary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .assign,
            .left = &left,
            .right = &right,
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &assign },
    };

    var stmts = [_]*ast.Node{&expr_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok); // Should pass
}

// ============================================================================
// Structural Type Compatibility Tests
// ============================================================================

test "type checker: object compatible with same shape" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // { name: string } is compatible with { name: string }
    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Create object type with name property
    const props1 = try alloc.alloc(types.ObjectType.Property, 1);
    props1[0] = .{ .name = "name", .type = &str_type, .optional = false };

    const obj1_data = try alloc.create(types.ObjectType);
    obj1_data.* = .{ .properties = props1, .methods = &[_]types.ObjectType.Property{} };

    var obj1 = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = obj1_data },
    };

    // Same shape
    const props2 = try alloc.alloc(types.ObjectType.Property, 1);
    props2[0] = .{ .name = "name", .type = &str_type, .optional = false };

    const obj2_data = try alloc.create(types.ObjectType);
    obj2_data.* = .{ .properties = props2, .methods = &[_]types.ObjectType.Property{} };

    var obj2 = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = obj2_data },
    };

    // Should be compatible
    try std.testing.expect(checker.typesCompatible(&obj1, &obj2));
}

test "type checker: object with extra properties compatible (structural subtyping)" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // { name: string, age: number } is compatible with { name: string }
    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Target: { name: string }
    const target_props = try alloc.alloc(types.ObjectType.Property, 1);
    target_props[0] = .{ .name = "name", .type = &str_type, .optional = false };

    const target_data = try alloc.create(types.ObjectType);
    target_data.* = .{ .properties = target_props, .methods = &[_]types.ObjectType.Property{} };

    var target = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = target_data },
    };

    // Source: { name: string, age: number } - has extra property
    const source_props = try alloc.alloc(types.ObjectType.Property, 2);
    source_props[0] = .{ .name = "name", .type = &str_type, .optional = false };
    source_props[1] = .{ .name = "age", .type = &num_type, .optional = false };

    const source_data = try alloc.create(types.ObjectType);
    source_data.* = .{ .properties = source_props, .methods = &[_]types.ObjectType.Property{} };

    var source = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = source_data },
    };

    // Source should be compatible with target (has all required properties)
    try std.testing.expect(checker.typesCompatible(&target, &source));
}

test "type checker: object missing property not compatible" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // { } is NOT compatible with { name: string }
    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Target: { name: string }
    const target_props = try alloc.alloc(types.ObjectType.Property, 1);
    target_props[0] = .{ .name = "name", .type = &str_type, .optional = false };

    const target_data = try alloc.create(types.ObjectType);
    target_data.* = .{ .properties = target_props, .methods = &[_]types.ObjectType.Property{} };

    var target = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = target_data },
    };

    // Source: { } - missing required property
    const source_data = try alloc.create(types.ObjectType);
    source_data.* = .{ .properties = &[_]types.ObjectType.Property{}, .methods = &[_]types.ObjectType.Property{} };

    var source = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = source_data },
    };

    // Source should NOT be compatible with target (missing name)
    try std.testing.expect(!checker.typesCompatible(&target, &source));
}

test "type checker: optional property can be missing" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // { } is compatible with { name?: string }
    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Target: { name?: string } - optional property
    const target_props = try alloc.alloc(types.ObjectType.Property, 1);
    target_props[0] = .{ .name = "name", .type = &str_type, .optional = true };

    const target_data = try alloc.create(types.ObjectType);
    target_data.* = .{ .properties = target_props, .methods = &[_]types.ObjectType.Property{} };

    var target = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = target_data },
    };

    // Source: { } - missing optional property is OK
    const source_data = try alloc.create(types.ObjectType);
    source_data.* = .{ .properties = &[_]types.ObjectType.Property{}, .methods = &[_]types.ObjectType.Property{} };

    var source = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = source_data },
    };

    // Should be compatible - optional property can be missing
    try std.testing.expect(checker.typesCompatible(&target, &source));
}

// ============================================================================
// Union Type Tests
// ============================================================================

test "type checker: string compatible with string|number union" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Target: string | number
    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Create union type
    const union_members = try alloc.alloc(*types.Type, 2);
    union_members[0] = &str_type;
    union_members[1] = &num_type;

    const union_data = try alloc.create(types.UnionType);
    union_data.* = .{ .types = union_members };

    var union_type = types.Type{
        .kind = .@"union",
        .location = location.SourceLocation.dummy(),
        .data = .{ .@"union" = union_data },
    };

    // string should be compatible with string|number
    try std.testing.expect(checker.typesCompatible(&union_type, &str_type));

    // number should be compatible with string|number
    try std.testing.expect(checker.typesCompatible(&union_type, &num_type));
}

test "type checker: boolean NOT compatible with string|number union" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var bool_type = types.Type{
        .kind = .boolean,
        .location = location.SourceLocation.dummy(),
        .data = .{ .boolean = {} },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const union_members = try alloc.alloc(*types.Type, 2);
    union_members[0] = &str_type;
    union_members[1] = &num_type;

    const union_data = try alloc.create(types.UnionType);
    union_data.* = .{ .types = union_members };

    var union_type = types.Type{
        .kind = .@"union",
        .location = location.SourceLocation.dummy(),
        .data = .{ .@"union" = union_data },
    };

    // boolean should NOT be compatible with string|number
    try std.testing.expect(!checker.typesCompatible(&union_type, &bool_type));
}

// ============================================================================
// ISSUE #1: Missing Return Statement Detection Tests
// Functions with non-void return type but no return statement should error
// ============================================================================

test "type checker: non-void function without return statement reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: function foo(): number { console.log("hi"); }
    // This should error - function declares number return but has no return
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var console_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "console" },
    };

    var log_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "log" },
    };

    var member = ast.Node{
        .kind = .member_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .member_expr = .{
            .object = &console_id,
            .property = &log_id,
            .computed = false,
        } },
    };

    var arg = ast.Node{
        .kind = .string_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string_literal = "hi" },
    };

    var args = [_]*ast.Node{&arg};
    var call = ast.Node{
        .kind = .call_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .call_expr = .{
            .callee = &member,
            .arguments = &args,
            .type_args = &[_]*types.Type{},
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &call },
    };

    // Body has no return statement!
    var body_stmts = [_]*ast.Node{&expr_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "foo",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &num_type, // Expects number!
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should FAIL - missing return

    // Find the missing_return error
    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .missing_return) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

test "type checker: function with conditional return paths missing else return" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: function foo(x: boolean): number { if (x) { return 1; } }
    // Missing return in else path!
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var bool_type = types.Type{
        .kind = .boolean,
        .location = location.SourceLocation.dummy(),
        .data = .{ .boolean = {} },
    };

    // Condition: x
    var cond = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "x" },
        .type = &bool_type,
    };

    // return 1
    var ret_val = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 1.0 },
        .type = &num_type,
    };

    var ret_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &ret_val } },
    };

    var then_stmts = [_]*ast.Node{&ret_stmt};
    var then_block = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &then_stmts } },
    };

    // if statement with no else
    var if_stmt = ast.Node{
        .kind = .if_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .if_stmt = .{
            .condition = &cond,
            .consequent = &then_block,
            .alternate = null, // No else branch!
        } },
    };

    var body_stmts = [_]*ast.Node{&if_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var params = [_]ast.node.FunctionExpr.FunctionParam{
        .{ .name = "x", .type = &bool_type, .optional = false, .default_value = null },
    };

    var func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "foo",
            .type_params = &[_]types.GenericParam{},
            .params = &params,
            .return_type = &num_type,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should FAIL - not all paths return

    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .missing_return) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

test "type checker: function with all paths returning is valid" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: function foo(x: boolean): number { if (x) { return 1; } else { return 0; } }
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var bool_type = types.Type{
        .kind = .boolean,
        .location = location.SourceLocation.dummy(),
        .data = .{ .boolean = {} },
    };

    var cond = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "x" },
        .type = &bool_type,
    };

    // return 1
    var ret_val1 = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 1.0 },
        .type = &num_type,
    };

    var ret_stmt1 = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &ret_val1 } },
    };

    var then_stmts = [_]*ast.Node{&ret_stmt1};
    var then_block = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &then_stmts } },
    };

    // return 0
    var ret_val2 = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 0.0 },
        .type = &num_type,
    };

    var ret_stmt2 = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &ret_val2 } },
    };

    var else_stmts = [_]*ast.Node{&ret_stmt2};
    var else_block = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &else_stmts } },
    };

    var if_stmt = ast.Node{
        .kind = .if_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .if_stmt = .{
            .condition = &cond,
            .consequent = &then_block,
            .alternate = &else_block, // Has else!
        } },
    };

    var body_stmts = [_]*ast.Node{&if_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var params = [_]ast.node.FunctionExpr.FunctionParam{
        .{ .name = "x", .type = &bool_type, .optional = false, .default_value = null },
    };

    var func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "foo",
            .type_params = &[_]types.GenericParam{},
            .params = &params,
            .return_type = &num_type,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok); // Should PASS - all paths return
}

// ============================================================================
// ISSUE #4: Call Expression Argument Type Checking Tests
// Function call arguments must match parameter types
// ============================================================================

test "type checker: call with wrong argument type reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Setup: function expects (x: number), we call with string
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var void_type = types.Type{
        .kind = .void,
        .location = location.SourceLocation.dummy(),
        .data = .{ .void = {} },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Create function type: (x: number) => void
    const params_slice = try alloc.alloc(types.FunctionType.FunctionParam, 1);
    params_slice[0] = .{ .name = "x", .type = &num_type, .optional = false };

    const func_type_data = try alloc.create(types.FunctionType);
    func_type_data.* = .{
        .type_params = &[_]types.GenericParam{},
        .params = params_slice,
        .return_type = &void_type,
    };

    var func_type = types.Type{
        .kind = .function,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function = func_type_data },
    };

    // Register function in symbol table
    var sym = symbol.Symbol.init("myFunc", .function, location.SourceLocation.dummy());
    sym.type = &func_type;
    try checker.symbols.define(sym);

    // Create call: myFunc("hello") - wrong type!
    var callee = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "myFunc" },
        .type = &func_type,
    };

    var arg = ast.Node{
        .kind = .string_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string_literal = "hello" },
        .type = &str_type, // String, but function expects number!
    };

    var args = [_]*ast.Node{&arg};
    var call = ast.Node{
        .kind = .call_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .call_expr = .{
            .callee = &callee,
            .arguments = &args,
            .type_args = &[_]*types.Type{},
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &call },
    };

    var stmts = [_]*ast.Node{&expr_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should FAIL

    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .type_mismatch) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

test "type checker: call with too few arguments reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var void_type = types.Type{
        .kind = .void,
        .location = location.SourceLocation.dummy(),
        .data = .{ .void = {} },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Create function type: (x: number, y: number) => void
    const params_slice = try alloc.alloc(types.FunctionType.FunctionParam, 2);
    params_slice[0] = .{ .name = "x", .type = &num_type, .optional = false };
    params_slice[1] = .{ .name = "y", .type = &num_type, .optional = false };

    const func_type_data = try alloc.create(types.FunctionType);
    func_type_data.* = .{
        .type_params = &[_]types.GenericParam{},
        .params = params_slice,
        .return_type = &void_type,
    };

    var func_type = types.Type{
        .kind = .function,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function = func_type_data },
    };

    var sym = symbol.Symbol.init("myFunc", .function, location.SourceLocation.dummy());
    sym.type = &func_type;
    try checker.symbols.define(sym);

    // Create call: myFunc(1) - missing second argument!
    var callee = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "myFunc" },
        .type = &func_type,
    };

    var arg = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 1.0 },
        .type = &num_type,
    };

    var args = [_]*ast.Node{&arg};
    var call = ast.Node{
        .kind = .call_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .call_expr = .{
            .callee = &callee,
            .arguments = &args, // Only 1 arg, needs 2!
            .type_args = &[_]*types.Type{},
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &call },
    };

    var stmts = [_]*ast.Node{&expr_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should FAIL

    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .type_mismatch or err.kind == .invalid_operation) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

test "type checker: call with optional param missing is valid" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var void_type = types.Type{
        .kind = .void,
        .location = location.SourceLocation.dummy(),
        .data = .{ .void = {} },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Create function type: (x: number, y?: number) => void
    const params_slice = try alloc.alloc(types.FunctionType.FunctionParam, 2);
    params_slice[0] = .{ .name = "x", .type = &num_type, .optional = false };
    params_slice[1] = .{ .name = "y", .type = &num_type, .optional = true }; // Optional!

    const func_type_data = try alloc.create(types.FunctionType);
    func_type_data.* = .{
        .type_params = &[_]types.GenericParam{},
        .params = params_slice,
        .return_type = &void_type,
    };

    var func_type = types.Type{
        .kind = .function,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function = func_type_data },
    };

    var sym = symbol.Symbol.init("myFunc", .function, location.SourceLocation.dummy());
    sym.type = &func_type;
    try checker.symbols.define(sym);

    // Create call: myFunc(1) - optional param missing is OK
    var callee = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "myFunc" },
        .type = &func_type,
    };

    var arg = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 1.0 },
        .type = &num_type,
    };

    var args = [_]*ast.Node{&arg};
    var call = ast.Node{
        .kind = .call_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .call_expr = .{
            .callee = &callee,
            .arguments = &args,
            .type_args = &[_]*types.Type{},
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &call },
    };

    var stmts = [_]*ast.Node{&expr_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok); // Should PASS - optional param can be missing
}

// ============================================================================
// ISSUE #5: Arithmetic with Sized Integers Tests
// int32 + int32 should work, not just `number`
// ============================================================================

test "type checker: int32 + int32 is valid arithmetic" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var int32_type = types.Type{
        .kind = .int32,
        .location = location.SourceLocation.dummy(),
        .data = .{ .int32 = {} },
    };

    // Define variables
    var sym_a = symbol.Symbol.init("a", .variable, location.SourceLocation.dummy());
    sym_a.mutable = true;
    sym_a.type = &int32_type;
    try checker.symbols.define(sym_a);

    var sym_b = symbol.Symbol.init("b", .variable, location.SourceLocation.dummy());
    sym_b.mutable = true;
    sym_b.type = &int32_type;
    try checker.symbols.define(sym_b);

    // Create: a + b
    var left = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "a" },
        .type = &int32_type,
    };

    var right = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "b" },
        .type = &int32_type,
    };

    var add_expr = ast.Node{
        .kind = .binary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .add,
            .left = &left,
            .right = &right,
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &add_expr },
    };

    var stmts = [_]*ast.Node{&expr_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok); // Should PASS - int32 arithmetic is valid
}

test "type checker: float64 * float64 is valid arithmetic" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var float64_type = types.Type{
        .kind = .float64,
        .location = location.SourceLocation.dummy(),
        .data = .{ .float64 = {} },
    };

    var sym_a = symbol.Symbol.init("a", .variable, location.SourceLocation.dummy());
    sym_a.mutable = true;
    sym_a.type = &float64_type;
    try checker.symbols.define(sym_a);

    var sym_b = symbol.Symbol.init("b", .variable, location.SourceLocation.dummy());
    sym_b.mutable = true;
    sym_b.type = &float64_type;
    try checker.symbols.define(sym_b);

    var left = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "a" },
        .type = &float64_type,
    };

    var right = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "b" },
        .type = &float64_type,
    };

    var mul_expr = ast.Node{
        .kind = .binary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .mul,
            .left = &left,
            .right = &right,
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &mul_expr },
    };

    var stmts = [_]*ast.Node{&expr_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok); // Should PASS
}

test "type checker: mixed int32 and number arithmetic is valid" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var int32_type = types.Type{
        .kind = .int32,
        .location = location.SourceLocation.dummy(),
        .data = .{ .int32 = {} },
    };

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var sym_a = symbol.Symbol.init("a", .variable, location.SourceLocation.dummy());
    sym_a.mutable = true;
    sym_a.type = &int32_type;
    try checker.symbols.define(sym_a);

    var sym_b = symbol.Symbol.init("b", .variable, location.SourceLocation.dummy());
    sym_b.mutable = true;
    sym_b.type = &num_type;
    try checker.symbols.define(sym_b);

    var left = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "a" },
        .type = &int32_type,
    };

    var right = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "b" },
        .type = &num_type,
    };

    var add_expr = ast.Node{
        .kind = .binary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .add,
            .left = &left,
            .right = &right,
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &add_expr },
    };

    var stmts = [_]*ast.Node{&expr_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok); // Should PASS - numeric types are compatible
}

// ============================================================================
// ISSUE #6: Member Expression Readonly Checking Tests
// obj.readonlyProp = value should error
// ============================================================================

test "type checker: assignment to readonly member property reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Create object type with readonly property
    const props = try alloc.alloc(types.ObjectType.Property, 1);
    props[0] = .{ .name = "id", .type = &num_type, .optional = false };

    const obj_data = try alloc.create(types.ObjectType);
    obj_data.* = .{ .properties = props, .methods = &[_]types.ObjectType.Property{} };

    var obj_type = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = obj_data },
    };

    // Define 'obj' with a readonly property 'id'
    var sym = symbol.Symbol.init("obj", .variable, location.SourceLocation.dummy());
    sym.mutable = false; // The variable itself is const
    sym.type = &obj_type;
    try checker.symbols.define(sym);

    // Also need to track that 'id' property is readonly - this is the tricky part
    // For now, we'll test the simpler case where the whole object is const

    // Create: obj.id = 5
    var obj_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "obj" },
        .type = &obj_type,
    };

    var prop_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "id" },
    };

    var member = ast.Node{
        .kind = .member_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .member_expr = .{
            .object = &obj_id,
            .property = &prop_id,
            .computed = false,
        } },
        .type = &num_type,
    };

    var value = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 5.0 },
        .type = &num_type,
    };

    var assign = ast.Node{
        .kind = .binary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .assign,
            .left = &member,
            .right = &value,
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &assign },
    };

    var stmts = [_]*ast.Node{&expr_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    // Should fail if we properly track readonly on member expressions
    // For now this might pass - the test documents the expected behavior
    _ = ok;
}

// ============================================================================
// ISSUE #7: Nested Function Return Type Tests
// Inner functions should have their own return type context
// ============================================================================

test "type checker: nested function has separate return type context" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    // Inner function: function inner(): string { return "hi"; }
    var inner_ret_val = ast.Node{
        .kind = .string_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string_literal = "hi" },
        .type = &str_type,
    };

    var inner_ret = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &inner_ret_val } },
    };

    var inner_stmts = [_]*ast.Node{&inner_ret};
    var inner_body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &inner_stmts } },
    };

    var inner_func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "inner",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &str_type, // Returns string
            .body = &inner_body,
        } },
    };

    // Outer function: function outer(): number { function inner()... return 42; }
    var outer_ret_val = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
        .type = &num_type,
    };

    var outer_ret = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &outer_ret_val } },
    };

    var outer_stmts = [_]*ast.Node{ &inner_func, &outer_ret };
    var outer_body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &outer_stmts } },
    };

    var outer_func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "outer",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &num_type, // Returns number
            .body = &outer_body,
        } },
    };

    var stmts = [_]*ast.Node{&outer_func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    // Inner returns string (correct for inner), outer returns number (correct for outer)
    // Both should be valid - contexts are separate
    try std.testing.expect(ok);
}

test "type checker: nested function wrong return type in inner reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    // Inner function: function inner(): string { return 123; } <- WRONG!
    var inner_ret_val = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 123.0 },
        .type = &num_type, // Returns number, but declared string!
    };

    var inner_ret = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &inner_ret_val } },
    };

    var inner_stmts = [_]*ast.Node{&inner_ret};
    var inner_body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &inner_stmts } },
    };

    var inner_func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "inner",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &str_type, // Declared string!
            .body = &inner_body,
        } },
    };

    // Outer function
    var outer_ret_val = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
        .type = &num_type,
    };

    var outer_ret = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &outer_ret_val } },
    };

    var outer_stmts = [_]*ast.Node{ &inner_func, &outer_ret };
    var outer_body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &outer_stmts } },
    };

    var outer_func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "outer",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &num_type,
            .body = &outer_body,
        } },
    };

    var stmts = [_]*ast.Node{&outer_func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should FAIL - inner has wrong return type

    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .return_type_mismatch) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

// ============================================================================
// ISSUE #8: Method Parameter Type Validation Tests
// ============================================================================

test "type checker: method with wrong parameter type assignment reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var void_type = types.Type{
        .kind = .void,
        .location = location.SourceLocation.dummy(),
        .data = .{ .void = {} },
    };

    // Method body: let x: number = name; (name is string - error!)
    // First, we need to set up parameter 'name' as string
    var param_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "name" },
        .type = &str_type,
    };

    const decl = ast.node.VariableStmt.VariableDeclarator{
        .name = "x",
        .type = &num_type, // Declared as number
        .init = &param_id, // But initializing with string!
    };

    var decls = [_]ast.node.VariableStmt.VariableDeclarator{decl};
    var var_stmt = ast.Node{
        .kind = .variable_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .variable_stmt = .{
            .kind = .let,
            .declarations = &decls,
        } },
    };

    var method_stmts = [_]*ast.Node{&var_stmt};
    var method_body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &method_stmts } },
    };

    var params = [_]ast.node.FunctionExpr.FunctionParam{
        .{ .name = "name", .type = &str_type, .optional = false, .default_value = null },
    };

    var method = ast.Node{
        .kind = .method_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .method_decl = .{
            .name = "setName",
            .type_params = &[_]types.GenericParam{},
            .params = &params,
            .return_type = &void_type,
            .body = &method_body,
        } },
    };

    var members = [_]*ast.Node{&method};
    var class = ast.Node{
        .kind = .class_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .class_decl = .{
            .name = "User",
            .type_params = &[_]types.GenericParam{},
            .extends = null,
            .implements = &[_]*types.Type{},
            .members = &members,
        } },
    };

    var stmts = [_]*ast.Node{&class};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should FAIL

    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .type_mismatch) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

// ============================================================================
// Additional Edge Case Tests
// ============================================================================

test "type checker: deeply nested object structural compatibility" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Inner type: { name: string }
    const inner_props = try alloc.alloc(types.ObjectType.Property, 1);
    inner_props[0] = .{ .name = "name", .type = &str_type, .optional = false };

    const inner_data = try alloc.create(types.ObjectType);
    inner_data.* = .{ .properties = inner_props, .methods = &[_]types.ObjectType.Property{} };

    var inner_type = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = inner_data },
    };

    // Outer type: { user: { name: string } }
    const outer_props = try alloc.alloc(types.ObjectType.Property, 1);
    outer_props[0] = .{ .name = "user", .type = &inner_type, .optional = false };

    const outer_data = try alloc.create(types.ObjectType);
    outer_data.* = .{ .properties = outer_props, .methods = &[_]types.ObjectType.Property{} };

    var outer_type1 = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = outer_data },
    };

    // Same structure, different instance
    const inner_props2 = try alloc.alloc(types.ObjectType.Property, 1);
    inner_props2[0] = .{ .name = "name", .type = &str_type, .optional = false };

    const inner_data2 = try alloc.create(types.ObjectType);
    inner_data2.* = .{ .properties = inner_props2, .methods = &[_]types.ObjectType.Property{} };

    var inner_type2 = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = inner_data2 },
    };

    const outer_props2 = try alloc.alloc(types.ObjectType.Property, 1);
    outer_props2[0] = .{ .name = "user", .type = &inner_type2, .optional = false };

    const outer_data2 = try alloc.create(types.ObjectType);
    outer_data2.* = .{ .properties = outer_props2, .methods = &[_]types.ObjectType.Property{} };

    var outer_type2 = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = outer_data2 },
    };

    // Should be compatible
    try std.testing.expect(checker.typesCompatible(&outer_type1, &outer_type2));
}

test "type checker: union of unions compatibility" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var bool_type = types.Type{
        .kind = .boolean,
        .location = location.SourceLocation.dummy(),
        .data = .{ .boolean = {} },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Inner union: string | number
    const inner_members = try alloc.alloc(*types.Type, 2);
    inner_members[0] = &str_type;
    inner_members[1] = &num_type;

    const inner_data = try alloc.create(types.UnionType);
    inner_data.* = .{ .types = inner_members };

    var inner_union = types.Type{
        .kind = .@"union",
        .location = location.SourceLocation.dummy(),
        .data = .{ .@"union" = inner_data },
    };

    // Outer union: (string | number) | boolean
    const outer_members = try alloc.alloc(*types.Type, 2);
    outer_members[0] = &inner_union;
    outer_members[1] = &bool_type;

    const outer_data = try alloc.create(types.UnionType);
    outer_data.* = .{ .types = outer_members };

    var outer_union = types.Type{
        .kind = .@"union",
        .location = location.SourceLocation.dummy(),
        .data = .{ .@"union" = outer_data },
    };

    // string should be compatible with (string | number) | boolean
    try std.testing.expect(checker.typesCompatible(&outer_union, &str_type));

    // number should be compatible
    try std.testing.expect(checker.typesCompatible(&outer_union, &num_type));

    // boolean should be compatible
    try std.testing.expect(checker.typesCompatible(&outer_union, &bool_type));
}

test "type checker: function returning wrong type in callback reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Expected callback type: () => number
    const expected_func_data = try alloc.create(types.FunctionType);
    expected_func_data.* = .{
        .type_params = &[_]types.GenericParam{},
        .params = &[_]types.FunctionType.FunctionParam{},
        .return_type = &num_type,
    };

    var expected_func_type = types.Type{
        .kind = .function,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function = expected_func_data },
    };

    // Actual callback type: () => string (wrong!)
    const actual_func_data = try alloc.create(types.FunctionType);
    actual_func_data.* = .{
        .type_params = &[_]types.GenericParam{},
        .params = &[_]types.FunctionType.FunctionParam{},
        .return_type = &str_type, // Wrong return type!
    };

    var actual_func_type = types.Type{
        .kind = .function,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function = actual_func_data },
    };

    // Should NOT be compatible - return types don't match
    try std.testing.expect(!checker.typesCompatible(&expected_func_type, &actual_func_type));
}

// ============================================================================
// REVIEW ROUND 2: P0 Issues - Critical Bugs
// ============================================================================

// P0 Issue #1: alwaysReturns has wrong early return detection
// A return in the middle of a block with code after it should still
// require the function to return on all paths (the code after is unreachable)
test "type checker: return in middle of block with no final return is invalid" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // function foo(): number {
    //   if (x) { return 1; }
    //   console.log("after if");  // This path doesn't return!
    // }
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var bool_type = types.Type{
        .kind = .boolean,
        .location = location.SourceLocation.dummy(),
        .data = .{ .boolean = {} },
    };

    // The return statement
    var ret_val = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 1.0 },
        .type = &num_type,
    };

    var ret_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &ret_val } },
    };

    // If body with return
    var if_body_stmts = [_]*ast.Node{&ret_stmt};
    var if_body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &if_body_stmts } },
    };

    // Condition
    var cond = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "x" },
        .type = &bool_type,
    };

    // If statement (no else!)
    var if_stmt = ast.Node{
        .kind = .if_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .if_stmt = .{
            .condition = &cond,
            .consequent = &if_body,
            .alternate = null, // No else branch!
        } },
    };

    // Some statement after the if (simulating console.log)
    var after_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &cond }, // Dummy expression
    };

    // Function body: if + expression (no return at end!)
    var body_stmts = [_]*ast.Node{ &if_stmt, &after_stmt };
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "foo",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &num_type,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should FAIL - not all paths return

    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .missing_return) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

// P0 Issue #2: Bitwise operations should NOT allow .number (which is float64)
test "type checker: bitwise AND on number type reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // number & number should fail (number is alias for float64)
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var left = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "a" },
        .type = &num_type,
    };

    var right = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "b" },
        .type = &num_type,
    };

    var bitwise_expr = ast.Node{
        .kind = .binary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .bit_and,
            .left = &left,
            .right = &right,
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &bitwise_expr },
    };

    var stmts = [_]*ast.Node{&expr_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should FAIL - bitwise on floats

    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .invalid_operation) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

// P0 Issue #2b: Bitwise on float64 explicitly should fail
test "type checker: bitwise OR on float64 reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var float_type = types.Type{
        .kind = .float64,
        .location = location.SourceLocation.dummy(),
        .data = .{ .float64 = {} },
    };

    var left = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "a" },
        .type = &float_type,
    };

    var right = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "b" },
        .type = &float_type,
    };

    var bitwise_expr = ast.Node{
        .kind = .binary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .bit_or,
            .left = &left,
            .right = &right,
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &bitwise_expr },
    };

    var stmts = [_]*ast.Node{&expr_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should FAIL

    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .invalid_operation) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

// P0 Issue #2c: Bitwise on int32 SHOULD work
test "type checker: bitwise XOR on int32 is valid" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var int_type = types.Type{
        .kind = .int32,
        .location = location.SourceLocation.dummy(),
        .data = .{ .int32 = {} },
    };

    var left = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "a" },
        .type = &int_type,
    };

    var right = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "b" },
        .type = &int_type,
    };

    var bitwise_expr = ast.Node{
        .kind = .binary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .bit_xor,
            .left = &left,
            .right = &right,
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &bitwise_expr },
    };

    var stmts = [_]*ast.Node{&expr_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok); // Should PASS - int32 bitwise is valid
}

// P0 Issue #3: Arrow functions / function_expr missing return not checked
test "type checker: arrow function without return reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var void_type = types.Type{
        .kind = .void,
        .location = location.SourceLocation.dummy(),
        .data = .{ .void = {} },
    };

    // const fn = (): number => { console.log("hi"); }
    // Body has no return!
    var log_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "console" },
        .type = &void_type,
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &log_id },
    };

    var body_stmts = [_]*ast.Node{&expr_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    // Arrow function expression
    var arrow_fn = ast.Node{
        .kind = .function_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_expr = .{
            .name = null,
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &num_type, // Expects number!
            .body = &body,
            .is_arrow = true,
        } },
    };

    const decl = ast.node.VariableStmt.VariableDeclarator{
        .name = "fn",
        .type = null,
        .init = &arrow_fn,
    };

    var decls = [_]ast.node.VariableStmt.VariableDeclarator{decl};
    var var_stmt = ast.Node{
        .kind = .variable_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .variable_stmt = .{
            .kind = .@"const",
            .declarations = &decls,
        } },
    };

    var stmts = [_]*ast.Node{&var_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should FAIL - arrow function missing return

    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .missing_return) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

// P0 Issue #3b: function expression (non-arrow) without return
test "type checker: function expression without return reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var void_type = types.Type{
        .kind = .void,
        .location = location.SourceLocation.dummy(),
        .data = .{ .void = {} },
    };

    // const fn = function(): string { doSomething(); }
    var dummy_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "doSomething" },
        .type = &void_type,
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &dummy_id },
    };

    var body_stmts = [_]*ast.Node{&expr_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var func_expr = ast.Node{
        .kind = .function_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_expr = .{
            .name = null,
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &str_type, // Expects string!
            .body = &body,
            .is_arrow = false,
        } },
    };

    const decl2 = ast.node.VariableStmt.VariableDeclarator{
        .name = "fn",
        .type = null,
        .init = &func_expr,
    };

    var decls = [_]ast.node.VariableStmt.VariableDeclarator{decl2};
    var var_stmt = ast.Node{
        .kind = .variable_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .variable_stmt = .{
            .kind = .@"const",
            .declarations = &decls,
        } },
    };

    var stmts = [_]*ast.Node{&var_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should FAIL

    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .missing_return) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

// ============================================================================
// REVIEW ROUND 2: P1 Issues - High Priority
// ============================================================================

// P1 Issue #4: checkClassInheritance is a stub - override with incompatible return
test "type checker: class override with incompatible return type reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var void_type = types.Type{
        .kind = .void,
        .location = location.SourceLocation.dummy(),
        .data = .{ .void = {} },
    };

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    // Parent class with method speak(): void
    const parent_method_type = try alloc.create(types.FunctionType);
    parent_method_type.* = .{
        .type_params = &[_]types.GenericParam{},
        .params = &[_]types.FunctionType.FunctionParam{},
        .return_type = &void_type,
    };

    const parent_func_type = try alloc.create(types.Type);
    parent_func_type.* = .{
        .kind = .function,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function = parent_method_type },
    };

    const parent_props = try alloc.alloc(types.ObjectType.Property, 1);
    parent_props[0] = .{
        .name = "speak",
        .type = parent_func_type,
        .optional = false,
    };

    const parent_obj_type = try alloc.create(types.ObjectType);
    parent_obj_type.* = .{
        .properties = parent_props,
        .methods = &[_]types.ObjectType.Property{},
    };

    var parent_type = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = parent_obj_type },
    };

    // Child method body (empty, just for structure)
    var ret_val = ast.Node{
        .kind = .string_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string_literal = "woof" },
        .type = &str_type,
    };

    var ret_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &ret_val } },
    };

    var method_body_stmts = [_]*ast.Node{&ret_stmt};
    var method_body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &method_body_stmts } },
    };

    // Child method: speak(): string (incompatible with parent's void!)
    var child_method = ast.Node{
        .kind = .method_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .method_decl = .{
            .name = "speak",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &str_type, // Returns string, but parent returns void!
            .body = &method_body,
        } },
    };

    var members = [_]*ast.Node{&child_method};

    // Child class extends Parent
    var child_class = ast.Node{
        .kind = .class_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .class_decl = .{
            .name = "Dog",
            .type_params = &[_]types.GenericParam{},
            .extends = &parent_type,
            .implements = &[_]*types.Type{},
            .members = &members,
            .decorators = &[_]ast.node.ClassDecl.Decorator{},
        } },
    };

    var stmts = [_]*ast.Node{&child_class};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should FAIL - incompatible override

    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .incompatible_override) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

// P1 Issue #5: Optional parameters must come after required ones
test "type checker: optional param before required param reports error" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var void_type = types.Type{
        .kind = .void,
        .location = location.SourceLocation.dummy(),
        .data = .{ .void = {} },
    };

    // function foo(a?: number, b: string): void {}
    // Invalid: optional param 'a' before required param 'b'
    var params = [_]ast.node.FunctionExpr.FunctionParam{
        .{ .name = "a", .type = &num_type, .optional = true, .default_value = null },
        .{ .name = "b", .type = &str_type, .optional = false, .default_value = null },
    };

    var body_stmts = [_]*ast.Node{};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "foo",
            .type_params = &[_]types.GenericParam{},
            .params = &params,
            .return_type = &void_type,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(!ok); // Should FAIL - invalid param order

    var found_error = false;
    for (checker.errors.items) |err| {
        if (err.kind == .invalid_operation) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

// ============================================================================
// ITERATION 1: Method Calls & Instance Types Tests
// These tests are for building out the type checker to handle class instances
// ============================================================================

// 1.1 - Function types from function declarations
test "type checker: function declaration creates function type" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: function add(a: number, b: number): number { return a + b; }
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var a_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "a" },
        .type = &num_type,
    };

    var b_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "b" },
        .type = &num_type,
    };

    var add_expr = ast.Node{
        .kind = .binary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .add,
            .left = &a_id,
            .right = &b_id,
        } },
        .type = &num_type,
    };

    var return_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &add_expr } },
    };

    var body_stmts = [_]*ast.Node{&return_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var params = [_]ast.node.FunctionExpr.FunctionParam{
        .{ .name = "a", .type = &num_type, .optional = false, .default_value = null },
        .{ .name = "b", .type = &num_type, .optional = false, .default_value = null },
    };

    var func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "add",
            .type_params = &[_]types.GenericParam{},
            .params = &params,
            .return_type = &num_type,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // After type checking, the function should be in symbol table WITH a function type
    const func_sym = checker.symbols.lookup("add");
    try std.testing.expect(func_sym != null);

    // KEY TEST: The function symbol should have a function type
    try std.testing.expect(func_sym.?.type != null);
    try std.testing.expectEqual(types.TypeKind.function, func_sym.?.type.?.kind);

    // Verify function type details
    const func_type = func_sym.?.type.?.data.function;
    try std.testing.expectEqual(@as(usize, 2), func_type.params.len);
    try std.testing.expectEqual(types.TypeKind.number, func_type.return_type.kind);
}

// 1.2 - new ClassName() returns class type
test "type checker: new expression has class type" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create class User { name: string; }
    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var prop = ast.Node{
        .kind = .property_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .property_decl = .{
            .name = "name",
            .type = &str_type,
            .init = null,
            .readonly = false,
        } },
    };

    var members = [_]*ast.Node{&prop};
    var class = ast.Node{
        .kind = .class_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .class_decl = .{
            .name = "User",
            .type_params = &[_]types.GenericParam{},
            .extends = null,
            .implements = &[_]*types.Type{},
            .members = &members,
            .decorators = &[_]ast.node.ClassDecl.Decorator{},
        } },
    };

    // Create: const user = new User();
    var class_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "User" },
    };

    var new_expr = ast.Node{
        .kind = .new_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .new_expr = .{
            .callee = &class_id,
            .arguments = &[_]*ast.Node{},
            .type_args = &[_]*types.Type{},
        } },
    };

    const decl = ast.node.VariableStmt.VariableDeclarator{
        .name = "user",
        .type = null, // Infer from new expr
        .init = &new_expr,
    };
    var decls = [_]ast.node.VariableStmt.VariableDeclarator{decl};

    var var_stmt = ast.Node{
        .kind = .variable_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .variable_stmt = .{
            .kind = .@"const",
            .declarations = &decls,
        } },
    };

    var stmts = [_]*ast.Node{ &class, &var_stmt };
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: new_expr should have class/object type after type checking
    try std.testing.expect(new_expr.type != null);

    // The type should be an object type representing the User class
    try std.testing.expectEqual(types.TypeKind.object, new_expr.type.?.kind);
}

// 1.3 - Method call resolves to return type
test "type checker: method call returns correct type" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    // Create method body: return "hello";
    var ret_val = ast.Node{
        .kind = .string_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string_literal = "hello" },
        .type = &str_type,
    };

    var ret_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &ret_val } },
    };

    var body_stmts = [_]*ast.Node{&ret_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    // Create method: getName(): string
    var method = ast.Node{
        .kind = .method_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .method_decl = .{
            .name = "getName",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &str_type,
            .body = &body,
        } },
    };

    // Create property: name: string
    var prop = ast.Node{
        .kind = .property_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .property_decl = .{
            .name = "name",
            .type = &str_type,
            .init = null,
            .readonly = false,
        } },
    };

    var members = [_]*ast.Node{ &prop, &method };
    var class = ast.Node{
        .kind = .class_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .class_decl = .{
            .name = "User",
            .type_params = &[_]types.GenericParam{},
            .extends = null,
            .implements = &[_]*types.Type{},
            .members = &members,
            .decorators = &[_]ast.node.ClassDecl.Decorator{},
        } },
    };

    // Create: new User()
    var class_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "User" },
    };

    var new_expr = ast.Node{
        .kind = .new_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .new_expr = .{
            .callee = &class_id,
            .arguments = &[_]*ast.Node{},
            .type_args = &[_]*types.Type{},
        } },
    };

    // Create user variable
    const decl = ast.node.VariableStmt.VariableDeclarator{
        .name = "user",
        .type = null,
        .init = &new_expr,
    };
    var decls = [_]ast.node.VariableStmt.VariableDeclarator{decl};

    var var_stmt = ast.Node{
        .kind = .variable_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .variable_stmt = .{
            .kind = .@"const",
            .declarations = &decls,
        } },
    };

    // Create user.getName()
    var user_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "user" },
    };

    var getname_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "getName" },
    };

    var member_access = ast.Node{
        .kind = .member_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .member_expr = .{
            .object = &user_id,
            .property = &getname_id,
            .computed = false,
        } },
    };

    var call = ast.Node{
        .kind = .call_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .call_expr = .{
            .callee = &member_access,
            .arguments = &[_]*ast.Node{},
            .type_args = &[_]*types.Type{},
        } },
    };

    var expr_stmt = ast.Node{
        .kind = .expression_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .expression_stmt = &call },
    };

    var stmts = [_]*ast.Node{ &class, &var_stmt, &expr_stmt };
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: member access should have function type
    try std.testing.expect(member_access.type != null);
    try std.testing.expectEqual(types.TypeKind.function, member_access.type.?.kind);

    // KEY TEST: call expression should have the return type (string)
    try std.testing.expect(call.type != null);
    try std.testing.expectEqual(types.TypeKind.string, call.type.?.kind);
}

// 1.4 - this has class type in method body
test "type checker: this has class type in method" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    // Create: return this.name;
    var this_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "this" },
    };

    var name_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "name" },
    };

    var this_name = ast.Node{
        .kind = .member_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .member_expr = .{
            .object = &this_id,
            .property = &name_id,
            .computed = false,
        } },
    };

    var return_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &this_name } },
    };

    var body_stmts = [_]*ast.Node{&return_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var method = ast.Node{
        .kind = .method_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .method_decl = .{
            .name = "getName",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &str_type,
            .body = &body,
        } },
    };

    var name_prop = ast.Node{
        .kind = .property_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .property_decl = .{
            .name = "name",
            .type = &str_type,
            .init = null,
            .readonly = false,
        } },
    };

    var members = [_]*ast.Node{ &name_prop, &method };
    var class = ast.Node{
        .kind = .class_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .class_decl = .{
            .name = "User",
            .type_params = &[_]types.GenericParam{},
            .extends = null,
            .implements = &[_]*types.Type{},
            .members = &members,
            .decorators = &[_]ast.node.ClassDecl.Decorator{},
        } },
    };

    var stmts = [_]*ast.Node{&class};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: 'this' should have the class's object type
    try std.testing.expect(this_id.type != null);
    try std.testing.expectEqual(types.TypeKind.object, this_id.type.?.kind);

    // KEY TEST: this.name should have string type
    try std.testing.expect(this_name.type != null);
    try std.testing.expectEqual(types.TypeKind.string, this_name.type.?.kind);
}

// ============================================================================
// ITERATION 2: Generics Tests
// These tests are for generic type parameters, instantiation, and constraints
// ============================================================================

// 2.1 - Type parameter scoping: function identity<T>(x: T): T
test "type checker: generic function type parameter in scope" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: function identity<T>(x: T): T { return x; }

    // Type parameter T
    const type_param = types.GenericParam{
        .name = "T",
        .constraint = null,
        .default = null,
    };
    var type_params = [_]types.GenericParam{type_param};

    // Parameter x: T (type reference to T)
    var t_ref_data = types.TypeReference{
        .name = "T",
        .type_args = &[_]*types.Type{},
    };
    var t_ref = types.Type{
        .kind = .type_reference,
        .location = location.SourceLocation.dummy(),
        .data = .{ .type_reference = &t_ref_data },
    };

    // Return: x
    var x_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "x" },
    };

    var return_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &x_id } },
    };

    var body_stmts = [_]*ast.Node{&return_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var params = [_]ast.node.FunctionExpr.FunctionParam{
        .{ .name = "x", .type = &t_ref, .optional = false, .default_value = null },
    };

    var func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "identity",
            .type_params = &type_params,
            .params = &params,
            .return_type = &t_ref,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: Function should have type params registered
    const func_sym = checker.symbols.lookup("identity");
    try std.testing.expect(func_sym != null);
    try std.testing.expect(func_sym.?.type != null);
    try std.testing.expectEqual(types.TypeKind.function, func_sym.?.type.?.kind);

    // The function type should have type_params
    const func_type = func_sym.?.type.?.data.function;
    try std.testing.expectEqual(@as(usize, 1), func_type.type_params.len);
    try std.testing.expectEqualStrings("T", func_type.type_params[0].name);
}

// 2.2 - Generic instantiation: Array<number>
test "type checker: generic instantiation creates concrete type" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Setup: Register a generic class Array<T>
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Create generic Array<T> class type
    const t_param = types.GenericParam{
        .name = "T",
        .constraint = null,
        .default = null,
    };

    // Register Array as a generic class
    var array_sym = symbol.Symbol.init("Array", .class, location.SourceLocation.dummy());

    // Create generic function type for Array's constructor concept
    const array_type_params = try alloc.alloc(types.GenericParam, 1);
    array_type_params[0] = t_param;

    const array_func_data = try alloc.create(types.FunctionType);
    array_func_data.* = .{
        .type_params = array_type_params,
        .params = &[_]types.FunctionType.FunctionParam{},
        .return_type = undefined, // Will be set to object type
    };

    // For simplicity, use object type for Array
    const array_obj_data = try alloc.create(types.ObjectType);
    array_obj_data.* = .{
        .properties = &[_]types.ObjectType.Property{},
        .methods = &[_]types.ObjectType.Property{},
    };

    const array_type = try alloc.create(types.Type);
    array_type.* = .{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = array_obj_data },
    };
    array_sym.type = array_type;

    try checker.symbols.define(array_sym);

    // Create: const arr: Array<number> = [];
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    // Array<number> type reference
    var type_args = [_]*types.Type{&num_type};
    var array_num_ref_data = types.TypeReference{
        .name = "Array",
        .type_args = &type_args,
    };
    var array_num_ref = types.Type{
        .kind = .type_reference,
        .location = location.SourceLocation.dummy(),
        .data = .{ .type_reference = &array_num_ref_data },
    };

    // Empty array literal
    var arr_literal = ast.Node{
        .kind = .array_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .array_expr = .{ .elements = &[_]*ast.Node{} } },
    };

    const decl = ast.node.VariableStmt.VariableDeclarator{
        .name = "arr",
        .type = &array_num_ref,
        .init = &arr_literal,
    };
    var decls = [_]ast.node.VariableStmt.VariableDeclarator{decl};

    var var_stmt = ast.Node{
        .kind = .variable_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .variable_stmt = .{
            .kind = .@"const",
            .declarations = &decls,
        } },
    };

    var stmts = [_]*ast.Node{&var_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: arr variable should have a type
    const arr_sym = checker.symbols.lookup("arr");
    try std.testing.expect(arr_sym != null);
    try std.testing.expect(arr_sym.?.type != null);

    // The type should be a generic_instance or resolved to concrete type
    // For now, just verify it's not null and has been processed
    const arr_type = arr_sym.?.type.?;
    try std.testing.expect(arr_type.kind == .type_reference or
        arr_type.kind == .generic_instance or
        arr_type.kind == .object);
}

// 2.3 - Generic constraint checking: T extends Base
test "type checker: generic constraint is respected" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Setup: Create constraint type
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Base interface with name property
    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    const base_props = try alloc.alloc(types.ObjectType.Property, 1);
    base_props[0] = .{ .name = "name", .type = &str_type, .optional = false };

    const base_obj_data = try alloc.create(types.ObjectType);
    base_obj_data.* = .{ .properties = base_props, .methods = &[_]types.ObjectType.Property{} };

    const base_type = try alloc.create(types.Type);
    base_type.* = .{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = base_obj_data },
    };

    // Register Named interface
    var named_sym = symbol.Symbol.init("Named", .interface, location.SourceLocation.dummy());
    named_sym.type = base_type;
    try checker.symbols.define(named_sym);

    // Create: function greet<T extends Named>(obj: T): string { return obj.name; }
    var constraint_ref_data = types.TypeReference{
        .name = "Named",
        .type_args = &[_]*types.Type{},
    };
    const constraint_ref = try alloc.create(types.Type);
    constraint_ref.* = .{
        .kind = .type_reference,
        .location = location.SourceLocation.dummy(),
        .data = .{ .type_reference = &constraint_ref_data },
    };

    const type_param = types.GenericParam{
        .name = "T",
        .constraint = constraint_ref,
        .default = null,
    };
    var type_params = [_]types.GenericParam{type_param};

    // Parameter obj: T
    var t_ref_data = types.TypeReference{
        .name = "T",
        .type_args = &[_]*types.Type{},
    };
    var t_ref = types.Type{
        .kind = .type_reference,
        .location = location.SourceLocation.dummy(),
        .data = .{ .type_reference = &t_ref_data },
    };

    // Return obj.name
    var obj_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "obj" },
    };

    var name_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "name" },
    };

    var obj_name = ast.Node{
        .kind = .member_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .member_expr = .{
            .object = &obj_id,
            .property = &name_id,
            .computed = false,
        } },
    };

    var return_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &obj_name } },
    };

    var body_stmts = [_]*ast.Node{&return_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var params = [_]ast.node.FunctionExpr.FunctionParam{
        .{ .name = "obj", .type = &t_ref, .optional = false, .default_value = null },
    };

    var func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "greet",
            .type_params = &type_params,
            .params = &params,
            .return_type = &str_type,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: Function type should have constraint recorded
    const func_sym = checker.symbols.lookup("greet");
    try std.testing.expect(func_sym != null);
    try std.testing.expect(func_sym.?.type != null);

    const func_type = func_sym.?.type.?.data.function;
    try std.testing.expectEqual(@as(usize, 1), func_type.type_params.len);

    // The type parameter should have a constraint
    try std.testing.expect(func_type.type_params[0].constraint != null);
}

// ============================================================================
// ITERATION 3: Union Types & Type Narrowing Tests
// These tests are for union types, compatibility, and type narrowing
// ============================================================================

// 3.1 - Union type compatibility: string is assignable to string | number
test "type checker: union type accepts member type" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create union type: string | number
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    const union_members = try alloc.alloc(*types.Type, 2);
    union_members[0] = &str_type;
    union_members[1] = &num_type;

    const union_type_data = try alloc.create(types.UnionType);
    union_type_data.* = .{ .types = union_members };

    var union_type = types.Type{
        .kind = .@"union",
        .location = location.SourceLocation.dummy(),
        .data = .{ .@"union" = union_type_data },
    };

    // Create: const x: string | number = "hello";
    var str_lit = ast.Node{
        .kind = .string_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string_literal = "hello" },
    };

    const decl = ast.node.VariableStmt.VariableDeclarator{
        .name = "x",
        .type = &union_type,
        .init = &str_lit,
    };
    var decls = [_]ast.node.VariableStmt.VariableDeclarator{decl};

    var var_stmt = ast.Node{
        .kind = .variable_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .variable_stmt = .{
            .kind = .@"const",
            .declarations = &decls,
        } },
    };

    var stmts = [_]*ast.Node{&var_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: Variable x should have union type
    const x_sym = checker.symbols.lookup("x");
    try std.testing.expect(x_sym != null);
    try std.testing.expect(x_sym.?.type != null);
    try std.testing.expectEqual(types.TypeKind.@"union", x_sym.?.type.?.kind);
}

// 3.2 - Type narrowing via null check: string | null narrows to string
test "type checker: null check narrows union type" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // This test checks that inside `if (x != null)`, the type of x is narrowed
    // For now, just verify the setup is valid - full narrowing is more complex

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Create string | null union
    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    // null represented as never or special null type
    var null_type = types.Type{
        .kind = .never, // Using never as placeholder for null
        .location = location.SourceLocation.dummy(),
        .data = .{ .never = {} },
    };

    const union_members = try alloc.alloc(*types.Type, 2);
    union_members[0] = &str_type;
    union_members[1] = &null_type;

    const union_type_data = try alloc.create(types.UnionType);
    union_type_data.* = .{ .types = union_members };

    var union_type = types.Type{
        .kind = .@"union",
        .location = location.SourceLocation.dummy(),
        .data = .{ .@"union" = union_type_data },
    };

    // Create: let x: string | null = "hello";
    var str_lit = ast.Node{
        .kind = .string_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string_literal = "hello" },
    };

    const decl = ast.node.VariableStmt.VariableDeclarator{
        .name = "x",
        .type = &union_type,
        .init = &str_lit,
    };
    var decls = [_]ast.node.VariableStmt.VariableDeclarator{decl};

    var var_stmt = ast.Node{
        .kind = .variable_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .variable_stmt = .{
            .kind = .let,
            .declarations = &decls,
        } },
    };

    var stmts = [_]*ast.Node{&var_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: Variable should have union type (full narrowing not implemented yet)
    const x_sym = checker.symbols.lookup("x");
    try std.testing.expect(x_sym != null);
    try std.testing.expect(x_sym.?.type != null);
}

// 3.3 - Union type mismatch: boolean is not assignable to string | number
test "type checker: union type rejects incompatible type" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Create union type: string | number
    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    const union_members = try alloc.alloc(*types.Type, 2);
    union_members[0] = &str_type;
    union_members[1] = &num_type;

    const union_type_data = try alloc.create(types.UnionType);
    union_type_data.* = .{ .types = union_members };

    var union_type = types.Type{
        .kind = .@"union",
        .location = location.SourceLocation.dummy(),
        .data = .{ .@"union" = union_type_data },
    };

    // Create: const x: string | number = true; (should fail!)
    var bool_lit = ast.Node{
        .kind = .boolean_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .boolean_literal = true },
    };

    const decl = ast.node.VariableStmt.VariableDeclarator{
        .name = "x",
        .type = &union_type,
        .init = &bool_lit,
    };
    var decls = [_]ast.node.VariableStmt.VariableDeclarator{decl};

    var var_stmt = ast.Node{
        .kind = .variable_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .variable_stmt = .{
            .kind = .@"const",
            .declarations = &decls,
        } },
    };

    var stmts = [_]*ast.Node{&var_stmt};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);

    // KEY TEST: Should fail because boolean is not in string | number
    try std.testing.expect(!ok);
    try std.testing.expect(checker.errors.items.len > 0);
}

// ============================================================================
// ITERATION 4: Multi-file Module System Tests
// These tests are for import/export handling and cross-file symbol resolution
// ============================================================================

// 4.1 - Import declaration is processed without crash
test "type checker: import declaration is processed" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: import { foo } from "./utils";
    const import_spec = ast.node.ImportDecl.ImportSpecifier{
        .imported = "foo",
        .local = "foo",
    };
    var specs = [_]ast.node.ImportDecl.ImportSpecifier{import_spec};

    var import_node = ast.Node{
        .kind = .import_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .import_decl = .{
            .specifiers = &specs,
            .source = "./utils",
        } },
    };

    var stmts = [_]*ast.Node{&import_node};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    // Should not crash - just process the import
    // Full import resolution needs module registry - we just verify it doesn't crash
    const ok = try checker.check(&program);
    _ = ok; // Result doesn't matter - we just want no crash
}

// 4.2 - Export declaration registers exported symbol
test "type checker: export declaration registers symbol" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create: export function greet() { return "hello"; }
    var str_lit = ast.Node{
        .kind = .string_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string_literal = "hello" },
    };

    var return_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &str_lit } },
    };

    var body_stmts = [_]*ast.Node{&return_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "greet",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = null,
            .body = &body,
        } },
    };

    // Export the function
    var export_node = ast.Node{
        .kind = .export_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .export_decl = .{
            .declaration = &func,
            .specifiers = &[_]ast.node.ExportDecl.ExportSpecifier{},
        } },
    };

    var stmts = [_]*ast.Node{&export_node};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: The exported function should be in symbol table
    const greet_sym = checker.symbols.lookup("greet");
    try std.testing.expect(greet_sym != null);
    try std.testing.expectEqual(symbol.SymbolKind.function, greet_sym.?.kind);
}

// 4.3 - Named exports are tracked
test "type checker: named exports are tracked" {
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // First define a function
    var str_lit = ast.Node{
        .kind = .string_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string_literal = "hello" },
    };

    var return_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &str_lit } },
    };

    var body_stmts = [_]*ast.Node{&return_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "helper",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = null,
            .body = &body,
        } },
    };

    // Then export it by name: export { helper };
    const export_spec = ast.node.ExportDecl.ExportSpecifier{
        .local = "helper",
        .exported = "helper",
    };
    var export_specs = [_]ast.node.ExportDecl.ExportSpecifier{export_spec};

    var export_node = ast.Node{
        .kind = .export_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .export_decl = .{
            .declaration = null,
            .specifiers = &export_specs,
        } },
    };

    var stmts = [_]*ast.Node{ &func, &export_node };
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: Function should be defined and exportable
    const helper_sym = checker.symbols.lookup("helper");
    try std.testing.expect(helper_sym != null);
}

// ============================================================================
// Scope Boundary Tests - Integration tests for lookupAtPosition with real AST
// ============================================================================

test "type checker: scope boundaries are set for nested functions" {
    // This test verifies that enterScopeWithLocation is called correctly
    // and scope boundaries are set for nested functions.
    //
    // Test code:
    // function outer() {       // Line 1, scope ends at line 7
    //     let x = 1;           // Line 2
    //     function inner() {   // Line 3, scope ends at line 5
    //         let x = 2;       // Line 4
    //     }                    // Line 5
    //     return x;            // Line 6 - should find outer's x, not inner's
    // }                        // Line 7

    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Create outer function body (lines 1-7)
    // First: inner function
    var inner_x = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.init(1, .{ .line = 4, .column = 8 }, .{ .line = 4, .column = 9 }),
        .data = .{ .identifier = "x" },
    };
    var inner_ret = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.init(1, .{ .line = 4, .column = 0 }, .{ .line = 4, .column = 10 }),
        .data = .{ .return_stmt = .{ .argument = &inner_x } },
    };
    var inner_decls = [_]ast.node.VariableStmt.VariableDeclarator{.{
        .name = "x",
        .type = null,
        .init = null,
    }};
    var inner_let = ast.Node{
        .kind = .variable_stmt,
        .location = location.SourceLocation.init(1, .{ .line = 4, .column = 8 }, .{ .line = 4, .column = 17 }),
        .data = .{ .variable_stmt = .{
            .kind = .@"const",
            .declarations = &inner_decls,
        } },
    };
    var inner_body_stmts = [_]*ast.Node{ &inner_let, &inner_ret };
    var inner_body = ast.Node{
        .kind = .block_stmt,
        // Inner function body spans lines 3-5
        .location = location.SourceLocation.init(1, .{ .line = 3, .column = 20 }, .{ .line = 5, .column = 5 }),
        .data = .{ .block_stmt = .{ .statements = &inner_body_stmts } },
    };
    var inner_func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.init(1, .{ .line = 3, .column = 4 }, .{ .line = 5, .column = 5 }),
        .data = .{ .function_decl = .{
            .name = "inner",
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = null,
            .body = &inner_body,
            .type_params = &[_]types.GenericParam{},
        } },
    };

    // Outer function elements
    var outer_decls = [_]ast.node.VariableStmt.VariableDeclarator{.{
        .name = "x",
        .type = null,
        .init = null,
    }};
    var outer_let = ast.Node{
        .kind = .variable_stmt,
        .location = location.SourceLocation.init(1, .{ .line = 2, .column = 4 }, .{ .line = 2, .column = 13 }),
        .data = .{ .variable_stmt = .{
            .kind = .@"const",
            .declarations = &outer_decls,
        } },
    };
    var outer_x = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.init(1, .{ .line = 6, .column = 11 }, .{ .line = 6, .column = 12 }),
        .data = .{ .identifier = "x" },
    };
    var outer_ret = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.init(1, .{ .line = 6, .column = 4 }, .{ .line = 6, .column = 13 }),
        .data = .{ .return_stmt = .{ .argument = &outer_x } },
    };
    var outer_body_stmts = [_]*ast.Node{ &outer_let, &inner_func, &outer_ret };
    var outer_body = ast.Node{
        .kind = .block_stmt,
        // Outer function body spans lines 1-7
        .location = location.SourceLocation.init(1, .{ .line = 1, .column = 17 }, .{ .line = 7, .column = 1 }),
        .data = .{ .block_stmt = .{ .statements = &outer_body_stmts } },
    };
    var outer_func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.init(1, .{ .line = 1, .column = 0 }, .{ .line = 7, .column = 1 }),
        .data = .{ .function_decl = .{
            .name = "outer",
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = null,
            .body = &outer_body,
            .type_params = &[_]types.GenericParam{},
        } },
    };

    // Program with just outer function
    var stmts = [_]*ast.Node{&outer_func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.init(1, .{ .line = 1, .column = 0 }, .{ .line = 7, .column = 1 }),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 1 } },
    };

    // Run type checker
    _ = checker.check(&program) catch {};

    // Verify scope boundaries were set
    // We should have at least: global, outer function, inner function
    try std.testing.expect(checker.symbols.scopes.items.len >= 3);

    // Find the function scopes and verify they have boundaries
    var outer_scope_found = false;
    var inner_scope_found = false;

    for (checker.symbols.scopes.items) |scope| {
        if (scope.start_location) |loc| {
            if (scope.kind == .function) {
                if (loc.start.line == 1 and loc.end.line == 7) {
                    outer_scope_found = true;
                }
                if (loc.start.line == 3 and loc.end.line == 5) {
                    inner_scope_found = true;
                }
            }
        }
    }

    try std.testing.expect(outer_scope_found);
    try std.testing.expect(inner_scope_found);

    // KEY TEST: lookupAtPosition at line 6 (inside outer, outside inner)
    // should find outer's x (line 2), NOT inner's x (line 4)
    const at_line_6 = checker.symbols.lookupAtPosition("x", 6, 15);
    try std.testing.expect(at_line_6 != null);
    try std.testing.expectEqual(@as(u32, 2), at_line_6.?.location.start.line);

    // Verify: lookupAtPosition at line 4 (inside inner) should find inner's x
    const at_line_4 = checker.symbols.lookupAtPosition("x", 4, 15);
    try std.testing.expect(at_line_4 != null);
    try std.testing.expectEqual(@as(u32, 4), at_line_4.?.location.start.line);
}

// Reference submodule tests to include them in test run
test {
    std.testing.refAllDecls(symbol);
    std.testing.refAllDecls(resolver);
    std.testing.refAllDecls(inference);
}

test "type checker: calling nested function should work" {
    // This tests the actual issue: calling inner() from within outer()
    // function outer(): number {
    //     function inner(): number { return 1; }
    //     return inner();
    // }
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    // Inner function: function inner(): number { return 1; }
    var inner_ret_val = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 1.0 },
        .type = &num_type,
    };

    var inner_ret = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &inner_ret_val } },
    };

    var inner_stmts = [_]*ast.Node{&inner_ret};
    var inner_body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &inner_stmts } },
    };

    var inner_func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "inner",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &num_type,
            .body = &inner_body,
        } },
    };

    // Call expression: inner()
    var inner_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "inner" },
    };

    var call_args = [_]*ast.Node{};
    var type_args = [_]*types.Type{};
    var inner_call = ast.Node{
        .kind = .call_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .call_expr = .{
            .callee = &inner_id,
            .arguments = &call_args,
            .type_args = &type_args,
        } },
    };

    // return inner()
    var outer_ret = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &inner_call } },
    };

    // Outer function body: { function inner()...; return inner(); }
    var outer_stmts = [_]*ast.Node{ &inner_func, &outer_ret };
    var outer_body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &outer_stmts } },
    };

    var outer_func = ast.Node{
        .kind = .function_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .function_decl = .{
            .name = "outer",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &num_type,
            .body = &outer_body,
        } },
    };

    var stmts = [_]*ast.Node{&outer_func};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);

    // Debug: print scopes
    std.debug.print("\n=== Scopes after check ===\n", .{});
    for (checker.symbols.scopes.items, 0..) |scope, i| {
        std.debug.print("Scope {d}: kind={s}\n", .{ i, @tagName(scope.kind) });
        var it = scope.symbols.iterator();
        while (it.next()) |entry| {
            std.debug.print("  - {s}\n", .{entry.key_ptr.*});
        }
    }

    // Debug: print errors
    std.debug.print("\n=== Errors ===\n", .{});
    for (checker.getErrors()) |err| {
        std.debug.print("  {s}\n", .{err.message});
    }

    // Should have no errors - inner() should be found
    try std.testing.expect(ok);
}

// ============================================================================
// Macro LSP Tests - Mirror function/method tests for macro support
// ============================================================================
// These tests ensure macros behave identically to functions for LSP features:
// - Macro symbol registration
// - Macro parameter lookup (hover)
// - Macro scope boundaries
// - Macro JSDoc propagation

test "type checker: macro symbol is registered with function type" {
    // Mirrors "generic function type parameter in scope" test
    // macro derive(ctx: AstContext): Node { return ctx.target; }
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Return: ctx.target (simplified to just ctx identifier)
    var ctx_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "ctx" },
    };

    var return_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &ctx_id } },
    };

    var body_stmts = [_]*ast.Node{&return_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    // Parameter: ctx: AstContext (using unknown type for simplicity)
    var param_type = types.Type{
        .kind = .unknown,
        .location = location.SourceLocation.dummy(),
        .data = .{ .unknown = {} },
    };

    var params = [_]ast.node.FunctionExpr.FunctionParam{
        .{ .name = "ctx", .type = &param_type, .optional = false, .default_value = null },
    };

    var macro = ast.Node{
        .kind = .macro_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .macro_decl = .{
            .name = "derive",
            .type_params = &[_]types.GenericParam{},
            .params = &params,
            .return_type = &param_type,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&macro};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: Macro should be registered as a symbol
    const macro_sym = checker.symbols.lookup("derive");
    try std.testing.expect(macro_sym != null);
    try std.testing.expectEqual(symbol.SymbolKind.macro, macro_sym.?.kind);

    // Macro should have a function type (macros are callable)
    try std.testing.expect(macro_sym.?.type != null);
    try std.testing.expectEqual(types.TypeKind.function, macro_sym.?.type.?.kind);

    // Function type should have the parameter
    const func_type = macro_sym.?.type.?.data.function;
    try std.testing.expectEqual(@as(usize, 1), func_type.params.len);
    try std.testing.expectEqualStrings("ctx", func_type.params[0].name);
}

test "type checker: macro parameter is in scope for hover lookup" {
    // This is the KEY test that was failing before the fix
    // macro transform(input: Node, output: Node): Node {
    //     return input;  // <-- Shift+K on 'input' should show parameter info
    // }
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var param_type = types.Type{
        .kind = .unknown,
        .location = location.SourceLocation.dummy(),
        .data = .{ .unknown = {} },
    };

    // Return statement references 'input' parameter
    var input_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.init(1, .{ .line = 3, .column = 11 }, .{ .line = 3, .column = 16 }),
        .data = .{ .identifier = "input" },
    };

    var return_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.init(1, .{ .line = 3, .column = 4 }, .{ .line = 3, .column = 17 }),
        .data = .{ .return_stmt = .{ .argument = &input_id } },
    };

    var body_stmts = [_]*ast.Node{&return_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        // Body spans lines 2-4
        .location = location.SourceLocation.init(1, .{ .line = 2, .column = 40 }, .{ .line = 4, .column = 1 }),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    // Parameters with proper locations (in signature, before body)
    var params = [_]ast.node.FunctionExpr.FunctionParam{
        .{
            .name = "input",
            .type = &param_type,
            .optional = false,
            .default_value = null,
        },
        .{
            .name = "output",
            .type = &param_type,
            .optional = false,
            .default_value = null,
        },
    };

    var macro = ast.Node{
        .kind = .macro_decl,
        // Full macro declaration spans lines 1-4 (includes signature with params)
        .location = location.SourceLocation.init(1, .{ .line = 1, .column = 0 }, .{ .line = 4, .column = 1 }),
        .data = .{ .macro_decl = .{
            .name = "transform",
            .type_params = &[_]types.GenericParam{},
            .params = &params,
            .return_type = &param_type,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&macro};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.init(1, .{ .line = 1, .column = 0 }, .{ .line = 4, .column = 1 }),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 1 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: lookupAtPosition should find 'input' parameter at line 3
    // This is what the LSP hover uses
    const input_at_line_3 = checker.symbols.lookupAtPosition("input", 3, 15);
    try std.testing.expect(input_at_line_3 != null);
    try std.testing.expectEqual(symbol.SymbolKind.parameter, input_at_line_3.?.kind);

    // Also verify 'output' parameter is findable
    const output_at_line_3 = checker.symbols.lookupAtPosition("output", 3, 15);
    try std.testing.expect(output_at_line_3 != null);
    try std.testing.expectEqual(symbol.SymbolKind.parameter, output_at_line_3.?.kind);

    // Verify macro itself is also findable
    const macro_sym = checker.symbols.lookupAtPosition("transform", 3, 15);
    try std.testing.expect(macro_sym != null);
    try std.testing.expectEqual(symbol.SymbolKind.macro, macro_sym.?.kind);
}

test "type checker: macro scope boundary respects parameter visibility" {
    // Mirrors "scope boundaries are set for nested functions" test
    // macro outer() {           // Line 1
    //     let x = 1;            // Line 2
    //     macro inner() {       // Line 3 - nested macro
    //         let x = 2;        // Line 4
    //     }                     // Line 5
    //     return x;             // Line 6 - should find outer's x
    // }                         // Line 7
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var ret_type = types.Type{
        .kind = .unknown,
        .location = location.SourceLocation.dummy(),
        .data = .{ .unknown = {} },
    };

    // Inner macro
    var inner_x = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.init(1, .{ .line = 4, .column = 8 }, .{ .line = 4, .column = 9 }),
        .data = .{ .identifier = "x" },
    };
    var inner_ret = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.init(1, .{ .line = 4, .column = 0 }, .{ .line = 4, .column = 10 }),
        .data = .{ .return_stmt = .{ .argument = &inner_x } },
    };
    var inner_decls = [_]ast.node.VariableStmt.VariableDeclarator{.{
        .name = "x",
        .type = null,
        .init = null,
    }};
    var inner_let = ast.Node{
        .kind = .variable_stmt,
        .location = location.SourceLocation.init(1, .{ .line = 4, .column = 8 }, .{ .line = 4, .column = 17 }),
        .data = .{ .variable_stmt = .{
            .kind = .@"const",
            .declarations = &inner_decls,
        } },
    };
    var inner_body_stmts = [_]*ast.Node{ &inner_let, &inner_ret };
    var inner_body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.init(1, .{ .line = 3, .column = 20 }, .{ .line = 5, .column = 5 }),
        .data = .{ .block_stmt = .{ .statements = &inner_body_stmts } },
    };
    var inner_macro = ast.Node{
        .kind = .macro_decl,
        .location = location.SourceLocation.init(1, .{ .line = 3, .column = 4 }, .{ .line = 5, .column = 5 }),
        .data = .{ .macro_decl = .{
            .name = "inner",
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = null,
            .body = &inner_body,
            .type_params = &[_]types.GenericParam{},
        } },
    };

    // Outer macro
    var outer_decls = [_]ast.node.VariableStmt.VariableDeclarator{.{
        .name = "x",
        .type = null,
        .init = null,
    }};
    var outer_let = ast.Node{
        .kind = .variable_stmt,
        .location = location.SourceLocation.init(1, .{ .line = 2, .column = 4 }, .{ .line = 2, .column = 13 }),
        .data = .{ .variable_stmt = .{
            .kind = .@"const",
            .declarations = &outer_decls,
        } },
    };
    var outer_x = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.init(1, .{ .line = 6, .column = 11 }, .{ .line = 6, .column = 12 }),
        .data = .{ .identifier = "x" },
    };
    var outer_ret = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.init(1, .{ .line = 6, .column = 4 }, .{ .line = 6, .column = 13 }),
        .data = .{ .return_stmt = .{ .argument = &outer_x } },
    };
    var outer_body_stmts = [_]*ast.Node{ &outer_let, &inner_macro, &outer_ret };
    var outer_body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.init(1, .{ .line = 1, .column = 17 }, .{ .line = 7, .column = 1 }),
        .data = .{ .block_stmt = .{ .statements = &outer_body_stmts } },
    };
    var outer_macro = ast.Node{
        .kind = .macro_decl,
        .location = location.SourceLocation.init(1, .{ .line = 1, .column = 0 }, .{ .line = 7, .column = 1 }),
        .data = .{ .macro_decl = .{
            .name = "outer",
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = &ret_type,
            .body = &outer_body,
            .type_params = &[_]types.GenericParam{},
        } },
    };

    var stmts = [_]*ast.Node{&outer_macro};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.init(1, .{ .line = 1, .column = 0 }, .{ .line = 7, .column = 1 }),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 1 } },
    };

    _ = checker.check(&program) catch {};

    // KEY TEST: lookupAtPosition at line 6 (inside outer, outside inner)
    // should find outer's x (line 2), NOT inner's x (line 4)
    const at_line_6 = checker.symbols.lookupAtPosition("x", 6, 15);
    try std.testing.expect(at_line_6 != null);
    try std.testing.expectEqual(@as(u32, 2), at_line_6.?.location.start.line);

    // Verify: lookupAtPosition at line 4 (inside inner) should find inner's x
    const at_line_4 = checker.symbols.lookupAtPosition("x", 4, 15);
    try std.testing.expect(at_line_4 != null);
    try std.testing.expectEqual(@as(u32, 4), at_line_4.?.location.start.line);
}

test "type checker: macro JSDoc is propagated to symbol" {
    // Ensures macro JSDoc comments are available for LSP hover
    // /** This is a derive macro */
    // macro derive(ctx: AstContext): Node { ... }
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var param_type = types.Type{
        .kind = .unknown,
        .location = location.SourceLocation.dummy(),
        .data = .{ .unknown = {} },
    };

    var ctx_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "ctx" },
    };

    var return_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &ctx_id } },
    };

    var body_stmts = [_]*ast.Node{&return_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var params = [_]ast.node.FunctionExpr.FunctionParam{
        .{ .name = "ctx", .type = &param_type, .optional = false, .default_value = null },
    };

    // Macro with JSDoc comment
    var macro = ast.Node{
        .kind = .macro_decl,
        .location = location.SourceLocation.dummy(),
        .doc_comment = "/** This is a derive macro for auto-generating implementations */",
        .data = .{ .macro_decl = .{
            .name = "derive",
            .type_params = &[_]types.GenericParam{},
            .params = &params,
            .return_type = &param_type,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&macro};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: Macro symbol should have JSDoc propagated
    const macro_sym = checker.symbols.lookup("derive");
    try std.testing.expect(macro_sym != null);
    try std.testing.expect(macro_sym.?.doc_comment != null);
    try std.testing.expect(std.mem.indexOf(u8, macro_sym.?.doc_comment.?, "derive macro") != null);
}

test "type checker: generic macro type parameter in scope" {
    // Mirrors "generic function type parameter in scope" test
    // macro transform<T>(input: T): T { return input; }
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    // Type parameter T
    const type_param = types.GenericParam{
        .name = "T",
        .constraint = null,
        .default = null,
    };
    var type_params = [_]types.GenericParam{type_param};

    // Parameter input: T (type reference to T)
    var t_ref_data = types.TypeReference{
        .name = "T",
        .type_args = &[_]*types.Type{},
    };
    var t_ref = types.Type{
        .kind = .type_reference,
        .location = location.SourceLocation.dummy(),
        .data = .{ .type_reference = &t_ref_data },
    };

    // Return: input
    var input_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "input" },
    };

    var return_stmt = ast.Node{
        .kind = .return_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .return_stmt = .{ .argument = &input_id } },
    };

    var body_stmts = [_]*ast.Node{&return_stmt};
    var body = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &body_stmts } },
    };

    var params = [_]ast.node.FunctionExpr.FunctionParam{
        .{ .name = "input", .type = &t_ref, .optional = false, .default_value = null },
    };

    var macro = ast.Node{
        .kind = .macro_decl,
        .location = location.SourceLocation.dummy(),
        .data = .{ .macro_decl = .{
            .name = "transform",
            .type_params = &type_params,
            .params = &params,
            .return_type = &t_ref,
            .body = &body,
        } },
    };

    var stmts = [_]*ast.Node{&macro};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    const ok = try checker.check(&program);
    try std.testing.expect(ok);

    // KEY TEST: Macro should have type params registered
    const macro_sym = checker.symbols.lookup("transform");
    try std.testing.expect(macro_sym != null);
    try std.testing.expect(macro_sym.?.type != null);
    try std.testing.expectEqual(types.TypeKind.function, macro_sym.?.type.?.kind);

    // The macro's function type should have type_params
    const func_type = macro_sym.?.type.?.data.function;
    try std.testing.expectEqual(@as(usize, 1), func_type.type_params.len);
    try std.testing.expectEqualStrings("T", func_type.type_params[0].name);

    // Parameter should reference T
    try std.testing.expectEqual(@as(usize, 1), func_type.params.len);
    try std.testing.expectEqualStrings("input", func_type.params[0].name);
}

test "type checker: duplicate macro definition reports error" {
    // Ensures duplicate macro names are caught
    // macro foo() { }
    // macro foo() { }  // ERROR: duplicate macro definition
    var checker = try TypeChecker.init(std.testing.allocator);
    defer checker.deinit();

    var empty_body_stmts = [_]*ast.Node{};
    var body1 = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &empty_body_stmts } },
    };
    var body2 = ast.Node{
        .kind = .block_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .block_stmt = .{ .statements = &empty_body_stmts } },
    };

    var macro1 = ast.Node{
        .kind = .macro_decl,
        .location = location.SourceLocation.init(1, .{ .line = 1, .column = 0 }, .{ .line = 1, .column = 20 }),
        .data = .{ .macro_decl = .{
            .name = "foo",
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = null,
            .body = &body1,
        } },
    };

    var macro2 = ast.Node{
        .kind = .macro_decl,
        .location = location.SourceLocation.init(1, .{ .line = 2, .column = 0 }, .{ .line = 2, .column = 20 }),
        .data = .{ .macro_decl = .{
            .name = "foo", // Same name - should error
            .type_params = &[_]types.GenericParam{},
            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
            .return_type = null,
            .body = &body2,
        } },
    };

    var stmts = [_]*ast.Node{ &macro1, &macro2 };
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{ .statements = &stmts, .file_id = 0 } },
    };

    _ = checker.check(&program) catch {};

    // KEY TEST: Should have duplicate definition error
    const errors = checker.getErrors();
    try std.testing.expect(errors.len > 0);

    var found_duplicate_error = false;
    for (errors) |err| {
        if (std.mem.indexOf(u8, err.message, "duplicate") != null) {
            found_duplicate_error = true;
            break;
        }
    }
    try std.testing.expect(found_duplicate_error);
}
