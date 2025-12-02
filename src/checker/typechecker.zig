const std = @import("std");
const ast = @import("../ast/ast.zig");
const types = @import("../ast/types.zig");
const location = @import("../ast/location.zig");

// Re-export checker submodules
pub const symbol = @import("symbol.zig");
pub const resolver = @import("resolver.zig");
pub const inference = @import("inference.zig");

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
        break_outside_loop,
        continue_outside_loop,
        duplicate_definition,
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

    pub fn init(allocator: std.mem.Allocator) !TypeChecker {
        return .{
            .allocator = allocator,
            .symbols = try symbol.SymbolTable.init(allocator),
            .errors = std.ArrayList(TypeError).init(allocator),
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        self.symbols.deinit();
        self.errors.deinit();
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
                    sym.type = decl.type;

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
                sym.type = null; // TODO: Create function type

                self.symbols.define(sym) catch {
                    try self.errors.append(.{
                        .message = "duplicate function definition",
                        .location = node.location,
                        .kind = .duplicate_definition,
                    });
                };

                // Enter function scope for body
                if (func.body) |body| {
                    try self.symbols.enterScope(.function);
                    defer self.symbols.exitScope();

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
                const sym = symbol.Symbol.init(class.name, .class, node.location);

                self.symbols.define(sym) catch {
                    try self.errors.append(.{
                        .message = "duplicate class definition",
                        .location = node.location,
                        .kind = .duplicate_definition,
                    });
                };

                // Enter class scope for members
                try self.symbols.enterScope(.class);
                defer self.symbols.exitScope();

                for (class.members) |member| {
                    try self.collectDeclarations(member);
                }
            },
            .interface_decl => {
                const iface = &node.data.interface_decl;
                const sym = symbol.Symbol.init(iface.name, .interface, node.location);

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
                const sym = symbol.Symbol.init(alias.name, .type_alias, node.location);

                self.symbols.define(sym) catch {
                    try self.errors.append(.{
                        .message = "duplicate type alias definition",
                        .location = node.location,
                        .kind = .duplicate_definition,
                    });
                };
            },
            .property_decl => {
                const prop = &node.data.property_decl;
                var sym = symbol.Symbol.init(prop.name, .property, node.location);
                sym.type = prop.type;
                sym.mutable = !prop.readonly;

                self.symbols.define(sym) catch {};
            },
            .method_decl => {
                const method = &node.data.method_decl;
                const sym = symbol.Symbol.init(method.name, .method, node.location);

                self.symbols.define(sym) catch {};

                // Collect declarations in method body
                if (method.body) |body| {
                    try self.symbols.enterScope(.function);
                    defer self.symbols.exitScope();

                    for (method.params) |param| {
                        var param_sym = symbol.Symbol.init(param.name, .parameter, node.location);
                        param_sym.type = param.type;
                        self.symbols.define(param_sym) catch {};
                    }

                    try self.collectDeclarations(body);
                }
            },
            .block_stmt => {
                try self.symbols.enterScope(.block);
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
                try self.symbols.enterScope(.loop);
                defer self.symbols.exitScope();
                try self.collectDeclarations(node.data.while_stmt.body);
            },
            .for_stmt => {
                try self.symbols.enterScope(.loop);
                defer self.symbols.exitScope();

                if (node.data.for_stmt.init) |for_init| {
                    try self.collectDeclarations(for_init);
                }
                try self.collectDeclarations(node.data.for_stmt.body);
            },
            // Other nodes don't introduce declarations
            else => {},
        }
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
                                try self.errors.append(.{
                                    .message = "type mismatch in variable initialization",
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
                    // TODO: Check return type matches function return type
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
                try self.checkTypes(node.data.binary_expr.left);
                try self.checkTypes(node.data.binary_expr.right);
                try self.checkBinaryExpr(node);
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
                if (node.data.for_stmt.init) |for_init| try self.checkTypes(for_init);
                if (node.data.for_stmt.condition) |cond| try self.checkTypes(cond);
                if (node.data.for_stmt.update) |update| try self.checkTypes(update);
                try self.checkTypes(node.data.for_stmt.body);
            },
            .function_decl => {
                if (node.data.function_decl.body) |body| {
                    try self.checkTypes(body);
                }
            },
            .class_decl => {
                for (node.data.class_decl.members) |member| {
                    try self.checkTypes(member);
                }
            },
            .expression_stmt => {
                try self.checkTypes(node.data.expression_stmt);
            },
            .call_expr => {
                try self.checkTypes(node.data.call_expr.callee);
                for (node.data.call_expr.arguments) |arg| {
                    try self.checkTypes(arg);
                }
            },
            else => {},
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
                // Check type compatibility
                if (!self.typesCompatible(left_type.?, right_type.?)) {
                    try self.errors.append(.{
                        .message = "assignment type mismatch",
                        .location = node.location,
                        .kind = .type_mismatch,
                    });
                }
            },
            // Arithmetic: both sides must be number
            .add, .sub, .mul, .div, .mod => {
                if (left_type.?.kind != .number or right_type.?.kind != .number) {
                    // Special case: string + string is allowed
                    if (expr.op == .add and left_type.?.kind == .string and right_type.?.kind == .string) {
                        return;
                    }
                    try self.errors.append(.{
                        .message = "arithmetic operation requires number operands",
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
            // Ordering: numbers or strings
            .lt, .le, .gt, .ge => {
                const left_ok = left_type.?.kind == .number or left_type.?.kind == .string;
                const right_ok = right_type.?.kind == .number or right_type.?.kind == .string;
                if (!left_ok or !right_ok or left_type.?.kind != right_type.?.kind) {
                    try self.errors.append(.{
                        .message = "comparison requires number or string operands of same type",
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
            // Bitwise: numbers
            .bit_and, .bit_or, .bit_xor, .shl, .shr => {
                if (left_type.?.kind != .number or right_type.?.kind != .number) {
                    try self.errors.append(.{
                        .message = "bitwise operation requires number operands",
                        .location = node.location,
                        .kind = .invalid_operation,
                    });
                }
            },
        }
    }

    /// Check if two types are compatible
    fn typesCompatible(self: *TypeChecker, a: *types.Type, b: *types.Type) bool {
        _ = self;

        // Same kind is compatible
        if (a.kind == b.kind) return true;

        // unknown is compatible with anything
        if (a.kind == .unknown or b.kind == .unknown) return true;

        // never is compatible with anything (bottom type)
        if (a.kind == .never or b.kind == .never) return true;

        // TODO: Check structural compatibility for objects/classes
        // TODO: Check function signature compatibility
        // TODO: Check generic instantiation compatibility

        return false;
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

// Reference submodule tests to include them in test run
test {
    std.testing.refAllDecls(symbol);
    std.testing.refAllDecls(resolver);
    std.testing.refAllDecls(inference);
}
