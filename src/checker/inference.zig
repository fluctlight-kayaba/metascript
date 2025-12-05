/// Type Inference for Metascript Type Checker
///
/// Infers types for expressions without explicit annotations:
/// - Literal types (number, string, boolean, null)
/// - Binary expression types (arithmetic → number, comparison → boolean)
/// - Call expression return types
/// - Variable types from initializers
///
/// This runs after type resolution and before type checking.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const types = @import("../ast/types.zig");
const location = @import("../ast/location.zig");
const symbol_mod = @import("symbol.zig");

pub const TypeInference = struct {
    allocator: std.mem.Allocator,
    symbols: *symbol_mod.SymbolTable,
    type_arena: std.heap.ArenaAllocator,
    errors: std.ArrayList(Error),
    /// Current class type for `this` binding in methods
    current_class_type: ?*types.Type = null,

    pub const Error = struct {
        message: []const u8,
        location: location.SourceLocation,
    };

    pub fn init(allocator: std.mem.Allocator, symbols: *symbol_mod.SymbolTable) TypeInference {
        return .{
            .allocator = allocator,
            .symbols = symbols,
            .type_arena = std.heap.ArenaAllocator.init(allocator),
            .errors = std.ArrayList(Error).init(allocator),
            .current_class_type = null,
        };
    }

    pub fn deinit(self: *TypeInference) void {
        self.type_arena.deinit();
        self.errors.deinit();
    }

    /// Infer types for all expressions in the AST
    pub fn infer(self: *TypeInference, node: *ast.Node) !void {
        _ = try self.inferNode(node);
    }

    /// Error type for inference operations
    pub const InferError = std.mem.Allocator.Error || error{OutOfMemory};

    /// Infer type for a node and return it
    fn inferNode(self: *TypeInference, node: *ast.Node) InferError!?*types.Type {
        return switch (node.kind) {
            // Literals have known types
            .number_literal => {
                // Infer int32 for integer literals, number for floats
                const val = node.data.number_literal;
                if (@floor(val) == val and val >= -2147483648 and val <= 2147483647) {
                    // Integer literal that fits in int32
                    node.type = try self.createPrimitive(.int32, node.location);
                } else {
                    // Float literal or too large for int32
                    node.type = try self.createPrimitive(.number, node.location);
                }
                return node.type;
            },
            .string_literal => {
                node.type = try self.createPrimitive(.string, node.location);
                return node.type;
            },
            .boolean_literal => {
                node.type = try self.createPrimitive(.boolean, node.location);
                return node.type;
            },
            .null_literal => {
                // null has type 'unknown' until we have union types
                node.type = try self.createPrimitive(.unknown, node.location);
                return node.type;
            },

            // Identifier - look up type from symbol table
            .identifier => {
                const name = node.data.identifier;
                // Use lookupAll to search ALL scopes, not just current + parents.
                // This is needed because Phase 3 creates new scope objects, but
                // variables were defined in Phase 1's scopes.
                if (self.symbols.lookupAll(name)) |sym| {
                    node.type = sym.type;
                    return node.type;
                } else {
                    // Skip error for macro-generated code (nodes with dummy locations)
                    // Macro-generated code may reference parameters/locals that aren't
                    // in the symbol table because we don't fully analyze method bodies
                    if (!node.location.isDummy()) {
                        const msg = std.fmt.allocPrint(self.type_arena.allocator(), "undefined identifier '{s}'", .{name}) catch "undefined identifier";
                        try self.errors.append(.{
                            .message = msg,
                            .location = node.location,
                        });
                    }
                    return null;
                }
            },

            // Binary expressions
            .binary_expr => try self.inferBinaryExpr(node),

            // Unary expressions
            .unary_expr => try self.inferUnaryExpr(node),

            // Call expression - return type is callee's return type
            .call_expr => {
                const callee_type = try self.inferNode(node.data.call_expr.callee);
                for (node.data.call_expr.arguments) |arg| {
                    _ = try self.inferNode(arg);
                }

                if (callee_type) |ct| {
                    if (ct.kind == .function) {
                        node.type = ct.data.function.return_type;
                        return node.type;
                    }
                }
                return null;
            },

            // Member expression
            .member_expr => {
                const member = &node.data.member_expr;
                const object_type = try self.inferNode(member.object);

                // Only resolve property as expression if computed (obj[expr])
                // For non-computed (obj.prop), property is just an identifier name
                if (member.computed) {
                    _ = try self.inferNode(member.property);
                }

                // Look up property type from object type
                if (object_type) |obj_type| {
                    if (obj_type.kind == .object) {
                        const obj_data = obj_type.data.object;
                        // Get property name
                        const prop_name = if (!member.computed and member.property.kind == .identifier)
                            member.property.data.identifier
                        else
                            null;

                        if (prop_name) |name| {
                            // Search in properties
                            for (obj_data.properties) |prop| {
                                if (std.mem.eql(u8, prop.name, name)) {
                                    node.type = prop.type;
                                    return node.type;
                                }
                            }
                            // Search in methods
                            for (obj_data.methods) |method| {
                                if (std.mem.eql(u8, method.name, name)) {
                                    node.type = method.type;
                                    return node.type;
                                }
                            }
                        }
                    } else if (obj_type.kind == .array) {
                        // Array access returns element type
                        if (member.computed) {
                            node.type = obj_type.data.array;
                            return node.type;
                        }
                    }
                }

                return null;
            },

            // Array expression - infer element type
            .array_expr => {
                var element_type: ?*types.Type = null;
                for (node.data.array_expr.elements) |elem| {
                    const elem_type = try self.inferNode(elem);
                    if (element_type == null) {
                        element_type = elem_type;
                    }
                    // TODO: unify element types
                }
                if (element_type) |et| {
                    node.type = try self.createArrayType(et, node.location);
                }
                return node.type;
            },

            // Object expression
            .object_expr => {
                // Collect properties with their types
                var properties = std.ArrayList(types.ObjectType.Property).init(self.allocator);
                defer properties.deinit();

                for (node.data.object_expr.properties) |prop| {
                    switch (prop) {
                        .property => |p| {
                            // Don't infer type for keys - they're property names, not expressions
                            // (except for computed properties like [expr]: value)
                            const value_type = try self.inferNode(p.value);

                            // Only create property if we have an identifier key and value type
                            if (p.key.kind == .identifier and value_type != null) {
                                const key_name = p.key.data.identifier;
                                try properties.append(.{
                                    .name = key_name,
                                    .type = value_type.?,
                                    .optional = false,
                                });
                            }
                        },
                        .spread => |spread_node| {
                            // Infer spread argument type
                            const spread_type = try self.inferNode(spread_node);

                            // If spread has object type, merge its properties
                            if (spread_type) |st| {
                                if (st.kind == .object) {
                                    const obj_data = st.data.object;
                                    for (obj_data.properties) |spread_prop| {
                                        try properties.append(spread_prop);
                                    }
                                }
                            }
                        },
                    }
                }

                // Create ObjectType from collected properties
                const arena_alloc = self.type_arena.allocator();

                const obj_type_data = try arena_alloc.create(types.ObjectType);
                obj_type_data.* = .{
                    .properties = try arena_alloc.dupe(
                        types.ObjectType.Property,
                        properties.items,
                    ),
                    .methods = &[_]types.ObjectType.Property{},
                };

                const obj_type = try arena_alloc.create(types.Type);
                obj_type.* = .{
                    .kind = .object,
                    .location = node.location,
                    .data = .{ .object = obj_type_data },
                };

                node.type = obj_type;
                return obj_type;
            },

            // Function expression
            .function_expr => {
                const func = &node.data.function_expr;
                _ = try self.inferNode(func.body);
                // Type is the function's declared type
                return func.return_type;
            },

            // Conditional expression
            .conditional_expr => {
                _ = try self.inferNode(node.data.conditional_expr.condition);
                const consequent = try self.inferNode(node.data.conditional_expr.consequent);
                _ = try self.inferNode(node.data.conditional_expr.alternate);
                // Type is union of branches (simplified: use consequent)
                node.type = consequent;
                return node.type;
            },

            // Spread element - type is the type of the argument
            .spread_element => {
                const arg_type = try self.inferNode(node.data.spread_element.argument);
                node.type = arg_type;
                return node.type;
            },

            // Move expression - type is the type of the operand being moved
            .move_expr => {
                const operand_type = try self.inferNode(node.data.move_expr.operand);
                node.type = operand_type;
                return node.type;
            },

            // New expression
            .new_expr => {
                _ = try self.inferNode(node.data.new_expr.callee);
                for (node.data.new_expr.arguments) |arg| {
                    _ = try self.inferNode(arg);
                }
                // Type is the class being instantiated
                // Look up the class name in the symbol table
                const callee = node.data.new_expr.callee;
                if (callee.kind == .identifier) {
                    const class_name = callee.data.identifier;
                    if (self.symbols.lookup(class_name)) |class_sym| {
                        // The class symbol's type is the object type for instances
                        node.type = class_sym.type;
                        return node.type;
                    }
                }
                return null;
            },

            // Statements - recurse but don't have types themselves
            .program => {
                for (node.data.program.statements) |stmt| {
                    _ = try self.inferNode(stmt);
                }
                return null;
            },
            .block_stmt => {
                for (node.data.block_stmt.statements) |stmt| {
                    _ = try self.inferNode(stmt);
                }
                return null;
            },
            .variable_stmt => {
                const is_const = node.data.variable_stmt.kind == .@"const";
                for (node.data.variable_stmt.declarations) |*decl| {
                    if (decl.init) |init_expr| {
                        const init_type = try self.inferNode(init_expr);

                        // Handle explicit type annotations
                        if (decl.type) |explicit_type| {
                            // WORKAROUND: If type is an unresolved type_reference, fallback to inferred type
                            // This allows: const base: Point = { x: 1, y: 2 } to work even without
                            // full interface support. The type reference is validated but ignored.
                            // TODO: Implement proper interface type resolution (see SPREAD_COMPLETE.md Phase 3)
                            if (explicit_type.kind == .type_reference) {
                                decl.type = init_type; // Use inferred type from literal
                            }
                            // Otherwise keep the explicit type (already resolved by TypeResolver)
                        } else {
                            // No explicit type - infer from initializer
                            decl.type = init_type;
                        }

                        // Update the symbol's type in the symbol table
                        // Use updateTypeAll because the symbol was defined in Phase 1's scope,
                        // not the current scope (which is a new scope created in Phase 3)
                        if (decl.type) |dt| {
                            self.symbols.updateTypeAll(decl.name, dt) catch {
                                // Symbol not found in any scope - define it in current scope
                                var var_sym = symbol_mod.Symbol.init(decl.name, .variable, node.location);
                                var_sym.type = dt;
                                var_sym.mutable = !is_const;
                                self.symbols.define(var_sym) catch {};
                            };
                        }
                    }
                }
                return null;
            },
            .expression_stmt => {
                _ = try self.inferNode(node.data.expression_stmt);
                return null;
            },
            .if_stmt => {
                _ = try self.inferNode(node.data.if_stmt.condition);
                _ = try self.inferNode(node.data.if_stmt.consequent);
                if (node.data.if_stmt.alternate) |alt| {
                    _ = try self.inferNode(alt);
                }
                return null;
            },
            .while_stmt => {
                _ = try self.inferNode(node.data.while_stmt.condition);
                // Enter loop scope for consistency with other phases
                try self.symbols.enterScopeWithLocation(.loop, node.location);
                defer self.symbols.exitScope();
                _ = try self.inferNode(node.data.while_stmt.body);
                return null;
            },
            .for_stmt => {
                try self.symbols.enterScopeWithLocation(.loop, node.location);
                defer self.symbols.exitScope();

                if (node.data.for_stmt.init) |for_init| _ = try self.inferNode(for_init);
                if (node.data.for_stmt.condition) |cond| _ = try self.inferNode(cond);
                if (node.data.for_stmt.update) |update| _ = try self.inferNode(update);
                _ = try self.inferNode(node.data.for_stmt.body);
                return null;
            },
            .return_stmt => {
                if (node.data.return_stmt.argument) |arg| {
                    _ = try self.inferNode(arg);
                }
                return null;
            },
            .function_decl => {
                const func = &node.data.function_decl;
                if (func.body) |body| {
                    // Enter function scope with body boundaries for correct shadowing lookup
                    try self.symbols.enterScopeWithLocation(.function, body.location);
                    defer self.symbols.exitScope();

                    // Register parameters in scope
                    for (func.params) |param| {
                        var param_sym = symbol_mod.Symbol.init(param.name, .parameter, node.location);
                        param_sym.type = param.type;
                        self.symbols.define(param_sym) catch {}; // Ignore duplicate errors
                    }

                    _ = try self.inferNode(body);
                }
                return null;
            },
            .class_decl => {
                // Look up the class type from symbol table
                const class_name = node.data.class_decl.name;
                const saved_class_type = self.current_class_type;
                if (self.symbols.lookup(class_name)) |class_sym| {
                    self.current_class_type = class_sym.type;
                }
                defer self.current_class_type = saved_class_type;

                for (node.data.class_decl.members) |member| {
                    _ = try self.inferNode(member);
                }
                return null;
            },
            .interface_decl => {
                for (node.data.interface_decl.members) |member| {
                    _ = try self.inferNode(member);
                }
                return null;
            },
            .property_decl => {
                if (node.data.property_decl.init) |prop_init| {
                    const init_type = try self.inferNode(prop_init);
                    if (node.data.property_decl.type == null) {
                        node.data.property_decl.type = init_type;
                    }
                }
                return null;
            },
            .method_decl => {
                const method = &node.data.method_decl;
                if (method.body) |body| {
                    // Enter function scope with body boundaries for correct shadowing lookup
                    try self.symbols.enterScopeWithLocation(.function, body.location);
                    defer self.symbols.exitScope();

                    // Register `this` with the current class type
                    if (self.current_class_type) |class_type| {
                        var this_sym = symbol_mod.Symbol.init("this", .variable, node.location);
                        this_sym.type = class_type;
                        this_sym.mutable = false;
                        self.symbols.define(this_sym) catch {};
                    }

                    // Register parameters in scope
                    for (method.params) |param| {
                        var param_sym = symbol_mod.Symbol.init(param.name, .parameter, node.location);
                        param_sym.type = param.type;
                        self.symbols.define(param_sym) catch {}; // Ignore duplicate errors
                    }

                    _ = try self.inferNode(body);
                }
                return null;
            },
            .constructor_decl => {
                // Constructor body is always present
                _ = try self.inferNode(node.data.constructor_decl.body);
                return null;
            },

            // Nodes that don't need inference
            .break_stmt,
            .continue_stmt,
            .import_decl,
            .export_decl,
            .type_alias_decl,
            .macro_decl,
            .extern_macro_decl,
            .macro_invocation,
            .comptime_block,
            .compile_error,
            .quote_expr,
            .type_annotation,
            => null,
        };
    }

    /// Infer type for binary expression
    fn inferBinaryExpr(self: *TypeInference, node: *ast.Node) InferError!?*types.Type {
        const expr = &node.data.binary_expr;
        const left_type = try self.inferNode(expr.left);
        const right_type = try self.inferNode(expr.right);

        // Determine result type based on operator
        node.type = switch (expr.op) {
            // Assignment → type of right-hand side
            .assign => right_type orelse try self.createPrimitive(.unknown, node.location),

            // Arithmetic: preserve int type if both operands are int
            .add => blk: {
                const is_left_string = if (left_type) |lt| lt.kind == .string else false;
                const is_right_string = if (right_type) |rt| rt.kind == .string else false;
                if (is_left_string or is_right_string) {
                    break :blk try self.createPrimitive(.string, node.location);
                }
                // int + int → int (preserve precision)
                break :blk try self.inferArithmeticResultType(left_type, right_type, node.location);
            },

            // Other arithmetic: preserve int if both are int
            .sub, .mul => try self.inferArithmeticResultType(left_type, right_type, node.location),

            // Division/modulo always produce number (could have fractional result)
            .div => try self.createPrimitive(.number, node.location),
            .mod => try self.inferArithmeticResultType(left_type, right_type, node.location),

            // Comparison operators → boolean
            .eq, .ne, .lt, .le, .gt, .ge => try self.createPrimitive(.boolean, node.location),

            // Logical operators → boolean
            .@"and", .@"or" => try self.createPrimitive(.boolean, node.location),

            // Bitwise operators → int32 (bitwise ops work on integers)
            .bit_and, .bit_or, .bit_xor, .shl, .shr => try self.createPrimitive(.int32, node.location),

            // Nullish coalesce (??) → type of left if not null, else type of right
            .nullish_coalesce => left_type orelse right_type orelse try self.createPrimitive(.unknown, node.location),
        };

        return node.type;
    }

    /// Infer result type for arithmetic operations
    /// int + int → int, otherwise → number (widening)
    fn inferArithmeticResultType(self: *TypeInference, left: ?*types.Type, right: ?*types.Type, loc: location.SourceLocation) InferError!*types.Type {
        const left_is_int = if (left) |lt| (lt.kind == .int32 or lt.kind == .int64) else false;
        const right_is_int = if (right) |rt| (rt.kind == .int32 or rt.kind == .int64) else false;

        // Both int → preserve int type (use wider if different)
        if (left_is_int and right_is_int) {
            // If either is int64, result is int64
            const left_is_64 = if (left) |lt| lt.kind == .int64 else false;
            const right_is_64 = if (right) |rt| rt.kind == .int64 else false;
            if (left_is_64 or right_is_64) {
                return try self.createPrimitive(.int64, loc);
            }
            return try self.createPrimitive(.int32, loc);
        }

        // Mixed or float → number (double)
        return try self.createPrimitive(.number, loc);
    }

    /// Infer type for unary expression
    fn inferUnaryExpr(self: *TypeInference, node: *ast.Node) InferError!?*types.Type {
        const expr = &node.data.unary_expr;
        _ = try self.inferNode(expr.argument);

        node.type = switch (expr.op) {
            // Negation → number
            .neg => try self.createPrimitive(.number, node.location),

            // Logical not → boolean
            .not => try self.createPrimitive(.boolean, node.location),

            // Bitwise not → number
            .bit_not => try self.createPrimitive(.number, node.location),

            // typeof → string
            .typeof => try self.createPrimitive(.string, node.location),

            // void → void
            .void => try self.createPrimitive(.void, node.location),
        };

        return node.type;
    }

    /// Create a primitive type
    fn createPrimitive(self: *TypeInference, kind: types.TypeKind, loc: location.SourceLocation) !*types.Type {
        const arena_alloc = self.type_arena.allocator();
        const t = try arena_alloc.create(types.Type);
        t.* = types.Type{
            .kind = kind,
            .location = loc,
            .data = switch (kind) {
                .number => .{ .number = {} },
                .string => .{ .string = {} },
                .boolean => .{ .boolean = {} },
                .void => .{ .void = {} },
                .unknown => .{ .unknown = {} },
                .never => .{ .never = {} },
                .int8 => .{ .int8 = {} },
                .int16 => .{ .int16 = {} },
                .int32 => .{ .int32 = {} },
                .int64 => .{ .int64 = {} },
                .uint8 => .{ .uint8 = {} },
                .uint16 => .{ .uint16 = {} },
                .uint32 => .{ .uint32 = {} },
                .uint64 => .{ .uint64 = {} },
                .float32 => .{ .float32 = {} },
                .float64 => .{ .float64 = {} },
                else => unreachable,
            },
        };
        return t;
    }

    /// Create an array type
    fn createArrayType(self: *TypeInference, element: *types.Type, loc: location.SourceLocation) !*types.Type {
        const arena_alloc = self.type_arena.allocator();
        const t = try arena_alloc.create(types.Type);
        t.* = types.Type{
            .kind = .array,
            .location = loc,
            .data = .{ .array = element },
        };
        return t;
    }

    /// Check if there are any errors
    pub fn hasErrors(self: *TypeInference) bool {
        return self.errors.items.len > 0;
    }

    /// Get all errors
    pub fn getErrors(self: *TypeInference) []const Error {
        return self.errors.items;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "inference: integer literal infers int32" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // Create an integer literal node (42.0 is an integer value)
    var node = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
    };

    _ = try inference.inferNode(&node);

    try std.testing.expect(node.type != null);
    // Integer literals should infer as int32, not number
    try std.testing.expectEqual(types.TypeKind.int32, node.type.?.kind);
}

test "inference: float literal infers number" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // Create a float literal node (3.14 has decimal part)
    var node = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 3.14 },
    };

    _ = try inference.inferNode(&node);

    try std.testing.expect(node.type != null);
    // Float literals should infer as number
    try std.testing.expectEqual(types.TypeKind.number, node.type.?.kind);
}

test "inference: string literal" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    var node = ast.Node{
        .kind = .string_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string_literal = "hello" },
    };

    _ = try inference.inferNode(&node);

    try std.testing.expect(node.type != null);
    try std.testing.expectEqual(types.TypeKind.string, node.type.?.kind);
}

test "inference: boolean literal" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    var node = ast.Node{
        .kind = .boolean_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .boolean_literal = true },
    };

    _ = try inference.inferNode(&node);

    try std.testing.expect(node.type != null);
    try std.testing.expectEqual(types.TypeKind.boolean, node.type.?.kind);
}

test "inference: arithmetic binary expression int + int = int32" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // Create 1 + 2 (both integers)
    var left = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 1.0 },
    };

    var right = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 2.0 },
    };

    var node = ast.Node{
        .kind = .binary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .add,
            .left = &left,
            .right = &right,
        } },
    };

    _ = try inference.inferNode(&node);

    try std.testing.expect(node.type != null);
    // int + int = int32 (type preservation)
    try std.testing.expectEqual(types.TypeKind.int32, node.type.?.kind);
}

test "inference: arithmetic binary expression int + float = number" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // Create 1 + 2.5 (int + float = number)
    var left = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 1.0 },
    };

    var right = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 2.5 },
    };

    var node = ast.Node{
        .kind = .binary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .add,
            .left = &left,
            .right = &right,
        } },
    };

    _ = try inference.inferNode(&node);

    try std.testing.expect(node.type != null);
    // int + float = number (widening)
    try std.testing.expectEqual(types.TypeKind.number, node.type.?.kind);
}

test "inference: comparison binary expression" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // Create 1 < 2
    var left = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 1.0 },
    };

    var right = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 2.0 },
    };

    var node = ast.Node{
        .kind = .binary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .binary_expr = .{
            .op = .lt,
            .left = &left,
            .right = &right,
        } },
    };

    _ = try inference.inferNode(&node);

    try std.testing.expect(node.type != null);
    try std.testing.expectEqual(types.TypeKind.boolean, node.type.?.kind);
}

test "inference: unary negation" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    var arg = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 5.0 },
    };

    var node = ast.Node{
        .kind = .unary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .unary_expr = .{
            .op = .neg,
            .argument = &arg,
        } },
    };

    _ = try inference.inferNode(&node);

    try std.testing.expect(node.type != null);
    try std.testing.expectEqual(types.TypeKind.number, node.type.?.kind);
}

test "inference: unary not" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    var arg = ast.Node{
        .kind = .boolean_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .boolean_literal = true },
    };

    var node = ast.Node{
        .kind = .unary_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .unary_expr = .{
            .op = .not,
            .argument = &arg,
        } },
    };

    _ = try inference.inferNode(&node);

    try std.testing.expect(node.type != null);
    try std.testing.expectEqual(types.TypeKind.boolean, node.type.?.kind);
}

test "inference: identifier lookup" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // Define a variable with a type
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var sym = symbol_mod.Symbol.init("x", .variable, location.SourceLocation.dummy());
    sym.type = &num_type;
    try symbols.define(sym);

    // Look up identifier
    var node = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "x" },
    };

    _ = try inference.inferNode(&node);

    try std.testing.expect(node.type != null);
    try std.testing.expectEqual(types.TypeKind.number, node.type.?.kind);
}

test "inference: undefined identifier reports error" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // Use a real location (not dummy) to test error reporting
    var node = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.init(1, .{ .line = 5, .column = 10 }, .{ .line = 5, .column = 22 }),
        .data = .{ .identifier = "undefinedVar" },
    };

    _ = try inference.inferNode(&node);

    try std.testing.expect(inference.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), inference.errors.items.len);
}

test "inference: macro-generated identifier skips error" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // Use dummy location (macro-generated code)
    var node = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "macroGenerated" },
    };

    _ = try inference.inferNode(&node);

    // Should NOT report error for macro-generated code
    try std.testing.expect(!inference.hasErrors());
}

test "inference: identifier in child scope found via lookupAll" {
    // This tests that identifiers defined in a child scope (e.g., function body)
    // can be found even when the current scope is global (after exitScope).
    // This is critical for Phase 3 type inference which creates new scopes
    // but needs to find symbols defined in Phase 1's scopes.
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    // Simulate Phase 1: define variable in function scope
    try symbols.enterScope(.function);

    var int_type = types.Type{
        .kind = .int32,
        .location = location.SourceLocation.dummy(),
        .data = .{ .int32 = {} },
    };

    var sym = symbol_mod.Symbol.init("counter", .variable, location.SourceLocation.dummy());
    sym.type = &int_type;
    try symbols.define(sym);

    symbols.exitScope(); // Back to global scope

    // Simulate Phase 3: inference runs with current scope = global
    // But needs to find "counter" which is in the (exited) function scope
    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    var node = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.init(1, .{ .line = 5, .column = 4 }, .{ .line = 5, .column = 11 }),
        .data = .{ .identifier = "counter" },
    };

    _ = try inference.inferNode(&node);

    // Should find the variable and NOT report an error
    try std.testing.expect(!inference.hasErrors());
    try std.testing.expect(node.type != null);
    try std.testing.expectEqual(types.TypeKind.int32, node.type.?.kind);
}

test "inference: nested function scope variables resolved" {
    // Tests that variables in deeply nested scopes are found
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    // Create nested scopes: global -> function -> block
    try symbols.enterScope(.function);
    try symbols.enterScope(.block);

    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var sym = symbol_mod.Symbol.init("deepVar", .variable, location.SourceLocation.dummy());
    sym.type = &str_type;
    try symbols.define(sym);

    symbols.exitScope(); // exit block
    symbols.exitScope(); // exit function, back to global

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    var node = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.init(1, .{ .line = 10, .column = 8 }, .{ .line = 10, .column = 15 }),
        .data = .{ .identifier = "deepVar" },
    };

    _ = try inference.inferNode(&node);

    try std.testing.expect(!inference.hasErrors());
    try std.testing.expect(node.type != null);
    try std.testing.expectEqual(types.TypeKind.string, node.type.?.kind);
}

test "inference: member expression resolves property type" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // Create object type { name: string }
    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    var props = [_]types.ObjectType.Property{
        .{ .name = "name", .type = &str_type, .optional = false },
    };
    var methods = [_]types.ObjectType.Property{};

    var obj_data = types.ObjectType{
        .properties = &props,
        .methods = &methods,
    };

    var obj_type = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = &obj_data },
    };

    // Create identifier for object
    var obj_node = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "user" },
        .type = &obj_type,
    };

    // Register the object in symbol table
    var sym = symbol_mod.Symbol.init("user", .variable, location.SourceLocation.dummy());
    sym.type = &obj_type;
    try symbols.define(sym);

    // Create property access: user.name
    var prop_node = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "name" },
    };

    var member_node = ast.Node{
        .kind = .member_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .member_expr = .{
            .object = &obj_node,
            .property = &prop_node,
            .computed = false,
        } },
    };

    const result_type = try inference.inferNode(&member_node);

    // Should resolve to string type
    try std.testing.expect(result_type != null);
    try std.testing.expectEqual(types.TypeKind.string, result_type.?.kind);
}

test "inference: array index access returns element type" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // Create array type number[]
    var num_type = types.Type{
        .kind = .number,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number = {} },
    };

    var arr_type = types.Type{
        .kind = .array,
        .location = location.SourceLocation.dummy(),
        .data = .{ .array = &num_type },
    };

    // Create identifier for array
    var arr_node = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "numbers" },
        .type = &arr_type,
    };

    // Register the array in symbol table
    var sym = symbol_mod.Symbol.init("numbers", .variable, location.SourceLocation.dummy());
    sym.type = &arr_type;
    try symbols.define(sym);

    // Create index: numbers[0]
    var index_node = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 0 },
    };

    var member_node = ast.Node{
        .kind = .member_expr,
        .location = location.SourceLocation.dummy(),
        .data = .{ .member_expr = .{
            .object = &arr_node,
            .property = &index_node,
            .computed = true, // Array access is computed
        } },
    };

    const result_type = try inference.inferNode(&member_node);

    // Should resolve to number type (element type)
    try std.testing.expect(result_type != null);
    try std.testing.expectEqual(types.TypeKind.number, result_type.?.kind);
}

// ============================================================================
// TDD: NEW FAILING TESTS for class type inference
// These tests document the expected behavior we need to implement
// ============================================================================

test "inference: new expression infers class type from symbol table" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // First, register a class "User" in the symbol table
    // The class symbol should have an object type
    var str_type = types.Type{
        .kind = .string,
        .location = location.SourceLocation.dummy(),
        .data = .{ .string = {} },
    };

    // Create object type with properties for User class
    var props = [_]types.ObjectType.Property{
        .{ .name = "name", .type = &str_type, .optional = false },
    };

    var user_obj = types.ObjectType{
        .properties = &props,
        .methods = &[_]types.ObjectType.Property{},
        .name = "User",
    };

    var user_type = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = &user_obj },
    };

    // Register the class in symbol table
    var class_sym = symbol_mod.Symbol.init("User", .class, location.SourceLocation.dummy());
    class_sym.type = &user_type;
    try symbols.define(class_sym);

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

    const result_type = try inference.inferNode(&new_expr);

    // KEY TEST: new User() should have the User class's object type
    try std.testing.expect(result_type != null);
    try std.testing.expectEqual(types.TypeKind.object, result_type.?.kind);
    // The object type should have name = "User"
    try std.testing.expectEqualStrings("User", result_type.?.data.object.name orelse "");
}

test "inference: variable initialized with new expr gets class type" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // Register class "Entity" with id: int property
    var int_type = types.Type{
        .kind = .int32,
        .location = location.SourceLocation.dummy(),
        .data = .{ .int32 = {} },
    };

    var props = [_]types.ObjectType.Property{
        .{ .name = "id", .type = &int_type, .optional = false },
    };

    var entity_obj = types.ObjectType{
        .properties = &props,
        .methods = &[_]types.ObjectType.Property{},
        .name = "Entity",
    };

    var entity_type = types.Type{
        .kind = .object,
        .location = location.SourceLocation.dummy(),
        .data = .{ .object = &entity_obj },
    };

    var class_sym = symbol_mod.Symbol.init("Entity", .class, location.SourceLocation.dummy());
    class_sym.type = &entity_type;
    try symbols.define(class_sym);

    // Create: let e = new Entity();
    var class_id = ast.Node{
        .kind = .identifier,
        .location = location.SourceLocation.dummy(),
        .data = .{ .identifier = "Entity" },
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
        .name = "e",
        .type = null, // Infer from init
        .init = &new_expr,
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

    _ = try inference.inferNode(&var_stmt);

    // KEY TEST: The variable "e" should now be in the symbol table with Entity type
    const e_sym = symbols.lookupAll("e");
    try std.testing.expect(e_sym != null);
    try std.testing.expect(e_sym.?.type != null);
    try std.testing.expectEqual(types.TypeKind.object, e_sym.?.type.?.kind);
    try std.testing.expectEqualStrings("Entity", e_sym.?.type.?.data.object.name orelse "");
}
