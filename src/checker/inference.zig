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
                node.type = try self.createPrimitive(.number, node.location);
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
                if (self.symbols.lookup(name)) |sym| {
                    node.type = sym.type;
                    return node.type;
                } else {
                    // Skip error for macro-generated code (nodes with dummy locations)
                    // Macro-generated code may reference parameters/locals that aren't
                    // in the symbol table because we don't fully analyze method bodies
                    if (!node.location.isDummy()) {
                        try self.errors.append(.{
                            .message = "undefined identifier",
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
                _ = try self.inferNode(member.object);
                // Only resolve property as expression if computed (obj[expr])
                // For non-computed (obj.prop), property is just an identifier name
                if (member.computed) {
                    _ = try self.inferNode(member.property);
                }
                // TODO: look up property type from object type
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

            // New expression
            .new_expr => {
                _ = try self.inferNode(node.data.new_expr.callee);
                for (node.data.new_expr.arguments) |arg| {
                    _ = try self.inferNode(arg);
                }
                // Type is the class being instantiated
                // TODO: get class type from callee
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

                        // Update symbol table with final type
                        if (decl.type) |dt| {
                            self.symbols.updateType(decl.name, dt) catch {
                                // Symbol not found - this can happen for macro-generated code
                                // Just continue without error
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
                _ = try self.inferNode(node.data.while_stmt.body);
                return null;
            },
            .for_stmt => {
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
                if (node.data.function_decl.body) |body| {
                    _ = try self.inferNode(body);
                }
                return null;
            },
            .class_decl => {
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
                if (node.data.method_decl.body) |body| {
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
            .macro_invocation,
            .comptime_block,
            .compile_error,
            .type_annotation,
            => null,
        };
    }

    /// Infer type for binary expression
    fn inferBinaryExpr(self: *TypeInference, node: *ast.Node) InferError!?*types.Type {
        const expr = &node.data.binary_expr;
        const left_type = try self.inferNode(expr.left);
        const right_type = try self.inferNode(expr.right);
        _ = left_type;

        // Determine result type based on operator
        node.type = switch (expr.op) {
            // Assignment → type of right-hand side
            .assign => right_type orelse try self.createPrimitive(.unknown, node.location),

            // Arithmetic operators → number
            .add, .sub, .mul, .div, .mod => try self.createPrimitive(.number, node.location),

            // Comparison operators → boolean
            .eq, .ne, .lt, .le, .gt, .ge => try self.createPrimitive(.boolean, node.location),

            // Logical operators → boolean
            .@"and", .@"or" => try self.createPrimitive(.boolean, node.location),

            // Bitwise operators → number
            .bit_and, .bit_or, .bit_xor, .shl, .shr => try self.createPrimitive(.number, node.location),
        };

        return node.type;
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

test "inference: number literal" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // Create a number literal node
    var node = ast.Node{
        .kind = .number_literal,
        .location = location.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
    };

    _ = try inference.inferNode(&node);

    try std.testing.expect(node.type != null);
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

test "inference: arithmetic binary expression" {
    const allocator = std.testing.allocator;

    var symbols = try symbol_mod.SymbolTable.init(allocator);
    defer symbols.deinit();

    var inference = TypeInference.init(allocator, &symbols);
    defer inference.deinit();

    // Create 1 + 2
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
