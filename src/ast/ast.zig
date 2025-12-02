const std = @import("std");

// Re-export all AST modules for convenience
pub const location = @import("location.zig");
pub const types = @import("types.zig");
pub const node = @import("node.zig");

pub const FileId = location.FileId;
pub const Position = location.Position;
pub const SourceLocation = location.SourceLocation;
pub const FileRegistry = location.FileRegistry;

pub const TypeKind = types.TypeKind;
pub const Type = types.Type;
pub const ObjectType = types.ObjectType;
pub const FunctionType = types.FunctionType;
pub const GenericParam = types.GenericParam;

pub const NodeKind = node.NodeKind;
pub const Node = node.Node;
pub const BinaryOp = node.BinaryOp;
pub const UnaryOp = node.UnaryOp;
pub const MacroInvocation = node.MacroInvocation;
pub const ComptimeBlock = node.ComptimeBlock;
pub const Program = node.Program;

/// AST Arena - manages memory for AST nodes
pub const ASTArena = struct {
    arena: std.heap.ArenaAllocator,
    file_registry: FileRegistry,

    pub fn init(backing_allocator: std.mem.Allocator) ASTArena {
        return .{
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
            .file_registry = FileRegistry.init(backing_allocator),
        };
    }

    pub fn deinit(self: *ASTArena) void {
        self.file_registry.deinit();
        self.arena.deinit();
    }

    pub fn allocator(self: *ASTArena) std.mem.Allocator {
        return self.arena.allocator();
    }

    /// Create a new AST node
    pub fn createNode(self: *ASTArena, kind: NodeKind, loc: SourceLocation, data: Node.NodeData) !*Node {
        const node_ptr = try self.allocator().create(Node);
        node_ptr.* = .{
            .kind = kind,
            .location = loc,
            .data = data,
        };
        return node_ptr;
    }

    /// Create a new type
    pub fn createType(self: *ASTArena, kind: TypeKind, loc: SourceLocation, data: Type.TypeData) !*Type {
        const type_ptr = try self.allocator().create(Type);
        type_ptr.* = .{
            .kind = kind,
            .location = loc,
            .data = data,
        };
        return type_ptr;
    }

    /// Register a file and get its ID
    pub fn addFile(self: *ASTArena, path: []const u8) !FileId {
        return try self.file_registry.addFile(path);
    }

    /// Get file path from ID
    pub fn getFilePath(self: *const ASTArena, file_id: FileId) ?[]const u8 {
        return self.file_registry.getPath(file_id);
    }
};

/// AST Visitor pattern for traversing the tree
pub const Visitor = struct {
    /// Visit a node and its children
    pub fn visit(self: *Visitor, node_ptr: *Node) anyerror!void {
        switch (node_ptr.kind) {
            .program => try self.visitProgram(node_ptr),
            .function_decl => try self.visitFunctionDecl(node_ptr),
            .class_decl => try self.visitClassDecl(node_ptr),
            .macro_invocation => try self.visitMacroInvocation(node_ptr),
            .binary_expr => try self.visitBinaryExpr(node_ptr),
            .call_expr => try self.visitCallExpr(node_ptr),
            .block_stmt => try self.visitBlockStmt(node_ptr),
            // Add more cases as needed
            else => {},
        }
    }

    // Subclasses override these methods
    pub fn visitProgram(self: *Visitor, node_ptr: *Node) anyerror!void {
        const program = &node_ptr.data.program;
        for (program.statements) |stmt| {
            try self.visit(stmt);
        }
    }

    pub fn visitFunctionDecl(self: *Visitor, node_ptr: *Node) anyerror!void {
        _ = self;
        _ = node_ptr;
    }

    pub fn visitClassDecl(self: *Visitor, node_ptr: *Node) anyerror!void {
        _ = self;
        _ = node_ptr;
    }

    pub fn visitMacroInvocation(self: *Visitor, node_ptr: *Node) anyerror!void {
        _ = self;
        _ = node_ptr;
    }

    pub fn visitBinaryExpr(self: *Visitor, node_ptr: *Node) anyerror!void {
        const binary = &node_ptr.data.binary_expr;
        try self.visit(binary.left);
        try self.visit(binary.right);
    }

    pub fn visitCallExpr(self: *Visitor, node_ptr: *Node) anyerror!void {
        const call = &node_ptr.data.call_expr;
        try self.visit(call.callee);
        for (call.arguments) |arg| {
            try self.visit(arg);
        }
    }

    pub fn visitBlockStmt(self: *Visitor, node_ptr: *Node) anyerror!void {
        const block = &node_ptr.data.block_stmt;
        for (block.statements) |stmt| {
            try self.visit(stmt);
        }
    }
};

/// Pretty printer for debugging AST
pub const ASTPrinter = struct {
    arena: *ASTArena,
    indent: usize,

    pub fn init(arena: *ASTArena) ASTPrinter {
        return .{
            .arena = arena,
            .indent = 0,
        };
    }

    pub fn print(self: *ASTPrinter, node_ptr: *const Node) void {
        self.printIndent();
        std.debug.print("{s}", .{@tagName(node_ptr.kind)});

        switch (node_ptr.kind) {
            .number_literal => std.debug.print(" {d}", .{node_ptr.data.number_literal}),
            .string_literal => std.debug.print(" \"{s}\"", .{node_ptr.data.string_literal}),
            .identifier => std.debug.print(" {s}", .{node_ptr.data.identifier}),
            .macro_invocation => {
                const macro = &node_ptr.data.macro_invocation;
                std.debug.print(" @{s}(...)", .{macro.name});
            },
            else => {},
        }

        std.debug.print("\n", .{});
    }

    fn printIndent(self: *ASTPrinter) void {
        for (0..self.indent) |_| {
            std.debug.print("  ", .{});
        }
    }
};

test "AST arena basic operations" {
    var arena = ASTArena.init(std.testing.allocator);
    defer arena.deinit();

    // Create a simple number literal node
    const loc = SourceLocation.dummy();
    const num_node = try arena.createNode(
        .number_literal,
        loc,
        .{ .number_literal = 42.0 },
    );

    try std.testing.expectEqual(NodeKind.number_literal, num_node.kind);
    try std.testing.expectEqual(@as(f64, 42.0), num_node.data.number_literal);
}

test "file registry" {
    var arena = ASTArena.init(std.testing.allocator);
    defer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    const path = arena.getFilePath(file_id);

    try std.testing.expect(path != null);
    try std.testing.expectEqualStrings("test.ms", path.?);
}

// ============================================================================
// AST Structural Hashing (for content-addressed macro caching)
// ============================================================================

/// Hash an AST node structurally (ignoring source locations).
/// Two nodes with the same structure but different locations produce the same hash.
/// This is critical for content-addressed macro output caching.
pub fn hashAstNode(n: *const Node) u64 {
    var hasher = std.hash.Wyhash.init(0);
    hashAstNodeImpl(&hasher, n);
    return hasher.final();
}

/// Internal implementation that updates a hasher
fn hashAstNodeImpl(hasher: *std.hash.Wyhash, n: *const Node) void {
    // Hash the node kind
    std.hash.autoHash(hasher, @intFromEnum(n.kind));

    // Hash kind-specific content (NOT location - we want structural equality)
    switch (n.kind) {
        // Literals
        .number_literal => {
            // Hash the bits of the float
            const bits: u64 = @bitCast(n.data.number_literal);
            std.hash.autoHash(hasher, bits);
        },
        .string_literal => hasher.update(n.data.string_literal),
        .boolean_literal => std.hash.autoHash(hasher, n.data.boolean_literal),
        .null_literal => {},

        // Identifier
        .identifier => hasher.update(n.data.identifier),

        // Binary expression
        .binary_expr => {
            const binary = &n.data.binary_expr;
            std.hash.autoHash(hasher, @intFromEnum(binary.op));
            hashAstNodeImpl(hasher, binary.left);
            hashAstNodeImpl(hasher, binary.right);
        },

        // Unary expression
        .unary_expr => {
            const unary = &n.data.unary_expr;
            std.hash.autoHash(hasher, @intFromEnum(unary.op));
            hashAstNodeImpl(hasher, unary.argument);
        },

        // Call expression
        .call_expr => {
            const call = &n.data.call_expr;
            hashAstNodeImpl(hasher, call.callee);
            std.hash.autoHash(hasher, call.arguments.len);
            for (call.arguments) |arg| {
                hashAstNodeImpl(hasher, arg);
            }
            std.hash.autoHash(hasher, call.type_args.len);
            for (call.type_args) |type_arg| {
                hashTypeImpl(hasher, type_arg);
            }
        },

        // Member expression
        .member_expr => {
            const member = &n.data.member_expr;
            hashAstNodeImpl(hasher, member.object);
            hashAstNodeImpl(hasher, member.property);
            std.hash.autoHash(hasher, member.computed);
        },

        // New expression
        .new_expr => {
            const new = &n.data.new_expr;
            hashAstNodeImpl(hasher, new.callee);
            std.hash.autoHash(hasher, new.arguments.len);
            for (new.arguments) |arg| {
                hashAstNodeImpl(hasher, arg);
            }
        },

        // Array expression
        .array_expr => {
            const arr = &n.data.array_expr;
            std.hash.autoHash(hasher, arr.elements.len);
            for (arr.elements) |elem| {
                hashAstNodeImpl(hasher, elem);
            }
        },

        // Object expression
        .object_expr => {
            const obj = &n.data.object_expr;
            std.hash.autoHash(hasher, obj.properties.len);
            for (obj.properties) |prop| {
                switch (prop) {
                    .property => |p| {
                        std.hash.autoHash(hasher, @as(u8, 0)); // Tag for property
                        hashAstNodeImpl(hasher, p.key);
                        hashAstNodeImpl(hasher, p.value);
                        std.hash.autoHash(hasher, p.shorthand);
                    },
                    .spread => |spread_node| {
                        std.hash.autoHash(hasher, @as(u8, 1)); // Tag for spread
                        hashAstNodeImpl(hasher, spread_node);
                    },
                }
            }
        },

        // Function expression
        .function_expr => {
            const func = &n.data.function_expr;
            if (func.name) |name| hasher.update(name);
            hashFunctionParams(hasher, func.params);
            if (func.return_type) |ret| hashTypeImpl(hasher, ret);
            hashAstNodeImpl(hasher, func.body);
            std.hash.autoHash(hasher, func.is_arrow);
        },

        // Conditional expression
        .conditional_expr => {
            const cond = &n.data.conditional_expr;
            hashAstNodeImpl(hasher, cond.condition);
            hashAstNodeImpl(hasher, cond.consequent);
            hashAstNodeImpl(hasher, cond.alternate);
        },

        // Spread element
        .spread_element => {
            hashAstNodeImpl(hasher, n.data.spread_element.argument);
        },

        // Block statement
        .block_stmt => {
            const block = &n.data.block_stmt;
            std.hash.autoHash(hasher, block.statements.len);
            for (block.statements) |stmt| {
                hashAstNodeImpl(hasher, stmt);
            }
        },

        // Expression statement
        .expression_stmt => {
            hashAstNodeImpl(hasher, n.data.expression_stmt);
        },

        // If statement
        .if_stmt => {
            const if_s = &n.data.if_stmt;
            hashAstNodeImpl(hasher, if_s.condition);
            hashAstNodeImpl(hasher, if_s.consequent);
            if (if_s.alternate) |alt| hashAstNodeImpl(hasher, alt);
        },

        // While statement
        .while_stmt => {
            const while_s = &n.data.while_stmt;
            hashAstNodeImpl(hasher, while_s.condition);
            hashAstNodeImpl(hasher, while_s.body);
        },

        // For statement
        .for_stmt => {
            const for_s = &n.data.for_stmt;
            if (for_s.init) |init| hashAstNodeImpl(hasher, init);
            if (for_s.condition) |cond| hashAstNodeImpl(hasher, cond);
            if (for_s.update) |update| hashAstNodeImpl(hasher, update);
            hashAstNodeImpl(hasher, for_s.body);
        },

        // Return statement
        .return_stmt => {
            if (n.data.return_stmt.argument) |arg| {
                hashAstNodeImpl(hasher, arg);
            }
        },

        // Break/continue (no data)
        .break_stmt, .continue_stmt => {},

        // Variable statement
        .variable_stmt => {
            const var_s = &n.data.variable_stmt;
            std.hash.autoHash(hasher, @intFromEnum(var_s.kind));
            std.hash.autoHash(hasher, var_s.declarations.len);
            for (var_s.declarations) |decl| {
                hasher.update(decl.name);
                if (decl.type) |t| hashTypeImpl(hasher, t);
                if (decl.init) |init| hashAstNodeImpl(hasher, init);
            }
        },

        // Function declaration
        .function_decl => {
            const func = &n.data.function_decl;
            hasher.update(func.name);
            hashFunctionParams(hasher, func.params);
            if (func.return_type) |ret| hashTypeImpl(hasher, ret);
            if (func.body) |body| hashAstNodeImpl(hasher, body);
        },

        // Class declaration
        .class_decl => {
            const class = &n.data.class_decl;
            hasher.update(class.name);
            if (class.extends) |ext| hashTypeImpl(hasher, ext);
            std.hash.autoHash(hasher, class.implements.len);
            for (class.implements) |impl| {
                hashTypeImpl(hasher, impl);
            }
            std.hash.autoHash(hasher, class.members.len);
            for (class.members) |member| {
                hashAstNodeImpl(hasher, member);
            }
            // Hash decorators
            std.hash.autoHash(hasher, class.decorators.len);
            for (class.decorators) |dec| {
                hasher.update(dec.name);
                std.hash.autoHash(hasher, dec.arguments.len);
                for (dec.arguments) |arg| {
                    hashAstNodeImpl(hasher, arg);
                }
            }
        },

        // Interface declaration
        .interface_decl => {
            const iface = &n.data.interface_decl;
            hasher.update(iface.name);
            std.hash.autoHash(hasher, iface.extends.len);
            for (iface.extends) |ext| {
                hashTypeImpl(hasher, ext);
            }
            std.hash.autoHash(hasher, iface.members.len);
            for (iface.members) |member| {
                hashAstNodeImpl(hasher, member);
            }
        },

        // Type alias declaration
        .type_alias_decl => {
            const alias = &n.data.type_alias_decl;
            hasher.update(alias.name);
            hashTypeImpl(hasher, alias.type);
        },

        // Import declaration
        .import_decl => {
            const imp = &n.data.import_decl;
            hasher.update(imp.source);
            std.hash.autoHash(hasher, imp.specifiers.len);
            for (imp.specifiers) |spec| {
                hasher.update(spec.imported);
                hasher.update(spec.local);
            }
        },

        // Export declaration
        .export_decl => {
            const exp = &n.data.export_decl;
            if (exp.declaration) |decl| hashAstNodeImpl(hasher, decl);
            std.hash.autoHash(hasher, exp.specifiers.len);
            for (exp.specifiers) |spec| {
                hasher.update(spec.local);
                hasher.update(spec.exported);
            }
        },

        // Property declaration
        .property_decl => {
            const prop = &n.data.property_decl;
            hasher.update(prop.name);
            if (prop.type) |t| hashTypeImpl(hasher, t);
            if (prop.init) |init| hashAstNodeImpl(hasher, init);
            std.hash.autoHash(hasher, prop.readonly);
        },

        // Method declaration
        .method_decl => {
            const method = &n.data.method_decl;
            hasher.update(method.name);
            hashFunctionParams(hasher, method.params);
            if (method.return_type) |ret| hashTypeImpl(hasher, ret);
            if (method.body) |body| hashAstNodeImpl(hasher, body);
        },

        // Constructor declaration
        .constructor_decl => {
            const ctor = &n.data.constructor_decl;
            hashFunctionParams(hasher, ctor.params);
            hashAstNodeImpl(hasher, ctor.body);
        },

        // Type annotation
        .type_annotation => {
            hashTypeImpl(hasher, n.data.type_annotation);
        },

        // Macro declaration
        .macro_decl => {
            const macro = &n.data.macro_decl;
            hasher.update(macro.name);
            hashFunctionParams(hasher, macro.params);
            if (macro.return_type) |ret| hashTypeImpl(hasher, ret);
            hashAstNodeImpl(hasher, macro.body);
        },

        // Macro invocation
        .macro_invocation => {
            const macro = &n.data.macro_invocation;
            hasher.update(macro.name);
            std.hash.autoHash(hasher, macro.arguments.len);
            for (macro.arguments) |arg| {
                switch (arg) {
                    .identifier => |id| hasher.update(id),
                    .type => |t| hashTypeImpl(hasher, t),
                    .expression => |expr| hashAstNodeImpl(hasher, expr),
                }
            }
            if (macro.target) |target| hashAstNodeImpl(hasher, target);
        },

        // Comptime block
        .comptime_block => {
            hashAstNodeImpl(hasher, n.data.comptime_block.body);
        },

        // Compile error
        .compile_error => {
            hasher.update(n.data.compile_error);
        },

        // Program
        .program => {
            const prog = &n.data.program;
            std.hash.autoHash(hasher, prog.statements.len);
            for (prog.statements) |stmt| {
                hashAstNodeImpl(hasher, stmt);
            }
            // Note: We don't hash file_id - structural equality only
        },
    }

    // Optionally hash the type annotation if present (for typed ASTs)
    if (n.type) |t| {
        hashTypeImpl(hasher, t);
    }
}

/// Hash a Type structurally
fn hashTypeImpl(hasher: *std.hash.Wyhash, t: *const types.Type) void {
    std.hash.autoHash(hasher, @intFromEnum(t.kind));

    switch (t.kind) {
        // Primitives have no data
        .number, .string, .boolean, .void, .unknown, .never,
        .int8, .int16, .int32, .int64,
        .uint8, .uint16, .uint32, .uint64,
        .float32, .float64 => {},

        // Reference types - hash the inner type
        .ref => {
            hashTypeImpl(hasher, t.data.ref);
        },
        .lent => {
            hashTypeImpl(hasher, t.data.lent);
        },

        // Object type
        .object => {
            const obj = t.data.object;
            std.hash.autoHash(hasher, obj.properties.len);
            for (obj.properties) |prop| {
                hasher.update(prop.name);
                hashTypeImpl(hasher, prop.type);
                std.hash.autoHash(hasher, prop.optional);
            }
            std.hash.autoHash(hasher, obj.methods.len);
            for (obj.methods) |method| {
                hasher.update(method.name);
                hashTypeImpl(hasher, method.type);
                std.hash.autoHash(hasher, method.optional);
            }
        },

        // Array type
        .array => {
            hashTypeImpl(hasher, t.data.array);
        },

        // Tuple type
        .tuple => {
            const tuple = t.data.tuple;
            std.hash.autoHash(hasher, tuple.elements.len);
            for (tuple.elements) |elem| {
                hashTypeImpl(hasher, elem);
            }
        },

        // Function type
        .function => {
            const func = t.data.function;
            std.hash.autoHash(hasher, func.params.len);
            for (func.params) |param| {
                hasher.update(param.name);
                hashTypeImpl(hasher, param.type);
                std.hash.autoHash(hasher, param.optional);
            }
            hashTypeImpl(hasher, func.return_type);
        },

        // Generic parameter
        .generic_param => {
            const param = t.data.generic_param;
            hasher.update(param.name);
            if (param.constraint) |c| hashTypeImpl(hasher, c);
            if (param.default) |d| hashTypeImpl(hasher, d);
        },

        // Generic instance
        .generic_instance => {
            const inst = t.data.generic_instance;
            hashTypeImpl(hasher, inst.base);
            std.hash.autoHash(hasher, inst.type_args.len);
            for (inst.type_args) |arg| {
                hashTypeImpl(hasher, arg);
            }
        },

        // Union type
        .@"union" => {
            const u = t.data.@"union";
            std.hash.autoHash(hasher, u.types.len);
            for (u.types) |member| {
                hashTypeImpl(hasher, member);
            }
        },

        // Intersection type
        .intersection => {
            const inter = t.data.intersection;
            std.hash.autoHash(hasher, inter.types.len);
            for (inter.types) |member| {
                hashTypeImpl(hasher, member);
            }
        },

        // Type reference
        .type_reference => {
            const ref = t.data.type_reference;
            hasher.update(ref.name);
            std.hash.autoHash(hasher, ref.type_args.len);
            for (ref.type_args) |arg| {
                hashTypeImpl(hasher, arg);
            }
        },
    }
}

/// Helper to hash function parameters
fn hashFunctionParams(hasher: *std.hash.Wyhash, params: []const node.FunctionExpr.FunctionParam) void {
    std.hash.autoHash(hasher, params.len);
    for (params) |param| {
        hasher.update(param.name);
        if (param.type) |t| hashTypeImpl(hasher, t);
        std.hash.autoHash(hasher, param.optional);
        if (param.default_value) |dv| hashAstNodeImpl(hasher, dv);
    }
}

// ============================================================================
// AST Hashing Tests
// ============================================================================

test "hashAstNode: identical nodes produce same hash" {
    var arena = ASTArena.init(std.testing.allocator);
    defer arena.deinit();

    // Same structure, different locations (file_id, line, column all different)
    const loc1 = SourceLocation{ .file_id = 1, .start = .{ .line = 1, .column = 1 }, .end = .{ .line = 1, .column = 5 } };
    const loc2 = SourceLocation{ .file_id = 2, .start = .{ .line = 10, .column = 20 }, .end = .{ .line = 10, .column = 25 } };

    const node1 = try arena.createNode(.number_literal, loc1, .{ .number_literal = 42.0 });
    const node2 = try arena.createNode(.number_literal, loc2, .{ .number_literal = 42.0 });

    const hash1 = hashAstNode(node1);
    const hash2 = hashAstNode(node2);

    // Should produce same hash (location ignored)
    try std.testing.expectEqual(hash1, hash2);
}

test "hashAstNode: different values produce different hash" {
    var arena = ASTArena.init(std.testing.allocator);
    defer arena.deinit();

    const loc = SourceLocation.dummy();

    const node1 = try arena.createNode(.number_literal, loc, .{ .number_literal = 42.0 });
    const node2 = try arena.createNode(.number_literal, loc, .{ .number_literal = 43.0 });

    const hash1 = hashAstNode(node1);
    const hash2 = hashAstNode(node2);

    // Should produce different hash
    try std.testing.expect(hash1 != hash2);
}

test "hashAstNode: string literals" {
    var arena = ASTArena.init(std.testing.allocator);
    defer arena.deinit();

    const loc = SourceLocation.dummy();

    const node1 = try arena.createNode(.string_literal, loc, .{ .string_literal = "hello" });
    const node2 = try arena.createNode(.string_literal, loc, .{ .string_literal = "hello" });
    const node3 = try arena.createNode(.string_literal, loc, .{ .string_literal = "world" });

    try std.testing.expectEqual(hashAstNode(node1), hashAstNode(node2));
    try std.testing.expect(hashAstNode(node1) != hashAstNode(node3));
}

test "hashAstNode: identifiers" {
    var arena = ASTArena.init(std.testing.allocator);
    defer arena.deinit();

    const loc = SourceLocation.dummy();

    const node1 = try arena.createNode(.identifier, loc, .{ .identifier = "foo" });
    const node2 = try arena.createNode(.identifier, loc, .{ .identifier = "foo" });
    const node3 = try arena.createNode(.identifier, loc, .{ .identifier = "bar" });

    try std.testing.expectEqual(hashAstNode(node1), hashAstNode(node2));
    try std.testing.expect(hashAstNode(node1) != hashAstNode(node3));
}
