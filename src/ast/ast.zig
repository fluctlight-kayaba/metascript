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
