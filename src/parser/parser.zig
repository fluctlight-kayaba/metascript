const std = @import("std");
const ast = @import("../ast/ast.zig");

/// Parser for TypeScript strict subset
pub const Parser = struct {
    allocator: std.mem.Allocator,
    arena: *ast.ASTArena,
    source: []const u8,
    file_id: ast.FileId,

    pub fn init(allocator: std.mem.Allocator, arena: *ast.ASTArena, source: []const u8, file_id: ast.FileId) Parser {
        return .{
            .allocator = allocator,
            .arena = arena,
            .source = source,
            .file_id = file_id,
        };
    }

    /// Parse source code into AST
    pub fn parse(self: *Parser) !*ast.Node {
        // TODO: Implement full TypeScript parser
        // For now, return an empty program
        const loc = ast.SourceLocation.init(
            self.file_id,
            .{ .line = 0, .column = 0 },
            .{ .line = 0, .column = 0 },
        );

        const statements = try self.allocator.alloc(*ast.Node, 0);

        return try self.arena.createNode(
            .program,
            loc,
            .{
                .program = .{
                    .statements = statements,
                    .file_id = self.file_id,
                },
            },
        );
    }
};

test "parser initialization" {
    var arena = ast.ASTArena.init(std.testing.allocator);
    defer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    var parser = Parser.init(std.testing.allocator, &arena, "const x = 42;", file_id);

    const program = try parser.parse();
    try std.testing.expectEqual(ast.NodeKind.program, program.kind);
}
