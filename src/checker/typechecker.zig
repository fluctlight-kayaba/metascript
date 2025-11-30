const std = @import("std");
const ast = @import("../ast/ast.zig");

/// Type checker for Metascript
pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    arena: *ast.ASTArena,

    pub fn init(allocator: std.mem.Allocator, arena: *ast.ASTArena) TypeChecker {
        return .{
            .allocator = allocator,
            .arena = arena,
        };
    }

    /// Type check the AST
    /// Validates that all expressions have correct types
    pub fn check(self: *TypeChecker, program: *ast.Node) !void {
        _ = self;
        std.debug.assert(program.kind == .program);

        // TODO: Implement type checking
        // 1. Build symbol table
        // 2. Infer types for all expressions
        // 3. Check type compatibility
        // 4. Report errors
    }
};

test "type checker initialization" {
    var arena = ast.ASTArena.init(std.testing.allocator);
    defer arena.deinit();

    const checker = TypeChecker.init(std.testing.allocator, &arena);
    _ = checker;
}
