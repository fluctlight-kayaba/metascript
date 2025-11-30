const std = @import("std");
const ast = @import("../ast/ast.zig");

/// Unified Intermediate Representation
/// Platform-agnostic representation that all three backends consume
pub const IR = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) IR {
        return .{ .allocator = allocator };
    }

    /// Lower AST to IR
    /// This happens AFTER macro expansion and type checking
    pub fn lower(self: *IR, program: *ast.Node) !void {
        _ = self;
        std.debug.assert(program.kind == .program);

        // TODO: Implement IR lowering
        // 1. Convert AST to platform-agnostic IR
        // 2. Perform optimizations
        // 3. Prepare for backend code generation
    }
};

test "IR initialization" {
    const ir = IR.init(std.testing.allocator);
    _ = ir;
}
