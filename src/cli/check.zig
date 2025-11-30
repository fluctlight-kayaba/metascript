// CLI Command: check
// Type check only (no code generation)

const std = @import("std");

pub fn run(allocator: std.mem.Allocator, path: []const u8) !void {
    _ = allocator;
    std.debug.print("=== TYPE CHECKING: {s} ===\n\n", .{path});
    std.debug.print("⏸ Type checker not implemented yet\n", .{});
    std.debug.print("\nNext: Implement parser + type checker\n", .{});
}
