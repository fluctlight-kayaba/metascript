// CLI Command: run
// Compile and execute

const std = @import("std");
const compile_cmd = @import("compile.zig");

pub fn run(allocator: std.mem.Allocator, input_file: []const u8) !void {
    try compile_cmd.run(allocator, input_file);

    std.debug.print("\n--- Running ---\n", .{});
    // TODO: Execute compiled binary
}
