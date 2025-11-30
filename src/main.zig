const std = @import("std");

// Import core compiler modules
const ast = @import("ast/ast.zig");
const parser = @import("parser/parser.zig");
const macro = @import("macro/expander.zig");
const checker = @import("checker/typechecker.zig");
const ir = @import("ir/ir.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try printUsage();
        return;
    }

    const command = args[1];

    if (std.mem.eql(u8, command, "compile")) {
        if (args.len < 3) {
            std.debug.print("Error: compile command requires input file\n", .{});
            try printUsage();
            return;
        }
        const input_file = args[2];
        try compile(allocator, input_file);
    } else if (std.mem.eql(u8, command, "run")) {
        if (args.len < 3) {
            std.debug.print("Error: run command requires input file\n", .{});
            try printUsage();
            return;
        }
        const input_file = args[2];
        try compileAndRun(allocator, input_file);
    } else if (std.mem.eql(u8, command, "--version")) {
        std.debug.print("Metascript v0.1.0\n", .{});
    } else {
        std.debug.print("Error: unknown command '{s}'\n", .{command});
        try printUsage();
    }
}

fn printUsage() !void {
    const usage =
        \\Metascript Compiler (msc) v0.1.0
        \\
        \\Usage:
        \\  msc compile [options] <file.ms>    Compile to target
        \\  msc run <file.ms>                  Compile and run
        \\  msc --version                      Show version
        \\
        \\Options:
        \\  --target=<c|js|erlang>             Target backend (default: c)
        \\  --output=<file>                    Output file path
        \\
        \\Examples:
        \\  msc compile hello.ms
        \\  msc compile --target=js app.ms
        \\  msc run fibonacci.ms
        \\
    ;
    std.debug.print("{s}", .{usage});
}

fn compile(allocator: std.mem.Allocator, input_file: []const u8) !void {
    std.debug.print("Compiling: {s}\n", .{input_file});

    // Phase 1: Parse TypeScript source → AST
    std.debug.print("[1/5] Parsing...\n", .{});
    // TODO: Implement parser
    _ = allocator;

    // Phase 2: Macro expansion (AST → AST)
    std.debug.print("[2/5] Expanding macros...\n", .{});
    // TODO: Implement macro expander

    // Phase 3: Type checking
    std.debug.print("[3/5] Type checking...\n", .{});
    // TODO: Implement type checker

    // Phase 4: Lower to unified IR
    std.debug.print("[4/5] Generating IR...\n", .{});
    // TODO: Implement IR lowering

    // Phase 5: Backend code generation
    std.debug.print("[5/5] Generating code...\n", .{});
    // TODO: Implement backends

    std.debug.print("✓ Compilation successful\n", .{});
}

fn compileAndRun(allocator: std.mem.Allocator, input_file: []const u8) !void {
    try compile(allocator, input_file);

    std.debug.print("\n--- Running ---\n", .{});
    // TODO: Execute compiled binary
}

test "basic compilation pipeline" {
    const allocator = std.testing.allocator;

    // Test that compiler initializes
    _ = allocator;
    try std.testing.expect(true);
}
