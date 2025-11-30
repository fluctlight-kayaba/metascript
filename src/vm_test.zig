/// VM Macro Test
/// Standalone test for the Hermes VM integration
///
/// Build: zig build -Denable-vm=true
/// Run: DYLD_LIBRARY_PATH=vendor/hermes/build/API/hermes ./zig-out/bin/msc-vm-test

const std = @import("std");
const ast = @import("ast/ast.zig");
const Lexer = @import("lexer/lexer.zig").Lexer;
const Parser = @import("parser/parser.zig").Parser;
const vm = @import("vm/vm.zig");

pub fn main() !void {
    // Force stdout flush
    const stdout = std.io.getStdOut().writer();
    try stdout.print("\n[VM Test] Starting...\n", .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    try stdout.print("[VM Test] Allocator initialized\n", .{});

    try stdout.print("\n", .{});
    try stdout.print("╔════════════════════════════════════════════════════════╗\n", .{});
    try stdout.print("║     Metascript Macro VM Test (Hermes Integration)      ║\n", .{});
    try stdout.print("╚════════════════════════════════════════════════════════╝\n\n", .{});

    // Test source code
    const source =
        \\@derive(Eq, Hash)
        \\class User {
        \\    name: string;
        \\    age: number;
        \\}
    ;

    std.debug.print("=== Source Code ===\n{s}\n\n", .{source});

    // Create AST arena
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    // Parse the source
    std.debug.print("=== Parsing ===\n", .{});
    const file_id: ast.FileId = 1;
    var lexer = try Lexer.init(allocator, source, file_id);
    defer lexer.deinit();

    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    const program = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return;
    };

    std.debug.print("Parsed successfully!\n", .{});
    printProgram(program);

    // Initialize Hermes VM
    try stdout.print("\n=== Initializing Hermes VM ===\n", .{});
    try stdout.print("[VM Test] Calling vm.MacroVM.init...\n", .{});

    var macro_vm = vm.MacroVM.init(allocator, &arena) catch |err| {
        try stdout.print("[VM Test] MacroVM.init failed: {}\n", .{err});
        return;
    };
    defer macro_vm.deinit();
    try stdout.print("Hermes VM initialized!\n", .{});

    // Find the class declaration and expand macros
    std.debug.print("\n=== Expanding Macros ===\n", .{});

    for (program.data.program.statements) |stmt| {
        if (stmt.kind == .class_decl) {
            const class = &stmt.data.class_decl;
            std.debug.print("Found class: {s}\n", .{class.name});

            if (class.decorators.len > 0) {
                for (class.decorators) |dec| {
                    std.debug.print("  Decorator: @{s}\n", .{dec.name});

                    // Extract trait arguments
                    var traits = std.ArrayList([]const u8).init(allocator);
                    defer traits.deinit();

                    for (dec.arguments) |arg| {
                        if (arg.kind == .identifier) {
                            try traits.append(arg.data.identifier);
                        }
                    }

                    std.debug.print("  Traits: ", .{});
                    for (traits.items, 0..) |t, i| {
                        if (i > 0) std.debug.print(", ", .{});
                        std.debug.print("{s}", .{t});
                    }
                    std.debug.print("\n", .{});

                    // Execute the macro!
                    std.debug.print("\n  Executing @{s} on Hermes VM...\n", .{dec.name});
                    macro_vm.executeMacro(dec.name, traits.items, stmt) catch |err| {
                        std.debug.print("  Macro error: {}\n", .{err});
                    };
                }
            }
        }
    }

    // Show result
    std.debug.print("\n=== Result After Macro Expansion ===\n", .{});
    printProgram(program);

    // Test bytecode execution
    std.debug.print("\n=== Testing Bytecode Execution ===\n", .{});
    try testBytecodeExecution(allocator, &arena, &macro_vm);

    std.debug.print("\n=== Done ===\n", .{});
}

/// Test bytecode execution vs source execution
fn testBytecodeExecution(allocator: std.mem.Allocator, arena: *ast.ASTArena, macro_vm: *vm.MacroVM) !void {
    _ = arena;

    // JavaScript source for benchmarking
    const js_source =
        \\(function() {
        \\    const result = 0;
        \\    for (let i = 0; i < 1000; i++) {
        \\        result + i;
        \\    }
        \\    return result;
        \\})();
    ;

    // Load pre-compiled bytecode from /tmp if available
    const bytecode_path = "/tmp/test_bytecode.hbc";
    const bytecode = std.fs.cwd().readFileAlloc(allocator, bytecode_path, 10 * 1024 * 1024) catch |err| {
        std.debug.print("  Bytecode file not found ({s}): {}\n", .{ bytecode_path, err });
        std.debug.print("  Create it with: vendor/hermes/build/bin/hermesc -emit-binary -out {s} -O <input.js>\n", .{bytecode_path});
        return;
    };
    defer allocator.free(bytecode);

    std.debug.print("  Loaded bytecode: {d} bytes\n", .{bytecode.len});

    // Benchmark iterations
    const iterations: usize = 100;

    // Test source execution
    std.debug.print("\n  Benchmarking {d} iterations...\n", .{iterations});

    // Create dummy target node
    var dummy_node = ast.Node{
        .kind = .program,
        .data = .{ .program = .{ .statements = &[_]*ast.Node{}, .file_id = 0 } },
        .location = ast.location.SourceLocation.dummy(),
    };

    // Source execution benchmark
    const source_start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        macro_vm.executeSource(js_source, &dummy_node) catch {};
    }
    const source_end = std.time.nanoTimestamp();
    const source_ns: u64 = @intCast(@as(i64, @intCast(source_end)) - @as(i64, @intCast(source_start)));
    const source_per_iter_ms = @as(f64, @floatFromInt(source_ns / iterations)) / 1_000_000.0;

    std.debug.print("  Source execution:   {d:.2}ms avg ({d} total)\n", .{
        source_per_iter_ms,
        @as(f64, @floatFromInt(source_ns)) / 1_000_000.0,
    });

    // Bytecode execution benchmark
    const bytecode_start = std.time.nanoTimestamp();
    for (0..iterations) |_| {
        macro_vm.executeBytecode(bytecode, "test.hbc", &dummy_node) catch {};
    }
    const bytecode_end = std.time.nanoTimestamp();
    const bytecode_ns: u64 = @intCast(@as(i64, @intCast(bytecode_end)) - @as(i64, @intCast(bytecode_start)));
    const bytecode_per_iter_ms = @as(f64, @floatFromInt(bytecode_ns / iterations)) / 1_000_000.0;

    std.debug.print("  Bytecode execution: {d:.2}ms avg ({d} total)\n", .{
        bytecode_per_iter_ms,
        @as(f64, @floatFromInt(bytecode_ns)) / 1_000_000.0,
    });

    // Speedup
    const speedup = source_per_iter_ms / bytecode_per_iter_ms;
    std.debug.print("\n  Speedup: {d:.1}x faster with bytecode!\n", .{speedup});
}

fn printProgram(program: *ast.Node) void {
    for (program.data.program.statements) |stmt| {
        printNode(stmt, 0);
    }
}

fn printNode(node: *ast.Node, indent: usize) void {
    const spaces = "                                        ";
    const ind = spaces[0 .. indent * 2];

    switch (node.kind) {
        .class_decl => {
            const class = &node.data.class_decl;
            std.debug.print("{s}class {s} {{\n", .{ ind, class.name });
            for (class.members) |member| {
                printNode(member, indent + 1);
            }
            std.debug.print("{s}}}\n", .{ind});
        },
        .property_decl => {
            const prop = &node.data.property_decl;
            std.debug.print("{s}{s}: (type)\n", .{ ind, prop.name });
        },
        .method_decl => {
            const method = &node.data.method_decl;
            std.debug.print("{s}{s}(): (method)\n", .{ ind, method.name });
        },
        else => {
            std.debug.print("{s}{s}\n", .{ ind, @tagName(node.kind) });
        },
    }
}
