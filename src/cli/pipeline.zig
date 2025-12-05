// CLI Command: pipeline
// Shows compilation pipeline stages with Trans-Am integration
// Useful for debugging and understanding the compilation process

const std = @import("std");
const transam = @import("../transam/transam.zig");
const colors = @import("colors.zig");

const StageStatus = enum {
    pending,
    running,
    success,
    failed,
};

fn printStage(comptime fmt: []const u8, args: anytype, status: StageStatus) void {
    const status_str = switch (status) {
        .success => "✓",
        .failed => "✗",
        .pending => "⏸",
        .running => "•",
    };
    const status_color = switch (status) {
        .success => colors.success,
        .failed => colors.error_color,
        .pending => colors.warning,
        .running => colors.info,
    };

    std.debug.print("{s}{s}{s} ", .{ status_color.code(), status_str, colors.Color.reset.code() });
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
}

pub fn run(allocator: std.mem.Allocator, path: []const u8) !void {
    colors.printBoxHeader("COMPILATION PIPELINE (Trans-Am)", 80);
    std.debug.print("{s}File:{s} {s}\n\n", .{ colors.dim_text.code(), colors.Color.reset.code(), path });

    // Initialize Trans-Am
    var db = try transam.TransAmDatabase.init(allocator);
    defer db.deinit();

    // Stage 1: File Loading
    printStage("[1/6] Loading file...", .{}, .running);
    const source = std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1024) catch |err| {
        printStage("[1/6] Loading file: {s}", .{@errorName(err)}, .failed);
        return;
    };
    defer allocator.free(source);
    printStage("[1/6] Loaded: {d} bytes", .{source.len}, .success);

    // Stage 2: Register with Trans-Am (Input Layer)
    printStage("[2/6] Registering with Trans-Am...", .{}, .running);
    _ = try transam.input_queries.setFileText(&db, path, source);
    const revision = db.getRevision();
    printStage("[2/6] Trans-Am revision: {d}", .{revision.value}, .success);

    // Stage 3: Parsing via Trans-Am
    printStage("[3/6] Parsing (Trans-Am query)...", .{}, .running);
    const parse_result = db.parse(path) catch |err| {
        printStage("[3/6] Parse failed: {s}", .{@errorName(err)}, .failed);
        return;
    };

    // Count AST nodes
    var node_count: usize = 0;
    countNodes(parse_result.tree, &node_count);

    if (parse_result.errors.len > 0) {
        printStage("[3/6] Parsed with {d} errors, {d} nodes", .{ parse_result.errors.len, node_count }, .failed);
        std.debug.print("\n", .{});
        for (parse_result.errors) |err| {
            std.debug.print("      {s}error:{s} {s} at {d}:{d}\n", .{
                colors.error_color.code(),
                colors.Color.reset.code(),
                err.message,
                err.location.start.line,
                err.location.start.column,
            });
        }
        std.debug.print("\n", .{});
    } else {
        printStage("[3/6] Parsed: {d} AST nodes", .{node_count}, .success);
    }

    // Stage 4: Extract macro call sites
    printStage("[4/6] Finding macro invocations...", .{}, .running);
    const macro_sites = transam.macro_queries.getMacroCallSites(&db, path) catch |err| {
        printStage("[4/6] Macro extraction failed: {s}", .{@errorName(err)}, .failed);
        return;
    };
    defer {
        for (macro_sites) |site| {
            allocator.free(site.arguments);
        }
        allocator.free(macro_sites);
    }

    if (macro_sites.len > 0) {
        printStage("[4/6] Found {d} macro invocation(s)", .{macro_sites.len}, .success);
        for (macro_sites) |site| {
            std.debug.print("      {s}@{s}{s}", .{
                colors.Color.bright_magenta.code(),
                site.macro_name,
                colors.Color.reset.code(),
            });
            if (site.arguments.len > 0) {
                std.debug.print("(", .{});
                for (site.arguments, 0..) |arg, i| {
                    if (i > 0) std.debug.print(", ", .{});
                    std.debug.print("{s}", .{arg});
                }
                std.debug.print(")", .{});
            }
            std.debug.print(" at line {d}\n", .{site.line + 1});
        }
    } else {
        printStage("[4/6] No macros found", .{}, .success);
    }

    // Stage 5: Macro Expansion (pending)
    std.debug.print("\n", .{});
    printStage("[5/6] Macro expansion...", .{}, .pending);
    colors.printDim("      Waiting for macro VM integration (Phase 7)\n", .{});

    // Stage 6: Type Checking (pending)
    printStage("[6/6] Type checking...", .{}, .pending);
    colors.printDim("      Waiting for type checker integration\n", .{});

    // Summary
    std.debug.print("\n", .{});
    colors.printSeparator(80);

    const completed: usize = if (parse_result.errors.len > 0) 3 else 4;
    const pending: usize = 6 - completed;

    if (parse_result.errors.len > 0) {
        colors.printError("Completed: {d}/6 stages (with errors)", .{completed});
    } else {
        colors.printSuccess("Completed: {d}/6 stages", .{completed});
    }
    colors.printWarning("Pending:   {d}/6 stages", .{pending});

    std.debug.print("\n", .{});
    colors.printInfo("Trans-Am Status:", .{});
    std.debug.print("  • Revision: {d}\n", .{revision.value});
    std.debug.print("  • AST cached: yes\n", .{});
    std.debug.print("  • Parse errors cached: yes\n", .{});
    std.debug.print("  • Macro sites cached: yes\n", .{});
    std.debug.print("\n", .{});
}

fn countNodes(node: *const @import("../ast/ast.zig").Node, count: *usize) void {
    count.* += 1;

    // Count children based on node kind
    switch (node.kind) {
        .program => {
            for (node.data.program.statements) |stmt| {
                countNodes(stmt, count);
            }
        },
        .class_decl => {
            for (node.data.class_decl.members) |member| {
                countNodes(member, count);
            }
        },
        .function_decl => {
            if (node.data.function_decl.body) |body| {
                countNodes(body, count);
            }
        },
        .block_stmt => {
            for (node.data.block_stmt.statements) |stmt| {
                countNodes(stmt, count);
            }
        },
        else => {}, // Leaf nodes
    }
}
