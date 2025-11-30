// CLI Command: pipeline
// Shows compilation pipeline stages with colored output

const std = @import("std");
const lexer_mod = @import("../lexer/lexer.zig");
const file_store = @import("../lsp/file_store.zig");
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
    colors.printBoxHeader("COMPILATION PIPELINE", 80);
    std.debug.print("{s}File:{s} {s}\n\n", .{ colors.dim_text.code(), colors.Color.reset.code(), path });

    // Stage 1: File Loading
    printStage("[1/6] Loading file...", .{}, .running);
    var store = file_store.FileStore.init(allocator);
    defer store.deinit();

    const file_id = store.load(path) catch |err| {
        printStage("[1/6] Loading file: {s}", .{@errorName(err)}, .failed);
        return;
    };
    const source = store.get(file_id).?;
    printStage("[1/6] Loaded: {d} bytes", .{source.len}, .success);

    // Stage 2: Lexical Analysis
    printStage("[2/6] Tokenizing...", .{}, .running);
    var lexer = try lexer_mod.Lexer.init(allocator, source, file_id);
    defer lexer.deinit();

    var token_count: usize = 0;
    while (true) {
        const tok = try lexer.next();
        token_count += 1;
        if (tok.kind == .end_of_file) break;
    }

    if (lexer.errors.items.len > 0) {
        printStage("[2/6] Tokenizing: {d} tokens, {d} errors", .{ token_count, lexer.errors.items.len }, .failed);
        for (lexer.errors.items) |err| {
            colors.printError("  {}: {s}", .{ err.loc, err.message });
        }
        return;
    } else {
        printStage("[2/6] Tokenized: {d} tokens", .{token_count}, .success);
    }

    // Stage 3: Parsing (TODO)
    std.debug.print("\n", .{});
    printStage("[3/6] Parsing...", .{}, .pending);
    colors.printDim("      Parser not implemented yet\n", .{});

    // Stage 4: Macro Expansion (TODO)
    printStage("[4/6] Expanding macros...", .{}, .pending);
    colors.printDim("      Waiting for parser\n", .{});

    // Stage 5: Type Checking (TODO)
    printStage("[5/6] Type checking...", .{}, .pending);
    colors.printDim("      Waiting for parser\n", .{});

    // Stage 6: Code Generation (TODO)
    printStage("[6/6] Generating code...", .{}, .pending);
    colors.printDim("      Waiting for IR\n", .{});

    // Summary
    std.debug.print("\n", .{});
    colors.printSeparator(80);
    colors.printSuccess("Completed: 2/6 stages", .{});
    colors.printWarning("Pending:   4/6 stages", .{});
    std.debug.print("\n", .{});
    colors.printInfo("Next: Implement parser (Stage 3)", .{});
    std.debug.print("\n", .{});
}
