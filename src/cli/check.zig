// CLI Command: check
// Type check only (no code generation)
// Uses Trans-Am engine for incremental analysis

const std = @import("std");
const transam = @import("../transam/transam.zig");
const colors = @import("colors.zig");

pub fn run(allocator: std.mem.Allocator, path: []const u8) !void {
    // Initialize Trans-Am database (same engine as LSP!)
    var db = try transam.TransAmDatabase.init(allocator);
    defer db.deinit();

    // Load file
    const source = std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1024) catch |err| {
        std.debug.print("{s}error:{s} Could not read file '{s}': {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            path,
            @errorName(err),
        });
        return;
    };
    defer allocator.free(source);

    // Register file with Trans-Am
    _ = try db.setFileText(path, source);

    // Collect all diagnostics (parse errors + type errors)
    var all_diagnostics = std.ArrayList(transam.Diagnostic).init(allocator);
    defer {
        for (all_diagnostics.items) |diag| {
            allocator.free(diag.message);
        }
        all_diagnostics.deinit();
    }

    // Phase 1: Get parse diagnostics
    const parse_diagnostics = db.getDiagnostics(path) catch |err| {
        std.debug.print("{s}error:{s} Analysis failed: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return;
    };
    defer allocator.free(parse_diagnostics);

    for (parse_diagnostics) |diag| {
        try all_diagnostics.append(diag);
    }

    // Phase 2: Run full type checking (only if no parse errors)
    if (parse_diagnostics.len == 0) {
        const check_result = db.checkFile(path) catch |err| {
            std.debug.print("{s}error:{s} Type checking failed: {s}\n", .{
                colors.error_color.code(),
                colors.Color.reset.code(),
                @errorName(err),
            });
            return;
        };

        // Convert type errors to diagnostics
        for (check_result.errors) |type_err| {
            const diag = transam.Diagnostic{
                .start_line = if (type_err.location.start.line > 0) type_err.location.start.line - 1 else 0,
                .start_col = type_err.location.start.column,
                .end_line = if (type_err.location.end.line > 0) type_err.location.end.line - 1 else 0,
                .end_col = type_err.location.end.column,
                .severity = .@"error",
                .message = try allocator.dupe(u8, type_err.message),
                .source = "typecheck",
            };
            try all_diagnostics.append(diag);
        }
    }

    const diagnostics = all_diagnostics.items;

    // Display results
    if (diagnostics.len == 0) {
        std.debug.print("{s}check:{s} {s} - {s}no errors{s}\n", .{
            colors.success.code(),
            colors.Color.reset.code(),
            path,
            colors.Color.bright_green.code(),
            colors.Color.reset.code(),
        });
        return;
    }

    // Show errors in rustc/zig style
    std.debug.print("\n", .{});
    for (diagnostics) |diag| {
        printDiagnostic(path, source, diag);
    }

    // Summary
    std.debug.print("\n{s}error:{s} aborting due to {d} error{s}\n", .{
        colors.error_color.code(),
        colors.Color.reset.code(),
        diagnostics.len,
        if (diagnostics.len > 1) "s" else "",
    });
}

fn printDiagnostic(path: []const u8, source: []const u8, diag: transam.Diagnostic) void {
    const severity_str = switch (diag.severity) {
        .@"error" => "error",
        .warning => "warning",
        .information => "info",
        .hint => "hint",
    };
    const severity_color = switch (diag.severity) {
        .@"error" => colors.error_color,
        .warning => colors.warning,
        .information => colors.info,
        .hint => colors.dim_text,
    };

    // Error header: error[E0001]: message
    std.debug.print("{s}{s}:{s} {s}\n", .{
        severity_color.code(),
        severity_str,
        colors.Color.reset.code(),
        diag.message,
    });

    // Location: --> file:line:col
    std.debug.print("  {s}-->{s} {s}:{d}:{d}\n", .{
        colors.Color.bright_blue.code(),
        colors.Color.reset.code(),
        path,
        diag.start_line + 1,
        diag.start_col + 1,
    });

    // Show source line with caret
    if (getLine(source, diag.start_line)) |line| {
        const line_num = diag.start_line + 1;
        const padding = countDigits(line_num);

        // Line number gutter
        std.debug.print("  {s}{d} |{s} {s}\n", .{
            colors.Color.bright_blue.code(),
            line_num,
            colors.Color.reset.code(),
            line,
        });

        // Caret pointing to error
        std.debug.print("  ", .{});
        for (0..padding) |_| std.debug.print(" ", .{});
        std.debug.print(" {s}|{s} ", .{ colors.Color.bright_blue.code(), colors.Color.reset.code() });

        for (0..diag.start_col) |_| std.debug.print(" ", .{});
        std.debug.print("{s}^", .{severity_color.code()});

        // Underline the error span
        const span_len = if (diag.end_col > diag.start_col) diag.end_col - diag.start_col else 1;
        for (1..span_len) |_| std.debug.print("~", .{});
        std.debug.print("{s}\n", .{colors.Color.reset.code()});
    }

    std.debug.print("\n", .{});
}

fn getLine(source: []const u8, line_num: u32) ?[]const u8 {
    var current_line: u32 = 0;
    var line_start: usize = 0;

    for (source, 0..) |c, i| {
        if (c == '\n') {
            if (current_line == line_num) {
                return source[line_start..i];
            }
            current_line += 1;
            line_start = i + 1;
        }
    }

    // Last line (no trailing newline)
    if (current_line == line_num and line_start < source.len) {
        return source[line_start..];
    }

    return null;
}

fn countDigits(n: u32) usize {
    if (n == 0) return 1;
    var count: usize = 0;
    var num = n;
    while (num > 0) : (num /= 10) {
        count += 1;
    }
    return count;
}
