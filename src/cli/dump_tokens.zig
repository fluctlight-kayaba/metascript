// CLI Command: dump-tokens
// Shows tokenization output with line numbers and positions

const std = @import("std");
const lexer_mod = @import("../lexer/lexer.zig");
const colors = @import("colors.zig");

pub fn run(allocator: std.mem.Allocator, path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(source);

    var lexer = try lexer_mod.Lexer.init(allocator, source, 1);
    defer lexer.deinit();

    // Header
    colors.printBoxHeader("TOKEN STREAM", 80);
    std.debug.print("{s}File:{s} {s}\n\n", .{ colors.dim_text.code(), colors.Color.reset.code(), path });

    // Table header
    std.debug.print("{s}{s}", .{ colors.header.code(), colors.Color.bold.code() });
    std.debug.print("{s:4} {s} {s:20} {s} {s:30} {s} {s}\n", .{
        "Line",
        colors.box_vertical,
        "Token",
        colors.box_vertical,
        "Text",
        colors.box_vertical,
        "Location",
    });
    std.debug.print("{s}", .{colors.Color.reset.code()});
    colors.printSeparator(80);

    var count: usize = 0;
    while (true) {
        const tok = try lexer.next();
        count += 1;

        const line_str = try std.fmt.allocPrint(allocator, "{d}", .{tok.loc.start.line + 1});
        defer allocator.free(line_str);

        const text = if (tok.text.len > 30)
            try std.fmt.allocPrint(allocator, "{s}...", .{tok.text[0..27]})
        else
            try std.fmt.allocPrint(allocator, "{s}", .{tok.text});
        defer allocator.free(text);

        const loc_str = try std.fmt.allocPrint(
            allocator,
            "{d}:{d}-{d}:{d}",
            .{ tok.loc.start.line + 1, tok.loc.start.column + 1, tok.loc.end.line + 1, tok.loc.end.column + 1 },
        );
        defer allocator.free(loc_str);

        // Color-code token types
        const token_color = getTokenColor(tok.kind);
        std.debug.print("{s}{s:4}{s} {s} {s}{s:20}{s} {s} {s:30} {s} {s}{s}{s}\n", .{
            colors.Color.bright_black.code(),
            line_str,
            colors.Color.reset.code(),
            colors.box_vertical,
            token_color.code(),
            tok.kind.toString(),
            colors.Color.reset.code(),
            colors.box_vertical,
            text,
            colors.box_vertical,
            colors.Color.bright_black.code(),
            loc_str,
            colors.Color.reset.code(),
        });

        if (tok.kind == .end_of_file) break;
        if (count > 1000) {
            colors.printWarning("Stopping at 1000 tokens", .{});
            break;
        }
    }

    // Summary
    std.debug.print("\n", .{});
    colors.printSeparator(80);
    colors.printSuccess("Total: {d} tokens", .{count});

    if (lexer.errors.items.len > 0) {
        std.debug.print("\n", .{});
        for (lexer.errors.items) |err| {
            colors.printError("{}: {s}", .{ err.loc, err.message });
        }
    } else {
        colors.printSuccess("No lexer errors", .{});
    }
    std.debug.print("\n", .{});
}

fn getTokenColor(kind: anytype) colors.Color {
    const kind_str = kind.toString();

    // Keywords - magenta
    if (std.mem.startsWith(u8, kind_str, "keyword_") or
        std.mem.eql(u8, kind_str, "class") or
        std.mem.eql(u8, kind_str, "function")) {
        return colors.Color.bright_magenta;
    }

    // Macros - cyan
    if (std.mem.startsWith(u8, kind_str, "@")) {
        return colors.Color.bright_cyan;
    }

    // Identifiers - white
    if (std.mem.eql(u8, kind_str, "identifier")) {
        return colors.Color.bright_white;
    }

    // Strings/numbers - green
    if (std.mem.eql(u8, kind_str, "string_literal") or
        std.mem.eql(u8, kind_str, "number_literal")) {
        return colors.Color.bright_green;
    }

    // Operators - yellow
    if (std.mem.indexOf(u8, kind_str, "=") != null or
        std.mem.indexOf(u8, kind_str, "+") != null or
        std.mem.indexOf(u8, kind_str, "-") != null) {
        return colors.Color.bright_yellow;
    }

    // Default - dim
    return colors.Color.bright_black;
}
