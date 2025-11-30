// LSP Server Entry Point
// Separate binary for long-running Language Server Protocol

const std = @import("std");
const colors = @import("cli/colors.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Check for --help or --version
    if (args.len > 1) {
        if (std.mem.eql(u8, args[1], "--help") or std.mem.eql(u8, args[1], "-h")) {
            printHelp();
            return;
        } else if (std.mem.eql(u8, args[1], "--version") or std.mem.eql(u8, args[1], "-v")) {
            std.debug.print("mls v0.1.0 (Metascript Language Server)\n", .{});
            return;
        }
    }

    // Start LSP server
    const Server = @import("lsp/server.zig").Server;
    var server = Server.init(allocator);
    defer server.deinit();
    try server.run();
}

fn printHelp() void {
    std.debug.print("\n{s}{s}Metascript Language Server (mls) v0.1.0{s}\n\n", .{
        colors.header.code(),
        colors.Color.bold.code(),
        colors.Color.reset.code(),
    });

    std.debug.print("{s}{s}USAGE:{s}\n", .{
        colors.header.code(),
        colors.Color.bold.code(),
        colors.Color.reset.code(),
    });
    std.debug.print("  {s}mls{s} [OPTIONS]\n\n", .{
        colors.Color.bright_cyan.code(),
        colors.Color.reset.code(),
    });

    std.debug.print("{s}{s}DESCRIPTION:{s}\n", .{
        colors.header.code(),
        colors.Color.bold.code(),
        colors.Color.reset.code(),
    });
    std.debug.print("  Language Server Protocol implementation for Metascript.\n", .{});
    std.debug.print("  Communicates via JSON-RPC over stdin/stdout.\n\n", .{});

    std.debug.print("{s}{s}OPTIONS:{s}\n", .{
        colors.header.code(),
        colors.Color.bold.code(),
        colors.Color.reset.code(),
    });
    std.debug.print("  {s}-h{s}, {s}--help{s}     Display this help\n", .{
        colors.Color.bright_magenta.code(),
        colors.Color.reset.code(),
        colors.Color.bright_magenta.code(),
        colors.Color.reset.code(),
    });
    std.debug.print("  {s}-v{s}, {s}--version{s}  Show version\n\n", .{
        colors.Color.bright_magenta.code(),
        colors.Color.reset.code(),
        colors.Color.bright_magenta.code(),
        colors.Color.reset.code(),
    });

    std.debug.print("{s}{s}EDITOR INTEGRATION:{s}\n", .{
        colors.success.code(),
        colors.Color.bold.code(),
        colors.Color.reset.code(),
    });
    std.debug.print("  VS Code:     Add to settings.json:\n", .{});
    std.debug.print("               \"metascript.lsp.path\": \"/path/to/mls\"\n\n", .{});
    std.debug.print("  Neovim:      Use nvim-lspconfig with custom setup\n\n", .{});

    std.debug.print("{s}Repository:{s} https://github.com/fluctlight-kayaba/metascript\n\n", .{
        colors.dim_text.code(),
        colors.Color.reset.code(),
    });
}
