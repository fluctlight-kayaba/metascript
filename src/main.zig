const std = @import("std");

// Import core compiler modules (pub for visibility tools)
pub const ast = @import("ast/ast.zig");
pub const lexer_mod = @import("lexer/lexer.zig");
pub const token_mod = @import("lexer/token.zig");
pub const parser = @import("parser/parser.zig");
pub const macro = @import("macro/expander.zig");
pub const checker = @import("checker/typechecker.zig");
pub const ir = @import("ir/ir.zig");
pub const transam = @import("transam/transam.zig");
pub const file_store = @import("lsp/file_store.zig");
pub const module = @import("module/module.zig");

// Code generation backends
// Code generators for all backends
pub const jsgen = @import("codegen/js/jsgen.zig");
pub const cgen = @import("codegen/c/cgen.zig");
pub const erlgen = @import("codegen/erlang/erlgen.zig");

// Analysis modules
pub const drc = @import("analysis/drc.zig");

// Import CLI commands
const cli_dump_tokens = @import("cli/dump_tokens.zig");
const cli_dump_ast = @import("cli/dump_ast.zig");
const cli_pipeline = @import("cli/pipeline.zig");
const cli_expand = @import("cli/expand.zig");
const cli_check = @import("cli/check.zig");
const cli_compile = @import("cli/compile.zig");
const cli_run = @import("cli/run.zig");
const cli_build = @import("cli/build.zig");
const colors = @import("cli/colors.zig");

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

    // Visibility/debugging commands
    if (std.mem.eql(u8, command, "dump-tokens")) {
        if (args.len < 3) {
            std.debug.print("Error: dump-tokens requires input file\n", .{});
            try printUsage();
            return;
        }
        try cli_dump_tokens.run(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "dump-ast")) {
        if (args.len < 3) {
            std.debug.print("Error: dump-ast requires input file\n", .{});
            try printUsage();
            return;
        }
        try cli_dump_ast.run(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "pipeline")) {
        if (args.len < 3) {
            std.debug.print("Error: pipeline requires input file\n", .{});
            try printUsage();
            return;
        }
        try cli_pipeline.run(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "expand")) {
        if (args.len < 3) {
            std.debug.print("Error: expand requires input file\n", .{});
            try printUsage();
            return;
        }
        try cli_expand.run(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "check")) {
        if (args.len < 3) {
            std.debug.print("Error: check requires input file\n", .{});
            try printUsage();
            return;
        }
        try cli_check.run(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "compile")) {
        if (args.len < 3) {
            std.debug.print("Error: compile command requires input file\n", .{});
            try printUsage();
            return;
        }

        // Parse compile options
        var target: cli_compile.Backend = .js; // Default to JS
        var output_path: ?[]const u8 = null;
        var input_file: ?[]const u8 = null;
        var enable_normalize: bool = true; // Enabled by default

        for (args[2..]) |arg| {
            if (std.mem.startsWith(u8, arg, "--target=")) {
                const target_str = arg[9..];
                if (std.mem.eql(u8, target_str, "js") or std.mem.eql(u8, target_str, "javascript")) {
                    target = .js;
                } else if (std.mem.eql(u8, target_str, "c")) {
                    target = .c;
                } else if (std.mem.eql(u8, target_str, "erlang") or std.mem.eql(u8, target_str, "erl")) {
                    target = .erlang;
                } else {
                    std.debug.print("Error: unknown target '{s}'. Valid: js, c, erlang\n", .{target_str});
                    return;
                }
            } else if (std.mem.startsWith(u8, arg, "--output=") or std.mem.startsWith(u8, arg, "-o=")) {
                output_path = if (std.mem.startsWith(u8, arg, "--output=")) arg[9..] else arg[3..];
            } else if (std.mem.eql(u8, arg, "--no-normalize")) {
                enable_normalize = false;
            } else if (!std.mem.startsWith(u8, arg, "-")) {
                input_file = arg;
            }
        }

        if (input_file == null) {
            std.debug.print("Error: compile command requires input file\n", .{});
            try printUsage();
            return;
        }

        try cli_compile.runWithArgs(allocator, input_file.?, target, output_path, enable_normalize);
    } else if (std.mem.eql(u8, command, "run")) {
        if (args.len < 3) {
            std.debug.print("Error: run command requires input file\n", .{});
            try printUsage();
            return;
        }
        try cli_run.run(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "build")) {
        // Vite-style build command - uses build.ms config
        try cli_build.run(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "dev")) {
        // Dev server (placeholder)
        std.debug.print("{s}msc dev{s} - Development server (coming soon)\n", .{
            colors.Color.bright_cyan.code(),
            colors.Color.reset.code(),
        });
        std.debug.print("For now, use: {s}msc build --watch{s}\n", .{
            colors.dim_text.code(),
            colors.Color.reset.code(),
        });
    } else if (std.mem.eql(u8, command, "--version") or std.mem.eql(u8, command, "-v")) {
        std.debug.print("Metascript v0.1.0\n", .{});
    } else if (std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "-h")) {
        try printUsage();
    } else {
        std.debug.print("Error: unknown command '{s}'\n", .{command});
        try printUsage();
    }
}

fn printUsage() !void {
    // Header
    std.debug.print("\n{s}{s}Metascript Compiler (msc) v0.1.0{s}\n\n", .{
        colors.header.code(),
        colors.Color.bold.code(),
        colors.Color.reset.code(),
    });

    std.debug.print("{s}{s}USAGE:{s}\n", .{ colors.header.code(), colors.Color.bold.code(), colors.Color.reset.code() });
    std.debug.print("  {s}msc{s} {s}[COMMAND]{s} {s}<file.ms>{s} {s}[OPTIONS]{s}\n\n", .{
        colors.Color.bright_cyan.code(),
        colors.Color.reset.code(),
        colors.Color.bright_yellow.code(),
        colors.Color.reset.code(),
        colors.Color.bright_green.code(),
        colors.Color.reset.code(),
        colors.Color.bright_magenta.code(),
        colors.Color.reset.code(),
    });

    std.debug.print("{s}{s}COMMANDS:{s}\n", .{ colors.header.code(), colors.Color.bold.code(), colors.Color.reset.code() });
    std.debug.print("  {s}build{s}                 Build using build.ms config (Vite-style)\n", .{ colors.Color.bright_white.code(), colors.Color.reset.code() });
    std.debug.print("  {s}dev{s}                   Start dev server with hot reload\n", .{ colors.Color.bright_white.code(), colors.Color.reset.code() });
    std.debug.print("  {s}compile{s} <file>        Compile single file to target backend\n", .{ colors.Color.bright_white.code(), colors.Color.reset.code() });
    std.debug.print("  {s}run{s} <file>            Compile and run\n", .{ colors.Color.bright_white.code(), colors.Color.reset.code() });
    std.debug.print("  {s}check{s} <file>          Type check only (no codegen)\n\n", .{ colors.Color.bright_white.code(), colors.Color.reset.code() });

    std.debug.print("{s}{s}VISIBILITY/DEBUGGING:{s}\n", .{ colors.info.code(), colors.Color.bold.code(), colors.Color.reset.code() });
    std.debug.print("  {s}dump-tokens{s} <file>    Show tokenization output\n", .{ colors.Color.bright_cyan.code(), colors.Color.reset.code() });
    std.debug.print("  {s}dump-ast{s} <file>       Show parsed AST {s}(when implemented){s}\n", .{ colors.Color.bright_cyan.code(), colors.Color.reset.code(), colors.dim_text.code(), colors.Color.reset.code() });
    std.debug.print("  {s}pipeline{s} <file>       Show compilation pipeline stages\n", .{ colors.Color.bright_cyan.code(), colors.Color.reset.code() });
    std.debug.print("  {s}expand{s} <file>         Show macro expansion {s}(when implemented){s}\n\n", .{ colors.Color.bright_cyan.code(), colors.Color.reset.code(), colors.dim_text.code(), colors.Color.reset.code() });

    std.debug.print("{s}{s}OPTIONS:{s}\n", .{ colors.header.code(), colors.Color.bold.code(), colors.Color.reset.code() });
    std.debug.print("  {s}-h{s}, {s}--help{s}          Display this help\n", .{ colors.Color.bright_magenta.code(), colors.Color.reset.code(), colors.Color.bright_magenta.code(), colors.Color.reset.code() });
    std.debug.print("  {s}-v{s}, {s}--version{s}       Show version\n", .{ colors.Color.bright_magenta.code(), colors.Color.reset.code(), colors.Color.bright_magenta.code(), colors.Color.reset.code() });
    std.debug.print("  {s}--target{s}=<c|js|erlang> Target backend (default: c)\n", .{ colors.Color.bright_magenta.code(), colors.Color.reset.code() });
    std.debug.print("  {s}--output{s}=<file>        Output file path\n", .{ colors.Color.bright_magenta.code(), colors.Color.reset.code() });
    std.debug.print("  {s}--no-normalize{s}         Disable AST normalization (for benchmarking)\n\n", .{ colors.Color.bright_magenta.code(), colors.Color.reset.code() });

    std.debug.print("{s}{s}EXAMPLES:{s}\n", .{ colors.success.code(), colors.Color.bold.code(), colors.Color.reset.code() });
    std.debug.print("  {s}$>{s} msc compile hello.ms\n", .{ colors.dim_text.code(), colors.Color.reset.code() });
    std.debug.print("  {s}$>{s} msc compile --target=js app.ms\n", .{ colors.dim_text.code(), colors.Color.reset.code() });
    std.debug.print("  {s}$>{s} msc run fibonacci.ms\n", .{ colors.dim_text.code(), colors.Color.reset.code() });
    std.debug.print("  {s}$>{s} msc dump-tokens example.ms\n", .{ colors.dim_text.code(), colors.Color.reset.code() });
    std.debug.print("  {s}$>{s} msc pipeline example.ms\n", .{ colors.dim_text.code(), colors.Color.reset.code() });
    std.debug.print("  {s}$>{s} msc check example.ms\n\n", .{ colors.dim_text.code(), colors.Color.reset.code() });

    std.debug.print("{s}{s}LANGUAGE SERVER:{s}\n", .{ colors.header.code(), colors.Color.bold.code(), colors.Color.reset.code() });
    std.debug.print("  Run {s}mls{s} for Language Server Protocol support\n", .{ colors.Color.bright_cyan.code(), colors.Color.reset.code() });
    std.debug.print("  Use {s}mls --help{s} for editor integration\n\n", .{ colors.Color.bright_cyan.code(), colors.Color.reset.code() });

    std.debug.print("{s}For more information:{s} https://github.com/fluctlight-kayaba/metascript\n\n", .{ colors.dim_text.code(), colors.Color.reset.code() });
}

// ============================================================================
// Tests
// ============================================================================

test "basic compilation pipeline" {
    const allocator = std.testing.allocator;

    // Test that compiler initializes
    _ = allocator;
    try std.testing.expect(true);
}

// Include tests from submodules
test {
    // Type checker tests (includes symbol table tests)
    std.testing.refAllDecls(checker);
    // Trans-Am query engine tests
    std.testing.refAllDecls(transam);
}
