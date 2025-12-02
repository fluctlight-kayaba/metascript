// Code generation build steps
// Generates derived files from source code (tree-sitter tokens, etc.)

const std = @import("std");

pub fn setup(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) void {
    // =========================================================================
    // Tree-sitter Token Generator
    // =========================================================================
    // Generates tree-sitter-metascript/tokens.json and tokens-generated.js
    // from src/lexer/token.zig (source of truth)

    const gen_tokens = b.addExecutable(.{
        .name = "gen_treesitter_tokens",
        .root_source_file = b.path("build/gen_treesitter_tokens.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_gen_tokens = b.addRunArtifact(gen_tokens);

    // Manual step
    const gen_tokens_step = b.step("gen-tokens", "Generate tree-sitter tokens from token.zig");
    gen_tokens_step.dependOn(&run_gen_tokens.step);

    // Auto-run on default build (cached - only runs if token.zig changed)
    b.getInstallStep().dependOn(&run_gen_tokens.step);

    // =========================================================================
    // Tree-sitter Parser Generator
    // =========================================================================
    // Runs `npx tree-sitter generate` to rebuild parser.c from grammar.js
    // Use: zig build gen-parser

    const gen_parser = b.addSystemCommand(&.{
        "npx", "tree-sitter", "generate",
    });
    gen_parser.setCwd(b.path("tree-sitter-metascript"));

    const gen_parser_step = b.step("gen-parser", "Regenerate tree-sitter parser from grammar.js");
    gen_parser_step.dependOn(&gen_parser.step);
}
