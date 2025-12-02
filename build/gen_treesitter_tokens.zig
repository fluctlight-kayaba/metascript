// Generates tree-sitter tokens from Zig lexer source code
// Run via: zig build gen-tokens (or automatic on zig build)
//
// Input:  src/lexer/token.zig (source of truth)
// Output: tree-sitter-metascript/tokens.json
//         tree-sitter-metascript/src/tokens-generated.js
//
// Includes timestamp check - skips if outputs are newer than input

const std = @import("std");

const input_path = "src/lexer/token.zig";
const json_output_path = "tree-sitter-metascript/tokens.json";
const js_output_path = "tree-sitter-metascript/src/tokens-generated.js";

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const cwd = std.fs.cwd();

    // Check if regeneration needed
    const input_stat = cwd.statFile(input_path) catch null;
    const json_stat = cwd.statFile(json_output_path) catch null;
    const js_stat = cwd.statFile(js_output_path) catch null;

    if (input_stat != null and json_stat != null and js_stat != null) {
        const input_mtime = input_stat.?.mtime;
        if (json_stat.?.mtime >= input_mtime and js_stat.?.mtime >= input_mtime) {
            return; // Up to date
        }
    }

    // Read token.zig
    const source = try cwd.readFileAlloc(allocator, input_path, 1024 * 1024);
    defer allocator.free(source);

    // Extract keywords from initKeywordMap()
    var keywords_js = std.ArrayList([]const u8).init(allocator);
    var keywords_ts = std.ArrayList([]const u8).init(allocator);
    var keywords_ms = std.ArrayList([]const u8).init(allocator);
    var keywords_types = std.ArrayList([]const u8).init(allocator);
    defer keywords_js.deinit();
    defer keywords_ts.deinit();
    defer keywords_ms.deinit();
    defer keywords_types.deinit();

    // Parse: try map.put("keyword", .keyword_xxx);
    var lines = std.mem.splitScalar(u8, source, '\n');
    var in_js_section = false;
    var in_ts_section = false;
    var in_ms_section = false;
    var in_types_section = false;

    while (lines.next()) |line| {
        // Track sections via comments
        if (std.mem.indexOf(u8, line, "// JavaScript/TypeScript keywords") != null) {
            in_js_section = true;
            in_ts_section = false;
            in_ms_section = false;
            in_types_section = false;
        } else if (std.mem.indexOf(u8, line, "// TypeScript-specific") != null) {
            in_js_section = false;
            in_ts_section = true;
            in_ms_section = false;
            in_types_section = false;
        } else if (std.mem.indexOf(u8, line, "// Metascript sized types") != null) {
            in_js_section = false;
            in_ts_section = false;
            in_ms_section = false;
            in_types_section = true;
        } else if (std.mem.indexOf(u8, line, "// Metascript type aliases") != null) {
            in_types_section = true;
        } else if (std.mem.indexOf(u8, line, "// Metascript keywords") != null) {
            in_js_section = false;
            in_ts_section = false;
            in_ms_section = true;
            in_types_section = false;
        }

        // Extract keyword from: try map.put("keyword", .keyword_xxx);
        if (std.mem.indexOf(u8, line, "try map.put(\"")) |start| {
            const after_quote = line[start + 13..];  // "try map.put(" = 13 chars
            if (std.mem.indexOf(u8, after_quote, "\"")) |end| {
                const keyword = after_quote[0..end];
                const kw_copy = try allocator.dupe(u8, keyword);

                if (in_types_section) {
                    try keywords_types.append(kw_copy);
                } else if (in_ms_section) {
                    try keywords_ms.append(kw_copy);
                } else if (in_ts_section) {
                    try keywords_ts.append(kw_copy);
                } else if (in_js_section) {
                    try keywords_js.append(kw_copy);
                }
            }
        }
    }

    // Generate tokens.json
    var json_out = std.ArrayList(u8).init(allocator);
    defer json_out.deinit();
    const jw = json_out.writer();

    try jw.writeAll("{\n");
    try jw.writeAll("  \"$comment\": \"AUTO-GENERATED from src/lexer/token.zig - DO NOT EDIT\",\n\n");
    try jw.writeAll("  \"keywords\": {\n");

    try jw.writeAll("    \"javascript\": [");
    for (keywords_js.items, 0..) |kw, i| {
        if (i > 0) try jw.writeAll(", ");
        try jw.print("\"{s}\"", .{kw});
    }
    try jw.writeAll("],\n");

    try jw.writeAll("    \"typescript\": [");
    for (keywords_ts.items, 0..) |kw, i| {
        if (i > 0) try jw.writeAll(", ");
        try jw.print("\"{s}\"", .{kw});
    }
    try jw.writeAll("],\n");

    try jw.writeAll("    \"metascript\": [");
    for (keywords_ms.items, 0..) |kw, i| {
        if (i > 0) try jw.writeAll(", ");
        try jw.print("\"{s}\"", .{kw});
    }
    try jw.writeAll("],\n");

    try jw.writeAll("    \"metascript_types\": [");
    for (keywords_types.items, 0..) |kw, i| {
        if (i > 0) try jw.writeAll(", ");
        try jw.print("\"{s}\"", .{kw});
    }
    try jw.writeAll("]\n");

    try jw.writeAll("  },\n\n");

    // Operators (hardcoded - these rarely change)
    try jw.writeAll(
        \\  "operators": {
        \\    "+": "plus", "-": "minus", "*": "star", "/": "slash", "%": "percent", "**": "star_star",
        \\    "=": "equals", "+=": "plus_equals", "-=": "minus_equals", "*=": "star_equals",
        \\    "==": "equals_equals", "===": "equals_equals_equals", "!=": "bang_equals", "!==": "bang_equals_equals",
        \\    "<": "less_than", "<=": "less_equals", ">": "greater_than", ">=": "greater_equals",
        \\    "&&": "ampersand_ampersand", "||": "pipe_pipe", "!": "bang",
        \\    "&": "ampersand", "|": "pipe", "^": "caret", "~": "tilde",
        \\    "<<": "less_less", ">>": "greater_greater", ">>>": "greater_greater_greater",
        \\    "++": "plus_plus", "--": "minus_minus", "?": "question",
        \\    ".": "dot", "...": "dot_dot_dot", "=>": "arrow"
        \\  },
        \\
        \\  "punctuation": ["(", ")", "{", "}", "[", "]", ";", ":", ",", "@"],
        \\
        \\  "primitive_types": ["string", "number", "boolean", "void", "any", "unknown"],
        \\
        \\  "literals": ["true", "false", "null", "undefined", "this"]
        \\}
        \\
    );

    // Write tokens.json
    const json_file = try cwd.createFile(json_output_path, .{});
    defer json_file.close();
    try json_file.writeAll(json_out.items);

    // Generate tokens-generated.js
    var js_out = std.ArrayList(u8).init(allocator);
    defer js_out.deinit();
    const jsw = js_out.writer();

    try jsw.writeAll("// AUTO-GENERATED from src/lexer/token.zig - DO NOT EDIT\n");
    try jsw.writeAll("// Run `zig build` to regenerate\n\n");
    try jsw.writeAll("module.exports = {\n  keywords: [\n");

    // All keywords
    var first = true;
    for (keywords_js.items) |kw| {
        if (!first) try jsw.writeAll(",\n");
        first = false;
        try jsw.print("    '{s}'", .{kw});
    }
    for (keywords_ts.items) |kw| {
        if (!first) try jsw.writeAll(",\n");
        first = false;
        try jsw.print("    '{s}'", .{kw});
    }
    for (keywords_ms.items) |kw| {
        if (!first) try jsw.writeAll(",\n");
        first = false;
        try jsw.print("    '{s}'", .{kw});
    }
    for (keywords_types.items) |kw| {
        if (!first) try jsw.writeAll(",\n");
        first = false;
        try jsw.print("    '{s}'", .{kw});
    }

    try jsw.writeAll("\n  ],\n\n");
    try jsw.writeAll(
        \\  primitiveTypes: ['string', 'number', 'boolean', 'void', 'any', 'unknown'],
        \\
        \\  literals: ['true', 'false', 'null', 'undefined', 'this']
        \\};
        \\
    );

    // Write tokens-generated.js
    const js_file = try cwd.createFile(js_output_path, .{});
    defer js_file.close();
    try js_file.writeAll(js_out.items);

    std.debug.print("Generated from {s}:\n  → {s}\n  → {s}\n", .{ input_path, json_output_path, js_output_path });
}
