// Build Configuration Loader
// Loads build.ms via Hermes VM and returns JSON config
//
// Strategy:
//   1. Inject std/build/runtime.js into Hermes
//   2. Load and execute build.ms
//   3. Capture the exported config as JSON
//   4. Parse JSON into BuildConfig

const std = @import("std");
const config = @import("config.zig");

// Hermes C API bindings (from macro/vm_expander.zig)
// const hermes_c = @cImport({
//     @cInclude("hermes/hermes.h");
// });

pub const LoadError = error{
    HermesInitFailed,
    RuntimeInjectionFailed,
    BuildMsExecutionFailed,
    ConfigExtractionFailed,
    InvalidConfig,
    OutOfMemory,
};

/// Load build.ms configuration via Hermes VM
pub fn loadBuildMs(allocator: std.mem.Allocator, build_ms_path: []const u8, cli_options: anytype) !config.BuildConfig {
    // Read build.ms file
    const build_ms_source = std.fs.cwd().readFileAlloc(allocator, build_ms_path, 1024 * 1024) catch |err| {
        std.debug.print("  Failed to read {s}: {s}\n", .{ build_ms_path, @errorName(err) });
        return error.InvalidConfig;
    };
    defer allocator.free(build_ms_source);

    // For now, try to parse as JSON-like config
    // TODO: Full Hermes integration
    const cfg = tryParseSimpleConfig(allocator, build_ms_source, cli_options) catch |err| {
        std.debug.print("  {s}Simple parse failed: {s}{s}\n", .{ "\x1b[2m", @errorName(err), "\x1b[0m" });
        // Fall back to executing via Hermes
        return executeViaHermes(allocator, build_ms_source, cli_options);
    };

    std.debug.print("  {s}Parsed build.ms successfully{s}\n", .{ "\x1b[32m", "\x1b[0m" });
    return cfg;
}

/// Try to parse simple JavaScript object literal config
/// Handles: export default { root: "...", build: { ... } }
fn tryParseSimpleConfig(allocator: std.mem.Allocator, source: []const u8, cli_options: anytype) !config.BuildConfig {
    _ = cli_options;

    // Find "export default" and extract the object
    const export_default = "export default";
    const start_idx = std.mem.indexOf(u8, source, export_default) orelse return error.InvalidConfig;
    const obj_start = std.mem.indexOfPos(u8, source, start_idx + export_default.len, "{") orelse return error.InvalidConfig;

    // Find matching closing brace
    var depth: i32 = 0;
    var obj_end: usize = obj_start;
    for (source[obj_start..], obj_start..) |char, i| {
        if (char == '{') depth += 1;
        if (char == '}') depth -= 1;
        if (depth == 0) {
            obj_end = i + 1;
            break;
        }
    }

    if (depth != 0) return error.InvalidConfig;

    const obj_str = source[obj_start..obj_end];

    // Convert JS object literal to JSON
    const json_str = try jsObjectToJson(allocator, obj_str);
    defer allocator.free(json_str);

    // Parse as JSON
    return config.parseJsonConfig(allocator, json_str);
}

/// Convert JavaScript object literal to JSON
/// Handles: unquoted keys, single quotes, trailing commas
fn jsObjectToJson(allocator: std.mem.Allocator, js_obj: []const u8) ![]const u8 {
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    var i: usize = 0;
    var in_string = false;
    var string_char: u8 = 0;
    var last_was_colon = false;

    while (i < js_obj.len) : (i += 1) {
        const c = js_obj[i];

        // Handle strings
        if (!in_string and (c == '"' or c == '\'')) {
            in_string = true;
            string_char = c;
            try result.append('"'); // Always use double quotes
            last_was_colon = false;
            continue;
        }

        if (in_string and c == string_char) {
            in_string = false;
            try result.append('"');
            continue;
        }

        if (in_string) {
            // Escape special characters
            if (c == '"' and string_char == '\'') {
                try result.append('\\');
            }
            try result.append(c);
            continue;
        }

        // Handle unquoted keys
        if (std.ascii.isAlphabetic(c) or c == '_' or c == '$') {
            // Check if this is an unquoted key (followed by :)
            var j = i;
            while (j < js_obj.len and (std.ascii.isAlphanumeric(js_obj[j]) or js_obj[j] == '_' or js_obj[j] == '$')) : (j += 1) {}

            // Skip whitespace
            var k = j;
            while (k < js_obj.len and std.ascii.isWhitespace(js_obj[k])) : (k += 1) {}

            if (k < js_obj.len and js_obj[k] == ':' and !last_was_colon) {
                // This is an unquoted key - quote it
                try result.append('"');
                try result.appendSlice(js_obj[i..j]);
                try result.append('"');
                i = j - 1;
                continue;
            }
        }

        // Skip comments
        if (c == '/' and i + 1 < js_obj.len) {
            if (js_obj[i + 1] == '/') {
                // Line comment - skip to end of line
                while (i < js_obj.len and js_obj[i] != '\n') : (i += 1) {}
                continue;
            }
            if (js_obj[i + 1] == '*') {
                // Block comment - skip to */
                i += 2;
                while (i + 1 < js_obj.len) : (i += 1) {
                    if (js_obj[i] == '*' and js_obj[i + 1] == '/') {
                        i += 1;
                        break;
                    }
                }
                continue;
            }
        }

        // Remove trailing commas before } or ]
        if (c == ',' and i + 1 < js_obj.len) {
            var j = i + 1;
            while (j < js_obj.len and std.ascii.isWhitespace(js_obj[j])) : (j += 1) {}
            if (j < js_obj.len and (js_obj[j] == '}' or js_obj[j] == ']')) {
                continue; // Skip trailing comma
            }
        }

        // Track colons for key detection
        if (c == ':') {
            last_was_colon = true;
        } else if (!std.ascii.isWhitespace(c)) {
            last_was_colon = false;
        }

        try result.append(c);
    }

    return result.toOwnedSlice();
}

/// Execute build.ms via Hermes VM
fn executeViaHermes(allocator: std.mem.Allocator, _: []const u8, _: anytype) LoadError!config.BuildConfig {
    // TODO: Implement full Hermes integration
    // For now, return default config
    std.debug.print("  {s}Hermes execution not yet implemented, using defaults{s}\n", .{
        "\x1b[2m",
        "\x1b[0m",
    });

    // Return a basic default config
    return config.BuildConfig.init(allocator);
}

/// Get the runtime JavaScript to inject before build.ms
pub fn getRuntimeJs() []const u8 {
    return @embedFile("../../std/build/runtime.js");
}

// Tests
test "jsObjectToJson basic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const input = "{ root: 'src/main.ms', build: { target: 'js' } }";
    const json = try jsObjectToJson(allocator, input);
    defer allocator.free(json);

    // Should have quoted keys and double-quoted strings
    try testing.expect(std.mem.indexOf(u8, json, "\"root\"") != null);
    try testing.expect(std.mem.indexOf(u8, json, "\"src/main.ms\"") != null);
}

test "jsObjectToJson trailing comma" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const input = "{ a: 1, b: 2, }";
    const json = try jsObjectToJson(allocator, input);
    defer allocator.free(json);

    // Should not have trailing comma
    try testing.expect(std.mem.indexOf(u8, json, ", }") == null);
}
