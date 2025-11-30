// CLI Color Utilities
// Provides ANSI color codes and formatting helpers

const std = @import("std");

pub const Color = enum {
    // Basic colors
    reset,
    bold,
    dim,

    // Foreground colors
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,

    // Bright foreground colors
    bright_black,
    bright_red,
    bright_green,
    bright_yellow,
    bright_blue,
    bright_magenta,
    bright_cyan,
    bright_white,

    // Background colors
    bg_black,
    bg_red,
    bg_green,
    bg_yellow,
    bg_blue,
    bg_magenta,
    bg_cyan,
    bg_white,

    pub fn code(self: Color) []const u8 {
        return switch (self) {
            .reset => "\x1b[0m",
            .bold => "\x1b[1m",
            .dim => "\x1b[2m",

            .black => "\x1b[30m",
            .red => "\x1b[31m",
            .green => "\x1b[32m",
            .yellow => "\x1b[33m",
            .blue => "\x1b[34m",
            .magenta => "\x1b[35m",
            .cyan => "\x1b[36m",
            .white => "\x1b[37m",

            .bright_black => "\x1b[90m",
            .bright_red => "\x1b[91m",
            .bright_green => "\x1b[92m",
            .bright_yellow => "\x1b[93m",
            .bright_blue => "\x1b[94m",
            .bright_magenta => "\x1b[95m",
            .bright_cyan => "\x1b[96m",
            .bright_white => "\x1b[97m",

            .bg_black => "\x1b[40m",
            .bg_red => "\x1b[41m",
            .bg_green => "\x1b[42m",
            .bg_yellow => "\x1b[43m",
            .bg_blue => "\x1b[44m",
            .bg_magenta => "\x1b[45m",
            .bg_cyan => "\x1b[46m",
            .bg_white => "\x1b[47m",
        };
    }
};

// Semantic colors for different message types
pub const success = Color.bright_green;
pub const error_color = Color.bright_red;
pub const warning = Color.bright_yellow;
pub const info = Color.bright_cyan;
pub const header = Color.bright_blue;
pub const dim_text = Color.bright_black;
pub const highlight = Color.bright_magenta;

// Helper functions for formatted output
pub fn printSuccess(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("{s}✓{s} ", .{ success.code(), Color.reset.code() });
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
}

pub fn printError(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("{s}✗{s} ", .{ error_color.code(), Color.reset.code() });
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
}

pub fn printWarning(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("{s}⚠{s} ", .{ warning.code(), Color.reset.code() });
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
}

pub fn printInfo(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("{s}ℹ{s} ", .{ info.code(), Color.reset.code() });
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
}

pub fn printHeader(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("\n{s}{s}", .{ header.code(), Color.bold.code() });
    std.debug.print(fmt, args);
    std.debug.print("{s}\n", .{Color.reset.code()});
}

pub fn printDim(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("{s}", .{dim_text.code()});
    std.debug.print(fmt, args);
    std.debug.print("{s}", .{Color.reset.code()});
}

// Box drawing characters
pub const box_top_left = "┌";
pub const box_top_right = "┐";
pub const box_bottom_left = "└";
pub const box_bottom_right = "┘";
pub const box_horizontal = "─";
pub const box_vertical = "│";
pub const box_vertical_right = "├";
pub const box_vertical_left = "┤";
pub const box_down_horizontal = "┬";
pub const box_up_horizontal = "┴";
pub const box_cross = "┼";

// Tree drawing characters (for AST/tree output)
pub const tree_branch = "├── ";
pub const tree_last = "└── ";
pub const tree_vertical = "│   ";
pub const tree_space = "    ";

// Draw a separator line
pub fn printSeparator(width: usize) void {
    var i: usize = 0;
    while (i < width) : (i += 1) {
        std.debug.print("{s}", .{box_horizontal});
    }
    std.debug.print("\n", .{});
}

// Draw a header box
pub fn printBoxHeader(title: []const u8, width: usize) void {
    std.debug.print("\n{s}", .{header.code()});
    std.debug.print("{s}", .{box_top_left});

    var i: usize = 0;
    while (i < width - 2) : (i += 1) {
        std.debug.print("{s}", .{box_horizontal});
    }
    std.debug.print("{s}\n", .{box_top_right});

    std.debug.print("{s} {s}{s}", .{ box_vertical, Color.bold.code(), title });
    const padding = width - title.len - 3;
    i = 0;
    while (i < padding) : (i += 1) {
        std.debug.print(" ", .{});
    }
    std.debug.print("{s}{s}\n", .{ Color.reset.code(), header.code() });

    std.debug.print("{s}", .{box_bottom_left});
    i = 0;
    while (i < width - 2) : (i += 1) {
        std.debug.print("{s}", .{box_horizontal});
    }
    std.debug.print("{s}{s}\n\n", .{ box_bottom_right, Color.reset.code() });
}

// Tree printing utilities (for AST dump)
pub const TreePrefix = struct {
    indent: []const u8,
    is_last: bool,

    pub fn child(self: TreePrefix, allocator: std.mem.Allocator, is_last_child: bool) !TreePrefix {
        const new_indent = if (self.is_last)
            try std.fmt.allocPrint(allocator, "{s}{s}", .{ self.indent, tree_space })
        else
            try std.fmt.allocPrint(allocator, "{s}{s}", .{ self.indent, tree_vertical });

        return TreePrefix{
            .indent = new_indent,
            .is_last = is_last_child,
        };
    }

    pub fn print(self: TreePrefix, text: []const u8, color: Color) void {
        const connector = if (self.is_last) tree_last else tree_branch;
        std.debug.print("{s}{s}{s}{s}{s}\n", .{
            self.indent,
            connector,
            color.code(),
            text,
            Color.reset.code(),
        });
    }

    pub fn printWithType(self: TreePrefix, name: []const u8, type_name: []const u8) void {
        const connector = if (self.is_last) tree_last else tree_branch;
        std.debug.print("{s}{s}{s}{s}{s} {s}:{s} {s}{s}{s}\n", .{
            self.indent,
            connector,
            Color.bright_cyan.code(),
            name,
            Color.reset.code(),
            dim_text.code(),
            Color.reset.code(),
            Color.bright_magenta.code(),
            type_name,
            Color.reset.code(),
        });
    }
};

// Create root tree prefix
pub fn treeRoot() TreePrefix {
    return TreePrefix{
        .indent = "",
        .is_last = false,
    };
}
