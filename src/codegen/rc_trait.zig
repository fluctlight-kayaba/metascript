///! Reference Counting Trait (Backend Interface)
///!
///! This module defines the interface that backends implement for RC operations.
///! Each backend translates these abstract operations into target-language syntax.
///!
///! Supported Backends:
///! - C: `ms_incref(x)`, `ms_decref(x)` (ORC runtime)
///! - Zig: `@atomicAdd(&x.rc, 1, .seq_cst)` (custom or ORC)
///! - Rust: `Rc::clone(&x)` (built-in)
///! - Swift: (automatic ARC, but we can emit hints)
///! - Objective-C: `[x retain]` / `[x release]` or ARC
///!
///! NOT applicable:
///! - JavaScript: GC handles everything
///! - Erlang: Immutable data, per-process GC
///! - Java: GC handles everything

const std = @import("std");

/// Abstract RC operation
pub const RcOperation = enum {
    /// Increment reference count (clone, retain, etc.)
    incref,

    /// Decrement reference count (release, drop, etc.)
    decref,

    /// Check if RC is zero (for conditional free)
    is_zero,

    /// Get current RC value (for debugging/introspection)
    get_count,

    /// Initialize RC to 1 (for newly allocated objects)
    init_one,
};

/// Backend-specific RC code generator
pub const RcCodeGen = struct {
    /// Backend name (for error messages)
    name: []const u8,

    /// Generate incref code
    /// Returns code string like "ms_incref(x)" or "Rc::clone(&x)"
    genIncref: *const fn (var_name: []const u8, writer: anytype) anyerror!void,

    /// Generate decref code
    genDecref: *const fn (var_name: []const u8, writer: anytype) anyerror!void,

    /// Generate RC initialization (for new objects)
    genInitRc: *const fn (var_name: []const u8, writer: anytype) anyerror!void,

    /// Generate conditional free (if RC == 0)
    genConditionalFree: ?*const fn (var_name: []const u8, writer: anytype) anyerror!void,

    /// Does this backend need explicit RC ops? (false for ARC/GC)
    needs_explicit_rc: bool,

    /// Does this backend handle cycles automatically? (true for GC)
    handles_cycles: bool,
};

// ============================================================================
// C Backend Implementation
// ============================================================================

pub const c_backend = RcCodeGen{
    .name = "C (ORC)",
    .genIncref = cGenIncref,
    .genDecref = cGenDecref,
    .genInitRc = cGenInitRc,
    .genConditionalFree = cGenConditionalFree,
    .needs_explicit_rc = true,
    .handles_cycles = false, // We need cycle collector
};

fn cGenIncref(var_name: []const u8, writer: anytype) !void {
    try writer.print("ms_incref({s});\n", .{var_name});
}

fn cGenDecref(var_name: []const u8, writer: anytype) !void {
    try writer.print("ms_decref({s});\n", .{var_name});
}

fn cGenInitRc(var_name: []const u8, writer: anytype) !void {
    try writer.print("// RC initialized to 1 by ms_alloc\n", .{});
    _ = var_name;
}

fn cGenConditionalFree(var_name: []const u8, writer: anytype) !void {
    try writer.print("if (ms_getrc({s}) == 0) ms_free({s});\n", .{ var_name, var_name });
}

// ============================================================================
// Zig Backend Implementation (for potential future use)
// ============================================================================

pub const zig_backend = RcCodeGen{
    .name = "Zig",
    .genIncref = zigGenIncref,
    .genDecref = zigGenDecref,
    .genInitRc = zigGenInitRc,
    .genConditionalFree = null, // Zig uses defer
    .needs_explicit_rc = true,
    .handles_cycles = false,
};

fn zigGenIncref(var_name: []const u8, writer: anytype) !void {
    try writer.print("_ = @atomicAdd(&{s}.rc, 1, .seq_cst);\n", .{var_name});
}

fn zigGenDecref(var_name: []const u8, writer: anytype) !void {
    try writer.print("if (@atomicSub(&{s}.rc, 1, .seq_cst) == 1) {s}.deinit();\n", .{ var_name, var_name });
}

fn zigGenInitRc(var_name: []const u8, writer: anytype) !void {
    try writer.print("{s}.rc = 1;\n", .{var_name});
}

// ============================================================================
// Rust Backend Implementation (for potential future use)
// ============================================================================

pub const rust_backend = RcCodeGen{
    .name = "Rust",
    .genIncref = rustGenIncref,
    .genDecref = rustGenDecref,
    .genInitRc = rustGenInitRc,
    .genConditionalFree = null, // Rust Drop handles this
    .needs_explicit_rc = true, // Explicit Rc::clone
    .handles_cycles = false, // Rc doesn't handle cycles, need Weak
};

fn rustGenIncref(var_name: []const u8, writer: anytype) !void {
    try writer.print("let {s}_clone = Rc::clone(&{s});\n", .{ var_name, var_name });
}

fn rustGenDecref(var_name: []const u8, writer: anytype) !void {
    try writer.print("drop({s});\n", .{var_name});
}

fn rustGenInitRc(var_name: []const u8, writer: anytype) !void {
    try writer.print("let {s} = Rc::new({s}_inner);\n", .{ var_name, var_name });
}

// ============================================================================
// Swift Backend Implementation (for potential future use)
// ============================================================================

pub const swift_backend = RcCodeGen{
    .name = "Swift (ARC)",
    .genIncref = swiftGenIncref,
    .genDecref = swiftGenDecref,
    .genInitRc = swiftGenInitRc,
    .genConditionalFree = null,
    .needs_explicit_rc = false, // ARC is automatic
    .handles_cycles = false, // Need weak refs for cycles
};

fn swiftGenIncref(var_name: []const u8, writer: anytype) !void {
    // Swift ARC handles this automatically
    try writer.print("// ARC: {s} retained automatically\n", .{var_name});
}

fn swiftGenDecref(var_name: []const u8, writer: anytype) !void {
    // Swift ARC handles this automatically
    try writer.print("// ARC: {s} released automatically\n", .{var_name});
}

fn swiftGenInitRc(var_name: []const u8, writer: anytype) !void {
    // Swift ARC handles this automatically
    try writer.print("// ARC: {s} RC=1 on allocation\n", .{var_name});
}

// ============================================================================
// Objective-C Backend Implementation (for potential future use)
// ============================================================================

pub const objc_backend = RcCodeGen{
    .name = "Objective-C",
    .genIncref = objcGenIncref,
    .genDecref = objcGenDecref,
    .genInitRc = objcGenInitRc,
    .genConditionalFree = null,
    .needs_explicit_rc = true, // Manual retain/release (non-ARC mode)
    .handles_cycles = false,
};

fn objcGenIncref(var_name: []const u8, writer: anytype) !void {
    try writer.print("[{s} retain];\n", .{var_name});
}

fn objcGenDecref(var_name: []const u8, writer: anytype) !void {
    try writer.print("[{s} release];\n", .{var_name});
}

fn objcGenInitRc(var_name: []const u8, writer: anytype) !void {
    try writer.print("// [[{s} alloc] init] sets RC=1\n", .{var_name});
}

// ============================================================================
// JavaScript Backend (No RC needed)
// ============================================================================

pub const js_backend = RcCodeGen{
    .name = "JavaScript (GC)",
    .genIncref = jsGenNoop,
    .genDecref = jsGenNoop,
    .genInitRc = jsGenNoop,
    .genConditionalFree = null,
    .needs_explicit_rc = false,
    .handles_cycles = true, // GC handles cycles
};

fn jsGenNoop(var_name: []const u8, writer: anytype) !void {
    // JavaScript GC handles everything
    _ = var_name;
    _ = writer;
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Get the appropriate backend for a target
pub fn getBackend(target: Target) RcCodeGen {
    return switch (target) {
        .c => c_backend,
        .zig => zig_backend,
        .rust => rust_backend,
        .swift => swift_backend,
        .objc => objc_backend,
        .javascript => js_backend,
        .erlang => js_backend, // Erlang also uses GC (per-process)
    };
}

pub const Target = enum {
    c,
    zig,
    rust,
    swift,
    objc,
    javascript,
    erlang,
};

// ============================================================================
// Tests
// ============================================================================

test "C backend generates correct incref" {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    try c_backend.genIncref("obj", buf.writer());
    try std.testing.expectEqualStrings("ms_incref(obj);\n", buf.items);
}

test "C backend generates correct decref" {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    try c_backend.genDecref("obj", buf.writer());
    try std.testing.expectEqualStrings("ms_decref(obj);\n", buf.items);
}

test "Rust backend generates clone" {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    try rust_backend.genIncref("obj", buf.writer());
    try std.testing.expectEqualStrings("let obj_clone = Rc::clone(&obj);\n", buf.items);
}

test "JS backend is noop" {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    try js_backend.genIncref("obj", buf.writer());
    try std.testing.expectEqual(@as(usize, 0), buf.items.len);
}

test "getBackend returns correct backend" {
    const c = getBackend(.c);
    try std.testing.expectEqualStrings("C (ORC)", c.name);
    try std.testing.expect(c.needs_explicit_rc);

    const js = getBackend(.javascript);
    try std.testing.expect(!js.needs_explicit_rc);
    try std.testing.expect(js.handles_cycles);
}
