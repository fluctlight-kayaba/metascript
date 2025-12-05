// Trans-Am Hash Functions
// Hash utilities for content-addressed caching

const std = @import("std");
const ast = @import("../ast/ast.zig");
const types = @import("types.zig");

// ===== BASIC HASH FUNCTIONS =====

/// Hash a string for content-addressing
pub fn hashString(s: []const u8) u64 {
    var hasher = std.hash.Wyhash.init(0);
    hasher.update(s);
    return hasher.final();
}

/// Get a unique type ID for a type (compile-time computed)
pub fn typeId(comptime T: type) u64 {
    const type_name = @typeName(T);
    return comptime blk: {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(type_name);
        break :blk hasher.final();
    };
}

/// Hash any value for content-addressing
pub fn hashValue(comptime T: type, value: *const T) u64 {
    var hasher = std.hash.Wyhash.init(0);

    // For simple types, hash the bytes directly
    const bytes = std.mem.asBytes(value);
    hasher.update(bytes);

    return hasher.final();
}

/// Hash a macro call ID
pub fn hashMacroCallId(id: types.MacroCallId) u64 {
    var hasher = std.hash.Wyhash.init(0);
    std.hash.autoHash(&hasher, id);
    return hasher.final();
}

/// Hash a macro call location
pub fn hashMacroCallLoc(loc: *const types.MacroCallLoc) u64 {
    var hasher = std.hash.Wyhash.init(0);
    hasher.update(loc.file_id);
    std.hash.autoHash(&hasher, loc.def_id);
    std.hash.autoHash(&hasher, loc.range.start);
    std.hash.autoHash(&hasher, loc.range.end);
    return hasher.final();
}

// ===== AST HASHING FOR CONTENT-ADDRESSING =====

/// Hash an AST node for content-addressed caching.
/// This produces a stable hash that ignores location info (whitespace changes).
/// Two semantically identical nodes will produce the same hash.
pub fn hashAstNode(node: *const ast.Node) u64 {
    var hasher = std.hash.Wyhash.init(0);
    hashAstNodeInto(&hasher, node);
    return hasher.final();
}

/// Internal: hash node into existing hasher (for recursive hashing)
pub fn hashAstNodeInto(hasher: *std.hash.Wyhash, node: *const ast.Node) void {
    // Hash the node kind
    std.hash.autoHash(hasher, @intFromEnum(node.kind));

    // Hash the data based on kind (ignoring location for content-addressing)
    switch (node.data) {
        .number_literal => |n| {
            // Hash the bits of the float
            std.hash.autoHash(hasher, @as(u64, @bitCast(n)));
        },
        .string_literal => |s| {
            hasher.update(s);
        },
        .boolean_literal => |b| {
            std.hash.autoHash(hasher, b);
        },
        .null_literal => {},
        .identifier => |id| {
            hasher.update(id);
        },
        .binary_expr => |be| {
            std.hash.autoHash(hasher, @intFromEnum(be.op));
            hashAstNodeInto(hasher, be.left);
            hashAstNodeInto(hasher, be.right);
        },
        .unary_expr => |ue| {
            std.hash.autoHash(hasher, @intFromEnum(ue.op));
            hashAstNodeInto(hasher, ue.argument);
        },
        .call_expr => |ce| {
            hashAstNodeInto(hasher, ce.callee);
            std.hash.autoHash(hasher, ce.arguments.len);
            for (ce.arguments) |arg| {
                hashAstNodeInto(hasher, arg);
            }
        },
        .member_expr => |me| {
            hashAstNodeInto(hasher, me.object);
            hashAstNodeInto(hasher, me.property);
            std.hash.autoHash(hasher, me.computed);
        },
        .array_expr => |ae| {
            std.hash.autoHash(hasher, ae.elements.len);
            for (ae.elements) |elem| {
                hashAstNodeInto(hasher, elem);
            }
        },
        .block_stmt => |bs| {
            std.hash.autoHash(hasher, bs.statements.len);
            for (bs.statements) |stmt| {
                hashAstNodeInto(hasher, stmt);
            }
        },
        .variable_stmt => |vs| {
            std.hash.autoHash(hasher, @intFromEnum(vs.kind));
            std.hash.autoHash(hasher, vs.declarations.len);
            for (vs.declarations) |decl| {
                hasher.update(decl.name);
                if (decl.init) |init| {
                    hashAstNodeInto(hasher, init);
                }
            }
        },
        .class_decl => |cd| {
            hasher.update(cd.name);
            std.hash.autoHash(hasher, cd.members.len);
            for (cd.members) |member| {
                hashAstNodeInto(hasher, member);
            }
        },
        .property_decl => |pd| {
            hasher.update(pd.name);
            std.hash.autoHash(hasher, pd.readonly);
            if (pd.init) |init| {
                hashAstNodeInto(hasher, init);
            }
        },
        .method_decl => |md| {
            hasher.update(md.name);
            std.hash.autoHash(hasher, md.params.len);
        },
        .macro_invocation => |mi| {
            hasher.update(mi.name);
            std.hash.autoHash(hasher, mi.arguments.len);
            for (mi.arguments) |arg| {
                switch (arg) {
                    .identifier => |id| hasher.update(id),
                    .string_literal => |s| hasher.update(s),
                    .expression => |expr| hashAstNodeInto(hasher, expr),
                    .type => {}, // Type hashing not implemented yet
                }
            }
            if (mi.target) |target| {
                hashAstNodeInto(hasher, target);
            }
        },
        .program => |prog| {
            std.hash.autoHash(hasher, prog.statements.len);
            for (prog.statements) |stmt| {
                hashAstNodeInto(hasher, stmt);
            }
        },
        // For other node types, just hash their existence
        else => {
            // Node kind already hashed above
        },
    }
}

/// Hash macro input: macro name + arguments + target class info
pub fn hashMacroInput(
    macro_name: []const u8,
    arguments: []const *const ast.Node,
    target_class: ?*const ast.Node,
) u64 {
    var hasher = std.hash.Wyhash.init(0);

    // Hash macro name
    hasher.update(macro_name);

    // Hash arguments
    std.hash.autoHash(&hasher, arguments.len);
    for (arguments) |arg| {
        hashAstNodeInto(&hasher, arg);
    }

    // Hash target class if present
    if (target_class) |tc| {
        hashAstNodeInto(&hasher, tc);
    }

    return hasher.final();
}

// ===== TESTS =====

test "hashString produces consistent hashes" {
    const hash1 = hashString("hello");
    const hash2 = hashString("hello");
    const hash3 = hashString("world");

    try std.testing.expectEqual(hash1, hash2);
    try std.testing.expect(hash1 != hash3);
}

test "hashAstNode produces stable hashes" {
    var node1 = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 42.0 },
    };

    var node2 = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(), // Different location, same content
        .data = .{ .number_literal = 42.0 },
    };

    var node3 = ast.Node{
        .kind = .number_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .number_literal = 43.0 }, // Different value
    };

    const hash1 = hashAstNode(&node1);
    const hash2 = hashAstNode(&node2);
    const hash3 = hashAstNode(&node3);

    // Same content should produce same hash (ignoring location)
    try std.testing.expectEqual(hash1, hash2);
    // Different content should produce different hash
    try std.testing.expect(hash1 != hash3);
}

test "hashAstNode handles strings" {
    var node1 = ast.Node{
        .kind = .string_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .string_literal = "hello" },
    };

    var node2 = ast.Node{
        .kind = .string_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .string_literal = "hello" },
    };

    var node3 = ast.Node{
        .kind = .string_literal,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .string_literal = "world" },
    };

    try std.testing.expectEqual(hashAstNode(&node1), hashAstNode(&node2));
    try std.testing.expect(hashAstNode(&node1) != hashAstNode(&node3));
}

test "hashAstNode handles identifiers" {
    var node1 = ast.Node{
        .kind = .identifier,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .identifier = "myVar" },
    };

    var node2 = ast.Node{
        .kind = .identifier,
        .location = ast.SourceLocation.dummy(),
        .data = .{ .identifier = "myVar" },
    };

    try std.testing.expectEqual(hashAstNode(&node1), hashAstNode(&node2));
}

test "hashMacroCallLoc" {
    const loc1 = types.MacroCallLoc{
        .file_id = "test.ms",
        .def_id = 1,
        .range = .{ .start = 0, .end = 10 },
    };

    const loc2 = types.MacroCallLoc{
        .file_id = "test.ms",
        .def_id = 1,
        .range = .{ .start = 0, .end = 10 },
    };

    const loc3 = types.MacroCallLoc{
        .file_id = "other.ms",
        .def_id = 1,
        .range = .{ .start = 0, .end = 10 },
    };

    try std.testing.expectEqual(hashMacroCallLoc(&loc1), hashMacroCallLoc(&loc2));
    try std.testing.expect(hashMacroCallLoc(&loc1) != hashMacroCallLoc(&loc3));
}
