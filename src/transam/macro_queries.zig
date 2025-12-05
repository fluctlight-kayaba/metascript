// Macro Queries Module
// Handles macro call site detection, expansion, and caching.
//
// Key features:
// - getMacroCallSites: Find all macro invocations in a file
// - expandMacroCallSite: Expand a single macro with content-addressed caching
// - Firewall pattern: Same input → same output → dependents stay GREEN

const std = @import("std");
const ast = @import("../ast/ast.zig");
const vm = @import("../vm/vm.zig");
const hash_mod = @import("hash.zig");
const types_mod = @import("types.zig");
const cache_mod = @import("cache.zig");

const hashString = hash_mod.hashString;
const MacroCallSite = types_mod.MacroCallSite;
const MacroCallInfo = types_mod.MacroCallInfo;
const MacroOutputCache = cache_mod.MacroOutputCache;

// Forward reference to TransAmDatabase and cancellation
const TransAmDatabase = @import("transam.zig").TransAmDatabase;
const cancellation = @import("cancellation.zig");

/// Result of expanding a macro call site
pub const MacroExpansionResult = struct {
    /// Generated AST nodes (methods, etc.)
    generated_nodes: []*ast.Node,
    /// Output hash for content-addressing
    output_hash: u64,
    /// Whether result was from cache
    from_cache: bool,
};

/// Query 1: Find all macro call sites in a file.
/// This is the entry point for macro processing.
pub fn getMacroCallSites(db: *TransAmDatabase, file_id: []const u8) ![]MacroCallSite {
    const file_id_hash = hashString(file_id);

    // Parse the file to get AST
    const parse_result = try db.parse(file_id);

    var call_sites = std.ArrayList(MacroCallSite).init(db.allocator);
    errdefer call_sites.deinit();

    var call_index: u32 = 0;

    // Walk AST to find macro invocations
    try collectMacroCallSites(db, parse_result.tree, file_id_hash, &call_sites, &call_index);

    return try call_sites.toOwnedSlice();
}

/// Recursively collect macro call sites from AST
fn collectMacroCallSites(
    db: *TransAmDatabase,
    node: *ast.Node,
    file_id_hash: u64,
    call_sites: *std.ArrayList(MacroCallSite),
    call_index: *u32,
) !void {
    // Cancellation checkpoint
    try cancellation.checkCancellation(db);

    switch (node.kind) {
        .program => {
            for (node.data.program.statements) |stmt| {
                try collectMacroCallSites(db, stmt, file_id_hash, call_sites, call_index);
            }
        },
        .class_decl => {
            const class = &node.data.class_decl;
            for (class.decorators) |decorator| {
                var call_site = try createMacroCallSite(
                    db,
                    decorator.name,
                    decorator.arguments,
                    node.location,
                    file_id_hash,
                    call_index,
                );
                call_site.target_node = node;
                try call_sites.append(call_site);
            }
            for (class.members) |member| {
                try collectMacroCallSites(db, member, file_id_hash, call_sites, call_index);
            }
        },
        .macro_invocation => {
            const macro = &node.data.macro_invocation;
            const arguments = try extractMacroArgStrings(db, macro.arguments);
            const call_site = MacroCallSite{
                .call_id = .{
                    .file_id_hash = file_id_hash,
                    .call_index = call_index.*,
                    .input_hash = computeMacroInputHash(macro.name, macro.arguments),
                },
                .macro_name = macro.name,
                .line = node.location.start.line,
                .column = node.location.start.column,
                .arguments = arguments,
            };
            try call_sites.append(call_site);
            call_index.* += 1;
        },
        .function_decl => {
            const func = &node.data.function_decl;
            if (func.body) |body| {
                try collectMacroCallSites(db, body, file_id_hash, call_sites, call_index);
            }
        },
        .block_stmt => {
            for (node.data.block_stmt.statements) |stmt| {
                try collectMacroCallSites(db, stmt, file_id_hash, call_sites, call_index);
            }
        },
        else => {},
    }
}

/// Create a MacroCallSite from a decorator
fn createMacroCallSite(
    db: *TransAmDatabase,
    name: []const u8,
    arg_nodes: []*ast.Node,
    loc: ast.SourceLocation,
    file_id_hash: u64,
    call_index: *u32,
) !MacroCallSite {
    var args = std.ArrayList([]const u8).init(db.allocator);
    defer args.deinit();

    for (arg_nodes) |arg_node| {
        if (arg_node.kind == .identifier) {
            try args.append(arg_node.data.identifier);
        }
    }

    const arguments = try db.allocator.dupe([]const u8, args.items);

    const call_site = MacroCallSite{
        .call_id = .{
            .file_id_hash = file_id_hash,
            .call_index = call_index.*,
            .input_hash = computeMacroInputHashFromStrings(name, arguments),
        },
        .macro_name = name,
        .line = loc.start.line,
        .column = loc.start.column,
        .arguments = arguments,
    };

    call_index.* += 1;
    return call_site;
}

/// Extract string representations of macro arguments
fn extractMacroArgStrings(db: *TransAmDatabase, macro_args: []const ast.node.MacroInvocation.MacroArgument) ![]const []const u8 {
    var args = std.ArrayList([]const u8).init(db.allocator);
    defer args.deinit();

    for (macro_args) |arg| {
        switch (arg) {
            .identifier => |id| try args.append(id),
            .string_literal => |s| try args.append(s),
            .type => try args.append("<type>"),
            .expression => try args.append("<expr>"),
        }
    }

    return try db.allocator.dupe([]const u8, args.items);
}

/// Compute input hash for macro invocation
fn computeMacroInputHash(name: []const u8, args: []const ast.node.MacroInvocation.MacroArgument) u64 {
    var hasher = std.hash.Wyhash.init(0);
    hasher.update(name);
    for (args) |arg| {
        switch (arg) {
            .identifier => |id| hasher.update(id),
            .string_literal => |s| hasher.update(s),
            .type => hasher.update("<type>"),
            .expression => hasher.update("<expr>"),
        }
    }
    return hasher.final();
}

/// Compute input hash from string arguments
fn computeMacroInputHashFromStrings(name: []const u8, args: []const []const u8) u64 {
    var hasher = std.hash.Wyhash.init(0);
    hasher.update(name);
    for (args) |arg| {
        hasher.update(arg);
    }
    return hasher.final();
}

/// Query 2: Get the input hash for a macro call
pub fn getMacroInputHash(call_id: MacroCallInfo) u64 {
    return call_id.input_hash;
}

/// Query 3: Check if macro output is unchanged
pub fn isMacroOutputUnchanged(db: *TransAmDatabase, input_hash: u64, new_output_hash: u64) bool {
    return db.macro_output_cache.outputUnchanged(input_hash, new_output_hash);
}

/// Query 4: Store macro expansion result
pub fn storeMacroExpansion(db: *TransAmDatabase, input_hash: u64, output_hash: u64, output_ast: *ast.Node) !bool {
    return try db.macro_output_cache.store(input_hash, output_hash, output_ast);
}

/// Query 5: Get cached macro expansion output
pub fn getMacroExpansion(db: *TransAmDatabase, input_hash: u64) ?*ast.Node {
    const output_hash = db.macro_output_cache.getOutputHash(input_hash) orelse return null;
    return db.macro_output_cache.getAst(output_hash);
}

/// Get macro cache statistics
pub fn getMacroCacheStats(db: *TransAmDatabase) MacroOutputCache.Stats {
    return db.macro_output_cache.getStats();
}

/// Get macro cache hit rate
pub fn getMacroCacheHitRate(db: *TransAmDatabase) f64 {
    return db.macro_output_cache.hitRate();
}

/// Expand a single macro call site with content-addressed caching
pub fn expandMacroCallSite(db: *TransAmDatabase, file_id: []const u8, call_site: MacroCallSite) !MacroExpansionResult {
    const input_hash = call_site.call_id.input_hash;

    // Get the AST arena for this file
    const file_hash = hashString(file_id);
    const arena = db.ast_arenas.get(file_hash) orelse return error.FileNotParsed;

    // Check content-addressed cache first
    if (db.macro_output_cache.getOutputHash(input_hash)) |cached_output_hash| {
        if (db.macro_output_cache.getAst(cached_output_hash)) |cached_ast| {
            var cached_nodes = try arena.allocator().alloc(*ast.Node, 1);
            cached_nodes[0] = cached_ast;
            return .{
                .generated_nodes = cached_nodes,
                .output_hash = cached_output_hash,
                .from_cache = true,
            };
        }
    }

    // Cache miss: expand the macro
    const generated = try generateMacroExpansion(
        db,
        arena,
        call_site.macro_name,
        call_site.arguments,
        call_site.target_node,
    );

    // Hash output for content-addressing
    const output_hash = hashGeneratedNodes(generated);

    // Store in cache
    if (generated.len > 0) {
        _ = try db.macro_output_cache.store(input_hash, output_hash, generated[0]);
    }

    return .{
        .generated_nodes = generated,
        .output_hash = output_hash,
        .from_cache = false,
    };
}

/// Generate macro expansion
fn generateMacroExpansion(
    db: *TransAmDatabase,
    arena: *ast.ASTArena,
    macro_name: []const u8,
    arguments: []const []const u8,
    target_node: ?*ast.Node,
) ![]*ast.Node {
    var generated = std.ArrayList(*ast.Node).init(arena.allocator());

    // Try real MacroVM execution if target is available
    if (target_node) |target| {
        if (target.kind == .class_decl) {
            const class = &target.data.class_decl;
            const original_member_count = class.members.len;

            var macro_vm = vm.MacroVM.initWithCache(
                db.allocator,
                arena,
                if (db.bytecode_cache) |*cache| cache else null,
            ) catch |err| {
                std.log.warn("[Trans-am] MacroVM init failed: {}, falling back to stub", .{err});
                return try generateStubExpansion(arena, macro_name, arguments);
            };
            defer macro_vm.deinit();

            macro_vm.executeMacro(macro_name, arguments, target) catch |err| {
                std.log.warn("[Trans-am] Macro execution failed: {}, falling back to stub", .{err});
                return try generateStubExpansion(arena, macro_name, arguments);
            };

            const new_member_count = class.members.len;
            if (new_member_count > original_member_count) {
                for (class.members[original_member_count..]) |new_member| {
                    try generated.append(new_member);
                }
            }

            return try generated.toOwnedSlice();
        }
    }

    return try generateStubExpansion(arena, macro_name, arguments);
}

/// Generate stub expansion when MacroVM isn't available
fn generateStubExpansion(arena: *ast.ASTArena, macro_name: []const u8, arguments: []const []const u8) ![]*ast.Node {
    var generated = std.ArrayList(*ast.Node).init(arena.allocator());

    if (std.mem.eql(u8, macro_name, "derive")) {
        for (arguments) |trait| {
            if (std.mem.eql(u8, trait, "Eq")) {
                const method = try arena.createNode(
                    .function_decl,
                    ast.SourceLocation.dummy(),
                    .{
                        .function_decl = .{
                            .name = "equals",
                            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
                            .return_type = null,
                            .body = null,
                            .type_params = &[_]ast.types.GenericParam{},
                        },
                    },
                );
                try generated.append(method);
            } else if (std.mem.eql(u8, trait, "Hash")) {
                const method = try arena.createNode(
                    .function_decl,
                    ast.SourceLocation.dummy(),
                    .{
                        .function_decl = .{
                            .name = "hash",
                            .params = &[_]ast.node.FunctionExpr.FunctionParam{},
                            .return_type = null,
                            .body = null,
                            .type_params = &[_]ast.types.GenericParam{},
                        },
                    },
                );
                try generated.append(method);
            }
        }
    }

    return try generated.toOwnedSlice();
}

/// Hash generated AST nodes for content-addressing
fn hashGeneratedNodes(nodes: []*ast.Node) u64 {
    var hasher = std.hash.Wyhash.init(0);

    for (nodes) |node| {
        std.hash.autoHash(&hasher, node.kind);
        if (node.kind == .function_decl) {
            hasher.update(node.data.function_decl.name);
        }
    }

    return hasher.final();
}

/// Expand all macros in a file
pub fn expandAllMacros(db: *TransAmDatabase, file_id: []const u8) !struct {
    total_macros: usize,
    expanded: usize,
    from_cache: usize,
} {
    const call_sites = try getMacroCallSites(db, file_id);
    defer {
        for (call_sites) |site| {
            db.allocator.free(site.arguments);
        }
        db.allocator.free(call_sites);
    }

    var expanded: usize = 0;
    var from_cache: usize = 0;

    for (call_sites, 0..) |site, i| {
        if (i % 10 == 0) {
            try cancellation.checkCancellation(db);
        }
        const result = expandMacroCallSite(db, file_id, site) catch continue;
        expanded += 1;
        if (result.from_cache) {
            from_cache += 1;
        }
    }

    return .{
        .total_macros = call_sites.len,
        .expanded = expanded,
        .from_cache = from_cache,
    };
}
