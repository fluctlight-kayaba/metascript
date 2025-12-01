/// AST API for Macro VM
/// Exposes Zig AST manipulation to JavaScript via Hermes JSI
///
/// This is the bridge that lets macro code (TypeScript) manipulate
/// the compiler's AST (Zig) with zero serialization overhead.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const c_api = @import("c_api.zig");
const c = c_api.c;

/// Context passed to all AST API functions
pub const ASTContext = struct {
    arena: *ast.ASTArena,
    allocator: std.mem.Allocator,
    /// Current node being processed (set during macro expansion)
    current_node: ?*ast.Node = null,
};

/// Register AST APIs with Hermes runtime
pub fn register(runtime: *c.MSHermesRuntime, ctx: *ASTContext) void {
    // Register the 'ast' global object
    c.ms_hermes_register_host_object(
        runtime,
        "ast",
        astGetter,
        null, // read-only
        ctx,
    );

    // Register the 'target' global (current macro target)
    c.ms_hermes_register_host_object(
        runtime,
        "target",
        targetGetter,
        targetSetter,
        ctx,
    );
}

// =============================================================================
// AST HostObject - ast.createMethod(), ast.createBinaryExpr(), etc.
// =============================================================================

fn astGetter(
    runtime: ?*c.MSHermesRuntime,
    context: ?*anyopaque,
    prop_name: [*c]const u8,
) callconv(.c) ?*c.MSHermesValue {
    const ctx = @as(*ASTContext, @ptrCast(@alignCast(context orelse return null)));
    const rt = runtime orelse return null;
    const name = std.mem.span(prop_name);

    // Return appropriate function for each AST operation
    if (std.mem.eql(u8, name, "createMethod")) {
        return c.ms_hermes_create_function(rt, "createMethod", astCreateMethod, ctx);
    } else if (std.mem.eql(u8, name, "createBinaryExpr")) {
        return c.ms_hermes_create_function(rt, "createBinaryExpr", astCreateBinaryExpr, ctx);
    } else if (std.mem.eql(u8, name, "createIdentifier")) {
        return c.ms_hermes_create_function(rt, "createIdentifier", astCreateIdentifier, ctx);
    } else if (std.mem.eql(u8, name, "createMemberExpr")) {
        return c.ms_hermes_create_function(rt, "createMemberExpr", astCreateMemberExpr, ctx);
    } else if (std.mem.eql(u8, name, "createReturnStmt")) {
        return c.ms_hermes_create_function(rt, "createReturnStmt", astCreateReturnStmt, ctx);
    } else if (std.mem.eql(u8, name, "createBlock")) {
        return c.ms_hermes_create_function(rt, "createBlock", astCreateBlock, ctx);
    }

    return null;
}

// =============================================================================
// Target HostObject - target.name, target.properties, target.addMethod()
// =============================================================================

fn targetGetter(
    runtime: ?*c.MSHermesRuntime,
    context: ?*anyopaque,
    prop_name: [*c]const u8,
) callconv(.c) ?*c.MSHermesValue {
    const ctx = @as(*ASTContext, @ptrCast(@alignCast(context orelse return null)));
    const rt = runtime orelse return null;
    const name = std.mem.span(prop_name);
    const node = ctx.current_node orelse return null;

    // Only handle class declarations for now
    if (node.kind != .class_decl) return null;
    const class = &node.data.class_decl;

    if (std.mem.eql(u8, name, "name")) {
        // Return class name as string
        return c.ms_value_string(rt, class.name.ptr, class.name.len);
    } else if (std.mem.eql(u8, name, "properties")) {
        // Return array of property names
        return getClassProperties(rt, class, ctx.allocator);
    } else if (std.mem.eql(u8, name, "addMethod")) {
        // Return function to add method
        return c.ms_hermes_create_function(rt, "addMethod", targetAddMethod, ctx);
    }

    return null;
}

fn targetSetter(
    runtime: ?*c.MSHermesRuntime,
    context: ?*anyopaque,
    prop_name: [*c]const u8,
    value: ?*c.MSHermesValue,
) callconv(.c) ?*c.MSHermesValue {
    _ = runtime;
    _ = context;
    _ = prop_name;
    _ = value;
    // Target is mostly read-only, mutations happen via methods
    return null;
}

/// Get class properties as JavaScript array
fn getClassProperties(
    runtime: *c.MSHermesRuntime,
    class: *const ast.node.ClassDecl,
    allocator: std.mem.Allocator,
) ?*c.MSHermesValue {
    _ = allocator;

    // Count properties
    var prop_count: usize = 0;
    for (class.members) |member| {
        if (member.kind == .property_decl) prop_count += 1;
    }

    // Create array
    const arr = c.ms_array_create(runtime, prop_count) orelse return null;

    var idx: usize = 0;
    for (class.members) |member| {
        if (member.kind == .property_decl) {
            const prop = &member.data.property_decl;
            const name_val = c.ms_value_string(runtime, prop.name.ptr, prop.name.len);
            if (name_val) |val| {
                c.ms_array_set(runtime, arr, idx, val);
                c.ms_value_destroy(val);
            }
            idx += 1;
        }
    }

    return arr;
}

// =============================================================================
// AST Creation Functions (called from JavaScript)
// =============================================================================

/// ast.createMethod(name, params, body) -> MethodNode
fn astCreateMethod(
    runtime: ?*c.MSHermesRuntime,
    context: ?*anyopaque,
    args: [*c]?*c.MSHermesValue,
    arg_count: usize,
) callconv(.c) ?*c.MSHermesValue {
    const ctx = @as(*ASTContext, @ptrCast(@alignCast(context orelse return null)));
    const rt = runtime orelse return null;

    if (arg_count < 2) return null;

    // Get method name - must copy since Hermes string buffer is temporary
    var name_len: usize = 0;
    const name_ptr = c.ms_value_get_string(rt, args[0], &name_len) orelse return null;
    const name = ctx.arena.allocator().dupe(u8, name_ptr[0..name_len]) catch return null;

    // Get body (should be a block node reference)
    // For now, create empty body - will be enhanced
    const loc = ast.SourceLocation.dummy();

    const body = ctx.arena.createNode(
        .block_stmt,
        loc,
        .{ .block_stmt = .{ .statements = &[_]*ast.Node{} } },
    ) catch return null;

    const method = ctx.arena.createNode(
        .method_decl,
        loc,
        .{
            .method_decl = .{
                .name = name,
                .type_params = &[_]ast.GenericParam{},
                .params = &[_]ast.node.FunctionExpr.FunctionParam{},
                .return_type = null,
                .body = body,
            },
        },
    ) catch return null;

    // Return opaque reference (store pointer as number for now)
    // TODO: Use proper HostObject wrapping
    return c.ms_value_number(rt, @floatFromInt(@intFromPtr(method)));
}

/// ast.createBinaryExpr(op, left, right) -> BinaryExprNode
fn astCreateBinaryExpr(
    runtime: ?*c.MSHermesRuntime,
    context: ?*anyopaque,
    args: [*c]?*c.MSHermesValue,
    arg_count: usize,
) callconv(.c) ?*c.MSHermesValue {
    const ctx = @as(*ASTContext, @ptrCast(@alignCast(context orelse return null)));
    const rt = runtime orelse return null;

    if (arg_count < 3) return null;

    // Get operator string
    var op_len: usize = 0;
    const op_ptr = c.ms_value_get_string(rt, args[0], &op_len) orelse return null;
    const op_str = op_ptr[0..op_len];

    // Parse operator
    const op: ast.node.BinaryOp = if (std.mem.eql(u8, op_str, "==="))
        .eq
    else if (std.mem.eql(u8, op_str, "&&"))
        .@"and"
    else if (std.mem.eql(u8, op_str, "||"))
        .@"or"
    else
        .eq; // default

    // Get left and right nodes (passed as pointer numbers)
    const left_ptr = @as(usize, @intFromFloat(c.ms_value_get_number(args[1])));
    const right_ptr = @as(usize, @intFromFloat(c.ms_value_get_number(args[2])));

    const left = @as(*ast.Node, @ptrFromInt(left_ptr));
    const right = @as(*ast.Node, @ptrFromInt(right_ptr));

    const loc = ast.SourceLocation.dummy();
    const expr = ctx.arena.createNode(
        .binary_expr,
        loc,
        .{
            .binary_expr = .{
                .op = op,
                .left = left,
                .right = right,
            },
        },
    ) catch return null;

    return c.ms_value_number(rt, @floatFromInt(@intFromPtr(expr)));
}

/// ast.createIdentifier(name) -> IdentifierNode
fn astCreateIdentifier(
    runtime: ?*c.MSHermesRuntime,
    context: ?*anyopaque,
    args: [*c]?*c.MSHermesValue,
    arg_count: usize,
) callconv(.c) ?*c.MSHermesValue {
    const ctx = @as(*ASTContext, @ptrCast(@alignCast(context orelse return null)));
    const rt = runtime orelse return null;

    if (arg_count < 1) return null;

    // Must copy string since Hermes buffer is temporary
    var name_len: usize = 0;
    const name_ptr = c.ms_value_get_string(rt, args[0], &name_len) orelse return null;
    const name = ctx.arena.allocator().dupe(u8, name_ptr[0..name_len]) catch return null;

    const loc = ast.SourceLocation.dummy();
    const node = ctx.arena.createNode(
        .identifier,
        loc,
        .{ .identifier = name },
    ) catch return null;

    return c.ms_value_number(rt, @floatFromInt(@intFromPtr(node)));
}

/// ast.createMemberExpr(object, property) -> MemberExprNode
fn astCreateMemberExpr(
    runtime: ?*c.MSHermesRuntime,
    context: ?*anyopaque,
    args: [*c]?*c.MSHermesValue,
    arg_count: usize,
) callconv(.c) ?*c.MSHermesValue {
    const ctx = @as(*ASTContext, @ptrCast(@alignCast(context orelse return null)));
    const rt = runtime orelse return null;

    if (arg_count < 2) return null;

    const obj_ptr = @as(usize, @intFromFloat(c.ms_value_get_number(args[0])));
    const prop_ptr = @as(usize, @intFromFloat(c.ms_value_get_number(args[1])));

    const obj = @as(*ast.Node, @ptrFromInt(obj_ptr));
    const prop = @as(*ast.Node, @ptrFromInt(prop_ptr));

    const loc = ast.SourceLocation.dummy();
    const node = ctx.arena.createNode(
        .member_expr,
        loc,
        .{
            .member_expr = .{
                .object = obj,
                .property = prop,
                .computed = false,
            },
        },
    ) catch return null;

    return c.ms_value_number(rt, @floatFromInt(@intFromPtr(node)));
}

/// ast.createReturnStmt(expr) -> ReturnStmtNode
fn astCreateReturnStmt(
    runtime: ?*c.MSHermesRuntime,
    context: ?*anyopaque,
    args: [*c]?*c.MSHermesValue,
    arg_count: usize,
) callconv(.c) ?*c.MSHermesValue {
    const ctx = @as(*ASTContext, @ptrCast(@alignCast(context orelse return null)));
    const rt = runtime orelse return null;

    var expr: ?*ast.Node = null;
    if (arg_count >= 1) {
        const expr_ptr = @as(usize, @intFromFloat(c.ms_value_get_number(args[0])));
        expr = @as(*ast.Node, @ptrFromInt(expr_ptr));
    }

    const loc = ast.SourceLocation.dummy();
    const node = ctx.arena.createNode(
        .return_stmt,
        loc,
        .{ .return_stmt = .{ .argument = expr } },
    ) catch return null;

    return c.ms_value_number(rt, @floatFromInt(@intFromPtr(node)));
}

/// ast.createBlock(statements) -> BlockStmtNode
fn astCreateBlock(
    runtime: ?*c.MSHermesRuntime,
    context: ?*anyopaque,
    args: [*c]?*c.MSHermesValue,
    arg_count: usize,
) callconv(.c) ?*c.MSHermesValue {
    const ctx = @as(*ASTContext, @ptrCast(@alignCast(context orelse return null)));
    const rt = runtime orelse return null;

    var stmts = std.ArrayList(*ast.Node).init(ctx.allocator);
    defer stmts.deinit();

    if (arg_count >= 1 and c.ms_value_is_array(rt, args[0])) {
        const arr = args[0];
        const len = c.ms_array_length(rt, arr);
        for (0..len) |i| {
            const elem = c.ms_array_get(rt, arr, i) orelse continue;
            const ptr = @as(usize, @intFromFloat(c.ms_value_get_number(elem)));
            const node = @as(*ast.Node, @ptrFromInt(ptr));
            stmts.append(node) catch continue;
            c.ms_value_destroy(elem);
        }
    }

    const loc = ast.SourceLocation.dummy();
    const statements = ctx.arena.allocator().dupe(*ast.Node, stmts.items) catch return null;

    const node = ctx.arena.createNode(
        .block_stmt,
        loc,
        .{ .block_stmt = .{ .statements = statements } },
    ) catch return null;

    return c.ms_value_number(rt, @floatFromInt(@intFromPtr(node)));
}

/// target.addMethod(methodNode) - adds method to current class
fn targetAddMethod(
    runtime: ?*c.MSHermesRuntime,
    context: ?*anyopaque,
    args: [*c]?*c.MSHermesValue,
    arg_count: usize,
) callconv(.c) ?*c.MSHermesValue {
    const ctx = @as(*ASTContext, @ptrCast(@alignCast(context orelse return null)));
    const rt = runtime orelse return null;

    if (arg_count < 1) return c.ms_value_bool(rt, false);

    const node = ctx.current_node orelse return c.ms_value_bool(rt, false);
    if (node.kind != .class_decl) return c.ms_value_bool(rt, false);

    // Get method node from pointer
    const method_ptr = @as(usize, @intFromFloat(c.ms_value_get_number(args[0])));
    const method = @as(*ast.Node, @ptrFromInt(method_ptr));

    // Add to class members (use arena allocator to avoid leaks)
    var class = &node.data.class_decl;
    const old_members = class.members;
    const new_members = ctx.arena.allocator().alloc(*ast.Node, old_members.len + 1) catch {
        return c.ms_value_bool(rt, false);
    };

    @memcpy(new_members[0..old_members.len], old_members);
    new_members[old_members.len] = method;
    class.members = new_members;

    return c.ms_value_bool(rt, true);
}
