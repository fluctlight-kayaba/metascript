const std = @import("std");
const ast = @import("../ast/ast.zig");
const expander = @import("expander.zig");

/// Expand @derive(Trait) on a class node directly
/// This is used by the VMMacroExpander when Hermes VM is not available
pub fn expandDerive(arena: *ast.ASTArena, allocator: std.mem.Allocator, class_node: *ast.Node, trait: []const u8) !void {
    if (class_node.kind != .class_decl) {
        return error.InvalidTarget;
    }

    const class = &class_node.data.class_decl;
    const loc = class_node.location;

    if (std.mem.eql(u8, trait, "Eq")) {
        // Generate equals() method
        const method = try generateEqualsMethodDirect(arena, allocator, class, loc);
        try addMethodToClass(allocator, class, method);
    } else if (std.mem.eql(u8, trait, "Hash")) {
        // Generate hash() method
        const method = try generateHashMethodDirect(arena, allocator, class, loc);
        try addMethodToClass(allocator, class, method);
    } else {
        std.log.warn("[MACRO] Unknown trait for @derive: {s}", .{trait});
        // Don't fail, just skip unknown traits
    }
}

/// Add a method to a class's members list
fn addMethodToClass(allocator: std.mem.Allocator, class: *ast.node.ClassDecl, method: *ast.Node) !void {
    const old_members = class.members;
    const new_members = try allocator.alloc(*ast.Node, old_members.len + 1);
    @memcpy(new_members[0..old_members.len], old_members);
    new_members[old_members.len] = method;
    class.members = new_members;
}

/// Register all built-in macros
pub fn registerAll(ctx: *expander.MacroContext) !void {
    try ctx.registerMacro("derive", deriveMacro);
    try ctx.registerMacro("comptime", comptimeMacro);
    try ctx.registerMacro("compileError", compileErrorMacro);
    try ctx.registerMacro("serialize", serializeMacro);
}

/// @derive(Eq, Hash, Clone, Debug, Serialize)
/// Auto-generates boilerplate methods for classes
fn deriveMacro(ctx: *expander.MacroContext, invocation: *ast.Node) !?*ast.Node {
    const macro = &invocation.data.macro_invocation;

    // Get the target (class declaration)
    const target = macro.target orelse {
        try ctx.reportError(invocation.location, "@derive requires a target class");
        return null;
    };

    if (target.kind != .class_decl) {
        try ctx.reportError(invocation.location, "@derive can only be applied to classes");
        return null;
    }

    var class = &target.data.class_decl;

    // Parse trait arguments
    var traits = std.ArrayList([]const u8).init(ctx.allocator);
    defer traits.deinit();

    for (macro.arguments) |arg| {
        switch (arg) {
            .identifier => |name| try traits.append(name),
            else => {
                try ctx.reportError(invocation.location, "@derive arguments must be trait identifiers");
                return null;
            },
        }
    }

    // Generate methods for each trait
    var generated_members = std.ArrayList(*ast.Node).init(ctx.allocator);
    defer generated_members.deinit();

    for (traits.items) |trait_name| {
        if (std.mem.eql(u8, trait_name, "Eq")) {
            // Generate equals() method
            const equals_method = try generateEqualsMethod(ctx, class, invocation.location);
            try generated_members.append(equals_method);
        } else if (std.mem.eql(u8, trait_name, "Hash")) {
            // Generate hash() method
            const hash_method = try generateHashMethod(ctx, class, invocation.location);
            try generated_members.append(hash_method);
        } else if (std.mem.eql(u8, trait_name, "Clone")) {
            // Generate clone() method
            const clone_method = try generateCloneMethod(ctx, class, invocation.location);
            try generated_members.append(clone_method);
        } else if (std.mem.eql(u8, trait_name, "Debug")) {
            // Generate toString() method
            const debug_method = try generateDebugMethod(ctx, class, invocation.location);
            try generated_members.append(debug_method);
        } else if (std.mem.eql(u8, trait_name, "Serialize")) {
            // Generate toJSON() / fromJSON() methods
            const to_json = try generateToJSONMethod(ctx, class, invocation.location);
            const from_json = try generateFromJSONMethod(ctx, class, invocation.location);
            try generated_members.append(to_json);
            try generated_members.append(from_json);
        } else {
            const err_msg = try std.fmt.allocPrint(
                ctx.allocator,
                "Unknown trait: {s}",
                .{trait_name},
            );
            try ctx.reportError(invocation.location, err_msg);
            return null;
        }
    }

    // Add generated members to class
    const old_members = class.members;
    const new_members = try ctx.allocator.alloc(*ast.Node, old_members.len + generated_members.items.len);
    @memcpy(new_members[0..old_members.len], old_members);
    @memcpy(new_members[old_members.len..], generated_members.items);
    class.members = new_members;

    // Return the modified class (macro has been expanded)
    return target;
}

/// Generate equals(other: ClassName): boolean method
/// Compares all fields for structural equality
fn generateEqualsMethod(ctx: *expander.MacroContext, class: *ast.node.ClassDecl, loc: ast.SourceLocation) !*ast.Node {
    // equals(other: User): boolean {
    //     return this.name === other.name && this.age === other.age;
    // }

    const arena = ctx.arena;

    // Build comparison expression: this.field === other.field for each property
    var comparisons = std.ArrayList(*ast.Node).init(ctx.allocator);
    defer comparisons.deinit();

    for (class.members) |member| {
        if (member.kind == .property_decl) {
            const prop = &member.data.property_decl;

            // this.field
            const this_member = try createMemberAccess(arena, loc, "this", prop.name);

            // other.field
            const other_member = try createMemberAccess(arena, loc, "other", prop.name);

            // this.field === other.field
            const comparison = try expander.createBinaryExpr(
                arena,
                loc,
                .eq,
                this_member,
                other_member,
            );

            try comparisons.append(comparison);
        }
    }

    // Chain comparisons with &&
    var result_expr = comparisons.items[0];
    for (comparisons.items[1..]) |comp| {
        result_expr = try expander.createBinaryExpr(
            arena,
            loc,
            .@"and",
            result_expr,
            comp,
        );
    }

    // return <result_expr>;
    const return_stmt = try arena.createNode(
        .return_stmt,
        loc,
        .{ .return_stmt = .{ .argument = result_expr } },
    );

    // { return ...; }
    const body_stmts = try ctx.allocator.alloc(*ast.Node, 1);
    body_stmts[0] = return_stmt;

    const body = try arena.createNode(
        .block_stmt,
        loc,
        .{ .block_stmt = .{ .statements = body_stmts } },
    );

    // Parameter: other: ClassName
    const type_ref_old = try ctx.allocator.create(ast.types.TypeReference);
    type_ref_old.* = .{
        .name = class.name,
        .type_args = &[_]*ast.Type{},
    };
    const param_type = try arena.createType(
        .type_reference,
        loc,
        .{ .type_reference = type_ref_old },
    );

    const params = try ctx.allocator.alloc(ast.node.FunctionExpr.FunctionParam, 1);
    params[0] = .{
        .name = "other",
        .type = param_type,
        .optional = false,
        .default_value = null,
    };

    // Return type: boolean
    const return_type = try arena.createType(
        .boolean,
        loc,
        .{ .boolean = {} },
    );

    // Create method declaration
    return try expander.createMethodDecl(
        arena,
        loc,
        "equals",
        params,
        return_type,
        body,
    );
}

/// Generate hash(): number method
fn generateHashMethod(ctx: *expander.MacroContext, class: *ast.node.ClassDecl, loc: ast.SourceLocation) !*ast.Node {
    // hash(): number {
    //     return hashString(this.name) ^ this.age;
    // }

    const arena = ctx.arena;

    // Build hash expression: hash each field and XOR them together
    var hash_expr: ?*ast.Node = null;

    for (class.members) |member| {
        if (member.kind == .property_decl) {
            const prop = &member.data.property_decl;

            // this.field
            const field_access = try createMemberAccess(arena, loc, "this", prop.name);

            // hashString(this.field) or just this.field for numbers
            const field_hash = if (prop.type) |t| blk: {
                if (t.kind == .string) {
                    // hashString(this.field)
                    const hash_fn = try expander.createIdentifier(arena, loc, "hashString");
                    const args = try ctx.allocator.alloc(*ast.Node, 1);
                    args[0] = field_access;

                    break :blk try arena.createNode(
                        .call_expr,
                        loc,
                        .{
                            .call_expr = .{
                                .callee = hash_fn,
                                .arguments = args,
                                .type_args = &[_]*ast.Type{},
                            },
                        },
                    );
                } else {
                    break :blk field_access;
                }
            } else field_access;

            // XOR with previous hash
            if (hash_expr) |prev| {
                hash_expr = try expander.createBinaryExpr(arena, loc, .bit_xor, prev, field_hash);
            } else {
                hash_expr = field_hash;
            }
        }
    }

    // return <hash_expr>;
    const return_stmt = try arena.createNode(
        .return_stmt,
        loc,
        .{ .return_stmt = .{ .argument = hash_expr } },
    );

    const body_stmts = try ctx.allocator.alloc(*ast.Node, 1);
    body_stmts[0] = return_stmt;

    const body = try arena.createNode(
        .block_stmt,
        loc,
        .{ .block_stmt = .{ .statements = body_stmts } },
    );

    const return_type = try arena.createType(.number, loc, .{ .number = {} });

    return try expander.createMethodDecl(
        arena,
        loc,
        "hash",
        &[_]ast.node.FunctionExpr.FunctionParam{},
        return_type,
        body,
    );
}

/// Generate clone(): ClassName method
fn generateCloneMethod(ctx: *expander.MacroContext, class: *ast.node.ClassDecl, loc: ast.SourceLocation) !*ast.Node {
    // clone(): User {
    //     return new User(this.name, this.age);
    // }

    _ = ctx;
    _ = class;
    _ = loc;

    // TODO: Implement clone() generation
    // For now, return a placeholder
    return error.NotImplemented;
}

/// Generate toString(): string method
fn generateDebugMethod(ctx: *expander.MacroContext, class: *ast.node.ClassDecl, loc: ast.SourceLocation) !*ast.Node {
    _ = ctx;
    _ = class;
    _ = loc;
    return error.NotImplemented;
}

/// Generate toJSON() method
fn generateToJSONMethod(ctx: *expander.MacroContext, class: *ast.node.ClassDecl, loc: ast.SourceLocation) !*ast.Node {
    _ = ctx;
    _ = class;
    _ = loc;
    return error.NotImplemented;
}

/// Generate fromJSON() method
fn generateFromJSONMethod(ctx: *expander.MacroContext, class: *ast.node.ClassDecl, loc: ast.SourceLocation) !*ast.Node {
    _ = ctx;
    _ = class;
    _ = loc;
    return error.NotImplemented;
}

/// @comptime { ... }
/// Execute code at compile-time and embed result
fn comptimeMacro(ctx: *expander.MacroContext, invocation: *ast.Node) !?*ast.Node {
    _ = ctx;
    _ = invocation;

    // TODO: Implement compile-time evaluation
    // This requires an interpreter that can execute code at compile-time
    return error.NotImplemented;
}

/// @compileError("message")
/// Emit a compile-time error
fn compileErrorMacro(ctx: *expander.MacroContext, invocation: *ast.Node) !?*ast.Node {
    const macro = &invocation.data.macro_invocation;

    if (macro.arguments.len < 1) {
        try ctx.reportError(invocation.location, "@compileError requires a message argument");
        return null;
    }

    const message = switch (macro.arguments[0]) {
        .expression => |expr| blk: {
            if (expr.kind == .string_literal) {
                break :blk expr.data.string_literal;
            } else {
                try ctx.reportError(invocation.location, "@compileError message must be a string literal");
                return null;
            }
        },
        else => {
            try ctx.reportError(invocation.location, "@compileError message must be a string literal");
            return null;
        },
    };

    try ctx.reportError(invocation.location, message);
    return null;
}

/// @serialize
/// Generate serialization methods
fn serializeMacro(ctx: *expander.MacroContext, invocation: *ast.Node) !?*ast.Node {
    // Delegate to @derive(Serialize)
    return try deriveMacro(ctx, invocation);
}

// === Helper functions ===

fn createMemberAccess(arena: *ast.ASTArena, loc: ast.SourceLocation, object: []const u8, property: []const u8) !*ast.Node {
    const obj_node = try expander.createIdentifier(arena, loc, object);
    const prop_node = try expander.createIdentifier(arena, loc, property);

    return try arena.createNode(
        .member_expr,
        loc,
        .{
            .member_expr = .{
                .object = obj_node,
                .property = prop_node,
                .computed = false,
            },
        },
    );
}

// === Direct generation functions (not requiring MacroContext) ===

/// Generate equals method directly (without MacroContext)
fn generateEqualsMethodDirect(arena: *ast.ASTArena, allocator: std.mem.Allocator, class: *ast.node.ClassDecl, loc: ast.SourceLocation) !*ast.Node {
    // Build comparison expression: this.field === other.field for each property
    var comparisons = std.ArrayList(*ast.Node).init(allocator);
    defer comparisons.deinit();

    for (class.members) |member| {
        if (member.kind == .property_decl) {
            const prop = &member.data.property_decl;

            // this.field
            const this_member = try createMemberAccess(arena, loc, "this", prop.name);

            // other.field
            const other_member = try createMemberAccess(arena, loc, "other", prop.name);

            // this.field === other.field
            const comparison = try expander.createBinaryExpr(arena, loc, .eq, this_member, other_member);

            try comparisons.append(comparison);
        }
    }

    // Handle case where there are no properties
    var result_expr: *ast.Node = undefined;
    if (comparisons.items.len == 0) {
        // If no properties, always return true
        result_expr = try arena.createNode(.boolean_literal, loc, .{ .boolean_literal = true });
    } else {
        // Chain comparisons with &&
        result_expr = comparisons.items[0];
        for (comparisons.items[1..]) |comp| {
            result_expr = try expander.createBinaryExpr(arena, loc, .@"and", result_expr, comp);
        }
    }

    // return <result_expr>;
    const return_stmt = try arena.createNode(
        .return_stmt,
        loc,
        .{ .return_stmt = .{ .argument = result_expr } },
    );

    // { return ...; }
    const body_stmts = try allocator.alloc(*ast.Node, 1);
    body_stmts[0] = return_stmt;

    const body = try arena.createNode(
        .block_stmt,
        loc,
        .{ .block_stmt = .{ .statements = body_stmts } },
    );

    // Parameter: other: ClassName
    const type_ref = try allocator.create(ast.types.TypeReference);
    type_ref.* = .{
        .name = class.name,
        .type_args = &[_]*ast.Type{},
    };
    const param_type = try arena.createType(
        .type_reference,
        loc,
        .{ .type_reference = type_ref },
    );

    const params = try allocator.alloc(ast.node.FunctionExpr.FunctionParam, 1);
    params[0] = .{
        .name = "other",
        .type = param_type,
        .optional = false,
        .default_value = null,
    };

    // Return type: boolean
    const return_type = try arena.createType(.boolean, loc, .{ .boolean = {} });

    // Create method declaration
    return try expander.createMethodDecl(arena, loc, "equals", params, return_type, body);
}

/// Generate hash method directly (without MacroContext)
fn generateHashMethodDirect(arena: *ast.ASTArena, allocator: std.mem.Allocator, class: *ast.node.ClassDecl, loc: ast.SourceLocation) !*ast.Node {
    // Build hash expression: XOR all field hashes together
    var hash_expr: ?*ast.Node = null;

    for (class.members) |member| {
        if (member.kind == .property_decl) {
            const prop = &member.data.property_decl;

            // this.field
            const field_access = try createMemberAccess(arena, loc, "this", prop.name);

            // For simplicity, just use the field value directly
            // In a real implementation, we'd call a hash function
            const field_hash = field_access;

            // XOR with previous hash
            if (hash_expr) |prev| {
                hash_expr = try expander.createBinaryExpr(arena, loc, .bit_xor, prev, field_hash);
            } else {
                hash_expr = field_hash;
            }
        }
    }

    // If no properties, return 0
    if (hash_expr == null) {
        hash_expr = try arena.createNode(.number_literal, loc, .{ .number_literal = 0 });
    }

    // return <hash_expr>;
    const return_stmt = try arena.createNode(
        .return_stmt,
        loc,
        .{ .return_stmt = .{ .argument = hash_expr } },
    );

    const body_stmts = try allocator.alloc(*ast.Node, 1);
    body_stmts[0] = return_stmt;

    const body = try arena.createNode(
        .block_stmt,
        loc,
        .{ .block_stmt = .{ .statements = body_stmts } },
    );

    const return_type = try arena.createType(.number, loc, .{ .number = {} });

    return try expander.createMethodDecl(arena, loc, "hash", &[_]ast.node.FunctionExpr.FunctionParam{}, return_type, body);
}
