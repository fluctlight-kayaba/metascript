// Transform Pipeline
// Runs configurable AST transforms after macro expansion, before type checking
//
// Transforms are specified in build.ms:
//   transforms: [
//       varHoist(),          // JavaScript-style var hoisting
//       optionalChain(),     // a?.b?.c → safe access
//       nullishCoalesce(),   // a ?? b → null coalescing
//   ]

const std = @import("std");
const ast = @import("../ast/ast.zig");
const node_mod = @import("../ast/node.zig");
const types = @import("../ast/types.zig");
const build_config = @import("../build/config.zig");

pub const TransformError = error{
    TransformFailed,
    InvalidTransform,
    DependencyCycle,
    OutOfMemory,
    InvalidNodeType,
};

/// Result of a transform operation
pub const TransformResult = struct {
    /// The transformed node (may be same as input if no changes)
    node: *node_mod.Node,
    /// Whether the transform made any changes
    changed: bool,
    /// Number of transformations applied
    transform_count: usize,
};

/// Context passed to transforms
pub const TransformContext = struct {
    /// Arena allocator for creating new nodes
    arena: *ast.ASTArena,
    /// Temp allocator for intermediate work
    allocator: std.mem.Allocator,
    /// Current file path (for error messages)
    file_path: []const u8,
    /// Transform options from build.ms
    options: ?std.json.Value,
    /// Stats tracking
    stats: Stats,

    pub const Stats = struct {
        nodes_visited: usize = 0,
        nodes_transformed: usize = 0,
        transforms_applied: usize = 0,
    };

    pub fn init(arena: *ast.ASTArena, allocator: std.mem.Allocator, file_path: []const u8) TransformContext {
        return .{
            .arena = arena,
            .allocator = allocator,
            .file_path = file_path,
            .options = null,
            .stats = .{},
        };
    }

    pub fn withOptions(self: TransformContext, options: ?std.json.Value) TransformContext {
        var ctx = self;
        ctx.options = options;
        return ctx;
    }

    /// Create a new AST node
    pub fn createNode(self: *TransformContext, kind: node_mod.NodeKind, loc: ast.SourceLocation, data: node_mod.Node.NodeData) !*node_mod.Node {
        return self.arena.createNode(kind, loc, data);
    }

    /// Create a deep copy of an existing node (recursively clones all children)
    pub fn cloneNode(self: *TransformContext, original: *const node_mod.Node) !*node_mod.Node {
        const new_node = try self.arena.allocator().create(node_mod.Node);
        new_node.kind = original.kind;
        new_node.location = original.location;

        // Deep clone based on node kind
        new_node.data = switch (original.kind) {
            // Leaf nodes - just copy the data
            .number_literal => .{ .number_literal = original.data.number_literal },
            .string_literal => .{ .string_literal = original.data.string_literal },
            .boolean_literal => .{ .boolean_literal = original.data.boolean_literal },
            .null_literal => .{ .null_literal = {} },
            .identifier => .{ .identifier = original.data.identifier },

            // Binary expressions - clone both children
            .binary_expr => .{ .binary_expr = .{
                .op = original.data.binary_expr.op,
                .left = try self.cloneNode(original.data.binary_expr.left),
                .right = try self.cloneNode(original.data.binary_expr.right),
            } },

            // Unary expressions - clone argument
            .unary_expr => .{ .unary_expr = .{
                .op = original.data.unary_expr.op,
                .argument = try self.cloneNode(original.data.unary_expr.argument),
            } },

            // Member expressions - clone object
            .member_expr => .{ .member_expr = .{
                .object = try self.cloneNode(original.data.member_expr.object),
                .property = original.data.member_expr.property,
                .computed = original.data.member_expr.computed,
            } },

            // Call expressions - clone callee and arguments
            .call_expr => blk: {
                const call = &original.data.call_expr;
                var new_args = try self.arena.allocator().alloc(*node_mod.Node, call.arguments.len);
                for (call.arguments, 0..) |arg, i| {
                    new_args[i] = try self.cloneNode(arg);
                }
                break :blk .{ .call_expr = .{
                    .callee = try self.cloneNode(call.callee),
                    .arguments = new_args,
                    .type_args = call.type_args, // Shallow copy type args
                } };
            },

            // Conditional expressions - clone all three parts
            .conditional_expr => .{ .conditional_expr = .{
                .condition = try self.cloneNode(original.data.conditional_expr.condition),
                .consequent = try self.cloneNode(original.data.conditional_expr.consequent),
                .alternate = try self.cloneNode(original.data.conditional_expr.alternate),
            } },

            // For other node types, fall back to shallow copy
            // TODO: Add deep cloning for all node types as needed
            else => original.data,
        };

        return new_node;
    }
};

/// Transform function signature
pub const TransformFn = *const fn (*TransformContext, *node_mod.Node) TransformError!TransformResult;

/// A registered transform
pub const Transform = struct {
    name: []const u8,
    transform_fn: TransformFn,
    /// Transforms that must run before this one
    run_after: []const []const u8,
    /// Transforms that must run after this one
    run_before: []const []const u8,
    /// Whether this is a builtin transform
    builtin: bool,
    /// Custom options from build.ms
    options: ?std.json.Value,
};

/// Transform Pipeline - manages and runs transforms
pub const Pipeline = struct {
    allocator: std.mem.Allocator,
    transforms: std.ArrayList(Transform),
    builtin_registry: std.StringHashMap(TransformFn),

    pub fn init(allocator: std.mem.Allocator) Pipeline {
        var pipeline = Pipeline{
            .allocator = allocator,
            .transforms = std.ArrayList(Transform).init(allocator),
            .builtin_registry = std.StringHashMap(TransformFn).init(allocator),
        };

        // Register builtin transforms
        pipeline.registerBuiltins();

        return pipeline;
    }

    pub fn deinit(self: *Pipeline) void {
        self.transforms.deinit();
        self.builtin_registry.deinit();
    }

    /// Register all builtin transforms
    fn registerBuiltins(self: *Pipeline) void {
        // Import builtin transforms
        const var_hoist = @import("builtin/var_hoist.zig");
        const optional_chain = @import("builtin/optional_chain.zig");
        const nullish_coalesce = @import("builtin/nullish_coalesce.zig");

        self.builtin_registry.put("var_hoist", var_hoist.transform) catch {};
        self.builtin_registry.put("optional_chain", optional_chain.transform) catch {};
        self.builtin_registry.put("nullish_coalesce", nullish_coalesce.transform) catch {};
    }

    /// Add a transform from build.ms config
    pub fn addFromConfig(self: *Pipeline, config: build_config.TransformConfig) !void {
        if (config.builtin) {
            // Look up builtin transform
            const transform_fn = self.builtin_registry.get(config.name) orelse {
                std.debug.print("Warning: Unknown builtin transform '{s}'\n", .{config.name});
                return error.InvalidTransform;
            };

            try self.transforms.append(.{
                .name = config.name,
                .transform_fn = transform_fn,
                .run_after = config.run_after,
                .run_before = config.run_before,
                .builtin = true,
                .options = config.options,
            });
        } else {
            // Custom transform - TODO: load from path
            std.debug.print("Warning: Custom transforms not yet implemented ('{s}')\n", .{config.name});
        }
    }

    /// Load transforms from build config
    pub fn loadFromBuildConfig(self: *Pipeline, build_cfg: *const build_config.BuildConfig) !void {
        for (build_cfg.transforms) |transform_config| {
            self.addFromConfig(transform_config) catch |err| {
                std.debug.print("Warning: Failed to load transform '{s}': {s}\n", .{
                    transform_config.name,
                    @errorName(err),
                });
            };
        }
    }

    /// Sort transforms respecting dependencies using Kahn's algorithm (topological sort)
    /// Dependencies:
    ///   - run_after: This transform must run AFTER the specified transforms
    ///   - run_before: This transform must run BEFORE the specified transforms
    pub fn sortTransforms(self: *Pipeline) !void {
        const n = self.transforms.items.len;
        if (n <= 1) return;

        // Build name -> index mapping
        var name_to_idx = std.StringHashMap(usize).init(self.allocator);
        defer name_to_idx.deinit();

        for (self.transforms.items, 0..) |t, i| {
            try name_to_idx.put(t.name, i);
        }

        // Build adjacency list and in-degree count
        // Edge A -> B means A must run before B
        var adj = try self.allocator.alloc(std.ArrayList(usize), n);
        defer {
            for (adj) |*list| list.deinit();
            self.allocator.free(adj);
        }
        for (adj) |*list| list.* = std.ArrayList(usize).init(self.allocator);

        var in_degree = try self.allocator.alloc(usize, n);
        defer self.allocator.free(in_degree);
        @memset(in_degree, 0);

        // Process run_after: if T has run_after = [A, B], then A->T, B->T
        for (self.transforms.items, 0..) |t, i| {
            for (t.run_after) |dep_name| {
                if (name_to_idx.get(dep_name)) |dep_idx| {
                    try adj[dep_idx].append(i);
                    in_degree[i] += 1;
                }
                // Note: Unknown dependencies are silently ignored (already warned during addFromConfig)
            }
        }

        // Process run_before: if T has run_before = [X, Y], then T->X, T->Y
        for (self.transforms.items, 0..) |t, i| {
            for (t.run_before) |before_name| {
                if (name_to_idx.get(before_name)) |before_idx| {
                    try adj[i].append(before_idx);
                    in_degree[before_idx] += 1;
                }
            }
        }

        // Kahn's algorithm: BFS starting from nodes with in_degree = 0
        var queue = std.ArrayList(usize).init(self.allocator);
        defer queue.deinit();

        for (in_degree, 0..) |deg, i| {
            if (deg == 0) try queue.append(i);
        }

        var sorted_indices = std.ArrayList(usize).init(self.allocator);
        defer sorted_indices.deinit();

        while (queue.items.len > 0) {
            const current = queue.orderedRemove(0);
            try sorted_indices.append(current);

            for (adj[current].items) |neighbor| {
                in_degree[neighbor] -= 1;
                if (in_degree[neighbor] == 0) {
                    try queue.append(neighbor);
                }
            }
        }

        // Check for cycle: if we didn't process all nodes, there's a cycle
        if (sorted_indices.items.len != n) {
            // Find which transforms are in the cycle for better error message
            var cycle_names = std.ArrayList([]const u8).init(self.allocator);
            defer cycle_names.deinit();

            for (in_degree, 0..) |deg, i| {
                if (deg > 0) {
                    try cycle_names.append(self.transforms.items[i].name);
                }
            }

            std.debug.print("Dependency cycle detected involving transforms: ", .{});
            for (cycle_names.items, 0..) |name, i| {
                if (i > 0) std.debug.print(", ", .{});
                std.debug.print("{s}", .{name});
            }
            std.debug.print("\n", .{});

            return error.DependencyCycle;
        }

        // Reorder transforms according to sorted order
        var new_transforms = try self.allocator.alloc(Transform, n);
        defer self.allocator.free(new_transforms);

        for (sorted_indices.items, 0..) |orig_idx, new_idx| {
            new_transforms[new_idx] = self.transforms.items[orig_idx];
        }

        // Copy back
        for (new_transforms, 0..) |t, i| {
            self.transforms.items[i] = t;
        }
    }

    /// Run all transforms on an AST
    pub fn run(self: *Pipeline, arena: *ast.ASTArena, allocator: std.mem.Allocator, root: *node_mod.Node, file_path: []const u8) !TransformResult {
        var current = root;
        var total_changed = false;
        var total_count: usize = 0;

        for (self.transforms.items) |transform| {
            var ctx = TransformContext.init(arena, allocator, file_path);
            ctx = ctx.withOptions(transform.options);

            const result = try transform.transform_fn(&ctx, current);
            if (result.changed) {
                current = result.node;
                total_changed = true;
                total_count += result.transform_count;
            }
        }

        return .{
            .node = current,
            .changed = total_changed,
            .transform_count = total_count,
        };
    }

    /// Get number of registered transforms
    pub fn count(self: *const Pipeline) usize {
        return self.transforms.items.len;
    }
};

/// Walk AST and apply a transform function to each node
pub fn walkAndTransform(
    ctx: *TransformContext,
    node_ptr: *node_mod.Node,
    comptime visitor_fn: fn (*TransformContext, *node_mod.Node) TransformError!?*node_mod.Node,
) TransformError!TransformResult {
    ctx.stats.nodes_visited += 1;

    // First, recursively transform children
    var changed = false;
    switch (node_ptr.kind) {
        .program => {
            const program = &node_ptr.data.program;
            for (program.statements, 0..) |stmt, i| {
                const result = try walkAndTransform(ctx, stmt, visitor_fn);
                if (result.changed) {
                    program.statements[i] = result.node;
                    changed = true;
                }
            }
        },
        .block_stmt => {
            const block = &node_ptr.data.block_stmt;
            for (block.statements, 0..) |stmt, i| {
                const result = try walkAndTransform(ctx, stmt, visitor_fn);
                if (result.changed) {
                    block.statements[i] = result.node;
                    changed = true;
                }
            }
        },
        .function_decl => {
            const func = &node_ptr.data.function_decl;
            if (func.body) |body| {
                const result = try walkAndTransform(ctx, body, visitor_fn);
                if (result.changed) {
                    func.body = result.node;
                    changed = true;
                }
            }
        },
        .function_expr => {
            const func = &node_ptr.data.function_expr;
            const result = try walkAndTransform(ctx, func.body, visitor_fn);
            if (result.changed) {
                func.body = result.node;
                changed = true;
            }
        },
        .if_stmt => {
            const if_s = &node_ptr.data.if_stmt;
            const cond_result = try walkAndTransform(ctx, if_s.condition, visitor_fn);
            if (cond_result.changed) {
                if_s.condition = cond_result.node;
                changed = true;
            }
            const cons_result = try walkAndTransform(ctx, if_s.consequent, visitor_fn);
            if (cons_result.changed) {
                if_s.consequent = cons_result.node;
                changed = true;
            }
            if (if_s.alternate) |alt| {
                const alt_result = try walkAndTransform(ctx, alt, visitor_fn);
                if (alt_result.changed) {
                    if_s.alternate = alt_result.node;
                    changed = true;
                }
            }
        },
        .while_stmt => {
            const while_s = &node_ptr.data.while_stmt;
            const cond_result = try walkAndTransform(ctx, while_s.condition, visitor_fn);
            if (cond_result.changed) {
                while_s.condition = cond_result.node;
                changed = true;
            }
            const body_result = try walkAndTransform(ctx, while_s.body, visitor_fn);
            if (body_result.changed) {
                while_s.body = body_result.node;
                changed = true;
            }
        },
        .for_stmt => {
            const for_s = &node_ptr.data.for_stmt;
            if (for_s.init) |init| {
                const init_result = try walkAndTransform(ctx, init, visitor_fn);
                if (init_result.changed) {
                    for_s.init = init_result.node;
                    changed = true;
                }
            }
            if (for_s.condition) |cond| {
                const cond_result = try walkAndTransform(ctx, cond, visitor_fn);
                if (cond_result.changed) {
                    for_s.condition = cond_result.node;
                    changed = true;
                }
            }
            if (for_s.update) |update| {
                const update_result = try walkAndTransform(ctx, update, visitor_fn);
                if (update_result.changed) {
                    for_s.update = update_result.node;
                    changed = true;
                }
            }
            const body_result = try walkAndTransform(ctx, for_s.body, visitor_fn);
            if (body_result.changed) {
                for_s.body = body_result.node;
                changed = true;
            }
        },
        .binary_expr => {
            const binary = &node_ptr.data.binary_expr;
            const left_result = try walkAndTransform(ctx, binary.left, visitor_fn);
            if (left_result.changed) {
                binary.left = left_result.node;
                changed = true;
            }
            const right_result = try walkAndTransform(ctx, binary.right, visitor_fn);
            if (right_result.changed) {
                binary.right = right_result.node;
                changed = true;
            }
        },
        .unary_expr => {
            const unary = &node_ptr.data.unary_expr;
            const arg_result = try walkAndTransform(ctx, unary.argument, visitor_fn);
            if (arg_result.changed) {
                unary.argument = arg_result.node;
                changed = true;
            }
        },
        .call_expr => {
            const call = &node_ptr.data.call_expr;
            const callee_result = try walkAndTransform(ctx, call.callee, visitor_fn);
            if (callee_result.changed) {
                call.callee = callee_result.node;
                changed = true;
            }
            for (call.arguments, 0..) |arg, i| {
                const arg_result = try walkAndTransform(ctx, arg, visitor_fn);
                if (arg_result.changed) {
                    call.arguments[i] = arg_result.node;
                    changed = true;
                }
            }
        },
        .member_expr => {
            const member = &node_ptr.data.member_expr;
            const obj_result = try walkAndTransform(ctx, member.object, visitor_fn);
            if (obj_result.changed) {
                member.object = obj_result.node;
                changed = true;
            }
            if (member.computed) {
                const prop_result = try walkAndTransform(ctx, member.property, visitor_fn);
                if (prop_result.changed) {
                    member.property = prop_result.node;
                    changed = true;
                }
            }
        },
        .return_stmt => {
            if (node_ptr.data.return_stmt.argument) |arg| {
                const arg_result = try walkAndTransform(ctx, arg, visitor_fn);
                if (arg_result.changed) {
                    node_ptr.data.return_stmt.argument = arg_result.node;
                    changed = true;
                }
            }
        },
        .expression_stmt => {
            const expr_result = try walkAndTransform(ctx, node_ptr.data.expression_stmt, visitor_fn);
            if (expr_result.changed) {
                node_ptr.data.expression_stmt = expr_result.node;
                changed = true;
            }
        },
        .variable_stmt => {
            const var_s = &node_ptr.data.variable_stmt;
            for (var_s.declarations, 0..) |*decl, i| {
                _ = i;
                if (decl.init) |init| {
                    const init_result = try walkAndTransform(ctx, init, visitor_fn);
                    if (init_result.changed) {
                        decl.init = init_result.node;
                        changed = true;
                    }
                }
            }
        },
        .array_expr => {
            const arr = &node_ptr.data.array_expr;
            for (arr.elements, 0..) |elem, i| {
                const elem_result = try walkAndTransform(ctx, elem, visitor_fn);
                if (elem_result.changed) {
                    arr.elements[i] = elem_result.node;
                    changed = true;
                }
            }
        },
        .object_expr => {
            const obj = &node_ptr.data.object_expr;
            for (obj.properties, 0..) |*prop, i| {
                _ = i;
                switch (prop.*) {
                    .property => |*p| {
                        const val_result = try walkAndTransform(ctx, p.value, visitor_fn);
                        if (val_result.changed) {
                            p.value = val_result.node;
                            changed = true;
                        }
                    },
                    .spread => |spread_node| {
                        const spread_result = try walkAndTransform(ctx, spread_node, visitor_fn);
                        if (spread_result.changed) {
                            prop.* = .{ .spread = spread_result.node };
                            changed = true;
                        }
                    },
                }
            }
        },
        .conditional_expr => {
            const cond = &node_ptr.data.conditional_expr;
            const condition_result = try walkAndTransform(ctx, cond.condition, visitor_fn);
            if (condition_result.changed) {
                cond.condition = condition_result.node;
                changed = true;
            }
            const cons_result = try walkAndTransform(ctx, cond.consequent, visitor_fn);
            if (cons_result.changed) {
                cond.consequent = cons_result.node;
                changed = true;
            }
            const alt_result = try walkAndTransform(ctx, cond.alternate, visitor_fn);
            if (alt_result.changed) {
                cond.alternate = alt_result.node;
                changed = true;
            }
        },
        .class_decl => {
            const class = &node_ptr.data.class_decl;
            for (class.members, 0..) |member, i| {
                const member_result = try walkAndTransform(ctx, member, visitor_fn);
                if (member_result.changed) {
                    class.members[i] = member_result.node;
                    changed = true;
                }
            }
        },
        .method_decl => {
            const method = &node_ptr.data.method_decl;
            if (method.body) |body| {
                const body_result = try walkAndTransform(ctx, body, visitor_fn);
                if (body_result.changed) {
                    method.body = body_result.node;
                    changed = true;
                }
            }
        },
        .constructor_decl => {
            const ctor = &node_ptr.data.constructor_decl;
            const body_result = try walkAndTransform(ctx, ctor.body, visitor_fn);
            if (body_result.changed) {
                ctor.body = body_result.node;
                changed = true;
            }
        },
        .new_expr => {
            const new_e = &node_ptr.data.new_expr;
            const callee_result = try walkAndTransform(ctx, new_e.callee, visitor_fn);
            if (callee_result.changed) {
                new_e.callee = callee_result.node;
                changed = true;
            }
            for (new_e.arguments, 0..) |arg, i| {
                const arg_result = try walkAndTransform(ctx, arg, visitor_fn);
                if (arg_result.changed) {
                    new_e.arguments[i] = arg_result.node;
                    changed = true;
                }
            }
        },
        .spread_element => {
            const spread = &node_ptr.data.spread_element;
            const arg_result = try walkAndTransform(ctx, spread.argument, visitor_fn);
            if (arg_result.changed) {
                spread.argument = arg_result.node;
                changed = true;
            }
        },
        .move_expr => {
            const move_e = &node_ptr.data.move_expr;
            const operand_result = try walkAndTransform(ctx, move_e.operand, visitor_fn);
            if (operand_result.changed) {
                move_e.operand = operand_result.node;
                changed = true;
            }
        },
        .import_decl => {
            // Import declarations don't have transformable children in most cases
            // The module specifier is a string, specifiers are identifiers
        },
        .export_decl => {
            const export_d = &node_ptr.data.export_decl;
            if (export_d.declaration) |decl| {
                const decl_result = try walkAndTransform(ctx, decl, visitor_fn);
                if (decl_result.changed) {
                    export_d.declaration = decl_result.node;
                    changed = true;
                }
            }
        },
        .property_decl => {
            const prop = &node_ptr.data.property_decl;
            if (prop.init) |init| {
                const init_result = try walkAndTransform(ctx, init, visitor_fn);
                if (init_result.changed) {
                    prop.init = init_result.node;
                    changed = true;
                }
            }
        },
        .type_alias_decl => {
            // Type alias RHS is a type expression, not a value expression
            // Transforms typically don't modify type-level code
        },
        .interface_decl => {
            // Interface members are type-level, not value-level
        },
        .macro_decl => {
            const macro_d = &node_ptr.data.macro_decl;
            const body_result = try walkAndTransform(ctx, macro_d.body, visitor_fn);
            if (body_result.changed) {
                macro_d.body = body_result.node;
                changed = true;
            }
        },
        .macro_invocation => {
            const macro_inv = &node_ptr.data.macro_invocation;
            // Only expression arguments can be transformed
            for (macro_inv.arguments, 0..) |*arg, i| {
                _ = i;
                if (arg.* == .expression) {
                    const arg_result = try walkAndTransform(ctx, arg.expression, visitor_fn);
                    if (arg_result.changed) {
                        arg.* = .{ .expression = arg_result.node };
                        changed = true;
                    }
                }
            }
            // Also transform target if present
            if (macro_inv.target) |target| {
                const target_result = try walkAndTransform(ctx, target, visitor_fn);
                if (target_result.changed) {
                    macro_inv.target = target_result.node;
                    changed = true;
                }
            }
        },
        .comptime_block => {
            const ct = &node_ptr.data.comptime_block;
            const body_result = try walkAndTransform(ctx, ct.body, visitor_fn);
            if (body_result.changed) {
                ct.body = body_result.node;
                changed = true;
            }
        },
        .quote_expr => {
            const quote = &node_ptr.data.quote_expr;
            const body_result = try walkAndTransform(ctx, quote.body, visitor_fn);
            if (body_result.changed) {
                quote.body = body_result.node;
                changed = true;
            }
        },
        // Leaf nodes - no children to transform
        .number_literal,
        .string_literal,
        .boolean_literal,
        .null_literal,
        .identifier,
        .break_stmt,
        .continue_stmt,
        .type_annotation,
        .extern_macro_decl,
        .compile_error,
        => {},
    }

    // Now apply the visitor function to this node
    if (try visitor_fn(ctx, node_ptr)) |new_node| {
        ctx.stats.nodes_transformed += 1;
        return .{
            .node = new_node,
            .changed = true,
            .transform_count = 1,
        };
    }

    return .{
        .node = node_ptr,
        .changed = changed,
        .transform_count = if (changed) 1 else 0,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "Pipeline init and deinit" {
    var pipeline = Pipeline.init(std.testing.allocator);
    defer pipeline.deinit();

    // Should have builtin transforms registered
    try std.testing.expect(pipeline.builtin_registry.count() > 0);
}

test "Pipeline add builtin transform" {
    var pipeline = Pipeline.init(std.testing.allocator);
    defer pipeline.deinit();

    try pipeline.addFromConfig(.{
        .name = "var_hoist",
        .builtin = true,
    });

    try std.testing.expectEqual(@as(usize, 1), pipeline.count());
}

test "sortTransforms: empty pipeline" {
    var pipeline = Pipeline.init(std.testing.allocator);
    defer pipeline.deinit();

    // Should not error on empty pipeline
    try pipeline.sortTransforms();
    try std.testing.expectEqual(@as(usize, 0), pipeline.count());
}

test "sortTransforms: single transform" {
    var pipeline = Pipeline.init(std.testing.allocator);
    defer pipeline.deinit();

    try pipeline.addFromConfig(.{ .name = "var_hoist", .builtin = true });
    try pipeline.sortTransforms();
    try std.testing.expectEqual(@as(usize, 1), pipeline.count());
}

test "sortTransforms: respects run_after dependency" {
    var pipeline = Pipeline.init(std.testing.allocator);
    defer pipeline.deinit();

    // Add B first, but B depends on A (run_after = ["var_hoist"])
    // After sort: var_hoist should come before optional_chain
    try pipeline.addFromConfig(.{
        .name = "optional_chain",
        .builtin = true,
        .run_after = &[_][]const u8{"var_hoist"},
    });
    try pipeline.addFromConfig(.{ .name = "var_hoist", .builtin = true });

    try pipeline.sortTransforms();

    // var_hoist should now be first
    try std.testing.expectEqualStrings("var_hoist", pipeline.transforms.items[0].name);
    try std.testing.expectEqualStrings("optional_chain", pipeline.transforms.items[1].name);
}

test "sortTransforms: respects run_before dependency" {
    var pipeline = Pipeline.init(std.testing.allocator);
    defer pipeline.deinit();

    // A has run_before = ["optional_chain"], meaning A runs before B
    try pipeline.addFromConfig(.{ .name = "optional_chain", .builtin = true });
    try pipeline.addFromConfig(.{
        .name = "var_hoist",
        .builtin = true,
        .run_before = &[_][]const u8{"optional_chain"},
    });

    try pipeline.sortTransforms();

    // var_hoist should be first (it runs before optional_chain)
    try std.testing.expectEqualStrings("var_hoist", pipeline.transforms.items[0].name);
    try std.testing.expectEqualStrings("optional_chain", pipeline.transforms.items[1].name);
}

test "sortTransforms: detects cycle" {
    var pipeline = Pipeline.init(std.testing.allocator);
    defer pipeline.deinit();

    // A depends on B, B depends on A -> cycle
    try pipeline.addFromConfig(.{
        .name = "var_hoist",
        .builtin = true,
        .run_after = &[_][]const u8{"optional_chain"},
    });
    try pipeline.addFromConfig(.{
        .name = "optional_chain",
        .builtin = true,
        .run_after = &[_][]const u8{"var_hoist"},
    });

    const result = pipeline.sortTransforms();
    try std.testing.expectError(error.DependencyCycle, result);
}

test "sortTransforms: chain of dependencies" {
    var pipeline = Pipeline.init(std.testing.allocator);
    defer pipeline.deinit();

    // Add in wrong order: C, B, A but dependencies say A -> B -> C
    try pipeline.addFromConfig(.{
        .name = "nullish_coalesce",
        .builtin = true,
        .run_after = &[_][]const u8{"optional_chain"},
    });
    try pipeline.addFromConfig(.{
        .name = "optional_chain",
        .builtin = true,
        .run_after = &[_][]const u8{"var_hoist"},
    });
    try pipeline.addFromConfig(.{ .name = "var_hoist", .builtin = true });

    try pipeline.sortTransforms();

    // Should be sorted as: var_hoist -> optional_chain -> nullish_coalesce
    try std.testing.expectEqualStrings("var_hoist", pipeline.transforms.items[0].name);
    try std.testing.expectEqualStrings("optional_chain", pipeline.transforms.items[1].name);
    try std.testing.expectEqualStrings("nullish_coalesce", pipeline.transforms.items[2].name);
}
