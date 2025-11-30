/// VM-based Macro Expander
/// Uses Hermes runtime to execute macros written in JavaScript/TypeScript
///
/// This is the bridge between Metascript's AST and the Hermes VM.
/// Macros are written in JavaScript and have access to AST manipulation APIs.
///
/// REQUIRES: Build with -Denable-vm=true (Hermes must be linked)

const std = @import("std");
const ast = @import("../ast/ast.zig");
const vm = @import("../vm/macro_vm.zig");

/// Error set for macro expansion
pub const MacroError = error{
    HermesInitFailed,
    MacroExecutionFailed,
    UnknownMacro,
    OutOfMemory,
};

/// Macro expansion error info
pub const MacroErrorInfo = struct {
    location: ast.SourceLocation,
    message: []const u8,
};

/// VM-based macro context - wraps Hermes runtime
pub const VMMacroContext = struct {
    arena: *ast.ASTArena,
    allocator: std.mem.Allocator,
    vm_instance: *vm.MacroVM,
    errors: std.ArrayList(MacroErrorInfo),

    pub fn init(arena: *ast.ASTArena, allocator: std.mem.Allocator) !VMMacroContext {
        const vm_instance = try allocator.create(vm.MacroVM);
        vm_instance.* = try vm.MacroVM.init(allocator, arena);

        return .{
            .arena = arena,
            .allocator = allocator,
            .vm_instance = vm_instance,
            .errors = std.ArrayList(MacroErrorInfo).init(allocator),
        };
    }

    pub fn deinit(self: *VMMacroContext) void {
        self.vm_instance.deinit();
        self.allocator.destroy(self.vm_instance);
        self.errors.deinit();
    }
};

/// VM-based Macro Expander - executes JavaScript macros via Hermes
pub const VMMacroExpander = struct {
    ctx: *VMMacroContext,

    pub fn init(ctx: *VMMacroContext) VMMacroExpander {
        return .{ .ctx = ctx };
    }

    /// Expand all macros in the AST using Hermes VM
    pub fn expandProgram(self: *VMMacroExpander, program: *ast.Node) !*ast.Node {
        std.debug.assert(program.kind == .program);

        const statements = program.data.program.statements;

        for (statements) |stmt| {
            try self.expandNode(stmt);
        }

        return program;
    }

    /// Expand macros in a single node
    fn expandNode(self: *VMMacroExpander, node: *ast.Node) !void {
        switch (node.kind) {
            .class_decl => {
                // Check for decorators on the class
                const class = &node.data.class_decl;
                if (class.decorators.len > 0) {
                    try self.expandClassDecorators(node);
                }

                // Recursively process class members
                for (class.members) |member| {
                    try self.expandNode(member);
                }
            },
            .function_decl => {
                // TODO: Function decorators not yet supported in AST
            },
            .block_stmt => {
                for (node.data.block_stmt.statements) |stmt| {
                    try self.expandNode(stmt);
                }
            },
            .program => {
                for (node.data.program.statements) |stmt| {
                    try self.expandNode(stmt);
                }
            },
            else => {},
        }
    }

    /// Expand decorators on a class declaration using Hermes VM
    fn expandClassDecorators(self: *VMMacroExpander, class_node: *ast.Node) !void {
        const class = &class_node.data.class_decl;

        for (class.decorators) |decorator| {
            const name = decorator.name;
            const args = decorator.arguments;

            // Parse decorator arguments into trait names
            var traits = std.ArrayList([]const u8).init(self.ctx.allocator);
            defer traits.deinit();

            for (args) |arg| {
                // Arguments should be identifiers like Eq, Hash, etc.
                if (arg.kind == .identifier) {
                    try traits.append(arg.data.identifier);
                }
            }

            // Log expansion
            std.log.info("[VM] Expanding @{s}({s}) on class {s}", .{
                name,
                if (traits.items.len > 0) traits.items[0] else "...",
                class.name,
            });

            // Execute the macro using Hermes VM
            try self.ctx.vm_instance.executeMacro(name, traits.items, class_node);
        }
    }
};

/// Execute a specific macro by name with the Hermes VM
pub fn executeMacroWithVM(
    arena: *ast.ASTArena,
    allocator: std.mem.Allocator,
    macro_name: []const u8,
    args: []const []const u8,
    target: *ast.Node,
) !void {
    var ctx = try VMMacroContext.init(arena, allocator);
    defer ctx.deinit();

    try ctx.vm_instance.executeMacro(macro_name, args, target);
}

/// Convenience function to expand all macros in a program
pub fn expandAllMacros(
    arena: *ast.ASTArena,
    allocator: std.mem.Allocator,
    program: *ast.Node,
) !*ast.Node {
    var ctx = try VMMacroContext.init(arena, allocator);
    defer ctx.deinit();

    var exp = VMMacroExpander.init(&ctx);
    return try exp.expandProgram(program);
}

// =============================================================================
// Tests (require Hermes to be linked)
// =============================================================================

test "VM macro expander initialization" {
    // This test requires Hermes runtime
    // Run with: zig build test -Denable-vm=true
    if (!@import("builtin").is_test) return;

    var arena = ast.ASTArena.init(std.testing.allocator);
    defer arena.deinit();

    var ctx = VMMacroContext.init(&arena, std.testing.allocator) catch |err| {
        // Expected to fail if Hermes not linked
        std.debug.print("Skipping VM test: {}\n", .{err});
        return;
    };
    defer ctx.deinit();

    try std.testing.expect(ctx.errors.items.len == 0);
}
