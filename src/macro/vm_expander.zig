/// VM-based Macro Expander
/// Uses Hermes runtime to execute macros written in JavaScript/TypeScript
///
/// This is the bridge between Metascript's AST and the Hermes VM.
/// Macros are written in JavaScript and have access to AST manipulation APIs.
///
/// Supports two types of macros:
/// 1. Built-in macros (hardcoded in macro_vm.zig)
/// 2. Source-defined macros (@macro function ... in .ms files)
///
/// REQUIRES: Build with -Denable-vm=true (Hermes must be linked)

const std = @import("std");
const ast = @import("../ast/ast.zig");
const vm = @import("../vm/macro_vm.zig");
const source_registry = @import("source_registry.zig");
const Lexer = @import("../lexer/lexer.zig").Lexer;
const Parser = @import("../parser/parser.zig").Parser;
const disk_cache = @import("../transam/disk_cache.zig");
const network_cache = @import("../transam/network_cache.zig");
const bytecode_compiler = @import("../vm/bytecode_compiler.zig");

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
    source_macros: source_registry.SourceMacroRegistry,
    errors: std.ArrayList(MacroErrorInfo),

    /// Source buffers that must stay alive for AST string references
    source_buffers: std.ArrayList([]const u8),

    /// Allocated path strings that need to be freed
    allocated_paths: std.ArrayList([]const u8),

    /// Optional bytecode cache for faster macro execution
    bytecode_cache: ?*disk_cache.BytecodeCache,

    /// Optional network cache for @comptime fetch() responses
    net_cache: ?*network_cache.NetworkCache,

    pub fn init(arena: *ast.ASTArena, allocator: std.mem.Allocator) !VMMacroContext {
        return initWithCaches(arena, allocator, null, null);
    }

    /// Initialize with optional bytecode cache for faster execution
    pub fn initWithCache(
        arena: *ast.ASTArena,
        allocator: std.mem.Allocator,
        cache: ?*disk_cache.BytecodeCache,
    ) !VMMacroContext {
        return initWithCaches(arena, allocator, cache, null);
    }

    /// Initialize with both bytecode and network caches
    pub fn initWithCaches(
        arena: *ast.ASTArena,
        allocator: std.mem.Allocator,
        bytecode_cache_ptr: ?*disk_cache.BytecodeCache,
        net_cache_ptr: ?*network_cache.NetworkCache,
    ) !VMMacroContext {
        const vm_instance = try allocator.create(vm.MacroVM);
        vm_instance.* = try vm.MacroVM.initWithCaches(allocator, arena, bytecode_cache_ptr, net_cache_ptr);

        return .{
            .arena = arena,
            .allocator = allocator,
            .vm_instance = vm_instance,
            .source_macros = source_registry.SourceMacroRegistry.init(allocator),
            .errors = std.ArrayList(MacroErrorInfo).init(allocator),
            .source_buffers = std.ArrayList([]const u8).init(allocator),
            .allocated_paths = std.ArrayList([]const u8).init(allocator),
            .bytecode_cache = bytecode_cache_ptr,
            .net_cache = net_cache_ptr,
        };
    }

    pub fn deinit(self: *VMMacroContext) void {
        self.vm_instance.deinit();
        self.allocator.destroy(self.vm_instance);
        self.source_macros.deinit();
        self.errors.deinit();
        // Free source buffers
        for (self.source_buffers.items) |buf| {
            self.allocator.free(buf);
        }
        self.source_buffers.deinit();
        // Free allocated path strings
        for (self.allocated_paths.items) |path| {
            self.allocator.free(path);
        }
        self.allocated_paths.deinit();
    }

    /// Collect all @macro function declarations from the program
    pub fn collectSourceMacros(self: *VMMacroContext, program: *ast.Node) !void {
        var collector = source_registry.MacroDeclCollector.init(&self.source_macros);
        try collector.collectFromProgram(program);
    }

    /// Load macros from a .ms file path
    pub fn loadMacrosFromFile(self: *VMMacroContext, path: []const u8) !void {
        // Read file
        const file = std.fs.cwd().openFile(path, .{}) catch |err| {
            std.log.warn("[VMMacroContext] Could not open {s}: {}", .{ path, err });
            return;
        };
        defer file.close();

        const source = try file.readToEndAlloc(self.allocator, 10 * 1024 * 1024);
        // Keep source alive - AST strings reference it
        try self.source_buffers.append(source);

        // Parse
        const file_id = try self.arena.addFile(path);
        var lexer = try Lexer.init(self.allocator, source, file_id);
        defer lexer.deinit();
        var parser = Parser.init(self.allocator, self.arena, &lexer, file_id);
        defer parser.deinit();

        const program = parser.parse() catch |err| {
            std.log.warn("[VMMacroContext] Parse error in {s}: {}", .{ path, err });
            return;
        };

        // Collect macros
        var collector = source_registry.MacroDeclCollector.init(&self.source_macros);
        try collector.collectFromProgram(program);

        std.log.info("[VMMacroContext] Loaded macros from {s}", .{path});
    }

    /// Load standard library macros from std/macros/
    pub fn loadStdMacros(self: *VMMacroContext) !void {
        // Try common locations
        const paths = [_][]const u8{
            "std/macros/derive.ms",
            "./std/macros/derive.ms",
        };

        for (paths) |path| {
            if (std.fs.cwd().access(path, .{})) |_| {
                try self.loadMacrosFromFile(path);
                return;
            } else |_| {}
        }

        std.log.debug("[VMMacroContext] std/macros not found, using built-in macros only", .{});
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

        // Pass 0: Process import statements to load macros from imported modules
        try self.processImports(program);

        // Pass 1: Load standard library macros (std/macros/derive.ms etc.)
        // Only if no explicit imports were found
        if (self.ctx.source_macros.count() == 0) {
            try self.ctx.loadStdMacros();
        }

        // Pass 2: Collect all source-defined macros from this file (@macro function ...)
        try self.ctx.collectSourceMacros(program);

        if (self.ctx.source_macros.count() > 0) {
            std.log.info("[VM] Found {} macro(s) total (imports + std + source-defined)", .{self.ctx.source_macros.count()});
        }

        // Pass 3: Expand all macro invocations
        const statements = program.data.program.statements;

        for (statements) |stmt| {
            // Skip macro_decl and import_decl nodes
            if (stmt.kind != .macro_decl and stmt.kind != .import_decl) {
                try self.expandNode(stmt);
            }
        }

        return program;
    }

    /// Process import statements to load macros from imported modules
    fn processImports(self: *VMMacroExpander, program: *ast.Node) !void {
        const statements = program.data.program.statements;

        for (statements) |stmt| {
            if (stmt.kind == .import_decl) {
                const import_decl = &stmt.data.import_decl;
                var source = import_decl.source;

                // Strip surrounding quotes if present
                if (source.len >= 2) {
                    if ((source[0] == '"' and source[source.len - 1] == '"') or
                        (source[0] == '\'' and source[source.len - 1] == '\''))
                    {
                        source = source[1 .. source.len - 1];
                    }
                }

                // Resolve the import path
                const resolved_path = try self.resolveImportPath(source);

                if (resolved_path) |path| {
                    std.log.info("[VM] Loading macros from import: {s} -> {s}", .{ source, path });
                    try self.ctx.loadMacrosFromFile(path);
                } else {
                    std.log.debug("[VM] Could not resolve import: {s}", .{source});
                }
            }
        }
    }

    /// Resolve an import path like "std/macros/derive" to a file path
    fn resolveImportPath(self: *VMMacroExpander, source: []const u8) !?[]const u8 {
        // Try source as-is first (if it ends with .ms)
        if (std.mem.endsWith(u8, source, ".ms")) {
            if (std.fs.cwd().access(source, .{})) |_| {
                return source;
            } else |_| {}
        }

        // Try appending .ms extension
        const with_ext = try std.fmt.allocPrint(self.ctx.allocator, "{s}.ms", .{source});

        if (std.fs.cwd().access(with_ext, .{})) |_| {
            // Track for cleanup
            try self.ctx.allocated_paths.append(with_ext);
            return with_ext;
        } else |_| {
            self.ctx.allocator.free(with_ext);
        }

        // Handle std/ imports - also try index.ms in directory
        if (std.mem.startsWith(u8, source, "std/")) {
            const index_path = try std.fmt.allocPrint(self.ctx.allocator, "{s}/index.ms", .{source});

            if (std.fs.cwd().access(index_path, .{})) |_| {
                // Track for cleanup
                try self.ctx.allocated_paths.append(index_path);
                return index_path;
            } else |_| {
                self.ctx.allocator.free(index_path);
            }
        }

        return null;
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

            // Check if this is a source-defined macro
            std.log.debug("[VM] Looking up macro '{s}' (len={})", .{ name, name.len });
            if (self.ctx.source_macros.contains(name)) {
                std.log.info("[VM] Expanding source-defined @{s} on class {s}", .{ name, class.name });

                // Get the transpiled JavaScript (also computes js_hash)
                if (try self.ctx.source_macros.getJavaScript(name)) |js_source| {
                    std.log.debug("[VM] Macro JS:\n{s}", .{js_source});

                    // Try bytecode execution (fast path)
                    const js_hash = self.ctx.source_macros.getJSHash(name);
                    if (js_hash != null and self.ctx.bytecode_cache != null) {
                        const cache = self.ctx.bytecode_cache.?;
                        if (cache.getBytecode(js_hash.?)) |bytecode| {
                            // Cache hit! Execute bytecode directly (~0.2ms)
                            // PE4 FIX: bytecode is now owned, must free after use
                            defer self.ctx.allocator.free(bytecode);
                            std.log.debug("[VM] Using cached bytecode for @{s}", .{name});
                            try self.ctx.vm_instance.executeBytecode(bytecode, "source_macro.hbc", class_node);
                            continue;
                        }

                        // Cache miss: compile to bytecode
                        if (bytecode_compiler.isHermescAvailable()) {
                            const bytecode = bytecode_compiler.compileToBytescode(
                                self.ctx.allocator,
                                js_source,
                                name,
                            ) catch |err| {
                                std.log.debug("[VM] Bytecode compilation failed: {}, using source", .{err});
                                try self.ctx.vm_instance.executeSourceMacro(js_source, class_node);
                                continue;
                            };
                            defer self.ctx.allocator.free(bytecode);

                            // Store in cache
                            cache.storeBytecode(js_hash.?, bytecode) catch |err| {
                                std.log.debug("[VM] Failed to cache bytecode: {}", .{err});
                            };

                            std.log.debug("[VM] Compiled and cached bytecode for @{s}", .{name});
                            try self.ctx.vm_instance.executeBytecode(bytecode, "source_macro.hbc", class_node);
                            continue;
                        }
                    }

                    // Fallback: execute source directly (~45ms)
                    try self.ctx.vm_instance.executeSourceMacro(js_source, class_node);
                } else {
                    std.log.warn("[VM] Failed to transpile source macro @{s}", .{name});
                }
            } else {
                // Built-in macro
                std.log.info("[VM] Expanding built-in @{s}({s}) on class {s}", .{
                    name,
                    if (traits.items.len > 0) traits.items[0] else "...",
                    class.name,
                });

                // Execute the macro using Hermes VM
                try self.ctx.vm_instance.executeMacro(name, traits.items, class_node);
            }
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
    return expandAllMacrosWithCaches(arena, allocator, program, null, null);
}

/// Expand all macros with optional bytecode cache for faster execution
pub fn expandAllMacrosWithCache(
    arena: *ast.ASTArena,
    allocator: std.mem.Allocator,
    program: *ast.Node,
    cache: ?*disk_cache.BytecodeCache,
) !*ast.Node {
    return expandAllMacrosWithCaches(arena, allocator, program, cache, null);
}

/// Expand all macros with both bytecode and network caches
/// Network cache enables @comptime fetch() to cache responses across compilations
pub fn expandAllMacrosWithCaches(
    arena: *ast.ASTArena,
    allocator: std.mem.Allocator,
    program: *ast.Node,
    bytecode_cache_ptr: ?*disk_cache.BytecodeCache,
    net_cache_ptr: ?*network_cache.NetworkCache,
) !*ast.Node {
    var ctx = try VMMacroContext.initWithCaches(arena, allocator, bytecode_cache_ptr, net_cache_ptr);
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
