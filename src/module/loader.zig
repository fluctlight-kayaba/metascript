/// Module Loader
///
/// Loads and parses Metascript modules, managing:
/// - File reading and caching
/// - AST parsing per module
/// - Macro collection from imported modules
/// - Import/export resolution
///
/// This is the core of the module system that enables:
/// - import { derive } from "std/macros"
/// - LSP go-to-definition
/// - Bundling/transpilation

const std = @import("std");
const ast = @import("../ast/ast.zig");
const Lexer = @import("../lexer/lexer.zig").Lexer;
const Parser = @import("../parser/parser.zig").Parser;
const resolver = @import("resolver.zig");
const source_registry = @import("../macro/source_registry.zig");

/// Represents a loaded module
pub const Module = struct {
    /// Absolute file path
    path: []const u8,

    /// Parsed AST
    ast: *ast.Node,

    /// Source-defined macros in this module
    macros: std.StringHashMap(*ast.Node),

    /// Exported symbols
    exports: std.StringHashMap(ExportedSymbol),

    /// Import dependencies
    imports: std.ArrayList(ImportDecl),

    pub const ExportedSymbol = struct {
        name: []const u8,
        node: *ast.Node,
        kind: SymbolKind,
    };

    pub const SymbolKind = enum {
        macro,
        function,
        class,
        type_alias,
        variable,
    };

    pub const ImportDecl = struct {
        specifier: []const u8,
        resolved_path: ?[]const u8,
        symbols: []const []const u8,
    };
};

/// Module loader with caching
pub const ModuleLoader = struct {
    allocator: std.mem.Allocator,
    arena: *ast.ASTArena,
    resolver: resolver.ModuleResolver,

    /// Cache of loaded modules by absolute path
    modules: std.StringHashMap(*Module),

    /// Collected macros from all loaded modules
    macro_registry: source_registry.SourceMacroRegistry,

    pub fn init(allocator: std.mem.Allocator, arena: *ast.ASTArena) !ModuleLoader {
        const std_lib_path = try resolver.getStdLibPath(allocator);

        return .{
            .allocator = allocator,
            .arena = arena,
            .resolver = resolver.ModuleResolver.init(allocator, std_lib_path),
            .modules = std.StringHashMap(*Module).init(allocator),
            .macro_registry = source_registry.SourceMacroRegistry.init(allocator),
        };
    }

    pub fn deinit(self: *ModuleLoader) void {
        // Free modules
        var it = self.modules.valueIterator();
        while (it.next()) |mod| {
            mod.*.macros.deinit();
            mod.*.exports.deinit();
            mod.*.imports.deinit();
            self.allocator.destroy(mod.*);
        }
        self.modules.deinit();
        self.resolver.deinit();
        self.macro_registry.deinit();
    }

    /// Load a module from a file path
    pub fn loadModule(self: *ModuleLoader, path: []const u8) !*Module {
        // Check cache
        if (self.modules.get(path)) |cached| {
            return cached;
        }

        std.log.info("[ModuleLoader] Loading module: {s}", .{path});

        // Read file
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();
        const source = try file.readToEndAlloc(self.allocator, 10 * 1024 * 1024);
        defer self.allocator.free(source);

        // Parse
        const file_id = try self.arena.addFile(path);
        var lexer = try Lexer.init(self.allocator, source, file_id);
        defer lexer.deinit();
        var parser = Parser.init(self.allocator, self.arena, &lexer, file_id);
        defer parser.deinit();

        const program = try parser.parse();

        // Create module
        const module = try self.allocator.create(Module);
        module.* = .{
            .path = try self.allocator.dupe(u8, path),
            .ast = program,
            .macros = std.StringHashMap(*ast.Node).init(self.allocator),
            .exports = std.StringHashMap(Module.ExportedSymbol).init(self.allocator),
            .imports = std.ArrayList(Module.ImportDecl).init(self.allocator),
        };

        // Collect macros and exports
        try self.collectSymbols(module, program);

        // Cache
        try self.modules.put(module.path, module);

        return module;
    }

    /// Load standard library macros
    pub fn loadStdMacros(self: *ModuleLoader) !void {
        const std_macros_path = try std.fmt.allocPrint(
            self.allocator,
            "{s}/macros/index.ms",
            .{self.resolver.std_lib_path},
        );
        defer self.allocator.free(std_macros_path);

        if (std.fs.cwd().access(std_macros_path, .{})) |_| {
            _ = try self.loadModule(std_macros_path);
            std.log.info("[ModuleLoader] Loaded std/macros", .{});
        } else |_| {
            std.log.warn("[ModuleLoader] std/macros not found at: {s}", .{std_macros_path});
        }
    }

    /// Collect macro and export symbols from a module AST
    fn collectSymbols(self: *ModuleLoader, module: *Module, program: *ast.Node) !void {
        for (program.data.program.statements) |stmt| {
            switch (stmt.kind) {
                .macro_decl => {
                    const name = stmt.data.macro_decl.name;
                    try module.macros.put(name, stmt);
                    try module.exports.put(name, .{
                        .name = name,
                        .node = stmt,
                        .kind = .macro,
                    });

                    // Also register in global macro registry
                    try self.macro_registry.registerFromNode(stmt);
                },
                .function_decl => {
                    const name = stmt.data.function_decl.name;
                    try module.exports.put(name, .{
                        .name = name,
                        .node = stmt,
                        .kind = .function,
                    });
                },
                .class_decl => {
                    const name = stmt.data.class_decl.name;
                    try module.exports.put(name, .{
                        .name = name,
                        .node = stmt,
                        .kind = .class,
                    });
                },
                .export_decl => {
                    // Handle export statements
                    try self.processExport(module, stmt);
                },
                .import_decl => {
                    // Track imports for dependency resolution
                    const imp = &stmt.data.import_decl;
                    var symbols = std.ArrayList([]const u8).init(self.allocator);
                    for (imp.specifiers) |spec| {
                        try symbols.append(spec.local);
                    }
                    try module.imports.append(.{
                        .specifier = imp.source,
                        .resolved_path = try self.resolver.resolve(imp.source, module.path),
                        .symbols = try symbols.toOwnedSlice(),
                    });
                },
                else => {},
            }
        }
    }

    fn processExport(self: *ModuleLoader, module: *Module, export_node: *ast.Node) !void {
        const exp = &export_node.data.export_decl;

        if (exp.declaration) |decl| {
            // export function foo() / export class Bar
            switch (decl.kind) {
                .function_decl => {
                    const name = decl.data.function_decl.name;
                    try module.exports.put(name, .{
                        .name = name,
                        .node = decl,
                        .kind = .function,
                    });
                },
                .class_decl => {
                    const name = decl.data.class_decl.name;
                    try module.exports.put(name, .{
                        .name = name,
                        .node = decl,
                        .kind = .class,
                    });
                },
                .macro_decl => {
                    const name = decl.data.macro_decl.name;
                    try module.macros.put(name, decl);
                    try module.exports.put(name, .{
                        .name = name,
                        .node = decl,
                        .kind = .macro,
                    });
                    try self.macro_registry.registerFromNode(decl);
                },
                else => {},
            }
        }
    }

    /// Look up a macro by name across all loaded modules
    pub fn findMacro(self: *ModuleLoader, name: []const u8) ?*source_registry.SourceMacro {
        return self.macro_registry.get(name);
    }

    /// Get the definition location of a symbol for LSP go-to-definition
    pub fn getSymbolDefinition(self: *ModuleLoader, symbol_name: []const u8) ?struct {
        path: []const u8,
        node: *ast.Node,
    } {
        var it = self.modules.valueIterator();
        while (it.next()) |module| {
            if (module.*.exports.get(symbol_name)) |exp| {
                return .{
                    .path = module.*.path,
                    .node = exp.node,
                };
            }
        }
        return null;
    }
};

// =============================================================================
// Tests
// =============================================================================

test "module loader initialization" {
    var arena = ast.ASTArena.init(std.testing.allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(std.testing.allocator, &arena);
    defer loader.deinit();

    try std.testing.expectEqual(@as(usize, 0), loader.modules.count());
}
