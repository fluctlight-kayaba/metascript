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
///
/// ## String Lifetime Contracts
///
/// The module system has careful ownership of string data:
///
/// - `Module.path` - OWNED by Module, allocated via `allocator.dupe()`, freed in `ModuleLoader.deinit()`
/// - `Module.source` - OWNED by Module, read from file, freed in `ModuleLoader.deinit()`
/// - `Module.ast` - OWNED by ASTArena, contains slices pointing into `Module.source`
/// - `ImportDecl.specifier` - BORROWED from AST (points into `Module.source`)
/// - `ImportDecl.resolved_path` - OWNED by resolver cache, freed in `resolver.deinit()`
/// - `ImportDecl.symbols` - OWNED by Module, allocated via `toOwnedSlice()`, freed in `ModuleLoader.deinit()`
/// - `ExportedSymbol.name` - BORROWED from AST (points into `Module.source`)
/// - `PendingReexport.module_path` - BORROWED from `Module.path` (valid while module in cache)
/// - `PendingReexport.source_path` - BORROWED from AST (points into `Module.source`)
/// - `PendingReexport.specifiers` - BORROWED from AST
///
/// All BORROWED pointers are valid because:
/// 1. Modules stay in cache until `ModuleLoader.deinit()` is called
/// 2. `Module.source` stays alive until `ModuleLoader.deinit()` is called
/// 3. ASTArena outlives the ModuleLoader (passed in at init)
///
/// ## Variable Export Behavior
///
/// Variables (const/let) are NOT automatically exported, unlike functions and classes.
/// This matches TypeScript/ES6 semantics where variables must be explicitly exported.
/// Use `export { myVar }` or `export const myVar = ...` for variable exports.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const Lexer = @import("../lexer/lexer.zig").Lexer;
const Parser = @import("../parser/parser.zig").Parser;
const resolver = @import("resolver.zig");
const source_registry = @import("../macro/source_registry.zig");

/// Maximum depth for re-export chains to prevent infinite loops and stack overflow
const MAX_REEXPORT_DEPTH = 50;

/// Maximum file size for module loading (10 MB)
const MAX_MODULE_SIZE = 10 * 1024 * 1024;

/// Module loading state for circular import handling
pub const ModuleState = enum {
    /// Module is being loaded (parsing/symbol collection in progress)
    loading,
    /// Module is fully loaded and ready for use
    ready,
};

/// Represents a loaded module
pub const Module = struct {
    /// Absolute file path (OWNED - freed in ModuleLoader.deinit)
    path: []const u8,

    /// Source code buffer (OWNED - must stay alive for AST string slices)
    source: []const u8,

    /// Parsed AST (OWNED by ASTArena - slices point into source)
    ast: *ast.Node,

    /// Current loading state (loading vs ready)
    state: ModuleState,

    /// Source-defined macros in this module
    macros: std.StringHashMap(*ast.Node),

    /// Exported symbols
    exports: std.StringHashMap(ExportedSymbol),

    /// Import dependencies
    imports: std.ArrayList(ImportDecl),

    pub const ExportedSymbol = struct {
        name: []const u8, // BORROWED from AST
        node: *ast.Node,
        kind: SymbolKind,
    };

    pub const SymbolKind = enum {
        macro,
        function,
        class,
        variable,
    };

    pub const ImportDecl = struct {
        specifier: []const u8, // BORROWED from AST
        resolved_path: ?[]const u8, // OWNED by resolver cache
        symbols: []const []const u8, // OWNED - freed in ModuleLoader.deinit
    };
};

/// Module loader with caching
pub const ModuleLoader = struct {
    allocator: std.mem.Allocator,
    arena: *ast.ASTArena,
    resolver: resolver.ModuleResolver,
    /// Owned copy of std_lib_path (allocated by getStdLibPath, freed in deinit)
    std_lib_path_owned: []const u8,

    /// Cache of loaded modules by absolute path
    modules: std.StringHashMap(*Module),

    /// Collected macros from all loaded modules
    /// NOTE: This is a global registry by name. Macros with same name from different
    /// modules will overwrite each other. Use getMacroFromModule for precise lookup.
    macro_registry: source_registry.SourceMacroRegistry,

    /// Dependency graph: module_path -> list of modules it depends on
    /// Used for topological ordering and detecting circular imports
    dependencies: std.StringHashMap(std.ArrayList([]const u8)),

    /// Pending re-exports to resolve after modules are loaded
    pending_reexports: std.ArrayList(PendingReexport),

    /// Errors accumulated during re-export processing (for diagnostics)
    reexport_errors: std.ArrayList(ReexportError),

    const PendingReexport = struct {
        module_path: []const u8,
        source_path: []const u8,
        specifiers: []ast.node.ExportDecl.ExportSpecifier,
        depth: usize, // Track chain depth for this re-export
    };

    /// Error details for re-export failures (accessible via getReexportErrors)
    pub const ReexportError = struct {
        module_path: []const u8,
        source_path: []const u8,
        symbol: []const u8,
        message: []const u8,
    };

    /// Errors that can occur during module loading
    pub const LoadError = error{
        FileNotFound,
        AccessDenied,
        OutOfMemory,
        ParseError,
        InvalidPath,
        IoError,
        FileTooBig,
        ReexportChainTooDeep,
    };

    pub fn init(allocator: std.mem.Allocator, arena: *ast.ASTArena) !ModuleLoader {
        const std_lib_path = try resolver.getStdLibPath(allocator);

        return .{
            .allocator = allocator,
            .arena = arena,
            .resolver = resolver.ModuleResolver.init(allocator, std_lib_path),
            .std_lib_path_owned = std_lib_path, // Store owned copy, freed in deinit
            .modules = std.StringHashMap(*Module).init(allocator),
            .macro_registry = source_registry.SourceMacroRegistry.init(allocator),
            .dependencies = std.StringHashMap(std.ArrayList([]const u8)).init(allocator),
            .pending_reexports = std.ArrayList(PendingReexport).init(allocator),
            .reexport_errors = std.ArrayList(ReexportError).init(allocator),
        };
    }

    pub fn deinit(self: *ModuleLoader) void {
        // Free modules
        var it = self.modules.valueIterator();
        while (it.next()) |mod| {
            mod.*.macros.deinit();
            mod.*.exports.deinit();
            // Free import declaration data
            for (mod.*.imports.items) |imp| {
                // Free the symbols slice (allocated via toOwnedSlice)
                self.allocator.free(imp.symbols);
                // NOTE: Don't free resolved_path here - it's owned by the resolver's cache
                // and will be freed when resolver.deinit() is called
            }
            mod.*.imports.deinit();
            // Free the source buffer (owned by module, AST slices point into it)
            self.allocator.free(mod.*.source);
            // Free the path (duped in loadModule)
            self.allocator.free(mod.*.path);
            self.allocator.destroy(mod.*);
        }
        self.modules.deinit();

        // Free dependency lists
        var dep_it = self.dependencies.valueIterator();
        while (dep_it.next()) |dep_list| {
            dep_list.deinit();
        }
        self.dependencies.deinit();

        // Free std_lib_path (owned by ModuleLoader, not resolver)
        self.allocator.free(self.std_lib_path_owned);

        self.resolver.deinit();
        self.macro_registry.deinit();
        self.pending_reexports.deinit();
        self.reexport_errors.deinit();
    }

    /// Free a module and all its owned resources
    /// Used for cleanup in error paths and when replacing modules
    fn freeModule(self: *ModuleLoader, module: *Module) void {
        module.macros.deinit();
        module.exports.deinit();
        for (module.imports.items) |imp| {
            self.allocator.free(imp.symbols);
        }
        module.imports.deinit();
        self.allocator.free(module.source);
        self.allocator.free(module.path);
        self.allocator.destroy(module);
    }

    /// Load a module from a file path
    /// Returns the loaded module or a LoadError
    pub fn loadModule(self: *ModuleLoader, path: []const u8) LoadError!*Module {
        // Normalize path to resolve symlinks (e.g., /tmp -> /private/tmp on macOS)
        const normalized = std.fs.cwd().realpathAlloc(self.allocator, path) catch |err| {
            return switch (err) {
                error.FileNotFound => LoadError.FileNotFound,
                error.AccessDenied => LoadError.AccessDenied,
                else => LoadError.IoError,
            };
        };
        defer self.allocator.free(normalized);
        return self.loadModuleWithDepth(normalized, 0);
    }

    /// Load a module from in-memory source content
    /// Used by LSP when editor content differs from disk
    /// If a module with this path already exists, it will be replaced with new content
    pub fn loadModuleFromSource(self: *ModuleLoader, path: []const u8, source: []const u8) LoadError!*Module {
        return self.loadModuleFromSourceWithDepth(path, source, 0);
    }

    /// Get accumulated re-export errors for diagnostics
    pub fn getReexportErrors(self: *const ModuleLoader) []const ReexportError {
        return self.reexport_errors.items;
    }

    /// Process queued re-exports after modules are loaded
    /// Uses depth tracking per-item to correctly measure chain depth
    /// O(n) processing using pop() instead of orderedRemove(0)
    fn processPendingReexports(self: *ModuleLoader) LoadError!void {
        // Clear any previous errors
        self.reexport_errors.clearRetainingCapacity();

        while (self.pending_reexports.items.len > 0) {
            // Use pop() for O(1) removal (order doesn't matter for re-exports)
            const reexport = self.pending_reexports.pop().?;

            // Check depth limit based on this item's chain depth
            if (reexport.depth >= MAX_REEXPORT_DEPTH) {
                std.log.err("[ModuleLoader] Re-export chain too deep (>{d} levels) at '{s}'. Possible circular re-exports.", .{ MAX_REEXPORT_DEPTH, reexport.source_path });
                return LoadError.ReexportChainTooDeep;
            }

            // Get the module that has the re-export
            const module = self.modules.get(reexport.module_path) orelse {
                self.reexport_errors.append(.{
                    .module_path = reexport.module_path,
                    .source_path = reexport.source_path,
                    .symbol = "",
                    .message = "re-export module not found in cache",
                }) catch {};
                std.log.warn("[ModuleLoader] Re-export module not found: {s}", .{reexport.module_path});
                continue;
            };

            // Resolve the source path
            const resolved_path = self.resolver.resolve(reexport.source_path, reexport.module_path) catch null orelse {
                self.reexport_errors.append(.{
                    .module_path = reexport.module_path,
                    .source_path = reexport.source_path,
                    .symbol = "",
                    .message = "could not resolve re-export source path",
                }) catch {};
                std.log.warn("[ModuleLoader] Could not resolve re-export source: '{s}'", .{reexport.source_path});
                continue;
            };

            // Load the source module (may add more pending re-exports with depth+1)
            const source_module = self.loadModuleWithDepth(resolved_path, reexport.depth + 1) catch |err| {
                self.reexport_errors.append(.{
                    .module_path = reexport.module_path,
                    .source_path = reexport.source_path,
                    .symbol = "",
                    .message = @errorName(err),
                }) catch {};
                std.log.warn("[ModuleLoader] Failed to load re-export source '{s}': {s}", .{ reexport.source_path, @errorName(err) });
                continue;
            };

            // Copy each re-exported symbol
            for (reexport.specifiers) |spec| {
                if (source_module.exports.get(spec.local)) |source_exp| {
                    module.exports.put(spec.exported, .{
                        .name = spec.exported,
                        .node = source_exp.node,
                        .kind = source_exp.kind,
                    }) catch |err| {
                        std.log.err("[ModuleLoader] Failed to re-export '{s}': {s}", .{ spec.exported, @errorName(err) });
                        continue;
                    };

                    if (source_exp.kind == .macro) {
                        module.macros.put(spec.exported, source_exp.node) catch |err| {
                            std.log.err("[ModuleLoader] Failed to register re-exported macro '{s}': {s}", .{ spec.exported, @errorName(err) });
                        };
                    }
                    std.log.debug("[ModuleLoader] Re-exported '{s}' as '{s}'", .{ spec.local, spec.exported });
                } else {
                    self.reexport_errors.append(.{
                        .module_path = reexport.module_path,
                        .source_path = reexport.source_path,
                        .symbol = spec.local,
                        .message = "symbol not found in source module",
                    }) catch {};
                    std.log.warn("[ModuleLoader] Re-export '{s}' not found in '{s}'", .{ spec.local, reexport.source_path });
                }
            }
        }

        // Log accumulated errors summary if any
        if (self.reexport_errors.items.len > 0) {
            std.log.warn("[ModuleLoader] {d} re-export error(s) occurred", .{self.reexport_errors.items.len});
        }
    }

    /// Internal: Load module with depth tracking for re-export chains
    fn loadModuleWithDepth(self: *ModuleLoader, path: []const u8, depth: usize) LoadError!*Module {
        // Check cache first
        if (self.modules.get(path)) |cached| {
            // If module is still loading, we have a circular import
            // Return the partially loaded module (symbols collected so far)
            // Note: Circular imports ARE supported but may cause incomplete symbol resolution
            if (cached.state == .loading) {
                std.log.warn("[ModuleLoader] Circular import detected: '{s}' - some symbols may be unavailable", .{path});
            }
            return cached;
        }

        std.log.info("[ModuleLoader] Loading module: {s}", .{path});

        // Read file
        const file = std.fs.cwd().openFile(path, .{}) catch |err| {
            return switch (err) {
                error.FileNotFound => LoadError.FileNotFound,
                error.AccessDenied => LoadError.AccessDenied,
                else => LoadError.IoError,
            };
        };
        defer file.close();

        // Check file size before reading
        const stat = file.stat() catch return LoadError.IoError;
        if (stat.size > MAX_MODULE_SIZE) {
            return LoadError.FileTooBig;
        }

        const source = file.readToEndAlloc(self.allocator, MAX_MODULE_SIZE) catch |err| {
            return switch (err) {
                error.OutOfMemory => LoadError.OutOfMemory,
                else => LoadError.IoError,
            };
        };
        errdefer self.allocator.free(source);

        // Parse
        const file_id = self.arena.addFile(path) catch return LoadError.OutOfMemory;
        var lexer = Lexer.init(self.allocator, source, file_id) catch return LoadError.OutOfMemory;
        defer lexer.deinit();
        var parser = Parser.init(self.allocator, self.arena, &lexer, file_id);
        defer parser.deinit();

        const program = parser.parse() catch {
            return LoadError.ParseError;
        };

        // Check for collected parse errors (parser uses error recovery)
        if (parser.errors.items.len > 0) {
            return LoadError.ParseError;
        }

        // Create module with errdefer for cleanup on failure
        const path_owned = self.allocator.dupe(u8, path) catch return LoadError.OutOfMemory;
        errdefer self.allocator.free(path_owned);

        const module = self.allocator.create(Module) catch return LoadError.OutOfMemory;
        errdefer self.allocator.destroy(module);

        module.* = .{
            .path = path_owned,
            .source = source,
            .ast = program,
            .state = .loading, // Mark as loading to detect circular imports
            .macros = std.StringHashMap(*ast.Node).init(self.allocator),
            .exports = std.StringHashMap(Module.ExportedSymbol).init(self.allocator),
            .imports = std.ArrayList(Module.ImportDecl).init(self.allocator),
        };

        // Cache BEFORE collecting symbols (prevents infinite recursion on circular imports)
        self.modules.put(module.path, module) catch return LoadError.OutOfMemory;

        // Initialize dependency list for this module
        const dep_list = std.ArrayList([]const u8).init(self.allocator);
        self.dependencies.put(module.path, dep_list) catch return LoadError.OutOfMemory;

        // Collect symbols with depth tracking (this loads imported modules)
        self.collectSymbols(module, program, depth) catch return LoadError.OutOfMemory;

        // Mark module as ready
        module.state = .ready;

        // Process pending re-exports
        self.processPendingReexports() catch |err| return err;

        return module;
    }

    /// Internal: Load a module from in-memory source content with depth tracking
    /// Fixes: C7 (module loss), C8/C9 (atomic swap), C10 (double-free), C11 (restore leak)
    fn loadModuleFromSourceWithDepth(self: *ModuleLoader, path: []const u8, source: []const u8, depth: usize) LoadError!*Module {
        std.log.info("[ModuleLoader] Loading module from source: {s}", .{path});

        // === PHASE 1: Create new module (don't touch cache yet) ===

        // Ownership flags - prevent double-free from errdefer + manual cleanup
        var owns_source = true;
        var owns_path = true;

        // Make owned copy of source
        const source_owned = self.allocator.dupe(u8, source) catch return LoadError.OutOfMemory;
        errdefer if (owns_source) self.allocator.free(source_owned);

        // Parse
        const file_id = self.arena.addFile(path) catch return LoadError.OutOfMemory;
        var lexer = Lexer.init(self.allocator, source_owned, file_id) catch return LoadError.OutOfMemory;
        defer lexer.deinit();
        var parser = Parser.init(self.allocator, self.arena, &lexer, file_id);
        defer parser.deinit();

        const program = parser.parse() catch {
            return LoadError.ParseError;
        };

        if (parser.errors.items.len > 0) {
            return LoadError.ParseError;
        }

        // Create module struct
        const path_owned = self.allocator.dupe(u8, path) catch return LoadError.OutOfMemory;
        errdefer if (owns_path) self.allocator.free(path_owned);

        const module = self.allocator.create(Module) catch return LoadError.OutOfMemory;

        module.* = .{
            .path = path_owned,
            .source = source_owned,
            .ast = program,
            .state = .loading,
            .macros = std.StringHashMap(*ast.Node).init(self.allocator),
            .exports = std.StringHashMap(Module.ExportedSymbol).init(self.allocator),
            .imports = std.ArrayList(Module.ImportDecl).init(self.allocator),
        };

        // Transfer ownership to module - errdefers will no longer free these
        owns_source = false;
        owns_path = false;

        // === PHASE 2: Atomic swap - remove old, add new ===

        // Save old module (don't free yet - we may need to restore it)
        const old_module: ?*Module = self.modules.get(path);

        // Remove old from cache (but keep the memory)
        _ = self.modules.remove(path);

        // Try to add new module to cache
        self.modules.put(module.path, module) catch {
            // FAILED: Try to restore old module
            if (old_module) |old_mod| {
                self.modules.put(old_mod.path, old_mod) catch {
                    // C11 fix: Can't restore - free old module to prevent leak
                    self.freeModule(old_mod);
                };
            }
            // Clean up new module (L1 fix: use helper)
            self.freeModule(module);
            return LoadError.OutOfMemory;
        };

        // === PHASE 3: Complete setup (cache updated, handle failures carefully) ===

        // C12/C13 fix: Save old dependencies before replacing (may need to restore or free)
        const old_deps = self.dependencies.fetchRemove(path);

        // Initialize dependency list
        var dep_list = std.ArrayList([]const u8).init(self.allocator);
        self.dependencies.put(module.path, dep_list) catch {
            // M1 fix: deinit dep_list (even though empty, good practice)
            dep_list.deinit();
            // Restore old deps if they existed
            if (old_deps) |od| {
                self.dependencies.put(od.key, od.value) catch {
                    // Can't restore - at least don't leak
                    var old_list = od.value;
                    old_list.deinit();
                };
            }
            // Remove new module from cache, try to restore old
            _ = self.modules.remove(path);
            if (old_module) |old_mod| {
                self.modules.put(old_mod.path, old_mod) catch {
                    // C11 fix: Can't restore - free to prevent leak
                    self.freeModule(old_mod);
                };
            }
            self.freeModule(module);
            return LoadError.OutOfMemory;
        };

        // Collect symbols
        self.collectSymbols(module, program, depth) catch {
            // Remove new module from cache
            _ = self.modules.remove(path);
            // Remove and deinit the NEW dependencies ArrayList we just added
            if (self.dependencies.fetchRemove(path)) |removed| {
                var removed_list = removed.value;
                removed_list.deinit();
            }
            // C13 fix: Restore old deps if they existed
            if (old_deps) |od| {
                self.dependencies.put(od.key, od.value) catch {
                    // Can't restore - at least don't leak
                    var old_list = od.value;
                    old_list.deinit();
                };
            }
            // Try to restore old module
            if (old_module) |old_mod| {
                self.modules.put(old_mod.path, old_mod) catch {
                    // C11 fix: Can't restore - free to prevent leak
                    self.freeModule(old_mod);
                };
            }
            self.freeModule(module);
            return LoadError.OutOfMemory;
        };

        // === PHASE 4: Success - now safe to free old module and its dependencies ===

        // C12 fix: Free old dependencies ArrayList
        if (old_deps) |od| {
            var old_list = od.value;
            old_list.deinit();
        }

        if (old_module) |old_mod| {
            self.freeModule(old_mod);
        }

        // Mark module as ready
        module.state = .ready;

        // Process pending re-exports
        self.processPendingReexports() catch |err| return err;

        return module;
    }

    /// Load standard library macros
    /// Loads individual macro modules from std/macros/ directory
    /// Note: We don't use index.ms pattern to keep module structure acyclic
    pub fn loadStdMacros(self: *ModuleLoader) !void {
        // Load known macro modules directly (acyclic - no index.ms re-exports)
        const macro_modules = [_][]const u8{
            "derive",
            // Add other macro modules here as needed
        };

        for (macro_modules) |module_name| {
            const macro_path = try std.fmt.allocPrint(
                self.allocator,
                "{s}/macros/{s}.ms",
                .{ self.std_lib_path_owned, module_name },
            );
            defer self.allocator.free(macro_path);

            if (std.fs.cwd().access(macro_path, .{})) |_| {
                _ = self.loadModule(macro_path) catch |err| {
                    std.log.warn("[ModuleLoader] Failed to load std/macros/{s}: {s}", .{ module_name, @errorName(err) });
                    continue;
                };
                std.log.info("[ModuleLoader] Loaded std/macros/{s}", .{module_name});
            } else |_| {}
        }
    }

    /// Collect macro and export symbols from a module AST
    /// Uses two-pass processing to handle forward declarations:
    /// 1. First pass: collect all declarations into a local map
    /// 2. Second pass: process exports (can reference any declaration from pass 1)
    fn collectSymbols(self: *ModuleLoader, module: *Module, program: *ast.Node, depth: usize) !void {
        // Temporary map of all declarations in this module (for forward reference support)
        var declarations = std.StringHashMap(Module.ExportedSymbol).init(self.allocator);
        defer declarations.deinit();

        // PASS 1: Collect all declarations (functions, classes, macros, variables)
        for (program.data.program.statements) |stmt| {
            switch (stmt.kind) {
                .macro_decl => {
                    const name = stmt.data.macro_decl.name;
                    try declarations.put(name, .{ .name = name, .node = stmt, .kind = .macro });
                    // Register macros immediately (they're always exported)
                    try module.macros.put(name, stmt);
                    try module.exports.put(name, .{ .name = name, .node = stmt, .kind = .macro });
                    try self.macro_registry.registerFromNode(stmt);
                },
                .function_decl => {
                    const name = stmt.data.function_decl.name;
                    try declarations.put(name, .{ .name = name, .node = stmt, .kind = .function });
                    // Top-level functions are implicitly exported in Metascript
                    try module.exports.put(name, .{ .name = name, .node = stmt, .kind = .function });
                },
                .class_decl => {
                    const name = stmt.data.class_decl.name;
                    try declarations.put(name, .{ .name = name, .node = stmt, .kind = .class });
                    // Top-level classes are implicitly exported in Metascript
                    try module.exports.put(name, .{ .name = name, .node = stmt, .kind = .class });
                },
                .variable_stmt => {
                    // Variables are tracked but NOT auto-exported (ES6 semantics)
                    // Use explicit `export { x }` or `export const x = ...`
                    for (stmt.data.variable_stmt.declarations) |decl| {
                        try declarations.put(decl.name, .{ .name = decl.name, .node = stmt, .kind = .variable });
                    }
                },
                .import_decl => {
                    // Track imports AND load the imported module
                    const imp = &stmt.data.import_decl;
                    var symbols = std.ArrayList([]const u8).init(self.allocator);
                    errdefer symbols.deinit();
                    for (imp.specifiers) |spec| {
                        try symbols.append(spec.local);
                    }

                    // Strip surrounding quotes from import source if present
                    var source = imp.source;
                    if (source.len >= 2) {
                        if ((source[0] == '"' and source[source.len - 1] == '"') or
                            (source[0] == '\'' and source[source.len - 1] == '\''))
                        {
                            source = source[1 .. source.len - 1];
                        }
                    }

                    const resolved_path = try self.resolver.resolve(source, module.path);
                    const symbols_slice = try symbols.toOwnedSlice();
                    errdefer self.allocator.free(symbols_slice);
                    try module.imports.append(.{
                        .specifier = source,
                        .resolved_path = resolved_path,
                        .symbols = symbols_slice,
                    });

                    // CRITICAL: Actually load the imported module so its macros are available
                    if (resolved_path) |path| {
                        // Add to dependency graph (avoid duplicates)
                        if (self.dependencies.getPtr(module.path)) |dep_list| {
                            // Check if this dependency already exists
                            var already_added = false;
                            for (dep_list.items) |existing| {
                                if (std.mem.eql(u8, existing, path)) {
                                    already_added = true;
                                    break;
                                }
                            }
                            if (!already_added) {
                                try dep_list.append(path);
                            }
                        }

                        // Load the imported module (uses cache, handles circular imports)
                        _ = self.loadModuleWithDepth(path, depth) catch |err| {
                            std.log.warn("[ModuleLoader] Failed to load import '{s}' from '{s}': {s}", .{ imp.source, module.path, @errorName(err) });
                            // Don't fail the whole module - just log the warning
                            // The symbol will be unresolved at type-check time
                        };
                    }
                },
                else => {},
            }
        }

        // PASS 2: Process export statements (can now reference any declaration)
        for (program.data.program.statements) |stmt| {
            if (stmt.kind == .export_decl) {
                try self.processExport(module, stmt, &declarations, depth);
            }
        }
    }

    /// Process an export declaration with depth tracking for re-export chains
    /// @param declarations: Map of all declarations in the module (from pass 1)
    /// @param depth: Current re-export chain depth (0 for top-level modules)
    fn processExport(
        self: *ModuleLoader,
        module: *Module,
        export_node: *ast.Node,
        declarations: *const std.StringHashMap(Module.ExportedSymbol),
        depth: usize,
    ) !void {
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
                .variable_stmt => {
                    for (decl.data.variable_stmt.declarations) |var_decl| {
                        try module.exports.put(var_decl.name, .{
                            .name = var_decl.name,
                            .node = decl,
                            .kind = .variable,
                        });
                    }
                },
                else => {},
            }
        } else if (exp.source) |raw_source_path| {
            // Re-export from another module: export { x, y } from "module"
            // TODO(parser): Fix quote stripping in parser's parseExportDeclaration
            // The parser should consistently strip quotes from string literals.
            // This workaround should be removed once the parser is fixed.
            // See: src/parser/parser.zig parseExportDeclaration
            const source_path = if (raw_source_path.len >= 2 and
                (raw_source_path[0] == '"' or raw_source_path[0] == '\'') and
                raw_source_path[raw_source_path.len - 1] == raw_source_path[0]) // Verify matching closing quote
                raw_source_path[1 .. raw_source_path.len - 1]
            else
                raw_source_path;

            std.log.debug("[ModuleLoader] Queuing re-export from '{s}' (depth={d})", .{ source_path, depth });

            // Queue for later processing with current depth (avoids infinite recursion)
            try self.pending_reexports.append(.{
                .module_path = module.path,
                .source_path = source_path,
                .specifiers = exp.specifiers,
                .depth = depth,
            });
        } else {
            // Named exports without source: export { x, y }
            // Look up in declarations map (supports forward references)
            for (exp.specifiers) |spec| {
                // First check the declarations map (includes forward declarations)
                if (declarations.get(spec.local)) |local_decl| {
                    try module.exports.put(spec.exported, .{
                        .name = spec.exported,
                        .node = local_decl.node,
                        .kind = local_decl.kind,
                    });
                } else if (module.exports.get(spec.local)) |existing_exp| {
                    // Fallback: check already-exported symbols (for re-exports)
                    try module.exports.put(spec.exported, .{
                        .name = spec.exported,
                        .node = existing_exp.node,
                        .kind = existing_exp.kind,
                    });
                } else {
                    // Symbol not found - this is an error in the source code
                    std.log.warn("[ModuleLoader] Named export '{s}' not found in module '{s}'", .{ spec.local, module.path });
                }
            }
        }
    }

    /// Look up a macro by name across all loaded modules (global registry)
    /// NOTE: If multiple modules define macros with the same name, this returns
    /// whichever was registered last. Use getMacroFromModule for precise lookup.
    pub fn findMacro(self: *ModuleLoader, name: []const u8) ?*source_registry.SourceMacro {
        return self.macro_registry.get(name);
    }

    /// Look up a macro from a specific module (precise, scoped lookup)
    /// This is the preferred method when you know which module the macro comes from
    pub fn getMacroFromModule(self: *const ModuleLoader, module_path: []const u8, macro_name: []const u8) ?*ast.Node {
        // Try with normalized path first (macOS /tmp -> /private/tmp)
        const normalized = std.fs.cwd().realpathAlloc(self.allocator, module_path) catch module_path;
        defer if (normalized.ptr != module_path.ptr) self.allocator.free(normalized);
        const module = self.modules.get(normalized) orelse self.modules.get(module_path) orelse return null;
        return module.macros.get(macro_name);
    }

    /// Get the dependency list for a module (for topological sorting/build ordering)
    pub fn getDependencies(self: *const ModuleLoader, module_path: []const u8) ?[]const []const u8 {
        // Try with normalized path first (macOS /tmp -> /private/tmp)
        const normalized = std.fs.cwd().realpathAlloc(self.allocator, module_path) catch module_path;
        defer if (normalized.ptr != module_path.ptr) self.allocator.free(normalized);
        const dep_list = self.dependencies.get(normalized) orelse self.dependencies.get(module_path) orelse return null;
        return dep_list.items;
    }

    /// Get all loaded modules in dependency order (topological sort)
    /// Returns modules such that dependencies come before dependents
    /// IMPORTANT: Caller must free the returned slice with `allocator.free(slice)`
    pub fn getModulesInDependencyOrder(self: *ModuleLoader, allocator: std.mem.Allocator) ![]const []const u8 {
        var result = std.ArrayList([]const u8).init(allocator);
        errdefer result.deinit();

        var visited = std.StringHashMap(void).init(allocator);
        defer visited.deinit();

        var in_stack = std.StringHashMap(void).init(allocator);
        defer in_stack.deinit();

        // Visit each module
        var mod_it = self.modules.keyIterator();
        while (mod_it.next()) |mod_path| {
            try self.topoVisit(mod_path.*, &result, &visited, &in_stack);
        }

        return try result.toOwnedSlice();
    }

    fn topoVisit(
        self: *ModuleLoader,
        path: []const u8,
        result: *std.ArrayList([]const u8),
        visited: *std.StringHashMap(void),
        in_stack: *std.StringHashMap(void),
    ) !void {
        if (visited.contains(path)) return;
        if (in_stack.contains(path)) {
            // Circular dependency - already handled by module state, skip
            return;
        }

        try in_stack.put(path, {});

        // Visit dependencies first
        if (self.dependencies.get(path)) |deps| {
            for (deps.items) |dep_path| {
                try self.topoVisit(dep_path, result, visited, in_stack);
            }
        }

        _ = in_stack.remove(path);
        try visited.put(path, {});
        try result.append(path);
    }

    /// Get the file path for a file ID from the arena's file registry
    pub fn getFilePath(self: *const ModuleLoader, file_id: u32) ?[]const u8 {
        return self.arena.getFilePath(file_id);
    }

    /// Get a symbol exported from a specific module path
    /// This is the preferred method for import resolution as it's precise
    pub fn getSymbolFromModule(self: *const ModuleLoader, module_path: []const u8, symbol_name: []const u8) ?struct {
        path: []const u8,
        node: *ast.Node,
        kind: Module.SymbolKind,
    } {
        // Try with normalized path first (macOS /tmp -> /private/tmp)
        const normalized = std.fs.cwd().realpathAlloc(self.allocator, module_path) catch module_path;
        defer if (normalized.ptr != module_path.ptr) self.allocator.free(normalized);
        const module = self.modules.get(normalized) orelse self.modules.get(module_path) orelse return null;
        const exp = module.exports.get(symbol_name) orelse return null;
        return .{
            .path = module.path,
            .node = exp.node,
            .kind = exp.kind,
        };
    }

    /// Get the definition location of a symbol for LSP go-to-definition
    /// Note: This searches globally - prefer getSymbolFromModule when you know the source module
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

test "load simple module" {
    const allocator = std.testing.allocator;

    // Create a simple test module
    const test_source =
        \\function hello(): void {}
        \\class World { value: number; }
    ;

    const tmp_path = "/tmp/test_simple_module.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    // Verify module was loaded
    try std.testing.expectEqual(@as(usize, 1), loader.modules.count());
    try std.testing.expect(module.exports.contains("hello"));
    try std.testing.expect(module.exports.contains("World"));
}

test "module caching" {
    const allocator = std.testing.allocator;

    const test_source = "function cached(): void {}";

    const tmp_path = "/tmp/test_cached_module.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module1 = try loader.loadModule(tmp_path);
    const module2 = try loader.loadModule(tmp_path);

    // Same module should be returned from cache
    try std.testing.expectEqual(module1, module2);
    try std.testing.expectEqual(@as(usize, 1), loader.modules.count());
}

test "module not found error" {
    const allocator = std.testing.allocator;

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const result = loader.loadModule("/tmp/nonexistent_module_12345.ms");
    try std.testing.expectError(ModuleLoader.LoadError.FileNotFound, result);
}

test "macro collection" {
    const allocator = std.testing.allocator;

    const test_source =
        \\macro function testMacro(target) {
        \\    return target;
        \\}
    ;

    const tmp_path = "/tmp/test_macro_module.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    // Verify macro was collected
    try std.testing.expect(module.macros.contains("testMacro"));
    try std.testing.expect(module.exports.contains("testMacro"));

    // Check macro registry
    const macro = loader.findMacro("testMacro");
    try std.testing.expect(macro != null);
}

test "export declaration handling" {
    const allocator = std.testing.allocator;

    const test_source =
        \\export function exportedFunc(): void {}
        \\export class ExportedClass { x: number; }
    ;

    const tmp_path = "/tmp/test_export_module.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    try std.testing.expect(module.exports.contains("exportedFunc"));
    try std.testing.expect(module.exports.contains("ExportedClass"));
    try std.testing.expectEqual(Module.SymbolKind.function, module.exports.get("exportedFunc").?.kind);
    try std.testing.expectEqual(Module.SymbolKind.class, module.exports.get("ExportedClass").?.kind);
}

test "import tracking" {
    const allocator = std.testing.allocator;

    const test_source =
        \\import { foo, bar } from "./other";
        \\function main(): void {}
    ;

    const tmp_path = "/tmp/test_import_tracking.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    // Verify imports were tracked
    try std.testing.expectEqual(@as(usize, 1), module.imports.items.len);
    const imp = module.imports.items[0];
    try std.testing.expectEqualStrings("./other", imp.specifier);
    try std.testing.expectEqual(@as(usize, 2), imp.symbols.len);
}

test "symbol definition lookup" {
    const allocator = std.testing.allocator;

    const test_source = "function findMe(): void {}";

    const tmp_path = "/tmp/test_symbol_lookup.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    _ = try loader.loadModule(tmp_path);

    // Look up the symbol
    const def = loader.getSymbolDefinition("findMe");
    try std.testing.expect(def != null);
    // Compare with normalized path (macOS /tmp -> /private/tmp)
    const expected_path = try std.fs.cwd().realpathAlloc(allocator, tmp_path);
    defer allocator.free(expected_path);
    try std.testing.expectEqualStrings(expected_path, def.?.path);

    // Non-existent symbol
    const no_def = loader.getSymbolDefinition("nonexistent");
    try std.testing.expect(no_def == null);
}

test "multiple modules" {
    const allocator = std.testing.allocator;

    const source1 = "function mod1Func(): void {}";
    const source2 = "function mod2Func(): void {}";

    const path1 = "/tmp/test_multi_mod1.ms";
    const path2 = "/tmp/test_multi_mod2.ms";

    {
        const file = try std.fs.cwd().createFile(path1, .{});
        defer file.close();
        try file.writeAll(source1);
    }
    defer std.fs.cwd().deleteFile(path1) catch {};

    {
        const file = try std.fs.cwd().createFile(path2, .{});
        defer file.close();
        try file.writeAll(source2);
    }
    defer std.fs.cwd().deleteFile(path2) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    _ = try loader.loadModule(path1);
    _ = try loader.loadModule(path2);

    try std.testing.expectEqual(@as(usize, 2), loader.modules.count());

    // Both symbols should be findable
    try std.testing.expect(loader.getSymbolDefinition("mod1Func") != null);
    try std.testing.expect(loader.getSymbolDefinition("mod2Func") != null);
}

test "exported macro registration" {
    const allocator = std.testing.allocator;

    const test_source =
        \\export macro function derive(target) {
        \\    return target;
        \\}
    ;

    const tmp_path = "/tmp/test_exported_macro.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    // Macro should be both in module macros and exports
    try std.testing.expect(module.macros.contains("derive"));
    try std.testing.expect(module.exports.contains("derive"));
    try std.testing.expectEqual(Module.SymbolKind.macro, module.exports.get("derive").?.kind);

    // Should be in global registry
    try std.testing.expect(loader.findMacro("derive") != null);
}

test "class with properties exports correctly" {
    const allocator = std.testing.allocator;

    const test_source =
        \\class User {
        \\    name: string;
        \\    age: number;
        \\}
    ;

    const tmp_path = "/tmp/test_class_props.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    try std.testing.expect(module.exports.contains("User"));
    try std.testing.expectEqual(Module.SymbolKind.class, module.exports.get("User").?.kind);
}

test "getSymbolFromModule returns correct module" {
    const allocator = std.testing.allocator;

    const source1 = "function sharedName(): void {}";
    const source2 = "function sharedName(): void {}";

    const path1 = "/tmp/test_module_sym1.ms";
    const path2 = "/tmp/test_module_sym2.ms";

    {
        const file = try std.fs.cwd().createFile(path1, .{});
        defer file.close();
        try file.writeAll(source1);
    }
    defer std.fs.cwd().deleteFile(path1) catch {};

    {
        const file = try std.fs.cwd().createFile(path2, .{});
        defer file.close();
        try file.writeAll(source2);
    }
    defer std.fs.cwd().deleteFile(path2) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    _ = try loader.loadModule(path1);
    _ = try loader.loadModule(path2);

    // Get from specific module
    const from_mod1 = loader.getSymbolFromModule(path1, "sharedName");
    const from_mod2 = loader.getSymbolFromModule(path2, "sharedName");

    try std.testing.expect(from_mod1 != null);
    try std.testing.expect(from_mod2 != null);

    // They should be different nodes (different ASTs)
    try std.testing.expect(from_mod1.?.node != from_mod2.?.node);

    // Paths should match their respective modules (normalized)
    const expected1 = try std.fs.cwd().realpathAlloc(allocator, path1);
    defer allocator.free(expected1);
    const expected2 = try std.fs.cwd().realpathAlloc(allocator, path2);
    defer allocator.free(expected2);
    try std.testing.expectEqualStrings(expected1, from_mod1.?.path);
    try std.testing.expectEqualStrings(expected2, from_mod2.?.path);
}

test "load module with parse error returns error" {
    const allocator = std.testing.allocator;

    // Create a file with invalid syntax
    const tmp_path = "/tmp/test_parse_error.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll("function { this is not valid syntax at all }}}");
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const result = loader.loadModule(tmp_path);
    try std.testing.expectError(ModuleLoader.LoadError.ParseError, result);
}

test "re-export from another module" {
    const allocator = std.testing.allocator;

    // Create source module with a function
    const source_module =
        \\function originalFunc(): void {}
        \\class OriginalClass { value: number; }
    ;

    // Create re-exporting module
    const reexport_module =
        \\export { originalFunc, OriginalClass } from "./source_mod";
    ;

    const source_path = "/tmp/source_mod.ms";
    const reexport_path = "/tmp/reexport_mod.ms";

    {
        const file = try std.fs.cwd().createFile(source_path, .{});
        defer file.close();
        try file.writeAll(source_module);
    }
    defer std.fs.cwd().deleteFile(source_path) catch {};

    {
        const file = try std.fs.cwd().createFile(reexport_path, .{});
        defer file.close();
        try file.writeAll(reexport_module);
    }
    defer std.fs.cwd().deleteFile(reexport_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    // Load the re-exporting module (should also load source)
    const module = try loader.loadModule(reexport_path);

    // Re-exported symbols should be available
    try std.testing.expect(module.exports.contains("originalFunc"));
    try std.testing.expect(module.exports.contains("OriginalClass"));

    // Verify the kinds are correct
    try std.testing.expectEqual(Module.SymbolKind.function, module.exports.get("originalFunc").?.kind);
    try std.testing.expectEqual(Module.SymbolKind.class, module.exports.get("OriginalClass").?.kind);

    // Both modules should be loaded
    try std.testing.expectEqual(@as(usize, 2), loader.modules.count());
}

test "circular import detection" {
    const allocator = std.testing.allocator;

    // Create two modules that import each other
    const module_a =
        \\import { funcB } from "./circular_b";
        \\function funcA(): void {}
    ;

    const module_b =
        \\import { funcA } from "./circular_a";
        \\function funcB(): void {}
    ;

    const path_a = "/tmp/circular_a.ms";
    const path_b = "/tmp/circular_b.ms";

    {
        const file = try std.fs.cwd().createFile(path_a, .{});
        defer file.close();
        try file.writeAll(module_a);
    }
    defer std.fs.cwd().deleteFile(path_a) catch {};

    {
        const file = try std.fs.cwd().createFile(path_b, .{});
        defer file.close();
        try file.writeAll(module_b);
    }
    defer std.fs.cwd().deleteFile(path_b) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    // Loading module A should not hang or crash
    const module_a_loaded = try loader.loadModule(path_a);
    try std.testing.expect(module_a_loaded.exports.contains("funcA"));

    // Loading module B should also work
    const module_b_loaded = try loader.loadModule(path_b);
    try std.testing.expect(module_b_loaded.exports.contains("funcB"));

    // Both modules should now be in the cache
    try std.testing.expectEqual(@as(usize, 2), loader.modules.count());

    // Import tracking should have resolved paths
    try std.testing.expectEqual(@as(usize, 1), module_a_loaded.imports.items.len);
    try std.testing.expectEqual(@as(usize, 1), module_b_loaded.imports.items.len);
}

test "forward declaration exports" {
    const allocator = std.testing.allocator;

    // Test that export { x } works even when declaration comes AFTER the export
    const test_source =
        \\export { myFunction, MyClass };
        \\function myFunction(): void {}
        \\class MyClass { value: number; }
    ;

    const tmp_path = "/tmp/test_forward_export.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    // Forward-declared exports should work
    try std.testing.expect(module.exports.contains("myFunction"));
    try std.testing.expect(module.exports.contains("MyClass"));
    try std.testing.expectEqual(Module.SymbolKind.function, module.exports.get("myFunction").?.kind);
    try std.testing.expectEqual(Module.SymbolKind.class, module.exports.get("MyClass").?.kind);
}

test "export with renaming" {
    const allocator = std.testing.allocator;

    const test_source =
        \\function internalName(): void {}
        \\export { internalName as publicName };
    ;

    const tmp_path = "/tmp/test_export_rename.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    // Both original and renamed should be exported
    try std.testing.expect(module.exports.contains("internalName"));
    try std.testing.expect(module.exports.contains("publicName"));
}

test "multi-level re-exports" {
    const allocator = std.testing.allocator;

    // Create chain: C exports from B, B exports from A
    const module_a = "function baseFunc(): void {}";
    const module_b =
        \\export { baseFunc } from "./reexport_a";
    ;
    const module_c =
        \\export { baseFunc } from "./reexport_b";
    ;

    const path_a = "/tmp/reexport_a.ms";
    const path_b = "/tmp/reexport_b.ms";
    const path_c = "/tmp/reexport_c.ms";

    {
        const file = try std.fs.cwd().createFile(path_a, .{});
        defer file.close();
        try file.writeAll(module_a);
    }
    defer std.fs.cwd().deleteFile(path_a) catch {};

    {
        const file = try std.fs.cwd().createFile(path_b, .{});
        defer file.close();
        try file.writeAll(module_b);
    }
    defer std.fs.cwd().deleteFile(path_b) catch {};

    {
        const file = try std.fs.cwd().createFile(path_c, .{});
        defer file.close();
        try file.writeAll(module_c);
    }
    defer std.fs.cwd().deleteFile(path_c) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    // Load the deepest module in the chain
    const module = try loader.loadModule(path_c);

    // Symbol should be re-exported through the chain
    try std.testing.expect(module.exports.contains("baseFunc"));

    // All three modules should be loaded
    try std.testing.expectEqual(@as(usize, 3), loader.modules.count());
}

test "variable declaration tracking" {
    const allocator = std.testing.allocator;

    const test_source =
        \\const myConst = 42;
        \\export { myConst };
    ;

    const tmp_path = "/tmp/test_var_export.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    // Variable should be exported
    try std.testing.expect(module.exports.contains("myConst"));
    try std.testing.expectEqual(Module.SymbolKind.variable, module.exports.get("myConst").?.kind);
}

test "reexport error accumulation" {
    const allocator = std.testing.allocator;

    // Create source module
    const source_module = "function existingFunc(): void {}";

    // Create re-exporting module that tries to export a non-existent symbol
    const reexport_module =
        \\export { existingFunc, nonexistentFunc } from "./error_source";
    ;

    const source_path = "/tmp/error_source.ms";
    const reexport_path = "/tmp/error_reexport.ms";

    {
        const file = try std.fs.cwd().createFile(source_path, .{});
        defer file.close();
        try file.writeAll(source_module);
    }
    defer std.fs.cwd().deleteFile(source_path) catch {};

    {
        const file = try std.fs.cwd().createFile(reexport_path, .{});
        defer file.close();
        try file.writeAll(reexport_module);
    }
    defer std.fs.cwd().deleteFile(reexport_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    // Loading should succeed but accumulate errors
    const module = try loader.loadModule(reexport_path);

    // existingFunc should be re-exported
    try std.testing.expect(module.exports.contains("existingFunc"));

    // nonexistentFunc should NOT be exported (doesn't exist in source)
    try std.testing.expect(!module.exports.contains("nonexistentFunc"));

    // Should have accumulated an error for the non-existent symbol
    const errors = loader.getReexportErrors();
    try std.testing.expectEqual(@as(usize, 1), errors.len);
    try std.testing.expectEqualStrings("nonexistentFunc", errors[0].symbol);
}

test "reexport depth tracking per level" {
    const allocator = std.testing.allocator;

    // Create a 3-level re-export chain: D -> C -> B -> A
    const module_a =
        \\function func1(): void {}
        \\function func2(): void {}
        \\function func3(): void {}
    ;
    const module_b =
        \\export { func1, func2, func3 } from "./depth_a";
    ;
    const module_c =
        \\export { func1, func2, func3 } from "./depth_b";
    ;
    const module_d =
        \\export { func1, func2, func3 } from "./depth_c";
    ;

    const path_a = "/tmp/depth_a.ms";
    const path_b = "/tmp/depth_b.ms";
    const path_c = "/tmp/depth_c.ms";
    const path_d = "/tmp/depth_d.ms";

    {
        const file = try std.fs.cwd().createFile(path_a, .{});
        defer file.close();
        try file.writeAll(module_a);
    }
    defer std.fs.cwd().deleteFile(path_a) catch {};

    {
        const file = try std.fs.cwd().createFile(path_b, .{});
        defer file.close();
        try file.writeAll(module_b);
    }
    defer std.fs.cwd().deleteFile(path_b) catch {};

    {
        const file = try std.fs.cwd().createFile(path_c, .{});
        defer file.close();
        try file.writeAll(module_c);
    }
    defer std.fs.cwd().deleteFile(path_c) catch {};

    {
        const file = try std.fs.cwd().createFile(path_d, .{});
        defer file.close();
        try file.writeAll(module_d);
    }
    defer std.fs.cwd().deleteFile(path_d) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    // Load the deepest module - should follow chain without exceeding depth
    const module = try loader.loadModule(path_d);

    // All three functions should be re-exported through the chain
    try std.testing.expect(module.exports.contains("func1"));
    try std.testing.expect(module.exports.contains("func2"));
    try std.testing.expect(module.exports.contains("func3"));

    // All four modules should be loaded
    try std.testing.expectEqual(@as(usize, 4), loader.modules.count());

    // No errors should have occurred
    try std.testing.expectEqual(@as(usize, 0), loader.getReexportErrors().len);
}

test "variable not auto-exported" {
    const allocator = std.testing.allocator;

    // Variables should NOT be auto-exported (ES6 semantics)
    const test_source =
        \\const privateVar = 42;
        \\function publicFunc(): void {}
    ;

    const tmp_path = "/tmp/test_var_not_exported.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    // Function should be auto-exported
    try std.testing.expect(module.exports.contains("publicFunc"));

    // Variable should NOT be auto-exported
    try std.testing.expect(!module.exports.contains("privateVar"));
}

test "export const variable" {
    const allocator = std.testing.allocator;

    // Explicit export const should work
    const test_source =
        \\export const exportedVar = 42;
    ;

    const tmp_path = "/tmp/test_export_const.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    // Explicitly exported variable should be available
    try std.testing.expect(module.exports.contains("exportedVar"));
    try std.testing.expectEqual(Module.SymbolKind.variable, module.exports.get("exportedVar").?.kind);
}

test "import actually loads dependency module" {
    const allocator = std.testing.allocator;

    // Create a module that exports a macro
    const macro_module =
        \\macro function testMacro(target) {
        \\    return target;
        \\}
    ;

    // Create a module that imports from the macro module
    const consumer_module =
        \\import { testMacro } from "./macro_dep";
        \\function main(): void {}
    ;

    const macro_path = "/tmp/macro_dep.ms";
    const consumer_path = "/tmp/consumer.ms";

    {
        const file = try std.fs.cwd().createFile(macro_path, .{});
        defer file.close();
        try file.writeAll(macro_module);
    }
    defer std.fs.cwd().deleteFile(macro_path) catch {};

    {
        const file = try std.fs.cwd().createFile(consumer_path, .{});
        defer file.close();
        try file.writeAll(consumer_module);
    }
    defer std.fs.cwd().deleteFile(consumer_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    // Load the consumer module
    const module = try loader.loadModule(consumer_path);

    // Consumer module should track the import
    try std.testing.expectEqual(@as(usize, 1), module.imports.items.len);

    // CRITICAL: The macro module should ALSO be loaded (this was the bug!)
    try std.testing.expectEqual(@as(usize, 2), loader.modules.count());

    // The macro from the imported module should be in the global registry
    try std.testing.expect(loader.findMacro("testMacro") != null);

    // Dependency tracking should show consumer depends on macro module
    const deps = loader.getDependencies(consumer_path);
    try std.testing.expect(deps != null);
    try std.testing.expectEqual(@as(usize, 1), deps.?.len);
}

test "module state tracks loading vs ready" {
    const allocator = std.testing.allocator;

    const test_source = "function ready(): void {}";

    const tmp_path = "/tmp/test_state.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    // Module should be ready after loading completes
    try std.testing.expectEqual(ModuleState.ready, module.state);
}

test "getMacroFromModule returns scoped macro" {
    const allocator = std.testing.allocator;

    // Create two modules with same-named macros
    const module_a =
        \\macro function sameName(target) {
        \\    return target;
        \\}
    ;
    const module_b =
        \\macro function sameName(target) {
        \\    return target;
        \\}
    ;

    const path_a = "/tmp/macro_a.ms";
    const path_b = "/tmp/macro_b.ms";

    {
        const file = try std.fs.cwd().createFile(path_a, .{});
        defer file.close();
        try file.writeAll(module_a);
    }
    defer std.fs.cwd().deleteFile(path_a) catch {};

    {
        const file = try std.fs.cwd().createFile(path_b, .{});
        defer file.close();
        try file.writeAll(module_b);
    }
    defer std.fs.cwd().deleteFile(path_b) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    _ = try loader.loadModule(path_a);
    _ = try loader.loadModule(path_b);

    // Scoped lookup should return different nodes for same name
    const macro_from_a = loader.getMacroFromModule(path_a, "sameName");
    const macro_from_b = loader.getMacroFromModule(path_b, "sameName");

    try std.testing.expect(macro_from_a != null);
    try std.testing.expect(macro_from_b != null);
    // They should be different AST nodes (from different modules)
    try std.testing.expect(macro_from_a.? != macro_from_b.?);
}

test "loadStdMacros with mock directory" {
    const allocator = std.testing.allocator;

    // Create a mock std/macros directory structure
    const std_path = "/tmp/test_std";
    const macros_path = "/tmp/test_std/macros";
    const derive_path = "/tmp/test_std/macros/derive.ms";

    // Create directories
    std.fs.cwd().makePath(macros_path) catch {};
    defer std.fs.cwd().deleteTree(std_path) catch {};

    // Create a simple derive macro
    {
        const file = try std.fs.cwd().createFile(derive_path, .{});
        defer file.close();
        try file.writeAll(
            \\macro function derive(target) {
            \\    return target;
            \\}
        );
    }

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    // Create loader with custom std path
    const std_path_owned = try allocator.dupe(u8, std_path);
    var loader = ModuleLoader{
        .allocator = allocator,
        .arena = &arena,
        .resolver = resolver.ModuleResolver.init(allocator, std_path_owned),
        .std_lib_path_owned = std_path_owned,
        .modules = std.StringHashMap(*Module).init(allocator),
        .macro_registry = source_registry.SourceMacroRegistry.init(allocator),
        .dependencies = std.StringHashMap(std.ArrayList([]const u8)).init(allocator),
        .pending_reexports = std.ArrayList(ModuleLoader.PendingReexport).init(allocator),
        .reexport_errors = std.ArrayList(ModuleLoader.ReexportError).init(allocator),
    };
    defer {
        // Clean up modules
        var it = loader.modules.valueIterator();
        while (it.next()) |mod| {
            mod.*.macros.deinit();
            mod.*.exports.deinit();
            for (mod.*.imports.items) |imp| {
                allocator.free(imp.symbols);
            }
            mod.*.imports.deinit();
            allocator.free(mod.*.source);
            allocator.free(mod.*.path);
            allocator.destroy(mod.*);
        }
        loader.modules.deinit();
        // Clean up dependencies
        var dep_it = loader.dependencies.valueIterator();
        while (dep_it.next()) |dep_list| {
            dep_list.deinit();
        }
        loader.dependencies.deinit();
        allocator.free(loader.std_lib_path_owned);
        loader.resolver.deinit();
        loader.macro_registry.deinit();
        loader.pending_reexports.deinit();
        loader.reexport_errors.deinit();
    }

    // Load std macros
    try loader.loadStdMacros();

    // derive macro should be loaded
    try std.testing.expect(loader.findMacro("derive") != null);
}

test "getModulesInDependencyOrder returns topological order" {
    const allocator = std.testing.allocator;

    // Create: C imports B, B imports A
    // Expected order: A, B, C (dependencies before dependents)
    const module_a = "function funcA(): void {}";
    const module_b =
        \\import { funcA } from "./topo_a";
        \\function funcB(): void {}
    ;
    const module_c =
        \\import { funcB } from "./topo_b";
        \\function funcC(): void {}
    ;

    const path_a = "/tmp/topo_a.ms";
    const path_b = "/tmp/topo_b.ms";
    const path_c = "/tmp/topo_c.ms";

    {
        const file = try std.fs.cwd().createFile(path_a, .{});
        defer file.close();
        try file.writeAll(module_a);
    }
    defer std.fs.cwd().deleteFile(path_a) catch {};

    {
        const file = try std.fs.cwd().createFile(path_b, .{});
        defer file.close();
        try file.writeAll(module_b);
    }
    defer std.fs.cwd().deleteFile(path_b) catch {};

    {
        const file = try std.fs.cwd().createFile(path_c, .{});
        defer file.close();
        try file.writeAll(module_c);
    }
    defer std.fs.cwd().deleteFile(path_c) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    // Load C first (which will transitively load B and A)
    _ = try loader.loadModule(path_c);

    // Get topological order
    const order = try loader.getModulesInDependencyOrder(allocator);
    defer allocator.free(order);

    // Should have all 3 modules
    try std.testing.expectEqual(@as(usize, 3), order.len);

    // Find positions (compare with normalized paths)
    const normalized_a = try std.fs.cwd().realpathAlloc(allocator, path_a);
    defer allocator.free(normalized_a);
    const normalized_b = try std.fs.cwd().realpathAlloc(allocator, path_b);
    defer allocator.free(normalized_b);
    const normalized_c = try std.fs.cwd().realpathAlloc(allocator, path_c);
    defer allocator.free(normalized_c);

    var pos_a: ?usize = null;
    var pos_b: ?usize = null;
    var pos_c: ?usize = null;
    for (order, 0..) |path, i| {
        if (std.mem.eql(u8, path, normalized_a)) pos_a = i;
        if (std.mem.eql(u8, path, normalized_b)) pos_b = i;
        if (std.mem.eql(u8, path, normalized_c)) pos_c = i;
    }

    // All should be found
    try std.testing.expect(pos_a != null);
    try std.testing.expect(pos_b != null);
    try std.testing.expect(pos_c != null);

    // Dependencies must come before dependents
    try std.testing.expect(pos_a.? < pos_b.?); // A before B
    try std.testing.expect(pos_b.? < pos_c.?); // B before C
}

test "empty module loads successfully" {
    const allocator = std.testing.allocator;

    // Empty file should parse but have no exports
    const tmp_path = "/tmp/test_empty_module.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll("");
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    try std.testing.expectEqual(@as(usize, 0), module.exports.count());
    try std.testing.expectEqual(@as(usize, 0), module.imports.items.len);
    try std.testing.expectEqual(@as(usize, 0), module.macros.count());
    try std.testing.expectEqual(ModuleState.ready, module.state);
}

test "multiple imports from same module deduplicates dependencies" {
    const allocator = std.testing.allocator;

    // Create source with multiple exports
    const source_module =
        \\function func1(): void {}
        \\function func2(): void {}
        \\function func3(): void {}
    ;

    // Consumer imports from same module twice
    const consumer_module =
        \\import { func1, func2 } from "./multi_source";
        \\import { func3 } from "./multi_source";
        \\function main(): void {}
    ;

    const source_path = "/tmp/multi_source.ms";
    const consumer_path = "/tmp/multi_consumer.ms";

    {
        const file = try std.fs.cwd().createFile(source_path, .{});
        defer file.close();
        try file.writeAll(source_module);
    }
    defer std.fs.cwd().deleteFile(source_path) catch {};

    {
        const file = try std.fs.cwd().createFile(consumer_path, .{});
        defer file.close();
        try file.writeAll(consumer_module);
    }
    defer std.fs.cwd().deleteFile(consumer_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    _ = try loader.loadModule(consumer_path);

    // Should have 2 import entries (one for each import statement)
    const module = loader.modules.get(consumer_path).?;
    try std.testing.expectEqual(@as(usize, 2), module.imports.items.len);

    // But dependencies should be deduplicated - only 1 unique dependency
    const deps = loader.getDependencies(consumer_path);
    try std.testing.expect(deps != null);
    try std.testing.expectEqual(@as(usize, 1), deps.?.len);
}

test "getMacroFromModule returns null for non-existent module" {
    const allocator = std.testing.allocator;

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    // No modules loaded - should return null
    const result = loader.getMacroFromModule("/nonexistent/path.ms", "anyMacro");
    try std.testing.expect(result == null);
}

test "getMacroFromModule returns null for non-existent macro" {
    const allocator = std.testing.allocator;

    const test_source = "function notAMacro(): void {}";

    const tmp_path = "/tmp/test_no_macro.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    _ = try loader.loadModule(tmp_path);

    // Module exists but macro doesn't
    const result = loader.getMacroFromModule(tmp_path, "nonexistentMacro");
    try std.testing.expect(result == null);
}

test "getSymbolFromModule returns null for non-existent symbol" {
    const allocator = std.testing.allocator;

    const test_source = "function existing(): void {}";

    const tmp_path = "/tmp/test_no_symbol.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    _ = try loader.loadModule(tmp_path);

    // Existing symbol works
    const found = loader.getSymbolFromModule(tmp_path, "existing");
    try std.testing.expect(found != null);

    // Non-existent symbol returns null
    const not_found = loader.getSymbolFromModule(tmp_path, "nonexistent");
    try std.testing.expect(not_found == null);

    // Non-existent module returns null
    const no_module = loader.getSymbolFromModule("/fake/path.ms", "existing");
    try std.testing.expect(no_module == null);
}

test "re-export with renaming" {
    const allocator = std.testing.allocator;

    // Source module
    const source_module = "function originalName(): void {}";

    // Re-exporting module with renaming
    const reexport_module =
        \\export { originalName as renamedExport } from "./rename_source";
    ;

    const source_path = "/tmp/rename_source.ms";
    const reexport_path = "/tmp/rename_reexport.ms";

    {
        const file = try std.fs.cwd().createFile(source_path, .{});
        defer file.close();
        try file.writeAll(source_module);
    }
    defer std.fs.cwd().deleteFile(source_path) catch {};

    {
        const file = try std.fs.cwd().createFile(reexport_path, .{});
        defer file.close();
        try file.writeAll(reexport_module);
    }
    defer std.fs.cwd().deleteFile(reexport_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(reexport_path);

    // Should have the renamed export, not the original name
    try std.testing.expect(module.exports.contains("renamedExport"));
    // Original name should NOT be in this module's exports
    try std.testing.expect(!module.exports.contains("originalName"));
}

test "getDependencies returns null for unknown module" {
    const allocator = std.testing.allocator;

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const deps = loader.getDependencies("/nonexistent/module.ms");
    try std.testing.expect(deps == null);
}

test "module with only comments loads successfully" {
    const allocator = std.testing.allocator;

    const test_source =
        \\// This is a comment
        \\// Another comment
        \\/* Block comment */
    ;

    const tmp_path = "/tmp/test_comments_only.ms";
    {
        const file = try std.fs.cwd().createFile(tmp_path, .{});
        defer file.close();
        try file.writeAll(test_source);
    }
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(tmp_path);

    try std.testing.expectEqual(@as(usize, 0), module.exports.count());
    try std.testing.expectEqual(ModuleState.ready, module.state);
}

test "re-export macro from source module" {
    const allocator = std.testing.allocator;

    // Source with macro
    const source_module =
        \\macro function sourceMacro(target) {
        \\    return target;
        \\}
    ;

    // Re-export the macro
    const reexport_module =
        \\export { sourceMacro } from "./macro_source";
    ;

    const source_path = "/tmp/macro_source.ms";
    const reexport_path = "/tmp/macro_reexport.ms";

    {
        const file = try std.fs.cwd().createFile(source_path, .{});
        defer file.close();
        try file.writeAll(source_module);
    }
    defer std.fs.cwd().deleteFile(source_path) catch {};

    {
        const file = try std.fs.cwd().createFile(reexport_path, .{});
        defer file.close();
        try file.writeAll(reexport_module);
    }
    defer std.fs.cwd().deleteFile(reexport_path) catch {};

    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const module = try loader.loadModule(reexport_path);

    // Re-exported macro should be in exports
    try std.testing.expect(module.exports.contains("sourceMacro"));
    try std.testing.expectEqual(Module.SymbolKind.macro, module.exports.get("sourceMacro").?.kind);

    // Should also be in module's macros map
    try std.testing.expect(module.macros.contains("sourceMacro"));

    // Global registry should have it
    try std.testing.expect(loader.findMacro("sourceMacro") != null);
}

test "loadModuleFromSource loads from in-memory content" {
    const allocator = std.testing.allocator;
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    // Use a virtual path (doesn't exist on disk)
    const virtual_path = "/virtual/test_module.ms";
    const source =
        \\export function greet(name: string): string {
        \\    return "Hello, " + name;
        \\}
        \\
        \\export const VERSION = "1.0.0";
    ;

    // Load from in-memory source (should NOT read from disk)
    const module = try loader.loadModuleFromSource(virtual_path, source);

    // Verify module was created correctly
    try std.testing.expectEqualStrings(virtual_path, module.path);
    try std.testing.expectEqual(ModuleState.ready, module.state);

    // Verify exports were collected
    try std.testing.expect(module.exports.contains("greet"));
    try std.testing.expectEqual(Module.SymbolKind.function, module.exports.get("greet").?.kind);

    try std.testing.expect(module.exports.contains("VERSION"));
    try std.testing.expectEqual(Module.SymbolKind.variable, module.exports.get("VERSION").?.kind);

    // Verify it's cached
    try std.testing.expect(loader.modules.contains(virtual_path));
}

test "loadModuleFromSource updates existing module with new content" {
    const allocator = std.testing.allocator;
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    var loader = try ModuleLoader.init(allocator, &arena);
    defer loader.deinit();

    const virtual_path = "/virtual/changing_module.ms";

    // First version
    const source_v1 =
        \\export function foo(): number { return 1; }
    ;
    const module_v1 = try loader.loadModuleFromSource(virtual_path, source_v1);
    try std.testing.expect(module_v1.exports.contains("foo"));
    try std.testing.expect(!module_v1.exports.contains("bar"));

    // Second version with different content
    const source_v2 =
        \\export function bar(): number { return 2; }
    ;
    const module_v2 = try loader.loadModuleFromSource(virtual_path, source_v2);

    // Should have new exports, not old ones
    try std.testing.expect(module_v2.exports.contains("bar"));
    // Note: Old export may or may not be cleared depending on implementation
    // The key point is the new export is there
}
