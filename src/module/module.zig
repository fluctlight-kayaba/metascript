/// Module System
///
/// Provides import/export resolution and module loading for Metascript.
///
/// Key components:
/// - ModuleResolver: Resolves import specifiers to file paths
/// - ModuleLoader: Loads and parses modules with caching
/// - Module: Represents a loaded module with its exports
///
/// Usage:
/// ```zig
/// var loader = try ModuleLoader.init(allocator, arena);
/// defer loader.deinit();
///
/// // Load standard library macros
/// try loader.loadStdMacros();
///
/// // Load a user module
/// const module = try loader.loadModule("./src/app.ms");
///
/// // Find a macro for expansion
/// if (loader.findMacro("derive")) |macro| {
///     // Use macro
/// }
/// ```

pub const resolver = @import("resolver.zig");
pub const loader = @import("loader.zig");

pub const ModuleResolver = resolver.ModuleResolver;
pub const ModuleLoader = loader.ModuleLoader;
pub const Module = loader.Module;

pub const getStdLibPath = resolver.getStdLibPath;

test {
    _ = resolver;
    _ = loader;
}
