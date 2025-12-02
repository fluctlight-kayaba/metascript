/// Metascript VM Module
/// Hermes-based runtime for compile-time macro execution

pub const MacroVM = @import("macro_vm.zig").MacroVM;
pub const ast_api = @import("ast_api.zig");
pub const c_api = @import("c_api.zig");
pub const bytecode_compiler = @import("bytecode_compiler.zig");
