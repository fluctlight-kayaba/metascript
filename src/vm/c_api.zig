/// Hermes C API - Centralized Import for Metascript
/// All VM modules must import this file for type compatibility

pub const c = @cImport({
    @cInclude("hermes_api.h");
});
