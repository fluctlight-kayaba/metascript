/**
 * Metascript Standard Library
 *
 * Node.js-compatible APIs for cross-platform development.
 * Zero-cost abstractions mapping directly to Zig stdlib.
 *
 * @module std
 * @example
 * ```typescript
 * // Import specific modules
 * import * as fs from "std/fs";
 * import * as path from "std/path";
 * import { spawn } from "std/child_process";
 * import { EventEmitter } from "std/events";
 *
 * // Or import from std/macros for compile-time features
 * import { derive } from "std/macros";
 * ```
 *
 * ## Available Modules
 *
 * ### Core I/O
 * - `std/fs` - File system operations
 * - `std/path` - Path manipulation (pure string ops)
 * - `std/process` - Process info, env, argv
 * - `std/child_process` - Spawn subprocesses
 *
 * ### Events & Utilities
 * - `std/events` - EventEmitter pattern
 * - `std/util` - Format, inspect, type checks
 *
 * ### System Info
 * - `std/os` - Operating system info
 *
 * ### Compile-time
 * - `std/macros` - @derive, @comptime, @serialize
 */

// Re-export macros (compile-time features)
export * from "./macros/index.ms";

// Note: Other modules are imported directly, e.g.:
//   import * as fs from "std/fs";
//   import { join } from "std/path";
//
// This avoids loading all modules when only one is needed.
