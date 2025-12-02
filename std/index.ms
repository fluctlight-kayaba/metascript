/**
 * Metascript Standard Library
 *
 * The standard library provides:
 * - Macros: @derive, @serialize, @ffi
 * - Collections: Array, Map, Set utilities
 * - Types: Option, Result, etc.
 *
 * @example
 * ```typescript
 * import { derive } from "std/macros";
 * import { Option, Result } from "std/types";
 * ```
 *
 * @module std
 */

// Re-export all standard modules
export * from "./macros/index.ms";
// export * from "./collections/index.ms";
// export * from "./types/index.ms";
