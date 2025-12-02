/**
 * Standard Macro Library
 *
 * Re-exports all standard macros for convenient importing:
 *
 * ```typescript
 * import { derive, serialize } from "std/macros";
 * ```
 *
 * @module std/macros
 */

// Trait derivation macros
export { derive, deriveEq, deriveHash, deriveClone, deriveDebug } from "./derive.ms";

// Serialization macros (TODO)
// export { serialize, deserialize } from "./serialize.ms";

// FFI macros (TODO)
// export { ffi, bindC } from "./ffi.ms";
