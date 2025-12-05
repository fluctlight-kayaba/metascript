/**
 * Compiler Intrinsic Macros
 *
 * These macros are implemented by the compiler itself.
 * The `extern macro` syntax indicates no Metascript body exists.
 *
 * @module std/macros/compiler
 */

// =============================================================================
// Conditional Compilation
// =============================================================================

/**
 * Conditional compilation for target backends
 *
 * Code inside the block is only compiled for the specified target(s).
 * Supports `else` clause for fallback code.
 *
 * @param platforms - One or more target backends: "c", "js", "erlang"
 *
 * @example
 * @target("c") {
 *     @emit("printf($msg)")
 * } else @target("js") {
 *     console.log(msg);
 * } else {
 *     // Fallback for other backends
 * }
 *
 * @example
 * @target("c", "js") {
 *     // Compiled for both C and JS backends
 * }
 */
extern macro @target(...platforms: string[]): void;


// =============================================================================
// Code Emission
// =============================================================================

/**
 * Emit raw backend code
 *
 * Bypasses AST and emits literal code string to the backend.
 * Use `$var` syntax for variable interpolation.
 *
 * @param code - Raw code string with $var interpolation
 *
 * @example
 * @target("c") {
 *     @emit("printf(\"%s\\n\", $msg);")
 * }
 *
 * @example
 * @target("js") {
 *     @emit("console.log($x, $y);")
 * }
 *
 * @example
 * @target("erlang") {
 *     @emit("io:format(\"~p~n\", [$value])")
 * }
 */
extern macro @emit(code: string): void;

/**
 * Emit raw backend code with typed return value
 *
 * Same as `@emit` but returns a value of the specified type.
 *
 * @param T - Return type
 * @param code - Raw code string with $var interpolation
 * @returns Value of type T from the emitted code
 *
 * @example
 * const result = @emit<boolean>("confirm($msg)");
 *
 * @example
 * const now = @emit<number>("Date.now()");
 */
extern macro @emit<T>(code: string): T;


// =============================================================================
// Compile-Time Execution
// =============================================================================

/**
 * Compile-time execution block
 *
 * Code inside runs during compilation; result embedded as constant.
 * Has access to file system, environment, and build information.
 *
 * @param block - Code block to execute at compile time
 * @returns Result of the block, embedded as compile-time constant
 *
 * @example
 * const version = @comptime {
 *     return readFile("VERSION").trim();
 * };
 *
 * @example
 * const config = @comptime {
 *     const env = getEnv("NODE_ENV");
 *     return {
 *         debug: env === "development",
 *         apiUrl: env === "production"
 *             ? "https://api.prod.com"
 *             : "http://localhost:3000"
 *     };
 * };
 */
extern macro @comptime<T>(block: Function): T;


// =============================================================================
// Optimization Hints
// =============================================================================

/**
 * Force inline expansion at call sites
 *
 * Marks a function for inline expansion. The compiler will
 * replace calls with the function body.
 *
 * @example
 * @inline
 * function add(a: number, b: number): number {
 *     return a + b;
 * }
 */
extern macro @inline(): void;


// =============================================================================
// Type Introspection
// =============================================================================

/**
 * Get size of type in bytes
 *
 * Returns the size of the type in the target backend.
 * Value is backend-specific.
 *
 * @param T - Type to measure
 * @returns Size in bytes
 *
 * @example
 * const intSize = @sizeof<i32>();  // 4 on most platforms
 * const ptrSize = @sizeof<*void>(); // 8 on 64-bit
 */
extern macro @sizeof<T>(): usize;

/**
 * Get alignment of type in bytes
 *
 * Returns the alignment requirement of the type.
 * Value is backend-specific.
 *
 * @param T - Type to check
 * @returns Alignment in bytes
 *
 * @example
 * const intAlign = @alignof<i32>();  // Usually 4
 * const doubleAlign = @alignof<f64>(); // Usually 8
 */
extern macro @alignof<T>(): usize;

/**
 * Get type information at compile time
 *
 * Returns structural information about a type for metaprogramming.
 *
 * @param T - Type to inspect
 * @returns TypeInfo structure with fields, methods, etc.
 *
 * @example
 * const info = @typeinfo<User>();
 * for (const field of info.fields) {
 *     console.log(field.name, field.type);
 * }
 */
extern macro @typeinfo<T>(): TypeInfo;


// =============================================================================
// Build Information
// =============================================================================

/**
 * Get current compilation target
 *
 * Returns the backend being compiled for.
 * Useful in generic code that needs runtime target detection.
 *
 * @returns "c" | "js" | "erlang"
 *
 * @example
 * if (@currentTarget() === "js") {
 *     // Browser-specific code
 * }
 */
extern macro @currentTarget(): string;


// =============================================================================
// Type Definitions for Introspection
// =============================================================================

/**
 * Type information returned by @typeinfo
 */
interface TypeInfo {
    name: string;
    kind: string;  // "class", "interface", "enum", "function", or "primitive"
    fields: FieldInfo[];
    methods: MethodInfo[];
    typeParameters: string[];
}

/**
 * Field information
 */
interface FieldInfo {
    name: string;
    fieldType: string;
    optional: boolean;
    isReadonly: boolean;
    visibility: string;  // "public", "private", or "protected"
}

/**
 * Method information
 */
interface MethodInfo {
    name: string;
    parameters: ParameterInfo[];
    returnType: string;
    isStatic: boolean;
    isAsync: boolean;
    visibility: string;  // "public", "private", or "protected"
}

/**
 * Parameter information
 */
interface ParameterInfo {
    name: string;
    paramType: string;
    optional: boolean;
    defaultValue: string;
}
