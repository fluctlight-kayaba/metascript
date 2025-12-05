/**
 * Utility functions
 *
 * Node.js-compatible util module with common helper functions.
 *
 * @module std/util
 * @example
 * ```typescript
 * import * as util from "std/util";
 *
 * const msg = util.format("Hello %s, you have %d messages", "Alice", 5);
 * console.log(msg);  // "Hello Alice, you have 5 messages"
 *
 * console.log(util.inspect({ name: "test", value: 42 }));
 * ```
 */

// =============================================================================
// String Formatting
// =============================================================================

/**
 * Format a string with placeholders
 *
 * @param format - Format string with placeholders
 * @param args - Values to substitute
 * @returns Formatted string
 *
 * @description
 * Supported placeholders:
 * - `%s` - String
 * - `%d` / `%i` - Integer
 * - `%f` - Float
 * - `%j` - JSON
 * - `%o` / `%O` - Object (inspect)
 * - `%%` - Literal percent sign
 *
 * @example
 * ```typescript
 * format("Hello %s!", "World");           // "Hello World!"
 * format("Count: %d", 42);                // "Count: 42"
 * format("Data: %j", { a: 1 });           // 'Data: {"a":1}'
 * format("%s has %d items", "Cart", 3);   // "Cart has 3 items"
 * ```
 */
export function format(format: string, ...args: any[]): string {
    if (args.length === 0) return format;

    let result = "";
    let argIndex = 0;
    let i = 0;

    while (i < format.length) {
        if (format[i] === "%" && i + 1 < format.length) {
            const specifier = format[i + 1];

            switch (specifier) {
                case "s":
                    result += String(args[argIndex++] ?? "");
                    i += 2;
                    break;
                case "d":
                case "i":
                    result += parseInt(args[argIndex++] ?? 0, 10).toString();
                    i += 2;
                    break;
                case "f":
                    result += parseFloat(args[argIndex++] ?? 0).toString();
                    i += 2;
                    break;
                case "j":
                    try {
                        result += JSON.stringify(args[argIndex++]);
                    } catch {
                        result += "[Circular]";
                    }
                    i += 2;
                    break;
                case "o":
                case "O":
                    result += inspect(args[argIndex++]);
                    i += 2;
                    break;
                case "%":
                    result += "%";
                    i += 2;
                    break;
                default:
                    result += format[i];
                    i++;
            }
        } else {
            result += format[i];
            i++;
        }
    }

    // Append remaining arguments
    while (argIndex < args.length) {
        result += " " + String(args[argIndex++]);
    }

    return result;
}

// =============================================================================
// Object Inspection
// =============================================================================

/**
 * Options for inspect()
 */
export interface InspectOptions {
    /**
     * Depth to recurse into objects
     * @default 2
     */
    depth?: number | null;

    /**
     * Use colors in output
     * @default false
     */
    colors?: boolean;

    /**
     * Show non-enumerable properties
     * @default false
     */
    showHidden?: boolean;

    /**
     * Maximum array elements to show
     * @default 100
     */
    maxArrayLength?: number | null;

    /**
     * Maximum string length to show
     * @default 10000
     */
    maxStringLength?: number | null;

    /**
     * Break long lines
     * @default 80
     */
    breakLength?: number;

    /**
     * Compact output (single line for small objects)
     * @default 3
     */
    compact?: boolean | number;

    /**
     * Sort object keys
     * @default false
     */
    sorted?: boolean;

    /**
     * Show object getters
     * @default false
     */
    getters?: boolean;
}

/**
 * Convert any value to a string representation for debugging
 *
 * @param object - Value to inspect
 * @param options - Inspection options
 * @returns String representation
 *
 * @description
 * Creates a string representation of an object that is useful for debugging.
 * Handles circular references, custom inspect methods, and various types.
 *
 * @example
 * ```typescript
 * inspect({ name: "test", nested: { a: 1 } });
 * // "{ name: 'test', nested: { a: 1 } }"
 *
 * inspect([1, 2, 3], { colors: true });
 * // "[ 1, 2, 3 ]" (with ANSI colors)
 *
 * inspect(new Map([["key", "value"]]));
 * // "Map(1) { 'key' => 'value' }"
 * ```
 */
export function inspect(object: any, options?: InspectOptions): string {
    const opts: InspectOptions = {
        depth: 2,
        colors: false,
        showHidden: false,
        maxArrayLength: 100,
        maxStringLength: 10000,
        breakLength: 80,
        compact: 3,
        sorted: false,
        getters: false,
        ...options
    };

    return inspectValue(object, opts, 0, new Set());
}

/**
 * @internal
 * Recursive inspect implementation
 */
function inspectValue(
    value: any,
    opts: InspectOptions,
    depth: number,
    seen: Set<any>
): string {
    // Handle null/undefined
    if (value === null) return "null";
    if (value === undefined) return "undefined";

    // Handle primitives
    const type = typeof value;

    if (type === "string") {
        const str = value.length > (opts.maxStringLength ?? 10000)
            ? value.substring(0, opts.maxStringLength) + "..."
            : value;
        return `'${str.replace(/'/g, "\\'")}'`;
    }

    if (type === "number" || type === "boolean") {
        return String(value);
    }

    if (type === "bigint") {
        return `${value}n`;
    }

    if (type === "symbol") {
        return value.toString();
    }

    if (type === "function") {
        const name = value.name || "(anonymous)";
        return `[Function: ${name}]`;
    }

    // Handle circular references
    if (seen.has(value)) {
        return "[Circular]";
    }

    // Check depth
    if (opts.depth !== null && depth > (opts.depth ?? 2)) {
        return Array.isArray(value) ? "[Array]" : "[Object]";
    }

    seen.add(value);

    // Handle arrays
    if (Array.isArray(value)) {
        const maxLen = opts.maxArrayLength ?? 100;
        const items = value
            .slice(0, maxLen)
            .map(item => inspectValue(item, opts, depth + 1, seen));

        if (value.length > maxLen) {
            items.push(`... ${value.length - maxLen} more items`);
        }

        seen.delete(value);
        return `[ ${items.join(", ")} ]`;
    }

    // Handle Date
    if (value instanceof Date) {
        return value.toISOString();
    }

    // Handle RegExp
    if (value instanceof RegExp) {
        return value.toString();
    }

    // Handle Error
    if (value instanceof Error) {
        return `${value.name}: ${value.message}`;
    }

    // Handle Map
    if (value instanceof Map) {
        const entries = Array.from(value.entries())
            .map(([k, v]) =>
                `${inspectValue(k, opts, depth + 1, seen)} => ${inspectValue(v, opts, depth + 1, seen)}`
            );
        seen.delete(value);
        return `Map(${value.size}) { ${entries.join(", ")} }`;
    }

    // Handle Set
    if (value instanceof Set) {
        const items = Array.from(value)
            .map(item => inspectValue(item, opts, depth + 1, seen));
        seen.delete(value);
        return `Set(${value.size}) { ${items.join(", ")} }`;
    }

    // Handle plain objects
    const keys = opts.sorted
        ? Object.keys(value).sort()
        : Object.keys(value);

    const props = keys.map(key => {
        const propValue = inspectValue(value[key], opts, depth + 1, seen);
        return `${key}: ${propValue}`;
    });

    seen.delete(value);
    return `{ ${props.join(", ")} }`;
}

/**
 * Custom inspect symbol
 * Objects can implement this to customize their inspect output
 */
inspect.custom = Symbol.for("nodejs.util.inspect.custom");

// =============================================================================
// Type Checking
// =============================================================================

/**
 * Check if value is an array
 * @deprecated Use Array.isArray() instead
 */
export function isArray(value: any): value is any[] {
    return Array.isArray(value);
}

/**
 * Check if value is a boolean
 */
export function isBoolean(value: any): value is boolean {
    return typeof value === "boolean";
}

/**
 * Check if value is null
 */
export function isNull(value: any): value is null {
    return value === null;
}

/**
 * Check if value is null or undefined
 */
export function isNullOrUndefined(value: any): value is null | undefined {
    return value === null || value === undefined;
}

/**
 * Check if value is a number
 */
export function isNumber(value: any): value is number {
    return typeof value === "number";
}

/**
 * Check if value is a string
 */
export function isString(value: any): value is string {
    return typeof value === "string";
}

/**
 * Check if value is a symbol
 */
export function isSymbol(value: any): value is symbol {
    return typeof value === "symbol";
}

/**
 * Check if value is undefined
 */
export function isUndefined(value: any): value is undefined {
    return value === undefined;
}

/**
 * Check if value is an object (not null, not primitive)
 */
export function isObject(value: any): value is object {
    return value !== null && typeof value === "object";
}

/**
 * Check if value is a function
 */
export function isFunction(value: any): value is Function {
    return typeof value === "function";
}

/**
 * Check if value is a primitive (null, undefined, boolean, number, string, symbol, bigint)
 */
export function isPrimitive(value: any): boolean {
    return value === null ||
           value === undefined ||
           typeof value === "boolean" ||
           typeof value === "number" ||
           typeof value === "string" ||
           typeof value === "symbol" ||
           typeof value === "bigint";
}

/**
 * Check if value is an Error
 */
export function isError(value: any): value is Error {
    return value instanceof Error;
}

/**
 * Check if value is a Date
 */
export function isDate(value: any): value is Date {
    return value instanceof Date;
}

/**
 * Check if value is a RegExp
 */
export function isRegExp(value: any): value is RegExp {
    return value instanceof RegExp;
}

// =============================================================================
// Promisify
// =============================================================================

/**
 * Convert callback-based function to Promise-based
 *
 * @param fn - Function with callback as last parameter
 * @returns Promise-returning function
 *
 * @description
 * Converts Node.js-style callback function (error-first callback)
 * to a function that returns a Promise.
 *
 * @example
 * ```typescript
 * import * as fs from "std/fs";
 *
 * // Original: fs.readFile(path, callback)
 * const readFileAsync = promisify(fs.readFile);
 *
 * // Now returns Promise
 * const content = await readFileAsync("file.txt");
 * ```
 */
export function promisify<T>(
    fn: (...args: any[]) => void
): (...args: any[]) => Promise<T> {
    return function (...args: any[]): Promise<T> {
        return new Promise((resolve, reject) => {
            fn(...args, (err: any, result: T) => {
                if (err) {
                    reject(err);
                } else {
                    resolve(result);
                }
            });
        });
    };
}

/**
 * Symbol to mark custom promisify implementation
 */
promisify.custom = Symbol.for("nodejs.util.promisify.custom");

// =============================================================================
// Callbackify
// =============================================================================

/**
 * Convert async/Promise function to callback-based
 *
 * @param fn - Async function or function returning Promise
 * @returns Function with callback as last parameter
 *
 * @example
 * ```typescript
 * async function fetchData(url: string): Promise<string> {
 *     const res = await fetch(url);
 *     return res.text();
 * }
 *
 * const fetchDataCb = callbackify(fetchData);
 * fetchDataCb("http://example.com", (err, data) => {
 *     if (err) console.error(err);
 *     else console.log(data);
 * });
 * ```
 */
export function callbackify<T>(
    fn: (...args: any[]) => Promise<T>
): (...args: any[]) => void {
    return function (...args: any[]): void {
        const callback = args.pop();
        fn(...args)
            .then(result => callback(null, result))
            .catch(err => callback(err));
    };
}

// =============================================================================
// Deep Equality
// =============================================================================

/**
 * Deep strict equality comparison
 *
 * @param val1 - First value
 * @param val2 - Second value
 * @returns true if values are deeply equal
 *
 * @description
 * Compares two values for deep equality using strict comparison (===).
 * Handles objects, arrays, Maps, Sets, and primitives.
 *
 * @example
 * ```typescript
 * isDeepStrictEqual({ a: 1 }, { a: 1 });  // true
 * isDeepStrictEqual([1, 2], [1, 2]);       // true
 * isDeepStrictEqual({ a: 1 }, { a: "1" }); // false (strict)
 * ```
 */
export function isDeepStrictEqual(val1: any, val2: any): boolean {
    // Same reference or primitives
    if (val1 === val2) return true;

    // Different types
    if (typeof val1 !== typeof val2) return false;

    // Handle null
    if (val1 === null || val2 === null) return false;

    // Handle NaN
    if (Number.isNaN(val1) && Number.isNaN(val2)) return true;

    // Must be objects from here
    if (typeof val1 !== "object") return false;

    // Handle arrays
    if (Array.isArray(val1) && Array.isArray(val2)) {
        if (val1.length !== val2.length) return false;
        for (let i = 0; i < val1.length; i++) {
            if (!isDeepStrictEqual(val1[i], val2[i])) return false;
        }
        return true;
    }

    // Handle Date
    if (val1 instanceof Date && val2 instanceof Date) {
        return val1.getTime() === val2.getTime();
    }

    // Handle RegExp
    if (val1 instanceof RegExp && val2 instanceof RegExp) {
        return val1.toString() === val2.toString();
    }

    // Handle Map
    if (val1 instanceof Map && val2 instanceof Map) {
        if (val1.size !== val2.size) return false;
        for (const [key, value] of val1) {
            if (!val2.has(key) || !isDeepStrictEqual(value, val2.get(key))) {
                return false;
            }
        }
        return true;
    }

    // Handle Set
    if (val1 instanceof Set && val2 instanceof Set) {
        if (val1.size !== val2.size) return false;
        for (const value of val1) {
            if (!val2.has(value)) return false;
        }
        return true;
    }

    // Handle plain objects
    const keys1 = Object.keys(val1);
    const keys2 = Object.keys(val2);

    if (keys1.length !== keys2.length) return false;

    for (const key of keys1) {
        if (!keys2.includes(key)) return false;
        if (!isDeepStrictEqual(val1[key], val2[key])) return false;
    }

    return true;
}

// =============================================================================
// Deprecation
// =============================================================================

/**
 * Mark a function as deprecated
 *
 * @param fn - Function to wrap
 * @param message - Deprecation warning message
 * @returns Wrapped function that logs warning on first call
 *
 * @example
 * ```typescript
 * const oldFunction = deprecate(
 *     () => doOldThing(),
 *     "oldFunction is deprecated, use newFunction instead"
 * );
 * ```
 */
export function deprecate<T extends Function>(fn: T, message: string): T {
    let warned = false;

    return function (this: any, ...args: any[]) {
        if (!warned) {
            console.warn(`DeprecationWarning: ${message}`);
            warned = true;
        }
        return fn.apply(this, args);
    } as unknown as T;
}

// =============================================================================
// Inherits (legacy)
// =============================================================================

/**
 * Inherit prototype methods from superclass
 *
 * @deprecated Use ES6 class extends instead
 *
 * @example
 * ```typescript
 * // Legacy pattern (don't use)
 * function Child() { Parent.call(this); }
 * inherits(Child, Parent);
 *
 * // Modern pattern (use this)
 * class Child extends Parent { }
 * ```
 */
export function inherits(
    ctor: Function,
    superCtor: Function
): void {
    Object.setPrototypeOf(ctor.prototype, superCtor.prototype);
}

// =============================================================================
// Debug Log
// =============================================================================

/**
 * Create a debug logger for a namespace
 *
 * @param namespace - Debug namespace (e.g., "myapp:server")
 * @returns Debug logging function
 *
 * @description
 * Returns a function that logs debug messages when DEBUG env var matches.
 * Set DEBUG=namespace or DEBUG=* to enable.
 *
 * @example
 * ```typescript
 * const debug = debuglog("myapp");
 *
 * debug("Server started on port %d", 3000);
 * // Only prints if DEBUG=myapp or DEBUG=*
 * ```
 */
export function debuglog(namespace: string): (...args: any[]) => void {
    const debugEnv = process.env["DEBUG"] || "";
    const enabled = debugEnv === "*" ||
                    debugEnv.split(",").some(s => s.trim() === namespace);

    if (enabled) {
        return function (...args: any[]) {
            const msg = format(...args);
            console.error(`${namespace}: ${msg}`);
        };
    }

    return function () {};
}
