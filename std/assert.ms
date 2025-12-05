/**
 * Assertion testing utilities
 *
 * Node.js-compatible assert module for testing and validation.
 *
 * @module std/assert
 * @example
 * ```typescript
 * import * as assert from "std/assert";
 *
 * assert.ok(value, "value should be truthy");
 * assert.equal(actual, expected);
 * assert.deepStrictEqual(obj1, obj2);
 * assert.throws(() => riskyOperation());
 * ```
 */

import { isDeepStrictEqual, inspect } from "std/util";

// =============================================================================
// Error Classes
// =============================================================================

/**
 * Assertion error thrown when an assertion fails
 */
export class AssertionError extends Error {
    /** Actual value */
    actual: any;
    /** Expected value */
    expected: any;
    /** Comparison operator */
    operator: string;
    /** Whether this is from assert() */
    generatedMessage: boolean;
    /** Code identifier */
    code: string = "ERR_ASSERTION";

    /**
     * Create assertion error
     *
     * @param options - Error options
     */
    constructor(options: {
        message?: string;
        actual?: any;
        expected?: any;
        operator?: string;
    }) {
        const message = options.message ||
            `${inspect(options.actual)} ${options.operator} ${inspect(options.expected)}`;

        super(message);

        this.name = "AssertionError";
        this.actual = options.actual;
        this.expected = options.expected;
        this.operator = options.operator || "==";
        this.generatedMessage = !options.message;

        // Capture stack trace
        if (Error.captureStackTrace) {
            Error.captureStackTrace(this, AssertionError);
        }
    }
}

// =============================================================================
// Basic Assertions
// =============================================================================

/**
 * Assert that value is truthy
 *
 * @param value - Value to test
 * @param message - Optional error message
 * @throws AssertionError if value is falsy
 *
 * @example
 * ```typescript
 * assert.ok(true);          // passes
 * assert.ok(1);             // passes
 * assert.ok("hello");       // passes
 * assert.ok(false);         // throws
 * assert.ok(0);             // throws
 * assert.ok(null);          // throws
 * ```
 */
export function ok(value: any, message?: string): asserts value {
    if (!value) {
        throw new AssertionError({
            message: message || `Expected truthy value, got ${inspect(value)}`,
            actual: value,
            expected: true,
            operator: "=="
        });
    }
}

/**
 * Assert that value is truthy (alias for ok)
 */
export function assert(value: any, message?: string): asserts value {
    ok(value, message);
}

/**
 * Always fail with message
 *
 * @param message - Error message
 * @throws AssertionError always
 *
 * @example
 * ```typescript
 * if (unexpectedCondition) {
 *     assert.fail("Should not reach here");
 * }
 * ```
 */
export function fail(message?: string): never {
    throw new AssertionError({
        message: message || "Assertion failed",
        operator: "fail"
    });
}

// =============================================================================
// Equality Assertions
// =============================================================================

/**
 * Assert loose equality (==)
 *
 * @param actual - Actual value
 * @param expected - Expected value
 * @param message - Optional error message
 * @throws AssertionError if actual != expected
 *
 * @deprecated Use strictEqual instead
 *
 * @example
 * ```typescript
 * assert.equal(1, 1);       // passes
 * assert.equal(1, "1");     // passes (loose)
 * assert.equal(1, 2);       // throws
 * ```
 */
export function equal(actual: any, expected: any, message?: string): void {
    if (actual != expected) {
        throw new AssertionError({
            message,
            actual,
            expected,
            operator: "=="
        });
    }
}

/**
 * Assert loose inequality (!=)
 *
 * @deprecated Use notStrictEqual instead
 */
export function notEqual(actual: any, expected: any, message?: string): void {
    if (actual == expected) {
        throw new AssertionError({
            message,
            actual,
            expected,
            operator: "!="
        });
    }
}

/**
 * Assert strict equality (===)
 *
 * @param actual - Actual value
 * @param expected - Expected value
 * @param message - Optional error message
 * @throws AssertionError if actual !== expected
 *
 * @example
 * ```typescript
 * assert.strictEqual(1, 1);       // passes
 * assert.strictEqual(1, "1");     // throws (strict)
 * assert.strictEqual(NaN, NaN);   // passes (special case)
 * ```
 */
export function strictEqual(actual: any, expected: any, message?: string): void {
    // Handle NaN specially
    if (Number.isNaN(actual) && Number.isNaN(expected)) {
        return;
    }

    if (actual !== expected) {
        throw new AssertionError({
            message,
            actual,
            expected,
            operator: "==="
        });
    }
}

/**
 * Assert strict inequality (!==)
 *
 * @param actual - Actual value
 * @param expected - Expected value
 * @param message - Optional error message
 * @throws AssertionError if actual === expected
 */
export function notStrictEqual(actual: any, expected: any, message?: string): void {
    if (actual === expected) {
        throw new AssertionError({
            message,
            actual,
            expected,
            operator: "!=="
        });
    }
}

// =============================================================================
// Deep Equality Assertions
// =============================================================================

/**
 * Assert deep strict equality
 *
 * @param actual - Actual value
 * @param expected - Expected value
 * @param message - Optional error message
 * @throws AssertionError if not deeply equal
 *
 * @description
 * Compares objects, arrays, Maps, Sets deeply using strict equality.
 *
 * @example
 * ```typescript
 * assert.deepStrictEqual({ a: 1 }, { a: 1 });     // passes
 * assert.deepStrictEqual([1, 2], [1, 2]);         // passes
 * assert.deepStrictEqual({ a: 1 }, { a: "1" });   // throws (strict)
 * ```
 */
export function deepStrictEqual(actual: any, expected: any, message?: string): void {
    if (!isDeepStrictEqual(actual, expected)) {
        throw new AssertionError({
            message,
            actual,
            expected,
            operator: "deepStrictEqual"
        });
    }
}

/**
 * Assert deep strict inequality
 *
 * @param actual - Actual value
 * @param expected - Expected value
 * @param message - Optional error message
 * @throws AssertionError if deeply equal
 */
export function notDeepStrictEqual(actual: any, expected: any, message?: string): void {
    if (isDeepStrictEqual(actual, expected)) {
        throw new AssertionError({
            message,
            actual,
            expected,
            operator: "notDeepStrictEqual"
        });
    }
}

// =============================================================================
// Exception Assertions
// =============================================================================

/**
 * Assert that function throws
 *
 * @param fn - Function to call
 * @param error - Expected error type or message regex
 * @param message - Optional error message
 * @throws AssertionError if function doesn't throw matching error
 *
 * @example
 * ```typescript
 * assert.throws(() => { throw new Error("fail"); });
 * assert.throws(() => { throw new TypeError("bad"); }, TypeError);
 * assert.throws(() => { throw new Error("oops"); }, /oops/);
 * ```
 */
export function throws(
    fn: () => void,
    error?: RegExp | Function | Error,
    message?: string
): void {
    let threw = false;
    let thrownError: any;

    try {
        fn();
    } catch (e) {
        threw = true;
        thrownError = e;
    }

    if (!threw) {
        throw new AssertionError({
            message: message || "Expected function to throw",
            actual: undefined,
            expected: error,
            operator: "throws"
        });
    }

    // Check error type/pattern if provided
    if (error !== undefined) {
        if (error instanceof RegExp) {
            if (!error.test(String(thrownError.message || thrownError))) {
                throw new AssertionError({
                    message: message || `Error message did not match ${error}`,
                    actual: thrownError,
                    expected: error,
                    operator: "throws"
                });
            }
        } else if (typeof error === "function") {
            if (!(thrownError instanceof error)) {
                throw new AssertionError({
                    message: message || `Expected ${error.name}, got ${thrownError.constructor.name}`,
                    actual: thrownError,
                    expected: error,
                    operator: "throws"
                });
            }
        }
    }
}

/**
 * Assert that function does not throw
 *
 * @param fn - Function to call
 * @param message - Optional error message
 * @throws AssertionError if function throws
 *
 * @example
 * ```typescript
 * assert.doesNotThrow(() => safeOperation());
 * ```
 */
export function doesNotThrow(fn: () => void, message?: string): void {
    try {
        fn();
    } catch (e) {
        throw new AssertionError({
            message: message || `Expected function not to throw, but it threw: ${e}`,
            actual: e,
            expected: undefined,
            operator: "doesNotThrow"
        });
    }
}

/**
 * Assert that async function rejects
 *
 * @param asyncFn - Async function or Promise
 * @param error - Expected error type or message regex
 * @param message - Optional error message
 * @returns Promise that resolves if assertion passes
 *
 * @example
 * ```typescript
 * await assert.rejects(async () => { throw new Error("fail"); });
 * await assert.rejects(fetchData(), /not found/);
 * ```
 */
export async function rejects(
    asyncFn: (() => Promise<any>) | Promise<any>,
    error?: RegExp | Function | Error,
    message?: string
): Promise<void> {
    let threw = false;
    let thrownError: any;

    try {
        const promise = typeof asyncFn === "function" ? asyncFn() : asyncFn;
        await promise;
    } catch (e) {
        threw = true;
        thrownError = e;
    }

    if (!threw) {
        throw new AssertionError({
            message: message || "Expected promise to reject",
            actual: undefined,
            expected: error,
            operator: "rejects"
        });
    }

    // Check error type/pattern if provided
    if (error !== undefined) {
        if (error instanceof RegExp) {
            if (!error.test(String(thrownError.message || thrownError))) {
                throw new AssertionError({
                    message: message || `Error message did not match ${error}`,
                    actual: thrownError,
                    expected: error,
                    operator: "rejects"
                });
            }
        } else if (typeof error === "function") {
            if (!(thrownError instanceof error)) {
                throw new AssertionError({
                    message: message || `Expected ${error.name}`,
                    actual: thrownError,
                    expected: error,
                    operator: "rejects"
                });
            }
        }
    }
}

/**
 * Assert that async function does not reject
 *
 * @param asyncFn - Async function or Promise
 * @param message - Optional error message
 * @returns Promise that resolves if assertion passes
 */
export async function doesNotReject(
    asyncFn: (() => Promise<any>) | Promise<any>,
    message?: string
): Promise<void> {
    try {
        const promise = typeof asyncFn === "function" ? asyncFn() : asyncFn;
        await promise;
    } catch (e) {
        throw new AssertionError({
            message: message || `Expected promise not to reject, but it rejected: ${e}`,
            actual: e,
            expected: undefined,
            operator: "doesNotReject"
        });
    }
}

// =============================================================================
// Type Assertions
// =============================================================================

/**
 * Assert that value matches expected pattern
 *
 * @param value - String to test
 * @param regexp - Regular expression pattern
 * @param message - Optional error message
 * @throws AssertionError if value doesn't match
 *
 * @example
 * ```typescript
 * assert.match("hello world", /world/);  // passes
 * assert.match("hello", /world/);         // throws
 * ```
 */
export function match(value: string, regexp: RegExp, message?: string): void {
    if (!regexp.test(value)) {
        throw new AssertionError({
            message: message || `Expected ${inspect(value)} to match ${regexp}`,
            actual: value,
            expected: regexp,
            operator: "match"
        });
    }
}

/**
 * Assert that value does not match pattern
 *
 * @param value - String to test
 * @param regexp - Regular expression pattern
 * @param message - Optional error message
 * @throws AssertionError if value matches
 */
export function doesNotMatch(value: string, regexp: RegExp, message?: string): void {
    if (regexp.test(value)) {
        throw new AssertionError({
            message: message || `Expected ${inspect(value)} not to match ${regexp}`,
            actual: value,
            expected: regexp,
            operator: "doesNotMatch"
        });
    }
}

// =============================================================================
// Strict Mode
// =============================================================================

/**
 * Strict assertion mode (all methods use strict equality)
 *
 * @description
 * In strict mode, equal/notEqual use === instead of ==,
 * and deepEqual uses deepStrictEqual.
 *
 * @example
 * ```typescript
 * import { strict as assert } from "std/assert";
 *
 * assert.equal(1, "1");  // throws (strict mode)
 * ```
 */
export const strict = {
    ok,
    fail,
    equal: strictEqual,
    notEqual: notStrictEqual,
    strictEqual,
    notStrictEqual,
    deepEqual: deepStrictEqual,
    deepStrictEqual,
    notDeepEqual: notDeepStrictEqual,
    notDeepStrictEqual,
    throws,
    doesNotThrow,
    rejects,
    doesNotReject,
    match,
    doesNotMatch,
    AssertionError
};

// Default export
export default assert;
