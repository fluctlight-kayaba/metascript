/**
 * Path manipulation utilities
 *
 * Node.js-compatible path module for cross-platform path operations.
 * All functions are pure string operations - no syscalls.
 *
 * @module std/path
 * @example
 * ```typescript
 * import * as path from "std/path";
 *
 * const full = path.join("/home", "user", "file.txt");
 * const dir = path.dirname(full);      // "/home/user"
 * const base = path.basename(full);    // "file.txt"
 * const ext = path.extname(full);      // ".txt"
 * ```
 */

// Platform-specific separator
// TODO: Detect at compile time via @comptime
export const sep: string = "/";
export const delimiter: string = ":";

/**
 * Parsed path components
 */
export interface ParsedPath {
    root: string;
    dir: string;
    base: string;
    ext: string;
    name: string;
}

/**
 * Join path segments with platform separator
 *
 * @example
 * path.join("/home", "user", "docs") // "/home/user/docs"
 * path.join("a", "..", "b")          // "b"
 */
export function join(...paths: string[]): string {
    if (paths.length === 0) return ".";

    let result = "";
    for (const segment of paths) {
        if (segment === "") continue;

        if (result === "") {
            result = segment;
        } else {
            // Add separator if needed
            if (!result.endsWith(sep) && !segment.startsWith(sep)) {
                result = result + sep + segment;
            } else if (result.endsWith(sep) && segment.startsWith(sep)) {
                result = result + segment.substring(1);
            } else {
                result = result + segment;
            }
        }
    }

    return normalize(result);
}

/**
 * Resolve path segments to absolute path
 *
 * @example
 * path.resolve("/home", "user", "docs") // "/home/user/docs"
 * path.resolve("docs", "file.txt")      // "{cwd}/docs/file.txt"
 */
export function resolve(...paths: string[]): string {
    let result = "";

    // Process from right to left, stop at first absolute path
    for (let i = paths.length - 1; i >= 0; i--) {
        const segment = paths[i];
        if (segment === "") continue;

        if (result === "") {
            result = segment;
        } else {
            result = segment + sep + result;
        }

        // Stop if we hit an absolute path
        if (isAbsolute(segment)) break;
    }

    // If still relative, prepend cwd
    // Note: In compiled code, this will use actual cwd
    if (!isAbsolute(result)) {
        // TODO: Use process.cwd() when available
        result = sep + result;
    }

    return normalize(result);
}

/**
 * Get directory portion of path
 *
 * @example
 * path.dirname("/home/user/file.txt") // "/home/user"
 * path.dirname("file.txt")            // "."
 */
export function dirname(p: string): string {
    if (p === "") return ".";

    // Remove trailing slashes
    let end = p.length;
    while (end > 1 && p[end - 1] === sep) {
        end--;
    }

    // Find last separator
    let i = end - 1;
    while (i >= 0 && p[i] !== sep) {
        i--;
    }

    if (i < 0) return ".";
    if (i === 0) return sep;

    return p.substring(0, i);
}

/**
 * Get filename portion of path
 *
 * @example
 * path.basename("/home/user/file.txt")       // "file.txt"
 * path.basename("/home/user/file.txt", ".txt") // "file"
 */
export function basename(p: string, ext?: string): string {
    if (p === "") return "";

    // Remove trailing slashes
    let end = p.length;
    while (end > 0 && p[end - 1] === sep) {
        end--;
    }

    // Find start after last separator
    let start = end - 1;
    while (start >= 0 && p[start] !== sep) {
        start--;
    }
    start++;

    let result = p.substring(start, end);

    // Remove extension if provided
    if (ext !== undefined && result.endsWith(ext)) {
        result = result.substring(0, result.length - ext.length);
    }

    return result;
}

/**
 * Get file extension including the dot
 *
 * @example
 * path.extname("file.txt")      // ".txt"
 * path.extname("file.tar.gz")   // ".gz"
 * path.extname("file")          // ""
 * path.extname(".gitignore")    // ""
 */
export function extname(p: string): string {
    const base = basename(p);

    // Find last dot (but not at start)
    let i = base.length - 1;
    while (i > 0 && base[i] !== ".") {
        i--;
    }

    if (i <= 0) return "";

    return base.substring(i);
}

/**
 * Normalize path, resolving . and ..
 *
 * @example
 * path.normalize("/home/user/../docs/./file.txt") // "/home/docs/file.txt"
 * path.normalize("a//b///c")                       // "a/b/c"
 */
export function normalize(p: string): string {
    if (p === "") return ".";

    const isAbs = isAbsolute(p);
    const trailingSlash = p.endsWith(sep);

    // Split and filter
    const parts: string[] = [];
    const segments = p.split(sep);

    for (const segment of segments) {
        if (segment === "" || segment === ".") {
            continue;
        } else if (segment === "..") {
            if (parts.length > 0 && parts[parts.length - 1] !== "..") {
                parts.pop();
            } else if (!isAbs) {
                parts.push("..");
            }
        } else {
            parts.push(segment);
        }
    }

    let result = parts.join(sep);

    if (isAbs) {
        result = sep + result;
    }

    if (trailingSlash && result !== sep) {
        result = result + sep;
    }

    if (result === "") {
        return ".";
    }

    return result;
}

/**
 * Check if path is absolute
 *
 * @example
 * path.isAbsolute("/home/user") // true
 * path.isAbsolute("./file")     // false
 * path.isAbsolute("file")       // false
 */
export function isAbsolute(p: string): boolean {
    return p.length > 0 && p[0] === sep;
}

/**
 * Get relative path from 'from' to 'to'
 *
 * @example
 * path.relative("/home/user", "/home/user/docs") // "docs"
 * path.relative("/home/user", "/home/other")     // "../other"
 */
export function relative(from: string, to: string): string {
    const fromParts = normalize(from).split(sep).filter(p => p !== "");
    const toParts = normalize(to).split(sep).filter(p => p !== "");

    // Find common prefix
    let i = 0;
    while (i < fromParts.length && i < toParts.length && fromParts[i] === toParts[i]) {
        i++;
    }

    // Build relative path
    const upCount = fromParts.length - i;
    const result: string[] = [];

    for (let j = 0; j < upCount; j++) {
        result.push("..");
    }

    for (let j = i; j < toParts.length; j++) {
        result.push(toParts[j]);
    }

    return result.join(sep) || ".";
}

/**
 * Parse path into components
 *
 * @example
 * path.parse("/home/user/file.txt")
 * // { root: "/", dir: "/home/user", base: "file.txt", ext: ".txt", name: "file" }
 */
export function parse(p: string): ParsedPath {
    const root = isAbsolute(p) ? sep : "";
    const dir = dirname(p);
    const base = basename(p);
    const ext = extname(p);
    const name = ext ? base.substring(0, base.length - ext.length) : base;

    return { root, dir, base, ext, name };
}

/**
 * Build path from components
 *
 * @example
 * path.format({ dir: "/home/user", base: "file.txt" }) // "/home/user/file.txt"
 */
export function format(pathObject: ParsedPath): string {
    const dir = pathObject.dir || pathObject.root || "";
    const base = pathObject.base || (pathObject.name || "") + (pathObject.ext || "");

    if (dir === "") return base;
    if (dir === pathObject.root) return dir + base;

    return dir + sep + base;
}
