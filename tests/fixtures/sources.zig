/// Test Fixtures: Source Code Samples
///
/// Contains Metascript source code samples for testing lexer, parser, and compiler.
/// Organized by complexity and feature coverage.

// ============================================================================
// Minimal / Atomic Fixtures
// ============================================================================

/// Empty source
pub const EMPTY = "";

/// Just whitespace
pub const WHITESPACE_ONLY = "   \t\n  \n\t  ";

/// Single line comment
pub const SINGLE_COMMENT = "// this is a comment";

/// Multi-line comment
pub const MULTI_COMMENT =
    \\/* multi-line
    \\   comment */
;

// ============================================================================
// Literals
// ============================================================================

pub const NUMBER_INT = "42";
pub const NUMBER_FLOAT = "3.14159";
pub const NUMBER_HEX = "0x1A2B";
pub const NUMBER_BINARY = "0b1010";

pub const STRING_SIMPLE = "\"hello world\"";
pub const STRING_ESCAPES = "\"hello\\nworld\\t!\"";
pub const STRING_SINGLE_QUOTE = "'single quotes'";

pub const BOOLEAN_TRUE = "true";
pub const BOOLEAN_FALSE = "false";
pub const NULL_LITERAL = "null";

// ============================================================================
// Variable Declarations
// ============================================================================

pub const VAR_CONST =
    \\const x = 42;
;

pub const VAR_LET =
    \\let y = "hello";
;

pub const VAR_TYPED =
    \\const name: string = "test";
    \\let count: number = 0;
;

pub const VAR_MULTIPLE =
    \\const a = 1, b = 2, c = 3;
;

// ============================================================================
// Functions
// ============================================================================

pub const FUNC_SIMPLE =
    \\function hello() {
    \\    return "hello";
    \\}
;

pub const FUNC_WITH_PARAMS =
    \\function add(a: number, b: number): number {
    \\    return a + b;
    \\}
;

pub const FUNC_ARROW =
    \\const add = (a, b) => a + b;
;

pub const FUNC_ARROW_BLOCK =
    \\const greet = (name: string) => {
    \\    return "Hello, " + name;
    \\};
;

pub const FUNC_ASYNC =
    \\async function fetchData() {
    \\    return await fetch("/api");
    \\}
;

// ============================================================================
// Classes
// ============================================================================

pub const CLASS_EMPTY =
    \\class Empty {}
;

pub const CLASS_WITH_PROPERTIES =
    \\class User {
    \\    name: string;
    \\    age: number;
    \\}
;

pub const CLASS_WITH_CONSTRUCTOR =
    \\class Point {
    \\    x: number;
    \\    y: number;
    \\
    \\    constructor(x: number, y: number) {
    \\        this.x = x;
    \\        this.y = y;
    \\    }
    \\}
;

pub const CLASS_WITH_METHODS =
    \\class Calculator {
    \\    add(a: number, b: number): number {
    \\        return a + b;
    \\    }
    \\
    \\    subtract(a: number, b: number): number {
    \\        return a - b;
    \\    }
    \\}
;

pub const CLASS_WITH_EXTENDS =
    \\class Animal {
    \\    name: string;
    \\}
    \\
    \\class Dog extends Animal {
    \\    breed: string;
    \\}
;

pub const CLASS_WITH_IMPLEMENTS =
    \\interface Printable {
    \\    print(): void;
    \\}
    \\
    \\class Document implements Printable {
    \\    print(): void {
    \\        console.log("printing");
    \\    }
    \\}
;

// ============================================================================
// Interfaces
// ============================================================================

pub const INTERFACE_SIMPLE =
    \\interface Named {
    \\    name: string;
    \\}
;

pub const INTERFACE_WITH_METHODS =
    \\interface Repository {
    \\    find(id: number): Entity;
    \\    save(entity: Entity): void;
    \\    delete(id: number): boolean;
    \\}
;

pub const INTERFACE_EXTENDS =
    \\interface Base {
    \\    id: number;
    \\}
    \\
    \\interface Extended extends Base {
    \\    name: string;
    \\}
;

// ============================================================================
// Type Aliases
// ============================================================================

pub const TYPE_ALIAS_SIMPLE =
    \\type ID = number;
;

pub const TYPE_ALIAS_UNION =
    \\type Result = Success | Error;
;

pub const TYPE_ALIAS_GENERIC =
    \\type Container<T> = { value: T };
;

// ============================================================================
// Control Flow
// ============================================================================

pub const IF_SIMPLE =
    \\if (condition) {
    \\    doSomething();
    \\}
;

pub const IF_ELSE =
    \\if (x > 0) {
    \\    positive();
    \\} else {
    \\    nonPositive();
    \\}
;

pub const IF_ELSE_IF =
    \\if (x > 0) {
    \\    positive();
    \\} else if (x < 0) {
    \\    negative();
    \\} else {
    \\    zero();
    \\}
;

pub const WHILE_LOOP =
    \\while (i < 10) {
    \\    i = i + 1;
    \\}
;

pub const FOR_LOOP =
    \\for (let i = 0; i < 10; i++) {
    \\    console.log(i);
    \\}
;

pub const SWITCH_STMT =
    \\switch (day) {
    \\    case "Monday":
    \\        work();
    \\        break;
    \\    case "Saturday":
    \\    case "Sunday":
    \\        rest();
    \\        break;
    \\    default:
    \\        work();
    \\}
;

// ============================================================================
// Imports / Exports
// ============================================================================

pub const IMPORT_NAMED =
    \\import { foo, bar } from "module";
;

pub const IMPORT_DEFAULT =
    \\import MyModule from "module";
;

pub const IMPORT_ALIASED =
    \\import { foo as bar } from "module";
;

pub const IMPORT_ALL =
    \\import * as Utils from "utils";
;

pub const EXPORT_NAMED =
    \\export { foo, bar };
;

pub const EXPORT_DEFAULT =
    \\export default function main() {}
;

pub const EXPORT_CONST =
    \\export const PI = 3.14159;
;

// ============================================================================
// Macros (Metascript-specific)
// ============================================================================

pub const MACRO_DERIVE =
    \\@derive(Eq, Hash)
    \\class User {
    \\    id: number;
    \\    name: string;
    \\}
;

pub const MACRO_DERIVE_EQ =
    \\@deriveEq
    \\class Point {
    \\    x: number;
    \\    y: number;
    \\}
;

pub const MACRO_COMPTIME =
    \\const config = @comptime {
    \\    return loadConfig();
    \\};
;

pub const MACRO_DEFINITION =
    \\@macro function log(target) {
    \\    console.log("called:", target.name);
    \\    return target;
    \\}
;

/// System macro: @extern in function body
pub const MACRO_EXTERN =
    \\export function platform(): string {
    \\    @extern("ms_os_platform");
    \\}
;

/// System macro: @target with block body
pub const MACRO_TARGET_BLOCK =
    \\export macro readFile(path: string): string {
    \\    @target("c") {
    \\        @emit("ms_read_file($path)")
    \\    }
    \\    @target("js") {
    \\        @emit("require('fs').readFileSync($path)")
    \\    }
    \\}
;

/// System macro: @emit as statement
pub const MACRO_EMIT =
    \\export macro inline_asm(): void {
    \\    @emit("asm volatile(\"nop\")");
    \\}
;

/// Export macro declaration
pub const MACRO_EXPORT =
    \\export macro validate(schema: string): void {
    \\    @comptime {
    \\        // validation logic
    \\    }
    \\}
;

// ============================================================================
// Complex / Integration Fixtures
// ============================================================================

pub const FULL_MODULE =
    \\// A complete module example
    \\import { deriveEq } from "std/macros/derive";
    \\
    \\interface Identifiable {
    \\    id: number;
    \\}
    \\
    \\@deriveEq
    \\class User implements Identifiable {
    \\    id: number;
    \\    name: string;
    \\    email: string;
    \\
    \\    constructor(id: number, name: string, email: string) {
    \\        this.id = id;
    \\        this.name = name;
    \\        this.email = email;
    \\    }
    \\
    \\    greet(): string {
    \\        return "Hello, " + this.name;
    \\    }
    \\}
    \\
    \\export { User };
;

pub const ERROR_INVALID_SYNTAX =
    \\class {
    \\    // missing class name
    \\}
;

pub const ERROR_UNCLOSED_BRACE =
    \\function foo() {
    \\    return 42;
    \\// missing closing brace
;

pub const ERROR_UNEXPECTED_TOKEN =
    \\const x = ;
;

// ============================================================================
// Typed Arrays (Primitive Array Types)
// ============================================================================

/// 1D typed array with primitive element type
pub const ARRAY_1D_INT32 =
    \\let nums: int32[] = [1, 2, 3];
;

/// 1D typed array with float element type
pub const ARRAY_1D_FLOAT64 =
    \\let values: float64[] = [1.1, 2.2, 3.3];
;

/// 2D typed array (array of arrays)
pub const ARRAY_2D_INT32 =
    \\let matrix: int32[][] = [[1, 2], [3, 4]];
;

/// Multiple typed arrays with different primitive types
pub const ARRAY_MULTIPLE_TYPES =
    \\let ints: int32[] = [1, 2, 3];
    \\let floats: float64[] = [1.1, 2.2, 3.3];
    \\let bytes: uint8[] = [0, 1, 2, 255];
;
