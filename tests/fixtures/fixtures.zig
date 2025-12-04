/// Unified Test Fixtures
///
/// Single source of truth for all test fixtures.
/// Uses @embedFile to load .ms files directly - no duplication!
///
/// Usage:
///   const fixtures = @import("fixtures.zig");
///   const source = fixtures.basic.simple_function;
///   const all = fixtures.all(); // Get all fixtures as slice

const std = @import("std");

// ============================================================================
// Basic Language Features
// ============================================================================

pub const basic = struct {
    pub const simple_function = @embedFile("basic/simple_function.ms");
    pub const factorial_recursive = @embedFile("basic/factorial_recursive.ms");
    pub const factorial_iterative = @embedFile("basic/factorial_iterative.ms");
    pub const fibonacci = @embedFile("basic/fibonacci.ms");
};

// ============================================================================
// Control Flow
// ============================================================================

pub const control_flow = struct {
    pub const while_loop_counter = @embedFile("control_flow/while_loop_counter.ms");
    pub const for_loop_sum = @embedFile("control_flow/for_loop_sum.ms");
    pub const nested_loops = @embedFile("control_flow/nested_loops.ms");
    pub const early_return = @embedFile("control_flow/early_return.ms");
};

// ============================================================================
// Variables
// ============================================================================

pub const variables = struct {
    pub const shadowing_simple = @embedFile("variables/shadowing_simple.ms");
    pub const shadowing_multiple = @embedFile("variables/shadowing_multiple.ms");
};

// ============================================================================
// Objects
// ============================================================================

pub const objects = struct {
    pub const object_literal = @embedFile("objects/object_literal.ms");
    pub const member_access = @embedFile("objects/member_access.ms");
};

// ============================================================================
// Arrays
// ============================================================================

pub const arrays = struct {
    pub const array_operations = @embedFile("arrays/array_operations.ms");
};

// ============================================================================
// Classes
// ============================================================================

pub const classes = struct {
    pub const simple_class = @embedFile("classes/simple_class.ms");
    pub const class_with_methods = @embedFile("classes/class_with_methods.ms");
    pub const inheritance_simple = @embedFile("classes/inheritance_simple.ms");
    pub const method_override = @embedFile("classes/method_override.ms");
    pub const polymorphism = @embedFile("classes/polymorphism.ms");
};

// ============================================================================
// Algorithms
// ============================================================================

pub const algorithms = struct {
    pub const quicksort = @embedFile("algorithms/quicksort.ms");
    pub const merge_sort = @embedFile("algorithms/merge_sort.ms");
    pub const linked_list = @embedFile("algorithms/linked_list.ms");
    pub const tree = @embedFile("algorithms/tree.ms");
    pub const binary_search = @embedFile("algorithms/binary_search.ms");
    pub const is_prime = @embedFile("algorithms/is_prime.ms");
};

// ============================================================================
// Patterns
// ============================================================================

pub const patterns = struct {
    pub const builder = @embedFile("patterns/builder.ms");
    pub const factory = @embedFile("patterns/factory.ms");
    pub const observer = @embedFile("patterns/observer.ms");
    pub const singleton = @embedFile("patterns/singleton.ms");
};

// ============================================================================
// Executable (with main() for testing)
// ============================================================================

pub const executable = struct {
    pub const simple_function_test = @embedFile("executable/simple_function_test.ms");
    pub const factorial_test = @embedFile("executable/factorial_test.ms");
};

// ============================================================================
// Fixture Metadata
// ============================================================================

pub const Fixture = struct {
    name: []const u8,
    category: Category,
    source: []const u8,

    pub const Category = enum {
        basic,
        control_flow,
        variables,
        objects,
        arrays,
        classes,
        algorithms,
        patterns,
        executable,
    };
};

/// Get all fixtures as a slice for iteration
pub fn all() []const Fixture {
    return &ALL_FIXTURES;
}

/// Get fixtures by category
pub fn byCategory(category: Fixture.Category) []const Fixture {
    var count: usize = 0;
    for (ALL_FIXTURES) |f| {
        if (f.category == category) count += 1;
    }

    // Note: This is a comptime function, returns static slice
    comptime var result: [count]Fixture = undefined;
    comptime var i: usize = 0;
    inline for (ALL_FIXTURES) |f| {
        if (f.category == category) {
            result[i] = f;
            i += 1;
        }
    }
    return &result;
}

const ALL_FIXTURES = [_]Fixture{
    // Basic
    .{ .name = "simple_function", .category = .basic, .source = basic.simple_function },
    .{ .name = "factorial_recursive", .category = .basic, .source = basic.factorial_recursive },
    .{ .name = "factorial_iterative", .category = .basic, .source = basic.factorial_iterative },
    .{ .name = "fibonacci", .category = .basic, .source = basic.fibonacci },

    // Control Flow
    .{ .name = "while_loop_counter", .category = .control_flow, .source = control_flow.while_loop_counter },
    .{ .name = "for_loop_sum", .category = .control_flow, .source = control_flow.for_loop_sum },
    .{ .name = "nested_loops", .category = .control_flow, .source = control_flow.nested_loops },
    .{ .name = "early_return", .category = .control_flow, .source = control_flow.early_return },

    // Variables
    .{ .name = "shadowing_simple", .category = .variables, .source = variables.shadowing_simple },
    .{ .name = "shadowing_multiple", .category = .variables, .source = variables.shadowing_multiple },

    // Objects
    .{ .name = "object_literal", .category = .objects, .source = objects.object_literal },
    .{ .name = "member_access", .category = .objects, .source = objects.member_access },

    // Arrays
    .{ .name = "array_operations", .category = .arrays, .source = arrays.array_operations },

    // Classes
    .{ .name = "simple_class", .category = .classes, .source = classes.simple_class },
    .{ .name = "class_with_methods", .category = .classes, .source = classes.class_with_methods },
    .{ .name = "inheritance_simple", .category = .classes, .source = classes.inheritance_simple },
    .{ .name = "method_override", .category = .classes, .source = classes.method_override },
    .{ .name = "polymorphism", .category = .classes, .source = classes.polymorphism },

    // Algorithms
    .{ .name = "quicksort", .category = .algorithms, .source = algorithms.quicksort },
    .{ .name = "merge_sort", .category = .algorithms, .source = algorithms.merge_sort },
    .{ .name = "linked_list", .category = .algorithms, .source = algorithms.linked_list },
    .{ .name = "tree", .category = .algorithms, .source = algorithms.tree },
    .{ .name = "binary_search", .category = .algorithms, .source = algorithms.binary_search },
    .{ .name = "is_prime", .category = .algorithms, .source = algorithms.is_prime },

    // Patterns
    .{ .name = "builder", .category = .patterns, .source = patterns.builder },
    .{ .name = "factory", .category = .patterns, .source = patterns.factory },
    .{ .name = "observer", .category = .patterns, .source = patterns.observer },
    .{ .name = "singleton", .category = .patterns, .source = patterns.singleton },

    // Executable
    .{ .name = "simple_function_test", .category = .executable, .source = executable.simple_function_test },
    .{ .name = "factorial_test", .category = .executable, .source = executable.factorial_test },
};

// ============================================================================
// Bug Reproduction Fixtures (inline - for specific test cases)
// ============================================================================

pub const bugs = struct {
    /// Erlang loop closure bug - variables captured incorrectly
    pub const erlang_loop_closure =
        \\function countdown(count: number): number {
        \\    while (count > 0) {
        \\        count = count - 1;
        \\    }
        \\    return count;
        \\}
    ;

    /// Erlang early return bug - code continues after case expression
    pub const erlang_early_return =
        \\function absoluteValue(x: number): number {
        \\    if (x < 0) {
        \\        return -x;
        \\    }
        \\    return x;
        \\}
    ;
};

// ============================================================================
// Advanced Features (inline - not yet as .ms files)
// ============================================================================

pub const advanced = struct {
    pub const comprehensive_demo =
        \\function fibonacci(n: number): number {
        \\    if (n <= 1) {
        \\        return n;
        \\    }
        \\    return fibonacci(n - 1) + fibonacci(n - 2);
        \\}
        \\
        \\function main(): number {
        \\    return fibonacci(10);
        \\}
    ;

    pub const generics_basic =
        \\function identity<T>(x: T): T {
        \\    return x;
        \\}
    ;

    pub const generics_constraints =
        \\interface HasLength {
        \\    length: number;
        \\}
        \\
        \\function getLength<T extends HasLength>(x: T): number {
        \\    return x.length;
        \\}
    ;

    pub const generic_class =
        \\class Box<T> {
        \\    value: T;
        \\    constructor(value: T) {
        \\        this.value = value;
        \\    }
        \\    getValue(): T {
        \\        return this.value;
        \\    }
        \\}
    ;

    pub const union_types =
        \\type StringOrNumber = string | number;
        \\
        \\function process(x: StringOrNumber): string {
        \\    if (typeof x === "string") {
        \\        return x;
        \\    }
        \\    return x.toString();
        \\}
    ;

    pub const intersection_types =
        \\interface HasName {
        \\    name: string;
        \\}
        \\
        \\interface HasAge {
        \\    age: number;
        \\}
        \\
        \\type Person = HasName & HasAge;
    ;

    pub const optional_chaining =
        \\interface User {
        \\    address?: {
        \\        city?: string;
        \\    };
        \\}
        \\
        \\function getCity(user: User): string | undefined {
        \\    return user?.address?.city;
        \\}
    ;

    pub const nullish_coalescing =
        \\function getDefault(x: number | null): number {
        \\    return x ?? 0;
        \\}
    ;

    pub const type_guards =
        \\function isString(x: unknown): x is string {
        \\    return typeof x === "string";
        \\}
    ;

    pub const closures =
        \\function makeCounter(): () => number {
        \\    let count = 0;
        \\    return function(): number {
        \\        count = count + 1;
        \\        return count;
        \\    };
        \\}
    ;

    pub const higher_order_functions =
        \\function map<T, U>(arr: T[], f: (x: T) => U): U[] {
        \\    const result: U[] = [];
        \\    for (let i = 0; i < arr.length; i = i + 1) {
        \\        result.push(f(arr[i]));
        \\    }
        \\    return result;
        \\}
    ;

    pub const arrow_functions =
        \\const add = (a: number, b: number): number => a + b;
        \\const square = (x: number): number => x * x;
    ;

    pub const spread_operator =
        \\function sum(...numbers: number[]): number {
        \\    let total = 0;
        \\    for (let i = 0; i < numbers.length; i = i + 1) {
        \\        total = total + numbers[i];
        \\    }
        \\    return total;
        \\}
    ;

    pub const destructuring =
        \\function getCoords(): { x: number; y: number } {
        \\    return { x: 10, y: 20 };
        \\}
        \\
        \\const { x, y } = getCoords();
    ;

    pub const async_await =
        \\async function fetchData(): Promise<string> {
        \\    return "data";
        \\}
        \\
        \\async function main(): Promise<void> {
        \\    const data = await fetchData();
        \\}
    ;

    pub const recursive_types =
        \\interface TreeNode {
        \\    value: number;
        \\    left?: TreeNode;
        \\    right?: TreeNode;
        \\}
    ;

    pub const enums =
        \\enum Color {
        \\    Red,
        \\    Green,
        \\    Blue
        \\}
        \\
        \\function getColor(): Color {
        \\    return Color.Red;
        \\}
    ;

    pub const tuple_types =
        \\type Point = [number, number];
        \\
        \\function distance(p1: Point, p2: Point): number {
        \\    const dx = p2[0] - p1[0];
        \\    const dy = p2[1] - p1[1];
        \\    return Math.sqrt(dx * dx + dy * dy);
        \\}
    ;

    pub const mapped_types =
        \\type Readonly<T> = {
        \\    readonly [P in keyof T]: T[P];
        \\};
    ;

    pub const conditional_types =
        \\type IsString<T> = T extends string ? true : false;
    ;
};

// ============================================================================
// Convenience accessors (for backwards compatibility)
// ============================================================================

// Basic
pub const SIMPLE_FUNCTION = basic.simple_function;
pub const FACTORIAL_RECURSIVE = basic.factorial_recursive;
pub const FACTORIAL_ITERATIVE = basic.factorial_iterative;
pub const FIBONACCI = basic.fibonacci;

// Control Flow
pub const WHILE_LOOP_COUNTER = control_flow.while_loop_counter;
pub const FOR_LOOP_SUM = control_flow.for_loop_sum;
pub const NESTED_LOOPS = control_flow.nested_loops;
pub const EARLY_RETURN = control_flow.early_return;

// Variables
pub const VARIABLE_SHADOWING_SIMPLE = variables.shadowing_simple;
pub const VARIABLE_SHADOWING_MULTIPLE = variables.shadowing_multiple;

// Objects
pub const OBJECT_LITERAL = objects.object_literal;
pub const OBJECT_MEMBER_ACCESS = objects.member_access;

// Arrays
pub const ARRAY_OPERATIONS = arrays.array_operations;

// Classes
pub const SIMPLE_CLASS = classes.simple_class;
pub const CLASS_WITH_METHODS = classes.class_with_methods;
pub const INHERITANCE_SIMPLE = classes.inheritance_simple;
pub const METHOD_OVERRIDE = classes.method_override;
pub const POLYMORPHISM = classes.polymorphism;

// Algorithms
pub const QUICKSORT = algorithms.quicksort;
pub const MERGE_SORT = algorithms.merge_sort;
pub const LINKED_LIST = algorithms.linked_list;
pub const BINARY_SEARCH = algorithms.binary_search;
pub const IS_PRIME = algorithms.is_prime;

// Patterns
pub const BUILDER_PATTERN = patterns.builder;
pub const FACTORY_PATTERN = patterns.factory;
pub const OBSERVER_PATTERN = patterns.observer;
pub const SINGLETON_PATTERN = patterns.singleton;

// Executable
pub const SIMPLE_FUNCTION_WITH_TESTS = executable.simple_function_test;
pub const FACTORIAL_WITH_TESTS = executable.factorial_test;

// Bugs (for regression testing)
pub const ERLANG_BUG_LOOP_CLOSURE = bugs.erlang_loop_closure;
pub const ERLANG_BUG_EARLY_RETURN = bugs.erlang_early_return;

// Advanced features
pub const COMPREHENSIVE_DEMO = advanced.comprehensive_demo;
pub const GENERICS_BASIC = advanced.generics_basic;
pub const GENERICS_CONSTRAINTS = advanced.generics_constraints;
pub const GENERIC_CLASS = advanced.generic_class;
pub const UNION_TYPES = advanced.union_types;
pub const INTERSECTION_TYPES = advanced.intersection_types;
pub const OPTIONAL_CHAINING = advanced.optional_chaining;
pub const NULLISH_COALESCING = advanced.nullish_coalescing;
pub const TYPE_GUARDS = advanced.type_guards;
pub const CLOSURES = advanced.closures;
pub const HIGHER_ORDER_FUNCTIONS = advanced.higher_order_functions;
pub const ARROW_FUNCTIONS = advanced.arrow_functions;
pub const SPREAD_OPERATOR = advanced.spread_operator;
pub const DESTRUCTURING = advanced.destructuring;
pub const ASYNC_AWAIT = advanced.async_await;
pub const RECURSIVE_TYPES = advanced.recursive_types;
pub const ENUMS = advanced.enums;
pub const TUPLE_TYPES = advanced.tuple_types;
pub const MAPPED_TYPES = advanced.mapped_types;
pub const CONDITIONAL_TYPES = advanced.conditional_types;

// ============================================================================
// Tests
// ============================================================================

test "fixtures: all fixtures load successfully" {
    const all_fixtures = all();
    try std.testing.expect(all_fixtures.len > 0);

    for (all_fixtures) |fixture| {
        try std.testing.expect(fixture.source.len > 0);
        try std.testing.expect(fixture.name.len > 0);
    }
}

test "fixtures: basic category has expected fixtures" {
    try std.testing.expect(basic.simple_function.len > 0);
    try std.testing.expect(basic.factorial_recursive.len > 0);
}

test "fixtures: backwards compatibility aliases work" {
    try std.testing.expectEqualStrings(SIMPLE_FUNCTION, basic.simple_function);
    try std.testing.expectEqualStrings(QUICKSORT, algorithms.quicksort);
}
