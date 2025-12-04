/// Executable Test Fixtures
///
/// These fixtures contain:
/// 1. Functionality to test
/// 2. Embedded unit tests (assertions)
/// 3. main() function that runs tests and returns exit code
///
/// The generated code in each backend will compile and execute these tests.
/// Exit code 0 = all tests passed, non-zero = failure
///
/// IMPLEMENTATION NOTE:
/// Fixtures are currently inline strings due to Zig build system constraints with @embedFile().
/// However, corresponding .ms files exist in tests/fixtures/executable/ for:
/// - Manual compilation: `msc compile tests/fixtures/executable/simple_function_test.ms`
/// - LSP support in editor (syntax highlighting, hover, etc.)
/// - Examples and documentation
/// - Future conversion to @embedFile() when build system supports it

// ============================================================================
// Executable fixtures with self-testing main()
// ============================================================================

// Source: tests/fixtures/executable/simple_function_test.ms
pub const SIMPLE_FUNCTION_WITH_TESTS =
    \\function add(a: number, b: number): number {
    \\    return a + b;
    \\}
    \\
    \\function main(): number {
    \\    const result1 = add(2, 3);
    \\    if (result1 !== 5) {
    \\        console.log("FAIL: add(2, 3) = " + result1 + ", expected 5");
    \\        return 1;
    \\    }
    \\
    \\    const result2 = add(-1, 1);
    \\    if (result2 !== 0) {
    \\        console.log("FAIL: add(-1, 1) = " + result2 + ", expected 0");
    \\        return 1;
    \\    }
    \\
    \\    console.log("PASS: All add() tests passed");
    \\    return 0;
    \\}
;

// Source: tests/fixtures/executable/factorial_test.ms
pub const FACTORIAL_WITH_TESTS =
    \\function factorial(n: number): number {
    \\    if (n <= 1) {
    \\        return 1;
    \\    }
    \\    return n * factorial(n - 1);
    \\}
    \\
    \\function main(): number {
    \\    const result1 = factorial(5);
    \\    if (result1 !== 120) {
    \\        console.log("FAIL: factorial(5) = " + result1 + ", expected 120");
    \\        return 1;
    \\    }
    \\
    \\    const result2 = factorial(0);
    \\    if (result2 !== 1) {
    \\        console.log("FAIL: factorial(0) = " + result2 + ", expected 1");
    \\        return 1;
    \\    }
    \\
    \\    console.log("PASS: All factorial() tests passed");
    \\    return 0;
    \\}
;

// ============================================================================
// Inline fixtures (TODO: convert to .ms files)
// ============================================================================

pub const CLASS_SIMPLE_WITH_TESTS =
    \\class Point {
    \\    x: number;
    \\    y: number;
    \\}
    \\
    \\function main(): number {
    \\    const p = new Point();
    \\    p.x = 10;
    \\    p.y = 20;
    \\
    \\    if (p.x !== 10) {
        \\        console.log("FAIL: p.x = " + p.x + ", expected 10");
    \\        return 1;
    \\    }
    \\
    \\    if (p.y !== 20) {
    \\        console.log("FAIL: p.y = " + p.y + ", expected 20");
    \\        return 1;
    \\    }
    \\
    \\    console.log("PASS: Point class works correctly");
    \\    return 0;
    \\}
;

pub const CLASS_WITH_METHODS_TESTS =
    \\class Calculator {
    \\    value: number;
    \\
    \\    add(x: number): void {
    \\        this.value = this.value + x;
    \\    }
    \\
    \\    getValue(): number {
    \\        return this.value;
    \\    }
    \\}
    \\
    \\function main(): number {
    \\    const calc = new Calculator();
    \\    calc.value = 0;
    \\
    \\    calc.add(5);
    \\    if (calc.getValue() !== 5) {
    \\        console.log("FAIL: After add(5), value = " + calc.getValue() + ", expected 5");
    \\        return 1;
    \\    }
    \\
    \\    calc.add(10);
    \\    if (calc.getValue() !== 15) {
    \\        console.log("FAIL: After add(10), value = " + calc.getValue() + ", expected 15");
    \\        return 1;
    \\    }
    \\
    \\    console.log("PASS: Calculator class works correctly");
    \\    return 0;
    \\}
;

pub const LOOP_SUM_WITH_TESTS =
    \\function sumUpTo(n: number): number {
    \\    let sum = 0;
    \\    let i = 1;
    \\    while (i <= n) {
    \\        sum = sum + i;
    \\        i = i + 1;
    \\    }
    \\    return sum;
    \\}
    \\
    \\function main(): number {
    \\    const result1 = sumUpTo(10);
    \\    if (result1 !== 55) {
    \\        console.log("FAIL: sumUpTo(10) = " + result1 + ", expected 55");
    \\        return 1;
    \\    }
    \\
    \\    const result2 = sumUpTo(100);
    \\    if (result2 !== 5050) {
    \\        console.log("FAIL: sumUpTo(100) = " + result2 + ", expected 5050");
    \\        return 1;
    \\    }
    \\
    \\    console.log("PASS: sumUpTo() tests passed");
    \\    return 0;
    \\}
;

pub const VARIABLE_SHADOWING_TEST =
    \\function shadowTest(): number {
    \\    let x = 10;
    \\    x = x + 5;  // x should be 15
    \\    x = x * 2;  // x should be 30
    \\    return x;
    \\}
    \\
    \\function main(): number {
    \\    const result = shadowTest();
    \\    if (result !== 30) {
    \\        console.log("FAIL: shadowTest() = " + result + ", expected 30");
    \\        return 1;
    \\    }
    \\
    \\    console.log("PASS: Variable shadowing works correctly");
    \\    return 0;
    \\}
;

pub const ARRAY_OPS_WITH_TESTS =
    \\function arrayTest(): number {
    \\    const arr: number[] = [];
    \\    arr.push(10);
    \\    arr.push(20);
    \\    arr.push(30);
    \\
    \\    if (arr.length !== 3) {
    \\        console.log("FAIL: arr.length = " + arr.length + ", expected 3");
    \\        return 1;
    \\    }
    \\
    \\    if (arr[0] !== 10 || arr[1] !== 20 || arr[2] !== 30) {
    \\        console.log("FAIL: Array values incorrect");
    \\        return 1;
    \\    }
    \\
    \\    return 0;
    \\}
    \\
    \\function main(): number {
    \\    const result = arrayTest();
    \\    if (result !== 0) {
    \\        return result;
    \\    }
    \\
    \\    console.log("PASS: Array operations work correctly");
    \\    return 0;
    \\}
;

pub const OBJECT_WITH_TESTS =
    \\function objectTest(): number {
    \\    const person = {
    \\        name: "Alice",
    \\        age: 30
    \\    };
    \\
    \\    if (person.age !== 30) {
    \\        console.log("FAIL: person.age = " + person.age + ", expected 30");
    \\        return 1;
    \\    }
    \\
    \\    person.age = 31;
    \\    if (person.age !== 31) {
    \\        console.log("FAIL: After update, person.age = " + person.age + ", expected 31");
    \\        return 1;
    \\    }
    \\
    \\    return 0;
    \\}
    \\
    \\function main(): number {
    \\    const result = objectTest();
    \\    if (result !== 0) {
    \\        return result;
    \\    }
    \\
    \\    console.log("PASS: Object operations work correctly");
    \\    return 0;
    \\}
;

pub const EARLY_RETURN_WITH_TESTS =
    \\function findFirst(target: number): number {
    \\    let i = 0;
    \\    while (i < 10) {
    \\        if (i === target) {
    \\            return i;
    \\        }
    \\        i = i + 1;
    \\    }
    \\    return -1;
    \\}
    \\
    \\function main(): number {
    \\    const result1 = findFirst(5);
    \\    if (result1 !== 5) {
    \\        console.log("FAIL: findFirst(5) = " + result1 + ", expected 5");
    \\        return 1;
    \\    }
    \\
    \\    const result2 = findFirst(20);
    \\    if (result2 !== -1) {
    \\        console.log("FAIL: findFirst(20) = " + result2 + ", expected -1");
    \\        return 1;
    \\    }
    \\
    \\    console.log("PASS: Early return works correctly");
    \\    return 0;
    \\}
;
