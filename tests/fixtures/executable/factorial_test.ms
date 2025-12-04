function factorial(n: number): number {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

function main(): number {
    const result1 = factorial(0);
    if (result1 !== 1) {
        console.log("FAIL: factorial(0) = " + result1 + ", expected 1");
        return 1;
    }

    const result2 = factorial(5);
    if (result2 !== 120) {
        console.log("FAIL: factorial(5) = " + result2 + ", expected 120");
        return 1;
    }

    console.log("PASS: All factorial() tests passed");
    return 0;
}
