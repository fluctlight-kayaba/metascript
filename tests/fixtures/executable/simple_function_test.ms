function add(a: number, b: number): number {
    return a + b;
}

function main(): number {
    const result1 = add(2, 3);
    if (result1 !== 5) {
        console.log("FAIL: add(2, 3) = " + result1 + ", expected 5");
        return 1;
    }

    const result2 = add(0, 0);
    if (result2 !== 0) {
        console.log("FAIL: add(0, 0) = " + result2 + ", expected 0");
        return 1;
    }

    console.log("PASS: All add() tests passed");
    return 0;
}
