function sumUpTo(n: number): number {
    let sum = 0;
    for (let i = 1; i <= n; i = i + 1) {
        sum = sum + i;
    }
    return sum;
}
