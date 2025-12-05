// Iterative Fibonacci - much faster than recursive
function fibIterative(n: number): number {
    if (n <= 1) {
        return n;
    }

    let prev = 0;
    let curr = 1;

    for (let i = 2; i <= n; i = i + 1) {
        const next = prev + curr;
        prev = curr;
        curr = next;
    }

    return curr;
}

// Calculate first 10 Fibonacci numbers
console.log(fibIterative(0));
console.log(fibIterative(1));
console.log(fibIterative(5));
console.log(fibIterative(10));
console.log(fibIterative(15));
console.log(fibIterative(20));
