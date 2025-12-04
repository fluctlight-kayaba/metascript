function printGrid(): void {
    for (let i = 0; i < 3; i = i + 1) {
        for (let j = 0; j < 3; j = j + 1) {
            console.log(i * 3 + j);
        }
    }
}
