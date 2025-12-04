function findFirst(arr: number[], target: number): number {
    for (let i = 0; i < arr.length; i = i + 1) {
        if (arr[i] === target) {
            return i;
        }
    }
    return -1;
}
