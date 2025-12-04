function quicksort(arr: number[]): number[] {
    if (arr.length <= 1) {
        return arr;
    }
    
    const pivot = arr[0];
    const less: number[] = [];
    const greater: number[] = [];
    
    for (let i = 1; i < arr.length; i = i + 1) {
        if (arr[i] < pivot) {
            less.push(arr[i]);
        } else {
            greater.push(arr[i]);
        }
    }
    
    return quicksort(less).concat([pivot]).concat(quicksort(greater));
}
