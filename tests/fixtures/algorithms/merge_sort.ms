function merge(left: number[], right: number[]): number[] {
    const result: number[] = [];
    let i = 0;
    let j = 0;

    while (i < left.length && j < right.length) {
        if (left[i] < right[j]) {
            result.push(left[i]);
            i = i + 1;
        } else {
            result.push(right[j]);
            j = j + 1;
        }
    }

    while (i < left.length) {
        result.push(left[i]);
        i = i + 1;
    }

    while (j < right.length) {
        result.push(right[j]);
        j = j + 1;
    }

    return result;
}

function mergeSort(arr: number[]): number[] {
    if (arr.length <= 1) {
        return arr;
    }

    const mid = Math.floor(arr.length / 2);
    const left = arr.slice(0, mid);
    const right = arr.slice(mid);

    return merge(mergeSort(left), mergeSort(right));
}
