class Calculator {
    value: number;

    add(x: number): void {
        this.value = this.value + x;
    }

    getValue(): number {
        return this.value;
    }
}
