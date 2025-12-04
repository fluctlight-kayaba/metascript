class Node {
    value: number;
    next: Node | null;
}

class LinkedList {
    head: Node | null;

    append(value: number): void {
        const newNode = new Node();
        newNode.value = value;
        newNode.next = null;

        if (this.head === null) {
            this.head = newNode;
        } else {
            let current = this.head;
            while (current.next !== null) {
                current = current.next;
            }
            current.next = newNode;
        }
    }

    find(value: number): Node | null {
        let current = this.head;
        while (current !== null) {
            if (current.value === value) {
                return current;
            }
            current = current.next;
        }
        return null;
    }

    length(): number {
        let count = 0;
        let current = this.head;
        while (current !== null) {
            count = count + 1;
            current = current.next;
        }
        return count;
    }
}
