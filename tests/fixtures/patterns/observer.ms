class Observer {
    name: string;

    update(message: string): void {
        console.log(this.name + " received: " + message);
    }
}

class Subject {
    observers: Observer[];
    observerCount: number;

    constructor() {
        this.observerCount = 0;
    }

    attach(observer: Observer): void {
        this.observers[this.observerCount] = observer;
        this.observerCount = this.observerCount + 1;
    }

    notify(message: string): void {
        let i = 0;
        while (i < this.observerCount) {
            this.observers[i].update(message);
            i = i + 1;
        }
    }
}
