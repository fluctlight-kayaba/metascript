// Simple inheritance
class Animal {
    name: string;

    speak(): void {
        console.log("Some sound");
    }
}

class Dog extends Animal {
    bark(): void {
        console.log("Woof!");
    }
}
