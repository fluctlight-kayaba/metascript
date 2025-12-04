class Vehicle {
    speed: number;

    move(): void {
        console.log("Moving at speed: " + this.speed);
    }
}

class Car extends Vehicle {
    wheels: number;

    move(): void {
        console.log("Car driving at " + this.speed + " mph");
    }
}

class Boat extends Vehicle {
    propellers: number;

    move(): void {
        console.log("Boat sailing at " + this.speed + " knots");
    }
}
