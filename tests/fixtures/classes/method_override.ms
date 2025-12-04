class Shape {
    getArea(): number {
        return 0;
    }
}

class Rectangle extends Shape {
    width: number;
    height: number;

    getArea(): number {
        return this.width * this.height;
    }
}

class Circle extends Shape {
    radius: number;

    getArea(): number {
        return 3.14159 * this.radius * this.radius;
    }
}
