class Pizza {
    size: string;
    cheese: boolean;
    pepperoni: boolean;
    bacon: boolean;
}

class PizzaBuilder {
    size: string;
    cheese: boolean;
    pepperoni: boolean;
    bacon: boolean;

    setSize(size: string): PizzaBuilder {
        this.size = size;
        return this;
    }

    addCheese(): PizzaBuilder {
        this.cheese = true;
        return this;
    }

    addPepperoni(): PizzaBuilder {
        this.pepperoni = true;
        return this;
    }

    addBacon(): PizzaBuilder {
        this.bacon = true;
        return this;
    }

    build(): Pizza {
        const pizza = new Pizza();
        pizza.size = this.size;
        pizza.cheese = this.cheese;
        pizza.pepperoni = this.pepperoni;
        pizza.bacon = this.bacon;
        return pizza;
    }
}
