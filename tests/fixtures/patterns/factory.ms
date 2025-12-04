class Button {
    label: string;

    render(): void {
        console.log("Rendering button: " + this.label);
    }
}

class Input {
    placeholder: string;

    render(): void {
        console.log("Rendering input: " + this.placeholder);
    }
}

class UIFactory {
    createButton(label: string): Button {
        const button = new Button();
        button.label = label;
        return button;
    }

    createInput(placeholder: string): Input {
        const input = new Input();
        input.placeholder = placeholder;
        return input;
    }
}
