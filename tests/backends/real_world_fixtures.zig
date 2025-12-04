/// Real-World Test Fixtures
///
/// IMPLEMENTATION NOTE:
/// Fixtures are currently inline strings due to Zig build system constraints with @embedFile().
/// However, corresponding .ms files exist in tests/fixtures/ for:
/// - Manual compilation: `msc compile tests/fixtures/basic/simple_function.ms`
/// - LSP support in editor (syntax highlighting, hover, etc.)
/// - Examples and documentation
/// - Future conversion to @embedFile() when build system supports it
///
/// The .ms files are the source of truth - inline strings should match their content.

// ============================================================================
// Basic Language Features
// ============================================================================

// Source: tests/fixtures/basic/simple_function.ms
pub const SIMPLE_FUNCTION =
    \\function add(a: number, b: number): number {
    \\    return a + b;
    \\}
;

// Source: tests/fixtures/basic/factorial_recursive.ms
pub const FACTORIAL_RECURSIVE =
    \\function factorial(n: number): number {
    \\    if (n <= 1) {
    \\        return 1;
    \\    }
    \\    return n * factorial(n - 1);
    \\}
;

// Source: tests/fixtures/basic/factorial_iterative.ms
pub const FACTORIAL_ITERATIVE =
    \\function factorialIterative(n: number): number {
    \\    let result = 1;
    \\    for (let i = 2; i <= n; i = i + 1) {
    \\        result = result * i;
    \\    }
    \\    return result;
    \\}
;

pub const FIBONACCI =
    \\function fibonacci(n: number): number {
    \\    if (n <= 1) {
    \\        return n;
    \\    }
    \\    return fibonacci(n - 1) + fibonacci(n - 2);
    \\}
;

// ============================================================================
// Control Flow
// ============================================================================

// Source: tests/fixtures/control_flow/while_loop_counter.ms
pub const WHILE_LOOP_COUNTER =
    \\function countDown(n: number): number {
    \\    let count = n;
    \\    while (count > 0) {
    \\        console.log(count);
    \\        count = count - 1;
    \\    }
    \\    return 0;
    \\}
;

// Source: tests/fixtures/control_flow/for_loop_sum.ms
pub const FOR_LOOP_SUM =
    \\function sumRange(n: number): number {
    \\    let sum = 0;
    \\    for (let i = 1; i <= n; i = i + 1) {
    \\        sum = sum + i;
    \\    }
    \\    return sum;
    \\}
;

pub const NESTED_LOOPS =
    \\function printGrid(): void {
    \\    for (let i = 0; i < 3; i = i + 1) {
    \\        for (let j = 0; j < 3; j = j + 1) {
    \\            console.log(i * 3 + j);
    \\        }
    \\    }
    \\}
;

pub const EARLY_RETURN =
    \\function findFirst(arr: number[], target: number): number {
    \\    for (let i = 0; i < arr.length; i = i + 1) {
    \\        if (arr[i] === target) {
    \\            return i;
    \\        }
    \\    }
    \\    return -1;
    \\}
;

// ============================================================================
// Variable Shadowing (Erlang-specific challenge)
// ============================================================================

pub const VARIABLE_SHADOWING_SIMPLE =
    \\function shadowTest(): number {
    \\    let x = 10;
    \\    x = x + 5;
    \\    return x;
    \\}
;

pub const VARIABLE_SHADOWING_MULTIPLE =
    \\function multiShadow(): number {
    \\    let x = 10;
    \\    let y = 20;
    \\    x = x + y;
    \\    y = y * 2;
    \\    x = x + y;
    \\    return x;
    \\}
;

// ============================================================================
// Objects and Arrays
// ============================================================================

pub const OBJECT_LITERAL =
    \\function createPerson(): void {
    \\    const person = {
    \\        name: "Alice",
    \\        age: 30
    \\    };
    \\}
;

pub const OBJECT_MEMBER_ACCESS =
    \\function getAge(): number {
    \\    const person = {
    \\        name: "Alice",
    \\        age: 30
    \\    };
    \\    return person.age;
    \\}
;

pub const ARRAY_OPERATIONS =
    \\function arrayTest(): number {
    \\    const arr: number[] = [];
    \\    arr.push(10);
    \\    arr.push(20);
    \\    arr.push(30);
    \\    return arr.length;
    \\}
;

// ============================================================================
// Classes
// ============================================================================

// Source: tests/fixtures/classes/simple_class.ms
pub const SIMPLE_CLASS =
    \\class Point {
    \\    x: number;
    \\    y: number;
    \\}
;

// Source: tests/fixtures/classes/class_with_methods.ms
pub const CLASS_WITH_METHODS =
    \\class Calculator {
    \\    add(a: number, b: number): number {
    \\        return a + b;
    \\    }
    \\
    \\    multiply(a: number, b: number): number {
    \\        return a * b;
    \\    }
    \\}
;

// Source: tests/fixtures/classes/inheritance_simple.ms
pub const INHERITANCE_SIMPLE =
    \\class Animal {
    \\    name: string;
    \\
    \\    speak(): void {
    \\        console.log("Some sound");
    \\    }
    \\}
    \\
    \\class Dog extends Animal {
    \\    bark(): void {
    \\        console.log("Woof!");
    \\    }
    \\}
;

// Source: tests/fixtures/classes/method_override.ms
pub const METHOD_OVERRIDE =
    \\class Shape {
    \\    getArea(): number {
    \\        return 0;
    \\    }
    \\}
    \\
    \\class Rectangle extends Shape {
    \\    width: number;
    \\    height: number;
    \\
    \\    getArea(): number {
    \\        return this.width * this.height;
    \\    }
    \\}
    \\
    \\class Circle extends Shape {
    \\    radius: number;
    \\
    \\    getArea(): number {
    \\        return 3.14159 * this.radius * this.radius;
    \\    }
    \\}
;

// Source: tests/fixtures/classes/polymorphism.ms
pub const POLYMORPHISM =
    \\class Vehicle {
    \\    speed: number;
    \\
    \\    move(): void {
    \\        console.log("Moving at speed: " + this.speed);
    \\    }
    \\}
    \\
    \\class Car extends Vehicle {
    \\    wheels: number;
    \\
    \\    move(): void {
    \\        console.log("Car driving at " + this.speed + " mph");
    \\    }
    \\}
    \\
    \\class Boat extends Vehicle {
    \\    propellers: number;
    \\
    \\    move(): void {
    \\        console.log("Boat sailing at " + this.speed + " knots");
    \\    }
    \\}
;

pub const CLASS_WITH_DECORATOR =
    \\@derive(Eq, Hash)
    \\class User {
    \\    name: string;
    \\    id: number;
    \\}
;

// ============================================================================
// Algorithms
// ============================================================================

// Source: tests/fixtures/algorithms/quicksort.ms
pub const QUICKSORT =
    \\function quicksort(arr: number[]): number[] {
    \\    if (arr.length <= 1) {
    \\        return arr;
    \\    }
    \\
    \\    const pivot = arr[0];
    \\    const less: number[] = [];
    \\    const greater: number[] = [];
    \\
    \\    for (let i = 1; i < arr.length; i = i + 1) {
    \\        if (arr[i] < pivot) {
    \\            less.push(arr[i]);
    \\        } else {
    \\            greater.push(arr[i]);
    \\        }
    \\    }
    \\
    \\    return quicksort(less).concat([pivot]).concat(quicksort(greater));
    \\}
;

// Source: tests/fixtures/algorithms/merge_sort.ms
pub const MERGE_SORT =
    \\function merge(left: number[], right: number[]): number[] {
    \\    const result: number[] = [];
    \\    let i = 0;
    \\    let j = 0;
    \\
    \\    while (i < left.length && j < right.length) {
    \\        if (left[i] < right[j]) {
    \\            result.push(left[i]);
    \\            i = i + 1;
    \\        } else {
    \\            result.push(right[j]);
    \\            j = j + 1;
    \\        }
    \\    }
    \\
    \\    while (i < left.length) {
    \\        result.push(left[i]);
    \\        i = i + 1;
    \\    }
    \\
    \\    while (j < right.length) {
    \\        result.push(right[j]);
    \\        j = j + 1;
    \\    }
    \\
    \\    return result;
    \\}
    \\
    \\function mergeSort(arr: number[]): number[] {
    \\    if (arr.length <= 1) {
    \\        return arr;
    \\    }
    \\
    \\    const mid = Math.floor(arr.length / 2);
    \\    const left = arr.slice(0, mid);
    \\    const right = arr.slice(mid);
    \\
    \\    return merge(mergeSort(left), mergeSort(right));
    \\}
;

// Source: tests/fixtures/algorithms/linked_list.ms
pub const LINKED_LIST =
    \\class Node {
    \\    value: number;
    \\    next: Node | null;
    \\}
    \\
    \\class LinkedList {
    \\    head: Node | null;
    \\
    \\    append(value: number): void {
    \\        const newNode = new Node();
    \\        newNode.value = value;
    \\        newNode.next = null;
    \\
    \\        if (this.head === null) {
    \\            this.head = newNode;
    \\        } else {
    \\            let current = this.head;
    \\            while (current.next !== null) {
    \\                current = current.next;
    \\            }
    \\            current.next = newNode;
    \\        }
    \\    }
    \\
    \\    find(value: number): Node | null {
    \\        let current = this.head;
    \\        while (current !== null) {
    \\            if (current.value === value) {
    \\                return current;
    \\            }
    \\            current = current.next;
    \\        }
    \\        return null;
    \\    }
    \\
    \\    length(): number {
    \\        let count = 0;
    \\        let current = this.head;
    \\        while (current !== null) {
    \\            count = count + 1;
    \\            current = current.next;
    \\        }
    \\        return count;
    \\    }
    \\}
;

// Source: tests/fixtures/algorithms/tree.ms
pub const BINARY_SEARCH_TREE =
    \\class TreeNode {
    \\    value: number;
    \\    left: TreeNode | null;
    \\    right: TreeNode | null;
    \\}
    \\
    \\class BinarySearchTree {
    \\    root: TreeNode | null;
    \\
    \\    insert(value: number): void {
    \\        const newNode = new TreeNode();
    \\        newNode.value = value;
    \\        newNode.left = null;
    \\        newNode.right = null;
    \\
    \\        if (this.root === null) {
    \\            this.root = newNode;
    \\        } else {
    \\            this.insertNode(this.root, newNode);
    \\        }
    \\    }
    \\
    \\    insertNode(node: TreeNode, newNode: TreeNode): void {
    \\        if (newNode.value < node.value) {
    \\            if (node.left === null) {
    \\                node.left = newNode;
    \\            } else {
    \\                this.insertNode(node.left, newNode);
    \\            }
    \\        } else {
    \\            if (node.right === null) {
    \\                node.right = newNode;
    \\            } else {
    \\                this.insertNode(node.right, newNode);
    \\            }
    \\        }
    \\    }
    \\
    \\    search(value: number): boolean {
    \\        return this.searchNode(this.root, value);
    \\    }
    \\
    \\    searchNode(node: TreeNode | null, value: number): boolean {
    \\        if (node === null) {
    \\            return false;
    \\        }
    \\
    \\        if (value < node.value) {
    \\            return this.searchNode(node.left, value);
    \\        } else if (value > node.value) {
    \\            return this.searchNode(node.right, value);
    \\        } else {
    \\            return true;
    \\        }
    \\    }
    \\}
;

pub const BINARY_SEARCH =
    \\function binarySearch(arr: number[], target: number): number {
    \\    let left = 0;
    \\    let right = arr.length - 1;
    \\
    \\    while (left <= right) {
    \\        const mid = Math.floor((left + right) / 2);
    \\        if (arr[mid] === target) {
    \\            return mid;
    \\        } else if (arr[mid] < target) {
    \\            left = mid + 1;
    \\        } else {
    \\            right = mid - 1;
    \\        }
    \\    }
    \\
    \\    return -1;
    \\}
;

pub const IS_PRIME =
    \\function isPrime(n: number): boolean {
    \\    if (n <= 1) return false;
    \\    if (n <= 3) return true;
    \\    if (n % 2 === 0 || n % 3 === 0) return false;
    \\
    \\    let i = 5;
    \\    while (i * i <= n) {
    \\        if (n % i === 0 || n % (i + 2) === 0) {
    \\            return false;
    \\        }
    \\        i = i + 6;
    \\    }
    \\
    \\    return true;
    \\}
;

// ============================================================================
// Design Patterns
// ============================================================================

// Source: tests/fixtures/patterns/builder.ms
pub const BUILDER_PATTERN =
    \\class Pizza {
    \\    size: string;
    \\    cheese: boolean;
    \\    pepperoni: boolean;
    \\    bacon: boolean;
    \\}
    \\
    \\class PizzaBuilder {
    \\    size: string;
    \\    cheese: boolean;
    \\    pepperoni: boolean;
    \\    bacon: boolean;
    \\
    \\    setSize(size: string): PizzaBuilder {
    \\        this.size = size;
    \\        return this;
    \\    }
    \\
    \\    addCheese(): PizzaBuilder {
    \\        this.cheese = true;
    \\        return this;
    \\    }
    \\
    \\    addPepperoni(): PizzaBuilder {
    \\        this.pepperoni = true;
    \\        return this;
    \\    }
    \\
    \\    addBacon(): PizzaBuilder {
    \\        this.bacon = true;
    \\        return this;
    \\    }
    \\
    \\    build(): Pizza {
    \\        const pizza = new Pizza();
    \\        pizza.size = this.size;
    \\        pizza.cheese = this.cheese;
    \\        pizza.pepperoni = this.pepperoni;
    \\        pizza.bacon = this.bacon;
    \\        return pizza;
    \\    }
    \\}
;

// Source: tests/fixtures/patterns/factory.ms
pub const FACTORY_PATTERN =
    \\class Button {
    \\    label: string;
    \\
    \\    render(): void {
    \\        console.log("Rendering button: " + this.label);
    \\    }
    \\}
    \\
    \\class Input {
    \\    placeholder: string;
    \\
    \\    render(): void {
    \\        console.log("Rendering input: " + this.placeholder);
    \\    }
    \\}
    \\
    \\class UIFactory {
    \\    createButton(label: string): Button {
    \\        const button = new Button();
    \\        button.label = label;
    \\        return button;
    \\    }
    \\
    \\    createInput(placeholder: string): Input {
    \\        const input = new Input();
    \\        input.placeholder = placeholder;
    \\        return input;
    \\    }
    \\}
;

// Source: tests/fixtures/patterns/singleton.ms
pub const SINGLETON_PATTERN =
    \\class Database {
    \\    connection: string;
    \\    static instance: Database | null;
    \\
    \\    constructor() {
    \\        this.connection = "Connected";
    \\    }
    \\
    \\    static getInstance(): Database {
    \\        if (Database.instance === null) {
    \\            Database.instance = new Database();
    \\        }
    \\        return Database.instance;
    \\    }
    \\
    \\    query(sql: string): void {
    \\        console.log("Executing: " + sql);
    \\    }
    \\}
;

// Source: tests/fixtures/patterns/observer.ms
pub const OBSERVER_PATTERN =
    \\class Observer {
    \\    name: string;
    \\
    \\    update(message: string): void {
    \\        console.log(this.name + " received: " + message);
    \\    }
    \\}
    \\
    \\class Subject {
    \\    observers: Observer[];
    \\    observerCount: number;
    \\
    \\    constructor() {
    \\        this.observerCount = 0;
    \\    }
    \\
    \\    attach(observer: Observer): void {
    \\        this.observers[this.observerCount] = observer;
    \\        this.observerCount = this.observerCount + 1;
    \\    }
    \\
    \\    notify(message: string): void {
    \\        let i = 0;
    \\        while (i < this.observerCount) {
    \\            this.observers[i].update(message);
    \\            i = i + 1;
    \\        }
    \\    }
    \\}
;

// ============================================================================
// Known Bugs (Expected to fail on specific backends)
// ============================================================================

pub const ERLANG_BUG_LOOP_CLOSURE =
    \\function testWhileLoop(): number {
    \\    const count = 5;
    \\    while (count > 0) {
    \\        console.log(count);
    \\        count = count - 1;
    \\    }
    \\    return 0;
    \\}
;

pub const ERLANG_BUG_EARLY_RETURN =
    \\function absoluteValue(x: number): number {
    \\    if (x < 0) {
    \\        return -x;
    \\    }
    \\    return x;
    \\}
;

// ============================================================================
// Comprehensive Demo
// ============================================================================

pub const COMPREHENSIVE_DEMO =
    \\function fibonacci(n: number): number {
    \\    if (n <= 1) return n;
    \\    let a = 0;
    \\    let b = 1;
    \\    for (let i = 2; i <= n; i = i + 1) {
    \\        const temp = a + b;
    \\        a = b;
    \\        b = temp;
    \\    }
    \\    return b;
    \\}
    \\
    \\function main(): number {
    \\    const result = fibonacci(10);
    \\    console.log(result);
    \\    return 0;
    \\}
;
