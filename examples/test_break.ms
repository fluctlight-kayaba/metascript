// Test break in for loop
for (let i = 0; i < 10; i = i + 1) {
    if (i > 4) {
        break;
    }
    console.log(i);
}
