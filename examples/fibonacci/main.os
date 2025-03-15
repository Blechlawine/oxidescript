fn fibonacci_recursive(n: number) -> number {
    if n < 2 {
        n
    } else {
        fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)
    }
}

console.log(fibonacci_recursive(10))
