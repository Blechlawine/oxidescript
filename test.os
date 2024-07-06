fn fibonacci_recursive(n: number) {
    if n < 2 {
        n
    } else {
        fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)
    }
}
