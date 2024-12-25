function fibonacci_recursive(n) {
	return (() => {
		if (n < 2) {
			return n;
		} else {
			return fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2);
		}
	})();
}
console.log(fibonacci_recursive(10));
