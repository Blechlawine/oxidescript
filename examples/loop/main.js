(() => {
	let SmVz2XZw = [];
	for (const test of ["foo", "bar"]) {
		SmVz2XZw.push((() => {
			console.log(test);
		})());
	}
	return SmVz2XZw;
})();
