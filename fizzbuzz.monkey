let fizzbuzz = fn(k) {
	if (100 < k) {
		return 0;
	};
	let fizz = (3 * (k / 3)) == k;
	let buzz = (5 * (k / 5)) == k;
	if (fizz and buzz) {
		put("fizzbuzz");
	} else {
		if (fizz) {
			put("fizz");
		};
		if (buzz) {
			put("buzz");
		};
	};
	if (!(fizz or buzz)) {
		put(k);
	};
	fizz(k + 1);
};

fizzbuzz(0);
