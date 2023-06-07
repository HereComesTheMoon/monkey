let fizzbuzz = fn(k) {
	if (100 < k) {
		return 0;
	};
	let fizz = (3 * (k / 3)) == k;
	let buzz = (5 * (k / 5)) == k;
	if (fizz and buzz) {
		print("fizzbuzz");
	} else {
		if (fizz) {
			print("fizz");
		};
		if (buzz) {
			print("buzz");
		};
	};
	if (!(fizz or buzz)) {
		print(k);
	};
	fizz(k + 1);
};

fizzbuzz(0);
