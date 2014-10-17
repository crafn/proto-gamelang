Functions are values.
Declaration syntax (in any block)

	let plusOne := fn (n : int) -> int
	{
		return n + 1;
	};

Functions declared as constant will be as cheap as ordinary functions in C. Functions declared as mutable variables will have some overhead, but can be reassigned.

Implicit return and return type deduction

	let plusOne := fn (n : int) { n + 1 };

Calling

	result = plusOne(1); // 2

Declaring function types

	let SomeFunc := fn (a : int);

Functions can be declared in-place

	sort(list, fn (a : int, b : int) { a < b });
