###Templates
Declaring a template which yields structs

	let Storage := tpl[T: struct] -> struct
	{ var value : T; }

This can be used like

	var st := Storage[int](10);
	st.value; // 10

Declaring a template yielding functions

	let print := tpl[T: struct] -> fn(v: T)
	{ ... }

Usage

	print[int](9);
	print("Hello World!"); // Template argument deduction

Named and default arguments should work with templates as well

	let p := Pair[.B= int, ?char]("a", 2);
	let flat_space := SpaceTime[];
	let black_hole := SpaceTime[.Metric = Schwarzschild];

If the syntax ends up causing confusion with indexing, it could always be changed to

	let Storage := tpl\T: struct/ -> struct ...
	var st := Storage\int/(10);

There's at least one a bit ambiguous case with the C++ method syntax:

	data.ptr[10] == (data.ptr)[10]
	data.tplAlias[int] == (data.tplAlias)[int] // Consistent with the previous row
	foo[int](data) == data.(foo[int])() =? data.foo[int]() // Not consistent if allowed

