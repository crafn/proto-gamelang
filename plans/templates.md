###Templates
Declaring a template which yields structs

	let Storage := tpl[T: struct] -> struct
	{ var value : T; }

This can be used like

	var st := Storage[int](10);
	st.value; // 10

Declaring a template yielding functions

	let print := tpl[T: struct] -> fn(v : T)
	{ ... }

Usage

	print[int](9);
	print("Hello World!"); // Template argument deduction

Named and default arguments should work with templates as well

	let p := Pair[.B= int, ?char]("a", 2);
	let flat_space := SpaceTime[];
	let black_hole := SpaceTime[.Metric = Schwarzschild];

