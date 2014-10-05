Pointer syntax

	var ptr : ?int = null;

Other semantics are similar to C pointers. Note how everything can be parsed from left to right: "variable `ptr` is a pointer-to-int with default value `null`"

Reference syntax

	var ref : ^int = &exampleInstance;

A reference is like a pointer, but it can never be set to null and it must be initialized when created. Dereferencing a reference uses the same syntax as dereferencing a pointer, because then indirection is explicit.

Compiler should give error from the following code

	let func := fn (ptr : ?int) {
		let ref : ^int = ptr;
	};

Fixed code
	
	let func := fn (ptr : ?int) {
		if (ptr == null)
			return;
		let ref : ^int = ptr;
	};



