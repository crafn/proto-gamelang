###Pointer syntax

	var ptr : ?int = null;

Other semantics are similar to C pointers. Note how everything can be parsed from left to right: "variable `ptr` is a pointer-to-int with default value `null`"

###Reference syntax

	var ref : ^int = 'exampleInstance;

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

###References to temporaries
Lets consider a huge struct which we don't want to copy when passed to a function

	{
		var u := Universe(.year = 95);
		simulate('u);
	}

That is kind of clumsy way to pass a parameter. A better way:

	simulate('Universe(.year = 95));

Taking reference to an rvalue is not possible in C or C++, but this language doesn't make the distinction. When taking a reference to a temporary, compiler makes sure that it'll have an address (essentially generating the clumsy code). This makes some funny-looking code possible though:

	// Modifies the reference pointing to a variable holding number 1
	modify('1); 
	// Math didn't change

