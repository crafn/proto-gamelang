Variable declaration syntax

	var foo : Type;
	var foo : Type = default_value;
	var foo := default_value; // Type is deduced automatically

Constant declaration

	let bar : Type = some_value;
	...

Plot twist: `let` declares a variable with default mutability - which is constant - and `var` is just syntactic sugar for marking the variable type as mutable. Marking type as mutable can be done with character `'` so that these declarations are equivalent:

	let foo : 'Type;
	var foo : Type;

