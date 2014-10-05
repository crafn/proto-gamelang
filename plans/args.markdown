Default values similar to C++

	let foo := fn (a : int, b := 10) { ... }
	foo(10, getValue());
	foo(10); // b == 10

Named arguments

	let bar := fn (a : int, b : int, c : int) { ... }
	bar(.c= 95, .b= 9, 1);
  
Dot is used to distinguish target parameters from local variables with the same name:

	var a : int;
	func(a = 0); // Assign 0 to local var a and pass it to the function
	func(.a = 0); // Pass 0 to function using parameter a

