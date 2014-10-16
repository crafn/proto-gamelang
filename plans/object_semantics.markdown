###Destructors

	let MemBlock := struct {
		var ptr: ^void = malloc(100);
	} ~ {
		free(ptr); // This is executed when instance dies
	};

Same should work for any block, for example functions

	let foo := fn() -> int
	{
		var mem := malloc(999);
		// ...
		if (ready)
			return 1;
		// ...
		return 2;
	} ~ {
		free(mem); // Freed on return
	}

###Copying
Copying should work like in C by default, but if destructor is specified some extra measures should be necessary.
TODO custom copy

###Moving
todo

