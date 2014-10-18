Types are constants.
Declaration syntax

	let Cat := struct {
		var poisonClaw := true;
		var toy : ^CatToy; // Every cat has one
		var owner : ?Human = null; // This is optional
	};

Instantiation

	var toy := CatToy();
	toy.featherness = 99;

	var cat1 := Cat(); // Error, cat1.toy isn't initialized
	var cat2 := Cat(.toy = 'toy); // Ok

The last row shows how struct can be instantiated by calling the implicit constructor function. It turns out that the struct block can be thought as an ordinary block of code with the difference that the variables declared in it will remain in the instance. Also, default values of struct members can be overridden by the constructor call.

	let Object := struct {
		var type : ObjectType = ObjectType::empty;
		var id := g_manager.nextIdForType(type);
		var special := false;

		if (id % 100 == 0)
			special = true;

		/// @todo Destructor syntax
	};

	var obj := Object(); // Creates `empty`
	var gun := Object(.type = ObjectType::bazooka);

