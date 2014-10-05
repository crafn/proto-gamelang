
	let Cat := struct {
		var poisonClaw := true;
		var toy : ^CatToy; // Every cat has one
		var owner : ?Human = null; // This is optional
	};

	var toy : CatToy;

	var cat1 : Cat; // Error, cat1.toy isn't initialized
	var cat2 : Cat(.toy = toy); // Ok

