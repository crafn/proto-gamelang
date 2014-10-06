This is a block
	{
		// ...
	}

Blocks contain code and can have properties, like
- being a function: `fn () { }`
- being a struct type: `struct { }`
- capturing variables: `[var] { }`

By default blocks also act as scopes, which means that variables declared inside them will be destroyed when the block ends. This doesn't apply to `struct` block though.

