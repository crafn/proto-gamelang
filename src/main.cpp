#include <iostream>

#include "token.hpp"

int main(int argc, const char* argv[])
{
	if (argc <= 1)
		return 0;

	const char* filepath= argv[1];
	std::cout << "file: " << filepath << std::endl;
	for (auto&& token : gamelang::tokenize(filepath)) {
		std::cout << token.text << "\t" << str(token.type) << std::endl;
	}
}
