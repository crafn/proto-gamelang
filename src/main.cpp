#include <iostream>

#include "ast.hpp"
#include "token.hpp"

int main(int argc, const char* argv[])
{
	if (argc <= 1)
		return 0;

	const char* filepath= argv[1];
	std::cout << "file: " << filepath << std::endl;

	std::cout << "*** Tokens ***\n";
	auto&& tokens= gamelang::tokenize(filepath);
	for (auto&& token : tokens) {
		std::cout << token.text << "\t" << str(token.type) << std::endl;
	}

	std::cout << "*** AST ***\n";
	gamelang::generateAst(tokens);
}
