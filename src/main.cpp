#include <iostream>
#include <fstream>

#include "ast.hpp"
#include "codegen.hpp"
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
		std::cout << token.text << "\t" << enumStr(token.type) << std::endl;
	}

	std::cout << "*** AST ***\n";
	auto&& ast= gamelang::genAst(tokens);

	std::cout << "*** C ***\n";
	std::string code= gamelang::genC(ast);
	std::cout << code;

	{
		std::ofstream output{"out.c", std::ios::binary};
		output << code;
		output.close();

		system("gcc out.c -o out");
	}
}
