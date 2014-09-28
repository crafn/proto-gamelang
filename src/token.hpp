#ifndef GAMELANG_TOKEN_HPP
#define GAMELANG_TOKEN_HPP

#include <cassert>
#include <cstdio>
#include <string>
#include <vector>

namespace gamelang
{

enum class TokenType {
	identifier, // single_word_like_this
	number, // 2538
	assign, // =
	declaration, // :
	endStatement, // ;
	openParen, // (
	closeParen, // )
	openBlock, // {
	closeBlock, // }
	openSquare, // [
	closeSquare, // ]
	unknown
};

static std::string str(TokenType type)
{
	switch (type) {
		case TokenType::identifier: return "identifier";
		case TokenType::number: return "number";
		case TokenType::assign: return "assign";
		case TokenType::declaration: return "declaration";
		case TokenType::endStatement: return "endStatement";
		case TokenType::openParen: return "openParen";
		case TokenType::closeParen: return "closeParen";
		case TokenType::openBlock: return "openBlock";
		case TokenType::closeBlock: return "closeBlock";
		case TokenType::openSquare: return "openSquare";
		case TokenType::closeSquare: return "closeSquare";
		case TokenType::unknown:
		default: return "unknown";
	}
}

struct Token {
	TokenType type;
	std::string text;
};

using Tokens= std::vector<Token>;
Tokens tokenize(const char* filepath);

} // gamelang

#endif // GAMELANG_TOKEN_HPP
