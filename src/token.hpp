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
	yields, // ->
	equals, // ==
	add, // +
	sub, // -
	unknown
};

static const char* enumStr(TokenType type)
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
		case TokenType::yields: return "yields";
		case TokenType::equals: return "equals";
		case TokenType::add: return "add";
		case TokenType::sub: return "sub";
		case TokenType::unknown:
		default: return "unknown";
	}
}

static const char* str(TokenType type)
{
	switch (type) {
		case TokenType::identifier: return "";
		case TokenType::number: return "";
		case TokenType::assign: return "=";
		case TokenType::declaration: return "";
		case TokenType::endStatement: return ";";
		case TokenType::openParen: return "(";
		case TokenType::closeParen: return ")";
		case TokenType::openBlock: return "{";
		case TokenType::closeBlock: return "}";
		case TokenType::openSquare: return "[";
		case TokenType::closeSquare: return "]";
		case TokenType::yields: return "->";
		case TokenType::equals: return "==";
		case TokenType::add: return "+";
		case TokenType::sub: return "-";
		case TokenType::unknown:
		default: return "???";
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
