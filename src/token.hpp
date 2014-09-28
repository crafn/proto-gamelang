#ifndef GAMELANG_TOKEN_HPP
#define GAMELANG_TOKEN_HPP

#include <cassert>
#include <cstdio>
#include <string>
#include <vector>

namespace gamelang
{

bool whitespace(char ch)
{ return ch == ' ' || ch == '\t' || ch == '\n'; }

bool number(char ch)
{ return ch >= '0' && ch <= '9'; }

bool number(const std::string& str)
{
	for (auto&& ch : str) {
		if (!number(ch))
			return false;
	}
	return true;
}

/// true if `ch` can be a character in an identifier
bool identifierChar(char ch)
{
	if (ch >= 'a' && ch <= 'z')
		return true;
	if (ch >= 'A' && ch <= 'Z')
		return true;
	if (number(ch))
		return true;
	if (ch >= '_')
		return true;
	return false;
}

/// true if str looks like an identifier (like_this236)
/// @todo 111 shouldn't be an identifier
bool identifier(const std::string& str)
{
	for (auto&& ch : str) {
		if (!identifierChar(ch))
			return false;
	}
	return true;
}

enum class TokenType {
	identifier, // single_word_like_this
	number, // 2538
	assign, // =
	declaration, // :
	exprEnd, // ;
	openParen, // (
	closeParen, // )
	openBlock, // {
	closeBlock, // }
	openSquare, // [
	closeSquare, // ]
	unknown
};

std::string str(TokenType type)
{
	switch (type) {
		case TokenType::identifier: return "identifier";
		case TokenType::number: return "number";
		case TokenType::assign: return "assign";
		case TokenType::declaration: return "declaration";
		case TokenType::exprEnd: return "exprEnd";
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

TokenType singleCharTokenType(char ch)
{
	switch (ch) {
		case '=': return TokenType::assign;
		case ':': return TokenType::declaration;
		case ';': return TokenType::exprEnd;
		case '(': return TokenType::openParen;
		case ')': return TokenType::closeParen;
		case '{': return TokenType::openBlock;
		case '}': return TokenType::closeBlock;
		case '[': return TokenType::openSquare;
		case ']': return TokenType::closeSquare;
		default: return TokenType::unknown;
	}
}

TokenType tokenType(const std::string& str)
{
	if (str.size() == 1) {
		TokenType t= singleCharTokenType(str[0]);
		if (t != TokenType::unknown)
			return t;
	}
	if (number(str))
		return TokenType::number;
	if (identifier(str))
		return TokenType::identifier;

	return TokenType::unknown;
}

bool tokenSeparator(char ch)
{
	return singleCharTokenType(ch) != TokenType::unknown;
}

using Tokens= std::vector<Token>;

Tokens tokenize(const char* filepath)
{
	Tokens tokens;
	char* contents= nullptr;
	std::size_t contents_size= 0;

	FILE* file= std::fopen(filepath, "rb");
	if (!file)
		goto cleanup;
	{ // Read file
		std::fseek(file, 0, SEEK_END);
		long size= std::ftell(file);
		std::fseek(file, 0, SEEK_SET);

		contents= static_cast<char*>(std::malloc(size + 1));
		int count= std::fread(contents, size, 1, file);
		assert(count == 1);
		contents_size= size;
	}
	{ // Tokenize
		char* cur= contents;
		char* tok_start= contents;
		char const* end= contents + contents_size;
		auto commitToken= [&cur, &tok_start, &tokens] ()
		{
			if (cur > tok_start) {
				std::string text(tok_start, cur);
				TokenType type= tokenType(text);
				tokens.emplace_back(Token{type, std::move(text)});
			}
		};

		while (cur < end) {
			char ch= *cur;

			if (whitespace(ch)) {
				commitToken();		
				tok_start= cur + 1;
			} else if (tokenSeparator(ch)) {
				commitToken();
				tok_start= cur;	
			}
			++cur;
		}
	}
	cleanup:
	std::fclose(file);
	std::free(contents);
	
	return tokens;
}

} // gamelang
#endif // GAMELANG_TOKEN_HPP
