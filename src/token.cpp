#include "token.hpp"

namespace gamelang
{
namespace {

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
/// @todo 111ab shouldn't be an identifier
bool identifier(const std::string& str)
{
	for (auto&& ch : str) {
		if (!identifierChar(ch))
			return false;
	}
	return true;
}

TokenType singleCharTokenType(char ch)
{
	switch (ch) {
		case '=': return TokenType::assign;
		case ':': return TokenType::declaration;
		case ';': return TokenType::endStatement;
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

} // anonymous

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

