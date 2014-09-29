#include <cassert>
#include <cstdio>

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
/// @todo 111ab shouldn't be an identifier, or should it?
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
		case ',': return TokenType::comma;
		case '(': return TokenType::openParen;
		case ')': return TokenType::closeParen;
		case '{': return TokenType::openBlock;
		case '}': return TokenType::closeBlock;
		case '[': return TokenType::openSquare;
		case ']': return TokenType::closeSquare;
		case '+': return TokenType::add;
		case '-': return TokenType::sub;
		default: return TokenType::unknown;
	}
}

TokenType doubleCharTokenType(char ch1, char ch2)
{
	if (ch1 == '-' && ch2 == '>')
		return TokenType::yields;
	if (ch1 == '=' && ch2 == '=')
		return TokenType::equals;

	return TokenType::unknown;
}

TokenType tokenType(const std::string& str)
{
	if (str.size() == 1) {
		TokenType t= singleCharTokenType(str[0]);
		if (t != TokenType::unknown)
			return t;
	}
	if (str.size() == 2) {
		TokenType t= doubleCharTokenType(str[0], str[1]);
		if (t != TokenType::unknown)
			return t;
	}
	if (number(str))
		return TokenType::number;
	if (identifier(str))
		return TokenType::identifier;

	return TokenType::unknown;
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
		char* next= contents;
		char* tok_begin= contents;
		char const* end= contents + contents_size;
		auto commit= [&tokens] (char* b, char* e)
		{
			if (e > b) {
				std::string text(b, e);
				TokenType type= tokenType(text);
				tokens.emplace_back(Token{type, std::move(text)});
			}
		};

		while (next < end && tok_begin < end) {
			if (identifierChar(*next)) {
				if (	tok_begin + 1 == next &&
						singleCharTokenType(*tok_begin) != TokenType::unknown) {
					// Token started as a single-char-token, but next
					// letter turns out to be the beginning of an identifier
					commit(tok_begin, next);
					tok_begin= next;
				}
				++next;
				continue;
			}

			if (whitespace(*next)) {
				commit(tok_begin, next);
				tok_begin= next + 1;
			} else {
				if (doubleCharTokenType(tok_begin[0], tok_begin[1]) !=
						TokenType::unknown) {
					commit(tok_begin, tok_begin + 2);
					tok_begin= tok_begin + 2;
					++next;
				} else {
					commit(tok_begin, next);
					tok_begin= next;
				}
			}
			++next;
		}
	}
	cleanup:
	std::fclose(file);
	std::free(contents);
	
	return tokens;
}

} // gamelang

