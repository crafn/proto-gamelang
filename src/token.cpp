#include <cassert>
#include <cstdio>

#include "token.hpp"

namespace gamelang
{
namespace {

bool whitespace(char ch)
{ return ch == ' ' || ch == '\t' || ch == '\n'; }

bool linebreak(char ch)
{ return ch == '\n'; }

/// @todo Restrict
bool number(const std::string& str)
{
	for (auto&& ch : str) {
		/// @todo Restrict more
		bool allowed= ch >= '0' && ch <= '9' || ch == '.' || ch == 'x';
		if (!allowed)
			return false;
	}
	return true;
}

/// true if `ch` can be a character in a name
bool nameChar(char ch)
{
	if (ch >= 'a' && ch <= 'z')
		return true;
	if (ch >= 'A' && ch <= 'Z')
		return true;
	if (ch >= '0' && ch <= '9')
		return true;
	if (ch >= '_')
		return true;
	return false;
}

/// true if str looks like a name (like_this236)
/// @todo 111ab shouldn't be an name, or should it?
bool name(const std::string& str)
{
	for (auto&& ch : str) {
		if (!nameChar(ch))
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
		case '<': return TokenType::less;
		case '>': return TokenType::greater;
		case '+': return TokenType::add;
		case '-': return TokenType::sub;
		case '*': return TokenType::mul;
		case '/': return TokenType::div;
		case '%': return TokenType::mod;
		case '.': return TokenType::dot;
		case '&': return TokenType::ref;
		case '^': return TokenType::hat;
		case '?': return TokenType::question;
		case '~': return TokenType::tilde;
		case '\'': return TokenType::squote;
		default: return TokenType::unknown;
	}
}

TokenType doubleCharTokenType(char ch1, char ch2)
{
	if (ch1 == '-' && ch2 == '>')
		return TokenType::rightArrow;
	if (ch1 == '=' && ch2 == '=')
		return TokenType::equals;
	if (ch1 == '!' && ch2 == '=')
		return TokenType::nequals;
	if (ch1 == '/' && ch2 == '/')
		return TokenType::comment;
	if (ch1 == '<' && ch2 == '.')
		return TokenType::openAngle;
	if (ch1 == '.' && ch2 == '>')
		return TokenType::closeAngle;

	return TokenType::unknown;
}

TokenType kwTokenType(const std::string& str)
{
	if (str == "var")
		return TokenType::kwVar;
	if (str == "let")
		return TokenType::kwLet;
	if (str == "fn")
		return TokenType::kwFn;
	if (str == "struct")
		return TokenType::kwStruct;
	if (str == "return")
		return TokenType::kwReturn;
	if (str == "goto")
		return TokenType::kwGoto;
	if (str == "break")
		return TokenType::kwBreak;
	if (str == "continue")
		return TokenType::kwContinue;
	if (str == "else")
		return TokenType::kwElse;
	if (str == "null")
		return TokenType::kwNull;
	if (str == "loop")
		return TokenType::kwLoop;
	if (str == "if")
		return TokenType::kwIf;
	if (str == "extern")
		return TokenType::kwExtern;
	if (str == "tpl")
		return TokenType::kwTpl;
	if (str == "sizeof")
		return TokenType::kwSizeof;
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
		
	if (name(str)) {
		TokenType kw= kwTokenType(str);
		if (kw != TokenType::unknown)
			return kw;
		return TokenType::name;
	}


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
		auto commit= [&tokens, end] (	char* b,
										char* e,
										TokenType t= TokenType::unknown)
		{
			if (e > b) {
				bool last_on_line= e + 1 < end && linebreak(*e);
				std::string text(b, e);
				if (t == TokenType::unknown)
					t= tokenType(text);
				tokens.emplace_back(Token{t, std::move(text), last_on_line});
			}
		};
		auto findEndQuote= [&tokens, end] (char* b) -> char*
		{
			/// @todo Escaping
			while (b < end && *b != '"')
				++b;
			return b;
		};
		auto findLineEnd= [&tokens, end] (char* b) -> char*
		{
			while (b < end && !linebreak(*b))
				++b;
			return b;
		};

		while (next < end && tok_begin < end) {
			// Comments
			if (	tok_begin + 1 < end &&
					doubleCharTokenType(tok_begin[0], tok_begin[1]) ==
						TokenType::comment) {
				auto comment_end= findLineEnd(tok_begin);
				commit(tok_begin + 2, comment_end, TokenType::comment);
				++comment_end; // Skip linebreak
				next= comment_end;
				tok_begin= comment_end;
				continue;
			}

			// String literals
			if (*tok_begin == '"') {
				auto str_end= findEndQuote(tok_begin + 1);
				commit(tok_begin + 1, str_end, TokenType::string);
				++str_end; // Skip `"`
				next= str_end;
				tok_begin= next;
				continue;
			}

			if (nameChar(*next)) {
				if (	tok_begin + 1 == next &&
						singleCharTokenType(*tok_begin) != TokenType::unknown) {
					// Token started as a single-char-token, but next
					// letter turns out to be the beginning of a name
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
		tokens.emplace_back(Token{TokenType::eof, "eof", true});
	}
	cleanup:
	std::fclose(file);
	std::free(contents);
	
	return tokens;
}

} // gamelang

