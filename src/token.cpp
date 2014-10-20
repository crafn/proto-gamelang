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
		case '\\': return TokenType::rdiv;
		case '%': return TokenType::mod;
		case '.': return TokenType::dot;
		case '&': return TokenType::amp;
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
		return TokenType::leftInsert;
	if (ch1 == '.' && ch2 == '>')
		return TokenType::rightInsert;
	if (ch1 == '=' && ch2 == '>')
		return TokenType::rdArrow;
	if (ch1 == '<' && ch2 == '=')
		return TokenType::leq;
	if (ch1 == '>' && ch2 == '=')
		return TokenType::geq;

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
	if (str == "true")
		return TokenType::kwTrue;
	if (str == "false")
		return TokenType::kwFalse;
	if (str == "true")
		return TokenType::kwTrue;
	if (str == "false")
		return TokenType::kwFalse;
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
		enum class State {
			none,
			maybeSingleChar,
			number,
			numberAfterDot,
			name,
			str,
			comment
		};
		State state= State::none;
		char* cur= contents;
		char* tok_begin= contents;
		char const* end= contents + contents_size;
		auto commit= [&state, &tokens, end]
		(char* b, char* e, TokenType t)
		{
			if (e > b) {
				bool last_on_line= e + 1 < end && linebreak(*e);
				std::string text(b, e);
				if (t == TokenType::name) {
					TokenType kw= kwTokenType(text);
					if (kw != TokenType::unknown)
						t= kw;
				}
				tokens.emplace_back(Token{t, std::move(text), last_on_line});
				state= State::none;
			}
		};

		while (cur < end && tok_begin < end) {
			switch (state) {
				case State::none:
					if (singleCharTokenType(*cur) != TokenType::unknown)
						state= State::maybeSingleChar;
					else if (*cur >= '0' && *cur <= '9')
						state= State::number;
					else if (	(*cur >= 'a' && *cur <= 'z') ||
								(*cur >= 'A' && *cur <= 'Z') ||
								(*cur == '_'))
						state= State::name;
					else if (*cur == '\"')
						state= State::str;
					tok_begin= cur;
				break;
				case State::maybeSingleChar: {
					TokenType t= doubleCharTokenType(*tok_begin, *cur);
					if (t == TokenType::unknown) {
						commit(tok_begin, cur, singleCharTokenType(*tok_begin));
						--cur;
					} else {
						if (t == TokenType::comment) {
							state= State::comment;
							tok_begin += 2;
						} else {
							commit(tok_begin, cur + 1, t);
						}
					}
				}
				break;
				case State::numberAfterDot:
				case State::number:
					if (	whitespace(*cur) ||
							singleCharTokenType(*cur) != TokenType::unknown) {
						if (state == State::numberAfterDot) {
							// `123.` <- last dot is detected and removed,
							// because `.>` is a token
							commit(tok_begin, cur - 1, TokenType::number);
							cur -= 2;
							break;
						} else if (*cur != '.') {
							commit(tok_begin, cur, TokenType::number);
							--cur;
							break;
						}
					}

					if (*cur == '.')
						state= State::numberAfterDot;
					else
						state= State::number;
				break;
				case State::name:
					if (	whitespace(*cur) ||
							singleCharTokenType(*cur) != TokenType::unknown) {
						commit(tok_begin, cur, TokenType::name);
						--cur;
					}
				break;
				case State::str:
					if (*cur == '\"')
						commit(tok_begin + 1, cur, TokenType::string);
				break;
				case State::comment:
					if (linebreak(*cur))
						commit(tok_begin, cur, TokenType::comment);
				default:;
			}
			++cur;
		}
		tokens.emplace_back(Token{TokenType::eof, "eof", true});
	}
	cleanup:
	std::fclose(file);
	std::free(contents);
	
	return tokens;
}

} // gamelang

