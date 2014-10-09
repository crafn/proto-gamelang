#ifndef GAMELANG_TOKEN_HPP
#define GAMELANG_TOKEN_HPP

#include <string>
#include <vector>

namespace gamelang
{

enum class TokenType {
	eof,
	name, // single_word_like_this
	number, // 2538
	assign, // =
	declaration, // :
	endStatement, // ;
	comma, // ,
	openParen, // (
	closeParen, // )
	openBlock, // {
	closeBlock, // }
	openSquare, // [
	closeSquare, // ]
	rightArrow, // ->
	equals, // ==
	nequals, // !=
	less, // <
	greater, // >
	add, // +
	sub, // -
	mul, // *
	div, // /
	dot, // .
	ref, // &
	hat, // ^
	question, // ?
	squote, // '
	comment, // //
	kwVar, // var
	kwLet, // let
	kwFn, // fn
	kwStruct, // struct
	kwReturn, // return
	kwGoto, // goto
	kwBreak, // break
	kwContinue, // continue
	kwElse, // else
	kwNull, // null
	kwLoop, // loop
	kwIf, // if
	unknown
};

static const char* enumStr(TokenType type)
{
	switch (type) {
		case TokenType::eof: return "eof";
		case TokenType::name: return "name";
		case TokenType::number: return "number";
		case TokenType::assign: return "assign";
		case TokenType::declaration: return "declaration";
		case TokenType::endStatement: return "endStatement";
		case TokenType::comma: return "comma";
		case TokenType::openParen: return "openParen";
		case TokenType::closeParen: return "closeParen";
		case TokenType::openBlock: return "openBlock";
		case TokenType::closeBlock: return "closeBlock";
		case TokenType::openSquare: return "openSquare";
		case TokenType::closeSquare: return "closeSquare";
		case TokenType::rightArrow: return "rightArrow";
		case TokenType::equals: return "equals";
		case TokenType::nequals: return "nequals";
		case TokenType::less: return "less";
		case TokenType::greater: return "greater";
		case TokenType::add: return "add";
		case TokenType::sub: return "sub";
		case TokenType::mul: return "mul";
		case TokenType::div: return "div";
		case TokenType::dot: return "dot";
		case TokenType::ref: return "ref";
		case TokenType::hat: return "hat";
		case TokenType::question: return "question";
		case TokenType::squote: return "squote";
		case TokenType::comment: return "comment";
		case TokenType::kwVar: return "kwVar";
		case TokenType::kwLet: return "kwLet";
		case TokenType::kwFn: return "kwFn";
		case TokenType::kwStruct: return "kwStruct";
		case TokenType::kwReturn: return "kwReturn";
		case TokenType::kwGoto: return "kwGoto";
		case TokenType::kwBreak: return "kwBreak";
		case TokenType::kwContinue: return "kwContinue";
		case TokenType::kwElse: return "kwElse";
		case TokenType::kwNull: return "kwNull";
		case TokenType::kwLoop: return "kwLoop";
		case TokenType::kwIf: return "kwIf";
		case TokenType::unknown:
		default: return "unknown";
	}
}

static const char* str(TokenType type)
{
	switch (type) {
		case TokenType::eof: return "";
		case TokenType::name: return "";
		case TokenType::number: return "";
		case TokenType::assign: return "=";
		case TokenType::declaration: return "";
		case TokenType::endStatement: return ";";
		case TokenType::comma: return ",";
		case TokenType::openParen: return "(";
		case TokenType::closeParen: return ")";
		case TokenType::openBlock: return "{";
		case TokenType::closeBlock: return "}";
		case TokenType::openSquare: return "[";
		case TokenType::closeSquare: return "]";
		case TokenType::rightArrow: return "->";
		case TokenType::equals: return "==";
		case TokenType::nequals: return "!=";
		case TokenType::less: return "<";
		case TokenType::greater: return ">";
		case TokenType::add: return "+";
		case TokenType::sub: return "-";
		case TokenType::mul: return "*";
		case TokenType::div: return "/";
		case TokenType::dot: return ".";
		case TokenType::ref: return "&";
		case TokenType::hat: return "^";
		case TokenType::question: return "?";
		case TokenType::squote: return "'";
		case TokenType::comment: return "//";
		case TokenType::kwVar: return "var";
		case TokenType::kwLet: return "let";
		case TokenType::kwFn: return "fn";
		case TokenType::kwStruct: return "struct";
		case TokenType::kwReturn: return "return";
		case TokenType::kwGoto: return "goto";
		case TokenType::kwBreak: return "break";
		case TokenType::kwContinue: return "continue";
		case TokenType::kwElse: return "else";
		case TokenType::kwNull: return "null";
		case TokenType::kwLoop: return "loop";
		case TokenType::kwIf: return "if";
		case TokenType::unknown:
		default: return "???";
	}
}

struct Token {
	TokenType type;
	std::string text;
	bool lastOnLine;
};

using Tokens= std::vector<Token>;
Tokens tokenize(const char* filepath);

} // gamelang

#endif // GAMELANG_TOKEN_HPP
