#ifndef GAMELANG_AST_HPP
#define GAMELANG_AST_HPP

#include <memory>
#include <string>
#include <vector>

#include "token.hpp"

namespace gamelang
{

struct AstNode;
using AstNodePtr= std::unique_ptr<AstNode>;

enum class AstNodeType {
	global,
	block,
	varDecl,
	type,
	structType,
	funcType,
};

struct AstNode {
	AstNodeType type;

	AstNode(AstNodeType t): type(t) {}
};

struct GlobalNode : AstNode {
	std::vector<AstNodePtr> nodes;

	GlobalNode(): AstNode(AstNodeType::global) {}
};

struct BlockNode : AstNode {
	bool structure= false;
	AstNodePtr functionType;
	std::vector<AstNodePtr> nodes;

	BlockNode(): AstNode(AstNodeType::block) {}
};

struct VarDeclNode : AstNode {
	bool constant= true;
	std::string name;
	AstNodePtr type;
	AstNodePtr value;

	VarDeclNode(): AstNode(AstNodeType::varDecl) {}
};

struct TypeNode : AstNode {
	TypeNode(): AstNode(AstNodeType::type) {}
	TypeNode(AstNodeType t): AstNode(t) {}
};

struct StructTypeNode : TypeNode {
	StructTypeNode(): TypeNode(AstNodeType::structType) {}
};

struct FuncTypeNode : TypeNode {
	FuncTypeNode(): TypeNode(AstNodeType::funcType) {}
};

namespace detail
{

struct Parser {
	const Tokens& tokens;
	int logIndent;

	using It= Tokens::const_iterator;
	template <typename T>
	using UPtr= std::unique_ptr<T>;
	using Str= std::string;

	template <typename T>
	UPtr<T> newNode() const { return UPtr<T>(new T{}); }

	void parseCheck(bool expr, const char* msg)
	{ if (!expr) log(msg); assert(expr); }

	void log(const Str& str) const
	{
		for (int i= 0; i < logIndent; ++i)
			std::cout << "  ";
		std::cout << str << std::endl;
	}

	struct LogIndent {
		int& value;
		LogIndent(int& v): value(v) { ++value; }
		~LogIndent() { --value; }
	};
#define LOG_INDENT_SCOPE() LogIndent indent{logIndent}

	void nextToken(It& it)
	{
		assert(it != tokens.end());
		++it;
		parseCheck(it != tokens.end(), "Unexpected end of file");
	}

	void advance(It& it) { ++it; }

	UPtr<VarDeclNode> parseVarDecl(It& tok)
	{
		log("parseVarDecl");
		LOG_INDENT_SCOPE();

		auto var= newNode<VarDeclNode>();
		nextToken(tok); // Skip "let"

		parseCheck(tok->type == TokenType::identifier, "Error in var decl name");
		var->name= tok->text;
		log(var->name);
		nextToken(tok);

		parseCheck(tok->type == TokenType::declaration, "Missing : in var decl");
		nextToken(tok);

		if (tok->type == TokenType::identifier) { // Explicit type
			log(":");
			var->type= parseType(tok);
			nextToken(tok);
		}
		
		if (tok->type == TokenType::endStatement) {
			advance(tok);
			return std::move(var);
		}

		if (tok->type == TokenType::assign) {
			log("=");
			nextToken(tok);
			var->value= parseExpr(tok);
		} 

		if (tok->type == TokenType::endStatement)
			advance(tok);

		return std::move(var);
	}

	UPtr<TypeNode> parseType(It& tok)
	{
		parseCheck(tok->type == TokenType::identifier, "Invalid type");
		if (tok->text == "fn") {
			return parseFuncType(tok);	
		} else {
			log(tok->text);
			return newNode<StructTypeNode>();
		}
	}

	UPtr<FuncTypeNode> parseFuncType(It& tok)
	{
		log("parseFuncType");
		LOG_INDENT_SCOPE();

		nextToken(tok); // Skip "fn"

		parseCheck(tok->type == TokenType::openParen, "Missing ( in fn type");
		nextToken(tok);

		/// @todo Parse arguments
		auto func_type= newNode<FuncTypeNode>();
		
		parseCheck(tok->type == TokenType::closeParen, "Missing ) in fn type");	
		nextToken(tok);

		return std::move(func_type);
	}

	UPtr<BlockNode> parseBlock(It& tok)
	{
		log("parseBlock");
		LOG_INDENT_SCOPE();

		nextToken(tok); // Skip "{"

		auto block= newNode<BlockNode>();
		while (tok->type != TokenType::closeBlock) {
			block->nodes.emplace_back(parseExpr(tok));
		}

		advance(tok); // Jump over "{"
		return std::move(block);
	}

	UPtr<AstNode> parseExpr(It& tok)
	{
		//log("parseExpr");
		//LOG_INDENT_SCOPE();

		if (tok->type == TokenType::identifier) {
			if (tok->text == "let") {
				return parseVarDecl(tok);
			} else if (tok->text == "fn") {
				auto func_type_node= parseFuncType(tok);
				/// @todo Other block properties
				if (tok->type == TokenType::endStatement) {
					return std::move(func_type_node);
				} else if (tok->type == TokenType::openBlock) {
					auto block= parseBlock(tok);
					block->functionType= std::move(func_type_node);
					return std::move(block);
				} else {
					parseCheck(false, "Rubbish after function type");
				}
			}
		}

		parseCheck(false, "Broken expression");
	}

	UPtr<AstNode> parse()
	{
		// Start parsing
		auto root= newNode<GlobalNode>();
		auto it= tokens.begin();
		while (it != tokens.end()) {
			root->nodes.emplace_back(parseExpr(it));
		}
		return std::move(root);
	}

#undef LOG_INDENT_SCOPE
};

} // detail

AstNodePtr generateAst(const Tokens& tokens)
{
	detail::Parser parser{tokens};
	return parser.parse();
}

} // gamelang

#endif // GAMELANG_AST_HPP
