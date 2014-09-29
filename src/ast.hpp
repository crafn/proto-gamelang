#ifndef GAMELANG_AST_HPP
#define GAMELANG_AST_HPP

#include <memory>
#include <string>
#include <vector>

// Debug
#include <iostream>

#include "token.hpp"

namespace gamelang
{

struct AstNode;
struct AstContext {
	/// First should be the GlobalNode
	std::vector<std::unique_ptr<AstNode>> nodes;
};

enum class AstNodeType {
	global,
	block,
	varDecl,
	identifier,
	funcType,
	numLiteral
};

struct AstNode {
	AstNodeType type;

	AstNode(AstNodeType t): type(t) {}
};

struct GlobalNode : AstNode {
	std::vector<AstNode*> nodes;

	GlobalNode(): AstNode(AstNodeType::global) {}
};

struct BlockNode : AstNode {
	bool structure= false;
	AstNode* functionType;
	std::vector<AstNode*> nodes;

	BlockNode(): AstNode(AstNodeType::block) {}
};

struct VarDeclNode : AstNode {
	bool constant= true;
	std::string name;
	AstNode* valueType= nullptr;
	AstNode* value= nullptr;

	VarDeclNode(): AstNode(AstNodeType::varDecl) {}
};

struct IdentifierNode : AstNode {
	std::string name;

	IdentifierNode(): AstNode(AstNodeType::identifier) {}
};

struct FuncTypeNode : AstNode {
	AstNode* returnType;

	FuncTypeNode(): AstNode(AstNodeType::funcType) {}
};

struct NumLiteralNode : AstNode {
	std::string value;

	NumLiteralNode(): AstNode(AstNodeType::numLiteral) {}
};

AstContext genAst(const Tokens& tokens);

} // gamelang

#endif // GAMELANG_AST_HPP
