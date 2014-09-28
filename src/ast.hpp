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
using AstNodePtr= std::unique_ptr<AstNode>;

enum class AstNodeType {
	global,
	block,
	varDecl,
	type,
	structType,
	funcType,
	numLiteral
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
	AstNodePtr valueType;
	AstNodePtr value;

	VarDeclNode(): AstNode(AstNodeType::varDecl) {}
};

struct TypeNode : AstNode {
	TypeNode(): AstNode(AstNodeType::type) {}
	TypeNode(AstNodeType t): AstNode(t) {}
};

struct StructTypeNode : TypeNode {
	std::string name;
	StructTypeNode(): TypeNode(AstNodeType::structType) {}
};

struct FuncTypeNode : TypeNode {
	FuncTypeNode(): TypeNode(AstNodeType::funcType) {}
};

struct NumLiteralNode : AstNode {
	std::string value;
	NumLiteralNode(): AstNode(AstNodeType::numLiteral) {}
};

AstNodePtr genAst(const Tokens& tokens);

} // gamelang

#endif // GAMELANG_AST_HPP
