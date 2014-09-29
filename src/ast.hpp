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
	paramDecl,
	funcType,
	numLiteral,
	biOp,
	ret,
	call
};

struct AstNode {
	AstNodeType type;
	/// true if node has `;` immediately after it
	bool endStatement= false;

	AstNode(AstNodeType t): type(t) {}
	virtual std::vector<AstNode*> getSubNodes() const { return {}; }
};

struct GlobalNode final : AstNode {
	std::vector<AstNode*> nodes;

	GlobalNode(): AstNode(AstNodeType::global) {}
	std::vector<AstNode*> getSubNodes() const override { return nodes; }
};

struct BlockNode final : AstNode {
	bool structure= false;
	AstNode* functionType;
	std::vector<AstNode*> nodes;

	BlockNode(): AstNode(AstNodeType::block) {}
	std::vector<AstNode*> getSubNodes() const override { return nodes; }
};

struct VarDeclNode final : AstNode {
	bool constant= true;
	std::string name;
	AstNode* valueType= nullptr;
	AstNode* value= nullptr;

	VarDeclNode(): AstNode(AstNodeType::varDecl) {}
	std::vector<AstNode*> getSubNodes() const override { return {valueType, value}; }
};

struct IdentifierNode final : AstNode {
	std::string name;

	IdentifierNode(): AstNode(AstNodeType::identifier) {}
	std::vector<AstNode*> getSubNodes() const override { return {}; }
};

struct ParamDeclNode final : AstNode {
	std::string name;
	AstNode* valueType= nullptr;

	ParamDeclNode(): AstNode(AstNodeType::paramDecl) {}
	std::vector<AstNode*> getSubNodes() const override { return {valueType}; }
};

struct FuncTypeNode final : AstNode {
	AstNode* returnType= nullptr;
	std::vector<ParamDeclNode*> params;

	FuncTypeNode(): AstNode(AstNodeType::funcType) {}
	std::vector<AstNode*> getSubNodes() const override
	{
		std::vector<AstNode*> ret{returnType};
		for (auto* p : params)
			ret.push_back(p);
		return ret;
	}
};

struct NumLiteralNode final : AstNode {
	std::string value;

	NumLiteralNode(): AstNode(AstNodeType::numLiteral) {}
	std::vector<AstNode*> getSubNodes() const override { return {}; }
};

struct ReturnNode final : AstNode {
	AstNode* value= nullptr;

	ReturnNode(): AstNode(AstNodeType::ret) {}
	std::vector<AstNode*> getSubNodes() const override { return {value}; }
};

struct CallNode final : AstNode {
	AstNode* function;
	std::vector<AstNode*> params;

	CallNode(): AstNode(AstNodeType::call) {}
	std::vector<AstNode*> getSubNodes() const override
	{ auto ret= params; ret.push_back(function); return ret; }
};

/// TokenType contains all needed values
using BiOpType= TokenType;

struct BiOpNode final : AstNode {
	AstNode* lhs= nullptr;
	AstNode* rhs= nullptr;
	BiOpType opType;

	BiOpNode(): AstNode(AstNodeType::biOp) {}
	std::vector<AstNode*> getSubNodes() const override { return {lhs, rhs}; }
};

bool containsEndStatement(const AstNode& node);
AstContext genAst(const Tokens& tokens);

} // gamelang

#endif // GAMELANG_AST_HPP
