#ifndef GAMELANG_AST_HPP
#define GAMELANG_AST_HPP

#include <cassert>
#include <list>
#include <memory>
#include <string>
#include <vector>

#include "nullsafety.hpp"
#include "token.hpp"

namespace gamelang
{

template <typename T>
std::vector<T> listAsVec(const std::list<T>& l)
{
	std::vector<T> v;
	for (auto&& m : l)
		v.emplace_back(m);
	return v;
}

struct AstNode;
struct AstContext {

	bool hasRootNode() const { return !nodes.empty(); }
	AstNode& getRootNode() const
	{
		assert(hasRootNode());
		return *NONULL(nodes.front().get());
	}

	template <typename T>
	T* newNode()
	{
		nodes.emplace_back(std::unique_ptr<T>(new T{}));
		return static_cast<T*>(nodes.back().get());
	}
private:
	/// First should be the GlobalNode
	std::list<std::unique_ptr<AstNode>> nodes;
};

enum class AstNodeType {
	global,
	block,
	varDecl,
	identifier,
	paramDecl,
	funcType,
	structType,
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
	virtual std::vector<AstNode*> getSubNodes() const = 0;
};

struct GlobalNode final : AstNode {
	std::list<AstNode*> nodes;

	GlobalNode(): AstNode(AstNodeType::global) {}
	std::vector<AstNode*> getSubNodes() const override { return listAsVec(nodes); }
};

struct BlockNode final : AstNode {
	bool structure= false;
	AstNode* funcType= nullptr;
	AstNode* condition= nullptr;
	std::list<AstNode*> nodes;

	BlockNode(): AstNode(AstNodeType::block) {}
	std::vector<AstNode*> getSubNodes() const override
	{
		auto ret= listAsVec(nodes);
		ret.push_back(funcType);
		ret.push_back(condition);
		return ret;
	}
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
	std::list<ParamDeclNode*> params;

	FuncTypeNode(): AstNode(AstNodeType::funcType) {}
	std::vector<AstNode*> getSubNodes() const override
	{
		std::vector<AstNode*> ret{returnType};
		for (auto* p : params)
			ret.push_back(p);
		return ret;
	}
};

struct StructTypeNode final : AstNode {
	StructTypeNode(): AstNode(AstNodeType::structType) {}
	std::vector<AstNode*> getSubNodes() const override { return {}; }
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
	AstNode* func= nullptr;
	std::list<AstNode*> args;

	CallNode(): AstNode(AstNodeType::call) {}
	std::vector<AstNode*> getSubNodes() const override
	{ auto ret= listAsVec(args); ret.push_back(func); return ret; }
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
