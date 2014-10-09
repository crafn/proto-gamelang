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
std::vector<T> listToVec(const std::list<T>& l)
{
	std::vector<T> v;
	for (auto&& m : l)
		v.emplace_back(m);
	return v;
}

template <typename T>
std::list<T> vecToList(const std::vector<T>& l)
{
	std::list<T> v;
	for (auto&& m : l)
		v.emplace_back(m);
	return v;
}

enum class AstNodeType {
	global,
	endStatement,
	identifier,
	block,
	varDecl,
	funcType,
	structType,
	builtinType,
	numLiteral,
	nullLiteral,
	uOp,
	biOp,
	ctrlStatement,
	call,
	qualifier,
	label,
	comment
};

struct AstNode {
	AstNodeType type;

	AstNode(AstNodeType t): type(t) {}
	virtual std::vector<AstNode*> getSubNodes() const = 0;
	virtual ~AstNode() {}
};

struct GlobalNode final : AstNode {
	std::list<AstNode*> nodes;

	GlobalNode(): AstNode(AstNodeType::global) {}
	std::vector<AstNode*> getSubNodes() const override { return listToVec(nodes); }
};

struct EndStatementNode final : AstNode {
	EndStatementNode(): AstNode(AstNodeType::endStatement) {}
	std::vector<AstNode*> getSubNodes() const override { return {}; }
};

struct VarDeclNode;
/// `foobar5`
struct IdentifierNode final : AstNode {
	/// Can be VarDeclNode or IdentifierNode (for labels)
	AstNode* boundTo= nullptr;

	std::string name;

	IdentifierNode(): AstNode(AstNodeType::identifier) {}
	std::vector<AstNode*> getSubNodes() const override { return {}; }
};

/// `{ ... }`
struct BlockNode final : AstNode {
	IdentifierNode* boundTo= nullptr;

	bool loop= false; // Defined with `loop` keyword
	AstNode* structType= nullptr;
	AstNode* funcType= nullptr;
	AstNode* condition= nullptr;
	std::list<AstNode*> nodes;

	BlockNode(): AstNode(AstNodeType::block) {}
	std::vector<AstNode*> getSubNodes() const override
	{
		auto ret= listToVec(nodes);
		ret.push_back(funcType);
		ret.push_back(condition);
		return ret;
	}
};

/// `let x : int = 15`
struct VarDeclNode final : AstNode {
	bool constant= true;
	IdentifierNode* identifier= nullptr;
	AstNode* valueType= nullptr;
	AstNode* value= nullptr;
	bool param= false;

	VarDeclNode(): AstNode(AstNodeType::varDecl) {}
	std::vector<AstNode*> getSubNodes() const override
	{ return {identifier, valueType, value}; }
};

/// `fn (n : int) -> void`
struct FuncTypeNode final : AstNode {
	AstNode* returnType= nullptr;
	std::list<VarDeclNode*> params;

	FuncTypeNode(): AstNode(AstNodeType::funcType) {}
	std::vector<AstNode*> getSubNodes() const override
	{
		std::vector<AstNode*> ret{returnType};
		for (auto* p : params)
			ret.push_back(p);
		return ret;
	}
};

/// `struct`
struct StructTypeNode final : AstNode {
	/// Var decls picked from the block for easy access
	std::vector<VarDeclNode*> varDecls;

	StructTypeNode(): AstNode(AstNodeType::structType) {}
	std::vector<AstNode*> getSubNodes() const override
	{
		std::vector<AstNode*> ret;
		for (auto* p : varDecls)
			ret.push_back(p);
		return ret;
	}
};

/// Type of `int`
/// Used in dummy declaration `let int : builtin;`
struct BuiltinTypeNode final : AstNode {
	BuiltinTypeNode(): AstNode(AstNodeType::builtinType) {}
	std::vector<AstNode*> getSubNodes() const override { return {}; }
};

struct NumLiteralNode final : AstNode {
	std::string value;

	NumLiteralNode(): AstNode(AstNodeType::numLiteral) {}
	std::vector<AstNode*> getSubNodes() const override { return {}; }
};

struct NullLiteralNode final : AstNode {
	NullLiteralNode(): AstNode(AstNodeType::nullLiteral) {}
	std::vector<AstNode*> getSubNodes() const override { return {}; }
};

enum class UOpType {
	declType,
	addrOf,
	pointer, // type qualifier
	reference // type qualifier
};

/// `!flag`
struct UOpNode final : AstNode {
	AstNode* target= nullptr;
	UOpType opType;

	UOpNode(): AstNode(AstNodeType::uOp) {}
	std::vector<AstNode*> getSubNodes() const override { return {target}; }
};

// TokenType contains all needed values
using BiOpType= TokenType;

/// `a + b`
struct BiOpNode final : AstNode {
	AstNode* lhs= nullptr;
	AstNode* rhs= nullptr;
	BiOpType opType;

	BiOpNode(): AstNode(AstNodeType::biOp) {}
	std::vector<AstNode*> getSubNodes() const override { return {lhs, rhs}; }
};

enum class CtrlStatementType {
	return_,
	goto_,
	break_,
	continue_,
	else_
};

/// `return value`, `break`
struct CtrlStatementNode final : AstNode {
	CtrlStatementType statementType= CtrlStatementType::return_;
	AstNode* value= nullptr;

	CtrlStatementNode(): AstNode(AstNodeType::ctrlStatement) {}
	std::vector<AstNode*> getSubNodes() const override { return {value}; }
};

/// `foo(a, b, c)`
struct CallNode final : AstNode {
	IdentifierNode* func= nullptr;
	std::list<AstNode*> args;
	/// Extends `args`
	std::vector<AstNode*> implicitArgs;

	/// namedArgs[i] corresponds to args[i]
	/// namedArgs[i].empty() == ordinary argument
	std::vector<std::string> namedArgs;

	/// argRouting[arg_i] == index in func decl
	std::vector<int> argRouting;

	CallNode(): AstNode(AstNodeType::call) {}
	std::vector<AstNode*> getSubNodes() const override
	{ auto ret= listToVec(args); ret.push_back(func); return ret; }
};

struct LabelNode final : AstNode {
	IdentifierNode* identifier= nullptr;
	LabelNode(): AstNode(AstNodeType::label) {}
	std::vector<AstNode*> getSubNodes() const override { return {identifier}; }
};

struct CommentNode final : AstNode {
	std::string	text;
	CommentNode(): AstNode(AstNodeType::comment) {}
	std::vector<AstNode*> getSubNodes() const override { return {}; }
};

struct AstContext {
	AstContext();

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

	VarDeclNode& getBuiltinTypeDecl() { return *NONULL(builtinDecl.get()); }

private:
	/// First should be the GlobalNode
	std::list<std::unique_ptr<AstNode>> nodes;

	/// Dummy declaration of int and others
	std::unique_ptr<BuiltinTypeNode> builtinType;
	std::unique_ptr<IdentifierNode> builtinId;
	std::unique_ptr<VarDeclNode> builtinDecl;
};

AstContext genAst(const Tokens& tokens);

} // gamelang

#endif // GAMELANG_AST_HPP
