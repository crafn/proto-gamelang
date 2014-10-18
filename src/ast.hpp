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

template <typename A, typename B>
void insert(A&& a, B&& b)
{
	for (auto&& item : b) {
		a.insert(a.end(), item);
	}
}

template <typename A, typename B>
A joined(const A& a, const B& b)
{
	A temp;
	insert(temp, a);
	insert(temp, b);
	return temp;
}

void parseCheck(bool expr, const std::string& msg);

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
	stringLiteral,
	nullLiteral,
	uOp,
	biOp,
	ctrlStatement,
	call,
	label,
	comment,
	tplType
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

struct StructTypeNode;
struct TplTypeNode;
/// `{ ... }`
struct BlockNode final : AstNode {
	IdentifierNode* boundTo= nullptr;

	bool loop= false;
	bool external= false; /// FFI
	StructTypeNode* structType= nullptr;
	AstNode* funcType= nullptr;
	TplTypeNode* tplType= nullptr; /// Used if block is a template
	AstNode* condition= nullptr;
	BlockNode* destructor= nullptr;
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
	/// @todo Remove
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

/// Type of struct type
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

/// Type of `int` and friends
/// Used in dummy declaration `let int : builtin;`
struct BuiltinTypeNode final : AstNode {
	BuiltinTypeNode(): AstNode(AstNodeType::builtinType) {}
	std::vector<AstNode*> getSubNodes() const override { return {}; }
};

enum class NumLiteralType {
	none,
	int_,
	uint_,
	i32_,
	i64_,
	bool_,
	f32_,
	f64_,
	char_,
	byte_
};

struct NumLiteralNode final : AstNode {
	NumLiteralType literalType= NumLiteralType::none;
	std::string value;

	VarDeclNode* builtinDecl= nullptr;

	NumLiteralNode(): AstNode(AstNodeType::numLiteral) {}
	std::vector<AstNode*> getSubNodes() const override { return {}; }
};

struct StringLiteralNode final : AstNode {
	std::string str;

	StringLiteralNode(): AstNode(AstNodeType::stringLiteral) {}
	std::vector<AstNode*> getSubNodes() const override { return {}; }
};

struct NullLiteralNode final : AstNode {
	NullLiteralNode(): AstNode(AstNodeType::nullLiteral) {}
	std::vector<AstNode*> getSubNodes() const override { return {}; }
};

enum class UOpType {
	declType,
	addrOf,
	deref,
	pointer, // type qualifier
	reference, // type qualifier
	sizeOf
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
	AstNode* func= nullptr;
	std::list<AstNode*> args;
	/// namedArgs[i] corresponds to args[i]
	/// namedArgs[i].empty() == ordinary argument
	std::vector<std::string> namedArgs;
	/// @todo Rename, indexing[4] uses this flag also at parser stage
	bool squareCall= false; /// true if call has form `foo[..]`

	// Metaprocessor sets

	/// Extends `args`
	std::vector<AstNode*> implicitArgs;
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

/// `tpl [params]`
struct TplTypeNode final : AstNode {
	std::vector<VarDeclNode*> params;

	TplTypeNode(): AstNode(AstNodeType::tplType) {}
	std::vector<AstNode*> getSubNodes() const override
	{
		std::vector<AstNode*> ret;
		for (auto* p : params)
			ret.push_back(p);
		return ret;
	}
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

private:
	/// First should be the GlobalNode
	std::list<std::unique_ptr<AstNode>> nodes;
};

/// e.g. identifier -> func block
///      vector -> tpl block
///      identifier -> extern identifier
const AstNode& traceValue(const AstNode& expr);
static AstNode& traceValue(AstNode& node)
{ return const_cast<AstNode&>(traceValue(static_cast<const AstNode&>(node))); }

/// e.g. identifier -> func type
///      vector -> tpl type
///      identifier -> struct type
const AstNode& traceType(const AstNode& expr);
static AstNode& traceType(AstNode& node)
{ return const_cast<AstNode&>(traceType(static_cast<const AstNode&>(node))); }

enum class BoundIdDist {
	furthest, /// The original declaration of some value/type
	nearest /// First name on the way when tracing towards the original decl
};
/// e.g. id -> id.boundTo->identifier
///      block -> block.boundTo
const IdentifierNode& traceBoundId(const AstNode& node, BoundIdDist dist);
static IdentifierNode& traceBoundId(AstNode& node, BoundIdDist dist)
{ return const_cast<IdentifierNode&>(traceBoundId(static_cast<const AstNode&>(node), dist)); }

std::string mangledName(AstNode& node);

/// Finds implicit parameters and sets up a routing table
void routeCallArgs(	std::vector<AstNode*>& implicit,
					std::vector<int>& routing,
					const AstNode& func,
					const std::vector<std::string>& arg_names);

/// Returns arguments in resolved order
std::vector<AstNode*> resolveRouting(	const std::vector<AstNode*>& args,
										const std::vector<int>& routing);
										

AstContext genAst(const Tokens& tokens);

} // gamelang

#endif // GAMELANG_AST_HPP
