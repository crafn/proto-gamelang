#ifndef GAMELANG_AST_HPP
#define GAMELANG_AST_HPP

#include <cassert>
#include <list>
#include <memory>
#include <string>
#include <vector>
#include <map>

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
};

struct GlobalNode final : AstNode {
	std::list<AstNode*> nodes;

	GlobalNode(): AstNode(AstNodeType::global) {}
};

struct EndStatementNode final : AstNode {
	EndStatementNode(): AstNode(AstNodeType::endStatement) {}
};

struct VarDeclNode;
/// `foobar5`
struct IdentifierNode final : AstNode {
	/// Can be VarDeclNode or IdentifierNode (for labels)
	AstNode* boundTo= nullptr;

	std::string name;

	IdentifierNode(): AstNode(AstNodeType::identifier) {}
};

struct StructTypeNode;
struct FuncTypeNode;
struct TplTypeNode;
/// `{ ... }`
struct BlockNode final : AstNode {
	IdentifierNode* boundTo= nullptr;
	BlockNode* enclosing= nullptr;

	bool loop= false;
	bool external= false; /// FFI
	StructTypeNode* structType= nullptr;
	FuncTypeNode* funcType= nullptr;
	TplTypeNode* tplType= nullptr; /// Used if block is a template
	AstNode* condition= nullptr;
	BlockNode* destructor= nullptr;
	std::list<AstNode*> nodes;

	BlockNode(): AstNode(AstNodeType::block) {}
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
};

/// `fn (n : int) -> void`
struct FuncTypeNode final : AstNode {
	AstNode* returnType= nullptr;
	std::list<VarDeclNode*> params;

	FuncTypeNode(): AstNode(AstNodeType::funcType) {}
};

/// Type of struct type
/// `struct`
struct StructTypeNode final : AstNode {
	/// Var decls picked from the block for easy access
	std::vector<VarDeclNode*> varDecls;

	StructTypeNode(): AstNode(AstNodeType::structType) {}
};

/// Type of `int` and friends
/// Used in dummy declaration `let int : builtin;`
struct BuiltinTypeNode final : AstNode {
	BuiltinTypeNode(): AstNode(AstNodeType::builtinType) {}
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
};

struct StringLiteralNode final : AstNode {
	std::string str;

	StringLiteralNode(): AstNode(AstNodeType::stringLiteral) {}
};

struct NullLiteralNode final : AstNode {
	NullLiteralNode(): AstNode(AstNodeType::nullLiteral) {}
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
};

// TokenType contains all needed values
using BiOpType= TokenType;

/// `a + b`
struct BiOpNode final : AstNode {
	AstNode* lhs= nullptr;
	AstNode* rhs= nullptr;
	BiOpType opType;

	BiOpNode(): AstNode(AstNodeType::biOp) {}
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
};

struct LabelNode final : AstNode {
	IdentifierNode* identifier= nullptr;
	LabelNode(): AstNode(AstNodeType::label) {}
};

struct CommentNode final : AstNode {
	std::string	text;
	CommentNode(): AstNode(AstNodeType::comment) {}
};

/// `tpl [params]`
struct TplTypeNode final : AstNode {
	std::vector<VarDeclNode*> params;

	TplTypeNode(): AstNode(AstNodeType::tplType) {}
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

	/// Declaration info of an identifier in ast
	struct IdDef {
		IdentifierNode* idNode= nullptr;
		BlockNode* enclosing= nullptr;
	};

	/// id-string -> all matching IdDefs
	std::map<std::string, std::vector<IdDef>> idDefs;

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
