#include "ast.hpp"
#include "nullsafety.hpp"

#include <stack>
#include <type_traits>

// Debug
#include <iostream>

namespace gamelang
{
namespace
{

int logIndent;
void log(const std::string& str)
{
	for (int i= 0; i < logIndent; ++i)
		std::cout << "  ";
	std::cout << str << std::endl;
}

struct LogIndentGuard {
	int& value;
	LogIndentGuard(int& v): value(v) { ++value; }
	~LogIndentGuard() { --value; }
};

LogIndentGuard logIndentGuard()
{ return LogIndentGuard{logIndent}; }

} // anonymous

void parseCheck(bool expr, const std::string& msg)
{
	if (!expr)
		log(msg);
	/// @todo Proper error messaging
	assert(expr);
}

namespace
{

/// Pratt parser binding power
enum class Bp : int {
	eof= 0,
	comment,
	endParens,
	keyword,
	literal,
	name,
	comma,
	statement, /// @todo This should probably be almost first
	assignment,
	comp,
	typedecl,
	sum,
	prod,
	insert,
	block,
	blockBind,
	parens,
	prefix,
	index,
	member
};

Bp tokenLbp(TokenType t)
{
	switch (t) {
		case TokenType::eof:          return Bp::eof;
		case TokenType::name:         return Bp::literal;
		case TokenType::number:       return Bp::literal;
		case TokenType::string:       return Bp::literal;
		case TokenType::assign:       return Bp::assignment;
		case TokenType::declaration:  return Bp::typedecl;
		case TokenType::endStatement: return Bp::statement;
		case TokenType::comma:        return Bp::comma;
		case TokenType::openParen:    return Bp::parens;
		case TokenType::closeParen:   return Bp::endParens;
		case TokenType::openBlock:    return Bp::block;
		case TokenType::closeBlock:   return Bp::endParens;
		case TokenType::openSquare:   return Bp::index;
		case TokenType::closeSquare:  return Bp::endParens;
		case TokenType::rightInsert:  return Bp::insert;
		case TokenType::rightArrow:   return Bp::member;
		case TokenType::equals:       return Bp::comp;
		case TokenType::nequals:      return Bp::comp;
		case TokenType::less:         return Bp::comp;
		case TokenType::greater:      return Bp::comp;
		case TokenType::add:          return Bp::sum;
		case TokenType::sub:          return Bp::sum;
		case TokenType::mul:          return Bp::prod;
		case TokenType::div:          return Bp::prod;
		case TokenType::mod:          return Bp::prod;
		case TokenType::dot:          return Bp::member;
		case TokenType::amp:          return Bp::statement;
		case TokenType::hat:          return Bp::prefix;
		case TokenType::question:     return Bp::prefix;
		case TokenType::tilde:        return Bp::blockBind;
		case TokenType::squote:       return Bp::statement;
		case TokenType::comment:      return Bp::comment;
		case TokenType::kwVar:        return Bp::keyword;
		case TokenType::kwLet:        return Bp::keyword;
		case TokenType::kwFn:         return Bp::keyword;
		case TokenType::kwStruct:     return Bp::keyword;
		case TokenType::kwReturn:     return Bp::keyword;
		case TokenType::kwGoto:       return Bp::keyword;
		case TokenType::kwBreak:      return Bp::keyword;
		case TokenType::kwContinue:   return Bp::keyword;
		case TokenType::kwNull:       return Bp::keyword;
		case TokenType::kwLoop:       return Bp::keyword;
		case TokenType::kwIf:         return Bp::keyword;
		case TokenType::kwElse:       return Bp::keyword;
		case TokenType::kwExtern:     return Bp::keyword;
		case TokenType::kwTpl:        return Bp::keyword;
		case TokenType::kwSizeof:     return Bp::keyword;
		case TokenType::kwTrue:       return Bp::literal;
		case TokenType::kwFalse:      return Bp::literal;
		default: log(enumStr(t)); assert(0 && "Missing token binding power");
	}
}

Bp tokenRbp(TokenType t)
{
	switch (t)
	{
		case TokenType::add:
		case TokenType::sub:
		case TokenType::mul:
		case TokenType::div:
		case TokenType::amp:
		case TokenType::squote:
			return Bp::prefix;
		default:;
	}
	return tokenLbp(t);
}

const char* str(NumLiteralType n)
{
	switch (n) {
		case NumLiteralType::int_: return "int";
		case NumLiteralType::uint_: return "uint";
		case NumLiteralType::i32_: return "i32";
		case NumLiteralType::i64_: return "i64";
		case NumLiteralType::bool_: return "bool";
		case NumLiteralType::f32_: return "f32";
		case NumLiteralType::f64_: return "f64";
		case NumLiteralType::char_: return "char";
		case NumLiteralType::byte_: return "byte";
		default: assert(0 && "Unknown NumLiteralType");
	}
}

/// Transforms tokens to an abstract syntax tree
struct Parser {
	Parser(const Tokens& t): tokens(t) {}

	AstContext parse()
	{
		auto root= newNode<GlobalNode>();

		// Insert builtin types of language
		std::vector<std::string> builtin_names= {
			str(NumLiteralType::int_),
			str(NumLiteralType::uint_),
			str(NumLiteralType::i32_),
			str(NumLiteralType::i64_),
			str(NumLiteralType::bool_),
			str(NumLiteralType::f32_),
			str(NumLiteralType::f64_),
			str(NumLiteralType::char_),
			str(NumLiteralType::byte_),
			"void"
		};
		for (auto& name : builtin_names) {
			auto builtin_type= context.newNode<BuiltinTypeNode>();
			auto builtin_id= context.newNode<IdentifierNode>();
			auto builtin_decl= context.newNode<VarDeclNode>();

			builtin_id->name= name;
			builtin_id->boundTo= builtin_decl;
			builtin_decl->identifier= builtin_id;
			builtin_decl->valueType= builtin_type;
			root->nodes.emplace_back(builtin_decl);

			IdDef id_def;
			id_def.idNode= builtin_id;
			context.idDefs[name].emplace_back(id_def);
		}

		// Start parsing
		token= tokens.begin();
		while (token->type != TokenType::eof) {
			root->nodes.emplace_back(parseExpr());
		}
		return std::move(context);
	}

private:
	const Tokens& tokens;
	Tokens::const_iterator token;
	AstContext context;
	std::stack<BlockNode*> blockStack;

	using IdDef= AstContext::IdDef;
	using It= Tokens::const_iterator;
	template <typename T>
	using UPtr= std::unique_ptr<T>;
	using Str= std::string;

	template <typename T>
	T* newNode()
	{ return context.newNode<T>(); }

	void advance()
	{
		assert(token != tokens.end());
		++token;
		assert(token != tokens.end());
	}

	void match(TokenType t, std::string err= "Match failed")
	{
		parseCheck(t == token->type, err);
		advance();
	}

	AstNode* deducedType(AstNode& thing)
	{
		if (thing.type == AstNodeType::block) {
			const BlockNode& block= static_cast<const BlockNode&>(thing);
			if (block.tplType)
				return block.tplType;
			if (block.structType)
				return block.structType;
			if (block.funcType)
				return block.funcType;		
		} else {
			auto op= newNode<UOpNode>();
			op->opType= UOpType::declType;
			op->target= &thing;
			return op;
		}
		
		parseCheck(false, "Unable to deduce type");
	}

	VarDeclNode& getBuiltinDecl(const std::string& str)
	{
		auto it= context.idDefs.find(str);
		parseCheck(it != context.idDefs.end(), "Builtin id not found");
		const std::vector<IdDef>& defs= it->second;
		parseCheck(defs.size() == 1, "Multiple definitions for a builtin");
		IdentifierNode* id= NONULL(defs.front().idNode);
		parseCheck(	id->boundTo && id->boundTo->type == AstNodeType::varDecl,
					"Builtin corrupted");
		return *static_cast<VarDeclNode*>(id->boundTo);
	}

	NumLiteralNode* parseNumLiteral(const std::string& text)
	{
		auto literal= newNode<NumLiteralNode>();
		/// @todo Rest of literals
		NumLiteralType lit_type= NumLiteralType::int_;
		if (text.find('.') != std::string::npos)
			lit_type= NumLiteralType::f64_;

		literal->literalType= lit_type;
		literal->value= text;
		log(literal->value);
		literal->builtinDecl= &getBuiltinDecl(str(literal->literalType));

		return literal;
	}

	IdentifierNode* parseIdentifier(const std::string& text)
	{
		auto type= newNode<IdentifierNode>();
		type->name= text;
		log(type->name);
		return type;
	}

	/// Parses `name : Type = defaultValue` part of the decl
	VarDeclNode* parseVarDecl(bool constant)
	{
		auto var= newNode<VarDeclNode>();
		var->constant= constant;

		auto var_name= token->text;
		match(TokenType::name, "Invalid var name: " + var_name);
		var->identifier= parseIdentifier(var_name);
		var->identifier->boundTo= var;

		IdDef id_def;
		id_def.idNode= var->identifier;
		id_def.enclosing= blockStack.empty() ? nullptr : blockStack.top();
		context.idDefs[var_name].emplace_back(id_def);

		if (token->type != TokenType::assign) { // Explicit type
			match(TokenType::declaration, "Expected :");
			log(":");
			if (token->type != TokenType::assign)
				var->valueType= parseExpr(Bp::typedecl);
		}

		if (token->type == TokenType::endStatement)
			return var;

		if (token->type == TokenType::assign) {
			match(TokenType::assign);
			log("=");
			var->value= parseExpr();

			if (NONULL(var->value)->type == AstNodeType::block) {
				auto& block= static_cast<BlockNode&>(*NONULL(var->value));
				block.boundTo= var->identifier;
			}

			if (!var->valueType) // Deduce implicit type
				var->valueType= deducedType(*var->value);
		}

		assert(var->valueType);
		return var;
	}

	/// `fn (a : int)`
	FuncTypeNode* parseFuncType()
	{
		log("parseFuncType");
		auto&& log_indent= logIndentGuard();

		log("(");
		match(TokenType::openParen, "Missing ( in fn type");

		// Parameters
		auto func_type= newNode<FuncTypeNode>();
		while (token->type != TokenType::closeParen) {
			if (token->type == TokenType::comma)
				advance();

			auto param= parseVarDecl(true);
			param->param= true;
			func_type->params.push_back(param);
			log(",");
		}

		match(TokenType::closeParen);
		log(")");

		// Return type
		if (token->type == TokenType::rightArrow) {
			advance();
			func_type->returnType= parseExpr(Bp::block);
		} else {
			/// @todo Implicit return type
			auto return_type= newNode<IdentifierNode>();
			return_type->name= "void";
			func_type->returnType= return_type;
		}

		return func_type;
	}

	StructTypeNode* parseStructType()
	{
		return newNode<StructTypeNode>();
	}

	AstNode* parseParens()
	{
		auto expr= parseExpr();
		match(TokenType::closeParen, "Missing )");
		return expr;
	}

	enum class BlockFlag : int {
		none= 0,
		external= 2
	};

	/// `{ code(); }`
	BlockNode* parseBlock(BlockFlag flags= BlockFlag::none)
	{
		log("block");
		auto&& log_indent= logIndentGuard();

		auto block= newNode<BlockNode>();
		if (flags == BlockFlag::external)
			block->external= true;

		if (!block->external) {
			if (!blockStack.empty())
				block->enclosing= blockStack.top();
			blockStack.push(block);
		}

		while (token->type != TokenType::closeBlock)
			block->nodes.emplace_back(parseExpr());
		match(TokenType::closeBlock);

		if (!block->external)
			blockStack.pop();

		return block;
	}

	/// `return foo;`
	CtrlStatementNode* parseCtrlStatement(CtrlStatementType t)
	{
		auto ret= newNode<CtrlStatementNode>();
		ret->statementType= t;
		if (token->type != TokenType::endStatement) {
			ret->value= parseExpr();
		}
		return ret;
	}

	CommentNode* parseComment(std::string text)
	{
		auto comment= newNode<CommentNode>();
		comment->text= std::move(text);
		return comment;
	}

	UOpNode* parseUOp(TokenType t)
	{
		log(std::string("uOp ") + str(t));
		auto op_rbp= tokenRbp(t);
		UOpType op_type;
		switch (t) {
			case TokenType::squote:
				op_type= UOpType::addrOf;
			break;
			case TokenType::mul:
				op_type= UOpType::deref;
			break;
			case TokenType::hat:
				op_type= UOpType::reference;
			break;
			case TokenType::question:
				op_type= UOpType::pointer;
			break;
			case TokenType::kwSizeof:
				op_type= UOpType::sizeOf;
			break;
			default: log(enumStr(t)); assert(0 && "Unknown UOp");
		}

		auto op= newNode<UOpNode>();
		op->opType= op_type;
		op->target= parseExpr(op_rbp);
		return op;
	}

	/// `foo(1, "asd")`
	CallNode* parseCall(AstNode& func, TokenType closing= TokenType::closeParen)
	{
		log("call");
		auto call= newNode<CallNode>();
		call->func= &func;
		while (token->type != closing) {
			if (token->type == TokenType::dot) { 
				// Named argument
				advance();
				call->namedArgs.emplace_back(token->text);
				match(	TokenType::name,
						"Named argument not an identifier");
				match(	TokenType::assign,
						"Missing `=` when specifying named argument: "
						+ call->namedArgs.back());
			} else {
				// Ordinary argument - empty string for name
				call->namedArgs.emplace_back();	
			}
			
			call->args.push_back(parseExpr(Bp::comma));

			if (token->type == TokenType::comma)
				advance();
		}
		match(closing, "Missing )");
		assert(call->args.size() == call->namedArgs.size());
		return call;
	}

	BlockNode* parseLoop()
	{
		match(TokenType::openBlock, "Missing { after loop");
		auto block= parseBlock();
		block->loop= true;
		return block;
	}

	BlockNode* parseIf()
	{
		match(TokenType::openParen, "Missing ( after if");
		auto expr= parseExpr();
		match(TokenType::closeParen, "Missing ) after if");
		match(TokenType::openBlock, "Missing { after if");

		auto block= parseBlock();
		block->condition= expr;
		return block;
	}

	BlockNode* parseExternal()
	{
		match(TokenType::openBlock, "Missing { after extern");
		auto block= parseBlock(BlockFlag::external);
		return block;
	}

	StringLiteralNode* parseStringLiteral(std::string text)
	{
		auto literal= newNode<StringLiteralNode>();
		literal->str= std::move(text);
		return literal;
	}

	TplTypeNode* parseTemplateType()
	{
		log("parseTemplateType");
		match(TokenType::openSquare);
		auto tpl_type= newNode<TplTypeNode>();
		while (token->type != TokenType::closeSquare) {
			if (token->type == TokenType::comma)
				advance();

			auto param= parseVarDecl(true);
			param->param= true;
			tpl_type->params.push_back(param);

			log(",");
		}
		match(TokenType::closeSquare);
		return tpl_type;
	}
	
	/// `left[..]`
	AstNode* parseSquare(AstNode& left)
	{
		auto call= parseCall(left, TokenType::closeSquare);
		call->squareCall= true;
		return call;
	}

	AstNode* parseSizeOf()
	{
		match(TokenType::openParen);
		auto op= parseUOp(TokenType::kwSizeof);
		match(TokenType::closeParen);
		return op;
	}

	AstNode* parseDestructor(AstNode& left)
	{
		parseCheck(	left.type == AstNodeType::block,
					"Only blocks can have destructors");
		BlockNode* left_block= static_cast<BlockNode*>(&left);
		match(TokenType::openBlock);
		while (left_block->destructor) // Allow chaining destructors
			left_block= left_block->destructor;
		left_block->destructor= NONULL(parseBlock());
		return &left;
	}

	AstNode* parseBoolLiteral(bool value)
	{
		auto literal= newNode<NumLiteralNode>();
		literal->literalType= NumLiteralType::bool_;
		literal->value= value ? "true" : "false";
		literal->builtinDecl= &getBuiltinDecl(str(NumLiteralType::bool_));
		return literal;
	}

	AstNode* nud(It it)
	{
		switch (it->type) {
			case TokenType::name:
				return parseIdentifier(it->text);
			case TokenType::number:
				return parseNumLiteral(it->text);
			case TokenType::string:
				return parseStringLiteral(it->text);
			case TokenType::endStatement:
				return newNode<EndStatementNode>();
			case TokenType::openParen:
				return parseParens();
			case TokenType::openBlock:
				return parseBlock();
			case TokenType::squote:
			case TokenType::mul:
			case TokenType::hat:
			case TokenType::question:
				return parseUOp(it->type);
			case TokenType::comment:
				return parseComment(it->text);
			case TokenType::kwVar:
				return parseVarDecl(false);
			case TokenType::kwLet:
				return parseVarDecl(true);
			case TokenType::kwFn:
				return parseFuncType();
			case TokenType::kwStruct:
				return parseStructType();
			case TokenType::kwReturn:
				return parseCtrlStatement(CtrlStatementType::return_);
			case TokenType::kwGoto:
				return parseCtrlStatement(CtrlStatementType::goto_);
			case TokenType::kwBreak:
				return parseCtrlStatement(CtrlStatementType::break_);
			case TokenType::kwContinue:
				return parseCtrlStatement(CtrlStatementType::continue_);
			case TokenType::kwElse:
				return parseCtrlStatement(CtrlStatementType::else_);
			case TokenType::kwNull:
				return newNode<NullLiteralNode>();
			case TokenType::kwLoop:
				return parseLoop();
			case TokenType::kwIf:
				return parseIf();
			case TokenType::kwExtern:
				return parseExternal();
			case TokenType::kwTpl:
				return parseTemplateType();
			case TokenType::kwSizeof:
				return parseSizeOf();
			case TokenType::kwTrue:
				return parseBoolLiteral(true);
			case TokenType::kwFalse:
				return parseBoolLiteral(false);
			default:;
		}
		parseCheck(false, "Invalid nud token: " + it->text);
	}

	AstNode* led(AstNode& left, It it)
	{
		// Not sure if good solution
		if (	left.type == AstNodeType::funcType &&
				it->type != TokenType::endStatement) {
			auto fn_type= static_cast<FuncTypeNode*>(&left);
			auto block= parseBlock();
			block->funcType= fn_type;
			return block;
		}
		if (	left.type == AstNodeType::tplType &&
				it->type != TokenType::endStatement) {
			parseCheck(	it->type == TokenType::rightArrow,
						"TPL must be followed by ->");
			auto tpl_type= static_cast<TplTypeNode*>(&left);
			auto yield= parseExpr();
			parseCheck(	yield->type == AstNodeType::block,
						"TPL must yield a block");
			auto block= static_cast<BlockNode*>(yield);
			block->tplType= tpl_type;
			return block;
		}
		if (	left.type == AstNodeType::structType &&
				it->type != TokenType::endStatement) {
			auto st_type= static_cast<StructTypeNode*>(&left);
			auto block= parseBlock();
			block->structType= st_type;
			for (AstNode* node : block->nodes) {
				if (node->type != AstNodeType::varDecl)
					continue;
				auto decl= static_cast<VarDeclNode*>(node);
				st_type->varDecls.emplace_back(decl);
			}
			return block;
		}
		// `this_is_label:`
		if (	left.type == AstNodeType::identifier &&
				it->type == TokenType::declaration) {
			auto label= newNode<LabelNode>();
			label->identifier= static_cast<IdentifierNode*>(&left);
			label->identifier->boundTo= label;
			return label;
		}

		switch (it->type) {
			case TokenType::endStatement:
				return &left;
			case TokenType::openParen:
				return parseCall(left);
			case TokenType::openSquare:
				return parseSquare(left);
			case TokenType::tilde:
				return parseDestructor(left);
			default: // Assuming BiOp
				{
					log("biOp " + it->text);
					auto op_type= it->type;
					auto op_lbp= tokenLbp(it->type);
					auto op= newNode<BiOpNode>();
					op->opType= op_type;
					op->lhs= &left;
					op->rhs= parseExpr(op_lbp);
					return op;
				}
		}

		parseCheck(false, "Invalid led token: " + it->text);
	}
	
	/// Pratt parser
	AstNode* parseExpr(Bp rbp= Bp::statement)
	{
		assert(token->type != TokenType::eof);
		auto guard= logIndentGuard();

		It t= token;
		advance();
		assert(t->type != TokenType::eof);
		AstNode* left= nud(t);
		while (rbp < tokenLbp(token->type)) {
			t= token;
			advance();
			left= led(*NONULL(left), t);
		}
		return left;
	}
};

} // anonymous

AstContext::AstContext()
{ }

const AstNode& traceValue(const AstNode& node)
{
	if (node.type == AstNodeType::identifier) {
		auto& id= static_cast<const IdentifierNode&>(node);
		if (id.boundTo)
			return traceValue(*id.boundTo);
		else
			return id;
	} else if (node.type == AstNodeType::varDecl) {
		auto& var_decl= static_cast<const VarDeclNode&>(node);
		if (	var_decl.constant &&
				var_decl.value)
			return traceValue(*var_decl.value);
		else
			return *NONULL(var_decl.identifier); // extern decl
	} else if (node.type == AstNodeType::uOp) {
		/// @todo Could maybe trace further in cases like `*&id`
		return node;
	} else if (node.type == AstNodeType::structType) {
		return node;
	} else if (node.type == AstNodeType::funcType) {
		return node;
	} else if (node.type == AstNodeType::builtinType) {
		return node;
	} else if (node.type == AstNodeType::block) {
		return node;
	} else if (node.type == AstNodeType::biOp) {
		auto& op= static_cast<const BiOpNode&>(node);
		if (	op.opType == BiOpType::dot ||
				op.opType == BiOpType::rightArrow) {
			return traceValue(*NONULL(op.rhs));
		}
	} else if (node.type == AstNodeType::call) {
		auto& call= static_cast<const CallNode&>(node);
		auto& traced_func= traceType(*NONULL(call.func));
		if (traced_func.type == AstNodeType::funcType) {
			auto& f_type= static_cast<const FuncTypeNode&>(traced_func);
			return *NONULL(f_type.returnType);
		}
		/// @todo Tpl calls

		parseCheck(false, "Unable to trace value (call)");
	} else if (node.type == AstNodeType::numLiteral) {
		auto& num= static_cast<const NumLiteralNode&>(node);
		return num;
	} else if (node.type == AstNodeType::tplType) {
		return node;
	}

	parseCheck(false, "Unable to trace value");
}

const AstNode& traceType(const AstNode& node)
{
	if (node.type == AstNodeType::identifier) {
		auto& id= static_cast<const IdentifierNode&>(node);
		if (id.boundTo)
			return traceType(*id.boundTo);
	} else if (node.type == AstNodeType::varDecl) {
		auto& var_decl= static_cast<const VarDeclNode&>(node);
		if (var_decl.valueType)
			return traceValue(*var_decl.valueType);
	} else if (node.type == AstNodeType::call) {
		auto& call= static_cast<const CallNode&>(node);
		assert(call.func);
		auto& val= traceValue(*call.func);
		if (call.squareCall) {
			assert(val.type == AstNodeType::block);
			return val;
		} else if (val.type == AstNodeType::block) {
			auto& block= static_cast<const BlockNode&>(val);
			assert(!block.tplType);
			if (block.structType) {
				return block; // ctor call
			} else if (block.funcType) {
				return traceValue(*NONULL(block.funcType->returnType));
			}
		} else {
			parseCheck(false, "Unable to trace type (call)");
		}
	} else if (node.type == AstNodeType::block) {
		auto& block= static_cast<const BlockNode&>(node);
		if (block.tplType)
			return *block.tplType;
		if (block.structType)
			return *block.structType;
		if (block.funcType)
			return *block.funcType;
	} else if (node.type == AstNodeType::uOp) {
		/// @todo Could maybe trace further in cases like `*&id`
		return node;
	} else if (node.type == AstNodeType::biOp) {
		auto& op= static_cast<const BiOpNode&>(node);
		if (	op.opType == BiOpType::dot ||
				op.opType == BiOpType::rightArrow) {
			return traceType(*NONULL(op.rhs));
		}
	} else if (node.type == AstNodeType::numLiteral) {
		auto& num= static_cast<const NumLiteralNode&>(node);
		// Not sure what to return here
		return *NONULL(num.builtinDecl)->identifier;
	}

	parseCheck(false, "Unable to trace type");
}

const IdentifierNode& traceBoundId(const AstNode& node, BoundIdDist dist)
{
	if (node.type == AstNodeType::identifier) {
		auto& id= static_cast<const IdentifierNode&>(node);
		if (!id.boundTo)
			return id;
		return traceBoundId(*id.boundTo, dist);
	} else if (node.type == AstNodeType::block) {
		auto& block= static_cast<const BlockNode&>(node);
		if (block.boundTo)
			return traceBoundId(*block.boundTo, dist);
	} else if (node.type == AstNodeType::varDecl) {
		auto& decl= static_cast<const VarDeclNode&>(node);
		if (	dist == BoundIdDist::furthest &&
				decl.constant &&
				decl.value &&
				decl.value->type == AstNodeType::identifier) {
			return traceBoundId(*decl.value, dist);
		}
		return *NONULL(decl.identifier);
	} else if (node.type == AstNodeType::label) {
		auto& label= static_cast<const LabelNode&>(node);
		return *NONULL(label.identifier);
	}
	parseCheck(false, "Unable to trace bound id");
}

std::string mangledName(AstNode& node)
{
	if (node.type == AstNodeType::identifier){
		auto& id= static_cast<IdentifierNode&>(node);
		if (id.boundTo) // Aliases will become the original name
			return traceBoundId(id, BoundIdDist::furthest).name;
		else
			return id.name;
	} else if (node.type == AstNodeType::varDecl) {
		return "@todo VarDecl";
	} else if (node.type == AstNodeType::uOp){
		auto& op= static_cast<UOpNode&>(node);
		std::string prefix;
		switch (op.opType) {
			case UOpType::addrOf: prefix= "addrof"; break;
			case UOpType::deref: prefix= "deref"; break;
			case UOpType::pointer: prefix= "ptr"; break;
			case UOpType::reference: prefix= "ref"; break;
			default: assert(0 && "Unknown op in mangling");
		}
		return prefix + "_" + mangledName(*NONULL(op.target));
	} else if (node.type == AstNodeType::block) {
		auto& block= static_cast<BlockNode&>(node);
		return mangledName(*NONULL(block.boundTo));
	}

	parseCheck(false, "Unable to mangle");
}

void routeCallArgs(	std::vector<AstNode*>& implicit,
					std::vector<int>& routing,
					const AstNode& func,
					const std::vector<std::string>& arg_names)

{
	assert(implicit.empty());
	assert(routing.empty());
	std::string call_name= "@todo str(complex call)";
	if (func.type == AstNodeType::identifier) {
		call_name= static_cast<const IdentifierNode&>(func).name;
	}

	auto routeArgsToParams=
		[&call_name] (	std::vector<AstNode*>& implicit_args, // output
						const std::vector<std::string>& names,
						const std::vector<VarDeclNode*>& params)
		-> std::vector<int>
	{
		parseCheck(names.size() <= params.size(),
				"Too many arguments in a call: " + call_name);
		std::vector<int> routing; // routing[arg_i] == param_i
		routing.resize(names.size(), -1);
		std::vector<bool> routed_params;
		routed_params.resize(params.size());

		// Route named args
		for (std::size_t i= 0; i < names.size(); ++i) {
			if (names[i].empty())
				continue;

			bool found= false;
			for (std::size_t param_i= 0; param_i < params.size(); ++param_i) {
				if (	NONULL(NONULL(params[param_i])->identifier)->name
						== names[i]) {
					routing[i]= param_i;
					routed_params[param_i]= true;
					found= true;
					break;
				}
			}
			parseCheck(found, "Named param not found: " + names[i]);
		}

		// Route ordinary args
		int next_param_i= 0;
		for (std::size_t i= 0; i < names.size(); ++i) {
			if (!names[i].empty())
				continue;
			
			while (routed_params[next_param_i] == true)
				++next_param_i;

			assert(next_param_i < names.size());
			assert(routing[i] == -1);
			routing[i]= next_param_i;
			routed_params[next_param_i]= true;
			++next_param_i;
		}

		// Create and route implicit args
		for (std::size_t i= 0; i < params.size(); ++i) {
			assert(i < routed_params.size());
			if (routed_params[i])
				continue;

			auto&& p= *NONULL(params[i]);
			auto impl_value= p.value;
			parseCheck(	impl_value != nullptr,
						"Missing argument: " + NONULL(p.identifier)->name);

			// Using same node as func decl
			implicit_args.emplace_back(impl_value);

			routing.emplace_back(i);
			routed_params[i]= true;
		}
		
		for (auto&& r : routing) {
			assert(r != -1);
		}

		return routing;
	};

	// Route call arguments to function/struct parameters

	const AstNode* traced= nullptr;
	if (	func.type == AstNodeType::funcType ||
			func.type == AstNodeType::structType ||
			func.type == AstNodeType::tplType)
		traced= &func;
	else
		traced= &traceType(func);
	if (traced->type == AstNodeType::funcType) {
		// Ordinary function call
		auto& func_type= static_cast<const FuncTypeNode&>(*traced);
		routing=
			routeArgsToParams(	implicit,
								arg_names,
								listToVec(func_type.params));
	} else if (traced->type == AstNodeType::structType) {
		// Constructor call
		auto& struct_type= static_cast<const StructTypeNode&>(*traced);
		routing=
			routeArgsToParams(	implicit,
								arg_names,
								struct_type.varDecls);
	} else if (traced->type == AstNodeType::tplType) {
		// Template instantiation
		auto& tpl_type= static_cast<const TplTypeNode&>(*traced);
		routing=
			routeArgsToParams(	implicit,
								arg_names,
								tpl_type.params);
	} else {
		parseCheck(false, "Illegal call");
	}
}

std::vector<AstNode*> resolveRouting(	const std::vector<AstNode*>& args,
										const std::vector<int>& routing)
{
	std::vector<AstNode*> new_args;
	new_args.resize(args.size());
	std::size_t i= 0;
	auto setNextArg= [&] (AstNode* arg)
	{
		int param_i= routing[i];
		assert(param_i >= 0 && param_i < new_args.size());
		new_args[param_i]= arg;
		++i;
	};
	for (auto&& arg : args)
		setNextArg(arg);
	return new_args;
}
	
AstContext genAst(const Tokens& tokens)
{
	Parser parser{tokens};
	AstContext&& ctx= parser.parse();
	return std::move(ctx);
}

} // gamelang
