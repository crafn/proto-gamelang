// Debug
#include <iostream>

#include <map>
#include <type_traits>

#include "ast.hpp"
#include "nullsafety.hpp"

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

void parseCheck(bool expr, const std::string& msg)
{
	if (!expr)
		log(msg);
	/// @todo Proper error messaging
	assert(expr);
}

bool isBuiltinIdentifier(const std::string& name)
{
	return	name == "int" ||
			name == "uint" ||
			name == "int32" ||
			name == "int64" ||
			name == "void" ||
			name == "bool" ||
			name == "true" ||
			name == "false" ||
			name == "char";
}

/// Binding power
enum class Bp : int {
	eof= 0,
	comment,
	endBraces,
	keyword,
	literal,
	name,
	comma,
	statement,
	assignment,
	comp,
	typedecl,
	sum,
	prod,
	block,
	member,
	parens,
	prefix
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
		case TokenType::closeParen:   return Bp::endBraces;
		case TokenType::openBlock:    return Bp::block;
		case TokenType::closeBlock:   return Bp::endBraces;
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
		case TokenType::ref:          return Bp::prefix;
		case TokenType::hat:          return Bp::prefix;
		case TokenType::question:     return Bp::prefix;
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
		default: log(enumStr(t)); assert(0 && "Missing token binding power");
	}
}

/// Transforms tokens to an abstract syntax tree
struct Parser {
	Parser(const Tokens& t): tokens(t) {}

	AstContext parse()
	{
		// Start parsing
		auto root= newNode<GlobalNode>();
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

	NumLiteralNode* parseNumLiteral(const std::string& text)
	{
		auto literal= newNode<NumLiteralNode>();
		literal->value= text;
		log(literal->value);
		return literal;
	}

	IdentifierNode* parseIdentifier(const std::string& text)
	{
		auto type= newNode<IdentifierNode>();
		type->name= text;
		if (isBuiltinIdentifier(type->name))
			type->boundTo= &context.getBuiltinTypeDecl();
		log(type->name);
		return type;
	}

	AstNode* parseParens()
	{
		auto expr= parseExpr();
		match(TokenType::closeParen, "Missing )");
		return expr;
	}
	
	/// Parses `name : Type = defaultValue` part of the decl
	VarDeclNode* parseVarDecl(bool constant)
	{
		auto var= newNode<VarDeclNode>();
		var->constant= constant;

		auto var_name= token->text;
		match(TokenType::name, "Var name must be in form abc123");
		var->identifier= parseIdentifier(var_name);
		var->identifier->boundTo= var;

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
			return_type->boundTo= &context.getBuiltinTypeDecl();
			func_type->returnType= return_type;
		}

		return func_type;
	}

	StructTypeNode* parseStructType()
	{
		return newNode<StructTypeNode>();
	}

	/// `{ code(); }`
	BlockNode* parseBlock()
	{
		log("parseBlock");
		auto&& log_indent= logIndentGuard();

		auto block= newNode<BlockNode>();
		while (token->type != TokenType::closeBlock) {
			block->nodes.emplace_back(parseExpr());
		}
		match(TokenType::closeBlock);

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
		auto op_lbp= tokenLbp(t);
		UOpType op_type;
		switch (t) {
			case TokenType::ref:
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
			default: assert(0 && "Unknown UOp");
		}

		auto op= newNode<UOpNode>();
		op->opType= op_type;
		op->target= parseExpr(op_lbp);
		return op;
	}

	/// `foo(1, "asd")`
	CallNode* parseCall(AstNode& func)
	{
		/// @todo Support for calling arbitrary expr
		parseCheck(	func.type == AstNodeType::identifier,
					"Only simple func calls supported");
		auto identifier= static_cast<IdentifierNode*>(&func);

		auto call= newNode<CallNode>();
		call->func= identifier;
		while (token->type != TokenType::closeParen) {
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
		match(TokenType::closeParen, "Missing )");
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
		auto block= parseBlock();
		block->external= true;
		return block;
	}

	StringLiteralNode* parseStringLiteral(std::string text)
	{
		auto literal= newNode<StringLiteralNode>();
		literal->str= std::move(text);
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
			case TokenType::ref:
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
				it->type == TokenType::declaration)
		{
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
			default: // Assuming BiOp
				{
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

/// Ties unbound identifiers of the ast tree (making a graph)
/// Goal is to have zero unbound identifiers after tying
struct TieIdentifiers {
	TieIdentifiers(AstContext& ctx): context(ctx) {}

	void tie()
	{
		/// @todo Scan ast first for declarations and loose-end identifiers
		///       to allow cyclic references to be resolved
		AstNode* root= &context.getRootNode();
		tie(root);
	}

private:
	AstContext& context;

	/// @todo Take scope into account
	/// Can be var decls or labels
	std::map<std::string, AstNode*> idTargets;

	/// Parent substitutes previously tied node with this
	AstNode* substitution= nullptr;

	template <typename T>
	void tie(T*& node)
	{
		assert(node);
		assert(!substitution);

		tie_choose(*node);

		if (substitution) {
			assert((substitution->type == node->type || 
					std::is_same<T, AstNode>::value));

			node= static_cast<T*>(substitution);
			substitution= nullptr;
		}
	}

	void tie_choose(AstNode& node)
	{
		CondTie<AstNodeType::global,        GlobalNode>::eval(*this, node);
		CondTie<AstNodeType::identifier,    IdentifierNode>::eval(*this, node);
		CondTie<AstNodeType::block,         BlockNode>::eval(*this, node);
		CondTie<AstNodeType::varDecl,       VarDeclNode>::eval(*this, node);
		CondTie<AstNodeType::funcType,      FuncTypeNode>::eval(*this, node);
		CondTie<AstNodeType::uOp,           UOpNode>::eval(*this, node);
		CondTie<AstNodeType::biOp,          BiOpNode>::eval(*this, node);
		CondTie<AstNodeType::ctrlStatement, CtrlStatementNode>::eval(*this, node);
		CondTie<AstNodeType::call,          CallNode>::eval(*this, node);
		CondTie<AstNodeType::label,         LabelNode>::eval(*this, node);
	}

	void tieSpecific(GlobalNode& global)
	{
		for (auto&& node : global.nodes) {
			tie(node);
		}
	}

	void tieSpecific(IdentifierNode& identifier)
	{
		if (identifier.boundTo)
			return;
		
		auto it= idTargets.find(identifier.name);
		parseCheck(it != idTargets.end(), "Unresolved identifier: " + identifier.name);
		
		AstNode* var= it->second;
		identifier.boundTo= var;
	}

	void tieSpecific(BlockNode& block)
	{
		for (auto&& node : block.nodes) {
			tie(node);
		}
	}

	void tieSpecific(VarDeclNode& var)
	{
		assert(NONULL(var.identifier)->boundTo &&
				"Variable identifiers should be bound by definition");

		// Loose identifiers can be bound to `var`
		idTargets[NONULL(var.identifier)->name]= &var;

		tie(var.valueType);
		if (var.value)
			tie(var.value);

		// Resolve decltype
		if (	var.valueType->type == AstNodeType::uOp &&
				static_cast<UOpNode*>(var.valueType)->opType
					== UOpType::declType) {
			/// @todo Resolving types and metaprograms probably need another pass
			auto op= static_cast<UOpNode*>(var.valueType);
			
			parseCheck(	NONULL(op->target)->type == AstNodeType::call,
						"Only deduction from call return type supported");
			auto call= static_cast<CallNode*>(op->target);

			// Identifier `Chicken` in ctor call `Chicken(10, 20)` is bound to
			// the declaration `let Chicken := struct {..}`
			assert(NONULL(call->func->boundTo)->type == AstNodeType::varDecl);
			auto ret_type_decl= static_cast<VarDeclNode*>(call->func->boundTo);

			// Resolve valueType to the identifier of the struct type
			var.valueType= ret_type_decl->identifier;
		}
	}

	void tieSpecific(FuncTypeNode& func)
	{
		tie(func.returnType);
		for (auto&& node : func.params)
			tie(node);
	}

	void tieSpecific(UOpNode& op)
	{
		tie(op.target);
	}

	void tieSpecific(BiOpNode& op)
	{
		if (	op.opType == BiOpType::dot &&
				NONULL(op.rhs)->type == AstNodeType::call) {
			// "Method" call
			auto call= static_cast<CallNode*>(op.rhs);
			auto ref= context.newNode<UOpNode>();
			ref->opType= UOpType::addrOf;
			ref->target= op.lhs;
			AstNode* arg= op.lhs;
			call->args.emplace(call->args.begin(), ref);
			call->namedArgs.emplace(call->namedArgs.begin(), "");
			call->methodLike= true;

			tie(call);

			// Replace op with call
			substitution= call;
		} else {
			tie(op.lhs);
			tie(op.rhs);
		}
	}

	void tieSpecific(CtrlStatementNode& ret)
	{
		if (ret.value)
			tie(ret.value);
	}

	void tieSpecific(CallNode& call)
	{
		auto&& call_name= NONULL(call.func)->name;
		auto routeArgsToParams=
			[&call_name] (	std::vector<AstNode*>& implicit_args, // output
							std::vector<std::string>& names,
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
							"Missing argument: " + p.identifier->name);

				// Using same node as func decl
				implicit_args.emplace_back(impl_value);

				names.emplace_back("");
				routing.emplace_back(i);
				routed_params[i]= true;
			}
			
			for (auto&& r : routing) {
				assert(r != -1);
				assert(r < names.size());
			}

			return routing;
		};

		tie(call.func);
		for (auto&& arg : call.args) {
			tie(arg); 
		}

		// Route call arguments to function/struct parameters
		auto func_id_bound= NONULL(call.func)->boundTo;
		assert(NONULL(func_id_bound)->type == AstNodeType::varDecl);
		auto var_decl= static_cast<VarDeclNode*>(func_id_bound);
		if (NONULL(var_decl)->valueType->type == AstNodeType::funcType) {
			// Ordinary function call
			auto func_type= static_cast<FuncTypeNode*>(var_decl->valueType);
			call.argRouting=
				routeArgsToParams(	call.implicitArgs,
									call.namedArgs,
									listToVec(func_type->params));
		} else if (NONULL(var_decl)->valueType->type == AstNodeType::structType) {
			// Constructor call
			auto struct_type= static_cast<StructTypeNode*>(var_decl->valueType);
			call.argRouting=
				routeArgsToParams(	call.implicitArgs,
									call.namedArgs,
									struct_type->varDecls);
		}

		assert(call.argRouting.size() == call.args.size() + call.implicitArgs.size());
	}

	void tieSpecific(LabelNode& label)
	{
		idTargets[NONULL(label.identifier)->name]= &label;
		tie(label.identifier);
	}

	template <AstNodeType nodeType, typename T>
	struct CondTie {
		static void eval(TieIdentifiers& self, AstNode& node)
		{ if (node.type == nodeType) self.tieSpecific(static_cast<T&>(node)); }
	};
};

} // anonymous

AstContext::AstContext()
{
	builtinType.reset(new BuiltinTypeNode{});
	builtinId.reset(new IdentifierNode{});
	builtinDecl.reset(new VarDeclNode{});

	builtinId->name= "builtin";
	builtinId->boundTo= builtinDecl.get();
	builtinDecl->identifier= builtinId.get();
	builtinDecl->valueType= builtinType.get();
}

AstContext genAst(const Tokens& tokens)
{
	Parser parser{tokens};
	AstContext&& ctx= parser.parse();

	TieIdentifiers t{ctx};
	t.tie();
	return std::move(ctx);
}

} // gamelang
