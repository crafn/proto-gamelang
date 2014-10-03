// Debug
#include <iostream>

#include <map>

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

void parseCheck(bool expr, const std::string& msg)
{ if (!expr) log(msg); assert(expr); }

/// Transforms tokens to an abstract syntax tree
struct Parser {
	Parser(const Tokens& t): tokens(t) {}

	AstContext parse()
	{
		// Start parsing
		auto root= newNode<GlobalNode>();
		auto tok= tokens.begin();
		while (tok != tokens.end()) {
			root->nodes.emplace_back(parseExpr(tok));
		}
		return std::move(context);
	}

private:
	const Tokens& tokens;
	AstContext context;

	using It= Tokens::const_iterator;
	template <typename T>
	using UPtr= std::unique_ptr<T>;
	using Str= std::string;

	template <typename T>
	T* newNode()
	{ return context.newNode<T>(); }

	struct LogIndentGuard {
		int& value;
		LogIndentGuard(int& v): value(v) { ++value; }
		~LogIndentGuard() { --value; }
	};

	LogIndentGuard logIndentGuard()
	{ return LogIndentGuard{logIndent}; }

	void nextToken(It& it)
	{
		assert(it != tokens.end());
		++it;
		parseCheck(it != tokens.end(), "Unexpected end of file");
	}

	void advance(It& it) { ++it; }

	AstNode* deducedType(const AstNode& thing)
	{
		if (thing.type == AstNodeType::block) {
			const BlockNode& block= static_cast<const BlockNode&>(thing);
			if (block.structure)
				return newNode<StructTypeNode>();
			if (block.funcType)
				return block.funcType;
		}

		assert(0 && "@todo deduction");
	}

	VarDeclNode* parseVarDecl(It& tok)
	{
		log("parseVarDecl");
		auto&& log_indent= logIndentGuard();

		auto var= newNode<VarDeclNode>();
		parseCheck(tok->type == TokenType::identifier, "Expected identifier");
		if (tok->text == "var") {
			var->constant= false;
		} else if (tok->text == "let") {
			var->constant= true;
		} else {
			parseCheck(false, "Expected var/let");
		}
		nextToken(tok);

		parseCheck(tok->type == TokenType::identifier, "Error in var decl name");
		var->identifier= parseIdentifier(tok);
		var->identifier->boundTo= var;

		parseCheck(tok->type == TokenType::declaration, "Missing : in var decl");
		nextToken(tok);

		if (tok->type == TokenType::identifier) { // Explicit type
			log(":");
			var->valueType= parseExpr(tok, false);
		}

		if (var->valueType && var->valueType->endStatement) 
			return var;

		if (tok->type == TokenType::assign) {
			log("=");
			nextToken(tok);
			var->value= parseExpr(tok);

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

	FuncTypeNode* parseFuncType(It& tok)
	{
		log("parseFuncType");
		auto&& log_indent= logIndentGuard();

		assert(tok->text == "fn");
		nextToken(tok); // Skip "fn"

		log(tok->text);
		parseCheck(tok->type == TokenType::openParen, "Missing ( in fn type");
		nextToken(tok);

		// Arguments
		auto func_type= newNode<FuncTypeNode>();
		while (tok->type != TokenType::closeParen) {
			if (tok->type == TokenType::comma)
				nextToken(tok);

			auto param= newNode<VarDeclNode>();
			param->param= true;

			parseCheck(tok->type == TokenType::identifier,
					"Missing identifier for func arg");
			param->identifier= parseIdentifier(tok);
			param->identifier->boundTo= param;

			parseCheck(tok->type == TokenType::declaration,
					"Missing : for func arg");
			nextToken(tok);

			param->valueType= parseExpr(tok, false);
			assert(param->valueType);
			func_type->params.push_back(param);
		}
		nextToken(tok);

		// Return type
		if (!func_type->endStatement && tok->type == TokenType::rightArrow) {
			nextToken(tok);
			func_type->returnType= parseExpr(tok);
		} else {
			/// @todo Implicit return type
			auto return_type= newNode<IdentifierNode>();
			return_type->name= "void";
			func_type->returnType= std::move(return_type);
		}

		return func_type;
	}

	CallNode* parseCall(It& tok, IdentifierNode& identifier)
	{
		parseCheck(tok->type == TokenType::openParen, "Missing (");
		nextToken(tok);

		auto call= newNode<CallNode>();
		call->func= &identifier;
		while (tok->type != TokenType::closeParen) {
			call->args.push_back(parseExpr(tok));

			if (tok->type == TokenType::comma)
				nextToken(tok);
		}
		nextToken(tok);

		return call;
	}

	BlockNode* parseBlock(It& tok)
	{
		log("parseBlock");
		auto&& log_indent= logIndentGuard();

		parseCheck(tok->type == TokenType::openBlock, "Missing {");
		nextToken(tok); // Skip "{"

		auto block= newNode<BlockNode>();
		while (tok->type != TokenType::closeBlock) {
			block->nodes.emplace_back(parseExpr(tok));
		}

		advance(tok);
		return block;
	}

	NumLiteralNode* parseNumLiteral(It& tok)
	{
		auto literal= newNode<NumLiteralNode>();
		literal->value= tok->text;
		log(literal->value);
		nextToken(tok);
		return literal;
	}

	IdentifierNode* parseIdentifier(It& tok)
	{
		auto type= newNode<IdentifierNode>();
		type->name= tok->text;
		if (type->name == "int" || type->name == "void")
			type->boundTo= &context.getBuiltinTypeDecl();
		log(type->name);
		nextToken(tok);
		return type;
	}

	CtrlStatementNode* parseCtrlStatement(	It& tok,
											std::string text,
											CtrlStatementType t)
	{
		assert(tok->text == text);
		nextToken(tok);

		auto ret= newNode<CtrlStatementNode>();
		ret->statementType= t;
		if (tok->type != TokenType::endStatement) {
			ret->value= parseExpr(tok);
		}
		return ret;
	}
	
	CtrlStatementNode* parseReturn(It& tok)
	{
		return parseCtrlStatement(tok, "return", CtrlStatementType::return_);
	}

	CtrlStatementNode* parseGoto(It& tok)
	{
		return parseCtrlStatement(tok, "goto", CtrlStatementType::goto_);
	}

	AstNode* parseIfExpr(It& tok)
	{
		assert(tok->text == "if");
		nextToken(tok);

		parseCheck(tok->type == TokenType::openParen, "Missing ( after if");
		nextToken(tok);

		auto expr= parseExpr(tok);

		parseCheck(tok->type == TokenType::closeParen, "Missing ) after if");
		nextToken(tok);

		return expr;
	}

	/// greedy: parse as much as possible
	///   e.g. `(stuff + 2) = 5;` -> whole statement is parsed
	/// non-greedy: parse first full sub-statement
	///   e.g. `(stuff + 2) = 5;` -> only `(stuff + 2)` is parsed
	AstNode* parseExpr(It& tok, bool greedy= true)
	{
		//log("parseExpr " + tok->text);
		if (tok->type == TokenType::identifier) {
			if (tok->text == "let" || tok->text == "var") {
				return parseVarDecl(tok);
			} else if (tok->text == "fn") {
				auto func_type_node= parseFuncType(tok);
				/// @todo Other block properties
				if (tok->type != TokenType::openBlock) {
					return std::move(func_type_node);
				} else {
					auto block= parseBlock(tok);
					block->funcType= std::move(func_type_node);
					return std::move(block);
				}
			} else if (tok->text == "if") {
				auto expr= parseIfExpr(tok);
				/// @todo Implicit block
				auto block= parseBlock(tok);
				block->condition= expr;
				return block;
			} else if (tok->text == "struct") {
				nextToken(tok);
				auto block= parseBlock(tok);
				block->structure= true;
				return block;
			} else if (tok->text == "return") {
				return parseReturn(tok);
			} else if (tok->text == "goto") {
				return parseGoto(tok);
			} else {
				return parseRestExpr(parseIdentifier(tok), tok, greedy);
			}
		} else if (tok->type == TokenType::openBlock) {
			return parseBlock(tok);
		} else if (tok->type == TokenType::number) {
			return parseRestExpr(parseNumLiteral(tok), tok, greedy);
		}

		parseCheck(false, "Broken expression at " + tok->text);
	}

	AstNode* parseRestExpr(AstNode* beginning, It& tok, bool greedy)
	{
		assert(beginning);

		//log("parseRestExpr " + tok->text + " " + str(tok->type));
		if (tok->type == TokenType::endStatement) {
			advance(tok);
			beginning->endStatement= true;
			return beginning;
		}

		/// @todo Don't return if parsing in brackets
		if (tok->type == TokenType::comma) {
			return beginning;
		}

		/// @todo Don't return if parsing in brackets
		if (tok->type == TokenType::closeParen) {
			return beginning;
		}

		// `this_is_label:`
		if (	beginning->type == AstNodeType::identifier &&
				tok->type == TokenType::declaration) {
			nextToken(tok);
			auto label= newNode<LabelNode>();
			label->identifier= static_cast<IdentifierNode*>(beginning);
			label->identifier->boundTo= label;
			return label;
		}

		if (	beginning->type == AstNodeType::identifier &&
				tok->type == TokenType::openParen) {
			auto call= parseCall(tok, static_cast<IdentifierNode&>(*NONULL(beginning)));
			return parseRestExpr(call, tok, greedy);
		}

		if (tok->type == TokenType::dot) {
			auto op_type= tok->type;
			nextToken(tok);
			auto op= newNode<BiOpNode>();
			op->opType= BiOpType::dot;
			op->lhs= beginning;
			op->rhs= parseExpr(tok);
			return op;
		}

		if (greedy) {
			switch (tok->type) {
				case TokenType::assign:
				case TokenType::add:
				case TokenType::sub:
				case TokenType::equals:
				{
					auto op_type= tok->type;
					nextToken(tok);
					auto op= newNode<BiOpNode>();
					op->opType= op_type;
					op->lhs= beginning;
					op->rhs= parseExpr(tok);
					return op;
				}
				default:;
			}
		}
		return beginning;
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
		tie(context.getRootNode());
	}

private:
	AstContext& context;

	/// @todo Take scope into account
	/// Can be var decls or labels
	std::map<std::string, AstNode*> idTargets;

	void tie(AstNode& node)
	{
		CondTie<AstNodeType::global,        GlobalNode>::eval(*this, node);
		CondTie<AstNodeType::identifier,    IdentifierNode>::eval(*this, node);
		CondTie<AstNodeType::block,         BlockNode>::eval(*this, node);
		CondTie<AstNodeType::varDecl,       VarDeclNode>::eval(*this, node);
		CondTie<AstNodeType::funcType,      FuncTypeNode>::eval(*this, node);
		CondTie<AstNodeType::biOp,          BiOpNode>::eval(*this, node);
		CondTie<AstNodeType::ctrlStatement, CtrlStatementNode>::eval(*this, node);
		CondTie<AstNodeType::call,          CallNode>::eval(*this, node);
		CondTie<AstNodeType::label,         LabelNode>::eval(*this, node);
	}

	void tie(GlobalNode& global)
	{
		for (auto&& node : global.nodes)
			tie(*NONULL(node));
	}

	void tie(IdentifierNode& identifier)
	{
		if (identifier.boundTo)
			return;
		
		auto it= idTargets.find(identifier.name);
		parseCheck(it != idTargets.end(), "Unresolved identifier: " + identifier.name);
		
		AstNode* var= it->second;
		identifier.boundTo= var;
	}

	void tie(BlockNode& block)
	{
		for (auto&& node : block.nodes)
			tie(*NONULL(node));
	}

	void tie(VarDeclNode& var)
	{
		assert(NONULL(var.identifier)->boundTo &&
				"Variable identifiers should be bound by definition");

		// Loose identifiers can be bound to `var`
		idTargets[NONULL(var.identifier)->name]= &var;

		tie(*NONULL(var.valueType));
		if (var.value)
			tie(*var.value);
	}

	void tie(FuncTypeNode& func)
	{
		tie(*NONULL(func.returnType));
		for (auto&& node : func.params)
			tie(*NONULL(node));
	}

	void tie(BiOpNode& op)
	{
		tie(*NONULL(op.lhs));
		tie(*NONULL(op.rhs));
	}

	void tie(CtrlStatementNode& ret)
	{
		tie(*NONULL(ret.value));
	}

	void tie(CallNode& call)
	{
		tie(*NONULL(call.func));
		for (auto&& arg : call.args)
			tie(*NONULL(arg));
	}

	void tie(LabelNode& label)
	{
		idTargets[NONULL(label.identifier)->name]= &label;
		tie(*NONULL(label.identifier));
	}

	template <AstNodeType nodeType, typename T>
	struct CondTie {
		static void eval(TieIdentifiers& self, AstNode& node)
		{ if (node.type == nodeType) self.tie(static_cast<T&>(node)); }
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

bool containsEndStatement(const AstNode& node)
{
	if (node.endStatement)
		return true;

	for (auto&& sub : node.getSubNodes()) {
		if (!sub)
			continue;
		if (containsEndStatement(*sub))
			return true;
	}

	return false;
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
