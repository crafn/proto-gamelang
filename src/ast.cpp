#include "ast.hpp"
#include "nullsafety.hpp"

namespace gamelang
{

namespace
{

struct Parser {
	const Tokens& tokens;
	int logIndent;
	AstContext context;

	using It= Tokens::const_iterator;
	template <typename T>
	using UPtr= std::unique_ptr<T>;
	using Str= std::string;

	template <typename T>
	T* newNode()
	{
		context.nodes.emplace_back(new T{});
		return static_cast<T*>(context.nodes.back().get());
	}

	void parseCheck(bool expr, const std::string& msg)
	{ if (!expr) log(msg); assert(expr); }

	void log(const Str& str)
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
			if (block.functionType) {
				return block.functionType;
			}
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
		var->name= tok->text;
		log(var->name);
		nextToken(tok);

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

			if (!var->valueType) // Deduce implicit type
				var->valueType= deducedType(*var->value);
		}

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

			auto param= newNode<ParamDeclNode>();

			parseCheck(tok->type == TokenType::identifier,
					"Missing identifier for func arg");
			param->name= tok->text;
			nextToken(tok);

			parseCheck(tok->type == TokenType::declaration,
					"Missing : for func arg");
			nextToken(tok);

			param->valueType= parseExpr(tok, false);
			func_type->params.push_back(param);
		}
		nextToken(tok);

		// Return type
		if (!func_type->endStatement && tok->type == TokenType::yields) {
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
		call->function= &identifier;
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
		log(type->name);
		nextToken(tok);
		return type;
	}

	ReturnNode* parseReturn(It& tok)
	{
		nextToken(tok);

		auto ret= newNode<ReturnNode>();
		if (tok->type != TokenType::endStatement) {
			ret->value= parseExpr(tok);
		}
		return ret;
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
					block->functionType= std::move(func_type_node);
					return std::move(block);
				}
			} else if (tok->text == "return") {
				return parseReturn(tok);
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

		/// @todo Don't return if inside ()
		if (tok->type == TokenType::comma) {
			return beginning;
		}

		if (	beginning->type == AstNodeType::identifier &&
				tok->type == TokenType::openParen)
		{
			auto call= parseCall(tok, static_cast<IdentifierNode&>(*NONULL(beginning)));
			return parseRestExpr(call, tok, greedy);
		}

		/// @todo Remove duplicate code
		if (greedy && tok->type == TokenType::assign) {
			nextToken(tok);
			auto op= newNode<BiOpNode>();
			op->opType= BiOpType::assign;
			op->lhs= beginning;
			op->rhs= parseExpr(tok);
			return op;
		}
		if (greedy && tok->type == TokenType::add) {
			nextToken(tok);
			auto op= newNode<BiOpNode>();
			op->opType= BiOpType::add;
			op->lhs= beginning;
			op->rhs= parseExpr(tok);
			return op;
		}
		if (greedy && tok->type == TokenType::sub) {
			nextToken(tok);
			auto op= newNode<BiOpNode>();
			op->opType= BiOpType::sub;
			op->lhs= beginning;
			op->rhs= parseExpr(tok);
			return op;
		}
		return beginning;
	}

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

};



} // anonymous

bool containsEndStatement(const AstNode& node)
{
	if (node.endStatement)
		return true;

	for (auto&& sub : node.getSubNodes()) {
		assert(sub);
		if (containsEndStatement(*sub))
			return true;
	}

	return false;
}

AstContext genAst(const Tokens& tokens)
{
	Parser parser{tokens};
	return parser.parse();
}

} // gamelang
