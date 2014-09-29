#include "ast.hpp"

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

	void parseCheck(bool expr, const char* msg)
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
		nextToken(tok); // Skip "let"

		parseCheck(tok->type == TokenType::identifier, "Error in var decl name");
		var->name= tok->text;
		log(var->name);
		nextToken(tok);

		parseCheck(tok->type == TokenType::declaration, "Missing : in var decl");
		nextToken(tok);

		if (tok->type == TokenType::identifier) { // Explicit type
			log(":");
			var->valueType= parseExpr(tok);
			nextToken(tok);
		}
		
		if (tok->type == TokenType::endStatement) {
			advance(tok);
			return std::move(var);
		}

		if (tok->type == TokenType::assign) {
			log("=");
			nextToken(tok);
			var->value= parseExpr(tok);

			if (!var->valueType) // Deduce implicit type
				var->valueType= deducedType(*var->value);
		} 

		if (tok->type == TokenType::endStatement)
			advance(tok);

		return std::move(var);
	}

	FuncTypeNode* parseFuncType(It& tok)
	{
		log("parseFuncType");
		auto&& log_indent= logIndentGuard();

		nextToken(tok); // Skip "fn"

		parseCheck(tok->type == TokenType::openParen, "Missing ( in fn type");
		nextToken(tok);

		/// @todo Parse arguments
		auto func_type= newNode<FuncTypeNode>();
		
		parseCheck(tok->type == TokenType::closeParen, "Missing ) in fn type");	
		nextToken(tok);

		/// @todo Parse return type
		auto return_type= newNode<IdentifierNode>();
		return_type->name= "void";
		func_type->returnType= std::move(return_type);

		return std::move(func_type);
	}

	BlockNode* parseBlock(It& tok)
	{
		log("parseBlock");
		auto&& log_indent= logIndentGuard();

		nextToken(tok); // Skip "{"

		auto block= newNode<BlockNode>();
		while (tok->type != TokenType::closeBlock) {
			block->nodes.emplace_back(parseExpr(tok));
		}

		advance(tok); // Jump over "{"
		return std::move(block);
	}

	NumLiteralNode* parseNumLiteral(It& tok)
	{
		auto literal= newNode<NumLiteralNode>();
		literal->value= tok->text;
		log(literal->value);

		nextToken(tok);

		return std::move(literal);
	}

	AstNode* parseExpr(It& tok)
	{
		if (tok->type == TokenType::identifier) {
			if (tok->text == "let") {
				return parseVarDecl(tok);
			} else if (tok->text == "fn") {
				auto func_type_node= parseFuncType(tok);
				/// @todo Other block properties
				if (tok->type == TokenType::endStatement) {
					return std::move(func_type_node);
				} else if (tok->type == TokenType::openBlock) {
					auto block= parseBlock(tok);
					block->functionType= std::move(func_type_node);
					return std::move(block);
				} else {
					parseCheck(false, "Rubbish after function type");
				}
			} else {
				auto type= newNode<IdentifierNode>();
				type->name= tok->text;
				log(type->name);
				return std::move(type);
			}
		} else if (tok->type == TokenType::number) {
			return parseNumLiteral(tok);
		}

		parseCheck(false, "Broken expression");
	}

	AstContext parse()
	{
		// Start parsing
		auto root= newNode<GlobalNode>();
		auto it= tokens.begin();
		while (it != tokens.end()) {
			root->nodes.emplace_back(parseExpr(it));
		}
		return std::move(context);
	}

};



} // anonymous

AstContext genAst(const Tokens& tokens)
{
	Parser parser{tokens};
	return parser.parse();
}

} // gamelang
