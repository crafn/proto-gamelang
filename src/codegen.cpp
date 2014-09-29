#include "codegen.hpp"

namespace gamelang
{
namespace  
{

struct CCodeGen {
	std::string code;
	int indent= 0;

	struct IndentGuard {
		int& value;
		IndentGuard(int& v): value(v) { ++value; }
		~IndentGuard(){ --value; }
	};

	IndentGuard indentGuard() { return IndentGuard{indent}; }

	void emit(std::string str)
	{
		if (!code.empty() && code[code.size() - 1] == '\n') {
			for (int i= 0; i < indent; ++i)
				code += "  ";
		}
		code += str;
	}

	void gen(const GlobalNode& global)
	{
		for (const AstNode* node : global.nodes) {
			assert(node);
			gen(*node);
			emit("\n");
		}
	}

	void gen(const BlockNode& block)
	{
		auto&& indent_guard= indentGuard();

		emit("\n{\n");
		for (const AstNode* node : block.nodes) {
			assert(node);
			gen(*node);
			emit("\n");
		}
		emit("\n}");
	}

	void gen(const VarDeclNode& var)
	{
		assert(var.valueType);
		
		if (var.valueType->type == AstNodeType::funcType) {
			// Function
			assert(var.constant && "@todo Non-constant func vars");
			genFuncProto(*var.valueType, var.name);

			if (var.value) {
				// Block
				gen(*var.value);
			} else {
				emit(";");
			}
		} else {
			// Variable
			gen(*var.valueType);
			emit(" " + var.name);
			if (var.value) {
				emit("= ");
				gen(*var.value);
			}
			emit(";");
		}
	}

	void gen(const IdentifierNode& type)
	{
		emit(type.name);
	}

	void genFuncProto(const AstNode& node, const std::string& name)
	{
		assert(node.type == AstNodeType::funcType);
		auto&& func= static_cast<const FuncTypeNode&>(node);
		assert(func.returnType);

		gen(*func.returnType);
		emit(" " + name);

		emit("(");
		/// @todo Params
		emit(")");
	}

	void gen(const NumLiteralNode& literal)
	{
		emit(literal.value);
	}

	template <AstNodeType nodeType, typename T>
	struct CondGen {
		static void eval(CCodeGen& self, const AstNode& node)
		{
			if (node.type == nodeType) {
				self.gen(static_cast<const T&>(node));
			}
		}
	};

	void gen(const AstNode& node)
	{
		std::string str;
		CondGen<AstNodeType::global,     GlobalNode>::eval(*this, node);
		CondGen<AstNodeType::block,      BlockNode>::eval(*this, node);
		CondGen<AstNodeType::varDecl,    VarDeclNode>::eval(*this, node);
		CondGen<AstNodeType::identifier, IdentifierNode>::eval(*this, node);
		CondGen<AstNodeType::numLiteral, NumLiteralNode>::eval(*this, node);
	}

};

} // anonymous

std::string genC(const AstContext& ctx)
{
	assert(!ctx.nodes.empty());
	assert(ctx.nodes.front());

	CCodeGen gen;
	gen.gen(*ctx.nodes.front());
	return gen.code;
}

} // gamelang
