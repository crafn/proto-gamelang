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
			emit(";\n");
		}
	}

	void gen(const BlockNode& block)
	{
		emit("\n");
		emit("{\n");
		{ auto&& indent_guard= indentGuard();
			for (std::size_t i= 0; i < block.nodes.size(); ++i) {
				AstNode* node= block.nodes[i];
				assert(node);

				if (	block.functionType && i + 1 == block.nodes.size() &&
						!containsEndStatement(*node)) {
					// Implicit return
					emit("return ");
				}

				gen(*node);
				emit(";\n");
			}
		}
		emit("}");
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
			}
		} else {
			// Variable
			gen(*var.valueType);
			if (var.constant)
				emit(" const");

			emit(" " + var.name);

			if (var.value) {
				emit(" = ");
				gen(*var.value);
			}
		}
	}

	void gen(const IdentifierNode& type)
	{
		emit(type.name);
	}

	void gen(const ParamDeclNode& param)
	{
		assert(param.valueType);
		gen(*param.valueType);
		emit(" " + param.name);
	}

	void genFuncProto(const AstNode& node, const std::string& name)
	{
		assert(node.type == AstNodeType::funcType);
		auto&& func= static_cast<const FuncTypeNode&>(node);
		assert(func.returnType);

		gen(*func.returnType);
		emit(" " + name);

		emit("(");
		for (std::size_t i= 0; i < func.params.size(); ++i) {
			ParamDeclNode* p= func.params[i];
			assert(p);
			gen(*p);
			if (i + 1 < func.params.size())
				emit(", ");
		}
		emit(")");
	}

	void gen(const NumLiteralNode& literal)
	{
		emit(literal.value);
	}

	void gen(const BiOpNode& op)
	{
		assert(op.lhs);
		assert(op.rhs);

		gen(*op.lhs);
		emit(" " + std::string(str(op.opType)) + " ");
		gen(*op.rhs);
	}

	void gen(const ReturnNode& ret)
	{
		emit("return ");
		if (ret.value)
			gen(*ret.value);
	}

	void gen(const CallNode& call)
	{
		assert(call.function);
		gen(*call.function);
		/// @todo params
		emit("()");
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
		CondGen<AstNodeType::global,     GlobalNode>::eval(*this, node);
		CondGen<AstNodeType::block,      BlockNode>::eval(*this, node);
		CondGen<AstNodeType::varDecl,    VarDeclNode>::eval(*this, node);
		CondGen<AstNodeType::identifier, IdentifierNode>::eval(*this, node);
		CondGen<AstNodeType::paramDecl,  ParamDeclNode>::eval(*this, node);
		CondGen<AstNodeType::numLiteral, NumLiteralNode>::eval(*this, node);
		CondGen<AstNodeType::biOp,       BiOpNode>::eval(*this, node);
		CondGen<AstNodeType::ret,        ReturnNode>::eval(*this, node);
		CondGen<AstNodeType::call,       CallNode>::eval(*this, node);
	}

};

} // anonymous

std::string genC(AstContext& ctx)
{
	assert(!ctx.nodes.empty());
	assert(ctx.nodes.front());

	CCodeGen gen;
	gen.gen(*ctx.nodes.front());
	return gen.code;
}

} // gamelang
