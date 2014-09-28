#include "codegen.hpp"

namespace gamelang
{
namespace  
{

struct CCodeGen {
	std::string gen(const GlobalNode& global)
	{
		std::string code;
		for (const AstNodePtr& node : global.nodes) {
			code += gen(*node);
			code += "\n";
		}
		return code;
	}

	std::string gen(const BlockNode& block)
	{
		std::string code;
		for (const AstNodePtr& node : block.nodes) {
			code += gen(*node);
			code += "\n";
		}
		return code;
	}

	std::string gen(const VarDeclNode& var)
	{
		std::string code;
		
		assert(var.valueType);

		code += gen(*var.valueType);
		code += " ";
		code += var.name;
		
		if (var.value) {
			code += "= ";
			code +=	gen(*var.value);
		}

		code += ";";

		return code;
	}

	std::string gen(const StructTypeNode& type)
	{
		std::string code;
		code += type.name;
		return code;
	}

	std::string gen(const FuncTypeNode& func)
	{
		std::string code;
		return code;
	}

	std::string gen(const NumLiteralNode& literal)
	{
		return literal.value;
	}

	template <AstNodeType nodeType, typename T>
	struct ConditionalGen {
		static void eval(CCodeGen& self, std::string& result, const AstNode& node)
		{
			if (node.type == nodeType) {
				result= self.gen(static_cast<const T&>(node));
			}
		}
	};

	template <AstNodeType nodeType, typename T>
	constexpr void conditionalGen(CCodeGen& self, std::string& result, const AstNode& node)
	{ ConditionalGen<nodeType, T>::eval(self, result, node); }

	std::string gen(const AstNode& node)
	{
		std::string str;
		conditionalGen<AstNodeType::global,     GlobalNode>(*this, str, node);
		conditionalGen<AstNodeType::block,      BlockNode>(*this, str, node);
		conditionalGen<AstNodeType::varDecl,    VarDeclNode>(*this, str, node);
		conditionalGen<AstNodeType::structType, StructTypeNode>(*this, str, node);
		conditionalGen<AstNodeType::funcType,   FuncTypeNode>(*this, str, node);
		conditionalGen<AstNodeType::numLiteral, NumLiteralNode>(*this, str, node);
		return str;
	}

};

} // anonymous

std::string genC(const AstNodePtr& root)
{
	assert(root);
	CCodeGen gen;
	return gen.gen(*root);
}

} // gamelang
