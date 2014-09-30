#include <stack>

#include "codegen.hpp"
#include "nullsafety.hpp"

// Debug
#include <iostream>

namespace gamelang
{
namespace  
{

struct CCodeGen {
	std::string code;

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

private:
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
			gen(*NONULL(node));
			emit(";\n");
		}
	}

	void gen(const BlockNode& block)
	{
		if (block.condition) {
			emit("if (");
			gen(*block.condition);
			emit(")");
		}

		emit("\n");
		emit("{\n");
		{ auto&& indent_guard= indentGuard();
			for (auto it= block.nodes.begin(); it != block.nodes.end(); ++it) {
				AstNode& node= *NONULL(*it);

				if (	block.funcType && std::next(it) == block.nodes.end() &&
						!containsEndStatement(node)) {
					// Implicit return
					emit("return ");
				}

				gen(node);
				emit(";\n");
			}
		}
		emit("}");
	}

	void gen(const VarDeclNode& var)
	{
		auto& value_type= *NONULL(var.valueType);
		
		if (value_type.type == AstNodeType::funcType) {
			assert(var.constant && "@todo Non-constant func vars");
			genFuncProto(value_type, var.name);
			if (var.value)
				gen(*var.value); // Block
		} else if (value_type.type == AstNodeType::structType) {
			assert(var.constant && "Non-constant struct var");
			emit("typedef struct ");
			gen(*NONULL(var.value)); // Block
			emit(" " + var.name);
		} else {
			// Variable
			gen(value_type);
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
		gen(*NONULL(param.valueType));
		emit(" " + param.name);
	}

	void genFuncProto(const AstNode& node, const std::string& name)
	{
		assert(node.type == AstNodeType::funcType);
		auto&& func= static_cast<const FuncTypeNode&>(node);

		gen(*NONULL(func.returnType));
		emit(" " + name);

		emit("(");
		for (auto it= func.params.begin(); it != func.params.end(); ++it) {
			const ParamDeclNode& p= *NONULL(*it);
			gen(p);
			if (std::next(it) != func.params.end())
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
		const char* spacing= "";
		if (op.opType != BiOpType::dot)
			spacing= " ";

		gen(*NONULL(op.lhs));
		emit(spacing + std::string(str(op.opType)) + spacing);
		gen(*NONULL(op.rhs));
	}

	void gen(const ReturnNode& ret)
	{
		emit("return ");
		if (ret.value)
			gen(*ret.value);
	}

	void gen(const CallNode& call)
	{
		gen(*NONULL(call.func));

		emit("(");
		for (auto it= call.args.begin(); it != call.args.end(); ++it) {
			const AstNode& arg= *NONULL(*it);
			gen(arg);
			if (std::next(it) != call.args.end())
				emit(", ");
		}
		emit(")");
	}

	template <AstNodeType nodeType, typename T>
	struct CondGen {
		static void eval(CCodeGen& self, const AstNode& node)
		{ if (node.type == nodeType) self.gen(static_cast<const T&>(node)); }
	};
};

/// Modifies Ast to conform C semantics
/// This makes code generation for the CCodeGen almost trivial
struct AstCModifier {
	AstContext& context;

	void mod()
	{ mod(context.getRootNode()); }

// private
	enum class ScopeType {
		global,
		structure,
		function,
		conditional,
		plainScope
	};

	std::stack<ScopeType> scopeStack;
	std::vector<AstNode*> globalInsertRequests;

	/// Variables initialized in current struct
	std::vector<VarDeclNode*> structInitVars;

	void mod(AstNode& node)
	{
		CondMod<AstNodeType::global,     GlobalNode>::eval(*this, node);
		CondMod<AstNodeType::block,      BlockNode>::eval(*this, node);
		CondMod<AstNodeType::varDecl,    VarDeclNode>::eval(*this, node);
	}

	void mod(GlobalNode& global)
	{
		scopeStack.push(ScopeType::global);
		for (auto it= global.nodes.begin(); it != global.nodes.end();) {
			mod(*NONULL(*it));

			if (!globalInsertRequests.empty()) {
				for (auto&& req : globalInsertRequests) {
					global.nodes.insert(std::next(it), req);
					++it;
				}
				globalInsertRequests.clear();
			} 
			else {
				++it;
			}
		}
		scopeStack.pop();
	}

	void mod(BlockNode& block)
	{
		ScopeType scope_type= ScopeType::plainScope;
		if (block.structure)
			scope_type= ScopeType::structure;
		else if (block.funcType)
			scope_type= ScopeType::function;
		else if (block.condition)
			scope_type= ScopeType::conditional;
		scopeStack.push(scope_type);

		for (auto it= block.nodes.begin(); it != block.nodes.end(); ++it) {
			AstNode& node= *NONULL(*it);
			mod(node);
		}

		scopeStack.pop();

		if (block.structure) {
			// Check for initialized vars and create ctor for them
			auto void_type= context.newNode<IdentifierNode>();
			void_type->name= "void";

			auto self_type= context.newNode<IdentifierNode>();
			self_type->name= "int"; /// @todo Correct type

			auto self_id= context.newNode<IdentifierNode>();
			self_id->name= "C_R_self";

			auto self_param= context.newNode<ParamDeclNode>();
			self_param->name= "C_R_self"; /// @todo Should use id, not str
			self_param->valueType= self_type;

			auto ctor_func_type= context.newNode<FuncTypeNode>();
			ctor_func_type->returnType= void_type;
			ctor_func_type->params.emplace_back(self_param);

			auto ctor_block= context.newNode<BlockNode>();
			ctor_block->funcType= ctor_func_type;

			auto ctor_func= context.newNode<VarDeclNode>();
			ctor_func->name= "C_R_ctor";
			ctor_func->valueType= ctor_func_type;
			ctor_func->value= ctor_block;

			for (VarDeclNode* decl : structInitVars) {
				/// @todo VarDeclNode should contain IdentifierNode, not str
				auto id= context.newNode<IdentifierNode>();
				id->name= decl->name;

				auto access_op= context.newNode<BiOpNode>();
				access_op->opType= BiOpType::dot;
				access_op->lhs= self_id;
				access_op->rhs= id;

				auto init_op= context.newNode<BiOpNode>();
				init_op->opType= BiOpType::assign;
				init_op->lhs= access_op;
				init_op->rhs= decl->value;

				// Add initialization to ctor func
				ctor_block->nodes.emplace_back(init_op);

				// Remove initialization from struct block
				decl->value= nullptr;
			}
			structInitVars.clear();

			globalInsertRequests.emplace_back(ctor_func);
		}
	}

	void mod(VarDeclNode& var)
	{
		if (scopeStack.top() == ScopeType::structure && var.value) {
			// Var is assigned inside struct
			// -> generate constructor
			structInitVars.push_back(&var);
		}
		if (var.value) {
			mod(*var.value);
		}
	}

	template <AstNodeType nodeType, typename T>
	struct CondMod {
		static void eval(AstCModifier& self, AstNode& node)
		{ if (node.type == nodeType) self.mod(static_cast<T&>(node)); }
	};
};

} // anonymous

std::string genC(AstContext& ctx)
{
	assert(ctx.hasRootNode());

	AstCModifier modifier{ctx};
	modifier.mod();

	CCodeGen gen;
	gen.gen(ctx.getRootNode());

	return gen.code;
}

} // gamelang
