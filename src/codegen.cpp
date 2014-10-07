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
		CondGen<AstNodeType::global,        GlobalNode>::eval(*this, node);
		CondGen<AstNodeType::block,         BlockNode>::eval(*this, node);
		CondGen<AstNodeType::varDecl,       VarDeclNode>::eval(*this, node);
		CondGen<AstNodeType::identifier,    IdentifierNode>::eval(*this, node);
		CondGen<AstNodeType::numLiteral,    NumLiteralNode>::eval(*this, node);
		CondGen<AstNodeType::nullLiteral,   NullLiteralNode>::eval(*this, node);
		CondGen<AstNodeType::uOp,           UOpNode>::eval(*this, node);
		CondGen<AstNodeType::biOp,          BiOpNode>::eval(*this, node);
		CondGen<AstNodeType::ctrlStatement, CtrlStatementNode>::eval(*this, node);
		CondGen<AstNodeType::call,          CallNode>::eval(*this, node);
		CondGen<AstNodeType::qualifier,     QualifierNode>::eval(*this, node);
		CondGen<AstNodeType::label,         LabelNode>::eval(*this, node);
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
		emit("#include <stdlib.h>\n");
		emit("#include <stdbool.h>\n");
		for (const AstNode* node : global.nodes) {
			gen(*NONULL(node));
			emit(";\n");
		}
	}

	void gen(const IdentifierNode& type)
	{
		emit(type.name);
	}

	void gen(const BlockNode& block)
	{
		if (block.condition) {
			emit("if (");
			gen(*block.condition);
			emit(")");
		}

		if (block.loop) {
			emit("while (1)");
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
		if (var.param) {
			gen(*NONULL(var.valueType));
			emit(" " + NONULL(var.identifier)->name);
		} else {
			auto& value_type= *NONULL(var.valueType);
			
			if (value_type.type == AstNodeType::funcType) {
				assert(var.constant && "@todo Non-constant func vars");
				genFuncProto(value_type, NONULL(var.identifier)->name);
				if (var.value)
					gen(*var.value); // Block
			} else if (value_type.type == AstNodeType::structType) {
				assert(var.constant && "Non-constant struct var");
				const std::string struct_name= NONULL(var.identifier)->name;
				emit("typedef struct " + struct_name + " " + struct_name + ";\n");
				emit("struct " + struct_name);
				gen(*NONULL(var.value)); // Block
			} else {
				// Ordinary variable
				gen(value_type);
				if (var.constant)
					emit(" const");

				emit(" " + NONULL(var.identifier)->name);

				if (var.value) {
					emit(" = ");
					gen(*var.value);
				}
			}
		}
	}

	void genFuncProto(const AstNode& node, const std::string& name)
	{
		assert(node.type == AstNodeType::funcType);
		auto&& func= static_cast<const FuncTypeNode&>(node);

		gen(*NONULL(func.returnType));
		emit(" " + name);

		emit("(");
		for (auto it= func.params.begin(); it != func.params.end(); ++it) {
			const VarDeclNode& p= *NONULL(*it);
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

	void gen(const NullLiteralNode& literal)
	{
		emit("0");
	}

	void gen(const UOpNode& op)
	{
		switch (op.opType) {
			case UOpType::addrOf: emit("&"); break;
			default: assert(0 && "Unknown UOP");
		}
		gen(*NONULL(op.target));
	}

	void gen(const BiOpNode& op)
	{
		const char* spacing= "";
		if (	op.opType != BiOpType::dot &&
				op.opType != BiOpType::rightArrow)
			spacing= " ";

		gen(*NONULL(op.lhs));
		emit(spacing + std::string(str(op.opType)) + spacing);
		gen(*NONULL(op.rhs));
	}

	void gen(const CtrlStatementNode& ctrl)
	{
		switch (ctrl.statementType) {
			case CtrlStatementType::return_: emit("return "); break;
			case CtrlStatementType::goto_: emit("goto "); break;
			case CtrlStatementType::break_: emit("break"); break;
			default: emit("unknown_ctrl_statement");
		}
	
		if (ctrl.value)
			gen(*ctrl.value);
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

	void gen(const QualifierNode& qual)
	{
		gen(*NONULL(qual.target));
		if (	qual.qualifierType == QualifierType::pointer ||
				qual.qualifierType == QualifierType::reference)
			emit("*");

	}

	void gen(const LabelNode& label)
	{
		gen(*NONULL(label.identifier));
		emit(":");
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
	AstCModifier(AstContext& ctx): context(ctx) {}

	void mod()
	{ mod(context.getRootNode()); }

private:
	enum class ScopeType {
		global,
		structure,
		function,
		conditional,
		plainScope
	};

	AstContext& context;
	std::stack<ScopeType> scopeStack;
	std::vector<AstNode*> globalInsertRequests;
	std::vector<AstNode*> localInsertRequests;

	std::string clashPrevention() const { return "_cR_"; }
	std::string ctorName(std::string type_name) const
	{ return clashPrevention() + "ctor_" + type_name; }

	void mod(AstNode& node)
	{
		CondMod<AstNodeType::global,     GlobalNode>::eval(*this, node);
		CondMod<AstNodeType::block,      BlockNode>::eval(*this, node);
		CondMod<AstNodeType::varDecl,    VarDeclNode>::eval(*this, node);
		CondMod<AstNodeType::uOp,           UOpNode>::eval(*this, node);
		CondMod<AstNodeType::biOp,          BiOpNode>::eval(*this, node);
		CondMod<AstNodeType::ctrlStatement, CtrlStatementNode>::eval(*this, node);
		CondMod<AstNodeType::call,          CallNode>::eval(*this, node);
		CondMod<AstNodeType::qualifier,     QualifierNode>::eval(*this, node);
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
			} else {
				++it;
			}
		}
		scopeStack.pop();
	}

	void mod(BlockNode& block)
	{
		ScopeType scope_type= ScopeType::plainScope;
		if (block.structType)
			scope_type= ScopeType::structure;
		else if (block.funcType)
			scope_type= ScopeType::function;
		else if (block.condition)
			scope_type= ScopeType::conditional;
		scopeStack.push(scope_type);

		for (auto it= block.nodes.begin(); it != block.nodes.end();) {
			AstNode& node= *NONULL(*it);
			mod(node);

			if (!localInsertRequests.empty()) {
				for (auto&& req : localInsertRequests) {
					block.nodes.insert(std::next(it), req);
					++it;
				}
				localInsertRequests.clear();
			} else {
				++it;
			}
		}

		scopeStack.pop();

		if (block.structType) {
			// Generate ctor for structure
			assert(block.structType->type == AstNodeType::structType);
			auto struct_type= static_cast<StructTypeNode*>(block.structType);

			auto ctor_func_type= context.newNode<FuncTypeNode>();
			ctor_func_type->returnType= block.boundTo;

			auto ctor_block= context.newNode<BlockNode>();
			ctor_block->funcType= ctor_func_type;

			auto func_id= context.newNode<IdentifierNode>();
			func_id->name= ctorName(NONULL(block.boundTo)->name);

			auto ctor_func= context.newNode<VarDeclNode>();
			ctor_func->identifier= func_id;
			ctor_func->valueType= ctor_func_type;
			ctor_func->value= ctor_block;

			auto self_id= context.newNode<IdentifierNode>();
			self_id->name= clashPrevention() + "self";

			auto self_var= context.newNode<VarDeclNode>();
			self_var->param= true;
			self_var->valueType= block.boundTo;
			self_var->endStatement= true;
			self_var->identifier= self_id;
			self_id->boundTo= self_var;
			ctor_block->nodes.emplace_back(self_var);

			for (VarDeclNode* decl : struct_type->varDecls) {
				auto member_param_id= context.newNode<IdentifierNode>();
				member_param_id->name= decl->identifier->name;
				
				auto member_param= context.newNode<VarDeclNode>();
				member_param->param= true;
				member_param->valueType= decl->valueType;
				member_param->identifier= member_param_id;
				member_param_id->boundTo= member_param;
				ctor_func_type->params.emplace_back(member_param);

				auto access_op= context.newNode<BiOpNode>();
				access_op->opType= BiOpType::dot;
				access_op->lhs= self_id;
				access_op->rhs= decl->identifier;

				auto init_op= context.newNode<BiOpNode>();
				init_op->opType= BiOpType::assign;
				init_op->lhs= access_op;
				init_op->rhs= member_param_id;
				init_op->endStatement= true;

				// Add initialization to ctor func
				ctor_block->nodes.emplace_back(init_op);

				// Remove initialization from struct block
				decl->value= nullptr;
			}

			auto ret= context.newNode<CtrlStatementNode>();
			ret->statementType= CtrlStatementType::return_;
			ret->value= self_id;
			ret->endStatement= true;
			ctor_block->nodes.emplace_back(ret);

			globalInsertRequests.emplace_back(ctor_func);
		}
	}

	void mod(VarDeclNode& var)
	{
		assert(var.valueType);
		if (var.value)
			mod(*var.value);
	}

	void mod(UOpNode& op)
	{
		mod(*NONULL(op.target));
	}

	void mod(BiOpNode& op)
	{
		mod(*NONULL(op.lhs));
		mod(*NONULL(op.rhs));
	}
	
	void mod(CtrlStatementNode& ctrl)
	{
		mod(*NONULL(ctrl.value));
	}

	void mod(CallNode& call)
	{
		// Resolve argument routing
		std::vector<AstNode*> new_args;
		new_args.resize(call.args.size() + call.implicitArgs.size());
		std::size_t i= 0;
		auto setNextArg= [&] (AstNode* arg)
		{
			int param_i= call.argRouting[i];
			assert(param_i >= 0 && param_i < new_args.size());
			new_args[param_i]= arg;
			++i;
		};
		for (auto&& arg : call.args)
			setNextArg(arg);
		for (auto&& arg : call.implicitArgs)
			setNextArg(arg);

		call.args= vecToList(new_args);
		call.implicitArgs.clear();
		call.argRouting.clear(); // Routing is not up-to-date anymore
		call.namedArgs.clear(); // No named args in C

		for (auto&& arg : call.args)
			mod(*NONULL(arg));

		// Handle ctor calls
		assert(NONULL(NONULL(call.func)->boundTo)->type == AstNodeType::varDecl);
		auto func_decl= static_cast<VarDeclNode*>(call.func->boundTo);
		if (NONULL(func_decl->valueType)->type == AstNodeType::structType) {
			// Swap `Type(..)` to compiler-generated ctor call
			auto ctor_id= context.newNode<IdentifierNode>();
			ctor_id->name= ctorName(NONULL(func_decl->identifier)->name);

			call.func= ctor_id;
		}
	}

	void mod(QualifierNode& qual)
	{
		mod(*NONULL(qual.target));
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
