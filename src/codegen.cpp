#include "codegen.hpp"
#include "metaprocessor.hpp"
#include "nullsafety.hpp"

#include <map>
#include <stack>

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
		CondGen<AstNodeType::endStatement,  EndStatementNode>::eval(*this, node);
		CondGen<AstNodeType::block,         BlockNode>::eval(*this, node);
		CondGen<AstNodeType::varDecl,       VarDeclNode>::eval(*this, node);
		CondGen<AstNodeType::identifier,    IdentifierNode>::eval(*this, node);
		CondGen<AstNodeType::numLiteral,    NumLiteralNode>::eval(*this, node);
		CondGen<AstNodeType::stringLiteral, StringLiteralNode>::eval(*this, node);
		CondGen<AstNodeType::nullLiteral,   NullLiteralNode>::eval(*this, node);
		CondGen<AstNodeType::uOp,           UOpNode>::eval(*this, node);
		CondGen<AstNodeType::biOp,          BiOpNode>::eval(*this, node);
		CondGen<AstNodeType::ctrlStatement, CtrlStatementNode>::eval(*this, node);
		CondGen<AstNodeType::call,          CallNode>::eval(*this, node);
		CondGen<AstNodeType::label,         LabelNode>::eval(*this, node);
		CondGen<AstNodeType::comment,       CommentNode>::eval(*this, node);
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
		emit("#include <stdbool.h>\n");
		emit("#include <stdint.h>\n");
		emit("typedef int32_t int32;\n");
		emit("typedef int64_t int64;\n");
		/// @todo Rest
		for (const AstNode* node : global.nodes) {
			gen(*NONULL(node));
			emit("\n");
		}
	}

	void gen(const EndStatementNode& end)
	{
		emit(";\n");
	}

	void gen(const IdentifierNode& type)
	{
		if (type.name == "uint")
			emit("unsigned int");
		else
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
		if (!block.external)
			emit("{\n");
		{ auto&& indent_guard= indentGuard();
			std::string row_prefix= "";
			if (block.external)
				row_prefix= "extern ";

			for (auto it= block.nodes.begin(); it != block.nodes.end(); ++it) {
				AstNode& node= *NONULL(*it);

				if (node.type == AstNodeType::varDecl)
					emit(row_prefix);

				if (	std::next(it) == block.nodes.end() && 
						node.type != AstNodeType::endStatement &&
						block.funcType) {
					emit("return ");
					gen(node);
					emit(";\n");
				} else {
					gen(node);
				}
			}
		}
		if (!block.external)
			emit("}");
	}

	void gen(const VarDeclNode& var)
	{
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
		} else if (value_type.type == AstNodeType::tplType) {
			assert(0 && "Trying to generate C for template type");
		} else {
			// Ordinary variable
			gen(value_type);
			if (var.constant)
				emit(" const");

			emit(" " + NONULL(var.identifier)->name);

			if (var.value && !var.param) {
				emit(" = ");
				gen(*var.value);
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

	void gen(const StringLiteralNode& literal)
	{
		emit("\"" + literal.str + "\"");
	}

	void gen(const NullLiteralNode& literal)
	{
		emit("0");
	}

	void gen(const UOpNode& op)
	{
		auto& target= *NONULL(op.target);
		switch (op.opType) {
			case UOpType::addrOf:
				emit("&");
				gen(target);
			break;
			case UOpType::deref:
				emit("*");
				gen(target);
			break;
			case UOpType::reference:
			case UOpType::pointer:
				gen(target);
				emit("*");
			break;
			case UOpType::sizeOf:
				emit("sizeof(");
				gen(target);
				emit(")");
			break;
			default: assert(0 && "Unknown UOP");
		}
	}

	void gen(const BiOpNode& op)
	{
		// Quick hack for incorrect parens due to lack of op precedence
		/// @todo Remove
		bool needs_parens= false;
		if (	op.opType == BiOpType::add ||
				op.opType == BiOpType::sub ||
				op.opType == BiOpType::mul ||
				op.opType == BiOpType::div ||
				op.opType == BiOpType::equals ||
				op.opType == BiOpType::nequals)
			needs_parens= true;

		const char* spacing= "";
		if (	op.opType != BiOpType::dot &&
				op.opType != BiOpType::rightArrow)
			spacing= " ";

		if (needs_parens)
			emit("(");
		gen(*NONULL(op.lhs));
		emit(spacing + std::string(str(op.opType)) + spacing);
		gen(*NONULL(op.rhs));
		if (needs_parens)
			emit(")");
	}

	void gen(const CtrlStatementNode& ctrl)
	{
		switch (ctrl.statementType) {
			case CtrlStatementType::return_: emit("return "); break;
			case CtrlStatementType::goto_: emit("goto "); break;
			case CtrlStatementType::break_: emit("break"); break;
			case CtrlStatementType::continue_: emit("continue"); break;
			case CtrlStatementType::else_: emit("else "); break;
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

	void gen(const LabelNode& label)
	{
		gen(*NONULL(label.identifier));
		emit(":");
	}

	void gen(const CommentNode& comment)
	{
		return;
		emit("//" + comment.text + "\n");
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
	{
		AstNode* root= &context.getRootNode();
		mod(root);
	}

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
	std::map<AstNode*, std::string> mangledNames;
	std::vector<AstNode*> globalInsertRequests;
	std::vector<AstNode*> localInsertRequests;
	bool removeThisRequest= false;

	std::string clashPrevention() const { return "_CG_"; }
	std::string ctorName(std::string type_name) const
	{ return "ctor___" + type_name; }

	template <typename T>
	void mod(T*& node)
	{
		assert(node);
		chooseMod(*node);
	}

	void chooseMod(AstNode& node)
	{
		CondMod<AstNodeType::global,     GlobalNode>::eval(*this, node);
		CondMod<AstNodeType::block,      BlockNode>::eval(*this, node);
		CondMod<AstNodeType::varDecl,    VarDeclNode>::eval(*this, node);
		CondMod<AstNodeType::uOp,        UOpNode>::eval(*this, node);
		CondMod<AstNodeType::biOp,          BiOpNode>::eval(*this, node);
		CondMod<AstNodeType::ctrlStatement, CtrlStatementNode>::eval(*this, node);
		CondMod<AstNodeType::call,          CallNode>::eval(*this, node);
	}

	void specificMod(GlobalNode& global)
	{
		scopeStack.push(ScopeType::global);
		for (auto it= global.nodes.begin(); it != global.nodes.end();) {
			mod(*it);

			if (removeThisRequest) {
				it= global.nodes.erase(it);
				removeThisRequest= false;
				continue;
			}

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

	void specificMod(BlockNode& block)
	{
		assert(!block.tplType);

		ScopeType scope_type= ScopeType::plainScope;
		if (block.structType)
			scope_type= ScopeType::structure;
		else if (block.funcType)
			scope_type= ScopeType::function;
		else if (block.condition)
			scope_type= ScopeType::conditional;
		scopeStack.push(scope_type);

		for (auto it= block.nodes.begin(); it != block.nodes.end();) {
			mod(*it);

			if (removeThisRequest) {
				it= block.nodes.erase(it);
				removeThisRequest= false;
				continue;
			}

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
			self_var->constant= false;
			self_var->identifier= self_id;
			self_id->boundTo= self_var;
			ctor_block->nodes.emplace_back(self_var);
			ctor_block->nodes.emplace_back(context.newNode<EndStatementNode>());

			for (VarDeclNode* decl : struct_type->varDecls) {
				auto member_param_id= context.newNode<IdentifierNode>();
				member_param_id->name= decl->identifier->name;
				
				auto member_param= context.newNode<VarDeclNode>();
				member_param->param= true;
				member_param->valueType= decl->valueType;
				member_param->constant= decl->constant;
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

				// Add initialization to ctor func
				ctor_block->nodes.emplace_back(init_op);
				ctor_block->nodes.emplace_back(
						context.newNode<EndStatementNode>());

				// Remove initialization from struct block
				decl->value= nullptr;
			}

			auto ret= context.newNode<CtrlStatementNode>();
			ret->statementType= CtrlStatementType::return_;
			ret->value= self_id;
			ctor_block->nodes.emplace_back(ret);
			ctor_block->nodes.emplace_back(
					context.newNode<EndStatementNode>());

			globalInsertRequests.emplace_back(
					context.newNode<EndStatementNode>());
			globalInsertRequests.emplace_back(ctor_func);
		}
	}

	void specificMod(VarDeclNode& var)
	{
		assert(var.valueType);
		if (var.valueType->type == AstNodeType::builtinType) {
			removeThisRequest= true;
			return;
		}

		if (var.valueType->type == AstNodeType::block) {
			// Replace in-place type with identifier
			var.valueType= static_cast<BlockNode*>(var.valueType)->boundTo;
			assert(var.valueType);
		}

		if (var.value) {
			mangledNames[var.value]= NONULL(var.identifier)->name;
			mod(var.value);
		}

	}

	void specificMod(UOpNode& op)
	{
		mod(op.target);
	}

	void specificMod(BiOpNode& op)
	{
		mod(op.lhs);
		mod(op.rhs);
	}
	
	void specificMod(CtrlStatementNode& ctrl)
	{
		if (ctrl.value)
			mod(ctrl.value);
	}

	void specificMod(CallNode& call)
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
			mod(arg);

		// Handle ctor calls
		auto& func= traceValue(*NONULL(call.func));
		if (func.type == AstNodeType::block) {
			auto& func_block= static_cast<BlockNode&>(func);
			if (func_block.structType) {
				// Swap `Type(..)` to compiler-generated ctor call
				auto ctor_id= context.newNode<IdentifierNode>();
				ctor_id->name= ctorName(mangledNames[&func_block]);
				call.func= ctor_id;
			}
		}
	}

	template <AstNodeType nodeType, typename T>
	struct CondMod {
		static void eval(AstCModifier& self, AstNode& node)
		{ if (node.type == nodeType) self.specificMod(static_cast<T&>(node)); }
	};
};

} // anonymous

std::string genC(AstContext& ctx)
{
	auto no_meta_ctx= runMetaprograms(ctx);

	AstCModifier modifier{no_meta_ctx};
	modifier.mod();

	CCodeGen gen;
	gen.gen(no_meta_ctx.getRootNode());

	return gen.code;
}

} // gamelang
