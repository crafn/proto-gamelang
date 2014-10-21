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
		emit("typedef int32_t i32;\n");
		emit("typedef int64_t i64;\n");
		emit("typedef float f32;\n");
		emit("typedef double f64;\n");
		emit("typedef unsigned char byte;\n");
		emit("typedef unsigned int uint;\n");
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

				if (	block.funcType &&
						std::next(it) == block.nodes.end() && 
						node.type != AstNodeType::endStatement &&
						node.type != AstNodeType::block // Prevents `return if`
					) {
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
			if (var.value->type == AstNodeType::block) {
				emit("typedef struct " + struct_name + " " + struct_name + ";\n");
				emit("struct " + struct_name);
				gen(*NONULL(var.value)); // Block
			} else if (var.value->type == AstNodeType::identifier) { // Alias
				auto& id= static_cast<IdentifierNode&>(*var.value);
				emit("typedef struct " + id.name + " " + struct_name);
			}
		} else if (value_type.type == AstNodeType::tplType) {
			assert(0 && "Trying to generate C for template type");
		} else {
			// Ordinary variable
			assert(value_type.type != AstNodeType::block);
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

		if (func.returnType)
			gen(*func.returnType);
		else
			emit("void");
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
		ModScope scope;
		mod(root, scope);
	}

private:
	enum class ScopeType {
		global,
		structure,
		function,
		conditional,
		loop,
		plainScope
	};

	struct ModScope {
		ScopeType type= ScopeType::plainScope;

		/// This will probably break soon
		/// @todo Replace with ast modifying
		std::map<IdentifierNode*, std::string> prefixes;

		/// Nodes which should be inserted at scope exit
		std::vector<AstNode*> cleanup;

		ModScope* parent= nullptr;
	};

	AstContext& context;
	std::map<AstNode*, std::string> mangledNames;
	std::vector<AstNode*> globalInsertRequests;
	/// Nodes inside block can request additional nodes inserted before them
	std::vector<AstNode*> scopeInsertRequests;
	bool removeThisRequest= false;

	std::string clashPrevention() const { return "_CG_"; }
	std::string ctorName(std::string type_name) const
	{ return "ctor___" + type_name; }
	std::string dtorName(std::string type_name) const
	{ return "dtor___" + type_name; }
	std::string selfName() const
	{ return clashPrevention() + "self"; }

	template <typename T>
	void mod(T*& node, ModScope& scope)
	{
		assert(node);
		chooseMod(*node, scope);
	}

	void chooseMod(AstNode& node, ModScope& scope)
	{
		CondMod<AstNodeType::global,     GlobalNode>::eval(*this, node, scope);
		CondMod<AstNodeType::identifier, IdentifierNode>::eval(*this, node, scope);
		CondMod<AstNodeType::block,      BlockNode>::eval(*this, node, scope);
		CondMod<AstNodeType::varDecl,    VarDeclNode>::eval(*this, node, scope);
		CondMod<AstNodeType::funcType,   FuncTypeNode>::eval(*this, node, scope);
		CondMod<AstNodeType::uOp,        UOpNode>::eval(*this, node, scope);
		CondMod<AstNodeType::biOp,          BiOpNode>::eval(*this, node, scope);
		CondMod<AstNodeType::ctrlStatement, CtrlStatementNode>::eval(*this, node, scope);
		CondMod<AstNodeType::call,          CallNode>::eval(*this, node, scope);
	}

	void modSpecific(GlobalNode& global, ModScope& scope)
	{
		for (auto it= global.nodes.begin(); it != global.nodes.end();) {
			mod(*it, scope);

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
	}

	void modSpecific(IdentifierNode& id, ModScope& scope)
	{
		ModScope* p_scope= &scope;
		while (p_scope) {
			auto it= p_scope->prefixes.find(
					&traceBoundId(id, BoundIdDist::nearest));
			if (it != p_scope->prefixes.end()) {
				id.name= it->second + id.name;
				break;
			}
			p_scope= p_scope->parent;
		}
	}

	void modSpecific(BlockNode& block, ModScope& scope)
	{
		assert(!block.tplType);

		if (block.funcType)
			mod(block.funcType, scope);

		ModScope block_scope;
		if (block.structType)
			block_scope.type= ScopeType::structure;
		else if (block.funcType)
			block_scope.type= ScopeType::function;
		else if (block.condition)
			block_scope.type= ScopeType::conditional;
		else if (block.loop)
			block_scope.type= ScopeType::loop;

		block_scope.parent= &scope;
		for (auto it= block.nodes.begin(); it != block.nodes.end(); ++it) {
			mod(*it, block_scope);

			if (removeThisRequest) {
				it= block.nodes.erase(it);
				removeThisRequest= false;
				continue;
			}

			if (!scopeInsertRequests.empty()) {
				// Pre-insert
				for (auto&& req : scopeInsertRequests) {
					block.nodes.insert(it, req);
				}
				scopeInsertRequests.clear();
			}
		}

		if (!block.structType) {
			for (auto&& node : block_scope.cleanup) {
				block.nodes.emplace_back(node);
			}
		}

		if (block.structType) {
			// Generate ctor for structure
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
			self_id->name= selfName();

			auto self_var= context.newNode<VarDeclNode>();
			self_var->valueType= block.boundTo;
			self_var->constant= false;
			self_var->identifier= self_id;
			self_id->boundTo= self_var;
			ctor_block->nodes.emplace_back(self_var);
			ctor_block->nodes.emplace_back(context.newNode<EndStatementNode>());

			for (VarDeclNode* decl : block.structType->varDecls) {
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

		if (block.structType) {
			// Generate dtor for block
			assert(block.structType && "@todo Arbitrary block dtors");
			auto dtor_func_type= context.newNode<FuncTypeNode>();

			auto dtor_block= context.newNode<BlockNode>();
			dtor_block->funcType= dtor_func_type;

			auto func_id= context.newNode<IdentifierNode>();
			func_id->name= dtorName(NONULL(block.boundTo)->name);

			auto dtor_func= context.newNode<VarDeclNode>();
			dtor_func->identifier= func_id;
			dtor_func->valueType= dtor_func_type;
			dtor_func->value= dtor_block;

			auto self_id= context.newNode<IdentifierNode>();
			self_id->name= clashPrevention() + "self";

			auto ptr_to_self= context.newNode<UOpNode>();
			ptr_to_self->opType= UOpType::pointer;
			ptr_to_self->target= block.boundTo;

			auto self_var= context.newNode<VarDeclNode>();
			self_var->param= true;
			self_var->valueType= ptr_to_self;
			self_var->constant= false;
			self_var->identifier= self_id;
			self_id->boundTo= self_var;
			dtor_func_type->params.emplace_back(self_var);

			ModScope dtor_scope;
			dtor_scope.parent= &scope;
			for (VarDeclNode* decl : block.structType->varDecls) {
				auto access_op= context.newNode<BiOpNode>();
				access_op->opType= BiOpType::dot;
				access_op->lhs= self_id;
				access_op->rhs= decl->identifier;
				// `member` -> `self->member`
				assert(decl->identifier->boundTo == decl);
				dtor_scope.prefixes[decl->identifier]= self_id->name + "->";
			}
			if (block.destructor) {
				for (auto&& node : block.destructor->nodes) {
					mod(node, dtor_scope);
					dtor_block->nodes.emplace_back(node); // Custom dtor code
				}
			}
			for (auto&& node : block_scope.cleanup) {
				dtor_block->nodes.emplace_back(node); // Dtors of members
			}

			globalInsertRequests.emplace_back(
					context.newNode<EndStatementNode>());
			globalInsertRequests.emplace_back(dtor_func);
		}
	}

	void modSpecific(VarDeclNode& var, ModScope& scope)
	{
		assert(var.valueType);
		if (var.valueType->type == AstNodeType::builtinType) {
			removeThisRequest= true;
			return;
		}

		// Add cleanup code for locals and parameters
		auto& traced_type= traceValue(*var.valueType);
		bool is_initialized= var.value || var.param;
		if (is_initialized && traced_type.type == AstNodeType::block) {
			auto ptr_to_val= context.newNode<UOpNode>();
			ptr_to_val->opType= UOpType::addrOf;
			ptr_to_val->target= var.identifier;
			if (scope.type == ScopeType::structure) {
				auto self_id= context.newNode<IdentifierNode>();
				self_id->name= selfName();

				auto access_op= context.newNode<BiOpNode>();
				access_op->opType= BiOpType::rightArrow;
				access_op->lhs= self_id;
				access_op->rhs= var.identifier;
				ptr_to_val->target= access_op;
			}

			auto block_bound_id= static_cast<BlockNode&>(traced_type).boundTo;
			auto type_name=
				traceBoundId(*NONULL(block_bound_id), BoundIdDist::nearest).name;

			auto dtor_call_id= context.newNode<IdentifierNode>();
			dtor_call_id->name= dtorName(type_name);


			auto dtor_call= context.newNode<CallNode>();
			dtor_call->func= dtor_call_id;
			dtor_call->args.emplace_back(ptr_to_val);

			scope.cleanup.emplace(	scope.cleanup.begin(),
									context.newNode<EndStatementNode>());
			scope.cleanup.emplace(	scope.cleanup.begin(),
									dtor_call);
		}

		if (var.valueType->type == AstNodeType::block) {
			// Replace in-place type with identifier
			var.valueType= &traceBoundId(*var.valueType, BoundIdDist::furthest);
		}
		mod(var.valueType, scope);

		if (var.value) {
			mangledNames[var.value]= NONULL(var.identifier)->name;
			mod(var.value, scope);
		}

	}

	void modSpecific(FuncTypeNode& func, ModScope& scope)
	{
		if (func.returnType)
			mod(func.returnType, scope);
		for (auto&& p : func.params) {
			mod(p, scope);
		}
	}

	void modSpecific(UOpNode& op, ModScope& scope)
	{
		if (op.target->type == AstNodeType::block) {
			auto& block= *NONULL(static_cast<BlockNode*>(op.target));
			op.target= block.boundTo;
		}
		mod(op.target, scope);
	}

	void modSpecific(BiOpNode& op, ModScope& scope)
	{
		mod(op.lhs, scope);
		mod(op.rhs, scope);
	}
	
	void modSpecific(CtrlStatementNode& ctrl, ModScope& scope)
	{
		auto addCleanupNodes= [this, &scope]
		(ScopeType top_scope) -> void {
			auto cur_scope= &scope;
			while (cur_scope){
				insert(scopeInsertRequests, cur_scope->cleanup);
				if (cur_scope->type == top_scope)
					break;
				cur_scope= cur_scope->parent;
			}
		};

		switch(ctrl.statementType) {
			case CtrlStatementType::return_:
				addCleanupNodes(ScopeType::function);
			break;
			case CtrlStatementType::break_:
			case CtrlStatementType::continue_:
				addCleanupNodes(ScopeType::loop);
			break;
			case CtrlStatementType::goto_:
				assert(0 && "@todo Cleanup code insertion with goto");
			break;
			default:;	
		}	

		if (ctrl.value)
			mod(ctrl.value, scope);
	}

	void modSpecific(CallNode& call, ModScope& scope)
	{
		// Resolve argument routing
		std::vector<AstNode*> new_args=
			resolveRouting(	joined(listToVec(call.args), call.implicitArgs),
							call.argRouting);

		call.args= vecToList(new_args);
		call.implicitArgs.clear();
		call.argRouting.clear(); // Routing is not up-to-date anymore
		call.namedArgs.clear(); // No named args in C

		for (auto&& arg : call.args)
			mod(arg, scope);

		// Handle ctor calls
		auto& func= traceValue(*NONULL(call.func));
		if (func.type == AstNodeType::block) {
			auto& func_block= static_cast<BlockNode&>(func);
			if (func_block.structType) {
				// Swap `Type(..)` with compiler-generated ctor call
				auto ctor_id= context.newNode<IdentifierNode>();
				ctor_id->name= ctorName(mangledNames[&func_block]);
				call.func= ctor_id;
			}
		}
	}

	template <AstNodeType nodeType, typename T>
	struct CondMod {
		static void eval(AstCModifier& self, AstNode& node, ModScope& scope)
		{ if (node.type == nodeType) self.modSpecific(static_cast<T&>(node), scope); }
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
