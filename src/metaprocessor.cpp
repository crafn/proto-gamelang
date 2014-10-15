#include "metaprocessor.hpp"
#include "ast.hpp"

#include <map>
#include <set>
#include <stack>
#include <tuple>

// Debug
#include <iostream>

namespace gamelang
{
namespace
{

struct Processor {
	Processor(const AstContext& ctx): input(ctx) {}

	AstContext run()
	{
		const AstNode* root= &input.getRootNode();
		run(root, TplScope{});
		return std::move(output);
	}

private:
	const AstContext& input;
	AstContext output;

	using BlockNodeIt= std::list<AstNode*>::iterator;
	struct TplArg {
		const IdentifierNode* id; // Points to tpl id in input ctx, e.g. `T`
		IdentifierNode* value; // Points to id in output ctx, e.g. `ConcreteType`

		bool operator<(const TplArg& other) const
		{
			auto v= &traceValue(*value);
			auto o_v= &traceValue(*other.value);
			assert(	(v == o_v) == (value->name == other.value->name) &&
					"Tracing bug");
			return	std::tie(id, value->name) <
					std::tie(other.id, other.value->name); }
	};
	struct TplScope {
		/// Given arguments to tpl struct/function
		std::vector<TplArg> args;

		bool operator<(const TplScope& other) const
		{ return args < other.args; }

		std::string str() const
		{
			std::string ret;
			for (auto&& a : args) {
				ret += "_";
				ret += NONULL(a.value)->name;
			}
			return ret;
		}
	};

	/// Output nodes
	std::stack<AstNode*> nodeStack;
	/// Maps input nodes to generated output nodes
	std::map<const AstNode*, std::map<TplScope, AstNode*>> inToOutNode;
	/// Used to insert template instantiations in place of original tpl decls
	std::map<const TplTypeNode*, BlockNodeIt> tplTypePlaces;
	/// Keeps track of instantiated templates
	std::set<std::tuple<const TplTypeNode*, AstNode*>> tplInstances;

	template <typename T>
	AstNode* run(const T* node, const TplScope& scope)
	{
		assert(node);
		return chooseRun(*node, scope);
	}

	AstNode* chooseRun(const AstNode& node, const TplScope& scope)
	{
		auto already_generated= findOutNodeOf(node, scope);
		if (already_generated)
			return already_generated;

		AstNode* ret= nullptr;
		CondRun<AstNodeType::global,        GlobalNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::endStatement,  EndStatementNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::identifier,    IdentifierNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::block,         BlockNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::varDecl,       VarDeclNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::funcType,      FuncTypeNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::structType,    StructTypeNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::builtinType,   BuiltinTypeNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::numLiteral,    NumLiteralNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::stringLiteral, StringLiteralNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::nullLiteral,   NullLiteralNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::uOp,           UOpNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::biOp,          BiOpNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::ctrlStatement, CtrlStatementNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::call,          CallNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::label,         LabelNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::comment,       CommentNode>::eval(*this, node, ret, scope);
		CondRun<AstNodeType::tplType,       TplTypeNode>::eval(*this, node, ret, scope);
		if (ret) {
			inToOutNode[&node][scope]= ret;
		}
		return ret;
	}

	void markTplTypePlace(const TplTypeNode& in_place)
	{
		AstNode& root= output.getRootNode();
		assert(root.type == AstNodeType::global);
		auto& global= static_cast<GlobalNode&>(root);
		tplTypePlaces[&in_place]= global.nodes.end();
	}

	void tryInsertTplInstance(const TplTypeNode& in_place, AstNode& node)
	{
		auto&& tpl_inst_identity= 
			std::make_tuple<const TplTypeNode*, AstNode*>
				(&in_place, &node);
		if (	tplInstances.find(tpl_inst_identity) !=
				tplInstances.end())
			return;
		tplInstances.insert(tpl_inst_identity);

		auto place_it= tplTypePlaces.find(&in_place);
		assert(place_it != tplTypePlaces.end());
		auto place= place_it->second;
		
		AstNode& root= output.getRootNode();
		assert(root.type == AstNodeType::global);
		auto& global= static_cast<GlobalNode&>(root);

		global.nodes.insert(place, &node);
	}

	AstNode* findOutNodeOf(const AstNode& in, const TplScope& scope)
	{
		auto m_it= inToOutNode.find(&in);
		if (m_it != inToOutNode.end()) {
			auto& node_map= m_it->second;
			auto out_it= node_map.find(scope);
			if (out_it != node_map.end())
				return out_it->second;
		}
		return nullptr;
	}

	AstNode* runSpecific(const GlobalNode& global_in, const TplScope& scope)
	{
		auto global_out= output.newNode<GlobalNode>();
		nodeStack.push(global_out);
		for (auto&& node : global_in.nodes) {
			AstNode* result= run(node, scope);
			if (result)
				global_out->nodes.emplace_back(result);
		}
		nodeStack.pop();
		return global_out;
	}

	AstNode* runSpecific(const EndStatementNode& end, const TplScope& scope)
	{
		return output.newNode<EndStatementNode>();
	}

	AstNode* runSpecific(const IdentifierNode& id_in, const TplScope& scope)
	{
		// If input id is bound to a tpl parameter,
		// substitute it with the scope argument
		for (auto&& arg : scope.args) {
			if (arg.id == &traceBoundId(id_in)) {
				return NONULL(arg.value);
			}
		}

		auto id_out= output.newNode<IdentifierNode>();
		id_out->name= id_in.name;
		
		std::cout << "ID " << id_in.name << " bound " << id_in.boundTo << std::endl;
		if (id_in.boundTo) {
			AstNode* bound_out= findOutNodeOf(*NONULL(id_in.boundTo), scope);
			if (bound_out)
				id_out->boundTo= bound_out;
		}
		return id_out;
	}

	AstNode* runSpecific(const BlockNode& block_in, const TplScope& scope)
	{
		AstNode& parent= *NONULL(nodeStack.top());

		assert(!(block_in.tplType && scope.args.empty()) && "Shouldn't be creating templates");

		auto block_out= output.newNode<BlockNode>();
		nodeStack.push(block_out);

		if (parent.type == AstNodeType::varDecl) {
			auto parent_var= static_cast<VarDeclNode*>(&parent);
			block_out->boundTo= parent_var->identifier;
		}

		block_out->loop= block_in.loop;
		block_out->external= block_in.external;

		// These are run also for tpl types!
		if (block_in.structType) {
			auto result= NONULL(run(block_in.structType, scope));
			assert(result->type == AstNodeType::structType);
			block_out->structType= static_cast<StructTypeNode*>(result);
		} else if (block_in.funcType) {
			block_out->funcType= NONULL(run(block_in.funcType, scope));
		} else if (block_in.condition) {
			block_out->condition= NONULL(run(block_in.condition, scope));
		}

		for (auto&& node : block_in.nodes) {
			auto result= run(node, scope);
			if (result) 
				block_out->nodes.emplace_back(result);
		}

		nodeStack.pop();
		return block_out;
	}

	AstNode* runSpecific(const VarDeclNode& var_in, const TplScope& scope)
	{
		/// @todo use traceValue
		bool is_tpl_decl= NONULL(var_in.valueType)->type == AstNodeType::tplType;
		if (is_tpl_decl && scope.args.empty()) {
			markTplTypePlace(static_cast<TplTypeNode&>(*var_in.valueType));
			return nullptr;
		} 

		auto var_out= output.newNode<VarDeclNode>();
		nodeStack.push(var_out);

		var_out->constant= var_in.constant;
		var_out->param= var_in.param;

		if (is_tpl_decl) {
			var_out->identifier= output.newNode<IdentifierNode>();
			var_out->identifier->boundTo= var_out;
			var_out->identifier->name=
				NONULL(var_in.identifier)->name + "__" + scope.str();
		} else {
			auto id_node= NONULL(run(var_in.identifier, scope));
			assert(id_node->type == AstNodeType::identifier);
			var_out->identifier= static_cast<IdentifierNode*>(id_node);
			var_out->identifier->boundTo= var_out;
		}

		var_out->valueType= run(var_in.valueType, scope); // null if tpl
		if (var_in.value)
			var_out->value= NONULL(run(var_in.value, scope));

		if (var_out->valueType == nullptr) {
			assert(var_out->value && "Can't have both null type and null value");
			var_out->valueType= &traceType(*var_out->value);
		}

		assert(var_out->valueType != nullptr);

		nodeStack.pop();
		return var_out;
	}

	AstNode* runSpecific(const FuncTypeNode& func_in, const TplScope& scope)
	{
		/// @todo tpl function
		auto func_out= output.newNode<FuncTypeNode>();
		nodeStack.push(func_out);

		for (auto&& node : func_in.params) {
			auto result= run(node, scope);
			if (result) {
				assert(result->type == AstNodeType::varDecl);
				auto var= static_cast<VarDeclNode*>(result);
				func_out->params.emplace_back(var);
			}
		}
		func_out->returnType= NONULL(run(func_in.returnType, scope));

		nodeStack.pop();
		return func_out;
	}

	AstNode* runSpecific(const StructTypeNode& st_type_in, const TplScope& scope)
	{
		auto st_type_out= output.newNode<StructTypeNode>();
		for (auto&& node : st_type_in.varDecls) {
			auto result= run(node, scope);
			if (result) {
				assert(result->type == AstNodeType::varDecl);
				auto var= static_cast<VarDeclNode*>(result);
				st_type_out->varDecls.emplace_back(var);
			}
		}
		return st_type_out;
	}

	AstNode* runSpecific(const BuiltinTypeNode& bt_type_in, const TplScope& scope)
	{
		return output.newNode<BuiltinTypeNode>();
	}

	AstNode* runSpecific(const NumLiteralNode& num_in, const TplScope& scope)
	{
		auto num_out= output.newNode<NumLiteralNode>();
		num_out->value= num_in.value;
		return num_out;
	}

	AstNode* runSpecific(const StringLiteralNode& str_in, const TplScope& scope)
	{
		auto str_out= output.newNode<StringLiteralNode>();
		str_out->str= str_in.str;
		return str_out;
	}

	AstNode* runSpecific(const NullLiteralNode& null_in, const TplScope& scope)
	{
		return output.newNode<NullLiteralNode>();
	}

	AstNode* runSpecific(const UOpNode& op_in, const TplScope& scope)
	{
		// Resolve decltype
		if (op_in.opType == UOpType::declType) {
			auto& expr= *NONULL(run(op_in.target, scope));
			return &traceType(expr);
		} else {
			auto op_out= output.newNode<UOpNode>();
			op_out->opType= op_in.opType;
			op_out->target= NONULL(run(op_in.target, scope));
			return op_out;
		}
	}

	AstNode* runSpecific(const BiOpNode& op_in, const TplScope& scope)
	{
		auto op_out= output.newNode<BiOpNode>();
		nodeStack.push(op_out);
		op_out->opType= op_in.opType;
		op_out->lhs= NONULL(run(op_in.lhs, scope));
		op_out->rhs= NONULL(run(op_in.rhs, scope));
		nodeStack.pop();
		return op_out;
	}

	AstNode* runSpecific(const CtrlStatementNode& ctrl_in, const TplScope& scope)
	{
		auto ctrl_out= output.newNode<CtrlStatementNode>();
		nodeStack.push(ctrl_out);
		ctrl_out->statementType= ctrl_in.statementType;
		if (ctrl_in.value) {
			ctrl_out->value= NONULL(run(ctrl_in.value, scope));
		}
		nodeStack.pop();
		return ctrl_out;
	}

	AstNode* runSpecific(const CallNode& call_in, const TplScope& scope)
	{
		if (call_in.tplCall) { // `vector[int]`
			auto& traced= traceValue(*NONULL(call_in.func));
			assert(traced.type == AstNodeType::block);
			auto& tpl_block= static_cast<const BlockNode&>(traced);
			assert(tpl_block.tplType);
			auto& tpl_params= tpl_block.tplType->params;
			auto& tpl_decl= *NONULL(NONULL(tpl_block.boundTo)->boundTo);
			assert(tpl_decl.type == AstNodeType::varDecl);

			{ // Instantiate template
				std::vector<AstNode*> implicit_args;
				std::vector<int> routing;
				routeCallArgs(implicit_args, routing, call_in);

				// Set up scope for template args
				TplScope sub_scope;
				sub_scope.args.resize(	call_in.args.size() +
										implicit_args.size());
				std::size_t i= 0;
				auto setNextArg= [&] (AstNode* arg_in)
				{
					assert(arg_in);
					assert(arg_in->type == AstNodeType::identifier && "@todo tpl arg exprs");

					auto arg_out= NONULL(run(arg_in, scope));
					assert(arg_out->type == AstNodeType::identifier);

					int param_i= routing[i];
					assert(param_i >= 0 && param_i < sub_scope.args.size());
					sub_scope.args[param_i].id= tpl_params[param_i]->identifier;
					sub_scope.args[param_i].value= static_cast<IdentifierNode*>(arg_out);
					++i;
				};
				for (auto&& arg : call_in.args)
					setNextArg(arg);
				for (auto&& arg : implicit_args)
					setNextArg(arg);

				auto tpl_inst= NONULL(run(&tpl_decl, sub_scope));
				assert(tpl_inst->type == AstNodeType::varDecl);
				auto tpl_inst_decl= static_cast<VarDeclNode*>(tpl_inst);

				tryInsertTplInstance(*tpl_block.tplType, *tpl_inst_decl);
				return NONULL(tpl_inst_decl->identifier);
			}
		} else { // `foo(bar)`
			assert(!call_in.tplCall && "Shouldn't be creating templates");
			auto call_out= output.newNode<CallNode>();
			nodeStack.push(call_out);

			call_out->namedArgs= call_in.namedArgs;
			call_out->func= NONULL(run(call_in.func, scope));

			for (auto&& arg : call_in.args) {
				call_out->args.emplace_back(NONULL(run(arg, scope))); 
			}

			routeCallArgs(	call_out->implicitArgs,
							call_out->argRouting,
							*call_out);

			for (auto&& arg : call_in.implicitArgs) {
				call_out->implicitArgs.emplace_back(NONULL(run(arg, scope))); 
			}
			nodeStack.pop();
			return call_out;
		}
	}

	AstNode* runSpecific(const LabelNode& label_in, const TplScope& scope)
	{
		auto label_out= output.newNode<LabelNode>();
		auto label_id= NONULL(run(label_in.identifier, scope));
		assert(label_id->type == AstNodeType::identifier);
		label_out->identifier= static_cast<IdentifierNode*>(label_id);
		return label_out;
	}

	AstNode* runSpecific(const CommentNode& comment_in, const TplScope& scope)
	{
		auto comment_out= output.newNode<CommentNode>();
		comment_out->text= comment_in.text;
		return comment_out;
	}

	AstNode* runSpecific(const TplTypeNode& tpl, const TplScope& scope)
	{
		assert(!scope.args.empty());
		// Block after tpl decl takes care of creating correct concrete type
		return nullptr;
	}

	template <AstNodeType nodeType, typename T>
	struct CondRun {
		static void eval(	Processor& self,
							const AstNode& in,
							AstNode*& out,
							const TplScope& s)
		{
			if (in.type == nodeType)
				out= self.runSpecific(static_cast<const T&>(in), s);
		}
	};

};

} // anonymous

AstContext runMetaprograms(const AstContext& input)
{
	Processor p{input};
	return p.run();
}

} // gamelang
