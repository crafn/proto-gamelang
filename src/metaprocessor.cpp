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
		run(root, newScope());
		return std::move(output);
	}

private:
	const AstContext& input;
	AstContext output;

	using BlockNodeIt= std::list<AstNode*>::iterator;
	struct TplArg {
		const IdentifierNode* id; // Points to tpl id in input ctx, e.g. `T`
		AstNode* value; // Points to node in output ctx, e.g. `^ConcreteType`

		bool operator<(const TplArg& other) const
		{
			auto v= &traceValue(*value);
			auto o_v= &traceValue(*other.value);
			/// @todo Value/type comparison should be done without mangling
			///       Can't use pointers though, because e.g. two `?char`
			///       nodes have equal contents but different addresses
			auto v_str= mangledName(*value);
			auto o_v_str= mangledName(*other.value);
			return	std::tie(id, v_str) <
					std::tie(other.id, o_v_str); }
	};
	struct TplScope {
		/// Given arguments to tpl struct/function
		std::vector<TplArg> args;

		bool operator<(const TplScope& other) const
		{ return std::tie(args) < std::tie(other.args); }

		std::string str() const
		{
			std::string ret;
			for (auto&& a : args) {
				ret += "_";
				ret += mangledName(*NONULL(a.value));
			}
			return ret;
		}
	};
	TplScope& newScope()
	{
		scopeStorage.emplace_back();
		return scopeStorage.back();
	}

	/// Output node creation stack
	std::stack<AstNode*> nodeStack;
	std::list<TplScope> scopeStorage;
	/// Maps input nodes to generated output nodes
	std::map<const AstNode*, std::map<TplScope, AstNode*>> inToOutNode;
	/// Maps output tpl block nodes to input tpl block nodes
	std::map<BlockNode*, const BlockNode*> outToInTpl;
	/// Used to insert template instantiations in the vicinity of original tpl decls
	std::map<const TplTypeNode*, BlockNodeIt> tplTypePlaces;
	/// Keeps track of instantiated templates
	std::set<std::tuple<const TplTypeNode*, AstNode*>> tplInstances;
	/// Non-instantiated templates and nodes depending on them
	/// These aren't going to make it to the ast even though they are output nodes
	std::set<const AstNode*> astExcluded;
	int astExclusionScope= 0;

	template <typename T>
	AstNode* run(const T* node, const TplScope& scope, bool caching= true)
	{
		assert(node);
		return chooseRun(*node, scope, caching);
	}

	AstNode* chooseRun(const AstNode& node, const TplScope& scope, bool caching)
	{
		if (caching) {
			auto* already_generated= findOutNodeOf(node, scope);
			if (already_generated) {
				return already_generated;
			}
		}

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
		if (ret && caching) {
			inToOutNode[&node][scope]= ret;
			if (astExclusionScope > 0)
				astExcluded.insert(ret);
		}
		return ret;
	}

	void markTplTypePlace(const TplTypeNode& tpl)
	{
		AstNode& root= output.getRootNode();
		assert(root.type == AstNodeType::global);
		auto& global= static_cast<GlobalNode&>(root);
		tplTypePlaces[&tpl]= global.nodes.end();
	}

	void tryInsertTplInstance(const TplTypeNode& tpl, AstNode& node)
	{
		auto&& tpl_inst_identity= 
			std::make_tuple<const TplTypeNode*, AstNode*>
				(&tpl, &node);
		if (	tplInstances.find(tpl_inst_identity) !=
				tplInstances.end())
			return;
		tplInstances.insert(tpl_inst_identity);

		auto place_it= tplTypePlaces.find(&tpl);
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
			if (out_it != node_map.end()) {
				assert(scope.args.size() == out_it->first.args.size());
				return out_it->second;
			}
			if (!scope.args.empty()) {
				// Check empty scope for non-templates,
				// because using e.g. `int` is desirable in tpl scope
				static TplScope empty;
				auto out_it= node_map.find(empty);
				if (out_it != node_map.end()) {
					if (!excludedFromAst(*out_it->second))
						return out_it->second;
				}
			}
		}
		return nullptr;
	}

	bool excludedFromAst(const AstNode& output_node) {
		return astExcluded.find(&output_node) != astExcluded.end();
	}

	AstNode* runSpecific(const GlobalNode& global_in, const TplScope& scope)
	{
		auto global_out= output.newNode<GlobalNode>();
		nodeStack.push(global_out);
		for (auto&& node : global_in.nodes) {
			AstNode* result= run(node, scope);
			if (result && !excludedFromAst(*result))
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
			/// @todo Pointer comparison
			if (arg.id->name == id_in.name) {
				return NONULL(arg.value);
			}
		}

		auto id_out= output.newNode<IdentifierNode>();
		id_out->name= id_in.name;
		
		if (id_in.boundTo) {
			AstNode* bound_out= findOutNodeOf(*NONULL(id_in.boundTo), scope);
			assert(bound_out);
			assert(bound_out->type != AstNodeType::identifier);
			id_out->boundTo= bound_out;
		} else {
			auto it= input.idDefs.find(id_in.name);
			parseCheck(it != input.idDefs.end(), "Unknown id: " + id_in.name);
			auto& defs= it->second;
			assert(!defs.empty());
			/// @todo Allow shadowing
			//parseCheck(defs.size() < 2, "Multiple ids with the same name: " + id_in.name);
			id_out->boundTo= findOutNodeOf(*defs.front().idNode->boundTo, scope);
			if (!id_out->boundTo) {
				for (auto&& m : defs)
					std::cout << "IDDD " << m.idNode << "\n";
			}
			assert(id_out->boundTo);
			assert(id_out->boundTo->type != AstNodeType::identifier);
		}
		return id_out;
	}

	AstNode* runSpecific(const BlockNode& block_in, const TplScope& scope)
	{
		AstNode& parent= *NONULL(nodeStack.top());
		
		bool uninstantiated_tpl= false;
		if (block_in.tplType && scope.args.empty())
			uninstantiated_tpl= true;

		auto block_out= output.newNode<BlockNode>();
		nodeStack.push(block_out);
		
		if (parent.type == AstNodeType::varDecl) {
			auto parent_var= static_cast<VarDeclNode*>(&parent);
			block_out->boundTo= parent_var->identifier;
			if (uninstantiated_tpl) {
				astExcluded.insert(parent_var);
				astExcluded.insert(parent_var->valueType);
			}
		}

		block_out->loop= block_in.loop;
		block_out->external= block_in.external;

		if (uninstantiated_tpl) {
			// This block is not a tpl instantiation
			outToInTpl[block_out]= &block_in;
			auto result= NONULL(run(block_in.tplType, scope));
			assert(result->type == AstNodeType::tplType);
			block_out->tplType= static_cast<TplTypeNode*>(result);

			astExcluded.insert(block_out);
			++astExclusionScope; // Everything inside is excluded also
		}

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

		if (block_in.destructor) {
			auto result= run(block_in.destructor, scope);
			assert(result && result->type == AstNodeType::block);
			block_out->destructor= static_cast<BlockNode*>(result);
		}

		if (uninstantiated_tpl)
			--astExclusionScope;

		nodeStack.pop();
		return block_out;
	}

	AstNode* runSpecific(const VarDeclNode& var_in, const TplScope& scope)
	{
		bool is_tpl_decl= NONULL(var_in.valueType)->type == AstNodeType::tplType;

		auto var_out= output.newNode<VarDeclNode>();
		nodeStack.push(var_out);

		var_out->constant= var_in.constant;
		var_out->param= var_in.param;
		var_out->identifier= output.newNode<IdentifierNode>();
		var_out->identifier->boundTo= var_out;
		if (is_tpl_decl && !scope.args.empty()) {
			// Tpl instance
			var_out->identifier->name=
				traceBoundId(var_in, BoundIdDist::furthest).name
					+ "__" + scope.str();
		} else {
			var_out->identifier->name= var_in.identifier->name;
		}
		inToOutNode[var_in.identifier][scope]= NONULL(var_out->identifier);

		var_out->valueType= run(var_in.valueType, scope); // Null on tpl instantiation
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
		
		auto result= NONULL(run(num_in.builtinDecl, scope));
		assert(result->type == AstNodeType::varDecl);
		num_out->builtinDecl= static_cast<VarDeclNode*>(result);

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
		if (op_in.opType == BiOpType::rightInsert) {
			parseCheck(	op_in.rhs->type == AstNodeType::call,
						".> must be followed by a call");
			// "Method" call
			CallNode call_copy= *static_cast<CallNode*>(op_in.rhs);
			call_copy.args.emplace(call_copy.args.begin(), op_in.lhs);
			call_copy.namedArgs.emplace(call_copy.namedArgs.begin(), "");
			return run(&call_copy, scope, false);
		}

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
		auto deduceTplArgs= []
		(	const std::vector<VarDeclNode*>& tpl_params_out,
			const std::vector<VarDeclNode*>& func_params_out,
			const std::vector<AstNode*>& func_args_out)
		-> std::vector<AstNode*>
		{
			assert(func_params_out.size() == func_args_out.size());

			std::vector<AstNode*> tpl_args;
			for (std::size_t i= 0; i < tpl_params_out.size(); ++i) {
				AstNode* deduced_type= nullptr;

				// Scan func_params_out until tpl_params_out[i] is found
				// Then search corresponding func_args_out for the concrete type
				/// @todo Deeper search, supporting stuff like Array[^Array[?T]]
				for (std::size_t k= 0; k < func_params_out.size(); ++k) {
					auto& func_p= func_params_out[k];
					if (func_p->valueType->type != AstNodeType::identifier)
						continue;
					auto& p_id= traceBoundId(*func_p->valueType, BoundIdDist::nearest);
					/// @todo Pointer check should be enough
					if ( p_id.name == tpl_params_out[i]->identifier->name) {
						deduced_type= &traceType(*NONULL(func_args_out[k]));
						break;
					}
				}

				parseCheck(deduced_type, "Couldn't deduce tpl argument");
				tpl_args.emplace_back(deduced_type);
			}
			assert(tpl_args.size() == tpl_params_out.size());
			return tpl_args;
		};

		auto instantiateTpl= [this]
		(	BlockNode& tpl_block_out,
			const std::vector<AstNode*>& tpl_args_out,
			const TplScope& scope)
		-> IdentifierNode*
		{
			auto& tpl_block_in= *NONULL(outToInTpl[&tpl_block_out]);
			auto& tpl_decl_in= *NONULL(NONULL(tpl_block_in.boundTo)->boundTo);
			auto& tpl_params_in= tpl_block_in.tplType->params;
			assert(tpl_params_in.size() == tpl_args_out.size());

			// Set up scope for template args
			TplScope& sub_scope= newScope();
			sub_scope.args.resize(tpl_args_out.size());

			assert(tpl_args_out.size() == tpl_params_in.size());
			for (std::size_t i= 0; i < tpl_args_out.size(); ++i) {
				sub_scope.args[i].id= tpl_params_in[i]->identifier;
				sub_scope.args[i].value= tpl_args_out[i];
			}

			auto tpl_inst= NONULL(run(&tpl_decl_in, sub_scope));
			assert(tpl_inst->type == AstNodeType::varDecl);
			auto tpl_inst_decl= static_cast<VarDeclNode*>(tpl_inst);

			assert(tpl_inst_decl->value->type == AstNodeType::block);
			assert(((BlockNode*)tpl_inst_decl->value)->tplType == nullptr);
			assert(tpl_inst_decl->valueType->type != AstNodeType::tplType);
			tryInsertTplInstance(*tpl_block_out.tplType, *tpl_inst_decl);

			return NONULL(tpl_inst_decl->identifier);
		};

		auto& traced_type= traceType(*run(call_in.func, scope));
		// `vector[int]` or `tplCall(123)` == `tplCall[int](123)`
		if (traced_type.type == AstNodeType::tplType) {
			AstNode& traced= traceValue(*NONULL(run(call_in.func, scope)));
			assert(traced.type == AstNodeType::block);
			auto& tpl_block_out= static_cast<BlockNode&>(traced);
			assert(tpl_block_out.tplType);
			auto& tpl_params= tpl_block_out.tplType->params;
			std::vector<AstNode*> call_args_out;
			for (auto&& arg : call_in.args)
				call_args_out.emplace_back(NONULL(run(arg, scope)));

			if (call_in.squareCall) { // `vector[int]`
				// Ordinary tpl call
				std::vector<AstNode*> implicit_tpl_args;
				std::vector<int> tpl_routing;
				routeCallArgs(	implicit_tpl_args,
								tpl_routing,
								*NONULL(run(call_in.func, scope)),
								call_in.namedArgs);
				auto tpl_args_out=
					resolveRouting(
							joined(call_args_out, implicit_tpl_args),
							tpl_routing);		
				return instantiateTpl(tpl_block_out, tpl_args_out, scope);
			} else { // `print(10)` == `print[int](10)`
				// Call includes implicit tpl call (tpl deduction)

				assert(tpl_block_out.funcType);
				auto& func_type_node= traceValue(*tpl_block_out.funcType);
				assert(func_type_node.type == AstNodeType::funcType);
				auto& func_type= static_cast<FuncTypeNode&>(func_type_node);

				std::vector<AstNode*> implicit_args;
				std::vector<int> routing;
				routeCallArgs(	implicit_args,
								routing,
								func_type,
								call_in.namedArgs);
				std::vector<AstNode*> all_args=
					resolveRouting(	joined(call_args_out, implicit_args),
									routing);

				auto tpl_args_out=
					deduceTplArgs(	tpl_params,
									listToVec(func_type.params),
									all_args);

				auto tpl_instance_id= instantiateTpl(tpl_block_out, tpl_args_out, scope);
				
				// Template has been created but not the actual fn call
				auto fn_call= output.newNode<CallNode>();
				fn_call->namedArgs= call_in.namedArgs;
				fn_call->func= tpl_instance_id;
				for (auto&& arg : call_in.args)
					fn_call->args.emplace_back(NONULL(run(arg, scope))); 
				routeCallArgs(	fn_call->implicitArgs,
								fn_call->argRouting,
								*fn_call->func,
								fn_call->namedArgs);
				for (auto&& arg : call_in.implicitArgs)
					fn_call->implicitArgs.emplace_back(NONULL(run(arg, scope))); 
				return fn_call;
			}
		} else if (call_in.squareCall) { // `indexing[2]`
			parseCheck(	call_in.args.size() == 1,
						"Indexing op [] should have exactly one argument");

			// Transform `arr[5]` to `*(arr + 5)`
			auto sum= output.newNode<BiOpNode>();
			sum->opType= BiOpType::add;
			sum->lhs= run(call_in.func, scope);
			sum->rhs= run(call_in.args.front(), scope);

			auto deref_op= output.newNode<UOpNode>();
			deref_op->opType= UOpType::deref;
			deref_op->target= sum;	
			return deref_op;
		} else { // `foo(bar)`
			assert(!call_in.squareCall);
			auto call_out= output.newNode<CallNode>();
			nodeStack.push(call_out);

			call_out->namedArgs= call_in.namedArgs;
			call_out->func= NONULL(run(call_in.func, scope));

			for (auto&& arg : call_in.args)
				call_out->args.emplace_back(NONULL(run(arg, scope))); 

			routeCallArgs(	call_out->implicitArgs,
							call_out->argRouting,
							*call_out->func,
							call_out->namedArgs);

			for (auto&& arg : call_in.implicitArgs)
				call_out->implicitArgs.emplace_back(NONULL(run(arg, scope))); 

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

	AstNode* runSpecific(const TplTypeNode& tpl_in, const TplScope& scope)
	{
		if (!scope.args.empty())
			return nullptr; // Tpl instance shouldn't have tpl type

		auto tpl_out= output.newNode<TplTypeNode>();
		for (auto&& p : tpl_in.params) {
			auto result= NONULL(run(p, scope));
			assert(result->type == AstNodeType::varDecl);
			tpl_out->params.emplace_back(static_cast<VarDeclNode*>(result));
		}
		markTplTypePlace(static_cast<TplTypeNode&>(*tpl_out));
		return tpl_out;
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
