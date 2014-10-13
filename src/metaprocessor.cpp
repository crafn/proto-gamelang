#include "metaprocessor.hpp"
#include "ast.hpp"

#include <map>
#include <stack>

namespace gamelang
{
namespace
{

struct Processor {
	Processor(const AstContext& ctx): input(ctx) {}

	AstContext run()
	{
		const AstNode* root= &input.getRootNode();
		run(root);
		return std::move(output);
	}

private:
	const AstContext& input;
	AstContext output;
	/// Output nodes
	std::stack<AstNode*> nodeStack;
	std::map<const AstNode*, AstNode*> inToOutNode;

	template <typename T>
	AstNode* run(const T* node)
	{
		assert(node);
		return chooseRun(*node);
	}

	AstNode* chooseRun(const AstNode& node)
	{
		auto it= inToOutNode.find(&node);
		if (it != inToOutNode.end())
			return it->second; // This input-node has already been processed

		AstNode* ret= nullptr;
		CondRun<AstNodeType::global,        GlobalNode>::eval(*this, node, ret);
		CondRun<AstNodeType::endStatement,  EndStatementNode>::eval(*this, node, ret);
		CondRun<AstNodeType::identifier,    IdentifierNode>::eval(*this, node, ret);
		CondRun<AstNodeType::block,         BlockNode>::eval(*this, node, ret);
		CondRun<AstNodeType::varDecl,       VarDeclNode>::eval(*this, node, ret);
		CondRun<AstNodeType::funcType,      FuncTypeNode>::eval(*this, node, ret);
		CondRun<AstNodeType::structType,    StructTypeNode>::eval(*this, node, ret);
		CondRun<AstNodeType::numLiteral,    NumLiteralNode>::eval(*this, node, ret);
		CondRun<AstNodeType::stringLiteral, StringLiteralNode>::eval(*this, node, ret);
		CondRun<AstNodeType::nullLiteral,   NullLiteralNode>::eval(*this, node, ret);
		CondRun<AstNodeType::uOp,           UOpNode>::eval(*this, node, ret);
		CondRun<AstNodeType::biOp,          BiOpNode>::eval(*this, node, ret);
		CondRun<AstNodeType::ctrlStatement, CtrlStatementNode>::eval(*this, node, ret);
		CondRun<AstNodeType::call,          CallNode>::eval(*this, node, ret);
		CondRun<AstNodeType::label,         LabelNode>::eval(*this, node, ret);
		CondRun<AstNodeType::comment,       CommentNode>::eval(*this, node, ret);
		CondRun<AstNodeType::tplType,       TplTypeNode>::eval(*this, node, ret);
		if (ret) {
			inToOutNode[&node]= ret;
		}
		return ret;
	}

	AstNode* runSpecific(const GlobalNode& global_in)
	{
		auto global_out= output.newNode<GlobalNode>();
		nodeStack.push(global_out);
		for (auto&& node : global_in.nodes) {
			AstNode* result= run(node);
			if (result)
				global_out->nodes.emplace_back(result);
		}
		nodeStack.pop();
		return global_out;
	}

	AstNode* runSpecific(const EndStatementNode& end)
	{
		return output.newNode<EndStatementNode>();
	}

	AstNode* runSpecific(const IdentifierNode& id_in)
	{
		auto id_out= output.newNode<IdentifierNode>();
		id_out->name= id_in.name;	
	
		auto it= inToOutNode.find(id_in.boundTo);
		if (it != inToOutNode.end()) {
			id_out->boundTo= it->second;
		}
		return id_out;
	}

	AstNode* runSpecific(const BlockNode& block_in)
	{
		AstNode& parent= *NONULL(nodeStack.top());

		assert(!block_in.tplType && "Shouldn't be creating templates");

		auto block_out= output.newNode<BlockNode>();
		nodeStack.push(block_out);

		if (parent.type == AstNodeType::varDecl) {
			auto& parent_var= static_cast<VarDeclNode&>(parent);
			block_out->boundTo= parent_var.identifier;
		}

		block_out->loop= block_in.loop;
		block_out->external= block_in.external;

		for (auto&& node : block_in.nodes) {
			auto result= run(node);
			if (result)
				block_out->nodes.emplace_back(result);
		}
	
		if (block_in.structType)
			block_out->structType= NONULL(run(block_in.structType));
		if (block_in.funcType)
			block_out->funcType= NONULL(run(block_in.funcType));
		if (block_in.condition)
			block_out->condition= NONULL(run(block_in.condition));

		nodeStack.pop();
		return block_out;
	}

	AstNode* runSpecific(const VarDeclNode& var_in)
	{
		/// @todo use traceValue
		if (NONULL(var_in.valueType)->type == AstNodeType::tplType) {
			return nullptr;
		} 

		auto var_out= output.newNode<VarDeclNode>();
		nodeStack.push(var_out);

		var_out->constant= var_in.constant;
		var_out->param= var_in.param;

		auto id_node= NONULL(run(var_in.identifier));
		assert(id_node->type == AstNodeType::identifier);
		var_out->identifier= static_cast<IdentifierNode*>(id_node);
		var_out->identifier->boundTo= var_out;

		if (var_in.valueType->type != AstNodeType::structType) {
			var_out->valueType= NONULL(run(var_in.valueType));
			if (var_in.value)
				var_out->value= NONULL(run(var_in.value));
		} else { // Struct type queries stuff from struct; hence reverse run order
			if (var_in.value)
				var_out->value= NONULL(run(var_in.value));
			var_out->valueType= NONULL(run(var_in.valueType));
		}


		// Resolve decltype
		/*if (	var.valueType->type == AstNodeType::uOp &&
				static_cast<UOpNode*>(var.valueType)->opType
					== UOpType::declType) {
			/// @todo Resolving types and metaprograms probably need another pass
			auto op= static_cast<UOpNode*>(var.valueType);
			
			parseCheck(	NONULL(op->target)->type == AstNodeType::call,
						"Only deduction from call return type supported");
			auto call= static_cast<CallNode*>(op->target);

			// Identifier `Chicken` in ctor call `Chicken(10, 20)` is bound to
			// the declaration `let Chicken := struct {..}`
			assert(NONULL(call->func)->type == AstNodeType::identifier);
			auto func_id= static_cast<IdentifierNode*>(call->func);
			assert(NONULL(func_id->boundTo)->type == AstNodeType::varDecl);
			auto ret_type_decl= static_cast<VarDeclNode*>(func_id->boundTo);

			// Resolve valueType to the identifier of the struct type
			var.valueType= ret_type_decl->identifier;
		}*/

		nodeStack.pop();
		return var_out;
	}

	AstNode* runSpecific(const FuncTypeNode& func_in)
	{
		/// @todo tpl function
		auto func_out= output.newNode<FuncTypeNode>();
		nodeStack.push(func_out);

		for (auto&& node : func_in.params) {
			auto result= run(node);
			if (result) {
				assert(result->type == AstNodeType::varDecl);
				auto var= static_cast<VarDeclNode*>(result);
				func_out->params.emplace_back(var);
			}
		}
		func_out->returnType= NONULL(run(func_in.returnType));

		nodeStack.pop();
		return func_out;
	}

	AstNode* runSpecific(const StructTypeNode& st_type_in)
	{
		auto st_type_out= output.newNode<StructTypeNode>();
		for (auto&& node_in : st_type_in.varDecls) {
			auto node_out= inToOutNode[node_in];
			assert(node_out);
			assert(node_out->type == AstNodeType::varDecl);
			auto var= static_cast<VarDeclNode*>(node_out);
			st_type_out->varDecls.emplace_back(var);
		}
		return st_type_out;
	}

	AstNode* runSpecific(const NumLiteralNode& num_in)
	{
		auto num_out= output.newNode<NumLiteralNode>();
		num_out->value= num_in.value;
		return num_out;
	}

	AstNode* runSpecific(const StringLiteralNode& str_in)
	{
		auto str_out= output.newNode<StringLiteralNode>();
		str_out->str= str_in.str;
		return str_out;
	}

	AstNode* runSpecific(const NullLiteralNode& null_in)
	{
		return output.newNode<NullLiteralNode>();
	}

	AstNode* runSpecific(const UOpNode& op_in)
	{
		auto op_out= output.newNode<UOpNode>();
		op_out->opType= op_in.opType;
		op_out->target= NONULL(run(op_in.target));
		return op_out;
	}

	AstNode* runSpecific(const BiOpNode& op_in)
	{
		auto op_out= output.newNode<BiOpNode>();
		nodeStack.push(op_out);
		op_out->opType= op_in.opType;
		op_out->lhs= NONULL(run(op_in.lhs));
		op_out->rhs= NONULL(run(op_in.rhs));
		nodeStack.pop();
		return op_out;
	}

	AstNode* runSpecific(const CtrlStatementNode& ctrl_in)
	{
		auto ctrl_out= output.newNode<CtrlStatementNode>();
		nodeStack.push(ctrl_out);
		ctrl_out->statementType= ctrl_in.statementType;
		if (ctrl_in.value) {
			ctrl_out->value= NONULL(run(ctrl_in.value));
		}
		nodeStack.pop();
		return ctrl_out;
	}

	AstNode* runSpecific(const CallNode& call_in)
	{
		/// @todo Tpl call
		assert(!call_in.tplCall && "Shouldn't be creating templates");
		auto call_out= output.newNode<CallNode>();
		nodeStack.push(call_out);

		call_out->namedArgs= call_in.namedArgs;
		call_out->argRouting= call_in.argRouting;
		call_out->methodLike= call_in.methodLike;
		
		call_out->func= NONULL(run(call_in.func));
		for (auto&& arg : call_in.args) {
			call_out->args.emplace_back(NONULL(run(arg))); 
		}
		for (auto&& arg : call_in.implicitArgs) {
			call_out->implicitArgs.emplace_back(NONULL(run(arg))); 
		}
		nodeStack.pop();
		return call_out;
	}

	AstNode* runSpecific(const LabelNode& label_in)
	{
		auto label_out= output.newNode<LabelNode>();
		auto label_id= NONULL(run(label_in.identifier));
		assert(label_id->type == AstNodeType::identifier);
		label_out->identifier= static_cast<IdentifierNode*>(label_id);
		return label_out;
	}

	AstNode* runSpecific(const CommentNode& comment_in)
	{
		auto comment_out= output.newNode<CommentNode>();
		comment_out->text= comment_in.text;
		return comment_out;
	}

	AstNode* runSpecific(const TplTypeNode& tpe)
	{
		/// @todo
		for (auto&& node : tpe.params)
			run(node);
		return nullptr;
	}

	template <AstNodeType nodeType, typename T>
	struct CondRun {
		static void eval(Processor& self, const AstNode& in, AstNode*& out)
		{ if (in.type == nodeType) out= self.runSpecific(static_cast<const T&>(in)); }
	};

};

} // anonymous

AstContext runMetaprograms(const AstContext& input)
{
	Processor p{input};
	return p.run();
}

} // gamelang
