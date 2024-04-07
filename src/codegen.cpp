#include "AST.h"
#include "AST_Visitor.h"
#include "type.h"

#include "llvm/ADT/StringSet.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"

#include "llvm/IR/Verifier.h"

using llvm::LLVMContext;

struct Local {
    std::string const *identifier;
    llvm::AllocaInst *alloca;
    Type *type;
    int depth;

    Local() {}
    Local(const std::string& identifier, llvm::AllocaInst& alloca, Type *type, int depth) 
        : identifier{&identifier}
        , alloca{&alloca}
        , type{type}
        , depth{depth} {}
};

IntegerType *defaultType;

void instantiateDefaultType(llvm::LLVMContext& context) 
{
    defaultType = new IntegerType(64, true, llvm::Type::getInt64Ty(context));
}

class Context {
public:
    llvm::Module& llvmModule;
    LLVMContext& llvmContext;
    //llvm::StringMap<AST::Declaration *> globals;

    std::vector<AST::FunctionDeclaration *> functions;
    llvm::DenseMap<AST::FunctionDeclaration *, llvm::Function *> llvmFunctions;
    std::vector<AST::VariableDeclaration *> variables;
    llvm::DenseMap<AST::VariableDeclaration *, llvm::Value *> llvmVariables;

    Context(LLVMContext& llvmContext, llvm::Module& llvmModule) : llvmContext{llvmContext}, llvmModule{llvmModule} {}
};

class ContextPopulator : public AST::DeclarationVisitorT<ContextPopulator, void> {
   
    Context& context;
public:

    ContextPopulator(Context& context) : context{context} {}

    void addDeclaration(AST::Declaration& declaration) {
        declaration.acceptVisitor(*this);
    }

    void visitVariableDeclaration(AST::VariableDeclaration& variable) {
        context.variables.push_back(&variable);
        //context.llvmVariables.insert(std::make_pair(&variable, 
    }

    void visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        context.functions.push_back(&function);

        llvm::Type *returnType = function.getReturnType()->getLLVMType(context.llvmContext);

        llvm::Type *parameters[function.getParameterCount()];

        for (int pi = 0; pi < function.getParameterCount(); pi++) {
            parameters[pi] = function.getParameter(pi).type->getLLVMType(context.llvmContext);
        }

        llvm::FunctionType *functionType = llvm::FunctionType::get(returnType, llvm::ArrayRef(parameters, function.getParameterCount()), false);

        llvm::Function *llvmFunction = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, function.getName(), context.llvmModule);
        context.llvmFunctions.insert(std::make_pair(&function, llvmFunction));
    }

    void visitStructDeclaration(AST::StructDeclaration& structDeclaration) {

    }

    void visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {

    }

    void visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {

    }

    void visitStatementDeclaration(AST::StatementDeclaration& statement) {

    }
};

bool populateContext(Context& context, std::vector<AST::unique_ptr<AST::Declaration>>& declarations) {
    ContextPopulator populator{context};

    for (auto& declaration : declarations) {
        populator.addDeclaration(*declaration);
    }
    
    return false;
}

//using namespace llvm;

class CompilingFunction {

    struct Local {
        AST::Node *node;
        llvm::AllocaInst *alloca;
        int depth;
        Local(AST::Node *node, llvm::AllocaInst *alloca, int depth) : node{node}, alloca{alloca}, depth{depth} {}
    };

    Context& context;
    llvm::Function& function;
    llvm::AllocaInst *lastAlloca = nullptr;
    llvm::BasicBlock *entry;

    llvm::IRBuilder<> allocaBuilder;

    std::vector<Local> locals;
    int currentScope = 1;
public:

    llvm::IRBuilder<> builder;

    CompilingFunction(Context& context, llvm::Function& function) 
        : context{context}
        , function{function}
        , builder{function.getContext()}
        , allocaBuilder{function.getContext()}
        , entry{llvm::BasicBlock::Create(context.llvmContext, "entry", &function)} 
    {
        allocaBuilder.SetInsertPoint(entry);
        builder.SetInsertPoint(entry, entry->end());
    }

    void pushScope() {
        currentScope += 1;
    }

    void popScope() {
        for (int i = locals.size() - 1; i >= 0; --i) {
            if (locals[i].depth == currentScope) {
                locals.pop_back();
            }
        }
        currentScope -= 1;
    }

    llvm::AllocaInst& addLocal(AST::VariableDeclaration& variable) {
        llvm::Type *type = variable.getType()->getLLVMType(context.llvmContext);
        auto& alloca = makeStackSlot(type);
        locals.push_back(Local(&variable, &alloca, currentScope));
        return alloca;
    }

    llvm::AllocaInst *getLocal(AST::Node *node) {
        for (int i = locals.size() - 1; i >= 0; --i) {
            if (locals[i].node == node) {
                return locals[i].alloca;
            }
        }
        return nullptr;
    }

    llvm::AllocaInst& makeStackSlot(llvm::Type *type) {
        allocaBuilder.SetInsertPointPastAllocas(&function);
        auto alloca = allocaBuilder.CreateAlloca(type, nullptr);
        return *alloca;
    }

    llvm::Argument& getArgument(unsigned i) {
        return *function.getArg(i);
    }

    llvm::Function& getFunction(AST::FunctionDeclaration& function) {
        return *context.llvmFunctions.at(&function);
    }

    llvm::BasicBlock& createBlock() {
        llvm::BasicBlock *block = llvm::BasicBlock::Create(context.llvmContext, "", &function);
        builder.SetInsertPoint(block);
        return *block;
    }

    llvm::BasicBlock& createOrphanedBlock() {
        llvm::BasicBlock *block = llvm::BasicBlock::Create(context.llvmContext);
        return *block;
    }

    llvm::BasicBlock& patchOrphanedBlock(llvm::BasicBlock& block) {
        function.insert(function.end(), &block);
        builder.SetInsertPoint(&block);
        return block;
    }

    llvm::BasicBlock& currentBlock() {
        return *builder.GetInsertBlock();
    }

    llvm::Type *getLLVMType(Type *type) {
        return type->getLLVMType(context.llvmContext);
    }
};


class FunctionCodeGenerator : public AST::DeclarationVisitorT<FunctionCodeGenerator, void>
                            , public AST::StatementVisitorT<FunctionCodeGenerator, void>
                            , public AST::ExpressionVisitorT<FunctionCodeGenerator, llvm::Value *> 
{
    CompilingFunction function;
    AST::FunctionDeclaration *functionDeclaration;
    bool error = false;
public:
    FunctionCodeGenerator(llvm::Function& function, Context& context) : function{context, function} {}
    
    
    llvm::Value *getAssignmentTarget(AST::Expression& expression) {
        if (auto *identifier = llvm::dyn_cast<AST::Identifier>(&expression)) {
            switch (identifier->getResolution()->getKind()) {
                case IdentifierResolution::IRK_Local: {
                    auto alloca = function.getLocal(&static_cast<LocalResolution *>(identifier->getResolution())->getVariableDeclaration());
                    return alloca;
                }
                case IdentifierResolution::IRK_Global: {
                    assert(false);
                }
                case IdentifierResolution::IRK_Parameter:
                case IdentifierResolution::IRK_Function:
                    assert(false);
            }
        }
        return nullptr;
    }

    void visitBlock(const AST::Block& block) {
        function.pushScope();
        for (int bi = 0; bi < block.size(); bi++) {
            block[bi].acceptVisitor(*this);
        }
        function.popScope();
    }

    bool startCodeGen(AST::FunctionDeclaration& function) {
        functionDeclaration = &function;
        auto& block = function.getCode();
        for (auto& declaration : block) {
            declaration.acceptVisitor(*this);
        }
        return error;
    }

    // Declaration

    void visitVariableDeclaration(AST::VariableDeclaration& variable) {
        auto& alloca = function.addLocal(variable);
        if (auto *initial = variable.getInitialValue()) {
            auto *value = initial->acceptVisitor(*this);
            function.builder.CreateStore(value, &alloca);
        }
    }

    void visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        assert(false);
    }

    void visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        assert(false);
        // Store type as struct type.
    }

    void visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        assert(false);
        // Store type as enum type.
    }

    void visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        assert(false);
        // Store type as protocol type.
    }

    void visitStatementDeclaration(AST::StatementDeclaration& statement) {
        statement.getStatement().acceptVisitor(*this);
    }

    // Statements

    void visitAssignmentStatement(AST::AssignmentStatement& assignment) {
        AST::AssignmentOperator op = assignment.getOp();
        using enum AST::AssignmentOperator;

        // TODO: Must be assignable.
        llvm::Value *value;
        if (op == Assign) {
            value = assignment.getValue().acceptVisitor(*this);
        } else {
            auto *targetValue = assignment.getTarget().acceptVisitor(*this);
            auto *rhs = assignment.getValue().acceptVisitor(*this);
            
            switch (op) {
                case AssignAdd:
                    value = function.builder.CreateAdd(targetValue, rhs); break;
                case AssignSub:
                    value = function.builder.CreateSub(targetValue, rhs); break;
                case AssignMultiply:
                    value = function.builder.CreateMul(targetValue, rhs); break;
                case AssignDivide:
                    // TODO: Fix signedness
                    value = function.builder.CreateSDiv(targetValue, rhs); break;
                case Assign: llvm_unreachable("Assign is handled elsewhere.");
            }
        }


        // FIXME: The codegen needs to know that this is an lvalue.
        auto *target = getAssignmentTarget(assignment.getTarget());
        auto *alloca = static_cast<llvm::AllocaInst *>(target);
        function.builder.CreateStore(value, alloca, false);
    }

    void visitIfStatement(AST::IfStatement& ifStatement) {
        llvm::BasicBlock& end = function.createOrphanedBlock();
        llvm::BasicBlock *next = nullptr;
        const int blockCount = ifStatement.getConditionCount() + (ifStatement.getFallback() ? 0 : -1);
        for (int i = 0; i < ifStatement.getConditionCount(); ++i) {
            llvm::BasicBlock& block = next ? function.patchOrphanedBlock(*next) : function.currentBlock();

            auto& branch = ifStatement.getCondition(i);
            auto condition = branch.getCondition().acceptVisitor(*this);

            if (i == blockCount) {
                next = &end;
            } else {
                next = &function.createOrphanedBlock();
            }
            llvm::BasicBlock& ifEntry = function.createOrphanedBlock();

            function.builder.CreateCondBr(condition, &ifEntry, next);

            function.patchOrphanedBlock(ifEntry);
           
            visitBlock(branch.getBlock());
            if (!function.currentBlock().getTerminator()) {
                function.builder.CreateBr(&end);
            }
        }

        if (auto fallback = ifStatement.getFallback()) {
            function.patchOrphanedBlock(*next);
            visitBlock(*fallback);
            if (!function.currentBlock().getTerminator()) {
                function.builder.CreateBr(&end);
            }
        }

        if (end.hasNUsesOrMore(1)) {
            function.patchOrphanedBlock(end);
        } else {
            delete &end;
        }
    }

    void visitReturnStatement(AST::ReturnStatement& returnStatement) {
        if (auto* value = returnStatement.getValue()) {
            auto *returnValue = value->acceptVisitor(*this);
            function.builder.CreateRet(returnValue);
        } else {
            function.builder.CreateRetVoid();
        }
    }

    void visitWhileStatement(AST::WhileStatement& whileStatement) {
        auto& loop = function.createOrphanedBlock();
        function.builder.CreateBr(&loop);
        function.patchOrphanedBlock(loop);
        visitBlock(whileStatement.getBlock());

        auto *condition = whileStatement.getCondition().acceptVisitor(*this);
        auto& loopEnd = function.createOrphanedBlock();
        function.builder.CreateCondBr(condition, &loop, &loopEnd);
        function.patchOrphanedBlock(loopEnd);
    }

    void visitForStatement(AST::ForStatement& forStatement) {
        assert(false);
        // TODO: Implement this.
    }

    void visitExpressionStatement(AST::ExpressionStatement& expression) {
        expression.getExpression().acceptVisitor(*this);
    }

    // Expressions

    llvm::Value *visitIdentifier(AST::Identifier& identifier) {
        switch (identifier.getResolution()->getKind()) {
            case IdentifierResolution::IRK_Parameter: return &function.getArgument(static_cast<FunctionParameterResolution *>(identifier.getResolution())->getParameterIndex());
            case IdentifierResolution::IRK_Function: return &function.getFunction(*static_cast<FunctionResolution *>(identifier.getResolution())->getFunctionDeclaration());
            case IdentifierResolution::IRK_Local: {
                auto type = function.getLLVMType(identifier.getType());
                auto alloca = function.getLocal(&static_cast<LocalResolution *>(identifier.getResolution())->getVariableDeclaration());
                return function.builder.CreateLoad(type, alloca);
            }
            case IdentifierResolution::IRK_Global: {
                assert(false);
            }
        }

        return nullptr;
    }

    llvm::Value *visitLiteral(AST::Literal& literal) {
        using enum AST::Literal::Type;
        switch (literal.getLiteralType()) {
            case Integer: {
                llvm::APInt i(64, literal.getInteger(), true);
                return llvm::Constant::getIntegerValue(function.getLLVMType(literal.getType()), i);
            }
            case Boolean: {
                llvm::APInt i(1, literal.getBoolean() ? 1 : 0, true);
                return llvm::Constant::getIntegerValue(function.getLLVMType(literal.getType()), i);
            }
            default:
                assert(false);
        }
        return nullptr;
    }

    llvm::Value *visitUnaryExpression(AST::UnaryExpression& unary) {
        assert(false);
        return nullptr;
    }

    llvm::Value *visitBinaryExpression(AST::BinaryExpression& binary) {
        auto left = binary.getLeft().acceptVisitor(*this);
        auto right = binary.getRight().acceptVisitor(*this);

        using enum AST::BinaryOperator;
        switch (binary.getOp()) {
            case Add: return function.builder.CreateAdd(left, right);
            case Subtract: return function.builder.CreateSub(left, right);
            case Multiply: return function.builder.CreateMul(left, right);

            // TODO: Fix signedness
            case Divide: return function.builder.CreateSDiv(left, right);
            case Modulo: return function.builder.CreateSRem(left, right);
            case Equal: return function.builder.CreateICmpEQ(left, right);
            case NotEqual: return function.builder.CreateICmpNE(left, right);
            case Less: return function.builder.CreateICmpSLT(left, right);
            case LessEqual: return function.builder.CreateICmpSLE(left, right);
            case Greater: return function.builder.CreateICmpSGT(left, right);
            case GreaterEqual: return function.builder.CreateICmpSGE(left, right);

            case LogicalAnd: assert(false);
            case LogicalOr: assert(false);
        }


        assert(false);
        return nullptr;
    }

    llvm::Value *visitCallExpression(AST::CallExpression& call) {
        auto callee = call.getTarget().acceptVisitor(*this);
        llvm::Function *llvmFunction = llvm::cast<llvm::Function>(callee);
        llvmFunction->getFunctionType();
        if (call.argumentCount()) {
            llvm::Value *arguments[call.argumentCount()];
            for (int i = 0; i < call.argumentCount(); ++i) {
                arguments[i] = call.getArgument(i).acceptVisitor(*this);
            }
            return function.builder.CreateCall(llvmFunction->getFunctionType(), llvmFunction, llvm::ArrayRef(arguments, call.argumentCount()));
        } else {
            return function.builder.CreateCall(llvmFunction->getFunctionType(), llvmFunction);
        }
    }
};

bool codeGenFunction(AST::FunctionDeclaration& function, llvm::Function& llvmFunction, Context& context) {
    FunctionCodeGenerator generator{llvmFunction, context};
    return generator.startCodeGen(function);
}

class GlobalCodeGenerator : public AST::DeclarationVisitorT<GlobalCodeGenerator, void> {

    Context& context;

public:
    GlobalCodeGenerator(Context& context) : context{context} {}

    void visitVariableDeclaration(AST::VariableDeclaration& variableDeclaration) {
        // TODO: Generate global variable.
        assert(false);
    }

    void visitFunctionDeclaration(AST::FunctionDeclaration& functionDeclaration) {
        llvm::Function *llvmFunction = context.llvmFunctions.at(&functionDeclaration);
        codeGenFunction(functionDeclaration, *context.llvmFunctions.at(&functionDeclaration), context);
    }

    void visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        assert(false);
    }

    void visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        assert(false);
    }

    void visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        assert(false);
    }

    void visitStatementDeclaration(AST::StatementDeclaration& statement) {
        assert(false);
    }
};

bool performCodegen(Context& context) {
    GlobalCodeGenerator generator{context};
    for (auto function : context.functions) {
        function->acceptVisitor(generator);
    }
    return false;
}

class DeclarationCodeGen : public AST::DeclarationVisitorT<DeclarationCodeGen, llvm::Value *> {

};

class StatementCodeGen : public AST::StatementVisitorT<StatementCodeGen, llvm::Value *> {

};

llvm::FunctionType *createFunctionType(AST::FunctionDeclaration& functionDeclaration)
{
    llvm::Type *parameterTypes[functionDeclaration.getParameterCount()];

    for (int i = 0; i < functionDeclaration.getParameterCount(); i++) {
        parameterTypes[i] = defaultType->type;
    }

    auto a = llvm::ArrayRef(parameterTypes, functionDeclaration.getParameterCount());
    return llvm::FunctionType::get(defaultType->type, a, false);

}

std::unique_ptr<llvm::Module> generateCode(std::vector<AST::unique_ptr<AST::Declaration>>& declarations)
{
    llvm::LLVMContext llvmContext;
    instantiateDefaultType(llvmContext);
    
    auto module = std::make_unique<llvm::Module>("test", llvmContext);

    Context context{llvmContext, *module};

    populateContext(context, declarations);

    performCodegen(context);

    llvm::verifyModule(*module, &llvm::outs());

    llvm::outs() << *module;

    std::ignore = module.release();

    return module;
}
