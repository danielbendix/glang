#include "codegen.h"
#include "AST.h"
#include "AST_Visitor.h"
#include "type.h"
#include "containers/pointer_map.h"

#include "llvm/ADT/TypeSwitch.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"

#include "llvm/IR/Verifier.h"

using llvm::LLVMContext;
using llvm::dyn_cast;

using Result = PassResult;
using enum PassResultKind;

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

class Context {
public:
    llvm::Module& llvmModule;
    LLVMContext& llvmContext;
    //llvm::StringMap<AST::Declaration *> globals;

    std::vector<AST::FunctionDeclaration *> functions;
    PointerMap<AST::FunctionDeclaration *, llvm::Function *> llvmFunctions;
    std::vector<AST::VariableDeclaration *> variables;
    PointerMap<AST::VariableDeclaration *, llvm::Value *> llvmVariables;

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
        context.llvmFunctions.insert(&function, llvmFunction);
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

bool populateContext(Context& context, ModuleDef& moduleDefinition) {
    ContextPopulator populator{context};

    for (auto& declaration : moduleDefinition.functions) {
        populator.addDeclaration(*declaration);
    }
    
    return false;
}

//using namespace llvm;

class CompilingFunction {

    struct Local {
        AST::Node *node;
        llvm::Value *value;
        int depth;
        Local(AST::Node *node, llvm::Value *value, int depth) : node{node}, value{value}, depth{depth} {}
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

    void addConstant(AST::VariableDeclaration& variable, llvm::Value *value) {
        locals.push_back(Local(&variable, value, currentScope));
    }

    llvm::AllocaInst& addVariable(AST::VariableDeclaration& variable) {
        llvm::Type *type = variable.getType()->getLLVMType(context.llvmContext);
        auto& alloca = makeStackSlot(type);
        locals.push_back(Local(&variable, &alloca, currentScope));
        return alloca;
    }

    llvm::Value *getConstant(const AST::Node *node) {
        for (int i = locals.size() - 1; i >= 0; --i) {
            if (locals[i].node == node) {
                return locals[i].value;
            }
        }
        llvm_unreachable("Unable to find local.");
    }

    llvm::AllocaInst *getVariable(const AST::Node *node) {
        for (int i = locals.size() - 1; i >= 0; --i) {
            if (locals[i].node == node) {
                return llvm::cast<llvm::AllocaInst>(locals[i].value);
            }
        }
        llvm_unreachable("Unable to find local.");
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
        return *context.llvmFunctions[&function];
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

    llvm::ConstantInt *getIntegerConstant(int bitWidth, int value) {
        auto type = llvm::IntegerType::get(context.llvmContext, bitWidth);
        return llvm::ConstantInt::get(type, value, false);
    }
};


class FunctionCodeGenerator : public AST::DeclarationVisitorT<FunctionCodeGenerator, void>
                            , public AST::StatementVisitorT<FunctionCodeGenerator, void>
                            , public AST::ExpressionVisitorT<FunctionCodeGenerator, llvm::Value *> 
{
    CompilingFunction function;
    AST::FunctionDeclaration *functionDeclaration;
    Result result = OK;
    bool error = false;
public:
    FunctionCodeGenerator(llvm::Function& function, Context& context) : function{context, function} {}
    
    llvm::Value *getAssignmentTarget(AST::Expression& expression) {
        if (auto *identifier = llvm::dyn_cast<AST::Identifier>(&expression)) {
            switch (identifier->getResolution()->getKind()) {
                case IdentifierResolution::IRK_Local: {
                    LocalResolution *local = static_cast<LocalResolution *>(identifier->getResolution());
                    assert(local->getVariableDeclaration().getIsMutable());
                    auto alloca = function.getVariable(&static_cast<LocalResolution *>(identifier->getResolution())->getVariableDeclaration());
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
        if (auto *memberAccess = dyn_cast<AST::MemberAccessExpression>(&expression)) {
            llvm::Value *target = getAssignmentTarget(memberAccess->getTarget());
            switch (memberAccess->getResolution().getKind()) {
                case MemberResolution::MRK_Struct_Field:
                    return function.builder.CreateConstGEP2_64(function.getLLVMType(memberAccess->getType()), target, 0, static_cast<const StructFieldResolution&>(memberAccess->getResolution()).getIndex());
                case MemberResolution::MRK_Struct_Method:
                    llvm_unreachable("Invalid assignment target");
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
        if (variable.getIsMutable()) {
            auto& alloca = function.addVariable(variable);
            if (auto *initial = variable.getInitialValue()) {
                auto *value = initial->acceptVisitor(*this);
                function.builder.CreateStore(value, &alloca);
            }
        } else {
            llvm::Value *value;
            if (auto *initial = variable.getInitialValue()) {
                value = initial->acceptVisitor(*this);
            } else {
                llvm::Type *type = function.getLLVMType(variable.getType());
                value = llvm::PoisonValue::get(type);
            }
            function.addConstant(variable, value);
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

    void visitGuardStatement(AST::GuardStatement& guardStatement) {
        auto *condition = guardStatement.getCondition().acceptVisitor(*this);


        llvm::BasicBlock& entry = function.createOrphanedBlock();
        llvm::BasicBlock& next = function.createOrphanedBlock();

        function.builder.CreateCondBr(condition, &next, &entry);

        function.patchOrphanedBlock(entry);

        visitBlock(guardStatement.getBlock());

        function.patchOrphanedBlock(next);
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
                LocalResolution *local = static_cast<LocalResolution *>(identifier.getResolution());
                auto *declaration = &local->getVariableDeclaration();

                if (local->getVariableDeclaration().getIsMutable()) {
                    auto alloca = function.getVariable(declaration);
                    return function.builder.CreateLoad(type, alloca);
                } else {
                    return function.getConstant(declaration);
                }
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

            case BitwiseAnd: return function.builder.CreateAnd(left, right);
            case BitwiseOr: return function.builder.CreateOr(left, right);
            case BitwiseXor: return function.builder.CreateXor(left, right);

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

    llvm::Value *visitMemberAccessExpression(AST::MemberAccessExpression& memberAccess) {
        auto target = memberAccess.getTarget().acceptVisitor(*this);
        auto targetType = memberAccess.getTarget().getType();

        auto& resolution = memberAccess.getResolution();

        llvm::Value *value = llvm::TypeSwitch<const MemberResolution *, llvm::Value *>(&resolution)
            .Case<StructFieldResolution>([&](auto field) {
                int index = field->getIndex();

//                llvm::Value *indices[2];
//                indices[0] = function.getIntegerConstant(64, 0);
//                indices[1] = function.getIntegerConstant(64, index);

//                auto gep = function.builder.CreateGEP(function.getLLVMType(targetType), target, {indices, 2});
//                return function.builder.CreateLoad(function.getLLVMType(memberAccess.getType()), gep);
                return function.builder.CreateExtractValue(target, {(unsigned int) index});
            })
        ;

        return value;
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
        llvm::Function *llvmFunction = context.llvmFunctions[&functionDeclaration];
        codeGenFunction(functionDeclaration, *context.llvmFunctions[&functionDeclaration], context);
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

Result performCodegen(Context& context) {
    GlobalCodeGenerator generator{context};

    for (auto function : context.functions) {
        function->acceptVisitor(generator);
    }

    return OK;
}

std::unique_ptr<llvm::Module> generateCode(ModuleDef& moduleDefinition)
{
    llvm::LLVMContext llvmContext;
    
    auto module = std::make_unique<llvm::Module>("test", llvmContext);

    Context context{llvmContext, *module};

    populateContext(context, moduleDefinition);

    performCodegen(context);

    llvm::verifyModule(*module, &llvm::outs());

    llvm::outs() << *module;

    std::ignore = module.release();

    return module;
}
