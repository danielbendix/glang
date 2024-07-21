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
using llvm::cast;
using llvm::isa;
using llvm::dyn_cast;
using llvm::TypeSwitch;

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
    std::vector<AST::VariableDeclaration *> globals;
    PointerMap<AST::VariableDeclaration *, llvm::Value *> llvmGlobals;

    Context(LLVMContext& llvmContext, llvm::Module& llvmModule) : llvmContext{llvmContext}, llvmModule{llvmModule} {}
};

class ContextPopulator {
   
    Context& context;
public:

    ContextPopulator(Context& context) : context{context} {}

    void addGlobal(AST::VariableDeclaration& global) {
        context.globals.push_back(&global);
        // TODO: Add llvm global
    }

    void addFunction(AST::FunctionDeclaration& function) {
        context.functions.push_back(&function);

        auto functionType = function.getType();
        auto llvmFunctionType = functionType->getFunctionType(context.llvmContext);

        llvm::Function *llvmFunction = llvm::Function::Create(llvmFunctionType, llvm::Function::ExternalLinkage, function.getName(), context.llvmModule);
        context.llvmFunctions.insert(&function, llvmFunction);
    }
};

bool populateContext(Context& context, ModuleDef& moduleDefinition) {
    ContextPopulator populator{context};

    for (auto& global : moduleDefinition._globals) {
        populator.addGlobal(*global);
    }

    for (auto& function : moduleDefinition._functions) {
        populator.addFunction(*function);
    }
    
    return false;
}

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

    void addConstant(AST::Binding binding, llvm::Value *value) {
        // TODO: Destructure bindings
        locals.push_back(Local{&binding, value, currentScope});
    }

    void pushBinding(AST::Binding& binding, llvm::Value *value) {
        locals.push_back(Local{&binding, value, currentScope});
    }

    llvm::AllocaInst& addVariable(AST::Binding& binding) {
        // TODO: Destructure bindings
        llvm::Type *type = binding.getType()->getLLVMType(context.llvmContext);
        auto& alloca = makeStackSlot(type);
        locals.push_back(Local{&binding, &alloca, currentScope});
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

    llvm::Value *getVariable(const AST::Node *node) {
        for (int i = locals.size() - 1; i >= 0; --i) {
            if (locals[i].node == node) {
                return locals[i].value;
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

    llvm::Type *getLLVMType(Type& type) {
        return type.getLLVMType(context.llvmContext);
    }

    llvm::ConstantInt *getIntegerConstant(IntegerType *type, uint64_t value) {
        return llvm::ConstantInt::get(type->getIntegerType(context.llvmContext), value, type->getIsSigned());
    }

    llvm::ConstantInt *getIntegerConstant(int bitWidth, uint64_t value) {
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
                    auto alloca = function.getVariable(&static_cast<LocalResolution *>(identifier->getResolution())->getBinding());
                    return alloca;
                }
                case IdentifierResolution::IRK_Global: {
                    assert(false && "TODO");
                }
                case IdentifierResolution::IRK_Parameter:
                case IdentifierResolution::IRK_Function:
                case IdentifierResolution::IRK_Type:
                    assert(false);
            }
        }
        if (auto *memberAccess = dyn_cast<AST::MemberAccessExpression>(&expression)) {
            llvm::Value *target = getAssignmentTarget(memberAccess->getTarget());
            switch (memberAccess->getResolution().getKind()) {
                case MemberResolution::MRK_Struct_Field: {
                    auto targetType = memberAccess->getTarget().getType();
                    return function.builder.CreateConstGEP2_32(function.getLLVMType(targetType), target, 0, static_cast<const StructFieldResolution&>(memberAccess->getResolution()).getIndex());
                }
                case MemberResolution::MRK_Struct_Method:
                    llvm_unreachable("Invalid assignment target");
            }
        }
        if (auto *unaryExpression = dyn_cast<AST::UnaryExpression>(&expression)) {
            if (unaryExpression->getOp() == AST::UnaryOperator::Dereference) {
                return unaryExpression->getTarget().acceptVisitor(*this);
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

    void finalize() {
        if (!function.currentBlock().getTerminator()) {
            function.builder.CreateRetVoid();
            // TODO: Assert that function returns void.
        }
    }

    bool startCodeGen(AST::FunctionDeclaration& function) {
        functionDeclaration = &function;
        auto& block = function.getCode();
        for (auto& declaration : block) {
            declaration.acceptVisitor(*this);
        }
        finalize();
        return error;
    }

    // Declaration

    void visitVariableDeclaration(AST::VariableDeclaration& variable) {
        auto& alloca = function.addVariable(variable.getBinding());
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
        auto value = assignment.getValue().acceptVisitor(*this);
        auto *target = getAssignmentTarget(assignment.getTarget());
        auto *alloca = static_cast<llvm::AllocaInst *>(target);
        function.builder.CreateStore(value, alloca, false);
    }

    void visitCompoundAssignmentStatement(AST::CompoundAssignmentStatement& assignment) {
        AST::BinaryOperator op = assignment.getOp();

        llvm::Value *value;
        auto *targetValue = assignment.getTarget().acceptVisitor(*this);
        auto *rhs = assignment.getOperand().acceptVisitor(*this);
        using enum AST::BinaryOperator;
        switch (op) {
            case Add:
                value = function.builder.CreateAdd(targetValue, rhs); break;
            case Subtract:
                value = function.builder.CreateSub(targetValue, rhs); break;
            case Multiply:
                value = function.builder.CreateMul(targetValue, rhs); break;
            case Divide: {
                IntegerType *integerType = cast<IntegerType>(assignment.getTarget().getType());
                if (integerType->getIsSigned()) {
                    value = function.builder.CreateSDiv(targetValue, rhs);
                } else {
                    value = function.builder.CreateUDiv(targetValue, rhs);
                }
                break;
            }
            case Modulo: {
                IntegerType *integerType = cast<IntegerType>(assignment.getTarget().getType());
                if (integerType->getIsSigned()) {
                    value = function.builder.CreateSRem(targetValue, rhs);
                } else {
                    value = function.builder.CreateURem(targetValue, rhs);
                }
                break;
            }
            case BitwiseAnd:
                value = function.builder.CreateAnd(targetValue, rhs); break;
            case BitwiseOr:
                value = function.builder.CreateOr(targetValue, rhs); break;
            case BitwiseXor:
                value = function.builder.CreateXor(targetValue, rhs); break;
            case ShiftLeft:
                value = function.builder.CreateShl(targetValue, rhs); break;
            case ShiftRight:
                // TODO: Add arithmetic shift for signed types.
                value = function.builder.CreateLShr(targetValue, rhs); break;
            case Equal:
            case NotEqual:
            case Less:
            case LessEqual:
            case Greater:
            case GreaterEqual:
            case LogicalAnd:
            case LogicalOr:
                llvm_unreachable("Unsupported assignment operator type.");
        }

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
        auto& header = function.createOrphanedBlock();
        auto& end = function.createOrphanedBlock();
        auto& loop = function.createOrphanedBlock();

        function.builder.CreateBr(&header);
        
        function.patchOrphanedBlock(header);
        auto *condition = whileStatement.getCondition().acceptVisitor(*this);

        function.builder.CreateCondBr(condition, &loop, &end);

        function.patchOrphanedBlock(loop);
        visitBlock(whileStatement.getBlock());

        function.builder.CreateBr(&header);

        function.patchOrphanedBlock(end);
    }

    void visitForStatement(AST::ForStatement& forStatement) {
        auto iterable = forStatement.getIterable().acceptVisitor(*this);
        auto iterableType = forStatement.getIterable().getType();
        if (auto arrayType = dyn_cast<ArrayType>(iterableType)) {
            auto elementType = function.getLLVMType(arrayType->getContained());

            auto& preheader = function.currentBlock();
            auto& header = function.createOrphanedBlock();
            auto& end = function.createOrphanedBlock();
            auto& loop = function.createOrphanedBlock();

            auto iterable = forStatement.getIterable().acceptVisitor(*this);

            auto initialPointer = function.builder.CreateExtractValue(iterable, {0});
            auto size = function.builder.CreateExtractValue(iterable, 1);

            auto firstAfterEnd = function.builder.CreateGEP(elementType, initialPointer, {size});

            function.builder.CreateBr(&header);

            function.patchOrphanedBlock(header);

            initialPointer->getType();
            auto currentPointer = function.builder.CreatePHI(initialPointer->getType(), 2);
            currentPointer->addIncoming(initialPointer, &preheader);

            auto ptrDiff = function.builder.CreatePtrDiff(elementType, currentPointer, firstAfterEnd);
            auto isAtEnd = function.builder.CreateICmpEQ(ptrDiff, function.getIntegerConstant(64, 0));
            function.builder.CreateCondBr(isAtEnd, &end, &loop);


            function.patchOrphanedBlock(loop);

            function.pushBinding(forStatement.getBinding(), currentPointer);
            visitBlock(forStatement.getBlock());

            auto& latch = function.currentBlock();
            auto incrementedPointer = function.builder.CreateConstGEP1_64(elementType, currentPointer, 1);

            currentPointer->addIncoming(incrementedPointer, &latch);
            function.builder.CreateBr(&header);

            function.patchOrphanedBlock(end);


        } else if (auto rangeType = dyn_cast<RangeType>(iterableType)) {
            assert(false);
        } else {
            llvm_unreachable("[PROGRAMMER ERROR]: Iterating over value that is not array or range.");
        }
        
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
                auto *binding = &local->getBinding();

                auto alloca = function.getVariable(binding);
                return function.builder.CreateLoad(type, alloca);
            }
            case IdentifierResolution::IRK_Global:
            case IdentifierResolution::IRK_Type:
                assert(false);
        }

        return nullptr;
    }

    llvm::Value *visitLiteral(AST::Literal& literal) {
        using enum AST::Literal::Type;
        switch (literal.getLiteralType()) {
            case Integer: {
                // FIXME: FIXME: FIXME: allow different number types.
                auto integerType = cast<IntegerType>(literal.getType());
                auto bitWidth = integerType->bitWidth;
                llvm::APInt i;
                if (integerType->isSigned) {
                    i = literal.getInteger().sextOrTrunc(bitWidth);
                } else {
                    i = literal.getInteger().zextOrTrunc(bitWidth);
                }
                return llvm::Constant::getIntegerValue(function.getLLVMType(literal.getType()), std::move(i));
            }
            case Double: {
                auto type = cast<FPType>(literal.getType());
                auto llvmType = function.getLLVMType(type);
                return llvm::ConstantFP::get(llvmType, literal.getDouble());
            }
            case Boolean: {
                llvm::APInt i(1, literal.getBoolean() ? 1 : 0, true);
                return function.getIntegerConstant(1, literal.getBoolean() ? 1 : 0);
                return llvm::Constant::getIntegerValue(function.getLLVMType(literal.getType()), i);
            }
            case Nil: {
                auto optionalType = cast<OptionalType>(literal.getType());
                if (isa<PointerType>(optionalType->getContained())) {
                    return llvm::Constant::getNullValue(function.getLLVMType(optionalType));
                } else {
                    auto structType = cast<llvm::StructType>(function.getLLVMType(optionalType));
                    return llvm::ConstantStruct::get(structType, {function.getIntegerConstant(1, 0), llvm::UndefValue::get(function.getLLVMType(optionalType->getContained()))});
                }
            }
            case String: assert(false);
        }
        return nullptr;
    }

    llvm::Value *visitUnaryExpression(AST::UnaryExpression& unary) {
        auto target = unary.getTarget().acceptVisitor(*this);

        using enum AST::UnaryOperator;
        switch (unary.getOp()) {
            case Not:
                return function.builder.CreateSelect(target, function.getIntegerConstant(1, 0), function.getIntegerConstant(1, 1));
            case Negate: return TypeSwitch<Type *, llvm::Value *>(unary.getType())
                .Case([&](IntegerType *integerType) {
                    return function.builder.CreateNeg(target);
                })
                .Case([&](FPType *fpType) {
                    return function.builder.CreateFNeg(target);
                });

            case BitwiseNegate: {
                auto& integerType = cast<IntegerType>(*unary.getType());
                return function.builder.CreateXor(target, function.getIntegerConstant(integerType.bitWidth, -1));
            }

            case AddressOf: {
                return getAssignmentTarget(unary.getTarget());
            }
            case Dereference: {
                Type *dereferencedType = unary.getType();
                return function.builder.CreateLoad(function.getLLVMType(dereferencedType), target);
            }
            case SignExtend: {
                auto& integerType = cast<IntegerType>(*unary.getType());
                return function.builder.CreateSExt(target, function.getLLVMType(integerType));
            }
            case ZeroExtend: {
                auto& integerType = cast<IntegerType>(*unary.getType());
                return function.builder.CreateZExt(target, function.getLLVMType(integerType));
            }
            case FPExtend: {
                auto& fpType = cast<FPType>(*unary.getType());
                return function.builder.CreateFPExt(target, function.getLLVMType(fpType));
            }
            case IntegerToFP: {
                auto& fpType = cast<FPType>(*unary.getType());
                auto& integerType = cast<IntegerType>(*unary.getTarget().getType());
                if (integerType.isSigned) {
                    return function.builder.CreateSIToFP(target, function.getLLVMType(fpType));
                } else {
                    return function.builder.CreateUIToFP(target, function.getLLVMType(fpType));
                }
            }
            case OptionalWrap: {
                auto optionalType = cast<OptionalType>(unary.getType());

                if (isa<PointerType>(optionalType->getContained())) {
                    return target;
                } else {
                    // FIXME: Distinguish between optionals that contain pointers, and optionals that do not.
                    auto structType = cast<llvm::StructType>(function.getLLVMType(optionalType));
                    auto constant = llvm::ConstantStruct::get(structType, {function.getIntegerConstant(1, 1), llvm::UndefValue::get(function.getLLVMType(optionalType->getContained()))});
                    return function.builder.CreateInsertValue(constant, target, {1});
                }
            }
        }

        assert(false);
        return nullptr;
    }

    llvm::Value *visitBinaryExpression(AST::BinaryExpression& binary) {
        auto left = binary.getLeft().acceptVisitor(*this);
        auto right = binary.getRight().acceptVisitor(*this);

        // TODO: Support FP math
        using enum AST::BinaryOperator;
        switch (binary.getOp()) {
            case Add: return TypeSwitch<Type *, llvm::Value *>(binary.getType())
                .Case([&](IntegerType *_) {
                    return function.builder.CreateAdd(left, right);
                })
                .Case([&](FPType *_) {
                    return function.builder.CreateFAdd(left, right);
                });

            case Subtract: return TypeSwitch<Type *, llvm::Value *>(binary.getType())
                .Case([&](IntegerType *_) {
                    return function.builder.CreateSub(left, right);
                })
                .Case([&](FPType *_) {
                    return function.builder.CreateFSub(left, right);
                });

            case Multiply: return TypeSwitch<Type *, llvm::Value *>(binary.getType())
                .Case([&](IntegerType *_) {
                    return function.builder.CreateMul(left, right);
                })
                .Case([&](FPType *_) {
                    return function.builder.CreateFMul(left, right);
                });

            case Divide: return TypeSwitch<Type *, llvm::Value *>(binary.getType())
                .Case([&](IntegerType *integerType) {
                    if (integerType->isSigned) {
                        return function.builder.CreateSDiv(left, right);
                    } else {
                        return function.builder.CreateUDiv(left, right);
                    }
                })
                .Case([&](FPType *_) {
                    return function.builder.CreateFDiv(left, right);
                });

            case Modulo: {
                IntegerType *integerType = cast<IntegerType>(binary.getType());
                if (integerType->isSigned) {
                    return function.builder.CreateSRem(left, right);
                } else {
                    return function.builder.CreateURem(left, right);
                }
                break;
            }

            case BitwiseAnd: return function.builder.CreateAnd(left, right);
            case BitwiseOr: return function.builder.CreateOr(left, right);
            case BitwiseXor: return function.builder.CreateXor(left, right);

            case ShiftLeft: return function.builder.CreateShl(left, right);
            case ShiftRight: {
                auto integerType = cast<IntegerType>(binary.getType());
                if (integerType->isSigned) {
                    return function.builder.CreateAShr(left, right);
                } else {
                    return function.builder.CreateLShr(left, right);
                }
            }

            case Equal: return TypeSwitch<Type *, llvm::Value *>(binary.getLeft().getType())
                .Case([&](IntegerType *integerType) {
                    return function.builder.CreateICmpEQ(left, right);
                })
                .Case([&](FPType *_) {
                    return function.builder.CreateFCmpOEQ(left, right);
                })
                .Case([&](BooleanType *_) {
                    return function.builder.CreateICmpEQ(left, right);
                });

            case NotEqual: return TypeSwitch<Type *, llvm::Value *>(binary.getLeft().getType())
                .Case([&](IntegerType *integerType) {
                    return function.builder.CreateICmpNE(left, right);
                    if (integerType->isSigned) {
                    } else {
                    }
                })
                .Case([&](FPType *_) {
                    return function.builder.CreateFCmpUNE(left, right);
                })
                .Case([&](BooleanType *_) {
                    return function.builder.CreateICmpNE(left, right);
                });

            case Less: return TypeSwitch<Type *, llvm::Value *>(binary.getLeft().getType())
                .Case([&](IntegerType *integerType) {
                    if (integerType->isSigned) {
                        return function.builder.CreateICmpSLT(left, right);
                    } else {
                        return function.builder.CreateICmpULT(left, right);
                    }
                })
                .Case([&](FPType *_) {
                    return function.builder.CreateFCmpOLT(left, right);
                })
                .Case([&](BooleanType *_) {
                    return function.builder.CreateICmpULT(left, right);
                });

            case LessEqual: return TypeSwitch<Type *, llvm::Value *>(binary.getLeft().getType())
                .Case([&](IntegerType *integerType) {
                    if (integerType->isSigned) {
                        return function.builder.CreateICmpSLE(left, right);
                    } else {
                        return function.builder.CreateICmpULE(left, right);
                    }
                })
                .Case([&](FPType *_) {
                    return function.builder.CreateFCmpOLE(left, right);
                })
                .Case([&](BooleanType *_) {
                    return function.builder.CreateICmpULE(left, right);
                });

            case Greater: return TypeSwitch<Type *, llvm::Value *>(binary.getLeft().getType())
                .Case([&](IntegerType *integerType) {
                    if (integerType->isSigned) {
                        return function.builder.CreateICmpSGT(left, right);
                    } else {
                        return function.builder.CreateICmpUGT(left, right);
                    }
                })
                .Case([&](FPType *_) {
                    return function.builder.CreateFCmpOGT(left, right);
                })
                .Case([&](BooleanType *_) {
                    return function.builder.CreateICmpUGT(left, right);
                });

            case GreaterEqual: return TypeSwitch<Type *, llvm::Value *>(binary.getLeft().getType())
                .Case([&](IntegerType *integerType) {
                    if (integerType->isSigned) {
                        return function.builder.CreateICmpSGE(left, right);
                    } else {
                        return function.builder.CreateICmpUGE(left, right);
                    }
                })
                .Case([&](FPType *_) {
                    return function.builder.CreateFCmpOGE(left, right);
                })
                .Case([&](BooleanType *_) {
                    return function.builder.CreateICmpUGT(left, right);
                });

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

    llvm::Value *visitSubscriptExpression(AST::SubscriptExpression& subscript) {
        auto& arrayType = cast<ArrayType>(*subscript.getTarget().getType());
        auto elementType = function.getLLVMType(arrayType.getContained());

        auto target = subscript.getTarget().acceptVisitor(*this);
        auto index = subscript.getIndex().acceptVisitor(*this);

        auto base = function.builder.CreateExtractValue(target, {0});

        if (arrayType.isBounded) {
            // TODO: Allow boundary checking to be toggled.
            auto size = function.builder.CreateExtractValue(target, {1});

            assert(false && "TODO: Implement array bounds checking.");
        } else {
            auto address = function.builder.CreateGEP(elementType, base, {index});
            return function.builder.CreateLoad(elementType, address);
        }
    }

    llvm::Value *visitInitializerExpression(AST::InitializerExpression& initializer) {
        auto structType = cast<StructType>(initializer.getType());
        llvm::Value *structValue = llvm::UndefValue::get(function.getLLVMType(structType));

        for (size_t i = 0; i < initializer.getNumberOfPairs(); ++i) {
            auto& pair = initializer.getPair(i);

            auto fieldResolution = cast<StructFieldResolution>(pair.first->getResolution());
            auto value = pair.second->acceptVisitor(*this);
            unsigned index = fieldResolution.getIndex();

            structValue = function.builder.CreateInsertValue(structValue, value, {index});
        }

        return structValue;
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

    llvm::Value *visitInferredMemberAccessExpression(AST::InferredMemberAccessExpression& inferredMemberAccess) {
        // This can only be a static member access or enum case
        
        auto& resolution = inferredMemberAccess.getResolution();

        if (auto *enumCaseResolution = llvm::dyn_cast<EnumCaseResolution>(&resolution)) {

            


        }

        // TODO: Implement static members

        assert(false);
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

llvm::LLVMContext llvmContext;

#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>

std::unique_ptr<llvm::Module> generateCode(ModuleDef& moduleDefinition)
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // Get the target triple for the host machine
    std::string targetTriple = llvm::sys::getDefaultTargetTriple();

    auto module = std::make_unique<llvm::Module>("test", llvmContext);
    module->setTargetTriple(targetTriple);

    Context context{llvmContext, *module};

    populateContext(context, moduleDefinition);

    performCodegen(context);

    llvm::outs() << *module;

    if (llvm::verifyModule(*module, &llvm::outs())) {
        module.reset();
        return module;
    }

    return module;
}
