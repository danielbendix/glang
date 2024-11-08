#include "codegen.h"
#include "AST.h"
#include "AST_Visitor.h"
#include "type.h"
#include "type/visitor.h"
#include "containers/pointer_map.h"
#include "containers/bitmap.h"

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
using llvm::enumerate;

using Result = PassResult;
using enum PassResultKind;

/// A wrapper to represent the different types of LLVM values dealt with during codegen.
class Value {
public:
    enum class Kind : unsigned {
        Value,
        Memory,
        COUNT,
    };

private:
    static constexpr unsigned BITS = 1;
    static_assert(unsigned(Kind::COUNT) <= 1 << BITS);
    llvm::PointerIntPair<llvm::Value *, BITS> _value;

    Value(llvm::Value *value, unsigned kind) : _value{value, kind} {
        assert(kind < (1 << BITS));
    }
public:
    static Value value(llvm::Value *value) {
        return Value(value, unsigned(Kind::Value));
    }

    static Value memory(llvm::Value *value) {
        return Value(value, unsigned(Kind::Memory));
    }

    llvm::Value *get() const {
        return _value.getPointer();
    }

    Kind getKind() const {
        return Kind{_value.getInt()};
    }

    bool isMemory() const {
        return getKind() == Kind::Memory;
    }
};

class Context {
public:
    llvm::Module& llvmModule;
    LLVMContext& llvmContext;
    //llvm::StringMap<AST::Declaration *> globals;

    std::vector<AST::FunctionDeclaration *> functions;
    PointerMap<AST::FunctionDeclaration *, llvm::Function *> llvmFunctions;
    std::vector<AST::VariableDeclaration *> globals;
    PointerMap<AST::IdentifierBinding *, llvm::GlobalVariable *> llvmGlobals;

    PointerMap<Type *, llvm::Function *> printFunctions;

    Context(LLVMContext& llvmContext, llvm::Module& llvmModule) : llvmContext{llvmContext}, llvmModule{llvmModule} {}

    void addGlobal(AST::VariableDeclaration& global) {
        globals.push_back(&global);

        auto& binding = cast<AST::IdentifierBinding>(global.getBinding());

        auto type = binding.getType()->getLLVMType(llvmContext);
        auto name = binding.getIdentifier().string_view();
        llvm::GlobalVariable *llvmGlobal = new llvm::GlobalVariable(
            llvmModule, 
            type, 
            false, 
            llvm::GlobalValue::ExternalLinkage, 
            llvm::UndefValue::get(type), name
        );
        
        llvmGlobals.insert(&binding, llvmGlobal);
    }

    llvm::Function *setupGlobalConstructor() {
        llvm::IRBuilder<> builder{llvmContext};
        llvm::ConstantInt *priority = builder.getInt32(65536);

        llvm::FunctionType *initializerType = llvm::FunctionType::get(llvm::Type::getVoidTy(llvmContext), false);
        // TODO: Add module name to initializer
        llvm::Function *initializer = llvm::Function::Create(initializerType, llvm::Function::InternalLinkage, "glang$initialize_globals", llvmModule);

        llvm::Type *int32Type = builder.getInt32Ty();
        llvm::Type *pointerType = builder.getPtrTy();

        auto globalConstructorType = llvm::StructType::get(int32Type, pointerType, pointerType);
        auto arrayType = llvm::ArrayType::get(globalConstructorType, 1);
        auto globalConstructor = llvm::ConstantStruct::get(globalConstructorType, {priority, initializer, llvm::Constant::getNullValue(pointerType)});
        llvm::Constant *globalConstructorArray = llvm::ConstantArray::get(arrayType, {globalConstructor});

        llvm::GlobalVariable *ctor = new llvm::GlobalVariable(
            llvmModule,
            arrayType,
            true,
            llvm::GlobalValue::AppendingLinkage,
            globalConstructorArray,
            "llvm.global_ctors"
        );
        return initializer;
    }

    void addFunction(AST::FunctionDeclaration& function) {
        functions.push_back(&function);

        auto functionType = function.getType();
        auto llvmFunctionType = functionType->getFunctionType(llvmContext);

        llvm::Function *llvmFunction = llvm::Function::Create(llvmFunctionType, llvm::Function::ExternalLinkage, function.getName().string_view(), llvmModule);
        llvmFunctions.insert(&function, llvmFunction);
    }

    llvm::Function *createPrintFunction(Type& type);

    llvm::Function *getPrintFunction(Type& type) {
        if (auto function = printFunctions.lookup(&type)) {
            return *function;
        }
        
        auto function = createPrintFunction(type);
        printFunctions.insert(&type, function);
        return function;
    }

};

bool populateContext(Context& context, ModuleDef& moduleDefinition) {
    for (auto global : moduleDefinition.globals) {
        context.addGlobal(*global);
    }

    for (auto function : moduleDefinition.functions) {
        context.addFunction(*function);
    }
    
    return false;
}

class CompilingFunction {

    using Binding = llvm::PointerIntPair<llvm::Value *, 1>;

    struct Local {
        AST::Node *node;
        Binding value;
        int depth;
        Local(AST::Node *node, Binding value, int depth) : node{node}, value{value}, depth{depth} {}
    };

    Context& context;
    llvm::Function& function;
    llvm::AllocaInst *lastAlloca = nullptr;
    llvm::BasicBlock *entry;

    llvm::IRBuilder<> allocaBuilder;

    std::vector<Local> locals;
    std::vector<llvm::BasicBlock *> trapBlocks;
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

    void addConstant(AST::Binding& binding, llvm::Value *value) {
        // TODO: Destructure bindings
        locals.push_back(Local{&binding, {value, 1}, currentScope});
    }

    void pushBinding(AST::Binding& binding, llvm::Value *value) {
        locals.push_back(Local{&binding, {value, 0}, currentScope});
    }

    void pushValueBinding(AST::Binding& binding, llvm::Value *value) {
        locals.push_back(Local{&binding, {value, 1}, currentScope});
    }

    void pushAddressBinding(AST::Binding& binding, llvm::Value *address) {
        locals.push_back(Local{&binding, {address, 0}, currentScope});
    }

    llvm::AllocaInst& addVariable(AST::Binding& binding) {
        // TODO: Destructure bindings
        llvm::Type *type = binding.getType()->getLLVMType(context.llvmContext);
        auto& alloca = makeStackSlot(type);
        locals.push_back(Local{&binding, {&alloca, 0}, currentScope});
        return alloca;
    }

    Binding getConstant(const AST::Node *node) {
        for (int i = locals.size() - 1; i >= 0; --i) {
            if (locals[i].node == node) {
                return locals[i].value;
            }
        }
        llvm_unreachable("Unable to find local.");
    }

    Binding getVariable(const AST::Node *node) {
        for (int i = locals.size() - 1; i >= 0; --i) {
            if (locals[i].node == node) {
                return locals[i].value;
            }
        }
        llvm_unreachable("Unable to find local.");
    }

    Binding getGlobal(AST::IdentifierBinding& binding) {
        return {context.llvmGlobals[&binding], 0};
    }

    Binding getGlobal(AST::IdentifierBinding *binding) {
        return {context.llvmGlobals[binding], 0};
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

    llvm::BasicBlock& createTrapBlock() {
        auto& block = createOrphanedBlock();
        trapBlocks.push_back(&block);
        return block;
    }

    void patchTrapBlocks() {
        for (auto trapBlock : trapBlocks) {
            patchOrphanedBlock(*trapBlock);
            llvm::Function *trap = llvm::Intrinsic::getDeclaration(&context.llvmModule, llvm::Intrinsic::trap);
            builder.CreateCall(trap);
            builder.CreateUnreachable();
        }
    }

    llvm::Value *createEqualsNil(llvm::Value *optionalValue, OptionalType& optionalType) {
        if (auto pointerType = dyn_cast<PointerType>(optionalType.getContained())) {
            auto null = llvm::Constant::getNullValue(getLLVMType(pointerType));
            return builder.CreateICmpEQ(optionalValue, null);
        } else {
            auto flag = builder.CreateExtractValue(optionalValue, {0});
            auto zero = getIntegerConstant(1, 0);
            return builder.CreateICmpEQ(flag, zero);
        }
    }

    llvm::Value *createExtractSome(llvm::Value *optionalValue, OptionalType& optionalType) {
        if (isa<PointerType>(optionalType.getContained())) {
            return optionalValue;
        } else {
            return builder.CreateExtractValue(optionalValue, {1});
        }
    }

    llvm::Type *getLLVMType(Type *type) {
        return type->getLLVMType(context.llvmContext);
    }

    llvm::Type *getLLVMType(Type& type) {
        return type.getLLVMType(context.llvmContext);
    }

    llvm::Type *getIntegerType(unsigned bitWidth) {
        return llvm::IntegerType::get(context.llvmContext, bitWidth);
    }

    llvm::Type *getFPType(FPType::Precision precision) {
        switch (precision) {
            case FPType::Precision::Single:
                return llvm::Type::getFloatTy(context.llvmContext);
            case FPType::Precision::Double:
                return llvm::Type::getDoubleTy(context.llvmContext);
        }
    }

    llvm::ConstantInt *getIntegerConstant(IntegerType *type, uint64_t value) {
        return llvm::ConstantInt::get(cast<llvm::IntegerType>(type->getLLVMType(context.llvmContext)), value, type->isSigned);
    }

    llvm::ConstantInt *getIntegerConstant(int bitWidth, uint64_t value) {
        auto type = llvm::IntegerType::get(context.llvmContext, bitWidth);
        return llvm::ConstantInt::get(type, value, false);
    }

    llvm::FunctionCallee getPrintFunction(Type& type) {
        auto function = context.getPrintFunction(type);
        return llvm::FunctionCallee{function->getFunctionType(), function};
    }

    llvm::FunctionCallee getPrintf() {
        llvm::FunctionType *printfType = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(context.llvmContext), 
            llvm::PointerType::get(llvm::Type::getInt8Ty(context.llvmContext), 0), 
            true
        );
        return context.llvmModule.getOrInsertFunction("printf", printfType);
    }

    llvm::FunctionCallee getPutchar() {
        llvm::FunctionType *putcharType = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(context.llvmContext), 
            llvm::Type::getInt32Ty(context.llvmContext),
            false
        );
        return context.llvmModule.getOrInsertFunction("putchar", putcharType);
    }
};

llvm::Function *Context::createPrintFunction(Type& type) {
    auto functionType = llvm::FunctionType::get(llvm::IntegerType::get(llvmContext, 32), {type.getLLVMType(llvmContext)}, false);
    // TODO: Escape characters in the name.
    auto llvmFunction = llvm::Function::Create(functionType, llvm::Function::InternalLinkage, "print$" + type.makeName(), llvmModule);

    llvm::Value *value = llvmFunction->getArg(0);
    CompilingFunction function{*this, *llvmFunction};

    auto printf = function.getPrintf();

    auto returnValue = TypeVisitor::visit(type, overloaded {
        [&](IntegerType& type) -> llvm::Value * {
            bool isLong;
            switch (type.bitWidth) {
                case 64:
                    isLong = true;
                    break;
                case 32:
                    isLong = false;
                    break;
                default:
                    isLong = false;
                    auto int32Type = function.getIntegerType(32);
                    if (type.isSigned) {
                        value = function.builder.CreateSExt(value, int32Type);
                    } else {
                        value = function.builder.CreateZExt(value, int32Type);
                    }
                    break;
            }
            char format[5];
            format[0] = '%';
            char formatChar = type.isSigned ? 'd' : 'u';
            if (isLong) {
                format[1] = 'l';
                format[2] = formatChar;
                format[3] = '\0';
            } else {
                format[1] = formatChar;
                format[2] = '\0';
            }

            auto string = function.builder.CreateGlobalStringPtr(format);
            return function.builder.CreateCall(printf, {string, value});
        },
        [&](FPType& type) -> llvm::Value * {
            switch (type.precision) {
                case FPType::Precision::Single:
                    value = function.builder.CreateFPExt(value, function.getFPType(FPType::Precision::Double));
                    break;
                case FPType::Precision::Double:
                    break;
            }
            auto string = function.builder.CreateGlobalStringPtr("%f");
            return function.builder.CreateCall(printf, {string, value});
        },
        [&](BooleanType& _) -> llvm::Value * {
            auto trueString = function.builder.CreateGlobalStringPtr("true");
            auto falseString = function.builder.CreateGlobalStringPtr("false");
            value = function.builder.CreateSelect(value, trueString, falseString);
            return function.builder.CreateCall(printf, {value});
        },
        [&](PointerType& type) -> llvm::Value * {
            auto string = function.builder.CreateGlobalStringPtr("%p");
            return function.builder.CreateCall(printf, {string, value});
        },
        [&](StructType& type) -> llvm::Value * {
            auto& fields = type.getFields();

            if (fields.empty()) {
                auto string = function.builder.CreateGlobalStringPtr(type.makeName() + " {}");
                return function.builder.CreateCall(printf, {string});
            }

            auto prefix = type.makeName();
            prefix += " { ";
            auto prefixString = function.builder.CreateGlobalStringPtr(prefix);
            function.builder.CreateCall(printf, {prefixString});

            bool needsSeparator = false;

            for (auto [i, field] : enumerate(type.getFields())) {
                std::string leader;
                if (needsSeparator) {
                    leader += ", ";
                } else {
                    needsSeparator = true;
                }

                auto& binding = cast<AST::IdentifierBinding>(field->getBinding());
                leader += binding.getIdentifier();
                leader += " = ";

                auto leaderString = function.builder.CreateGlobalStringPtr(leader);
                function.builder.CreateCall(printf, {leaderString});
                auto printFunction = function.getPrintFunction(*field->getType());
                auto element = function.builder.CreateExtractValue(value, {(unsigned int)i});
                function.builder.CreateCall(printFunction, {element});
            }

            auto suffixString = function.builder.CreateGlobalStringPtr(" }");
            return function.builder.CreateCall(printf, {suffixString});
        },
        [&](OptionalType& type) -> llvm::Value * {
            auto isNil = function.createEqualsNil(value, type);

            auto& onNil = function.createOrphanedBlock();
            auto& onSome = function.createOrphanedBlock();
            auto& end = function.createOrphanedBlock();
            function.builder.CreateCondBr(isNil, &onNil, &onSome);

            function.patchOrphanedBlock(onNil);
            auto nilString = function.builder.CreateGlobalStringPtr("nil");
            auto nilCall = function.builder.CreateCall(printf, {nilString});
            function.builder.CreateBr(&end);

            function.patchOrphanedBlock(onSome);
            auto someValue = function.createExtractSome(value, type);
            auto printFunction = getPrintFunction(*type.getContained());
            auto functionType = llvm::FunctionType::get(function.getIntegerType(32), {function.getLLVMType(type.getContained())}, false);
            auto someCall = function.builder.CreateCall(functionType, printFunction, {someValue});
            function.builder.CreateBr(&end);

            function.patchOrphanedBlock(end);
            auto phi = function.builder.CreatePHI(nilCall->getType(), 2);
            phi->addIncoming(nilCall, &onNil);
            phi->addIncoming(someCall, &onSome);

            return phi;
        },
        [&](auto& other) -> llvm::Value * {
            llvm_unreachable("Unsupported type.");
        }
    });

    function.builder.CreateRet(returnValue);

    return llvmFunction;
}

class FunctionCodeGenerator : public AST::DeclarationVisitorT<FunctionCodeGenerator, void>
                            , public AST::StatementVisitorT<FunctionCodeGenerator, void>
                            , public AST::ExpressionVisitorT<FunctionCodeGenerator, Value>
{
    struct LoopInfo {
        llvm::BasicBlock& continueBlock;
        llvm::BasicBlock& endBlock;

        LoopInfo(llvm::BasicBlock& continueBlock, llvm::BasicBlock& endBlock)
            : continueBlock{continueBlock}
            , endBlock{endBlock}
        {}
    };

    CompilingFunction function;
    AST::FunctionDeclaration *functionDeclaration;
    Result result = OK;

    LoopInfo *currentLoop = nullptr;

    template <typename Func>
    __attribute__((always_inline))
    void withLoop(LoopInfo&& loop, Func&& f) {
        LoopInfo loop_ = loop;
        LoopInfo *previous = currentLoop;
        currentLoop = &loop_;
        std::invoke(f);
        currentLoop = previous;
    }

    llvm::Value *convertToValueIfNecessary(Type *type, Value value) {
        switch (value.getKind()) {
            case Value::Kind::Value:
                return value.get();
            case Value::Kind::Memory:
                return function.builder.CreateLoad(function.getLLVMType(type), value.get());
            case Value::Kind::COUNT:
                llvm_unreachable("[PROGRAMMER ERROR]: Value::Kind::COUNT should never be used.");
        }
    }

    Value visitExpression(AST::Expression& expression) {
        return expression.acceptVisitor(*this);
    }

    Value visitExpression(AST::Expression *expression) {
        return expression->acceptVisitor(*this);
    }

    llvm::Value *visitExpressionAsValue(AST::Expression& expression) {
        auto value = visitExpression(expression);
        return convertToValueIfNecessary(expression.getType(), value);
    }

    llvm::Value *visitExpressionAsValue(AST::Expression *expression) {
        return visitExpressionAsValue(*expression);
    }

public:
    FunctionCodeGenerator(llvm::Function& function, Context& context) : function{context, function} {}
    
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
        function.patchTrapBlocks();
    }

    void generateFunctionCode(AST::FunctionDeclaration& function) {
        functionDeclaration = &function;
        auto& block = function.getCode();
        for (auto& declaration : block) {
            declaration.acceptVisitor(*this);
        }
        finalize();
    }

    void generateGlobalCode(AST::VariableDeclaration& global) {
        auto& binding = cast<AST::IdentifierBinding>(global.getBinding());
        auto value = visitExpressionAsValue(global.getInitialValue());
        auto llvmGlobal = function.getGlobal(binding).getPointer();
        function.builder.CreateStore(value, llvmGlobal);
        function.builder.CreateRetVoid();
    }

    // Declaration

    void visitVariableDeclaration(AST::VariableDeclaration& variable) {
        auto& alloca = function.addVariable(variable.getBinding());
        if (auto *initial = variable.getInitialValue()) {
            auto *value = visitExpressionAsValue(initial);
            function.builder.CreateStore(value, &alloca);
        }
    }

    void visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        llvm_unreachable("Nested function.");
    }

    void visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        llvm_unreachable("Nested struct type.");
    }

    void visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {
        llvm_unreachable("Nested enum type.");
    }

    void visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {
        llvm_unreachable("Nested protocol type.");
    }

    void visitStatementDeclaration(AST::StatementDeclaration& statement) {
        statement.getStatement().acceptVisitor(*this);
    }

    // Statements

    void visitAssignmentStatement(AST::AssignmentStatement& assignment) {
        auto value = visitExpressionAsValue(assignment.getValue());
        auto target = visitExpression(assignment.getTarget());
        assert(target.getKind() == Value::Kind::Memory);
        function.builder.CreateStore(value, target.get(), false);
    }

    void visitCompoundAssignmentStatement(AST::CompoundAssignmentStatement& assignment) {
        AST::BinaryOperator op = assignment.getOp();

        llvm::Value *value;
        auto target = visitExpression(assignment.getTarget());
        auto targetValue = convertToValueIfNecessary(assignment.getTarget().getType(), target);
        auto rhs = visitExpressionAsValue(assignment.getOperand());
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
            case OpenRange:
            case ClosedRange:
                llvm_unreachable("Unsupported assignment operator type.");
        }

        assert(target.isMemory());
        auto *alloca = static_cast<llvm::AllocaInst *>(target.get());
        function.builder.CreateStore(value, alloca, false);
    }

    llvm::Value *codegenConditionalBinding(AST::VariableDeclaration& declaration, llvm::BasicBlock& onFailure) {
        auto initial = declaration.getInitialValue();
        assert(initial);

        auto optionalValue = visitExpressionAsValue(initial);
        auto optionalType = cast<OptionalType>(initial->getType());

        llvm::Value *test;
        llvm::Value *value;
        if (isa<PointerType>(optionalType->getContained())) {
            test = function.builder.CreateIsNotNull(optionalValue);
            value = optionalValue;
        } else {
            auto flag = function.builder.CreateExtractValue(optionalValue, {0});
            test = function.builder.CreateIsNotNull(flag);
            value = function.builder.CreateExtractValue(optionalValue, {1});
        }

        auto& onSuccess = function.createOrphanedBlock();
        function.builder.CreateCondBr(test, &onSuccess, &onFailure);
        function.patchOrphanedBlock(onSuccess);

        if (declaration.getIsMutable()) {
            auto& alloca = function.addVariable(declaration.getBinding());
            function.builder.CreateStore(value, &alloca);
        } else {
            function.pushValueBinding(declaration.getBinding(), value);
        }

        return test;
    }

    llvm::Value *codegenConditions(const AST::vector<AST::Condition>& conditions, llvm::BasicBlock& onFalse) {
        auto last = conditions.size() - 1;

        llvm::Value *value = nullptr;
        llvm::BasicBlock *next = nullptr;
        for (auto [i, condition] : enumerate(conditions)) {
            value = TypeSwitch<AST::Condition, llvm::Value *>(condition)
                .Case<AST::VariableDeclaration *>([&](AST::VariableDeclaration *variable) {
                    return codegenConditionalBinding(*variable, onFalse);
                })
                .Case<AST::Expression *>([&](AST::Expression *expression) {
                    auto condition = visitExpressionAsValue(expression);
                    auto& onTrue = function.createOrphanedBlock();
                    function.builder.CreateCondBr(condition, &onTrue, &onFalse);
                    function.patchOrphanedBlock(onTrue);
                    return condition;
                });
        }
        return value;
    }

    void visitIfStatement(AST::IfStatement& ifStatement) {
        llvm::BasicBlock& end = function.createOrphanedBlock();
        llvm::BasicBlock *next = nullptr;
        const int blockCount = ifStatement.getConditionCount() + (ifStatement.getFallback() ? 0 : -1);
        for (int i = 0; i < ifStatement.getConditionCount(); ++i) {
            if (next) {
                function.patchOrphanedBlock(*next);
            }

            auto& branch = ifStatement.getCondition(i);
            if (i == blockCount) {
                next = &end;
            } else {
                next = &function.createOrphanedBlock();
            }
            auto condition = codegenConditions(branch.getConditions(), *next);

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
        // TODO: Because of the code reuse with if & while, this currently
        // generates a superfluous branch that we could avoid.
        llvm::BasicBlock& exit = function.createOrphanedBlock();
        codegenConditions(guardStatement.getConditions(), exit);

        llvm::BasicBlock& next = function.createOrphanedBlock();
        function.builder.CreateBr(&next);

        function.patchOrphanedBlock(exit);

        visitBlock(guardStatement.getBlock());

        function.patchOrphanedBlock(next);
    }

    void visitReturnStatement(AST::ReturnStatement& returnStatement) {
        if (auto* value = returnStatement.getValue()) {
            auto *returnValue = visitExpressionAsValue(value);
            function.builder.CreateRet(returnValue);
        } else {
            function.builder.CreateRetVoid();
        }
    }

    void visitWhileStatement(AST::WhileStatement& whileStatement) {
        auto& header = function.createOrphanedBlock();
        auto& end = function.createOrphanedBlock();

        function.builder.CreateBr(&header);
        
        function.patchOrphanedBlock(header);

        codegenConditions(whileStatement.getConditions(), end);

        withLoop({header, end}, [&]() {
            visitBlock(whileStatement.getBlock());
        });

        if (!function.currentBlock().getTerminator()) {
            function.builder.CreateBr(&header);
        }

        function.patchOrphanedBlock(end);
    }

    void visitForStatement(AST::ForStatement& forStatement) {
        auto *iterable = visitExpressionAsValue(forStatement.getIterable());
        auto iterableType = forStatement.getIterable().getType();
        if (auto arrayType = dyn_cast<ArrayType>(iterableType)) {
            auto elementType = function.getLLVMType(arrayType->getContained());

            auto& preheader = function.currentBlock();
            auto& header = function.createOrphanedBlock();
            auto& loop = function.createOrphanedBlock();
            auto& end = function.createOrphanedBlock();

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

            function.pushAddressBinding(forStatement.getBinding(), currentPointer);

            auto& latch = function.createOrphanedBlock();

            withLoop({latch, end}, [&]() {
                visitBlock(forStatement.getBlock());
            });

            if (!function.currentBlock().getTerminator()) {
                function.builder.CreateBr(&latch);
            }

            function.patchOrphanedBlock(latch);

            auto incrementedPointer = function.builder.CreateConstGEP1_64(elementType, currentPointer, 1);

            currentPointer->addIncoming(incrementedPointer, &latch);
            function.builder.CreateBr(&header);

            function.patchOrphanedBlock(end);
        } else if (auto rangeType = dyn_cast<RangeType>(iterableType)) {
            auto elementType = rangeType->getBoundType();
            auto llvmElementType = function.getLLVMType(rangeType->getBoundType());

            auto& preheader = function.currentBlock();
            auto& header = function.createOrphanedBlock();
            auto& loop = function.createOrphanedBlock();
            auto& end = function.createOrphanedBlock();

            auto *range = iterable;

            auto startValue = function.builder.CreateExtractValue(range, {0});
            auto endValue = function.builder.CreateExtractValue(range, {1});

            function.builder.CreateBr(&header);
            function.patchOrphanedBlock(header);
            
            auto current = function.builder.CreatePHI(llvmElementType, 2);
            current->addIncoming(startValue, &preheader);

            llvm::Value *condition;
            if (rangeType->isClosed) {
                if (elementType->isSigned) {
                    condition = function.builder.CreateICmpSLE(current, endValue);
                } else {
                    condition = function.builder.CreateICmpULE(current, endValue);
                }
            } else {
                if (elementType->isSigned) {
                    condition = function.builder.CreateICmpSLT(current, endValue);
                } else {
                    condition = function.builder.CreateICmpULT(current, endValue);
                }
            }

            function.builder.CreateCondBr(condition, &loop, &end);

            function.patchOrphanedBlock(loop);

            function.pushValueBinding(forStatement.getBinding(), current);

            auto& latch = function.createOrphanedBlock();
            withLoop({latch, end}, [&]() {
                visitBlock(forStatement.getBlock());
            });

            if (!function.currentBlock().getTerminator()) {
                function.builder.CreateBr(&latch);
            }
            function.patchOrphanedBlock(latch);

            auto incremented = function.builder.CreateAdd(current, function.getIntegerConstant(elementType->bitWidth, 1));
            current->addIncoming(incremented, &latch);
            function.builder.CreateBr(&header);

            function.patchOrphanedBlock(end);
        } else {
            llvm_unreachable("[PROGRAMMER ERROR]: Iterating over value that is not array or range.");
        }
    }

    void visitBreakStatement(AST::BreakStatement&) {
        function.builder.CreateBr(&currentLoop->endBlock);
    }

    void visitContinueStatement(AST::ContinueStatement&) {
        function.builder.CreateBr(&currentLoop->continueBlock);
    }

    void visitExpressionStatement(AST::ExpressionStatement& expression) {
        expression.getExpression().acceptVisitor(*this);
    }

    // Expressions

    Value visitIdentifier(AST::Identifier& identifier) {
        const auto resolution = identifier.getResolution();

        switch (resolution.getKind()) {
            case IdentifierResolution::Kind::Local: {
                const auto local = resolution.as.local;
                auto type = function.getLLVMType(identifier.getType());
                auto *binding = local.binding;

                auto value = function.getVariable(binding);
                if (value.getInt()) {
                    return Value::value(value.getPointer());
                } else {
                    return Value::memory(value.getPointer());
                }
            }
            case IdentifierResolution::Kind::Global: {
                const auto global = resolution.as.global;
                auto type = function.getLLVMType(identifier.getType());
                auto value = function.getGlobal(global.binding);
                if (value.getInt()) {
                    return Value::value(value.getPointer());
                } else {
                    return Value::memory(value.getPointer());
                }
            }
            case IdentifierResolution::Kind::Function:
                return Value::value(&function.getFunction(*resolution.as.function.function));
            case IdentifierResolution::Kind::Parameter:
                return Value::value(&function.getArgument(resolution.as.parameter.parameterIndex));
            case IdentifierResolution::Kind::Type:
                llvm_unreachable("[PROGRAMMER ERROR]: Type resolution in codegen.");
            case IdentifierResolution::Kind::UNRESOLVED:
                llvm_unreachable("UNRESOLVED identifier resolution in codegen.");
        }

        llvm_unreachable("[PROGRAMMER ERROR]: Bad identifier resolution in codegen.");
    }

    llvm::Value *codegenLiteral(AST::Literal& literal) {
        using namespace AST;
        using llvm::Value;
        return AST::visitLiteral(literal, overloaded {
            [&](const AST::NilLiteral& nil) -> Value * {
                auto optionalType = cast<OptionalType>(literal.getType());
                if (isa<PointerType>(optionalType->getContained())) {
                    return llvm::Constant::getNullValue(function.getLLVMType(optionalType));
                } else {
                    auto structType = cast<llvm::StructType>(function.getLLVMType(optionalType));
                    return llvm::ConstantStruct::get(structType, {
                        function.getIntegerConstant(1, 0), 
                        llvm::UndefValue::get(function.getLLVMType(optionalType->getContained()))
                    });
                }
            },
            [&](const AST::BooleanLiteral& boolean) -> Value * {
                llvm::APInt i(1, boolean.getValue() ? 1 : 0, true);
                return function.getIntegerConstant(1, boolean.getValue() ? 1 : 0);
                return llvm::Constant::getIntegerValue(function.getLLVMType(literal.getType()), i);
            },
            [&](const AST::IntegerLiteral& integer) -> Value * {
                return TypeSwitch<Type *, llvm::Value *>(integer.getType())
                    .Case([&](IntegerType *integerType) {
                        auto bitWidth = integerType->bitWidth;
                        llvm::APInt i;
                        if (integerType->isSigned) {
                            i = integer.getValue().sextOrTrunc(bitWidth);
                        } else {
                            i = integer.getValue().zextOrTrunc(bitWidth);
                        }
                        return llvm::Constant::getIntegerValue(function.getLLVMType(literal.getType()), std::move(i));
                    })
                    .Case([&](FPType *fpType) {
                        // TODO: We need to think about rounding here.
                        switch (fpType->precision) {
                            case FPType::Precision::Single: {
                                const llvm::fltSemantics &semantics = llvm::APFloat::IEEEsingle();
                                llvm::APFloat fp{semantics};
                                fp.convertFromAPInt(integer.getValue(), false, llvm::APFloat::roundingMode::TowardZero);
                                return llvm::ConstantFP::get(function.getLLVMType(literal.getType()), fp);
                            }
                            case FPType::Precision::Double: {
                                const llvm::fltSemantics &semantics = llvm::APFloat::IEEEdouble();
                                llvm::APFloat fp{semantics};
                                fp.convertFromAPInt(integer.getValue(), false, llvm::APFloat::roundingMode::TowardZero);
                                return llvm::ConstantFP::get(function.getLLVMType(literal.getType()), fp);
                            }
                        }
                    });
            },
            [&](const AST::FloatingPointLiteral& floating) -> Value * {
                auto type = cast<FPType>(literal.getType());
                auto llvmType = function.getLLVMType(type);
                return llvm::ConstantFP::get(llvmType, floating.getValue());
            },
            [&](const AST::CharacterLiteral& string) -> Value * {
                llvm_unreachable("String literals are not supported.");
            },
            [&](const AST::StringLiteral& string) -> Value * {
                llvm_unreachable("String literals are not supported.");
            }
        });
    }

    Value visitLiteral(AST::Literal& literal) {
        return Value::value(codegenLiteral(literal));
    }

    llvm::Value *codegenUnary(AST::UnaryExpression& unary, llvm::Value *target) {
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

            case AddressOf: llvm_unreachable("");
            case PrefixDereference:
            case PostfixDereference:
                llvm_unreachable("");
            case ForceUnwrap: {
                auto optionalType = cast<OptionalType>(unary.getTarget().getType());
                llvm::Value *isZero;
                if (auto pointerType = dyn_cast<PointerType>(optionalType->getContained())) {
                    auto null = llvm::Constant::getNullValue(function.getLLVMType(pointerType));
                    isZero = function.builder.CreateICmpEQ(target, null);
                } else {
                    auto flag = function.builder.CreateExtractValue(target, {0});
                    auto zero = function.getIntegerConstant(1, 0);
                    isZero = function.builder.CreateICmpEQ(flag, zero);
                }

                auto& trap = function.createTrapBlock();
                auto& success = function.createOrphanedBlock();

                function.builder.CreateCondBr(isZero, &trap, &success);
                function.patchOrphanedBlock(success);

                if (auto pointerType = dyn_cast<PointerType>(optionalType->getContained())) {
                    return target;
                } else {
                    return function.builder.CreateExtractValue(target, {1});
                }
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
    }

    Value visitUnaryExpression(AST::UnaryExpression& unary) {
        Value target = visitExpression(unary.getTarget());

        auto op = unary.getOp();
        if (op == AST::UnaryOperator::AddressOf) {
            assert(target.getKind() == Value::Kind::Memory);
            // TODO: Ensure that this is right.
            return Value::value(target.get());
        } 
        llvm::Value *targetValue = convertToValueIfNecessary(unary.getTarget().getType(), target);
        if (op == AST::UnaryOperator::PrefixDereference || op == AST::UnaryOperator::PostfixDereference) {
            return Value::memory(targetValue);
        }

        return Value::value(codegenUnary(unary, targetValue));
    }

    llvm::Value *codegenLogicalOperator(AST::BinaryExpression& binary) {
        using enum AST::BinaryOperator;
        auto booleanType = function.getLLVMType(binary.getType());
        switch (binary.getOp()) {
            case LogicalAnd: {
                auto left = visitExpressionAsValue(binary.getLeft());
                auto& leftExit = function.currentBlock();

                auto& rightEntry = function.createOrphanedBlock();
                auto& endBlock = function.createOrphanedBlock();

                function.builder.CreateCondBr(left, &rightEntry, &endBlock);

                function.patchOrphanedBlock(rightEntry);
                auto right = visitExpressionAsValue(binary.getRight());
                auto& rightExit = function.currentBlock();
                function.builder.CreateBr(&endBlock);

                function.patchOrphanedBlock(endBlock);
                auto phi = function.builder.CreatePHI(booleanType, 2);
                phi->addIncoming(function.getIntegerConstant(1, false), &leftExit);
                phi->addIncoming(right, &rightExit);
                
                return phi;
            }
            case LogicalOr: {
                auto left = visitExpressionAsValue(binary.getLeft());
                auto& leftExit = function.currentBlock();

                auto& rightEntry = function.createOrphanedBlock();
                auto& endBlock = function.createOrphanedBlock();

                function.builder.CreateCondBr(left, &endBlock, &rightEntry);

                function.patchOrphanedBlock(rightEntry);
                auto right = visitExpressionAsValue(binary.getRight());
                auto& rightExit = function.currentBlock();
                function.builder.CreateBr(&endBlock);

                function.patchOrphanedBlock(endBlock);
                auto phi = function.builder.CreatePHI(booleanType, 2);
                phi->addIncoming(function.getIntegerConstant(1, true), &leftExit);
                phi->addIncoming(right, &rightExit);

                return phi;
            }
            default: llvm_unreachable("[PROGRAMMER ERROR]");
        }

    }

    llvm::Value *codegenEquality(llvm::Value *left, llvm::Value *right, Type *type) {
        return TypeSwitch<Type *, llvm::Value *>(type)
        .Case([&](IntegerType *integerType) {
            return function.builder.CreateICmpEQ(left, right);
        })
        .Case([&](FPType *_) {
            return function.builder.CreateFCmpOEQ(left, right);
        })
        .Case([&](BooleanType *_) {
            return function.builder.CreateICmpEQ(left, right);
        })
        .Case([&](OptionalType *optionalType) -> llvm::Value * {
            if (auto pointerType = dyn_cast<PointerType>(optionalType->getContained())) {
                return function.builder.CreateICmpEQ(left, right);
            } else {
                auto leftFlag = function.builder.CreateExtractValue(left, {0});
                auto rightFlag = function.builder.CreateExtractValue(right, {0});
                auto zero = function.getIntegerConstant(1, 0);
                auto one = function.getIntegerConstant(1, 1);
                auto llvmBoolean = zero->getType();

                auto& compareFlags = function.currentBlock();
                auto& testFlags = function.createOrphanedBlock();
                auto& compareValues = function.createOrphanedBlock();
                auto& end = function.createOrphanedBlock();

                auto flagComparison = function.builder.CreateICmpEQ(leftFlag, rightFlag);
                function.builder.CreateCondBr(flagComparison, &testFlags, &end);

                function.patchOrphanedBlock(testFlags);
                auto flagTest = function.builder.CreateICmpEQ(leftFlag, one);
                function.builder.CreateCondBr(flagTest, &compareValues, &end);

                function.patchOrphanedBlock(compareValues);
                auto leftValue = function.builder.CreateExtractValue(left, {1});
                auto rightValue = function.builder.CreateExtractValue(right, {1});
                auto valueComparison = codegenEquality(leftValue, rightValue, optionalType->getContained());
                function.builder.CreateBr(&end);

                function.patchOrphanedBlock(end);
                auto phi = function.builder.CreatePHI(llvmBoolean, 3);
                phi->addIncoming(zero, &compareFlags);
                phi->addIncoming(one, &testFlags);
                phi->addIncoming(valueComparison, &compareValues);
                return phi;
            }
        });
    }

    llvm::Value *testAgainstNil(llvm::Value *value, OptionalType *type, bool equal) {
        if (auto pointerType = dyn_cast<PointerType>(type->getContained())) {
            auto null = llvm::Constant::getNullValue(function.getLLVMType(pointerType));
            if (equal) {
                return function.builder.CreateICmpEQ(value, null);
            } else {
                return function.builder.CreateICmpNE(value, null);
            }
        } else {
            auto flag = function.builder.CreateExtractValue(value, {0});
            auto zero = function.getIntegerConstant(1, 0);

            if (equal) {
                return function.builder.CreateICmpEQ(flag, zero);
            } else {
                return function.builder.CreateICmpNE(flag, zero);
            }
        }
    }

    llvm::Value *codegenBinary(AST::BinaryExpression& binary) {
        auto left = visitExpressionAsValue(binary.getLeft());
        auto right = visitExpressionAsValue(binary.getRight());

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

            case Equal: 
                if (auto optionalType = dyn_cast<OptionalType>(binary.getLeft().getType())) {
                    if (isa<AST::NilLiteral>(binary.getLeft())) {
                        return testAgainstNil(right, cast<OptionalType>(binary.getRight().getType()), true);
                    } else if (isa<AST::NilLiteral>(binary.getRight())) {
                        return testAgainstNil(left, cast<OptionalType>(binary.getLeft().getType()), true);
                    }
                }
                return codegenEquality(left, right, binary.getLeft().getType());

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
                })
                .Case([&](OptionalType *optionalType) {
                    if (isa<AST::NilLiteral>(binary.getLeft())) {
                        return testAgainstNil(right, cast<OptionalType>(binary.getRight().getType()), false);
                    } else if (isa<AST::NilLiteral>(binary.getRight())) {
                        return testAgainstNil(left, cast<OptionalType>(binary.getLeft().getType()), false);
                    } else {
                        // TODO: Compare discriminant bits. If both are 1, compare contents.
                        llvm_unreachable("");

                    }
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

            case OpenRange: 
            case ClosedRange: {
                auto start = left;
                auto end = right;

                auto llvmType = function.getLLVMType(binary.getType());
                llvm::Value *structValue = llvm::UndefValue::get(llvmType);

                auto insertStart = function.builder.CreateInsertValue(structValue, start, {0});
                auto insertEnd = function.builder.CreateInsertValue(insertStart, end, {1});

                return insertEnd;
            }

            case LogicalAnd: llvm_unreachable("[PRORAMMER EROR]");
            case LogicalOr: llvm_unreachable("[PRORAMMER EROR]");
        }

        llvm_unreachable("[PROGRAMMER ERROR]");
    }

    Value visitBinaryExpression(AST::BinaryExpression& binary) {
        using enum AST::BinaryOperator;
        if (binary.getOp() == LogicalAnd || binary.getOp() == LogicalOr) {
            return Value::value(codegenLogicalOperator(binary));
        }
        return Value::value(codegenBinary(binary));
    }

    llvm::Value *createPrint(AST::IntrinsicExpression& printIntrinsic) {
        auto argument = printIntrinsic.getArguments()[0];
        auto printf = function.getPrintf();
        llvm::Value *returnValue;
        if (auto literal = dyn_cast<AST::Literal>(argument)) {
            using namespace AST;
            // We should probably constrain this to only string literals.
            returnValue = AST::visitLiteral(*literal, overloaded {
                [&](StringLiteral& stringLiteral) {
                    auto formatString = function.builder.CreateGlobalStringPtr("%s");
                    auto& stringLiteralValue = stringLiteral.getValue();
                    auto string = function.builder.CreateGlobalStringPtr({stringLiteralValue.data(), stringLiteralValue.size()});
                    return function.builder.CreateCall(printf, {formatString, string});
                },
                [&](IntegerLiteral& integerLiteral) {
                    auto& value = integerLiteral.getValue();
                    llvm::SmallVector<char, 18> literalString;
                    value.toString(literalString, 10, false);

                    auto formatString = function.builder.CreateGlobalStringPtr("%s");
                    auto string = function.builder.CreateGlobalStringPtr({literalString.data(), literalString.size()});
                    return function.builder.CreateCall(printf, {formatString, string});
                },
                [&](FloatingPointLiteral& fpLiteral) {
                    auto fpValue = fpLiteral.getValue();
                    auto value = llvm::ConstantFP::get(function.getFPType(FPType::Precision::Double), fpValue);
                    auto formatString = function.builder.CreateGlobalStringPtr("%s");
                    return function.builder.CreateCall(printf, {formatString, value});

                },
                [&](NilLiteral& nilLiteral) {
                    auto string = function.builder.CreateGlobalStringPtr("nil");
                    return function.builder.CreateCall(printf, {string});
                },
                [&](BooleanLiteral& booleanLiteral) {
                    auto string = function.builder.CreateGlobalStringPtr(booleanLiteral.getValue() ? "true" : "false");
                    return function.builder.CreateCall(printf, {string});
                },
                [&](auto& _) -> llvm::CallInst * {
                    llvm_unreachable("Implement");
                }
            });
        } else {
            auto printFunction = function.getPrintFunction(*argument->getType());
            llvm::Value *value = visitExpressionAsValue(argument);
            auto returnValue = function.builder.CreateCall(printFunction, {value});
        }
        auto putchar = function.getPutchar();
        function.builder.CreateCall(putchar, {function.getIntegerConstant(32, 10)});
        return returnValue;
    }

    llvm::Value *codegenIntrinsic(AST::IntrinsicExpression& intrinsic) {
        switch (intrinsic.getIntrinsic()) {
            case IntrinsicKind::Truncate: {
                auto value = visitExpressionAsValue(intrinsic.getArguments()[0]);
                auto destination = function.getLLVMType(intrinsic.getType());
                return function.builder.CreateTrunc(value, destination);
            }
            case IntrinsicKind::Print: {
                return createPrint(intrinsic);
            }
            case IntrinsicKind::Assert: {
                // TODO: Check build type.
                auto& trap = function.createTrapBlock();
                auto& next = function.createOrphanedBlock();

                auto condition = visitExpressionAsValue(intrinsic.getArguments()[0]);
                function.builder.CreateCondBr(condition, &next, &trap);
                function.patchOrphanedBlock(next);
                return nullptr;
            }
            case IntrinsicKind::Bitcast: {
                auto from = visitExpressionAsValue(intrinsic.getArguments()[0]);
                auto type = function.getLLVMType(intrinsic.getType());
                return function.builder.CreateBitCast(from, type);
            }
        }
        llvm_unreachable("[TODO: Codegen intrinsics]");
        return nullptr;
    }

    Value visitIntrinsicExpression(AST::IntrinsicExpression& intrinsic) {
        return Value::value(codegenIntrinsic(intrinsic));
    }

    Value visitCallExpression(AST::CallExpression& call) {
        // TODO: We need to handle stack & pack returns here, once they're implemented.
        auto callee = visitExpressionAsValue(call);
        llvm::Function *llvmFunction = llvm::cast<llvm::Function>(callee);
        llvmFunction->getFunctionType();
        if (call.argumentCount()) {
            llvm::Value *arguments[call.argumentCount()];
            for (int i = 0; i < call.argumentCount(); ++i) {
                arguments[i] = visitExpressionAsValue(call.getArgument(i));
            }
            return Value::value(function.builder.CreateCall(llvmFunction->getFunctionType(), llvmFunction, llvm::ArrayRef(arguments, call.argumentCount())));
        } else {
            return Value::value(function.builder.CreateCall(llvmFunction->getFunctionType(), llvmFunction));
        }
    }

    Value visitSubscriptExpression(AST::SubscriptExpression& subscript) {
        auto& arrayType = cast<ArrayType>(*subscript.getTarget().getType());
        auto elementType = function.getLLVMType(arrayType.getContained());

        // TODO: See if we can handle the memory case directly, instead of struct load.
        auto target = visitExpressionAsValue(subscript.getTarget());
        auto index = visitExpressionAsValue(subscript.getIndex());

        auto base = function.builder.CreateExtractValue(target, {0});

        if (arrayType.isBounded) {
            auto integerType = cast<IntegerType>(subscript.getIndex().getType());

            auto& trapBlock = function.createTrapBlock();

            if (integerType->isSigned) {
                auto cond = function.builder.CreateICmpSGE(index, function.getIntegerConstant(integerType->bitWidth, 0));
                auto& next = function.createOrphanedBlock();
                function.builder.CreateCondBr(cond, &next, &trapBlock);
                function.patchOrphanedBlock(next);
            }

            // The index value can now be treated as unsigned.
            
            // TODO: Allow boundary checking to be toggled.
            // TODO: We need to have a default index type, usize.
            // For 32 bit, we might need to zext the length to 64 bit, if the index is 64 bit.

            if (integerType->bitWidth < 64) {
                // FIXME: get integer type properly.
                index = function.builder.CreateZExt(index, function.getIntegerConstant(64, 0)->getIntegerType());
            }

            auto& next = function.createOrphanedBlock();
            auto count = function.builder.CreateExtractValue(target, {1});
            auto cond = function.builder.CreateICmpULT(index, count);
            function.builder.CreateCondBr(cond, &next, &trapBlock);
            function.patchOrphanedBlock(next);

            auto address = function.builder.CreateGEP(elementType, base, {index});
            return Value::memory(address);

            auto size = function.builder.CreateExtractValue(target, {1});

            assert(false && "TODO: Implement array bounds checking.");
        } else {
            auto address = function.builder.CreateGEP(elementType, base, {index});
            return Value::memory(address);
        }
    }

    Value visitInitializerExpression(AST::InitializerExpression& initializer) {
        auto structType = cast<StructType>(initializer.getType());
        llvm::Value *structValue = llvm::UndefValue::get(function.getLLVMType(structType));

        Bitmap undefined{(uint32_t) structType->getFields().size()};

        for (size_t i = 0; i < initializer.getNumberOfPairs(); ++i) {
            auto& pair = initializer.getPair(i);

            auto resolution = pair.first->getResolution();
            assert(resolution.getKind() == MemberResolution::Kind::StructField);

            auto value = visitExpressionAsValue(pair.second);
            unsigned index = resolution.as.structField.index;
            undefined.set(index);

            structValue = function.builder.CreateInsertValue(structValue, value, {index});
        }

        undefined.iterate_zeros([&](uint32_t index) {
            auto initial = structType->getFields()[index]->getInitialValue();
            assert(initial);
            auto value = visitExpressionAsValue(initial);
            structValue = function.builder.CreateInsertValue(structValue, value, {index});
        });

        return Value::value(structValue);
    }

    Value visitMemberAccessExpression(AST::MemberAccessExpression& memberAccess) {
        auto _target = memberAccess.getTarget().acceptVisitor(*this);
        auto target = Value::value(nullptr);
        auto targetType = memberAccess.getTarget().getType();

        auto resolution = memberAccess.getResolution();

        switch (resolution.getKind()) {
            case MemberResolution::Kind::StructField: {
                switch (target.getKind()) {
                    case Value::Kind::Value:
                        return Value::value(function.builder.CreateExtractValue(target.get(), {resolution.as.structField.index}));
                    case Value::Kind::Memory:
                        return Value::memory(function.builder.CreateConstGEP2_32(function.getLLVMType(targetType), target.get(), 0, resolution.as.structField.index));
                    case Value::Kind::COUNT:
                        llvm_unreachable("");
                    }
                }
                llvm_unreachable("");
            case MemberResolution::Kind::StructMethod:
            case MemberResolution::Kind::EnumCase:
                llvm_unreachable("[TODO] Unsupported member access types.");
            case MemberResolution::Kind::UNRESOLVED:
                llvm_unreachable("UNRESOLVED member access in codegen.");
        }
    }

    Value visitInferredMemberAccessExpression(AST::InferredMemberAccessExpression& inferredMemberAccess) {
        // This can only be a static member access or enum case
        
        auto resolution = inferredMemberAccess.getResolution();

        switch (resolution.getKind()) {
            case MemberResolution::Kind::StructField:
                break;
            case MemberResolution::Kind::StructMethod:
                break;
            case MemberResolution::Kind::EnumCase:
                break;
            case MemberResolution::Kind::UNRESOLVED:
                llvm_unreachable("UNRESOLVED member access in codegen.");
        }


        // TODO: Implement static members

        assert(false);
        llvm_unreachable("[TODO] Implement static members");
    }
};

void codeGenFunction(AST::FunctionDeclaration& function, llvm::Function& llvmFunction, Context& context) {
    FunctionCodeGenerator generator{llvmFunction, context};
    generator.generateFunctionCode(function);
}

void codegenGlobals(Context& context) {
    if (context.globals.empty()) return;

    llvm::Function *globalConstructor = context.setupGlobalConstructor();
    CompilingFunction function{context, *globalConstructor};

    llvm::FunctionType *initializerType = llvm::FunctionType::get(llvm::Type::getVoidTy(context.llvmContext), false);
    const char *initializerName = "__glang_global_initializer";

    for (const auto global : context.globals) {
        llvm::Function *initializer = llvm::Function::Create(initializerType, llvm::Function::ExternalLinkage, initializerName, context.llvmModule);
        FunctionCodeGenerator generator{*initializer, context};
        generator.generateGlobalCode(*global);

        function.builder.CreateCall(initializerType, initializer);
    }

    function.builder.CreateRetVoid();
}

Result performCodegen(Context& context) {
    for (const auto function : context.functions) {
        llvm::Function *llvmFunction = context.llvmFunctions[function];
        codeGenFunction(*function, *llvmFunction, context);
    }

    codegenGlobals(context);

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

    if (llvm::verifyModule(*module, &llvm::outs())) {
        llvm::outs() << *module;
        module.reset();
        return module;
    }

    return module;
}
