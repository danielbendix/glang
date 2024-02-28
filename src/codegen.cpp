#include "AST.h"
#include "AST_Visitor.h"
#include "type.h"

#include "llvm/ADT/StringSet.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"


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
    llvm::StringMap<AST::Declaration *> globals;

    Context() : globals{} {}
};


//using namespace llvm;

class CompilingFunction {
    Context& context;
    llvm::Function& function;
    llvm::AllocaInst *lastAlloca = nullptr;
    llvm::IRBuilder<> builder;
    Local locals[UINT8_MAX];
    int localCount = 0;
    int currentScope = 1;

    CompilingFunction(Context& context, llvm::Function& function) : context{context}, function{function}, builder{function.getContext()} {}

    llvm::BasicBlock& entryBlock() {
        return function.getEntryBlock();
    }

    void pushScope() {
        currentScope += 1;
    }

    void popScope() {
        while (localCount > 0 && locals[localCount - 1].depth == currentScope) {
            localCount -= 1;
        }
        currentScope -= 1;
    }

    void addLocal(AST::VariableDeclaration& variable) {
        // TODO: overflow check
        auto& alloca = makeStackSlot(nullptr);
        locals[localCount] = Local(variable.getName(), alloca, nullptr, currentScope);
    }

    llvm::AllocaInst *getVariable(std::string& identifier) {
        for (int i = localCount - 1; i >= 0; --i) {
            if (*locals[i].identifier == identifier) {
                return locals[i].alloca;
            }
        }
        return nullptr;
    }

    llvm::AllocaInst& makeStackSlot(llvm::Type *type) {
        builder.SetInsertPointPastAllocas(&function);
        auto alloca = builder.CreateAlloca(type, nullptr);
        builder.Insert(alloca);
        return *alloca;
    }
};


class DeclarationTableVisitor2 : public AST::DeclarationVisitorT<DeclarationTableVisitor2, void> {

    llvm::StringMap<AST::Declaration *>& table;
    // TODO: Should we store duplicates here?

    void addDeclaration(const std::string& name, AST::Declaration& declaration) {
        auto it = table.insert(std::make_pair(name, &declaration));

        if (!it.second) {
            duplicateDetected = true;
            // EMIT diagnostic
            // Global namespace conflict
            // show initial and attempt to replace.
        }
    }
public:
    void visitVariableDeclaration(AST::VariableDeclaration& variable) {
        addDeclaration(variable.getName(), variable);
    }

    void visitFunctionDeclaration(AST::FunctionDeclaration& function) {
        addDeclaration(function.getName(), function);
    }

    void visitStructDeclaration(AST::StructDeclaration& structDeclaration) {
        addDeclaration(structDeclaration.getName(), structDeclaration);
    }

    void visitEnumDeclaration(AST::EnumDeclaration& enumDeclaration) {

    }

    void visitClassDeclaration(AST::ClassDeclaration& classDeclaration) {

    }

    void visitProtocolDeclaration(AST::ProtocolDeclaration& protocolDeclaration) {

    }

    void visitStatementDeclaration(AST::StatementDeclaration& statement) {

    }

    bool duplicateDetected = false;
    DeclarationTableVisitor2(llvm::StringMap<AST::Declaration *>& table) : table{table} {}
};

class DeclarationCodeGen : public AST::DeclarationVisitorT<DeclarationCodeGen, llvm::Value *> {

};

class StatementCodeGen : public AST::StatementVisitorT<StatementCodeGen, llvm::Value *> {

};

class ExpressionCodeGen : public AST::ExpressionVisitorT<ExpressionCodeGen, llvm::Value *> {

    llvm::Value *visitBinaryExpression(AST::BinaryExpression& binaryExpression) {
        return nullptr;
    }

    llvm::Value *visitCallExpression(AST::CallExpression& callExpression) {
        return nullptr;
    }

    llvm::Value *visitUnaryExpression(AST::UnaryExpression& unaryExpression) {
        return nullptr;
    }

    llvm::Value *visitLiteral(AST::Literal& literal) {
        return nullptr;
    }
};

bool populateSymbolTable(std::vector<AST::unique_ptr<AST::Declaration>>& declarations, llvm::StringMap<AST::Declaration *>& table)
{
    DeclarationTableVisitor2 visitor{table};

    for (const auto& declaration : declarations) {
        declaration->acceptVisitor(visitor);
    }

    return !visitor.duplicateDetected;
}

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

    llvm::StringMap<AST::Declaration *> globals;

    populateSymbolTable(declarations, globals);

    auto module = std::make_unique<llvm::Module>("test", llvmContext);

    return module;
}
