#include "AST.h"
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


using namespace llvm;

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


class DeclarationTableVisitor : public AST::DeclarationVisitor {

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


    virtual void visitFunctionDeclaration(AST::FunctionDeclaration& functionDeclaration) override {
        addDeclaration(functionDeclaration.getName(), functionDeclaration);
    }

    virtual void visitVariableDeclaration(AST::VariableDeclaration& variableDeclaration) override {
        addDeclaration(variableDeclaration.getName(), variableDeclaration);
    }

    virtual void visitStructDeclaration(AST::StructDeclaration& structDeclaration) override {
        addDeclaration(structDeclaration.getName(), structDeclaration);
    }

    virtual void visitStatementDeclaration(AST::StatementDeclaration& statementDeclaration) override {

    }
public:
    bool duplicateDetected = false;
    DeclarationTableVisitor(llvm::StringMap<AST::Declaration *>& table) : table{table} {}
};

class DeclarationCodeGen : public AST::DeclarationVisitor {

};

class StatementCodeGen : public AST::StatementVisitor {

};

class ExpressionCodeGen : public AST::ExpressionVisitor {

    virtual void visitBinaryExpression(AST::BinaryExpression& binaryExpression) override {

    }

    virtual void visitCallExpression(AST::CallExpression& callExpression) override {

    }

    virtual void visitUnaryExpression(AST::UnaryExpression& unaryExpression) override {

    }

    virtual void visitLiteral(AST::Literal& literal) override {

    }
};

bool populateSymbolTable(std::vector<unique_ptr<AST::Declaration>>& declarations, llvm::StringMap<AST::Declaration *>& table)
{
    DeclarationTableVisitor visitor{table};

    for (const auto& declaration : declarations) {
        declaration->acceptVisitor(visitor);
    }

    return !visitor.duplicateDetected;
}

FunctionType *createFunctionType(AST::FunctionDeclaration& functionDeclaration)
{
    llvm::Type *parameterTypes[functionDeclaration.getParameterCount()];

    for (int i = 0; i < functionDeclaration.getParameterCount(); i++) {
        parameterTypes[i] = defaultType->type;
    }

    auto a = ArrayRef(parameterTypes, functionDeclaration.getParameterCount());
    return FunctionType::get(defaultType->type, a, false);

}

std::unique_ptr<llvm::Module> generateCode(std::vector<unique_ptr<AST::Declaration>>& declarations)
{
    LLVMContext llvmContext;
    instantiateDefaultType(llvmContext);

    llvm::StringMap<AST::Declaration *> globals;

    populateSymbolTable(declarations, globals);

    auto module = std::make_unique<Module>("test", llvmContext);


    return module;
}
