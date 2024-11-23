#ifndef LANG_namespace_h
#define LANG_namespace_h

#include "AST.h"
#include "AST_Visitor.h"

#include "common.h"
#include "containers/symbol_map.h"
#include "diagnostic.h"

#include "type.h"
#include "type/struct.h"
#include "type/enum.h"

#include "llvm/ADT/PointerUnion.h"

struct Function {
    const u16 parameterCount;
    const u32 file;
    FunctionType *type = nullptr;

    Function(u16 parameterCount, u32 file)
        : parameterCount{parameterCount}, file{file} {}
};

struct Method {
    const u16 parameterCount;
    const u32 file;
    Type *self;
    FunctionType *type = nullptr;

    Method(u16 parameterCount, u32 file, Type *self)
        : parameterCount{parameterCount}, file{file}, self{self} {}
};

// There may be better names for these two.

struct GlobalDeclaration {
    AST::VariableDeclaration *declaration;
    u32 bindingsIndex;
    u32 bindingsSize;
    const u32 file;

    GlobalDeclaration(AST::VariableDeclaration *declaration, u32 bindingsIndex, u32 bindingsSize, u32 file) 
        : declaration{declaration}, bindingsIndex{bindingsIndex}, bindingsSize{bindingsSize}, file{file} {}
};

struct GlobalBinding {
    AST::IdentifierBinding *binding;
    u32 declarationIndex;

    GlobalBinding(AST::IdentifierBinding *binding, u32 declarationIndex) 
        : binding{binding}, declarationIndex{declarationIndex} {}
};

struct Definition {
    enum class Kind : u8 {
        Function,
        Global,
        Struct,
        Enum,
    };

    static constexpr u32 indexBitMask = (1 << 28) - 1;

    const u32 bits;

    Definition(Kind kind, u32 index) : bits{(u32(kind) << 28) | (index & indexBitMask)}  {
        assert(index < (1 << 28));
    }

    static Definition fromFunctionIndex(u32 functionIndex) {
        return Definition(Kind::Function, functionIndex);
    }

    static Definition fromGlobalIndex(u32 globalIndex) {
        return Definition(Kind::Global, globalIndex);
    }

    static Definition fromStructIndex(u32 structIndex) {
        return Definition(Kind::Struct, structIndex);
    }

    static Definition fromEnumIndex(u32 enumIndex) {
        return Definition(Kind::Enum, enumIndex);
    }

    u32 index() const {
        return bits & indexBitMask;
    }

    Kind kind() const {
        return Kind(bits >> 28);
    }
};

struct Module {
    //using Definition = llvm::PointerUnion<AST::FunctionDeclaration *, AST::VariableDeclaration *, AST::IdentifierBinding *, Type *>;
    SymbolMap<Definition> all;

    SymbolMap<Type *NONNULL> types;

    std::vector<StructType *NONNULL> structs;
    /// parallel to `structs`.
    std::vector<AST::StructDeclaration *NONNULL> structDeclarations;

    std::vector<EnumType *NONNULL> enums;
    /// parallel to `structs`.
    std::vector<AST::EnumDeclaration *NONNULL> enumDeclarations;

    std::vector<Function> functions;
    /// parallel to `functions`.
    std::vector<AST::FunctionDeclaration *NONNULL> functionDeclarations;

    std::vector<GlobalDeclaration> globalDeclarations;
    std::vector<GlobalBinding> globalBindings;
};

struct ModuleBuilder {
    using Result = PassResult;
    using enum PassResultKind;

    PassResult result = PassResultKind::OK;
    std::unique_ptr<Module> module = std::make_unique<Module>();

    void addDeclarations(std::span<AST::Declaration *NONNULL> declarations, u32 file);

    std::unique_ptr<Module> finalize() {
        if (result.ok()) {
            return std::move(module);
        } else {
            return nullptr;
        }
    }
};

#endif // LANG_namespace_h
