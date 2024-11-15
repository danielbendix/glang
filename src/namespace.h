#ifndef LANG_namespace_h
#define LANG_namespace_h

#include "AST.h"
#include "AST_Visitor.h"

#include "containers/symbol_map.h"
#include "diagnostic.h"

#include "type.h"
#include "type/struct.h"
#include "type/enum.h"

#include "llvm/ADT/PointerUnion.h"

struct Function {
    uint16_t parameterCount;
    uint32_t file;
    FunctionType *type = nullptr;
};

struct Method {
    uint16_t parameterCount;
    uint32_t file;
    Type *self;
    FunctionType *type = nullptr;
};

// For now, we require a global to be an identifier binding.
struct Global {
    AST::IdentifierBinding *binding;
    AST::Expression *value;
    Type *type;
};

struct Definition {
    enum class Kind : uint8_t {
        Function,
        Global,
        Struct,
        Enum,
    };

    static constexpr uint32_t indexBitMask = (1 << 28) - 1;

    uint32_t bits;

    Definition(Kind kind, uint32_t index) : bits{(uint32_t(kind) << 28) | (index & indexBitMask)}  {
        assert(index < (1 << 28));
    }

    static Definition fromFunctionIndex(uint32_t functionIndex) {
        return Definition(Kind::Function, functionIndex);
    }

    static Definition fromGlobalIndex(uint32_t globalIndex) {
        return Definition(Kind::Global, globalIndex);
    }

    static Definition fromStructIndex(uint32_t structIndex) {
        return Definition(Kind::Struct, structIndex);
    }

    static Definition fromEnumIndex(uint32_t enumIndex) {
        return Definition(Kind::Enum, enumIndex);
    }

    uint32_t index() const {
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

    std::vector<AST::VariableDeclaration *NONNULL> globals;

    std::vector<StructType *NONNULL> structs;
    /// parallel to `structs`.
    std::vector<AST::StructDeclaration *NONNULL> structDeclarations;

    std::vector<EnumType *NONNULL> enums;
    /// parallel to `structs`.
    std::vector<AST::EnumDeclaration *NONNULL> enumDeclarations;

    std::vector<Function> functions;
    /// parallel to `functions`.
    std::vector<AST::FunctionDeclaration *NONNULL> functionDeclarations;

    std::vector<Global> globals_;
    /// parallel to `globals`.
    std::vector<AST::VariableDeclaration *NONNULL> globalDeclarations;
};

struct ModuleBuilder {
    using Result = PassResult;
    using enum PassResultKind;

    PassResult result = PassResultKind::OK;
    std::unique_ptr<Module> module = std::make_unique<Module>();

    void addDeclarations(std::span<AST::Declaration *NONNULL> declarations, uint32_t file);

    std::unique_ptr<Module> finalize() {
        if (result.ok()) {
            return std::move(module);
        } else {
            return nullptr;
        }
    }
};

#endif // LANG_namespace_h
