#ifndef LANG_scope_h
#define LANG_scope_h

#include "common.h"
#include "namespace.h"
#include "AST.h"
#include "containers/symbol_table.h"

#include "llvm/Support/Casting.h"
#include "llvm/ADT/TypeSwitch.h"

using Result = PassResult;
using enum PassResultKind;

class ScopeManager {
    
    struct Local {
        enum class Kind : u8 {
            Parameter,
            Constant,
            Variable,
        };
        const Symbol* identifier;
        union {
            Kind kind;
            struct {
                Kind kind;
                u32 index;
                u32 functionIndex;
            } parameter;
            struct {
                Kind kind;
                AST::IdentifierBinding *binding;
            } local;
        } as;
        bool isWritten = false;
        bool isRead = false;

        [[noreturn]]
        Local() {
            llvm_unreachable("Programmer error: Default constructor of Local should never be called.");
        }

        Local(const Symbol& identifier, u32 functionIndex, u32 index) : identifier{&identifier}, as{.parameter = {.kind = Kind::Parameter, .index = index, .functionIndex = functionIndex}} {}

        Local(const Symbol& identifier, AST::IdentifierBinding& binding, bool isVariable) : identifier{&identifier}, as{.local = {.kind = isVariable ? Kind::Variable : Kind::Constant, .binding = &binding}} {}
    };

    static_assert(sizeof(Local) <= 32);

    std::vector<Local> locals = {};
    std::vector<u32> scopes = {0};

    Module& module;

public:

    ScopeManager(Module& module) : module{module} {
        locals.reserve(64);
        scopes.reserve(8);
    }

    void reset() {
        locals.clear();
        scopes.clear();
        scopes.push_back(0);
    }

    void pushOuterScope() {

    }

    void popOuterScope() {

    }

    void pushInnerScope() {
        scopes.push_back(locals.size());
    }

    void warnOnUnusedLocal(Local& local) {
        switch (local.as.kind) {
            case Local::Kind::Parameter:
                if (!local.isRead) {
                    // TODO: Get location of parameter
                    auto *declaration = module.functionDeclarations[local.as.parameter.functionIndex];
                    Diagnostic::error(*declaration, "Unused parameter [TODO: Add name and location].");
                }
                break;
            case Local::Kind::Constant:
                if (!local.isRead) {
                    Diagnostic::warning(*local.as.local.binding, "Unused immutable value.");
                }
                break;
            case Local::Kind::Variable:
                if (local.isWritten) {
                    if (!local.isRead) {
                        Diagnostic::warning(*local.as.local.binding, "Variable was written to, but never read.");
                    }
                } else if (local.isRead) {
                    Diagnostic::warning(*local.as.local.binding, "Variable was never mutated.");
                } else {
                    Diagnostic::warning(*local.as.local.binding, "Unused variable.");
                }
                break;
        }
    }

    void popInnerScope() {
        u32 resetTo = scopes.back();
        scopes.pop_back();

        popLocalsEmittingWarnings(resetTo);
    }

    void popLocalsEmittingWarnings(size_t resetTo) {
        for (int i = resetTo; i < locals.size(); ++i) {
            warnOnUnusedLocal(locals[i]);
        }

        locals.resize(resetTo);
    }

    auto withScope(auto handler) {
        pushInnerScope();

        handler();

        popInnerScope();
    }

    template<typename F>
    static constexpr bool returns_void = std::is_same_v<void, std::invoke_result_t<F>>;

    /// This function will reset to the current scope, and remove all bindings
    /// after invoking the handler.
    template <typename Lambda>
    auto withAutoResetScope(Lambda handler) {
        auto index = scopes.size();
        pushInnerScope();

        if constexpr (returns_void<Lambda>) {
            handler();

            auto resetTo = scopes[index];
            scopes.resize(index);
            popLocalsEmittingWarnings(resetTo);
        } else {
            auto value = handler();

            auto resetTo = scopes[index];
            locals.resize(resetTo);
            popLocalsEmittingWarnings(resetTo);

            return value;
        }
    }

    /// This function will merge all scopes added inside the handler into the
    /// currently open scope at call time.
    template <typename Lambda>
    auto withAutoMergingScopes(Lambda handler) {
        auto resetScopesTo = scopes.size();

        if constexpr (returns_void<Lambda>) {
            handler();
            popLocalsEmittingWarnings(resetScopesTo);
        } else {
            auto value = handler();
            popLocalsEmittingWarnings(resetScopesTo);
            return value;
        }
    }

    Result pushParameter(const Symbol& identifier, u32 functionIndex, u32 parameterIndex, AST::FunctionDeclaration *declaration) {
        assert(!scopes.empty());
        i64 maxIndex = scopes.back();
        
        for (int i = locals.size() - 1; i >= maxIndex; --i) {
            if (*locals[i].identifier == identifier) {
                Diagnostic::error(*declaration, "Invalid redeclaration of parameter " + identifier.string());
                return ERROR;
            }
        }
        locals.emplace_back(identifier, functionIndex, parameterIndex);
        return OK;
    }

    Result pushBinding(const Symbol& identifier, AST::IdentifierBinding& binding) {
        assert(!scopes.empty());
        i64 maxIndex = scopes.back();

        for (int i = locals.size() - 1; i >= maxIndex; --i) {
            if (*locals[i].identifier == identifier) {
                Diagnostic::error(binding, "Invalid redeclaration of " + identifier.string());
                Diagnostic::note(*locals[i].as.local.binding, identifier.string() + " previously declared here.");
                return ERROR;
            }
        }
        locals.emplace_back(identifier, binding, binding.getIsMutable());
        return OK;
    }

    IdentifierResolution getResolution(const Symbol& identifier, bool isRead = true, bool isWrite = false) {
        for (int i = locals.size() - 1; i >= 0; --i) {
            Local& local = locals[i];
            if (*local.identifier == identifier) {
                if (isWrite) { 
                    local.isWritten = true;
                }
                if (isRead) {
                    local.isRead = true;
                }
                if (local.as.kind == Local::Kind::Parameter) {
                    auto *function = &module.functions[local.as.parameter.functionIndex];
                    return IdentifierResolution::parameter(function, local.as.parameter.index);
                } else {
                    return IdentifierResolution::local(local.as.local.binding);
                }
            }
        }
        
        if (auto declaration = module.all.lookup(identifier)) {
            auto kind = declaration->kind();
            auto index = declaration->index();
            switch (kind) {
                case Definition::Kind::Function:
                    return IdentifierResolution::function(&module.functions[index], index);
                case Definition::Kind::Global:
                    return IdentifierResolution::global(index, module.globalBindings[index].binding, false);
                case Definition::Kind::Struct:
                    return IdentifierResolution::type(module.structs[index]);
                case Definition::Kind::Enum:
                    return IdentifierResolution::type(module.enums[index]);
            }
        }

        return IdentifierResolution::unresolved();
    }
};

#endif // LANG_scope_h
