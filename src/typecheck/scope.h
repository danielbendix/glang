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

using llvm::TypeSwitch;
using llvm::cast;

class ScopeManager {
    
    struct Local {
        const Symbol* identifier;
        bool isParameter;
        union {
            struct {
                AST::FunctionDeclaration *function;
                int index;
            } parameter;
            AST::IdentifierBinding *binding;
        } as;

        [[noreturn]]
        Local() {
            llvm_unreachable("Programmer error: Default constructor of Local should never be called.");
        }

        Local(const Symbol& identifier, AST::FunctionDeclaration& function, int index) : identifier{&identifier}, isParameter{true}, as{.parameter = {.function=&function, index = index}} {}

        Local(const Symbol& identifier, AST::IdentifierBinding& binding) : identifier{&identifier}, isParameter{false}, as{.binding = &binding} {}
    };

    static_assert(sizeof(Local) <= 32);

    std::vector<Local> locals = {};
    std::vector<uint32_t> scopes = {0};

    ModuleDef& moduleDefinition;

public:

    ScopeManager(ModuleDef& moduleDefinition) : moduleDefinition{moduleDefinition} {
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

    void popInnerScope() {
        uint32_t resetTo = scopes.back();
        scopes.pop_back();

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
            locals.resize(resetTo);
            scopes.resize(index);
        } else {
            auto value = handler();

            auto resetTo = scopes[index];
            locals.resize(resetTo);
            scopes.resize(index);

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
            scopes.resize(resetScopesTo);
        } else {
            auto value = handler();
            scopes.resize(resetScopesTo);
            return value;
        }
    }

    Result pushParameter(const Symbol& identifier, AST::FunctionDeclaration& function, int index) {
        assert(!scopes.empty());
        int64_t maxIndex = scopes.back();
        
        for (int i = locals.size() - 1; i >= maxIndex; --i) {
            if (*locals[i].identifier == identifier) {
                Diagnostic::error(function, "Invalid redeclaration of parameter " + identifier.string());
                return ERROR;
            }
        }
        locals.emplace_back(identifier, function, index);
        return OK;
    }

    Result pushBinding(const Symbol& identifier, AST::IdentifierBinding& binding) {
        assert(!scopes.empty());
        int64_t maxIndex = scopes.back();

        for (int i = locals.size() - 1; i >= maxIndex; --i) {
            if (*locals[i].identifier == identifier) {
                Diagnostic::error(binding, "Invalid redeclaration of " + identifier.string());
                Diagnostic::note(*locals[i].as.binding, identifier.string() + " previously declared here.");
                return ERROR;
            }
        }
        locals.emplace_back(identifier, binding);
        return OK;
    }

    IdentifierResolution getResolution(const Symbol& identifier) {
        for (int i = locals.size() - 1; i >= 0; --i) {
            Local& local = locals[i];
            if (*local.identifier == identifier) {
                if (local.isParameter) {
                    return IdentifierResolution::parameter(local.as.parameter.function, local.as.parameter.index);
                } else {
                    return IdentifierResolution::local(local.as.binding);
                }
            }
        }
        
        if (auto declaration = moduleDefinition.all.lookup(identifier)) {
            auto result = TypeSwitch<ModuleDef::Definition, IdentifierResolution>(*declaration)
                .Case<AST::FunctionDeclaration *>([](auto function) {
                    return IdentifierResolution::function(function);
                })
                .Case<AST::VariableDeclaration *>([](auto variable) {
                    return IdentifierResolution::global(cast<AST::IdentifierBinding>(&variable->getBinding()), false);
                })
                .Case<Type *>([](auto type) {
                    return IdentifierResolution::type(type);
                })
                .Case<AST::IdentifierBinding *>([](auto binding) {
                    return IdentifierResolution::global(binding, false);
                })
                .Default([](auto any) {
                    assert(false);
                    return IdentifierResolution::unresolved();
                })
            ;
            if (result) {
                return result;
            }
        }

        return IdentifierResolution::unresolved();
    }
};

#endif // LANG_scope_h
