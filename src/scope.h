

#include "llvm/ADT/StringMap.h"

enum ScopeKind {
    SK_Scope_Global,
    SK_Scope_Local,


};

class GlobalScope {

};

class LocalScope {

};

class ClassScope {

};

struct Scope {
    ScopeKind kind;

    ~Scope() {

    }

};

template <typename T>
struct Global {
    bool isExternal;

    T meta;

    T& get() {
        return meta;
    }
};

template <typename Metadata>
struct Local {
    const std::string& name;
    int scope;
    int depth;
    int shadows;

    Metadata meta;

    Metadata& get() {
        return meta;
    }

    Local(const std::string& name, int scope, int depth, int shadows, Metadata meta) 
        : name{name}
        , scope{scope}
        , depth{depth}
        , shadows{shadows}
        , meta{meta} {}
};

template <typename Metadata>
class ScopeManager {
    int currentScope;
    int currentDepth;

    llvm::StringMap<int> symbols;
    std::vector<Local<Metadata>> locals;

public:
    void pushScope() {
        currentDepth += 1;
    }

    void popScope() {
        for (int i = locals.size() - 1; i >= 0; --i) {
            if (locals[i].depth == currentDepth) {
                int shadows = locals[i].shadows;
                if (shadows != -1) {
                    symbols.insert_or_assign(locals[i].name, shadows);
                }
                locals.pop_back();
            }
        }
    }

    void addLocal(const std::string& name, Metadata meta) {
        auto it = symbols.find(name);

        if (it != symbols.end()) {
            int shadows = it->getValue();
            it->setValue(locals.size());
            locals.emplace_back(name, currentScope, currentDepth, shadows, meta);
        } else {
            symbols.insert(std::make_pair(name, locals.size()));
            locals.emplace_back(name, currentScope, currentDepth, -1, meta);
        }
    }

    void addLocal(const std::string& name) {
        addLocal(name, Metadata{});
    }

    template <typename M = Metadata, typename = std::enable_if_t<!std::is_pointer_v<M>>>
    Metadata& resolve(const std::string& name) {
        auto it = symbols.find(name);

        if (it != symbols.end()) {
            int index = it->getValue();
            if (index >= 0) {
                return locals[index].meta;
            } else {

            }
        } else {
            return Metadata{};
        }
    }

    template <typename M = Metadata, typename = std::enable_if_t<std::is_pointer_v<M>>>
    Metadata resolve(const std::string& name) {
        auto it = symbols.find(name);

        if (it != symbols.end()) {
            int index = it->getValue();
            if (index >= 0) {
                return locals[index].meta;
            } else {
                // TODO: return global
                return nullptr;
            }
        } else {
            return nullptr;
        }
    }

    
};
