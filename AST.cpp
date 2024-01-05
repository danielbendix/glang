#include "AST.h"

namespace AST {
    void FunctionDeclaration::print(std::ostream& os) const {
        os << name << "(";
        for (auto& p : parameters) {
            os << p.name << ": " << *p.type.get();
        }
        os << ")";
        if (returnType) {
            os << " -> " << *returnType.get();
        }
        os << " {\n";
        for (auto const& d : declarations) {
            os << *d.get();
        }
        os << "}";
    }
    void AssignmentStatement::print(std::ostream& os) const {
        target->print(os);
        os << " = ";
        value->print(os);
    }

    void IfStatement::print(std::ostream& os) const {
        auto it = conditionals.cbegin();
        bool isElse = false;
        for (auto const& branch : conditionals) {
            if (isElse) {
                os << " else if " << *branch.condition << " {\n";
            } else {
                os << "if " << *branch.condition << " {\n";
            }
            // TODO: Convert to range-based for loop
            for (int i = 0; i < branch.block.size(); i++) {
                os << branch.block[i];
            }
            os << "}";
            isElse = true;
        }
    }

    void ReturnStatement::print(std::ostream& os) const {
        os << "return " << *expression << ";\n";
    }

    void StatementDeclaration::print(std::ostream& os) const {
        os << *statement.get();
    }

    void VarDeclaration::print(std::ostream& os) const {
        os << "var " << identifier;
        if (type) {
            os << ": " << *type.get();
        }
        if (value) {
            os << " = " << *value.get();
        }
    }

    void Literal::print(std::ostream& os) const {
        using enum Type;
        switch (type) {
            case Boolean: 
                std::cout << internal.boolean;
                break;
            case Int8:
                std::cout << internal.int8;
                break;
            case Int16:
                std::cout << internal.int16;
                break;
            case Int32:
                std::cout << internal.int32;
                break;
            case Int64:
                std::cout << internal.int64;;
                break;
            case UInt8:
                std::cout << internal.uint8;
                break;
            case UInt16:
                std::cout << internal.uint16;
                break;
            case UInt32:
                std::cout << internal.uint32;
                break;
            case UInt64:
                std::cout << internal.uint64;
                break;
            case Float:
                std::cout << internal.float_;
                break;
            case Double:
                std::cout << internal.double_;
                break;
            case String:
                std::cout << '"' << *internal.string << '"';
                break;
        }
    }
}

std::ostream& operator<<(std::ostream& os, const AST::Node& node) {
    node.print(os);
    return os;
}

std::ostream& operator<<(std::ostream& os, AST::Node& node) {
    node.print(os);
    return os;
}
