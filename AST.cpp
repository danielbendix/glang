#include "AST.h"

namespace AST {
    
    void Node::print(std::ostream& os) const {
        PrintContext pc{os};
        this->print(pc);
    }

    PrintContext& PrintContext::operator<<(const Node& value) {
        value.print(*this);
        return *this;
    }

    void Block::print(PrintContext& pc) const {
        pc.indent();
        // TODO: Convert to range-based for loop
        for (int i = 0; i < size(); i++) {
            pc << (*this)[i];
        }
        pc.outdent();
    }

    void TypeLiteral::print(PrintContext& pc) const {
        pc << identifier;
    }

    void Identifier::print(PrintContext& pc) const { 
        pc << name; 
    }

    void BinaryExpression::print(PrintContext& pc) const {
        pc << *left << ' ' << int(op) << ' ' << *right;
    }

    void FunctionDeclaration::print(PrintContext& pc) const {
        pc << name << "(";
        for (auto& p : parameters) {
            pc << p.name << ": " << *p.type;
        }
        pc << ")";
        if (returnType) {
            pc << " -> " << *returnType;
        }
        pc << " {\n";
        pc.indent();
        for (auto const& d : declarations) {
            pc << *d;
        }
        pc.outdent();
        pc << "}";
    }
    void AssignmentStatement::print(PrintContext& pc) const {
        pc << *target << " = " << *value;
    }

    void IfStatement::print(PrintContext& pc) const {
        auto it = conditionals.cbegin();
        bool isElse = false;
        for (auto const& branch : conditionals) {
            if (isElse) {
                pc << " else if " << *branch.condition << " {\n";
            } else {
                pc.startLine();
                pc << "if " << *branch.condition << " {\n";
            }
            branch.block.print(pc);
            pc.startLine();
            pc << "}";

            isElse = true;
        }
        pc << "\n";
    }

    void ReturnStatement::print(PrintContext& pc) const {
        pc.startLine();
        pc << "return " << *expression << ";\n";
    }

    void StatementDeclaration::print(PrintContext& pc) const {
        pc << *statement;
    }

    void VarDeclaration::print(PrintContext& pc) const {
        pc << "var " << identifier;
        if (type) {
            pc << ": " << *type.get();
        }
        if (value) {
            pc << " = " << *value.get();
        }
    }

    void Literal::print(PrintContext& pc) const {
        using enum Type;
        switch (type) {
            case Boolean: 
                pc << internal.boolean;
                break;
            case Int8:
                pc << internal.int8;
                break;
            case Int16:
                pc << internal.int16;
                break;
            case Int32:
                pc << internal.int32;
                break;
            case Int64:
                pc << internal.int64;;
                break;
            case UInt8:
                pc << internal.uint8;
                break;
            case UInt16:
                pc << internal.uint16;
                break;
            case UInt32:
                pc << internal.uint32;
                break;
            case UInt64:
                pc << internal.uint64;
                break;
            case Float:
                pc << internal.float_;
                break;
            case Double:
                pc << internal.double_;
                break;
            case String:
                pc << '"' << *internal.string << '"';
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
