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

    PrintContext& operator<<(PrintContext& pc, UnaryOperator op) {
        using enum UnaryOperator;
        switch (op) {
            case Negate: pc << '-'; break;
            case Not: pc << "not "; break;
        }
        return pc;
    }

    PrintContext& operator<<(PrintContext& pc, BinaryOperator op) {
        using enum BinaryOperator;
        switch (op) {
            case Add: pc << '+'; break;
            case Subtract: pc << '-'; break;
            case Multiply: pc << '*'; break;
            case Divide: pc << '/'; break;
            case Modulo: pc << '%'; break;

            case Equal: pc << "=="; break;
            case NotEqual: pc << "!="; break;

            case Greater: pc << '>'; break;
            case GreaterEqual: pc << ">="; break;
            case Less: pc << '<'; break;
            case LessEqual: pc << "<="; break;

            case LogicalAnd: pc << "and"; break;
            case LogicalOr: pc << "or"; break;
        }
        return pc;
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
        pc << *left << ' ' << op << ' ' << *right;
    }

    void UnaryExpression::print(PrintContext& pc) const {
        pc << op << *target;
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
        pc << "}\n";
    }

    void AssignmentStatement::print(PrintContext& pc) const {
        // FIXME: Assignment type
        pc.startLine();
        pc << *target << " = " << *value << ";\n";
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

    void WhileStatement::print(PrintContext& pc) const {
        pc.startLine();
        pc << "while " << *condition << " {\n";
        code.print(pc);
        pc.startLine();
        pc << "}\n";
    }

    void StatementDeclaration::print(PrintContext& pc) const {
        pc << *statement;
    }

    void VariableDeclaration::print(PrintContext& pc) const {
        pc.startLine();
        pc << (isMutable ? "var " : "let ") << identifier;
        if (type.node()) {
            pc << ": " << *type.node();
        }
        if (initial) {
            pc << " = " << *initial;
        }
        pc << ";\n";
    }

    void Literal::print(PrintContext& pc) const {
        using enum Type;
        switch (type) {
            case Boolean: 
                if (internal.boolean) {
                    pc << "true";
                } else {
                    pc << "false";
                }
                break;
            case Integer:
                pc << internal.integer;
                break;
            case Double:
                pc << internal.double_;
                break;
            case String:
                pc << '"' << *internal.string << '"';
                break;
        }
    }

    void CallExpression::print(PrintContext& pc) const {
        pc << *target << "(";

        bool addSeparator = false;
        for (const auto& argument : arguments) {
            if (addSeparator) {
                pc << ", ";
            } else {
                addSeparator = true;
            }
            pc << *argument;
        }
        pc << ")";
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
