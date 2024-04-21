#include "AST.h"

namespace AST {
    void Node::deleteNode(AST::Node *node) {
        switch (node->getKind()) {
            case NK_Decl_Variable:
                delete static_cast<VariableDeclaration *>(node);
                break;
            case NK_Decl_Function:
                delete static_cast<FunctionDeclaration *>(node);
                break;
            case NK_Decl_Struct:
                delete static_cast<StructDeclaration *>(node);
                break;
            case NK_Decl_Enum:
                delete static_cast<EnumDeclaration *>(node);
                break;
            case NK_Decl_Protocol:
                delete static_cast<ProtocolDeclaration *>(node);
                break;
            case NK_Decl_Statement:
                delete static_cast<StatementDeclaration *>(node);
                break;

            // TODO: reorder statements
            case NK_Stmt_Assignment:
                delete static_cast<AssignmentStatement *>(node);
                break;
            case NK_Stmt_If:
                delete static_cast<IfStatement *>(node);
                break;
            case NK_Stmt_Guard:
                delete static_cast<GuardStatement *>(node);
                break;
            case NK_Stmt_For:
                delete static_cast<ForStatement *>(node);
                break;
            case NK_Stmt_While:
                delete static_cast<WhileStatement *>(node);
                break;
            case NK_Stmt_Return:
                delete static_cast<ReturnStatement *>(node);
                break;
            case NK_Stmt_Expression:
                delete static_cast<ExpressionStatement *>(node);
                break;

           // TODO: Reorder expressions
            case NK_Expr_Literal:
                return delete static_cast<Literal *>(node);
            case NK_Expr_Identifier:
                return delete static_cast<Identifier *>(node);
            case NK_Expr_Self:
                return delete static_cast<Self *>(node);
            case NK_Expr_Unary:
                return delete static_cast<UnaryExpression *>(node);
            case NK_Expr_Binary:
                return delete static_cast<BinaryExpression *>(node);
            case NK_Expr_Call:
                return delete static_cast<CallExpression *>(node);
            case NK_Expr_Member_Access:
                return delete static_cast<MemberAccessExpression *>(node);

            case NK_Type_Literal:
                return delete static_cast<TypeLiteral *>(node);
                break;
            case NK_Type_Modifier:
                return delete static_cast<TypeModifier *>(node);
                break;
        }
    }
}

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
            case AddressOf: pc << '&'; break;
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
                         
            case BitwiseAnd: pc << '&'; break;
            case BitwiseOr: pc << '|'; break;
            case BitwiseXor: pc << '^'; break;

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

    void TypeModifier::print(PrintContext& pc) const {
        pc << *child;
        for (auto modifier : modifiers) {
            switch (modifier) {
                case Modifier::Pointer:
                    pc << '*';
                    break;
                case Modifier::Optional:
                    pc << '?';
                    break;
            }
        }
    }

    void Identifier::print(PrintContext& pc) const { 
        pc << name; 
    }

    void Self::print(PrintContext& pc) const { 
        pc << "self"; 
    }

    void BinaryExpression::print(PrintContext& pc) const {
        pc << *left << ' ' << op << ' ' << *right;
    }

    void UnaryExpression::print(PrintContext& pc) const {
        pc << op << *target;
    }

    void FunctionDeclaration::print(PrintContext& pc) const {
        pc.startLine();
        pc << "fn " << name << "(";
        for (auto& p : parameters) {
            // This could fail after type checking
            pc << p.name << ": " << *p.typeDeclaration;
        }
        pc << ")";
        if (returnTypeDeclaration) {
            pc << " -> " << *returnTypeDeclaration;
        }
        pc << " {\n";
        code.print(pc);
        pc << "}\n";
    }

    void StructDeclaration::print(PrintContext& pc) const {
        pc << "struct " << name << " {\n";
        pc.indent();
        for (auto& declaration : declarations) {
            pc << *declaration;
        }
        pc.outdent();
        pc.startLine();
        pc << "}\n";
    }

    void ProtocolDeclaration::print(PrintContext& pc) const {

    }

    void EnumDeclaration::print(PrintContext& pc) const {

    }

    void AssignmentStatement::print(PrintContext& pc) const {
        // FIXME: Assignment type
        pc.startLine();
        char const *opString;
        using enum AssignmentOperator;
        switch (this->op) {
            case Assign: opString = " = "; break;
            case AssignAdd: opString = " += "; break;
            case AssignSub: opString = " -= "; break;
            case AssignMultiply: opString = " *= "; break;
            case AssignDivide: opString = " /= "; break;
        }

        pc << *target << opString << *value << ";\n";
    }

    void IfStatement::print(PrintContext& pc) const {
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

    void GuardStatement::print(PrintContext& pc) const {
        pc.startLine();
        pc << "guard " << *condition << " else {\n";
        block.print(pc);
        pc.startLine();
        pc << "}\n";
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

    void ForStatement::print(PrintContext& pc) const {
        pc.startLine();
        pc << "for (";
        if (auto *initialization = getInitialization()) {
            pc << *initialization;
        }
        pc << "; ";
        pc << getCondition() << ";";
        if (auto *increment = getIncrement()) {
            pc << *increment;
        }
        pc << ") {\n";
        code.print(pc);
        pc.startLine();
        pc << "}\n";
    }

    void ExpressionStatement::print(PrintContext& pc) const {
        pc.startLine();
        pc << getExpression();
        pc << ";\n";
    }

    void StatementDeclaration::print(PrintContext& pc) const {
        pc << *statement;
    }

    void VariableDeclaration::print(PrintContext& pc) const {
        pc.startLine();
        pc << (isMutable ? "var " : "let ") << identifier;
        if (typeDeclaration) {
            pc << ": " << *typeDeclaration;
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
            case Nil:
                pc << "nil";
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

    void MemberAccessExpression::print(PrintContext& pc) const {
        pc << *target << '.' << memberName;
    }
}

std::ostream& operator<<(std::ostream& os, const AST::Node& node) {
    node.print(os);
    return os;
}
