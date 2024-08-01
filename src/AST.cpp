#include "AST.h"
#include "templates.h"


namespace AST {
    void Node::deleteNode(AST::Node *node) {
        switch (node->getKind()) {
            case NK_Decl_Variable:
                return delete static_cast<VariableDeclaration *>(node);
            case NK_Decl_Function:
                return delete static_cast<FunctionDeclaration *>(node);
            case NK_Decl_Struct:
                return delete static_cast<StructDeclaration *>(node);
            case NK_Decl_Enum:
                return delete static_cast<EnumDeclaration *>(node);
            case NK_Decl_Protocol:
                return delete static_cast<ProtocolDeclaration *>(node);
            case NK_Decl_Statement:
                return delete static_cast<StatementDeclaration *>(node);

            // TODO: reorder statements
            case NK_Stmt_Assignment:
                return delete static_cast<AssignmentStatement *>(node);
            case NK_Stmt_Compound_Assignment:
                return delete static_cast<CompoundAssignmentStatement *>(node);
            case NK_Stmt_If:
                return delete static_cast<IfStatement *>(node);
            case NK_Stmt_Guard:
                return delete static_cast<GuardStatement *>(node);
            case NK_Stmt_For:
                return delete static_cast<ForStatement *>(node);
            case NK_Stmt_While:
                return delete static_cast<WhileStatement *>(node);
            case NK_Stmt_Return:
                return delete static_cast<ReturnStatement *>(node);
            case NK_Stmt_Break:
                return delete static_cast<BreakStatement *>(node);
            case NK_Stmt_Continue:
                return delete static_cast<ContinueStatement *>(node);
            case NK_Stmt_Expression:
                return delete static_cast<ExpressionStatement *>(node);

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
            case NK_Expr_Subscript:
                return delete static_cast<SubscriptExpression *>(node);
            case NK_Expr_Member_Access:
                return delete static_cast<MemberAccessExpression *>(node);
            case NK_Expr_Inferred_Member_Access:
                return delete static_cast<InferredMemberAccessExpression *>(node);
            case NK_Expr_Initializer:
                return delete static_cast<InitializerExpression *>(node);

            case NK_Type_Literal:
                return delete static_cast<TypeLiteral *>(node);
            case NK_Type_Modifier:
                return delete static_cast<TypeModifier *>(node);

            case NK_Binding_Identifier:
                return delete static_cast<IdentifierBinding *>(node);
        }
        llvm_unreachable("Unsupported node kind.");
    }
}

namespace AST {
    std::ostream& operator<<(std::ostream& os, const Location& location) {
        if (location.length == 1) {
            return os << '[' << location.line << ':' << location.column <<']';
        } else {
            return os << '[' << location.line << ':' << location.column << '-' << location.column + location.length - 1 <<']';
        }
    }

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
            case Negate: return pc << '-';
            case BitwiseNegate: return pc << '~';
            case Not: return pc << "not ";
            case AddressOf: return pc << '&';
            case Dereference: return pc << '*';
            case ZeroExtend: return pc << "#zext ";
            case SignExtend: return pc << "#sext ";
            case IntegerToFP: return pc << "#itoFP ";
            case FPExtend: return pc << "#fpext ";
            case OptionalWrap: return pc << "#wrap";
}
        return pc;
    }

    PrintContext& operator<<(PrintContext& pc, BinaryOperator op) {
        using enum BinaryOperator;
        switch (op) {
            case OpenRange: return pc << "..<";
            case ClosedRange: return pc << "...";

            case Add: return pc << '+';
            case Subtract: return pc << '-';
            case Multiply: return pc << '*';
            case Divide: return pc << '/';
            case Modulo: return pc << '%';
                         
            case BitwiseAnd: return pc << '&';
            case BitwiseOr: return pc << '|';
            case BitwiseXor: return pc << '^';

            case ShiftLeft: return pc << "<<";
            case ShiftRight: return pc << ">>";

            case Equal: return pc << "==";
            case NotEqual: return pc << "!=";

            case Greater: return pc << '>';
            case GreaterEqual: return pc << ">=";
            case Less: return pc << '<';
            case LessEqual: return pc << "<=";

            case LogicalAnd: return pc << "and";
            case LogicalOr: return pc << "or";
        }
    }

    void Block::print(PrintContext& pc) const {
        pc.indent();
        for (auto& declaration : *this) {
            pc << declaration;
        }
        pc.outdent();
    }

    void TypeLiteral::print(PrintContext& pc) const {
        pc << name;
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
        bool needsSeparator = false;
        for (auto& p : parameters) {
            if (needsSeparator) {
                pc << ", ";
            } else {
                needsSeparator = true;
            }
            pc << p.name << ": " << *p.typeDeclaration;
        }
        pc << ")";
        if (returnTypeDeclaration) {
            pc << " -> " << *returnTypeDeclaration;
        }
        pc << " {\n";
        code.print(pc);
        pc.startLine();
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
        pc.startLine();
        char const *opString;
        pc << *target << " = " << *value << ";\n";
    }

    void CompoundAssignmentStatement::print(PrintContext& pc) const {
        pc.startLine();

        pc << *target;

        pc << ' ' << op << "= " << *operand << ";\n";
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
        if (auto value = getValue()) {
            pc << "return " << *value << ";\n";
        } else {
            pc << "return;\n";
        }
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
        pc << "for " << *binding << " in " << *iterable << " {\n";
        code.print(pc);
        pc.startLine();
        pc << "}\n";
    }

    void BreakStatement::print(PrintContext& pc) const {
        pc.startLine();
        pc << "break;\n";
    }

    void ContinueStatement::print(PrintContext& pc) const {
        pc.startLine();
        pc << "continue;\n";
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
        pc << (isMutable ? "var " : "let ") << *binding;
        if (typeDeclaration) {
            pc << ": " << *typeDeclaration;
        }
        if (initial) {
            pc << " = " << *initial;
        }
        pc << ";\n";
    }

    void Literal::print(PrintContext& pc) const {
        std::visit([&](auto&& arg) {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, bool>) {
                pc << (arg ? "true" : "false");
            } else if constexpr (std::is_same_v<T, llvm::APInt>) {
                switch (integerType) {
                case IntegerType::Binary:
                    pc << "0b";
                    return pc.printInteger(arg, 2);
                case IntegerType::Octal:
                    pc << "0o";
                    return pc.printInteger(arg, 8);
                case IntegerType::Decimal:
                    return pc.printInteger(arg, 10);
                case IntegerType::Hexadecimal:
                    pc << "0o";
                    return pc.printInteger(arg, 16);
                }
                pc.printInteger(arg, 10);
            } else if constexpr (std::is_same_v<T, double>) {
                pc << arg;
            } else if constexpr (std::is_same_v<T, std::string>) {
                pc << arg;
            } else if constexpr (std::is_same_v<T, std::monostate>) {
                pc << "nil";
            } else { 
                static_assert(always_false_v<T>, "Switch falls through.");
            }
        }, internal);
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

    void SubscriptExpression::print(PrintContext& pc) const {
        pc << *target << "[" << *index << "]";
    }

    void InitializerExpression::print(PrintContext& pc) const {
        if (identifier) {
            pc << *identifier << " ";
        }
        pc << "{\n";
        pc.indent();

        for (const auto& pair : pairs) {
            pc.startLine();
            pc << pair.first->getMemberName() << " = " << *pair.second << ",\n";
        }

        pc.outdent();
        pc.startLine();
        pc << "}";
    }

    void MemberAccessExpression::print(PrintContext& pc) const {
        pc << *target << '.' << memberName;
    }

    void InferredMemberAccessExpression::print(PrintContext& pc) const {
        pc << '.' << memberName;
    }

    // Bindings

    void IdentifierBinding::print(PrintContext& pc) const {
        pc << identifier;
    }
}

std::ostream& operator<<(std::ostream& os, const AST::Node& node) {
    node.print(os);
    return os;
}
