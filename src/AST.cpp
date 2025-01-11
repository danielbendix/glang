#include "AST.h"
#include "AST_Visitor.h"
#include "templates.h"

#include "llvm/ADT/TypeSwitch.h"
using llvm::TypeSwitch;

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
            case NK_Expr_Literal_Nil:
                return delete static_cast<NilLiteral *>(node);
            case NK_Expr_Literal_False:
            case NK_Expr_Literal_True:
                return delete static_cast<BooleanLiteral *>(node);
            case NK_Expr_Literal_Integer:
                return delete static_cast<IntegerLiteral *>(node);
            case NK_Expr_Literal_Floating:
                return delete static_cast<FloatingPointLiteral *>(node);
            case NK_Expr_Literal_Character:
                return delete static_cast<CharacterLiteral *>(node);
            case NK_Expr_Literal_String:
                return delete static_cast<StringLiteral *>(node);
            case NK_Expr_Identifier:
                return delete static_cast<Identifier *>(node);
            case NK_Expr_Self:
                return delete static_cast<Self *>(node);
            case NK_Expr_Unary:
                return delete static_cast<UnaryExpression *>(node);
            case NK_Expr_Binary:
                return delete static_cast<BinaryExpression *>(node);
            case NK_Expr_Intrinsic:
                return delete static_cast<IntrinsicExpression *>(node);
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

    FileLocation Node::getFileLocation() const {
        return AST::visit(*this, [](auto& node) {
            return node.getFileLocation();
        });
    }

    FileLocation TypeLiteral::getFileLocation() const {
        return {offset, name.length()};
    }

    FileLocation TypeModifier::getFileLocation() const {
        // TODO: Get total length of modifiers.
        return {offset, 1};
    }

    FileLocation Identifier::getFileLocation() const { 
        return {offset, name.length()};
    }

    FileLocation Self::getFileLocation() const { 
        return {offset, 4};
    }

    FileLocation BinaryExpression::getFileLocation() const {
        auto getLength = [](BinaryOperator op) -> u32 {
            switch (op) {
                case BinaryOperator::OpenRange:
                case BinaryOperator::ClosedRange:
                    return 3;
                case BinaryOperator::Add:
                case BinaryOperator::Subtract:
                case BinaryOperator::Multiply:
                case BinaryOperator::Divide:
                case BinaryOperator::Modulo:
                    return 1;
                case BinaryOperator::ShiftLeft:
                case BinaryOperator::ShiftRight:
                    return 2;
                case BinaryOperator::BitwiseAnd:
                case BinaryOperator::BitwiseOr:
                case BinaryOperator::BitwiseXor:
                    return 1;
                case BinaryOperator::Equal:
                case BinaryOperator::NotEqual:
                    return 2;
                case BinaryOperator::Less:
                case BinaryOperator::Greater:
                    return 1;
                case BinaryOperator::LessEqual:
                case BinaryOperator::GreaterEqual:
                    return 2;
                case BinaryOperator::LogicalAnd:
                    return 3;
                case BinaryOperator::LogicalOr:
                    return 2;
            }
        };
        u32 length = getLength(op);
        return {offset, length};
    }

    FileLocation IntrinsicExpression::getFileLocation() const {
        return {offset, name.length() + 1};
    }

    FileLocation UnaryExpression::getFileLocation() const {
        auto getLength = [](UnaryOperator op) -> u32 {
            using enum UnaryOperator;
            switch (op) {
                case Negate: 
                    return 1;
                case BitwiseNegate:
                    return 1;
                case Not:
                    return 3;
                case AddressOf:
                case PrefixDereference:
                case PostfixDereference:
                    return 1;
                case ForceUnwrap:
                    return 1;
                case ZeroExtend: 
                case SignExtend:
                case IntegerToFP:
                case FPExtend:
                case OptionalWrap: 
                    llvm_unreachable("Cannot get file location for wrapper node.");
            }
        };
        return {offset, getLength(op)};
    }

    FileLocation FunctionDeclaration::getFileLocation() const {
        return {offset, name.length()};
    }

    FileLocation FunctionDeclaration::getClosingBracketLocation() const {
        return {closingBracket, 1};
    }

    FileLocation StructDeclaration::getFileLocation() const {
        return {offset, name.length()};
    }

    FileLocation ProtocolDeclaration::getFileLocation() const {
        return {offset, name.length()};
    }

    FileLocation EnumDeclaration::getFileLocation() const {
        return {offset, name.length()};
    }

    FileLocation AssignmentStatement::getFileLocation() const {
        return {offset, 1};
    }

    FileLocation CompoundAssignmentStatement::getFileLocation() const {
        auto getLength = [](BinaryOperator op) -> u32 {
            using enum BinaryOperator;
            switch (op) {
                case OpenRange:
                case ClosedRange:
                case Equal:
                case NotEqual:
                case Less:
                case LessEqual:
                case Greater:
                case GreaterEqual:
                case LogicalAnd:
                case LogicalOr:
                    llvm_unreachable("Unsupported binary operator for compound assignment.");
                case Add:
                case Subtract:
                case Multiply:
                case Divide:
                case Modulo:
                    return 2;
                case AST::BinaryOperator::ShiftLeft:
                case AST::BinaryOperator::ShiftRight:
                    return 3;
                case AST::BinaryOperator::BitwiseAnd:
                case AST::BinaryOperator::BitwiseOr:
                case AST::BinaryOperator::BitwiseXor:
                    return 2;
            }

        };
        return {offset, getLength(op)};
    }

    FileLocation IfStatement::getFileLocation() const {
        return {offset, 2};
    }

    FileLocation GuardStatement::getFileLocation() const {
        return {offset, 5};
    }

    FileLocation ReturnStatement::getFileLocation() const {
        return {offset, 6};
    }

    FileLocation WhileStatement::getFileLocation() const {
        return {offset, 5};
    }

    FileLocation ForStatement::getFileLocation() const {
        return {offset, 3};
    }

    FileLocation BreakStatement::getFileLocation() const {
        return {offset, 5};
    }

    FileLocation ContinueStatement::getFileLocation() const {
        return {offset, 8};
    }

    FileLocation ExpressionStatement::getFileLocation() const {
        return expression->getFileLocation();
    }

    FileLocation StatementDeclaration::getFileLocation() const {
        return statement->getFileLocation();
    }

    FileLocation VariableDeclaration::getFileLocation() const {
        return {offset, 3};
    }

    FileLocation NilLiteral::getFileLocation() const {
        return {offset, 3};
    }

    FileLocation BooleanLiteral::getFileLocation() const {
        u32 length = getValue() ? 4 : 5;
        return {offset, length};
    }

    FileLocation IntegerLiteral::getFileLocation() const {
        // FIXME: Fix for folded literals.
        return {offset, value.getLength()};
    }

    FileLocation FloatingPointLiteral::getFileLocation() const {
        // TODO: Needs length
        llvm_unreachable(__FUNCTION__);
    }

    FileLocation CharacterLiteral::getFileLocation() const {
        // TODO: Needs length.
        llvm_unreachable(__FUNCTION__);
    }

    FileLocation StringLiteral::getFileLocation() const {
        // TODO: Needs length.
        llvm_unreachable(__FUNCTION__);
    }

    FileLocation CallExpression::getFileLocation() const {
        // TODO: Get closing parenthesis.
        return {offset, 1};
    }

    FileLocation SubscriptExpression::getFileLocation() const {
        // TODO: Get closing bracket.
        return {offset, 1};
    }

    FileLocation InitializerExpression::getFileLocation() const {
        return {offset, 1};
    }

    FileLocation MemberAccessExpression::getFileLocation() const {
        return {offset, memberName.length()};
    }

    FileLocation InferredMemberAccessExpression::getFileLocation() const {
        // TODO: This should perhaps include the dot.
        return {offset, memberName.length()};
    }

    // Bindings

    FileLocation IdentifierBinding::getFileLocation() const {
        return {offset, identifier.length()};
    }
}

namespace AST {
    void Node::print(std::ostream& os) const {
        PrintContext pc{os};
        pc << *this;
    }

    PrintContext& PrintContext::operator<<(const Node& node) {
        visit(node, [this](auto& node) {
            node.print(*this);
        });
        return *this;
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

    PrintContext& operator<<(PrintContext& pc, Condition condition) {
        TypeSwitch<AST::Condition>(condition)
            .Case<AST::VariableDeclaration *>([&](AST::VariableDeclaration *variable) {
                pc << *variable;
            })
            .Case<AST::Expression *>([&](AST::Expression *expression) {
                pc << *expression;
            });
        return pc;
    }

    PrintContext& operator<<(PrintContext& pc, const vector<Condition>& conditions) {
        pc.withSeparator(", ", conditions);
        return pc;
    }

    PrintContext& operator<<(PrintContext& pc, const FunctionParameter& parameter) {
        return pc << parameter.name << ": " << *parameter.typeDeclaration;
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
                case Modifier::Array:
                    pc << "[]";
                case Modifier::UnboundedArray:
                    pc << "[!]";
                case Modifier::Location:
                    pc << '&';
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

    void IntrinsicExpression::print(PrintContext& pc) const {
        pc << '#' << name;
        if (hasTypeArguments) {
            pc << '<';
            pc.withSeparator(", ", typeArguments);
            pc << '>';
        }
        if (hasCall) {
            pc << '(';
            pc.withSeparator(", ", arguments);
            pc << ')';
        }
    }

    void UnaryExpression::print(PrintContext& pc) const {
        using enum UnaryOperator;
        switch (op) {
            case Negate: pc << '-'; break;
            case BitwiseNegate: pc << '~'; break;
            case Not: pc << "not "; break;
            case AddressOf: pc << '&'; break;
            case PrefixDereference: pc << '*'; break;
            case PostfixDereference: break;
            case ForceUnwrap: break;
            case ZeroExtend: pc << "#zext("; break;
            case SignExtend: pc << "#sext("; break;
            case IntegerToFP: pc << "#itoFP("; break;
            case FPExtend: pc << "#fpext("; break;
            case OptionalWrap: pc << "#wrap("; break;
        }
        pc << *target;
        switch (op) {
            case Negate:
            case BitwiseNegate:
            case Not:
            case AddressOf:
            case PrefixDereference:
                  break;
            case PostfixDereference: pc << '@'; break;
            case ForceUnwrap: pc << '!'; break;
            case ZeroExtend: pc << ")"; break;
            case SignExtend: pc << ")"; break;
            case IntegerToFP: pc << ")"; break;
            case FPExtend: pc << ")"; break;
            case OptionalWrap: pc << ")"; break;
        }
    }

    void FunctionDeclaration::print(PrintContext& pc) const {
        pc.startLine();
        pc << "fn " << name << "(";
        pc.withSeparator(", ", parameters);
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
        for (auto const& branch : branches) {
            if (isElse) {
                pc << " else if " << branch.conditions << " {\n";
            } else {
                pc.startLine();
                pc << "if " << branch.conditions << " {\n";
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
        pc << "guard " << conditions << " else {\n";
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
        pc << "while " << conditions << " {\n";
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

    void NilLiteral::print(PrintContext& pc) const {
        pc << "nil";
    }

    void BooleanLiteral::print(PrintContext& pc) const {
        pc << (getValue() ? "true" : "false");
    }

    void IntegerLiteral::print(PrintContext& pc) const {
        switch (value.getType()) {
            case Type::Binary:
                pc << "0b";
                return pc.printInteger(value, 2);
            case Type::Octal:
                pc << "0o";
                return pc.printInteger(value, 8);
            case Type::Decimal:
                return pc.printInteger(value, 10);
            case Type::Hexadecimal:
                pc << "0x";
                return pc.printInteger(value, 16);
        }
    }

    void FloatingPointLiteral::print(PrintContext& pc) const {
        pc << value;
    }

    void CharacterLiteral::print(PrintContext& pc) const {
        Character codepoint = value;
        pc << '\'';
        if (codepoint <= 0x7F) {
            pc << static_cast<char>(codepoint);
        } else if (codepoint <= 0x7FF) {
            pc << static_cast<char>(0xC0 | ((codepoint >> 6) & 0x1F))
               << static_cast<char>(0x80 | (codepoint & 0x3F));
        } else if (codepoint <= 0xFFFF) {
            pc << static_cast<char>(0xE0 | ((codepoint >> 12) & 0x0F))
               << static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F))
               << static_cast<char>(0x80 | (codepoint & 0x3F));
        } else if (codepoint <= 0x10FFFF) {
            pc << static_cast<char>(0xF0 | ((codepoint >> 18) & 0x07))
               << static_cast<char>(0x80 | ((codepoint >> 12) & 0x3F))
               << static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F))
               << static_cast<char>(0x80 | (codepoint & 0x3F));
        } else {
            pc << "[Invalid codepoint: " << codepoint << "]";
        }
        pc << '\'';
    }

    void StringLiteral::print(PrintContext& pc) const {
        pc << value;
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
