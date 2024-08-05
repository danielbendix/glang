
type_pairs = [
    ("TK_Void", "VoidType"),
    ("TK_Boolean", "BooleanType"),
    ("TK_Num_Integer", "IntegerType"),
    ("TK_Num_FP", "FPType"),
    ("TK_String", "StringType"),
    ("TK_Pointer", "PointerType"),
    ("TK_Optional", "OptionalType"),
    ("TK_Function", "FunctionType"),
    ("TK_Struct", "StructType"),
    ("TK_Enum", "EnumType"),
    #("TK_Protocol", "ProtocolType"),
    ("TK_Array", "ArrayType"),
    ("TK_Range", "RangeType"),
]

ast_pairs = [
    ("NK_Decl_Variable", "VariableDeclaration"),
    ("NK_Decl_Function", "FunctionDeclaration"),
    ("NK_Decl_Struct", "StructDeclaration"),
    ("NK_Decl_Enum", "EnumDeclaration"),
    ("NK_Decl_Protocol", "ProtocolDeclaration"),
    ("NK_Decl_Statement", "StatementDeclaration"),

    ("NK_Stmt_Assignment", "AssignmentStatement"),
    ("NK_Stmt_Compound_Assignment", "CompoundAssignmentStatement"),
    ("NK_Stmt_If", "IfStatement"),
    ("NK_Stmt_Guard", "GuardStatement"),
    ("NK_Stmt_Return", "ReturnStatement"),
    ("NK_Stmt_While", "WhileStatement"),
    ("NK_Stmt_For", "ForStatement"),
    ("NK_Stmt_Break", "BreakStatement"),
    ("NK_Stmt_Continue", "ContinueStatement"),
    ("NK_Stmt_Expression", "ExpressionStatement"),

    ("NK_Expr_Identifier", "Identifier"),
    ("NK_Expr_Self", "Self"),
    ("NK_Expr_Literal", "Literal"),
    ("NK_Expr_Unary", "UnaryExpression"),
    ("NK_Expr_Binary", "BinaryExpression"),
    ("NK_Expr_Call", "CallExpression"),
    ("NK_Expr_Subscript", "SubscriptExpression"),
    ("NK_Expr_Initializer", "InitializerExpression"),
    ("NK_Expr_Member_Access", "MemberAccessExpression"),
    ("NK_Expr_Inferred_Member_Access", "InferredMemberAccessExpression"),

    ("NK_Type_Literal", "TypeLiteral"),
    ("NK_Type_Modifier", "TypeModifier"),

    ("NK_Binding_Identifier", "IdentifierBinding"),
]

def create_case(tag, name, const: bool = False) -> str:
    return (
        f"case {tag}:\n" +
        f"    return std::invoke(visitor, *static_cast<{'const ' if const else ''}{name} *>(&node));"
    )

#print("\n".join([create_case(*args) for args in ast_pairs]))
print("\n".join([create_case(*args, const=True) for args in type_pairs]))
