from dataclasses import dataclass

@dataclass
class VisitedType:
    typename: str
    argumentName: str
    name_in_function: str | None

@dataclass
class Argument:
    typename: str
    name: str


declarations = [
    VisitedType("VariableDeclaration", "variable", None),
    VisitedType("FunctionDeclaration", "function", None),
    VisitedType("StructDeclaration", "structDeclaration", None),
    VisitedType("ProtocolDeclaration", "protocol", None),
    VisitedType("StatementDeclaration", "statement", None),
]

statements = [
    VisitedType("ReturnStatement", "returnStatement", None),
    VisitedType("AssignmentStatement", "assignment", None),
    VisitedType("IfStatement", "ifStatement", None),
    VisitedType("WhileStatement", "whileStatement", None),
    VisitedType("ForStatement", "forStatement", None),
    VisitedType("ExpressionStatement", "expression", None),
]

expressions = [
    VisitedType("Literal", "literal", None),
    VisitedType("Identifier", "identifier", None),
    VisitedType("UnaryExpression", "unary", None),
    VisitedType("BinaryExpression", "binary", None),
    VisitedType("CallExpression", "call", None),
]

def generate(t: VisitedType, return_type: str, arguments: list[Argument] = []):
    return f"{return_type} visit{t.typename}(AST::{t.typename}& {t.argumentName}) {{}}"

# Output is pasted into src/AST_Visitor.h; emit clang-format markers so the
# formatter never fights regenerated content.
print("// clang-format off")
for declaration in declarations:
    print(generate(declaration, "bool"))
print()

for statement in statements:
    print(generate(statement, "bool"))
print()

for expression in expressions:
    print(generate(expression, "bool"))
print("// clang-format on")
