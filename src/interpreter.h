#include <AST.h>

enum class InterpretResult {
    OK,
    TypeError,
    RuntimeError,
};

InterpretResult interpret(std::vector<std::unique_ptr<AST::Declaration>>& program);
