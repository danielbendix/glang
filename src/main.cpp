#include <fstream>
#include <iostream>

#include "AST.h"
#include "diagnostic.h"
#include "scanner.h"
#include "parser.h"
#include "namespace.h"
#include "typecheck.h"
#include "codegen.h"
#include "control.h"

void compile(std::string&& string)
{
    auto parser = Parser{std::move(string)};

    std::cout << "sizeof(llvm::Module)" << sizeof(llvm::Module) << '\n';

    try {
        IODiagnosticWriter writer{std::cout};
        Diagnostic::setWriter(writer);

        auto pf = parser.parse();

        for (const auto &d : pf.declarations) {
            std::cout << *d;
        }

        auto moduleDef = createModuleDefinition(pf.declarations);
        if (resolveNamesInModuleDefinition(*moduleDef).failed()) {
            exit(1);
        }
        std::cout << "Name resolution succeeded.\n";
        if (typecheckModuleDefinition(*moduleDef).failed()) {
            std::cout << "Type checking failed. Exiting...\n";
            exit(1);
        }
        std::cout << "Type check succeeded.\n";
        if (analyzeControlFlow(*moduleDef).failed()) {
            std::cout << "Control flow analysis failed. Exiting...\n";
            exit(1);
        }
        std::cout << "Control flow analysis succeeded\n";
        auto module = generateCode(*moduleDef);
        if (!module) {
            std::cout << "Codegen failed. Exiting...\n";
            exit(1);
        }
        std::cout << "Code generation succeeded.\n";
    } catch (ParserException exception) {
        std::cout << "EXCEPTION CAUGHT:\n";
        std::cout << int(exception.cause) << "\n";
        std::cout << exception.token.line << ":" << exception.token.offset << "\n";
        std::cout << exception.token.chars << "\n";
    }
}

int main(int argc, char **argv)
{
    std::string filename;
    if (argc <= 1) {
        std::cout << "No input file specified. Exiting." << std::endl;
    }
    filename = std::string(argv[1]);
    std::cout << "Using " << filename << "\n";

    std::ifstream file(filename, std::ios::in | std::ios::binary);

    std::cout << file.good() << file.bad() << "\n";
    if (file.fail()) {
        std::cout << "File " << filename << " does not exist. Exiting...\n";
        return 1;
    }

    std::string testString((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    compile(std::move(testString));
}
