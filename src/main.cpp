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
    std::cout << sizeof(AST::unique_ptr<AST::Declaration>) << "\n";
    auto parser = Parser{std::move(string)};

    try {
        IODiagnosticWriter writer{std::cout};
        auto pf = parser.parse();

        for (const auto &d : pf.declarations) {
            std::cout << *d;
        }

        auto globals = globalTable(pf.declarations);
        if (resolveNames(pf.declarations, globals.get())) {
            exit(1);
        }
        std::cout << "Name resolution succeeded.\n";
        if (typeCheckDeclarations(pf.declarations, *globals, writer).failed()) {
            exit(1);
        }
        std::cout << "Type check succeeded.\n";
        if (analyzeControlFlow(pf.declarations, writer)) {
            exit(1);
        }
        std::cout << "Control flow analysis succeeded\n";
        auto module = generateCode(pf.declarations);
        if (!module) {
            exit(1);
        }
        std::cout << "Code generation succeeded.\n";
    } catch (ParserException exception) {
        std::cout << "EXCEPTION CAUGHT:\n";
        std::cout << int(exception.cause) << "\n";
        std::cout << exception.token.line << ":" << exception.token.offset;
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
    std::string testString((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    compile(std::move(testString));
}
