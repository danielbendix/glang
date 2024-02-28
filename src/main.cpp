#include <fstream>
#include <iostream>

#include "scanner.h"
#include "parser.h"
#include "namespace.h"
#include "typecheck.h"
#include "AST.h"



void compile(std::string&& string)
{
    auto parser = Parser{std::move(string)};

    try {
        auto pf = parser.parse();

        auto globals = globalTable(pf.declarations);

        for (const auto &d : pf.declarations) {
            std::cout << *d;
        }

        bool typeCheckedGlobals = typeCheckDeclarations(pf.declarations, *globals);


        std::cout << typeCheckedGlobals;


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
