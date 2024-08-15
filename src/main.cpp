#include <fstream>
#include <iostream>

#include "cli.h"
#include "AST.h"
#include "builtins.h"
#include "diagnostic.h"
#include "scanner.h"
#include "parser.h"
#include "namespace.h"
#include "typecheck.h"
#include "codegen.h"
#include "control.h"

void initialize(SymbolTable& symbols)
{
    setupBuiltins(symbols);
}

ParsedFile parse(SymbolTable& symbols, std::string&& string) {
    try {
        auto parser = Parser{symbols, std::move(string)};
        return parser.parse();
    } catch (ParserException exception) {
        Diagnostic::writer().error(exception);
        exit(-1);
    }
}

std::unique_ptr<ModuleDef> validate(ParsedFile&& parsed, bool verbose = false) {
    auto moduleDef = createModuleDefinition(parsed.declarations);
    if (resolveNamesInModuleDefinition(*moduleDef).failed()) {
        exit(1);
    }
    if (verbose) std::cout << "Name resolution succeeded.\n";
    if (typecheckModuleDefinition(*moduleDef).failed()) {
        if (verbose) std::cout << "Type checking failed. Exiting...\n";
        exit(1);
    }
    if (verbose) std::cout << "Type check succeeded.\n";
    if (analyzeControlFlow(*moduleDef).failed()) {
        if (verbose) std::cout << "Control flow analysis failed. Exiting...\n";
        exit(1);
    }
    if (verbose) std::cout << "Control flow analysis succeeded\n";

    return moduleDef;
}

std::unique_ptr<llvm::Module> codegen(ModuleDef& moduleDef, bool verbose = false) {
    auto module = generateCode(moduleDef);
    if (!module) {
        if (verbose) std::cout << "Codegen failed. Exiting...\n";
        exit(1);
    }
    if (verbose) std::cout << "Code generation succeeded.\n";

    return module;
}

int main(int argc, char **argv)
{
    Options options = parseOptionsOrExit(std::span(argv, argc));

    SymbolTable symbols;
    ThreadContext threadContext{&symbols};

    initialize(symbols);

    std::string filename = options.files[0];
    std::ifstream file(filename, std::ios::in | std::ios::binary);
    if (file.fail()) {
        std::cout << "File " << filename << " does not exist. Exiting...\n";
        return 1;
    }

    std::unique_ptr<DiagnosticWriter> writer;
    if (options.flags.json) {
        writer.reset(new JSONDiagnosticWriter{std::cout});
        Diagnostic::setWriter(*writer);
    } else {
        writer.reset(new IODiagnosticWriter{std::cout});
        Diagnostic::setWriter(*writer);
    }

    file.seekg(0, file.end);
    size_t file_size = file.tellg();
    file.seekg(0, file.beg);

    std::string contents(file_size, '\0');

    file.read(contents.data(), file_size);

    auto parsed = parse(symbols, std::move(contents));

    std::visit([&](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, Validate>) {
            std::ignore = validate(std::move(parsed), options.flags.verbose);
        } else if constexpr (std::is_same_v<T, Codegen>) {
            if (arg.printCode) {
                for (const auto &d : parsed.declarations) {
                    std::cout << *d;
                }
            }
            auto module =  validate(std::move(parsed), options.flags.verbose);
            auto llvmModule = codegen(*module);

            if (arg.printIR && llvmModule) {
                llvm::outs() << *llvmModule;
            }

            // TODO: Write bitcode to file.
        } else {
            static_assert(always_false_v<T>, "Switch falls through.");
        }
    }, options.mode);
}
