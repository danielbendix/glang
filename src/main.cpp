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

#include "llvm/Support/raw_ostream.h"
#include "llvm/Bitcode/BitcodeWriter.h"

#include "util/stopwatch.h"

void initialize(SymbolTable& symbols)
{
    auto cpu = detectCPU();
    Architecture::populate(cpu);
    setupBuiltins(symbols, Architecture::current());
}

ParsedFile parse(SymbolTable& symbols, std::string&& string) {
    try {
        Stopwatch watch;
        auto parser = Parser{symbols, std::move(string)};
        auto result = parser.parse();
        watch.lap("Parsed file.");
        return result;
    } catch (ParserException exception) {
        Diagnostic::writer().error(exception);
        exit(-1);
    }
}

std::unique_ptr<ModuleDef> validate(ParsedFile&& parsed, bool verbose = false) {
    Stopwatch watch;
    auto moduleDef = createModuleDefinition(parsed.declarations);
    watch.lap("Created module definition");
    if (typecheckModuleDefinition(*moduleDef).failed()) {
        if (verbose) std::cout << "Sema phase failed. Exiting...\n";
        exit(1);
    }
    watch.lap("Typechecked.");
    if (verbose) std::cout << "Type check succeeded.\n";
    if (analyzeControlFlow(*moduleDef).failed()) {
        if (verbose) std::cout << "Control flow analysis failed. Exiting...\n";
        exit(1);
    }
    watch.lap("Checked control flow.");
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
    Stopwatch totalWatch;
    Options options = parseOptionsOrExit(std::span(argv, argc));

    SymbolTable symbols;
    ThreadContext threadContext{&symbols};

    initialize(symbols);

    Stopwatch watch;

    std::string filename = options.files[0];
    std::ifstream file(filename, std::ios::in | std::ios::binary);
    if (file.fail()) {
        std::cout << "File " << filename << " does not exist. Exiting...\n";
        return 1;
    }

    watch.lap("Opened file.");

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

    watch.lap("Searched file.");

    std::string contents(file_size, '\0');

    watch.lap("Allocated memory.");

    file.read(contents.data(), file_size);
    watch.lap("Read file.");

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

            if (llvmModule) {
                if (arg.printIR) {
                    llvm::outs() << *llvmModule;
                }
                if (arg.outputFile) {
                    std::error_code error;
                    llvm::raw_fd_ostream output(*arg.outputFile, error);

                    if (error) {
                        std::cerr << "Could not open file '" << *arg.outputFile << "' for writing.";
                    } else {
                        llvm::WriteBitcodeToFile(*llvmModule, output);
                        output.close();
                    }
                }
            }
        } else {
            static_assert(always_false_v<T>, "Switch falls through.");
        }
    }, options.mode);

    totalWatch.lap("Total");
}
