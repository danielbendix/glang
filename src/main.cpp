#include <fstream>
#include <iostream>
#include <variant>

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

void initialize(SymbolTable& symbols)
{
    auto cpu = detectCPU();
    Architecture::populate(cpu);
    setupBuiltins(symbols, Architecture::current());
}

void validate(Module& module, bool verbose = false) {
    if (typecheckModule(module).failed()) {
        Diagnostic::flush();
        if (verbose) std::cout << "Sema phase failed. Exiting...\n";
        exit(1);
    }
    if (verbose) std::cout << "Sema phase succeeded.\n";
    if (analyzeControlFlow(module).failed()) {
        Diagnostic::flush();
        if (verbose) std::cout << "Control flow analysis failed. Exiting...\n";
        exit(1);
    }
    Diagnostic::flush();
    if (verbose) std::cout << "Control flow analysis succeeded\n";
}

std::unique_ptr<llvm::Module> codegen(Module& module, bool verbose = false) {
    auto llvmModule = generateCode(module);
    if (!llvmModule) {
        if (verbose) std::cout << "Codegen failed. Exiting...\n";
        exit(1);
    }
    if (verbose) std::cout << "Code generation succeeded.\n";

    return llvmModule;
}

int main(int argc, char **argv)
{
    Options options = parseOptionsOrExit(std::span(argv, argc));

    if (options.flags.json) {
        enableJSONDiagnostics();
    } else {
        enableStdoutDiagnostics();
    }

    SymbolTable symbols;
    ThreadContext threadContext{&symbols};

    initialize(symbols);

    globalContext.files.reserve(options.files.size());

    bool hadError = false;

    ModuleBuilder builder;

    for (auto *filePath : options.files) {
        u32 fileHandle = globalContext.addFile(filePath);
        File& file = globalContext.files[fileHandle];

        ParsedFile parsed = parseFile(fileHandle, file, Diagnostic::writer());

        file.lineBreaks = std::move(parsed.lineBreaks);
        parsed.diagnostics.flush(Diagnostic::writer());

        if (parsed.result != ParseResult::OK) {
            hadError = true;
            continue;
        }

        file.size = parsed.size;
        file.astHandle = std::move(parsed.astHandle);
        builder.addDeclarations(parsed.declarations, fileHandle);
    }

    auto module = builder.finalize();

    if (!module) {
        if (options.flags.verbose) {
            std::cout << "Exiting...\n";
        }
        exit(1);
    }

    std::visit(overloaded {
        [&](Validate& _) {
            validate(*module, options.flags.verbose);
        },
        [&](Codegen& arg) {
            validate(*module, options.flags.verbose);
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
        }
    }, options.mode);
}
