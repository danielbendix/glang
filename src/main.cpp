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

static bool verbose = false;

template <typename Step, typename... Parameters>
auto exitOnFailure(std::string&& name, Step step, Parameters&&... parameters) {
    auto result = step(parameters...);
    bool failed;
    if constexpr (std::is_same_v<decltype(result), PassResult>) {
        failed = result.failed();
    } else {
        failed = !result;
    }
    if (failed) {
        Diagnostic::flush();
        if (verbose) {
            std::cout << name << " failed. Exiting...\n";
        }
        exit(1);
    } else {
        Diagnostic::flush();
        if (verbose) {
            std::cout << name << " succeeded\n";
        }
        return result;
    }
}

void initialize(SymbolTable& symbols)
{
    auto cpu = detectCPU();
    Architecture::populate(cpu);
    setupBuiltins(symbols, Architecture::current());
}

void validate(Module& module, bool verbose = false) {
    exitOnFailure("Sema phase", [](Module& module) {
        return typecheckModule(module);
    }, module);
    exitOnFailure("Control flow analysis", [](Module& module) {
        return analyzeControlFlow(module);
    }, module);
}

std::unique_ptr<llvm::Module> codegen(Module& module, bool verbose = false) {
    return exitOnFailure("Code generation", [](Module& module) {
        return generateCode(module);
    }, module);
}

int main(int argc, char **argv)
{
    Options options = parseOptionsOrExit(std::span(argv, argc));

    verbose = options.flags.verbose;
    if (options.flags.json) {
        enableJSONDiagnostics();
    } else {
        enableStdoutDiagnostics();
    }

    SymbolTable symbols;
    ThreadContext threadContext{&symbols};

    initialize(symbols);

    globalContext.files.reserve(options.files.size());



    auto module = exitOnFailure("Building namespace", [](std::vector<const char *>& files) -> std::unique_ptr<Module> {
        ModuleBuilder builder;
        bool hadError = false;

        for (auto *filePath : files) {
            FileID fileID = globalContext.addFile(filePath);
            File& file = globalContext.files[fileID];

            ParsedFile parsed = parseFile(fileID, file, Diagnostic::writer());

            file.lineBreaks = std::move(parsed.lineBreaks);
            parsed.diagnostics.flush(Diagnostic::writer());

            if (parsed.result != ParseResult::OK) {
                hadError = true;
                continue;
            }

            file.size = parsed.size;
            file.astHandle = std::move(parsed.astHandle);
            builder.addDeclarations(parsed.declarations, fileID);
        }

        if (hadError) {
            return nullptr;
        }

        return builder.finalize();
    }, options.files);

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
