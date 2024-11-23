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

void initialize(SymbolTable& symbols)
{
    auto cpu = detectCPU();
    Architecture::populate(cpu);
    setupBuiltins(symbols, Architecture::current());
}

enum class FileError {
    FileNotFound,
};

std::variant<ParsedFile, FileError, ParserException> parseFile(const char *path) {
    std::ifstream file(path, std::ios::in | std::ios::binary);
    if (file.fail()) {
        return FileError::FileNotFound;
    }

    file.seekg(0, file.end);
    size_t file_size = file.tellg();
    file.seekg(0, file.beg);

    std::string contents(file_size, '\0');

    file.read(contents.data(), file_size);
    file.close();

    try {
        return parseString(std::move(contents));
    } catch (ParserException exception) {
        return exception;
    }
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

    for (auto *file : options.files) {
        u32 fileHandle = globalContext.addFile(file);

        auto result = parseFile(file);

        std::visit(overloaded {
            [&](ParsedFile& parsedFile) {
                File& file = globalContext.files[fileHandle];
                file.size = parsedFile.size;
                file.lineBreaks = std::move(parsedFile.lineBreaks);
                file.astHandle = std::move(parsedFile.astHandle);
                builder.addDeclarations(parsedFile.declarations, fileHandle);
            },
            [&](FileError error) {
                switch (error) {
                    case FileError::FileNotFound:
                        std::cout << "error: file '" << file << "' does not exist.\n";
                        hadError = true;
                }
            },
            [&](ParserException& exception) {
                Diagnostic::writer().error(exception, fileHandle);
                hadError = true;
            }
        }, result);
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
