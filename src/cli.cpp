#include "cli.h"
#include <getopt.h>

#include <iostream>

enum OptionValues {
    VALIDATE_ONLY = 0,
    JSON = 1,
    PRINT_IR = 2,
    VERBOSE = 3,
    OUTPUT = 'o',
    HELP = 'h',
};

static const 
struct option options[] = {
    {"validate-only", no_argument,        NULL, VALIDATE_ONLY},
    {"json",          no_argument,        NULL, JSON},
    {"print-ir",      no_argument,        NULL, PRINT_IR},
    {"verbose",       no_argument,        NULL, VERBOSE},
    {"output",        required_argument,  NULL, OUTPUT},
    {"help",          no_argument,        NULL, HELP},
    {NULL, 0, 0, 0},
};

[[noreturn]]
void printHelp() {
    auto& os = std::cout;

    os << "glang - G compiler";
    os << "\n\n";

    //os << "Usage: glang [OPTIONS] FILE1 [FILE2 ...]";
    os << "Usage: glang [OPTIONS] FILE";
    os << "\n\n";

    os << "Options:";
    os << "\n";

    os << "  --validate-only        Only validate code. Do not run codegen.\n";
    os << "  --json                 Output diagnostics in JSON format.\n";
    os << "  --print-ir             Write the produced LLVM IR to stdout.\n";
    os << "  --verbose              Enable verbose mode.\n";
    os << "  -o or --output         Specify a file to write the produced LLVM bitcode to.\n";
    os << "  -h or --help           Print help (this message) and exit.\n";

    exit(0);
}

Options parseOptionsOrExit(const std::span<char *const> args) {
    Flags flags;

    bool validateOnly = false;
    bool printCode = false;
    bool printIR = false;
    std::optional<std::string> outputFile;

    while (true) {
        int argIndex = optind ? optind : 1;
        int option;
        int optionIndex;
        if ((option = getopt_long((int) args.size(), args.data(), "o:h", options, &optionIndex)) == -1) {
            break;
        }

        switch (option) {
            case VALIDATE_ONLY:
                validateOnly = true;
                break;
            case JSON:
                flags.json = true;
                break;
            case PRINT_IR:
                printIR = true;
                break;
            case VERBOSE:
                flags.verbose = true;
                break;
            case OUTPUT:
                if (outputFile) {
                    std::cerr << "ERROR: Input file specified twice. Exiting...\n";
                    exit(1);
                }
                outputFile = optarg;
                break;
            case HELP:
                printHelp();
        }
    }

    std::vector<const char *> files;

    if (optind < args.size()) {
        int count = args.size() - optind;

        while (optind < args.size()) {
            files.push_back(args[optind++]);
        }
    } else {
        std::cerr << "No input file(s) specified. Exiting...\n";
        exit(1);
    }

    if (validateOnly) {
        return {Validate{}, flags, std::move(files)};
    } else {
        return {Codegen{printCode, printIR, std::move(outputFile)}, flags, std::move(files)};
    }
}
