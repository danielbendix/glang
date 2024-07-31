#include "cli.h"
#include <getopt.h>

#include <iostream>

static const 
struct option options[] = {
    {"validate-only", no_argument,  0, 0},
    {"json",          no_argument,  0, 0},
    {"print-code",    no_argument,  0, 0},
    {"print-ir",      no_argument,  0, 0},
    {"verbose",       no_argument,  0, 0},
    {"help",          no_argument,  0, 0},
    {NULL, 0, 0, 0},

};


Options parseOptionsOrExit(const std::span<char *const> args) {
    Flags flags;

    bool validateOnly = false;
    bool printCode = false;
    bool printIR = false;

    while (true) {
        int argIndex = optind ? optind : 1;
        int optionIndex;
        if (getopt_long((int) args.size(), args.data(), "", options, &optionIndex) == -1) {
            break;
        }

        switch (optionIndex) {
            case 0:
                validateOnly = true;
                break;
            case 1:
                flags.json = true;
                break;
            case 2:
                printCode = true;
                break;
            case 3:
                printIR = true;
                break;
            case 4:
                flags.verbose = true;
                break;
            case 5:
                // TODO: Print help;
                exit(0);
        }
    }

    std::vector<const char *> files;

    if (optind < args.size()) {
        int count = args.size() - optind;

        if (count > 1) {
            std::cerr << "Only one file is currently supported.";
            exit(1);
        }
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
        return {Codegen{printCode, printIR}, flags, std::move(files)};
    }
}
