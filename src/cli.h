#ifndef LANG_cli_h
#define LANG_cli_h

#include <span>
#include <filesystem>

struct Validate {
    bool json = false;
};

struct Codegen {
    bool printCode = false;
    bool printIR = false;
};

/// General-purpose flags
struct Flags {
    bool verbose = false;
};

using Mode = std::variant<Validate, Codegen>;

struct Options {
    Mode mode;
    Flags flags;
    std::vector<const char *> files;
};

Options parseOptionsOrExit(const std::span<char *const> args);

#endif // LANG_cli_h
