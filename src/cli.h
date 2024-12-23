#ifndef LANG_cli_h
#define LANG_cli_h

#include <vector>
#include <optional>
#include <variant>
#include <span>
#include <vector>
#include <optional>
#include <variant>
#include <filesystem>

struct Validate {
};

struct Codegen {
    bool printCode = false;
    bool printIR = false;
    std::optional<std::string> outputFile;
};

/// General-purpose flags
struct Flags {
    bool json = false;
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
