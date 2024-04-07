#ifndef LANG_diagnostic_h
#define LANG_diagnostic_h

#include "AST.h"

class DiagnosticWriter {

public:
    virtual void error(AST::Node& node, std::string&& message) = 0;

    virtual void warning(AST::Node& node, std::string&& message) = 0;
};

class IODiagnosticWriter : public DiagnosticWriter {

    std::ostream& out;

public:
    IODiagnosticWriter(std::ostream& out) : out{out} {}

    virtual void error(AST::Node& node, std::string&& message) override {
        out << "ERROR: " << message << "\n";
    }

    virtual void warning(AST::Node& node, std::string&& message) override {
        out << "Warning: " << message << "\n";
    }
};

#endif // LANG_diagnostic_h
