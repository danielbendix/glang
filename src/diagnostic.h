#ifndef LANG_diagnostic_h
#define LANG_diagnostic_h

#include "AST.h"


class DiagnosticWriter {

public:
    virtual void error(AST::Node& node, std::string& message) = 0;

    virtual void warning(AST::Node& node, std::string& message) = 0;

    virtual void note(AST::Node& node, std::string& message) = 0;
};

class IODiagnosticWriter : public DiagnosticWriter {

    std::ostream& out;

public:
    IODiagnosticWriter(std::ostream& out) : out{out} {}

    virtual void error(AST::Node& node, std::string& message) override {
        out << "Error: " << node.getLocation() << ' ' << message << "\n";
    }

    virtual void warning(AST::Node& node, std::string& message) override {
        out << "Warning: " << node.getLocation() << ' ' << message << "\n";
    }

    virtual void note(AST::Node& node, std::string& message) override {
        out << "Note: " << node.getLocation() << ' ' << message << "\n";
    }
};

class NoopWriter : public DiagnosticWriter {
    virtual void error(AST::Node& node, std::string& message) override {}

    virtual void warning(AST::Node& node, std::string& message) override {}

    virtual void note(AST::Node& node, std::string& message) override {}
};

class Diagnostic {
    static DiagnosticWriter *current;
public:
    static DiagnosticWriter& writer() {
        return *current;
    }

    static void setWriter(DiagnosticWriter& writer) {
        current = &writer;
    }

    static void error(AST::Node& node, std::string&& message) {
        writer().error(node, message);
    }

    static void warning(AST::Node& node, std::string&& message) {
        writer().warning(node, message);
    }

    static void note(AST::Node& node, std::string&& message) {
        writer().note(node, message);
    }
};

#endif // LANG_diagnostic_h
