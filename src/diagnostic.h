#ifndef LANG_diagnostic_h
#define LANG_diagnostic_h

#include "AST.h"
#include "parser.h"


class DiagnosticWriter {

public:
    virtual void error(ParserException& parserException) = 0;

    virtual void error(const AST::Node& node, std::string& message) = 0;

    virtual void warning(const AST::Node& node, std::string& message) = 0;

    virtual void note(const AST::Node& node, std::string& message) = 0;

    virtual ~DiagnosticWriter() = default;
};


class IODiagnosticWriter : public DiagnosticWriter {

    std::ostream& out;

public:
    IODiagnosticWriter(std::ostream& out) : out{out} {}

    virtual void error(ParserException& parserException) override {
        out << "Parser error: " << AST::Location{parserException.token} << ' ' << parserException.description() << '\n';
    }

    virtual void error(const AST::Node& node, std::string& message) override {
        out << "Error: " << node.getLocation() << ' ' << message << "\n";
    }

    virtual void warning(const AST::Node& node, std::string& message) override {
        out << "Warning: " << node.getLocation() << ' ' << message << "\n";
    }

    virtual void note(const AST::Node& node, std::string& message) override {
        out << "Note: " << node.getLocation() << ' ' << message << "\n";
    }
};

class JSONDiagnosticWriter : public DiagnosticWriter {
    std::ostream& out;
public:
    JSONDiagnosticWriter(std::ostream& out) : out{out} {}

    void printLocation(const AST::Location& location) {
        out << R"({"line": )" << location.line << R"(, "column": )" << location.column << R"(, "length": )" << location.length << R"(})";
    }

    virtual void error(ParserException& parserException) override {
        out << R"({"kind": "error", "message": ")" << parserException.description() << R"(", "location": )";
        auto location = AST::Location(parserException.token);
        printLocation(location);
        out << R"(})" << '\n';
    }

    virtual void error(const AST::Node& node, std::string& message) override {
        out << R"({"kind": "error", "message": ")" << message << R"(", "location": )";
        printLocation(node.getLocation());
        out << R"(})" << '\n';
    }

    virtual void warning(const AST::Node& node, std::string& message) override {
        out << R"({"kind": "warning", "message": ")" << message << R"(", "location": )";
        printLocation(node.getLocation());
        out << R"(})" << '\n';
    }

    virtual void note(const AST::Node& node, std::string& message) override {
        out << R"({"kind": "note", "message": ")" << message << R"(", "location": )";
        printLocation(node.getLocation());
        out << R"(})" << '\n';
    }

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

    static void error(const AST::Node& node, std::string&& message) {
        writer().error(node, message);
    }

    static void warning(const AST::Node& node, std::string&& message) {
        writer().warning(node, message);
    }

    static void note(const AST::Node& node, std::string&& message) {
        writer().note(node, message);
    }
};

#endif // LANG_diagnostic_h
