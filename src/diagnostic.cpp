#include "diagnostic.h"

const char *diagnosticKindString(BufferedDiagnostic::Kind kind) {
    using enum BufferedDiagnostic::Kind;
    switch (kind) {
        case Error:
            return "error";
        case Warning:
            return "warning";
        case Note:
            return "note";
    }
}

class IODiagnosticWriter : public DiagnosticWriter {

    std::ostream& out;

public:
    IODiagnosticWriter(std::ostream& out) : out{out} {}

    virtual void error(ParserException& parserException) override {
        out << "Parser error: " << AST::Location{parserException.token} << ' ' << parserException.description() << '\n';
    }

    virtual void writeDiagnostic(BufferedDiagnostic& diagnostic, DiagnosticLocation *locations) override {
        const char *description = diagnosticKindString(diagnostic.kind);
        out << "[" << locations->offset << ']' << description << ": " << std::string_view{diagnostic.description, diagnostic.descriptionLength};
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

    virtual void writeDiagnostic(BufferedDiagnostic& diagnostic, DiagnosticLocation *locations) override {
        const char *description = diagnosticKindString(diagnostic.kind);
        out << description << ": [" << locations->offset << ']' << std::string_view{diagnostic.description, diagnostic.descriptionLength};
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

JSONDiagnosticWriter jsonWriter{std::cout};

void enableJSONDiagnostics() {
    Diagnostic::setWriter(jsonWriter);
}

IODiagnosticWriter ioWriter{std::cout};

void enableStdoutDiagnostics() {
    Diagnostic::setWriter(ioWriter);
}

class NoopWriter : public DiagnosticWriter {
    virtual void error(ParserException& parserException) override {}

    virtual void writeDiagnostic(BufferedDiagnostic& diagnostic, DiagnosticLocation *locations) override {}

    virtual void error(const AST::Node& node, std::string& message) override {}

    virtual void warning(const AST::Node& node, std::string& message) override {}

    virtual void note(const AST::Node& node, std::string& message) override {}
};

NoopWriter noop;

DiagnosticWriter *Diagnostic::current = &noop;
