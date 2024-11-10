#include "diagnostic.h"


class NoopWriter : public DiagnosticWriter {
    virtual void error(ParserException& parserException) override {}

    virtual void error(const AST::Node& node, std::string& message) override {}

    virtual void warning(const AST::Node& node, std::string& message) override {}

    virtual void note(const AST::Node& node, std::string& message) override {}
};

NoopWriter noop;

DiagnosticWriter *Diagnostic::current = &noop;
