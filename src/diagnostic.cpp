#include "diagnostic.h"


class NoopWriter : public DiagnosticWriter {
    virtual void error(ParserException& parserException) override {}

    virtual void error(AST::Node& node, std::string& message) override {}

    virtual void warning(AST::Node& node, std::string& message) override {}

    virtual void note(AST::Node& node, std::string& message) override {}
};

NoopWriter noop;

DiagnosticWriter *Diagnostic::current = &noop;
