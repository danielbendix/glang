#ifndef LANG_diagnostic_h
#define LANG_diagnostic_h

#include "AST.h"
#include "parser.h"

#include "llvm/Support/Allocator.h"

void enableJSONDiagnostics();
void enableStdoutDiagnostics();


struct DiagnosticLocation {
    u32 offset;
    u32 length;
};

struct BufferedDiagnostic {
    enum class Kind {
        Error,
        Warning,
        Note,
    };
    char *description;
    /// The file the diagnostic emanated from.
    u32 sourceFile;
    u32 sourceOffset;
    /// The file the diagnostic is in. Should only differ from sourceFile for notes.
    u32 file;
    Kind kind;
    /// number of secondary locations after the primary one.
    u8 extraLocations;
    u32 locationsIndex;
    u32 descriptionLength;
};

class DiagnosticWriter {
public:
    virtual void error(ParserException& parserException) = 0;

    virtual void error(const AST::Node& node, std::string& message) = 0;

    virtual void warning(const AST::Node& node, std::string& message) = 0;

    virtual void note(const AST::Node& node, std::string& message) = 0;

    virtual void writeDiagnostic(BufferedDiagnostic& diagnostic, DiagnosticLocation *locations) = 0;

    virtual ~DiagnosticWriter() = default;
};

class DiagnosticBuffer {
    llvm::BumpPtrAllocator stringAllocator;
    std::vector<BufferedDiagnostic> diagnostics;
    std::vector<DiagnosticLocation> locations;

    void flush(DiagnosticWriter& writer) {
        // Potentially get a lock on the output.
        for (auto& diagnostic : diagnostics) {
            writer.writeDiagnostic(diagnostic, &locations[diagnostic.locationsIndex]);
        }

        clear();
    }

    void clear() {
        stringAllocator.Reset();
        diagnostics.clear();
        locations.clear();
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

    static void duplicateDeclaration(AST::Declaration& duplicate, AST::Declaration& initial) {

    }
};

#endif // LANG_diagnostic_h
