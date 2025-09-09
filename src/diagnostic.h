#ifndef LANG_diagnostic_h
#define LANG_diagnostic_h

#include "AST.h"
#include "location.h"

#include "llvm/Support/Allocator.h"

void enableJSONDiagnostics();
void enableStdoutDiagnostics();


struct BufferedDiagnostic {
    enum class Kind : u8 {
        Error,
        Warning,
        Note,
    };
    const char *description;
    /// The file the diagnostic emanated from.
    const u32 sourceFile;
    const u32 sourceOffset;
    /// The file the diagnostic is in. Should only differ from sourceFile for notes.
    const u32 file;
    const Kind kind;
    /// number of secondary locations after the primary one.
    const u8 extraLocations;
    const u32 locationsIndex;
    const u32 descriptionLength;

    BufferedDiagnostic(Kind kind, const char *description, u32 descriptionLength, u32 file, u32 sourceFile, u32 sourceOffset, u32 locations, u8 extraLocations)
        : kind{kind}
        , description{description}
        , descriptionLength{descriptionLength}
        , file{file}
        , sourceFile{sourceFile}
        , sourceOffset{sourceOffset}
        , locationsIndex{locations}
        , extraLocations{extraLocations}
        {}
};

class DiagnosticWriter {
public:
    virtual void start() = 0;
    virtual void end() = 0;

    virtual void writeDiagnostic(BufferedDiagnostic& diagnostic, DiagnosticLocation *locations) = 0;

    virtual ~DiagnosticWriter() = default;
};

class DiagnosticBuffer {
    llvm::BumpPtrAllocator stringAllocator;
    std::vector<BufferedDiagnostic> diagnostics;
    std::vector<DiagnosticLocation> locations;

    u32 pushLocation(DiagnosticLocation location) {
        u32 locationsIndex = this->locations.size();

        locations.push_back(location);

        return locationsIndex;
    }

    void diagnostic(
        BufferedDiagnostic::Kind kind,
        std::string_view message, 
        u32 sourceFile, 
        u32 sourceOffset,
        u32 file,
        DiagnosticLocation location
    ) {
        u32 locationsIndex = pushLocation(location);

        char *messageBuffer = (char *) stringAllocator.Allocate(message.size() + 1, alignof(char));
        memcpy(messageBuffer, message.data(), message.size() * sizeof(char));
        messageBuffer[message.size()] = '\0';

        diagnostics.push_back(BufferedDiagnostic {
            kind,
            messageBuffer,
            u32(message.size()),
            file,
            sourceFile,
            sourceOffset, 
            locationsIndex,
            0
        });
    }

public:
    DiagnosticBuffer() = default;
    DiagnosticBuffer(DiagnosticBuffer&&) = default;

    void error(std::string_view message, u32 sourceFile, u32 sourceOffset, DiagnosticLocation location) {
        diagnostic(BufferedDiagnostic::Kind::Error, message, sourceFile, sourceOffset, sourceFile, location);
    }

    void warning(std::string_view message, u32 sourceFile, u32 sourceOffset, DiagnosticLocation location) {
        diagnostic(BufferedDiagnostic::Kind::Warning, message, sourceFile, sourceOffset, sourceFile, location);
    }

    void note(std::string_view message, u32 sourceFile, u32 sourceOffset, u32 file, DiagnosticLocation location) {
        diagnostic(BufferedDiagnostic::Kind::Note, message, sourceFile, sourceOffset, file, location);
    }

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

    static DiagnosticBuffer buffer;

public:
    static DiagnosticWriter& writer() {
        return *current;
    }

    static void setWriter(DiagnosticWriter& writer) {
        current = &writer;
    }

    static void error(AST::FileLocation location, std::string&& message) {
        u32 file = ThreadContext::get()->currentFile;
        buffer.error(std::move(message), file, location.offset, location);
    }

    static void error(const AST::Node& node, std::string&& message) {
        u32 file = ThreadContext::get()->currentFile;
        error(node, std::move(message), file);
    }

    static void error(const AST::Node& node, std::string&& message, u32 file) {
        DiagnosticLocation location = node.getFileLocation();
        buffer.error(std::move(message), file, location.offset, location);
    }

    static void warning(const AST::Node& node, std::string&& message) {
        u32 file = ThreadContext::get()->currentFile;
        warning(node, std::move(message), file);
    }

    static void warning(const AST::Node& node, std::string&& message, u32 file) {
        DiagnosticLocation location = node.getFileLocation();
        buffer.warning(message, file, location.offset, location);
    }

    static void note(const AST::Node& node, std::string&& message, u32 file, u32 sourceOffset) {
        u32 sourceFile = ThreadContext::get()->currentFile;
        note(node, std::move(message), file, sourceFile, sourceOffset);
    }

    static void note(const AST::Node& node, std::string&& message, u32 file, u32 sourceFile, u32 sourceOffset) {
        DiagnosticLocation location = node.getFileLocation();
        buffer.note(message, sourceFile, sourceOffset, file, location);
    }

    static void flush() {
        buffer.flush(*current);
        buffer.clear();
    }
};

#endif // LANG_diagnostic_h
