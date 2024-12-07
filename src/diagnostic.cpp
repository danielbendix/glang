#include "diagnostic.h"
#include "location.h"

#include <cstdio>


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

    struct OpenFile {
        FILE *file;
        u32 handle;
    };

    std::ostream& out;
    std::vector<OpenFile> files;

public:
    IODiagnosticWriter(std::ostream& out) : out{out} {}

    virtual void start() override {
        
    }

    virtual void end() override {
        for (auto& file : files) {
            fclose(file.file);
        }
        files.clear();
    }

    virtual void error(ParserException& parserException, u32 fileHandle) override {
        auto& file = globalContext.files[fileHandle];
        auto token = parserException.token;
        auto location = Location::fromToken(token, file.lineBreaks);

        out 
            << std::string_view{file.path, file.pathSize} 
            << ":" 
            << location.line
            << ":" 
            << location.column
            << ": error: " 
            << parserException.description() 
            << '\n';
    }

    OpenFile getOpenFile(u32 fileHandle) {
        for (auto openFile : files) {
            if (fileHandle == openFile.handle) {
                return openFile;
            }
        }   
        OpenFile result;
        auto& file = globalContext.files[fileHandle];
        result.file = fopen(file.path, "r");
        result.handle = fileHandle;

        // TODO: Check that file was opened.

        files.push_back(result);

        return result;
    }

    std::string_view createUnderlineString(u32 start, u32 end, u32 offset, u32 length, char *line) {
        assert(offset >= start);
        assert(offset + length <= end);

        u32 spaces = offset - start;

        // TODO: Make sure that bad length won't overflow the buffer.
        char *result = line;

        while (spaces) {
            *result++ = ' ';
            --spaces;
        }

        *result++ = '^';
        --length;

        while (length) {
            *result++ = '~';
            --length;
        }

        return {line, size_t(result - line)};
    }

    virtual void writeDiagnostic(BufferedDiagnostic& diagnostic, DiagnosticLocation *locations) override {
        assert(diagnostic.extraLocations == 0);

        auto& file = globalContext.files[diagnostic.file];
        auto location = Location::fromDiagnosticLocation(locations[0], file.lineBreaks);

        std::string_view filepath = {file.path, file.pathSize};

        const char *description = diagnosticKindString(diagnostic.kind);
        out << filepath << ':' << location.line << ':' << location.column << ": " << description << ": " << std::string_view{diagnostic.description, diagnostic.descriptionLength} << '\n';

        auto openFile = getOpenFile(diagnostic.file);
        u32 start = location.line == 1 ? 0 : file.lineBreaks[location.line - 2] + 1;
        u32 end = location.line <= file.lineBreaks.size() ? file.lineBreaks[location.line - 1] : file.size;

        u32 lineSize = end - start;
        fseek(openFile.file, start, SEEK_SET);

        assert(end > start);

        char *line = (char *) malloc(lineSize * sizeof(char));
        fread(line, sizeof(char), lineSize, openFile.file);

        out << std::string_view{line, lineSize} << '\n';

        u32 offset = locations[0].offset;
        u32 length = locations[0].length;

        auto underline = createUnderlineString(start, end, offset, length, line);
        out << underline << '\n';

        // TODO: Handle tabs and unicode characters.

        free(line);
    }
};

class JSONDiagnosticWriter : public DiagnosticWriter {
    std::ostream& out;
public:
    JSONDiagnosticWriter(std::ostream& out) : out{out} {}

    virtual void start() override {}
    virtual void end() override {}

    void printLocation(Location location) {
        out << R"({"line": )" << location.line << R"(, "column": )" << (location.column - 1) << R"(, "length": )" << location.length << R"(})";
    }

    virtual void error(ParserException& parserException, u32 fileHandle) override {
        out << R"({"kind": "error", "message": ")" << parserException.description() << R"(", "location": )";
        auto& file = globalContext.files[fileHandle];
        auto token = parserException.token;
        auto location = Location::fromToken(token, file.lineBreaks);
        printLocation(location);
        out << R"(})" << '\n';
    }

    void write(std::string_view kind, std::string_view message, std::string_view path, Location location) {
        out << R"({"kind": ")" << kind << R"(", "message": ")" << message << R"(", "file": ")" << path << R"(", "location": )";
        printLocation(location);
        out << R"(})" << '\n';
    }

    std::string_view getKindString(BufferedDiagnostic::Kind kind) {
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

    virtual void writeDiagnostic(BufferedDiagnostic& diagnostic, DiagnosticLocation *locations) override {
        assert(diagnostic.extraLocations == 0);

        const char *description = diagnosticKindString(diagnostic.kind);

        auto& file = globalContext.files[diagnostic.file];
        auto location = Location::fromDiagnosticLocation(locations[0], file.lineBreaks);

        std::string_view kind = getKindString(diagnostic.kind);
        std::string_view message = {diagnostic.description, diagnostic.descriptionLength};
        std::string_view path = {file.path, file.pathSize};

        write(kind, message, path, location);
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
    virtual void start() override {}
    virtual void end() override {}

    virtual void error(ParserException& parserException, u32 fileHandle) override {}

    virtual void writeDiagnostic(BufferedDiagnostic& diagnostic, DiagnosticLocation *locations) override {}
};

NoopWriter noop;

DiagnosticWriter *Diagnostic::current = &noop;
DiagnosticBuffer Diagnostic::buffer;
