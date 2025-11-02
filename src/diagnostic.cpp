#include "diagnostic.h"
#include "location.h"

#include <cstdio>

#define RED_COLOR "\033[31m"
#define RESET_COLOR "\033[0m"
#define MAGENTA_COLOR "\033[95m"
#define CYAN_COLOR "\033[96m"

#define BOLD_TEXT "\033[1m"
#define RESET_WEIGHT "\033[22m"

const char *diagnosticColorString(BufferedDiagnostic::Kind kind) {
    using enum BufferedDiagnostic::Kind;
    switch (kind) {
        case Error:
            return RED_COLOR;
        case Warning:
            return MAGENTA_COLOR; 
        case Note:
            return CYAN_COLOR;
    }
}


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
        FileID id;
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

    OpenFile getOpenFile(FileID fileID) {
        for (auto openFile : files) {
            if (fileID == openFile.id) {
                return openFile;
            }
        }   
        auto& file = globalContext.files[fileID];
        FILE *openedFile = fopen(file.path, "r");

        OpenFile result = {openedFile, fileID};

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

    std::span<char> readUntilNewlineOrEOF(FILE *file) {
        char *line = nullptr;
        size_t allocated = 0;
        // TODO: Use something available on more platforms.
        u32 count = getline(&line, &allocated, file);
        return {line, count - 1};
    }

    std::span<char> readLine(FILE *file, u32 lineSize) {
        char *line = (char *) malloc(lineSize * sizeof(char));
        fread(line, sizeof(char), lineSize, file);
        return {line, lineSize};
    }

    virtual void writeDiagnostic(BufferedDiagnostic& diagnostic, DiagnosticLocation *locations) override {
        assert(diagnostic.extraLocations == 0);

        auto& file = globalContext.files[diagnostic.file];
        auto location = Location::fromDiagnosticLocation(locations[0], file.lineBreaks);

        std::string_view filepath = {file.path, file.pathSize};

        const char *color = diagnosticColorString(diagnostic.kind);
        const char *label = diagnosticKindString(diagnostic.kind);
        out << BOLD_TEXT 
            << filepath 
            << ':' 
            << location.line 
            << ':' 
            << location.column 
            << ": " 
            << color
            << label
            << ": " 
            << RESET_COLOR
            << std::string_view{diagnostic.description, diagnostic.descriptionLength} 
            << RESET_WEIGHT 
            << '\n';

        // 0 is used for diagnostics that cannot have file content printed, e.g. EOF.
        if (location.length == 0) {
            return;
        }

        auto openFile = getOpenFile(diagnostic.file);
        u32 start = location.line == 1 ? 0 : file.lineBreaks[location.line - 2] + 1;
        u32 end = location.line <= file.lineBreaks.size() ? file.lineBreaks[location.line - 1] : file.size;

        u32 lineSize = end - start;
        fseek(openFile.file, start, SEEK_SET);

        std::span<char> line;

        if (end == 0) {
            line = readUntilNewlineOrEOF(openFile.file);
            end = start + line.size();
        } else {
            line = readLine(openFile.file, end - start);
        }

        out << std::string_view{line.data(), line.size()} << '\n';

        u32 offset = locations[0].offset;
        u32 length = locations[0].length;

        auto underline = createUnderlineString(start, end, offset, length, line.data());
        out << underline << '\n';

        // TODO: Handle tabs and unicode characters.

        free(line.data());
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

    virtual void writeDiagnostic(BufferedDiagnostic& diagnostic, DiagnosticLocation *locations) override {}
};

NoopWriter noop;

DiagnosticWriter *Diagnostic::current = &noop;
DiagnosticBuffer Diagnostic::buffer;
