#ifndef LANG_location_h
#define LANG_location_h

#include "common.h"
#include "token.h"
#include "AST.h"

struct DiagnosticLocation {
    const u32 offset;
    const u32 length;

    DiagnosticLocation(u32 offset, u32 length)
        : offset{offset}, length{length} {}

    DiagnosticLocation(AST::FileLocation location)
        : offset{location.offset}, length{location.length} {}
};

struct Location {
    u32 line;
    u32 column;
    u32 length;

    Location(u32 line, u32 column, u32 length)
        : line{line}, column{column}, length{length} {}

    static u32 getLine(u32 offset, std::span<const u32> lineBreaks) {
        u32 line = 1;

        for (auto lineBreak : lineBreaks) {
            if (lineBreak > offset) {
                break;
            } else {
                ++line;
            }
        }
        return line;
    }

    static Location fromToken(Token token, std::span<const u32> lineBreaks) {
        auto line = getLine(token.offset, lineBreaks);
        u32 column;
        if (line == 1) {
            column = token.offset;
        } else {
            column = token.offset - lineBreaks[line - 2];
        }
        return Location(line, column, token.length);
    }

    static Location fromFileLocation(AST::FileLocation fileLocation, std::span<const u32> lineBreaks) {
        auto line = getLine(fileLocation.offset, lineBreaks);
        u32 column;
        if (line == 1) {
            column = fileLocation.offset;
        } else {
            column = fileLocation.offset - lineBreaks[line - 2];
        }
        return Location(line, column, fileLocation.length);
    }

    static Location fromDiagnosticLocation(DiagnosticLocation diagnosticLocation, std::span<const u32> lineBreaks) {
        auto line = getLine(diagnosticLocation.offset, lineBreaks);
        u32 column;
        if (line == 1) {
            column = diagnosticLocation.offset;
        } else {
            column = diagnosticLocation.offset - lineBreaks[line - 2];
        }
        return Location(line, column, diagnosticLocation.length);
    }
    
    static Location fromNode(const AST::Node& node, std::span<const u32> lineBreaks) {
        auto fileLocation = node.getFileLocation();
        return Location::fromFileLocation(fileLocation, lineBreaks);
    }

    friend std::ostream& operator<<(std::ostream& os, const Location& location) {
        if (location.length == 1) {
            return os << '[' << location.line << ':' << location.column <<']';
        } else {
            return os << '[' << location.line << ':' << location.column << '-' << location.column + location.length - 1 <<']';
        }
    }
};

#endif // LANG_location_h
