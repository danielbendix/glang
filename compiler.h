#ifndef LANG_compiler_h
#define LANG_compiler_h

#include <string>

#include "scanner.h"

class Compiler {
    Scanner scanner;

public:
    Compiler(std::string&& string) : scanner{std::move(string)} {
        
    }


};







#endif
