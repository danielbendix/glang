#ifndef LANG_typecheck_util_h
#define LANG_typecheck_util_h

#include "type.h"

#include "llvm/Support/Casting.h"

using llvm::isa;


template <typename TypeType>
bool expectEqualAs(Type& type1, Type& type2, auto function) {
    if (!isa<TypeType>(type1) || !is<TypeType>(type2)) {
        function();
        return true;
    } else {
        return false;
    }
}

template <typename TypeType>
bool expectBothAs(Type& type1, Type& type2, auto function) {
    if (!isa<TypeType>(type1) || !is<TypeType>(type2)) {
        function();
        return true;
    } else {
        return false;
    }
}







#endif
