#include "identifier.h"

void IdentifierResolution::deleteValue(IdentifierResolution *value) {
    switch (value->getKind()) {
        case IRK_Local: return delete static_cast<LocalResolution *>(value);
        case IRK_Global: return delete static_cast<GlobalResolution *>(value);
        case IRK_Function: return delete static_cast<FunctionResolution *>(value);
        case IRK_Parameter: return delete static_cast<FunctionParameterResolution *>(value);
    }
}
