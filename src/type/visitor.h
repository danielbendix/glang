#ifndef LANG_type_visitor_h
#define LANG_type_visitor_h

#include "type.h"
#include "type/struct.h"
#include "type/enum.h"

namespace TypeVisitor {
    template <typename Func>
    auto visit(Type& type, Func&& visitor) {
        switch (type.getKind()) {
            case TK_Void:
                return std::invoke(visitor, *static_cast<VoidType *>(&type));
            case TK_Boolean:
                return std::invoke(visitor, *static_cast<BooleanType *>(&type));
            case TK_Num_Integer:
                return std::invoke(visitor, *static_cast<IntegerType *>(&type));
            case TK_Num_FP:
                return std::invoke(visitor, *static_cast<FPType *>(&type));
            case TK_String:
                return std::invoke(visitor, *static_cast<StringType *>(&type));
            case TK_Pointer:
                return std::invoke(visitor, *static_cast<PointerType *>(&type));
            case TK_Optional:
                return std::invoke(visitor, *static_cast<OptionalType *>(&type));
            case TK_Function:
                return std::invoke(visitor, *static_cast<FunctionType *>(&type));
            case TK_Struct:
                return std::invoke(visitor, *static_cast<StructType *>(&type));
            case TK_Enum:
                return std::invoke(visitor, *static_cast<EnumType *>(&type));
            case TK_Protocol:
                llvm_unreachable("");
            case TK_Array:
                return std::invoke(visitor, *static_cast<ArrayType *>(&type));
            case TK_Range:
                return std::invoke(visitor, *static_cast<RangeType *>(&type));
        }
    }

    template <typename Func>
    auto visit(const Type& type, Func&& visitor) {
        switch (type.getKind()) {
            case TK_Void:
                return std::invoke(visitor, *static_cast<const VoidType *>(&type));
            case TK_Boolean:
                return std::invoke(visitor, *static_cast<const BooleanType *>(&type));
            case TK_Num_Integer:
                return std::invoke(visitor, *static_cast<const IntegerType *>(&type));
            case TK_Num_FP:
                return std::invoke(visitor, *static_cast<const FPType *>(&type));
            case TK_String:
                return std::invoke(visitor, *static_cast<const StringType *>(&type));
            case TK_Pointer:
                return std::invoke(visitor, *static_cast<const PointerType *>(&type));
            case TK_Optional:
                return std::invoke(visitor, *static_cast<const OptionalType *>(&type));
            case TK_Function:
                return std::invoke(visitor, *static_cast<const FunctionType *>(&type));
            case TK_Struct:
                return std::invoke(visitor, *static_cast<const StructType *>(&type));
            case TK_Enum:
                return std::invoke(visitor, *static_cast<const EnumType *>(&type));
            case TK_Protocol:
                llvm_unreachable("");
            case TK_Array:
                return std::invoke(visitor, *static_cast<const ArrayType *>(&type));
            case TK_Range:
                return std::invoke(visitor, *static_cast<const RangeType *>(&type));
        }
    }
}

#endif // LANG_type_visitor_h
