#include "common.h"
#include "layout.h"

#include "containers/optional.h"
#include "containers/symbol_table.h"
#include "containers/symbol_map.h"

#include <cassert>
#include <concepts>


enum class TypeTag : u8 {
    Special,        //< Types that are not parametric in any way, and require zero runtime storage.
    Integer,        //< Integral types
    Struct,
    Pointer,        //< Pointer with default alignment
    AlignedPointer, //< Pointer with non-default alignment.
    Optional,
    Function,
    Array,
    Range,
};

class TypeID {
    using enum TypeTag;

    static constexpr inline u8 TAG_BITS = 4;
    static constexpr inline u8 TAG_MAX = (1 << TAG_BITS) - 1;
    static constexpr inline u8 INDEX_BITS = 28;
    static constexpr inline u32 INDEX_MAX = (1 << INDEX_BITS) - 1;

    static constexpr u32 makeValue(TypeTag tag, u32 index) {
        u8 tagValue = static_cast<u8>(tag);
        assert(tagValue <= TAG_MAX);
        assert(index <= INDEX_MAX);

        return tagValue << INDEX_BITS & index;
    }
public:
    const u32 value;

    constexpr TypeID(TypeTag tag, u32 index) : value{makeValue(tag, index)} {}

    constexpr TypeID(TypeID const& other) : value{other.value} {}

    static TypeID toFunction(u32 index) {
        return {Function, index};
    }

    static TypeID toPointer(u32 index) {
        return {Pointer, index};
    }

    constexpr TypeTag tag() const {
        return TypeTag(value >> INDEX_BITS);
    }

    constexpr u32 index() const {
        return value & INDEX_MAX;
    }

    friend bool operator==(TypeID lhs, TypeID rhs) {
        return lhs.value == rhs.value;
    }

    friend bool operator!=(TypeID lhs, TypeID rhs) {
        return lhs.value != rhs.value;
    }
};

template <typename T>
concept TypeConcept = requires(T type)
{
    { T::typeTag } -> std::convertible_to<TypeTag>;
    { type.id() } -> std::convertible_to<TypeID>;
};

template <>
struct Optional<TypeID> {
    TypeID id;

    Optional() : id{TypeTag(0), 0} {}
    Optional(TypeID id) : id{id} {}

    operator bool() const {
        return id.value != 0;
    }
};

struct OptionalType {
    static constexpr TypeTag typeTag = TypeTag::Optional;

    TypeID contained;
};

struct StructType {
    static constexpr TypeTag typeTag = TypeTag::Struct;

    struct Member {
        enum class Type : u8 {
            Field,
            Method,
        };

        struct Field {
            const Type type = Type::Field;
            u32 offset;
        };

        struct Method {
            const Type type = Type::Method;
        };

        union {
            const Type type;
            Method method;
            Field field;
        } as;
    };
    Symbol& name;
    Layout layout;
    union {
        u64 single;
        u64 *multiple;
    } initializedFields;
    u32 fieldCount;


    SymbolMap<Member> members;
    SymbolMap<Member> staticMembers;
};

// A block containing the most commonly looked up parameterized types.
struct TypeIndex {
    Optional<TypeID> optionalType = {};
    Optional<TypeID> pointerType = {};
    Optional<TypeID> arrayType = {};
    Optional<TypeID> boundedArrayType = {};
};

template <u8 Offset, u8 Size, typename Result, typename From>
constexpr Result extractBits(From value)
{
    using FromLimits = std::numeric_limits<From>;
    using ResultLimits = std::numeric_limits<Result>;

    static_assert(Offset + Size <= FromLimits::digits);
    static_assert(ResultLimits::digits >= Size);

    Result result = (value >> Offset) & ((1U << Size) - 1);
    return result;
}

// TODO: Define coercion hierarchy between types with identical semantics.
struct IntegerType {
    static constexpr TypeTag typeTag = TypeTag::Integer;

    enum class Tag {
        Regular, //< {i, u}{8, 16, 32, 64}
        Pointer, //< intptr, uintptr
        Size,    //< isize, usize
    };
    const u16 bitWidth; //< Bit width. Stored as 16 bits.
    const u16 index; //< Index into metadata table. Stored as 11 bits.
    const bool isSigned; //< Signedness. Stored as 1 bit.

    IntegerType(TypeID id) 
        : bitWidth{extractBits<0, 16, u16>(id.value)}
        , index{extractBits<16, 11, u16>(id.value)}
        , isSigned{extractBits<27, 1, bool>(id.value)} 
    {
        assert(id.tag() == TypeTag::Integer);
    }

    static constexpr TypeID makeTypeID(u16 bitWidth, u16 index, bool isSigned) {
        return TypeID(TypeTag::Integer, (isSigned ? 1U : 0U) << 27 | index << 16 | bitWidth);
    }
};

struct IntegerTypeMetadata {
    Symbol& name;
};

struct AlignedPointer {
    TypeID pointee;
    uint8_t alignment;
};

namespace SpecialTypes {
    constexpr u8 FP_BIT = 1 << 4;
    
    enum Tag : u8 {
        Void = 0, 
        Bool = 1,
        Byte = 2,
        
        FP_Half     = FP_BIT + 0,  //< Half-prevision floating-point.
        FP_Single   = FP_BIT + 1,  //< Single-precision floating-point.
        FP_Double   = FP_BIT + 2,  //< Double-precision floating-point.
        FP_Quad     = FP_BIT + 3,  //< Quadruple-precision floating-point.
        FP_BFloat16 = FP_BIT + 4,  //< Brain floating point.
    };
    
    constexpr TypeID makeTypeID(Tag tag) {
        return TypeID{TypeTag::Special, u32(tag)};
    }

    /// Void type. Used when functions return nothing, and as a type erased pointer.
    constexpr TypeID voidType = makeTypeID(Tag::Void);
    /// Boolean type. A non-numeric type that represents true or false.
    constexpr TypeID boolType = makeTypeID(Tag::Bool);
    /// A special type that represents a byte. This type does not allow any arithmetic,
    /// but allows loading and storing, and has a well-defined size.
    constexpr TypeID byteType = makeTypeID(Tag::Byte);

    constexpr bool isFloatingPoint(TypeID typeID) {
        return (typeID.index() & FP_BIT) != 0;
    }
}

namespace FPTypes {
    constexpr TypeID halfType       = SpecialTypes::makeTypeID(SpecialTypes::FP_Half);
    constexpr TypeID singleType     = SpecialTypes::makeTypeID(SpecialTypes::FP_Single);
    constexpr TypeID doubleType     = SpecialTypes::makeTypeID(SpecialTypes::FP_Double);
    constexpr TypeID quadType       = SpecialTypes::makeTypeID(SpecialTypes::FP_Quad);
    constexpr TypeID bfloat16Type   = SpecialTypes::makeTypeID(SpecialTypes::FP_BFloat16);
};

namespace IntegerTypes {
    constexpr TypeID u8Type = IntegerType::makeTypeID(8, 1, false);
    constexpr TypeID u16Type = IntegerType::makeTypeID(16, 2, false);
    constexpr TypeID u32Type = IntegerType::makeTypeID(32, 3, false);
    constexpr TypeID u64Type = IntegerType::makeTypeID(64, 4, false);
    constexpr TypeID u128Type = IntegerType::makeTypeID(128, 5, false);

    constexpr TypeID i8Type = IntegerType::makeTypeID(8, 6, true);
    constexpr TypeID i16Type = IntegerType::makeTypeID(16, 7, true);
    constexpr TypeID i32Type = IntegerType::makeTypeID(32, 8, true);
    constexpr TypeID i64Type = IntegerType::makeTypeID(64, 9, true);
    constexpr TypeID i128Type = IntegerType::makeTypeID(128, 10, true);
};
