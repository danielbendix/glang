#include "AST.h"

#include <cstdint>
#include <memory>
#include <algorithm>

/**
 * GIR - G Intermedidate Representation
 *
 * GIR only contains conditional binary branches or self-contained branches within single instructions,
 * e.g. `and`, `or`, and `try`. Everything else is encoded with blocks and branch instructions.
 */

// TODO: Pointers are still used in this, so base size is kept at 16 bytes.
// TODO: This should be changed to 8 bytes, by only using references.
// TODO: This will require some changes in how other things are referenced and laid out,
// TODO: particularly functions and types.

namespace GIR {
    enum class Opcode : u8 {
        ARGUMENT,

        INTCAST,
        SEXT,
        ZEXT,

        TRUNCATE,

        ADD,
        SUBTRACT,
        MULTIPLY,
        DIVIDE,
        MODULO,

        SHIFT_LEFT,
        SHIFT_RIGHT,

        EQUAL,
        NOT_EQUAL,
        GREATER,
        GREATER_EQUAL,
        LESS,
        LESS_EQUAL,

        BITWISE_AND,
        BITWISE_OR,
        BITWISE_XOR,
        BITWISE_NOT,

        // Extract success value from error union.
        EXTRACT_SUCCESS,
        // Extract error value from error union.
        EXTRACT_ERROR,

        SUBSCRIPT,

        GET_LOCAL,
        SET_LOCAL,

        LOAD,
        STORE,

        CALL,

        // Terminators
        JUMP,
        BRANCH,
        SWITCH, // For low-level switches, e.g. individual values => cases, not ranges etc.
        RETURN,
        TRY,

        // Debugging
        DEBUG_STMT,
    };

    struct Op {
        u8 cached;
        Opcode op;
    };

    struct InstIndex {
        u32 offset;

        explicit InstIndex(u32 offset) : offset{offset} {}

        operator u32() const {
            return offset;
        }
    };

    /// A basic block. If entered, all instructions are executed.
    struct Block {
        InstIndex entry;
        InstIndex terminator;
    };

    struct BlockIndex {
        u32 offset;

        explicit BlockIndex(u32 offset) : offset{offset} {}

        operator u32() const {
            return offset;
        }
    };

    struct LocalIndex {
        u32 index;

        explicit LocalIndex(u32 index) : index{index} {}

        operator u32() const {
            return index;
        }
    };

    struct ExtraIndex {
        u32 index;

        explicit ExtraIndex(u32 index) : index{index} {}

        operator u32() const {
            return index;
        }
    };

    struct Data {

        struct BinaryOp {
            InstIndex left;
            InstIndex right;
        };

        struct IntCast {
            InstIndex operand;
            IntegerType *to;
        };

        struct Truncate {
            InstIndex operand;
            NumericType *to;
        };

        struct Subscript {
            InstIndex target;
            InstIndex index;
        };

        struct GetLocal {
            LocalIndex local;
        };

        struct SetLocal {
            LocalIndex local;
            InstIndex value;
        };

        struct ExtractResult {
            InstIndex value;
            // TODO: Make this a function reference.
            InstIndex call;
        };

        struct Jump {
            InstIndex to;
        };

        struct Branch {
            InstIndex condition;
            BlockIndex onTrue;
            BlockIndex onFalse;
        };


        // TODO: The next block could always be after this one, eliding the success block index.
        struct Try {
            InstIndex tried;
            BlockIndex onSuccess;
            BlockIndex onFailure;
        };

        struct Call {
            struct Argument {
                InstIndex argument;
            };
            // TODO: Point to function struct.
            using Function = void;
            Function *function;
        };

        struct Switch {
            u32 count;
            ExtraIndex extra;

            // Stored in extra
            struct Case {
                InstIndex value;
                BlockIndex target;
            };
        };

        struct Return {
            // is 0 on void return.
            InstIndex value;
        };

        struct Store {
            InstIndex target;
            InstIndex value;
        };

        struct Load {
            InstIndex target;
        };

        struct MemberLookup {
            InstIndex target;
            u32 index;
        };

        union alignas(void *) {
            Block block;

            BinaryOp binary;
            GetLocal getLocal;
            SetLocal setLocal;

            IntCast intCast;
            Truncate truncate;

            MemberLookup memberLookup;
            Load load;
            Store store;

            ExtractResult extractResult;
    
            Call call;

            Jump jump;
            Branch branch;
            Switch switch_;
            Try try_;
            Return return_;

            struct {
                void *a;
                void *b;
            } _;
        } as;

    };

    static_assert(sizeof(Data) <= 16);




    struct ConditionalBranch {
        InstIndex condition;
        InstIndex onTrue;
        InstIndex onFalse;
    };

    struct GetStructMember {
        InstIndex target;
        u16 member;
        u8 alignment;
    };

    static_assert(sizeof(ConditionalBranch) <= 3 * sizeof(Data));

    class ExtraArray {
        static constexpr size_t INITIAL_CAPACITY = 8;
        using Storage = u64;
        u32 _size = 0;
        u32 capacity = 0;
        Storage *extra = nullptr;
    public:
        ExtraArray() = default;

        void resize(u32 newCapacity) {
            size_t newSize = newCapacity * sizeof(Storage);
            extra = (Storage *) realloc(extra, newSize);
        }

        void ensureCapacity(u32 newSlots) {
            if (_size + newSlots > capacity) {
                resize(capacity == 0 ? INITIAL_CAPACITY : capacity << 1);
            }
        }

        template <typename Extra>
        [[nodiscard]]
        ExtraIndex push(Extra *extra, u32 count) {
            u32 slots = (count * sizeof(Extra) + (sizeof(Storage) - 1)) / sizeof(Storage);
            ensureCapacity(slots);

            memcpy(&this->extra[_size], extra, slots * sizeof(Storage));

            auto index = ExtraIndex{_size};
            _size += slots;
            return index;
        }

        ~ExtraArray() {
            free(extra);
        }
    };

    class OpArray {
        static constexpr size_t INITIAL_CAPACITY = 16;
        u32 _size = 0;
        u32 capacity = 0;
        Opcode *ops = nullptr;
        Data *datas = nullptr;
        ExtraArray extras;

    public:
        OpArray() = default;

        OpArray(OpArray&) = delete;
        OpArray& operator=(OpArray&) = delete;

        void resize(size_t newCapacity) {
            size_t opSize = newCapacity * sizeof(Opcode);

            constexpr size_t alignmentMask = sizeof(Data) - 1;
            size_t dataOffset = (opSize + alignmentMask) & (~alignmentMask);
            size_t total = dataOffset + newCapacity * sizeof(Data);

            void *allocated = malloc(total);

            Opcode *ops = (Opcode *) allocated;
            Data *datas = (Data *) ((std::byte *) allocated) + dataOffset;
            memcpy(ops, this->ops, _size * sizeof(Opcode));
            memcpy(datas, this->datas, _size * sizeof(Data));

            free(ops);
            this->ops = ops;
            this->datas = datas;
        }

        void ensureCapacity(u32 newSlots) {
            if (_size + newSlots > capacity) {
                resize(capacity == 0 ? INITIAL_CAPACITY : capacity << 1);
            }
        }

        template <typename Payload>
        requires std::is_trivial_v<Payload> && requires { alignof(Payload) <= alignof(void *); }
        [[nodiscard]]
        InstIndex push(Opcode opcode, Payload payload) {
            ops[_size] = opcode;
            Payload *data = reinterpret_cast<Payload *>(&datas[_size]);
            *data = payload;

            auto index = InstIndex(_size);
            _size += 1;
            return index;
        }

        template <typename Payload, typename Extra>
        requires std::is_trivial_v<Payload> && requires { alignof(Payload) == alignof(uint64_t); }
        [[nodiscard]]
        InstIndex pushVariable(Opcode opcode, Payload payload, Extra *extra, u32 count) {
            using Child = typename Payload::Child;
            size_t slots = (sizeof(Payload) + sizeof(Child) * count) / sizeof(Data);
            ensureCapacity(slots);

            ops[_size] = opcode;
            Payload *data = reinterpret_cast<Payload *>(&datas[_size]);
            *data = payload;

            payload.extra = extras.push(extra, count);

            auto index = InstIndex(_size);
            _size += 1;
            return index;
        }

        [[nodiscard]]
        Opcode getOp(u32 index) const {
            return ops[index];
        }

        [[nodiscard]]
        Data getData(u32 index) const {
            return datas[index];
        }

        [[nodiscard]]
        u32 size() const {
            return _size;
        }
        
//        template <>
//        InstIndex push(Opcode op, Data::SwitchBr payload) {
//            size_t slots = (sizeof(Data::SwitchBr) + payload.count * sizeof(Data::SwitchBr::Case)) / sizeof(Data);
//            if (size + slots > capacity) {
//                resize(capacity == 0 ? INITIAL_CAPACITY : capacity << 1);
//            }
//
//            ops[size] = op;
//            memset(&ops[size + 1], u8(Opcode::SPACE), slots - 1);
//            Payload *data = reinterpret_cast<Payload *>(&datas[size]);
//            *data = payload;
//
//            auto index = size;
//            size += slots;
//            return index;
//        }

//        InstIndex push(Opcode op, Data data) {
//            if (size + 1 >= capacity) {
//                resize(capacity == 0 ? INITIAL_CAPACITY : capacity << 1);
//            }
//
//            ops[size] = op;
//            datas[size] = data;
//            return InstIndex(size++);
//        }

        ~OpArray() {
            free(ops);
            free(extra);
        }
    };

    class GIR {
        OpArray ops;
    };

    struct Function {
        struct Parameter {
            Type *type;
        };

        struct Local {
            llvm::PointerIntPair<Type *, 1, bool> type;
            AST::IdentifierBinding *binding;
        };

        std::vector<Parameter> parameters;
        std::vector<Local> locals;
        std::vector<Block> blocks;

        OpArray ops;

        u32 addParameter(Type *type) {
            parameters.push_back({ .type = type });
            return parameters.size() - 1;
        }

        u32 addLocal(AST::IdentifierBinding *binding, Type *type, bool isMutable) {
            locals.push_back({ .type = {type, isMutable}, .binding = binding});
            return locals.size() - 1;
        }
    };
}

enum class CallingConvention {
    Default,
    C,
    CPP,
};


struct Function {
    struct Parameter {
        Type *type;
    };

    struct Local {
        llvm::PointerIntPair<Type *, 1> type;
        AST::IdentifierBinding *binding;
    };

    AST::FunctionDeclaration *declaration;
    CallingConvention callingConvention;
    std::vector<Parameter> parameters;
    std::vector<Local> locals;
    std::vector<GIR::Block> blocks;

    GIR::OpArray ops;

    u32 addParameter(Type *type) {
        parameters.push_back({ .type = type });
        return parameters.size() - 1;
    }

    u32 addLocal(AST::IdentifierBinding *binding, Type *type, bool isMutable) {
        locals.push_back({ .type = {type, isMutable}, .binding = binding});
        return locals.size() - 1;
    }
};
