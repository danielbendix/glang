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

namespace GIR {
    enum class Opcode : u8 {
        /// Used for instructions that take more than 1 slot.
        CONTINUED = 0, 

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

        SUBSCRIPT,

        LOGICAL_AND,
        LOGICAL_OR,
        LOGICAL_NOT,

        GET_LOCAL,
        SET_LOCAL,

        LOAD,
        STORE,

        CALL,

        // Terminators
        COND_BR,
        SWITCH_BR,
        RETURN,

        // Debugging
        DEBUG_STMT,
    };

    struct InstIndex {
        u32 offset;

        explicit InstIndex(u32 offset) : offset{offset} {}

        operator u32() {
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

        operator u32() {
            return offset;
        }
    };

    struct LocalIndex {
        u32 index;

        explicit LocalIndex(u32 index) : index{index} {}

        operator u32() {
            return index;
        }
    };

    struct Data {

        struct BinaryOp {
            InstIndex left;
            InstIndex right;
        };

        static_assert(sizeof(BinaryOp) <= 8);

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

        struct CondBr {
            InstIndex condition;
            BlockIndex onTrue;
            BlockIndex onFalse;
        };

        struct Jump {
            InstIndex to;
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
            Argument arguments[];

            using Child = Argument;
            static constexpr size_t ChildOffset() {
                return offsetof(Call, arguments);
            }
        };

        struct SwitchBr {
            struct Case {
                u64 __reserve;
            };
            u32 count;
            Case cases[0];

            using Child = Case;
        };

        struct Return {
            InstIndex value;
        };


        struct Store {
            InstIndex target;
            InstIndex value;
        };

        struct Load {
            InstIndex target;
            InstIndex target2;
        };

        struct MemberLookup {
            InstIndex target;
            u32 index;
        };

        union alignas(void *) {
            Block block;

            u64 _data;
            BinaryOp binary;
            GetLocal getLocal;
            SetLocal setLocal;

            MemberLookup memberLookup;
            Load load;
    
            Call call;

            CondBr condBr;
            Jump jump;
            SwitchBr _switch;
        } as;

        union __SingleSlot {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-declarations"
            BinaryOp _01;
            Block _00;

#pragma clang diagnostic pop
        };

        static_assert(sizeof(__SingleSlot) <= 8);
    };




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

    class OpArray {
        static constexpr size_t INITIAL_CAPACITY = 8;
        size_t size = 0;
        size_t capacity = 0;
        Opcode *ops = nullptr;
        Data *datas = nullptr;

    public:
        OpArray() {}

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
            memcpy(ops, this->ops, size * sizeof(Opcode));
            memcpy(datas, this->datas, size * sizeof(Data));

            free(ops);
            this->ops = ops;
            this->datas = datas;
        }

        void ensureCapacity(u32 newSlots) {
            if (size + newSlots > capacity) {
                resize(capacity == 0 ? INITIAL_CAPACITY : capacity << 1);
            }
        }


        template <typename Payload>
        requires std::is_trivial_v<Payload> && requires { alignof(Payload) == 8; }
        InstIndex push(Opcode op, Payload payload) {
            size_t slots = sizeof(Payload) / sizeof(Data);
            ensureCapacity(slots);

            ops[size] = op;
            memset(&ops[size + 1], u8(Opcode::CONTINUED), slots - 1);
            Payload *data = reinterpret_cast<Payload *>(&datas[size]);
            *data = payload;

            auto index = InstIndex(size);
            size += slots;
            return index;
        }

        template <typename Payload>
        requires std::is_trivial_v<Payload> && requires { alignof(Payload) == 8; }
        InstIndex pushVariable(Opcode op, Payload payload, Payload::Child *children, u32 count) {
            using Child = Payload::Child;
            size_t slots = (sizeof(Payload) + sizeof(Child) * count) / sizeof(Data);
            ensureCapacity(slots);

            ops[size] = op;
            memset(&ops[size + 1], u8(Opcode::CONTINUED), slots - 1);
            Payload *data = reinterpret_cast<Payload *>(&datas[size]);
            *data = payload;

            Child *childData = (Child *) (((std::byte *) &datas[size]) + Payload::ChildOffset());
            memcpy(childData, children, sizeof(Child) * count);

            auto index = InstIndex(size);
            size += slots;
            return index;
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
        }
    };

    class GIR {
        OpArray ops;
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
