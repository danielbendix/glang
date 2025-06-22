#include "common.h"
#include "GIR.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"

using llvm::LLVMContext;
using llvm::enumerate;

struct GIRCodegen {

    LLVMContext& llvmContext;

    PassResult codegen(GIR::Function& function, llvm::Function& llvmFunction) {
        std::vector<llvm::BasicBlock *> blocks;
        blocks.reserve(function.blocks.size());

        for (const auto _ : function.blocks) {
            blocks.push_back(llvm::BasicBlock::Create(llvmContext, "", &llvmFunction));
        }

        std::vector<llvm::Instruction *> instructions{function.ops.size(), nullptr};

        auto getInstruction = [&instructions](GIR::InstIndex index) -> llvm::Value * {
            assert(index < instructions.size());
            assert(instructions[index]);
            return instructions[index];
        };

        auto setInstruction = [&instructions](u32 index, llvm::Instruction *value) {
            assert(index < instructions.size());
            assert(instructions[index] == nullptr);
            assert(value);
            instructions[index] = value;
        };

        llvm::IRBuilder<> builder{llvmContext};

        for (const auto [i, block] : enumerate(function.blocks)) {
            u32 entry = block.entry;
            u32 terminator = block.terminator;

            builder.SetInsertPoint(blocks[i]);

            for (u32 i = entry; i <= terminator; ++i) {
                GIR::Opcode opcode = function.ops.getOp(i);
                GIR::Data data = function.ops.getData(i);

                switch (opcode) {
                case GIR::Opcode::ARGUMENT:
                case GIR::Opcode::INTCAST:
                case GIR::Opcode::SEXT:
                case GIR::Opcode::ZEXT:
                case GIR::Opcode::TRUNCATE:
                case GIR::Opcode::ADD:
                case GIR::Opcode::SUBTRACT:
                case GIR::Opcode::MULTIPLY:
                case GIR::Opcode::DIVIDE:
                case GIR::Opcode::MODULO:
                case GIR::Opcode::SHIFT_LEFT:
                case GIR::Opcode::SHIFT_RIGHT:
                case GIR::Opcode::EQUAL: {
                    auto compare = data.as.binary;
                    auto *left = getInstruction(compare.left);
                    auto *right = getInstruction(compare.right);
                    auto *instruction = builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_EQ, left, right);
                }
                    

                case GIR::Opcode::NOT_EQUAL:
                case GIR::Opcode::GREATER:
                case GIR::Opcode::GREATER_EQUAL:
                case GIR::Opcode::LESS:
                case GIR::Opcode::LESS_EQUAL:
                case GIR::Opcode::BITWISE_AND:
                case GIR::Opcode::BITWISE_OR:
                case GIR::Opcode::BITWISE_XOR:
                case GIR::Opcode::BITWISE_NOT:
                case GIR::Opcode::EXTRACT_SUCCESS:
                case GIR::Opcode::EXTRACT_ERROR:
                case GIR::Opcode::SUBSCRIPT:
                case GIR::Opcode::GET_LOCAL:
                case GIR::Opcode::SET_LOCAL:
                case GIR::Opcode::LOAD:
                case GIR::Opcode::STORE:
                case GIR::Opcode::CALL:
                case GIR::Opcode::JUMP:
                case GIR::Opcode::BRANCH:
                case GIR::Opcode::SWITCH:
                case GIR::Opcode::RETURN:
                case GIR::Opcode::TRY:
                case GIR::Opcode::DEBUG_STMT:
                  break;
                }

                // TODO: generate every instruction.

                // TODO: If we're using PHIs, then we need to backpatch:
                // TODO: For every "future" instruction, we need to store backpatch info.
            }

            blocks.push_back(llvm::BasicBlock::Create(llvmContext, "", &llvmFunction));
        }
    }
}



