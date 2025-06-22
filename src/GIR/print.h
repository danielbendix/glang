#include "GIR.h"

#include <iostream>

#include "llvm/ADT/STLExtras.h"
using llvm::enumerate;

namespace GIR {

    void printUnary(std::ostream& out, std::string_view mnemonic, u32 index, InstIndex operand) {
        out << "%" << index << " = " << mnemonic << " %" << operand.offset;
    }

    void printBinary(std::ostream& out, std::string_view mnemonic, u32 index, InstIndex lhs, InstIndex rhs) {
        out << "%" << index << " = " << mnemonic << " %" << lhs.offset << ", " << rhs.offset;
    }

    void print(std::ostream& out, Function& function) {
        for (const auto [index, block] : enumerate(function.blocks)) {
            out << "BB" << index << ":\n";
            for (u32 i = block.entry; i <= block.terminator; ++i) {
                Opcode opcode = function.ops.getOp(i);
                Data data = function.ops.getData(i);

                out << "    ";
                switch (opcode) {
                case Opcode::ARGUMENT:

                case Opcode::INTCAST:
                    printUnary(out, "INTCAST", i, data.as.intCast.operand);
                    break;

                case Opcode::SEXT:

                case Opcode::ZEXT:

                case Opcode::TRUNCATE:
                    printUnary(out, "TRUNC", i, data.as.truncate.operand);
                    break;

                case Opcode::ADD:
                    printBinary(out, "ADD", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::SUBTRACT:
                    printBinary(out, "SUB", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::MULTIPLY:
                    printBinary(out, "MUL", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::DIVIDE:
                    printBinary(out, "DIV", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::MODULO:
                    printBinary(out, "MOD", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::SHIFT_LEFT:
                    printBinary(out, "SHL", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::SHIFT_RIGHT:
                    printBinary(out, "SHR", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::EQUAL:
                    printBinary(out, "CMP.EQ", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::NOT_EQUAL:
                    printBinary(out, "CMP.NEQ", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::GREATER:
                    printBinary(out, "CMP.GT", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::GREATER_EQUAL:
                    printBinary(out, "CMP.GE", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::LESS:
                    printBinary(out, "CMP.LT", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::LESS_EQUAL:
                    printBinary(out, "CMP.LE", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::BITWISE_AND:
                    printBinary(out, "AND", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::BITWISE_OR:
                    printBinary(out, "OR", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::BITWISE_XOR:
                    printBinary(out, "XOR", i, data.as.binary.left, data.as.binary.right);
                    break;
                case Opcode::BITWISE_NOT:
                    printBinary(out, "NOT", i, data.as.binary.left, data.as.binary.right);
                    break;

                case Opcode::EXTRACT_SUCCESS:
                    out << "EXT.SUC %" << data.as.extractResult.value;
                    break;
                case Opcode::EXTRACT_ERROR:
                    out << "EXT.ERR %" << data.as.extractResult.value;
                    break;

                case Opcode::SUBSCRIPT:
                    printBinary(out, "SUBSCRIPT", i, data.as.binary.left, data.as.binary.right);
                    break;

                // Are these two required?
                case Opcode::GET_LOCAL:
                    out << "GETLOCAL _" << data.as.getLocal.local.index;
                    break;
                case Opcode::SET_LOCAL:
                    out << "SETLOCAL _" << data.as.setLocal.local.index << ", %" << data.as.setLocal.value.offset;
                    break;
                case Opcode::LOAD:
                    out << "LOAD %" << data.as.load.target;
                    break;

                case Opcode::STORE:
                    out << "STORE %" << data.as.store.value << " at %" << data.as.store.target;
                    break;

                case Opcode::CALL:
                    out << "CALL ";
                    break;

                case Opcode::BRANCH:
                    out << "BR %" << data.as.branch.condition << "BB" << data.as.branch.onTrue.offset << ", BB" << data.as.branch.onFalse.offset;
                    break;
                case Opcode::JUMP:
                    out << "JMP BB" << data.as.jump.to;
                    break;
                case Opcode::SWITCH:
                    break;
                case Opcode::TRY:
                    out << "TRY %" << data.as.try_.tried << ", BB" << data.as.try_.onSuccess << ", BB" << data.as.try_.onFailure;
                    break;
                case Opcode::RETURN:
                    if (data.as.return_.value.offset == 0) {
                        out << "RET";
                    } else {
                        out << "RET %" << data.as.return_.value.offset;
                    }
                    break;

                case Opcode::DEBUG_STMT:
                   
                    break;
                }
                out << "\n";
            }
        }
    }
}

