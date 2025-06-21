# MIR
## Signedness of instructions

Perhaps there should be no distinction between signed and unsigned instructions, and the code should branch based on the type.
LLVM is sufficiently abstract that it can create operations without explicity specifying the size of the operands.
The same cannot be said for custom lowering to assembly. This will require exact knowledge of operator types at all points.
