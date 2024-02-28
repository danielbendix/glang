#include "AST.h"
#include "AST_Visitor.h"

namespace AST {
    
    // TODO: Use a macro for visiting like llvm/InstVisitor.h#27

    class ExpressionVisitorImpl : ExpressionVisitorT<ExpressionVisitorImpl, int> {
        int visitIdentifier(Identifier *identifier) {
            return 1;
        }

        int visitLiteral(Literal *literal) {
            return 0;
        }
    };
}

namespace AST {

}
