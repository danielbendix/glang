
add_executable(arith
    main.cpp
    AST.cpp
    AST_Visitor.cpp
    scanner.cpp
    parser.cpp
    codegen.cpp
    type.cpp
    typecheck.cpp
    diagnostic.cpp
)
