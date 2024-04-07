
add_executable(glang
    main.cpp
    common.cpp
    AST.cpp
    AST_Visitor.cpp
    scanner.cpp
    parser.cpp
    namespace.cpp
    control.cpp
    codegen.cpp
    type.cpp
    typecheck.cpp
    diagnostic.cpp
)
