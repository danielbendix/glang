
add_executable(glang
    main.cpp
    common.cpp
    AST.cpp
    AST_Visitor.cpp
    token.cpp
    scanner.cpp
    parser.cpp
    namespace.cpp
    control.cpp
    codegen.cpp
    type.cpp
    typeconstraint.cpp
    typecheck.cpp
    diagnostic.cpp
    builtins.cpp
    cli.cpp
    context.cpp
    layout.cpp
)

add_subdirectory(namespace)
add_subdirectory(resolution)
add_subdirectory(type)
add_subdirectory(typecheck)
add_subdirectory(sema)
add_subdirectory(containers)
add_subdirectory(target)

include_directories(.)

#find_program(CLANG_TIDY_EXE NAMES "clang-tidy" REQUIRED)
#set(CLANG_TIDY_COMMAND "${CLANG_TIDY_EXE}")
#set_target_properties(glang PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_COMMAND}")


