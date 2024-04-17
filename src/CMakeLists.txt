
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
    typecheck.cpp
    diagnostic.cpp
)

add_subdirectory(namespace)
add_subdirectory(resolution)
add_subdirectory(type)

include_directories(.)

#find_program(CLANG_TIDY_EXE NAMES "clang-tidy" REQUIRED)
#set(CLANG_TIDY_COMMAND "${CLANG_TIDY_EXE}")
#set_target_properties(glang PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_COMMAND}")


