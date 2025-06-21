cmake_minimum_required(VERSION 3.20.0)

project(glang)

option(ENABLE_UBSAN "Enable UB Sanitizer" OFF)
if(ENABLE_UBSAN)
    message("Enabling UB Sanitizer")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=nullability-arg,nullability-assign,nullability-return,enum,return,bounds")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=nullability-arg,nullability-assign,nullability-return,enum,return,bounds")
endif()


if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wunused-variable -Wno-nullability-completeness -Werror=return-type -Wmissing-field-initializers")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions -Wno-nullability-completeness -Werror=return-type -Wmissing-field-initializers")
    #set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-nullability-completeness -Werror=return-type -Wmissing-field-initializers")
endif()

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED On)
set(CMAKE_CXX_EXTENSIONS Off)

set(CMAKE_EXPORT_COMPILE_COMMANDS On)

find_package(LLVM 18.1.0...<19.0.0 REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

llvm_map_components_to_libnames(llvm_libs ${LLVM_TARGETS_TO_BUILD} support core irreader)

add_subdirectory(src)

target_link_libraries(glang PUBLIC ${llvm_libs})
