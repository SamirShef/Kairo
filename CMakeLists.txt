cmake_minimum_required(VERSION 3.10)
project(StageLang)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_VERSION}")
message(STATUS "LLVM includes: ${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM libraries: ${LLVM_LIBRARY_DIRS}")

include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})

add_definitions(${LLVM_DEFINITIONS})

file(GLOB_RECURSE SOURCES "src/*.cpp")

add_executable(stc ${SOURCES})

target_link_libraries(stc
    libLLVM-${LLVM_VERSION_MAJOR}.so
    rt dl m z zstd
)