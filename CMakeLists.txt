cmake_minimum_required(VERSION 3.10)
project(StageLang)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_VERSION}")
message(STATUS "LLVM includes: ${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM libraries: ${LLVM_LIBRARY_DIRS}")

file(GLOB_RECURSE SOURCES "src/*.cpp")

add_executable(stc ${SOURCES})

target_include_directories(stc PRIVATE ${LLVM_INCLUDE_DIRS})
target_compile_definitions(stc PRIVATE ${LLVM_DEFINITIONS})

if (TARGET LLVM)
    target_link_libraries(stc PRIVATE LLVM)
else()
    llvm_map_components_to_libnames(LLVM_LIBS all)
    target_link_libraries(stc PRIVATE ${LLVM_LIBS})
endif()

file(COPY libs DESTINATION ${CMAKE_BINARY_DIR})

include(GNUInstallDirs)
install(TARGETS stc RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
install(DIRECTORY libs DESTINATION ${CMAKE_INSTALL_DATADIR}/stagelang)