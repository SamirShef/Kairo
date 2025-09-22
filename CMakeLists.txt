cmake_minimum_required(VERSION 3.10)
project(Kairo)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_VERSION}")
message(STATUS "LLVM includes: ${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM libraries: ${LLVM_LIBRARY_DIRS}")

file(GLOB_RECURSE SOURCES "src/*.cpp")

add_executable(krc ${SOURCES})

target_include_directories(krc PRIVATE ${LLVM_INCLUDE_DIRS})
target_compile_definitions(krc PRIVATE ${LLVM_DEFINITIONS})

if (TARGET LLVM)
    target_link_libraries(krc PRIVATE LLVM)
else()
    llvm_map_components_to_libnames(LLVM_LIBS all)
    target_link_libraries(krc PRIVATE ${LLVM_LIBS})
endif()

file(COPY libs DESTINATION ${CMAKE_BINARY_DIR})

include(GNUInstallDirs)
install(TARGETS krc RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
install(DIRECTORY libs DESTINATION ${CMAKE_INSTALL_DATADIR}/kairo)