cmake_minimum_required(VERSION 3.16)

project(process LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(process
    main.cpp
    ../src/process.cpp
    ../src/process.h
)

include(GNUInstallDirs)
install(TARGETS process
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

target_include_directories(process
    PRIVATE
        ../src
)
