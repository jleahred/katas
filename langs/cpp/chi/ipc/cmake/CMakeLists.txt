cmake_minimum_required(VERSION 3.16)

project(ipc LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(ipc
    main.cpp
    ../src/ipc.cpp
    ../src/ipc.h
)

include(GNUInstallDirs)
install(TARGETS ipc
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

target_include_directories(ipc
    PRIVATE
        ../src
)
