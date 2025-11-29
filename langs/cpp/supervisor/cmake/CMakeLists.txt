cmake_minimum_required(VERSION 3.16)

project(supervisor LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(supervisor
    main.cpp
    ../src/supervisor.cpp
    ../src/supervisor.h
)

include(GNUInstallDirs)
install(TARGETS supervisor
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

target_include_directories(supervisor
    PRIVATE
        ../src
)
