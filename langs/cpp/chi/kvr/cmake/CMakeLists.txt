cmake_minimum_required(VERSION 3.16)

project(kvr LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(kvr
    main.cpp
    ../src/kvr.cpp
    ../src/kvr_sub_keys.cpp
    ../src/rest_server.cpp
    ../third_party/sqlite/sqlite-amalgamation-3450200/sqlite3.c
)

find_package(Threads REQUIRED)

include(GNUInstallDirs)
install(TARGETS kvr
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

target_include_directories(kvr
    PRIVATE
        ../src
        ../third_party/asio
        ../third_party/httplib
        ../third_party/sqlite/sqlite-amalgamation-3450200
)

target_link_libraries(kvr
    PRIVATE
        Threads::Threads
)
