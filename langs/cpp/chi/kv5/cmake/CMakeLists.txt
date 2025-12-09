cmake_minimum_required(VERSION 3.16)

project(kv5 LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(kv5
    main.cpp
    ../src/kv5.cpp
    ../chi/ipcnet/src/ipc.cpp
    ../chi/ipcnet/src/ipc_extra.cpp
    ../chi/ipcnet/src/netipc.cpp
    ../third_party/sqlite/sqlite-amalgamation-3450200/sqlite3.c
)

find_package(Threads REQUIRED)

include(GNUInstallDirs)
install(TARGETS kv5
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

target_include_directories(kv5
    PRIVATE
        ../src
        ../chi/ipcnet/src
        ../chi/ipcnet/third_party/asio
        ../third_party/sqlite/sqlite-amalgamation-3450200
)

target_link_libraries(kv5
    PRIVATE
        Threads::Threads
)
