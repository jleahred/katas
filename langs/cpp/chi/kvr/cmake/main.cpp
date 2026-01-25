#include <chrono>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <memory>
#include <string>
#include <thread>

#include "kvr.h"
#include "rest_server.h"

int main(int argc, char* argv[]) {
    const auto enable_rest = argc > 1 && std::string(argv[1]) == "rest";
    std::unique_ptr<kvr::RestServer> mut_rest_server;
    std::unique_ptr<kvr::RestServer> mut_rest_client_server;
    const std::string bucket_name = "users";
    kvr::Kvr server_store("kvr_server.db");
    kvr::Kvr client_store("kvr_client.db");

    if (enable_rest) {
        mut_rest_server = std::make_unique<kvr::RestServer>(server_store, "127.0.0.1", 8080);
        if (!mut_rest_server->start()) {
            std::cerr << "REST start failed: " << mut_rest_server->last_error() << "\n";
            return 1;
        }
        std::cout << "REST server running on 127.0.0.1:8080\n";

        mut_rest_client_server = std::make_unique<kvr::RestServer>(client_store, "127.0.0.1", 8081);
        if (!mut_rest_client_server->start()) {
            std::cerr << "REST client start failed: " << mut_rest_client_server->last_error() << "\n";
            return 1;
        }
        std::cout << "REST client running on 127.0.0.1:8081\n";
    }

    server_store.add_poll_server(bucket_name, kvr::TcpSide::server, "127.0.0.1", 9000);
    client_store.add_poll_client(bucket_name, kvr::TcpSide::client, "127.0.0.1", 9000);

    if (!server_store.start()) {
        std::cerr << "Server start failed: " << server_store.last_error() << "\n";
        return 1;
    }
    if (!client_store.start()) {
        std::cerr << "Client start failed: " << client_store.last_error() << "\n";
        return 1;
    }

    const auto now = std::chrono::system_clock::now();
    const std::time_t now_time = std::chrono::system_clock::to_time_t(now);
    std::ostringstream stream;
    stream << "hello " << std::put_time(std::localtime(&now_time), "%Y-%m-%d %H:%M:%S");

    if (!server_store.set(bucket_name, "alice", stream.str())) {
        std::cerr << "Server set failed: " << server_store.last_error() << "\n";
        return 1;
    }
    std::cout << "writted " << bucket_name << "alice " << stream.str() << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(3));

    const auto value = client_store.get(bucket_name, "alice");
    if (!value.has_value()) {
        std::cerr << "Client get failed: " << client_store.last_error() << "\n";
        return 1;
    }

    std::cout << "Client value: " << value.value() << "\n";


    if (enable_rest) {
        std::cout << "Press Enter to stop REST server.\n";
        std::string line;
        std::getline(std::cin, line);
        mut_rest_server->stop();
        mut_rest_client_server->stop();
    }

    client_store.stop();
    server_store.stop();
    return 0;
}
