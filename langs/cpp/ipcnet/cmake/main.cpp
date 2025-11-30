#include <chrono>
#include <cstdint>
#include <filesystem>
#include <iostream>
#include <string>
#include <thread>

#include "../src/ipc.h"
#include "../src/netipc.h"

namespace {

void log_header(const std::string& title) {
    std::cout << "=== " << title << " ===\n";
}

}  // namespace

int main() {
    try {
        // Configure on-disk queue root and ensure it exists.
        const auto queue_root = std::filesystem::path("queues");
        std::filesystem::create_directories(queue_root);

        // Resolver maps logical queue names to disk files.
        const chi::QueuePathResolver resolver = [queue_root](const std::string& queue_name) {
            return queue_root / (queue_name + ".queue");
        };

        // Server side callbacks.
        chi::ServerCallbacks mut_server_callbacks{};
        mut_server_callbacks.on_th_client_connected = [](const std::string& queue_name) {
            std::cout << "[server] client connected for " << queue_name << "\n";
        };
        mut_server_callbacks.on_th_client_disconnected = [](const std::string& queue_name) {
            std::cout << "[server] client disconnected for " << queue_name << "\n";
        };
        mut_server_callbacks.on_th_stats = [](const std::string& queue_name,
                                              const chi::NetStats& stats) {
            std::cout << "[server] stats for " << queue_name << " msgs=" << stats.messages
                      << " bytes=" << stats.bytes << " chunks=" << stats.chunks << "\n";
        };

        const std::uint16_t writer_port = 6001;
        const std::uint16_t reader_port = 6002;

        // Server-side reader/writer listening on loopback.
        chi::ServerWriter mut_server_writer("127.0.0.1", writer_port, resolver, {}, mut_server_callbacks);
        chi::ServerReader mut_server_reader("127.0.0.1", reader_port, resolver, {}, mut_server_callbacks);
        mut_server_writer.start();
        mut_server_reader.start();

        // Client callbacks.
        chi::ClientCallbacks mut_client_callbacks{};
        mut_client_callbacks.on_th_connected = []() { std::cout << "[client] connected\n"; };
        mut_client_callbacks.on_th_disconnected = []() { std::cout << "[client] disconnected\n"; };
        mut_client_callbacks.on_th_stats = [](const chi::NetStats& stats) {
            std::cout << "[client] stats msgs=" << stats.messages << " bytes=" << stats.bytes
                      << " chunks=" << stats.chunks << "\n";
        };

        // Client writer/reader dial the writer/reader ports.
        chi::ClientWriter mut_client_writer("127.0.0.1", writer_port, {}, mut_client_callbacks);
        chi::ClientReader mut_client_reader("127.0.0.1", reader_port, mut_client_callbacks);
        mut_client_writer.start();
        mut_client_reader.start();

        // Enqueue some messages. With the current stub transport this only exercises the API.
        log_header("enqueue messages");
        mut_client_writer.enqueue("orders", std::string_view("alpha"));
        mut_client_writer.enqueue("orders", std::string_view("beta"));
        mut_client_writer.enqueue("orders", std::string_view("gamma"));

        // Request a batch of messages from the server.
        log_header("request batch");
        auto mut_future = mut_client_reader.request("orders", 10);

        // Wait a bit to simulate work and allow background threads to run.
        std::this_thread::sleep_for(std::chrono::milliseconds(200));

        const auto batch = mut_future.get();
        if (batch.error) {
            std::cout << "[client] error reading\n";
        } else if (batch.disconnected) {
            std::cout << "[client] disconnected before receiving data\n";
        } else {
            for (const auto& message : batch.messages) {
                std::cout << "[client] got: " << message << "\n";
            }
            if (batch.messages.empty()) {
                std::cout << "[client] no data\n";
            }
        }

        // Show current stats snapshot.
        const auto writer_stats = mut_client_writer.stats();
        std::cout << "[client] writer stats msgs=" << writer_stats.messages
                  << " bytes=" << writer_stats.bytes << " chunks=" << writer_stats.chunks << "\n";

        // Shutdown.
        mut_client_writer.stop();
        mut_client_reader.stop();
        mut_server_writer.stop();
        mut_server_reader.stop();

    } catch (const std::exception& ex) {
        std::cerr << "Error: " << ex.what() << "\n";
        return 1;
    }
    return 0;
}
