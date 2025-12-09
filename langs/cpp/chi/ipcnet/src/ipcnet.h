#pragma once

#include <atomic>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <condition_variable>
#include <deque>
#include <filesystem>
#include <functional>
#include <future>
#include <memory>
#include <mutex>
#include <span>
#include <stop_token>
#include <string>
#include <string_view>
#include <thread>
#include <unordered_map>
#include <vector>

namespace chi {
class QueueWriter;
class QueueReader;
}

namespace chi::ipcnet {

class TcpSession;

struct NetThrottleConfig {
    std::size_t max_chunk_size{64 * 1024};  // bytes
    std::chrono::milliseconds inter_chunk_delay{std::chrono::milliseconds{0}};
};

struct NetStats {
    std::uint64_t messages{0};
    std::uint64_t bytes{0};
    std::uint64_t chunks{0};

    void merge(const NetStats& other) {
        messages += other.messages;
        bytes += other.bytes;
        chunks += other.chunks;
    }
};

struct ReadBatch {
    std::vector<std::string> messages;
    bool disconnected{false};
    bool error{false};
    bool signature_changed{false};
};

using QueuePathResolver = std::function<std::filesystem::path(const std::string& queue_name)>;

struct ClientCallbacks {
    // Invoked from the internal thread when the connection is established.
    std::function<void()> on_th_connected;
    // Invoked from the internal thread when the connection is lost or stopped.
    std::function<void()> on_th_disconnected;
    // Invoked from the internal thread to report cumulative statistics.
    std::function<void(const NetStats&)> on_th_stats;
};

struct ServerCallbacks {
    // Invoked from the internal thread when a new client connects.
    std::function<void(const std::string&)> on_th_client_connected;
    // Invoked from the internal thread when a client disconnects.
    std::function<void(const std::string&)> on_th_client_disconnected;
    // Invoked from the internal thread to report cumulative statistics per queue.
    // The queue_name argument is the logical queue requested by the client.
    std::function<void(const std::string&, const NetStats&)> on_th_stats;
};

class ClientWriter {
public:
    ClientWriter(std::string host,
                 std::uint16_t port,
                 NetThrottleConfig throttle = {},
                 ClientCallbacks callbacks = {});
    ~ClientWriter();

    void start();
    void stop();

    void enqueue(const std::string& queue_name, std::span<const std::byte> payload);
    void enqueue(const std::string& queue_name, std::string_view payload);

    NetStats stats() const;
    bool running() const { return mut_running_; }

private:
    struct PendingWrite {
        std::string mut_queue_name;
        std::vector<std::byte> mut_payload;
    };

    const std::string host_;
    const std::uint16_t port_{0};
    const NetThrottleConfig throttle_;
    const ClientCallbacks callbacks_;

    std::atomic<bool> mut_running_{false};
    std::atomic<bool> mut_connected_{false};
    std::jthread mut_worker_;
    mutable std::mutex mut_mutex_;
    std::condition_variable mut_cv_;
    std::deque<PendingWrite> mut_queue_;
    NetStats mut_stats_{};

    void run(std::stop_token stop_token);
};

class ClientReader {
public:
    ClientReader(std::string host, std::uint16_t port, ClientCallbacks callbacks = {});
    ~ClientReader();

    void start();
    void stop();

    std::future<ReadBatch> request(const std::string& queue_name, std::size_t max_records);

    NetStats stats() const;
    bool running() const { return mut_running_; }

private:
    struct PendingRead {
        std::string mut_queue_name;
        std::size_t mut_max_records{0};
        std::promise<ReadBatch> mut_promise;
    };

    const std::string host_;
    const std::uint16_t port_{0};
    const ClientCallbacks callbacks_;

    std::atomic<bool> mut_running_{false};
    std::atomic<bool> mut_connected_{false};
    std::jthread mut_worker_;
    mutable std::mutex mut_mutex_;
    std::condition_variable mut_cv_;
    std::deque<PendingRead> mut_queue_;
    NetStats mut_stats_{};

    void run(std::stop_token stop_token);
};

class ServerWriter {
public:
    ServerWriter(std::string bind_address,
                 std::uint16_t port,
                 QueuePathResolver resolver,
                 NetThrottleConfig throttle = {},
                 ServerCallbacks callbacks = {});
    ~ServerWriter();

    void start();
    void stop();
    bool running() const { return mut_running_; }

    // Drop queue and cursor on disk for a given logical queue name.
    bool drop_queue(const std::string& queue_name);

private:
    const std::string bind_address_;
    const std::uint16_t port_{0};
    const QueuePathResolver resolver_;
    const NetThrottleConfig throttle_;
    const ServerCallbacks callbacks_;

    std::atomic<bool> mut_running_{false};
    std::jthread mut_acceptor_;
    std::mutex mut_writers_mutex_;
    std::unordered_map<std::string, std::unique_ptr<class QueueWriter>> mut_writers_;
    std::mutex mut_clients_mutex_;
    std::vector<std::jthread> mut_client_threads_;

    void run(std::stop_token stop_token);
};

class ServerReader {
public:
    ServerReader(std::string bind_address,
                 std::uint16_t port,
                 QueuePathResolver resolver,
                 NetThrottleConfig throttle = {},
                 ServerCallbacks callbacks = {});
    ~ServerReader();

    void start();
    void stop();
    bool running() const { return mut_running_; }

    // Drop queue and cursor on disk for a given logical queue name.
    bool drop_queue(const std::string& queue_name);

private:
    const std::string bind_address_;
    const std::uint16_t port_{0};
    const QueuePathResolver resolver_;
    const NetThrottleConfig throttle_;
    const ServerCallbacks callbacks_;

    std::atomic<bool> mut_running_{false};
    std::jthread mut_acceptor_;
    std::mutex mut_clients_mutex_;
    std::vector<std::jthread> mut_client_threads_;

    void run(std::stop_token stop_token);
};

}  // namespace chi
