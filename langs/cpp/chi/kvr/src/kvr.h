#pragma once

#include <asio.hpp>
#include <array>
#include <chrono>
#include <cstdint>
#include <memory>
#include <mutex>
#include <optional>
#include <thread>
#include <string>
#include <unordered_set>
#include <vector>

struct sqlite3;

namespace kvr {

enum class TcpSide {
    server,
    client,
};

class Kvr {
public:
    explicit Kvr(const std::string& db_path);
    ~Kvr();

    Kvr(const Kvr&) = delete;
    Kvr& operator=(const Kvr&) = delete;

    bool ensure_bucket(const std::string& bucket_name);
    bool reset_bucket(const std::string& bucket_name);
    bool set(const std::string& bucket_name, const std::string& key, const std::string& value);
    bool del(const std::string& bucket_name, const std::string& key, const std::string& value);
    std::optional<std::string> get(const std::string& bucket_name, const std::string& key);

    struct PollServerBucketConfig {
        std::string name;
        uint32_t poll_max_reply_count;
    };

    struct PollClientBucketConfig {
        std::string name;
        std::chrono::milliseconds poll_interval;
    };

    struct PollConfig {
        TcpSide tcp_side;
        std::string host;
        uint16_t port;
        std::vector<PollServerBucketConfig> poll_server_buckets;
        std::vector<PollClientBucketConfig> poll_client_buckets;
    };

    std::vector<std::string> list_buckets();
    std::vector<PollConfig> list_poll_configs() const;
    int64_t last_id_for_bucket(const std::string& bucket_name);
    struct KeyListResult {
        std::vector<std::string> keys;
        int64_t next_id;
        std::string next_key;
    };

    KeyListResult list_active_keys(const std::string& bucket_name, const std::string& key_prefix,
        const std::string& start_key, int64_t next_id, int64_t skip, int64_t limit);

    void add_poll_server(const std::string& bucket_name, TcpSide tcp_side,
        const std::string& host, uint16_t port,
        uint32_t poll_max_reply_count = 3);
    void add_poll_client(const std::string& bucket_name, TcpSide tcp_side,
        const std::string& host, uint16_t port,
        std::chrono::milliseconds poll_interval = std::chrono::seconds(1));
    bool start();
    void stop();

    const std::string& last_error() const;

private:
    bool execute(const std::string& sql);
    bool is_valid_bucket_name(const std::string& bucket_name) const;
    std::string table_name(const std::string& bucket_name);
    void set_last_error(const std::string& message);

    struct PollEndpoint {
        TcpSide tcp_side;
        std::string host;
        uint16_t port;
        std::vector<PollServerBucketConfig> mut_poll_server_buckets;
        std::vector<PollClientBucketConfig> mut_poll_client_buckets;
        bool mut_active;
    };

    struct PollClientState {
        std::string name;
        std::chrono::milliseconds interval;
        std::unique_ptr<asio::steady_timer> mut_timer;
    };

    struct PollConnection {
        std::shared_ptr<asio::ip::tcp::socket> mut_socket;
        std::vector<PollServerBucketConfig> mut_poll_server_buckets;
        std::vector<PollClientState> mut_poll_client_states;
        std::array<uint8_t, 4> mut_header;
        std::vector<uint8_t> mut_body;
    };

    void add_poll_server_bucket(PollEndpoint& endpoint, const std::string& bucket_name,
        uint32_t poll_max_reply_count);
    void add_poll_client_bucket(PollEndpoint& endpoint, const std::string& bucket_name,
        std::chrono::milliseconds poll_interval);
    PollEndpoint* find_poll_endpoint(TcpSide tcp_side, const std::string& host, uint16_t port);
    bool start_acceptor(PollEndpoint& endpoint);
    bool start_connect(PollEndpoint& endpoint);
    void start_polling(const std::shared_ptr<PollConnection>& connection);
    void schedule_poll(const std::shared_ptr<PollConnection>& connection, size_t index);
    void start_reading(const std::shared_ptr<PollConnection>& connection);
    void handle_message(const std::shared_ptr<PollConnection>& connection);
    void send_poll(const std::shared_ptr<PollConnection>& connection, const std::string& bucket_name);
    void send_reply(const std::shared_ptr<PollConnection>& connection, const std::string& bucket_name,
        int64_t id, int64_t created_at, int64_t deleted_at, const std::string& key, const std::string& value);
    void send_reset(const std::shared_ptr<PollConnection>& connection, const std::string& bucket_name);
    bool get_first_record(const std::string& bucket_name, int64_t& id, std::string& key, std::string& value);
    int64_t get_last_id(const std::string& bucket_name);
    bool apply_reply(const std::string& bucket_name, int64_t id, int64_t created_at, int64_t deleted_at,
        const std::string& key, const std::string& value);

    sqlite3* mut_db_;
    std::recursive_mutex mut_db_mutex_;
    std::string mut_last_error_;
    std::unordered_set<std::string> mut_ensured_buckets_;
    mutable std::mutex mut_poll_mutex_;

    std::vector<PollEndpoint> mut_poll_endpoints_;
    std::unique_ptr<asio::io_context> mut_io_context_;
    std::unique_ptr<asio::executor_work_guard<asio::io_context::executor_type>> mut_work_guard_;
    std::vector<std::unique_ptr<asio::ip::tcp::acceptor>> mut_acceptors_;
    std::vector<std::shared_ptr<PollConnection>> mut_connections_;
    std::thread mut_io_thread_;
    bool mut_started_;
};

}  // namespace kvr
