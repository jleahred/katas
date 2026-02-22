#pragma once

#include <asio.hpp>
#include <array>
#include <chrono>
#include <cstdint>
#include <memory>
#include <mutex>
#include <thread>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

struct sqlite3;

namespace kvr {

enum class TcpSide {
    server,
    client,
};

template <typename T>
class Optional {
public:
    Optional() : has_value_(false), value_() {}
    Optional(const T& v) : has_value_(true), value_(v) {}
    Optional(T&& v) : has_value_(true), value_(std::move(v)) {}

    bool has_value() const { return has_value_; }
    explicit operator bool() const { return has_value_; }
    const T& value() const { return value_; }
    T& value() { return value_; }

private:
    bool has_value_;
    T value_;
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
    Optional<std::string> get(const std::string& bucket_name, const std::string& key);

    struct PollServerBucketConfig {
        std::string name;
        uint32_t poll_max_reply_count;
    };

    struct PollClientBucketConfig {
        std::string name;
        std::chrono::milliseconds poll_interval;
        std::chrono::milliseconds poll_interval_active;
        uint32_t max_records_per_poll;
    };

    struct PollConfig {
        TcpSide tcp_side;
        std::string host;
        uint16_t port;
        std::vector<PollServerBucketConfig> poll_server_buckets;
        std::vector<PollClientBucketConfig> poll_client_buckets;
    };
    struct PollClientStatus {
        std::string bucket;
        bool connected;
        int64_t last_poll_ms_ago;
        int64_t last_reply_ms_ago;
    };

    std::vector<std::string> list_buckets();
    std::vector<PollConfig> list_poll_configs() const;
    std::vector<PollClientStatus> list_poll_client_status() const;
    size_t poll_connection_count() const;
    int64_t last_id_for_bucket(const std::string& bucket_name);
    struct KeyListResult {
        std::vector<std::string> keys;
        int64_t next_id;
        std::string next_key;
    };
    struct SubKeyListResult {
        std::vector<std::string> sub_keys;
        std::string last_full_key;
    };
    struct HistoryRecord {
        int64_t id;
        int64_t created_at;
        int64_t deleted_at;
        std::string value;
    };
    struct DbSize {
        int64_t db_bytes;
        int64_t wal_bytes;
        int64_t shm_bytes;
        int64_t total_bytes;
        int64_t page_count;
        int64_t page_size;
        int64_t freelist_count;
        int64_t used_bytes;
        int64_t free_bytes;
        double fragmentation_pct;
    };
    struct MetaInfo {
        int64_t max_deleted_per_key = 1;
    };

    KeyListResult list_active_keys(const std::string& bucket_name, const std::string& key_prefix,
        const std::string& start_key, int64_t next_id, int64_t skip, int64_t limit);
    SubKeyListResult get_sub_keys(const std::string& bucket_name, const std::string& sub_key,
        const std::string& last_full_key = std::string(), int64_t limit = 50);
    std::vector<HistoryRecord> list_history(const std::string& bucket_name, const std::string& key,
        int64_t start = 0, int64_t limit = 50);
    DbSize db_size() const;
    bool incremental_vacuum(int64_t pages = 0);
    bool compact_full();
    bool optimize();
    bool wal_checkpoint();
    bool begin_transaction();
    bool commit_transaction();
    bool rollback_transaction();
    bool set_meta_info(const std::string& bucket_name, const MetaInfo& info);
    MetaInfo get_meta_info(const std::string& bucket_name);

    void add_poll_server(const std::string& bucket_name, TcpSide tcp_side,
        const std::string& host, uint16_t port,
        uint32_t poll_max_reply_records_count = 10);
    void add_poll_client(const std::string& bucket_name, TcpSide tcp_side,
        const std::string& host, uint16_t port,
        std::chrono::milliseconds poll_interval = std::chrono::seconds(10),
        std::chrono::milliseconds poll_interval_active = std::chrono::milliseconds(1000),
        uint32_t max_records_per_poll = 3);
    bool start_polling();
    void stop_polling();

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
        std::chrono::milliseconds idle_interval;
        std::chrono::milliseconds active_interval;
        std::chrono::milliseconds current_interval;
        bool using_active;
        int empty_poll_streak;
        bool had_reply_since_last_poll;
        std::chrono::steady_clock::time_point last_poll_at;
        std::chrono::steady_clock::time_point last_reply_at;
        uint32_t max_records_per_poll;
        std::unique_ptr<asio::steady_timer> mut_timer;
    };

    struct PollConnection {
        TcpSide tcp_side;
        std::string host;
        uint16_t port;
        std::shared_ptr<asio::ip::tcp::socket> mut_socket;
        std::vector<PollServerBucketConfig> mut_poll_server_buckets;
        std::vector<PollClientState> mut_poll_client_states;
        std::array<uint8_t, 4> mut_header;
        std::vector<uint8_t> mut_body;
    };

    void add_poll_server_bucket(PollEndpoint& endpoint, const std::string& bucket_name,
        uint32_t poll_max_reply_count);
    void add_poll_client_bucket(PollEndpoint& endpoint, const std::string& bucket_name,
        std::chrono::milliseconds poll_interval, std::chrono::milliseconds poll_interval_active,
        uint32_t max_records_per_poll);
    PollEndpoint* find_poll_endpoint(TcpSide tcp_side, const std::string& host, uint16_t port);
    bool start_acceptor(PollEndpoint& endpoint);
    bool start_connect(PollEndpoint& endpoint);
    void start_polling(const std::shared_ptr<PollConnection>& connection);
    void schedule_poll(const std::shared_ptr<PollConnection>& connection, size_t index);
    void start_reading(const std::shared_ptr<PollConnection>& connection);
    void handle_disconnect(const std::shared_ptr<PollConnection>& connection);
    void handle_message(const std::shared_ptr<PollConnection>& connection);
    void send_poll(const std::shared_ptr<PollConnection>& connection, const std::string& bucket_name,
        uint32_t max_records_per_poll);
    void send_reply(const std::shared_ptr<PollConnection>& connection, const std::string& bucket_name,
        int64_t id, int64_t created_at, int64_t deleted_at, const std::string& key, const std::string& value);
    void send_reset(const std::shared_ptr<PollConnection>& connection, const std::string& bucket_name);
    bool get_first_record(const std::string& bucket_name, int64_t& id, std::string& key, std::string& value);
    int64_t get_last_id(const std::string& bucket_name);
    bool apply_reply(const std::string& bucket_name, int64_t id, int64_t created_at, int64_t deleted_at,
        const std::string& key, const std::string& value);
    void enforce_max_deleted_per_key(const std::string& bucket_name, const std::string& key);
    static bool is_meta_key(const std::string& key);
    MetaInfo get_meta_info_locked(const std::string& bucket_name);
    static std::string serialize_meta_info(const MetaInfo& info);
    static MetaInfo parse_meta_info(const std::string& value);
    void schedule_reconnect(TcpSide tcp_side, const std::string& host, uint16_t port);
    static std::string endpoint_key(TcpSide tcp_side, const std::string& host, uint16_t port);
    bool begin_if_needed(bool& owned_tx);

    sqlite3* mut_db_;
    std::string mut_db_path_;
    std::recursive_mutex mut_db_mutex_;
    std::string mut_last_error_;
    std::unordered_set<std::string> mut_ensured_buckets_;
    std::unordered_map<std::string, MetaInfo> mut_meta_info_;
    bool mut_in_transaction_;
    mutable std::mutex mut_poll_mutex_;

    std::vector<PollEndpoint> mut_poll_endpoints_;
    std::unique_ptr<asio::io_context> mut_io_context_;
    std::unique_ptr<asio::executor_work_guard<asio::io_context::executor_type>> mut_work_guard_;
    std::vector<std::unique_ptr<asio::ip::tcp::acceptor>> mut_acceptors_;
    std::vector<std::shared_ptr<PollConnection>> mut_connections_;
    std::unordered_map<std::string, std::unique_ptr<asio::steady_timer>> mut_reconnect_timers_;
    std::thread mut_io_thread_;
    bool mut_started_;
};

}  // namespace kvr
