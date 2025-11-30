#pragma once

#include <filesystem>
#include <cstdint>
#include <memory>
#include <optional>
#include <regex>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace chi {

class ClientWriter;
class ClientReader;
class QueueWriter;
class QueueReader;

struct KvStoreConfig {
    std::filesystem::path db_path{"kv5.db"};
    std::size_t scan_window{50};
    std::string table{"kv_items"};
    std::string import_table{"kv_items_import"};
    std::filesystem::path queue_root{"queues"};
    std::string replica_host{"127.0.0.1"};
    std::uint16_t replica_writer_port{6001};
    std::uint16_t replica_reader_port{6002};
};

struct ScanResult {
    std::vector<std::string> values;
    std::int64_t continue_from{0};
};

struct PruneResult {
    std::int64_t continue_from{0};
    std::size_t pruned{0};
};

struct OutReplicaStats {
    std::string queue;
    std::int64_t last_sent{0};
    std::int64_t max_id{0};
};

struct InReplicaStats {
    std::string queue;
    std::int64_t last_imported{0};
    bool last_batch_empty{false};
};

struct LookupResult {
    std::vector<std::string> paths;
    std::vector<std::pair<std::string, std::string>> values;  // key, value (or extracted JSON)
    std::int64_t continue_from{0};
};

class KvStore {
public:
    explicit KvStore(KvStoreConfig config);
    ~KvStore();

    KvStore(const KvStore&) = delete;
    KvStore& operator=(const KvStore&) = delete;
    KvStore(KvStore&&) = delete;
    KvStore& operator=(KvStore&&) = delete;

    bool set_value(std::string_view key, std::string_view value);
    std::optional<std::string> get_value(std::string_view key) const;
    LookupResult get(std::string_view key_pattern, std::int64_t start_on) const;

    // ScanResult get_paths(std::string_view key_pattern, std::int64_t start_on) const;
    // ScanResult get_values(std::string_view key_pattern, std::int64_t start_on) const;

    // Replication helpers using chi::QueueReader/QueueWriter.
    void replicate_to(const std::string& queue_name);
    bool send2replica(const std::string& queue_name, std::size_t limit);
    void replicate_from(const std::string& queue_name);
    bool get_from_replica(std::size_t limit);
    bool replica2master();
    bool delete_replica_to(const std::string& queue_name);
    bool mark_deleted(std::string_view key);
    PruneResult prune_deleted(std::int64_t start_on);
    bool apply_replica_messages(const std::vector<std::string>& messages, bool update_meta = false);
    std::vector<std::string> replica_outputs() const { return replica_outputs_; }
    std::optional<std::string> replica_input() const { return replica_input_; }
    std::vector<OutReplicaStats> replica_outputs_stats() const;
    std::optional<InReplicaStats> replica_input_stats() const;

    struct JsonValue;
    struct PathToken;
    struct ValueSelector;

private:
    KvStoreConfig config_;
    void* db_{nullptr};  // sqlite3*, kept as void* in header to avoid exposing sqlite headers.

    std::unordered_map<std::string, std::unique_ptr<class QueueWriter>> mut_replica_writers_;
    std::unordered_map<std::string, std::unique_ptr<class ClientWriter>> mut_net_replica_writers_;
    std::unique_ptr<class ClientReader> mut_net_replica_reader_;
    std::vector<std::string> replica_outputs_;
    std::optional<std::string> replica_input_;
    bool use_import_replica_{false};

    bool ensure_tables();
    bool load_replication_config();
    bool upsert_replication_config(const std::string& queue_name, const std::string& direction);
    bool remove_replication_config(const std::string& queue_name);
    bool ensure_import_table() const;
    bool ensure_paths_table();
    bool rebuild_paths();
    void add_paths_for_key(const std::string& key);
    void remove_paths_for_key_if_unused(const std::string& key);

    std::string build_record_json(std::int64_t id, std::int64_t updated, std::string_view key, std::string_view value) const;
    std::string build_record_json(std::int64_t id, std::int64_t updated, std::string_view key, std::string_view value, bool is_deleted) const;
};

}  // namespace chi
