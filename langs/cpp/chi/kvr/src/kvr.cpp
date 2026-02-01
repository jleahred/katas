#include "kvr.h"

#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <functional>
#include <sys/stat.h>
#include <sqlite3.h>

namespace {

struct StatementFinalizer {
  explicit StatementFinalizer(sqlite3_stmt *mut_stmt) : mut_stmt(mut_stmt) {}
  ~StatementFinalizer() {
    if (mut_stmt != nullptr) {
      sqlite3_finalize(mut_stmt);
    }
  }
  sqlite3_stmt *mut_stmt;
};

constexpr uint8_t k_message_poll = 1;
constexpr uint8_t k_message_reply = 2;
constexpr uint8_t k_message_reset = 3;
constexpr uint32_t k_max_message_size = 1024 * 1024;
constexpr const char* k_meta_key = "__meta_info__";
constexpr int k_poll_empty_streak_to_idle = 2;

struct PollRecord {
  int64_t id;
  int64_t created_at;
  int64_t deleted_at;
  std::string key;
  std::string value;
};

void append_uint16(std::vector<uint8_t> &buffer, uint16_t value) {
  buffer.push_back(static_cast<uint8_t>((value >> 8) & 0xFF));
  buffer.push_back(static_cast<uint8_t>(value & 0xFF));
}

void append_uint32(std::vector<uint8_t> &buffer, uint32_t value) {
  buffer.push_back(static_cast<uint8_t>((value >> 24) & 0xFF));
  buffer.push_back(static_cast<uint8_t>((value >> 16) & 0xFF));
  buffer.push_back(static_cast<uint8_t>((value >> 8) & 0xFF));
  buffer.push_back(static_cast<uint8_t>(value & 0xFF));
}

void append_uint64(std::vector<uint8_t> &buffer, uint64_t value) {
  for (int shift = 56; shift >= 0; shift -= 8) {
    buffer.push_back(static_cast<uint8_t>((value >> shift) & 0xFF));
  }
}

bool read_uint16(const std::vector<uint8_t> &buffer, size_t &offset,
                 uint16_t &value) {
  if (offset + 2 > buffer.size()) {
    return false;
  }
  value = static_cast<uint16_t>(buffer[offset] << 8 | buffer[offset + 1]);
  offset += 2;
  return true;
}

bool read_uint32(const std::vector<uint8_t> &buffer, size_t &offset,
                 uint32_t &value) {
  if (offset + 4 > buffer.size()) {
    return false;
  }
  value = (static_cast<uint32_t>(buffer[offset]) << 24) |
          (static_cast<uint32_t>(buffer[offset + 1]) << 16) |
          (static_cast<uint32_t>(buffer[offset + 2]) << 8) |
          static_cast<uint32_t>(buffer[offset + 3]);
  offset += 4;
  return true;
}

bool read_uint64(const std::vector<uint8_t> &buffer, size_t &offset,
                 uint64_t &value) {
  if (offset + 8 > buffer.size()) {
    return false;
  }
  uint64_t result = 0;
  for (uint64_t index = 0; index < 8; ++index) {
    result = (result << 8) | buffer[offset + index];
  }
  offset += 8;
  value = result;
  return true;
}

} // namespace

namespace kvr {

bool Kvr::is_meta_key(const std::string& key) {
  return key == k_meta_key;
}

std::string Kvr::serialize_meta_info(const MetaInfo& info) {
  return std::string("{\"max_deleted_per_key\":") + std::to_string(info.max_deleted_per_key) + "}";
}

Kvr::MetaInfo Kvr::parse_meta_info(const std::string& value) {
  MetaInfo info;
  const std::string key = "\"max_deleted_per_key\"";
  const auto key_pos = value.find(key);
  if (key_pos == std::string::npos) {
    return info;
  }
  auto pos = value.find(':', key_pos + key.size());
  if (pos == std::string::npos) {
    return info;
  }
  ++pos;
  while (pos < value.size() && std::isspace(static_cast<unsigned char>(value[pos]))) {
    ++pos;
  }
  if (pos >= value.size()) {
    return info;
  }
  const char* start = value.c_str() + pos;
  char* end = nullptr;
  const auto parsed = std::strtoll(start, &end, 10);
  if (end == start) {
    return info;
  }
  info.max_deleted_per_key = static_cast<int64_t>(parsed);
  return info;
}

Kvr::Kvr(const std::string &db_path)
    : mut_db_(nullptr),
      mut_db_path_(db_path),
      mut_started_(false) {
  const auto result = sqlite3_open(db_path.c_str(), &mut_db_);
  if (result != SQLITE_OK) {
    set_last_error("Failed to open sqlite database.");
    if (mut_db_ != nullptr) {
      sqlite3_close(mut_db_);
      mut_db_ = nullptr;
    }
    return;
  }

  const auto pragma_result = sqlite3_exec(mut_db_, "PRAGMA foreign_keys = ON;",
                                          nullptr, nullptr, nullptr);
  if (pragma_result != SQLITE_OK) {
    set_last_error("Failed to set sqlite pragmas.");
  }

  const auto wal_result = sqlite3_exec(mut_db_, "PRAGMA journal_mode = WAL;",
                                       nullptr, nullptr, nullptr);
  if (wal_result != SQLITE_OK) {
    set_last_error("Failed to enable sqlite WAL.");
  }

  const auto vacuum_result =
      sqlite3_exec(mut_db_, "PRAGMA auto_vacuum = INCREMENTAL;", nullptr,
                   nullptr, nullptr);
  if (vacuum_result != SQLITE_OK) {
    set_last_error("Failed to enable sqlite auto_vacuum.");
  }
}

namespace {

int64_t file_size_bytes(const std::string &path) {
  struct stat st;
  if (stat(path.c_str(), &st) != 0) {
    return 0;
  }
  return static_cast<int64_t>(st.st_size);
}

bool read_single_int64(sqlite3 *db, const std::string &sql, int64_t &out_value) {
  sqlite3_stmt *mut_stmt = nullptr;
  const auto prepare_result =
      sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt, nullptr);
  if (prepare_result != SQLITE_OK) {
    return false;
  }
  const StatementFinalizer finalizer(mut_stmt);

  const auto step_result = sqlite3_step(mut_stmt);
  if (step_result != SQLITE_ROW) {
    return false;
  }
  out_value = sqlite3_column_int64(mut_stmt, 0);
  return true;
}

}  // namespace

Kvr::~Kvr() {
  stop();
  if (mut_db_ != nullptr) {
    sqlite3_close(mut_db_);
    mut_db_ = nullptr;
  }
}

bool Kvr::ensure_bucket(const std::string &bucket_name) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  if (mut_db_ == nullptr) {
    set_last_error("Database is not open.");
    return false;
  }

  const auto table = table_name(bucket_name);
  if (table.empty()) {
    return false;
  }

  const auto create_table_sql = "CREATE TABLE IF NOT EXISTS \"" + table +
                                "\" ("
                                "id INTEGER PRIMARY KEY, "
                                "created_at INTEGER NOT NULL, "
                                "deleted_at INTEGER NOT NULL DEFAULT 0, "
                                "key TEXT NOT NULL, "
                                "value TEXT NOT NULL"
                                ");";
  if (!execute(create_table_sql)) {
    return false;
  }

  const auto id_index_name = table + "_id_idx";
  const auto create_id_index_sql = "CREATE UNIQUE INDEX IF NOT EXISTS \"" +
                                   id_index_name +
                                   "\" "
                                   "ON \"" +
                                   table + "\" (id);";
  if (!execute(create_id_index_sql)) {
    return false;
  }

  const auto index_name = table + "_key_deleted_idx";
  const auto create_index_sql = "CREATE INDEX IF NOT EXISTS \"" +
                                index_name +
                                "\" "
                                "ON \"" +
                                table + "\" (key, deleted_at);";
  if (!execute(create_index_sql)) {
    return false;
  }

  const auto active_index_name = table + "_key_active_idx";
  const auto create_active_index_sql =
      "CREATE UNIQUE INDEX IF NOT EXISTS \"" + active_index_name +
      "\" "
      "ON \"" +
      table + "\" (key, deleted_at) WHERE deleted_at = 0;";
  if (!execute(create_active_index_sql)) {
    return false;
  }

  const auto active_paging_index_name = table + "_key_active_id_idx";
  const auto create_active_paging_index_sql =
      "CREATE INDEX IF NOT EXISTS \"" + active_paging_index_name +
      "\" "
      "ON \"" +
      table + "\" (key, id) WHERE deleted_at = 0;";
  if (!execute(create_active_paging_index_sql)) {
    return false;
  }

  mut_ensured_buckets_.insert(bucket_name);
  return true;
}

bool Kvr::reset_bucket(const std::string &bucket_name) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  if (mut_db_ == nullptr) {
    set_last_error("Database is not open.");
    return false;
  }

  const auto table = table_name(bucket_name);
  if (table.empty()) {
    return false;
  }

  const auto drop_sql = "DROP TABLE IF EXISTS \"" + table + "\";";
  if (!execute(drop_sql)) {
    return false;
  }

  mut_ensured_buckets_.erase(bucket_name);
  mut_meta_info_.erase(bucket_name);
  return ensure_bucket(bucket_name);
}

bool Kvr::set(const std::string &bucket_name, const std::string &key,
              const std::string &value) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  if (!ensure_bucket(bucket_name)) {
    return false;
  }

  const auto table = table_name(bucket_name);
  if (!execute("BEGIN IMMEDIATE;")) {
    return false;
  }

  const auto update_sql = "UPDATE \"" + table +
                          "\" "
                          "SET deleted_at = strftime('%s','now') "
                          "WHERE key = ? AND deleted_at = 0;";
  sqlite3_stmt *mut_update_stmt = nullptr;
  const auto update_prepare = sqlite3_prepare_v2(mut_db_, update_sql.c_str(),
                                                 -1, &mut_update_stmt, nullptr);
  if (update_prepare != SQLITE_OK) {
    set_last_error("Failed to prepare set update.");
    execute("ROLLBACK;");
    return false;
  }
  const StatementFinalizer update_finalizer(mut_update_stmt);

  sqlite3_bind_text(mut_update_stmt, 1, key.c_str(), -1, SQLITE_TRANSIENT);
  const auto update_step = sqlite3_step(mut_update_stmt);
  if (update_step != SQLITE_DONE) {
    set_last_error("Failed to execute set update.");
    execute("ROLLBACK;");
    return false;
  }

  const auto insert_sql = "INSERT INTO \"" + table +
                          "\" (created_at, deleted_at, key, value) "
                          "VALUES (strftime('%s','now'), 0, ?, ?);";
  sqlite3_stmt *mut_insert_stmt = nullptr;
  const auto insert_prepare = sqlite3_prepare_v2(mut_db_, insert_sql.c_str(),
                                                 -1, &mut_insert_stmt, nullptr);
  if (insert_prepare != SQLITE_OK) {
    set_last_error("Failed to prepare set insert.");
    execute("ROLLBACK;");
    return false;
  }
  const StatementFinalizer insert_finalizer(mut_insert_stmt);

  sqlite3_bind_text(mut_insert_stmt, 1, key.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_text(mut_insert_stmt, 2, value.c_str(), -1, SQLITE_TRANSIENT);

  const auto insert_step = sqlite3_step(mut_insert_stmt);
  if (insert_step != SQLITE_DONE) {
    set_last_error("Failed to execute set insert.");
    execute("ROLLBACK;");
    return false;
  }

  if (!execute("COMMIT;")) {
    execute("ROLLBACK;");
    return false;
  }

  enforce_max_deleted_per_key(bucket_name, key);
  return true;
}

bool Kvr::del(const std::string &bucket_name, const std::string &key,
              const std::string &value) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  if (!ensure_bucket(bucket_name)) {
    return false;
  }

  const auto table = table_name(bucket_name);
  if (!execute("BEGIN IMMEDIATE;")) {
    return false;
  }

  const auto sql = "UPDATE \"" + table +
                   "\" "
                   "SET deleted_at = strftime('%s','now') "
                   "WHERE key = ? AND deleted_at = 0;";

  sqlite3_stmt *mut_stmt = nullptr;
  const auto prepare_result =
      sqlite3_prepare_v2(mut_db_, sql.c_str(), -1, &mut_stmt, nullptr);
  if (prepare_result != SQLITE_OK) {
    set_last_error("Failed to prepare delete statement.");
    execute("ROLLBACK;");
    return false;
  }
  const StatementFinalizer finalizer(mut_stmt);

  sqlite3_bind_text(mut_stmt, 1, key.c_str(), -1, SQLITE_TRANSIENT);

  const auto step_result = sqlite3_step(mut_stmt);
  if (step_result != SQLITE_DONE) {
    set_last_error("Failed to execute delete statement.");
    execute("ROLLBACK;");
    return false;
  }

  const auto changes = sqlite3_changes(mut_db_);
  if (changes <= 0) {
    execute("ROLLBACK;");
    return false;
  }

  const auto insert_sql = "INSERT INTO \"" + table +
                          "\" (created_at, deleted_at, key, value) "
                          "VALUES (strftime('%s','now'), strftime('%s','now'), ?, ?);";
  sqlite3_stmt *mut_insert_stmt = nullptr;
  const auto insert_prepare = sqlite3_prepare_v2(mut_db_, insert_sql.c_str(),
                                                 -1, &mut_insert_stmt, nullptr);
  if (insert_prepare != SQLITE_OK) {
    set_last_error("Failed to prepare delete insert.");
    execute("ROLLBACK;");
    return false;
  }
  const StatementFinalizer insert_finalizer(mut_insert_stmt);

  sqlite3_bind_text(mut_insert_stmt, 1, key.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_text(mut_insert_stmt, 2, value.c_str(), -1, SQLITE_TRANSIENT);

  const auto insert_step = sqlite3_step(mut_insert_stmt);
  if (insert_step != SQLITE_DONE) {
    set_last_error("Failed to execute delete insert.");
    execute("ROLLBACK;");
    return false;
  }

  if (!execute("COMMIT;")) {
    execute("ROLLBACK;");
    return false;
  }

  enforce_max_deleted_per_key(bucket_name, key);
  return true;
}

Optional<std::string> Kvr::get(const std::string &bucket_name,
                               const std::string &key) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  if (!ensure_bucket(bucket_name)) {
    return Optional<std::string>();
  }

  const auto table = table_name(bucket_name);
  const auto sql = "SELECT value FROM \"" + table +
                   "\" "
                   "WHERE key = ? AND deleted_at = 0 LIMIT 1;";

  sqlite3_stmt *mut_stmt = nullptr;
  const auto prepare_result =
      sqlite3_prepare_v2(mut_db_, sql.c_str(), -1, &mut_stmt, nullptr);
  if (prepare_result != SQLITE_OK) {
    set_last_error("Failed to prepare get statement.");
    return Optional<std::string>();
  }
  const StatementFinalizer finalizer(mut_stmt);

  sqlite3_bind_text(mut_stmt, 1, key.c_str(), -1, SQLITE_TRANSIENT);

  const auto step_result = sqlite3_step(mut_stmt);
  if (step_result == SQLITE_ROW) {
    const auto mut_value =
        reinterpret_cast<const char *>(sqlite3_column_text(mut_stmt, 0));
    if (mut_value != nullptr) {
      return Optional<std::string>(std::string(mut_value));
    }
    return Optional<std::string>(std::string());
  }
  if (step_result == SQLITE_DONE) {
    return Optional<std::string>();
  }

  set_last_error("Failed to execute get statement.");
  return Optional<std::string>();
}

std::vector<std::string> Kvr::list_buckets() {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  std::vector<std::string> result;
  if (mut_db_ == nullptr) {
    set_last_error("Database is not open.");
    return result;
  }

  const auto sql =
      "SELECT name FROM sqlite_master "
      "WHERE type = 'table' AND name NOT LIKE 'sqlite_%' ORDER BY name ASC;";
  sqlite3_stmt *mut_stmt = nullptr;
  const auto prepare_result =
      sqlite3_prepare_v2(mut_db_, sql, -1, &mut_stmt, nullptr);
  if (prepare_result != SQLITE_OK) {
    set_last_error("Failed to prepare bucket list.");
    return result;
  }
  const StatementFinalizer finalizer(mut_stmt);

  while (sqlite3_step(mut_stmt) == SQLITE_ROW) {
    const auto mut_name =
        reinterpret_cast<const char *>(sqlite3_column_text(mut_stmt, 0));
    if (mut_name != nullptr) {
      result.emplace_back(mut_name);
    }
  }

  return result;
}

std::vector<Kvr::PollConfig> Kvr::list_poll_configs() const {
  const std::lock_guard<std::mutex> lock(mut_poll_mutex_);
  std::vector<PollConfig> result;
  result.reserve(mut_poll_endpoints_.size());
  for (const auto &endpoint : mut_poll_endpoints_) {
    PollConfig config;
    config.tcp_side = endpoint.tcp_side;
    config.host = endpoint.host;
    config.port = endpoint.port;
    config.poll_server_buckets = endpoint.mut_poll_server_buckets;
    config.poll_client_buckets = endpoint.mut_poll_client_buckets;
    result.push_back(config);
  }
  return result;
}

int64_t Kvr::last_id_for_bucket(const std::string &bucket_name) {
  return get_last_id(bucket_name);
}

Kvr::KeyListResult Kvr::list_active_keys(const std::string &bucket_name,
                                         const std::string &key_prefix,
                                         const std::string &start_key,
                                         int64_t next_id, int64_t skip,
                                         int64_t limit) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  KeyListResult result;
  result.next_id = 0;
  if (!ensure_bucket(bucket_name)) {
    return result;
  }

  if (limit <= 0) {
    return result;
  }

  const auto table = table_name(bucket_name);
  const auto sql = "SELECT key, id FROM \"" + table +
                   "\" "
                   "WHERE deleted_at = 0 AND key LIKE ? AND (key > ? OR (key = "
                   "? AND id > ?)) "
                   "ORDER BY key ASC, id ASC LIMIT ? OFFSET ?;";
  sqlite3_stmt *mut_stmt = nullptr;
  const auto prepare_result =
      sqlite3_prepare_v2(mut_db_, sql.c_str(), -1, &mut_stmt, nullptr);
  if (prepare_result != SQLITE_OK) {
    set_last_error("Failed to prepare active id list.");
    return result;
  }
  const StatementFinalizer finalizer(mut_stmt);

  const auto pattern = key_prefix + "%";
  sqlite3_bind_text(mut_stmt, 1, pattern.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_text(mut_stmt, 2, start_key.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_text(mut_stmt, 3, start_key.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_int64(mut_stmt, 4, static_cast<sqlite3_int64>(next_id));
  sqlite3_bind_int64(mut_stmt, 5, static_cast<sqlite3_int64>(limit));
  sqlite3_bind_int64(mut_stmt, 6, static_cast<sqlite3_int64>(skip));

  while (sqlite3_step(mut_stmt) == SQLITE_ROW) {
    const auto mut_key =
        reinterpret_cast<const char *>(sqlite3_column_text(mut_stmt, 0));
    const auto mut_id = sqlite3_column_int64(mut_stmt, 1);
    if (mut_key != nullptr) {
      result.keys.emplace_back(mut_key);
      result.next_key = result.keys.back();
      result.next_id = mut_id;
    }
  }

  return result;
}

std::vector<Kvr::HistoryRecord> Kvr::list_history(const std::string &bucket_name,
                                                  const std::string &key,
                                                  int64_t start,
                                                  int64_t limit) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  std::vector<HistoryRecord> result;
  if (!ensure_bucket(bucket_name)) {
    return result;
  }

  if (limit <= 0) {
    return result;
  }
  if (start < 0) {
    start = 0;
  }

  const auto table = table_name(bucket_name);
  const auto sql = "SELECT id, created_at, deleted_at, value FROM \"" + table +
                   "\" "
                   "WHERE key = ? ORDER BY id DESC LIMIT ? OFFSET ?;";
  sqlite3_stmt *mut_stmt = nullptr;
  const auto prepare_result =
      sqlite3_prepare_v2(mut_db_, sql.c_str(), -1, &mut_stmt, nullptr);
  if (prepare_result != SQLITE_OK) {
    set_last_error("Failed to prepare history query.");
    return result;
  }
  const StatementFinalizer finalizer(mut_stmt);

  sqlite3_bind_text(mut_stmt, 1, key.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_int64(mut_stmt, 2, static_cast<sqlite3_int64>(limit));
  sqlite3_bind_int64(mut_stmt, 3, static_cast<sqlite3_int64>(start));

  while (sqlite3_step(mut_stmt) == SQLITE_ROW) {
    HistoryRecord record;
    record.id = sqlite3_column_int64(mut_stmt, 0);
    record.created_at = sqlite3_column_int64(mut_stmt, 1);
    record.deleted_at = sqlite3_column_int64(mut_stmt, 2);
    const auto mut_value =
        reinterpret_cast<const char *>(sqlite3_column_text(mut_stmt, 3));
    record.value = mut_value == nullptr ? std::string() : std::string(mut_value);
    result.push_back(std::move(record));
  }

  return result;
}

Kvr::DbSize Kvr::db_size() const {
  DbSize result;
  result.db_bytes = 0;
  result.wal_bytes = 0;
  result.shm_bytes = 0;
  result.total_bytes = 0;
  result.page_count = 0;
  result.page_size = 0;
  result.freelist_count = 0;
  result.used_bytes = 0;
  result.free_bytes = 0;
  result.fragmentation_pct = 0.0;

  if (mut_db_ == nullptr) {
    return result;
  }

  read_single_int64(mut_db_, "PRAGMA page_count;", result.page_count);
  read_single_int64(mut_db_, "PRAGMA page_size;", result.page_size);
  read_single_int64(mut_db_, "PRAGMA freelist_count;", result.freelist_count);

  if (result.page_count > 0 && result.page_size > 0) {
    result.db_bytes = result.page_count * result.page_size;
    result.free_bytes = result.freelist_count * result.page_size;
    result.used_bytes = result.db_bytes - result.free_bytes;
    if (result.db_bytes > 0) {
      result.fragmentation_pct =
          (static_cast<double>(result.free_bytes) * 100.0) /
          static_cast<double>(result.db_bytes);
    }
  }

  result.wal_bytes = file_size_bytes(mut_db_path_ + "-wal");
  result.shm_bytes = file_size_bytes(mut_db_path_ + "-shm");
  result.total_bytes = result.db_bytes + result.wal_bytes + result.shm_bytes;
  return result;
}

bool Kvr::incremental_vacuum(int64_t pages) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  if (mut_db_ == nullptr) {
    set_last_error("Database is not open.");
    return false;
  }

  if (pages < 0) {
    pages = 0;
  }

  std::string sql = "PRAGMA incremental_vacuum";
  if (pages > 0) {
    sql += "(" + std::to_string(pages) + ")";
  }
  sql += ";";

  const auto result = sqlite3_exec(mut_db_, sql.c_str(), nullptr, nullptr, nullptr);
  if (result != SQLITE_OK) {
    set_last_error("Failed to run incremental vacuum.");
    return false;
  }
  return true;
}

bool Kvr::optimize() {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  if (mut_db_ == nullptr) {
    set_last_error("Database is not open.");
    return false;
  }

  const auto result =
      sqlite3_exec(mut_db_, "PRAGMA optimize;", nullptr, nullptr, nullptr);
  if (result != SQLITE_OK) {
    set_last_error("Failed to run sqlite optimize.");
    return false;
  }
  return true;
}

bool Kvr::set_meta_info(const std::string& bucket_name, const MetaInfo& info) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  if (!ensure_bucket(bucket_name)) {
    return false;
  }

  const auto value = serialize_meta_info(info);
  if (!set(bucket_name, k_meta_key, value)) {
    return false;
  }

  mut_meta_info_[bucket_name] = info;
  return true;
}

Kvr::MetaInfo Kvr::get_meta_info(const std::string& bucket_name) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  return get_meta_info_locked(bucket_name);
}

Kvr::MetaInfo Kvr::get_meta_info_locked(const std::string& bucket_name) {
  const auto it = mut_meta_info_.find(bucket_name);
  if (it != mut_meta_info_.end()) {
    return it->second;
  }

  MetaInfo info;
  const auto stored = get(bucket_name, k_meta_key);
  if (stored.has_value()) {
    info = parse_meta_info(stored.value());
  }
  mut_meta_info_[bucket_name] = info;
  return info;
}

void Kvr::enforce_max_deleted_per_key(const std::string& bucket_name,
                                      const std::string& key) {
  if (is_meta_key(key)) {
    return;
  }

  const auto info = get_meta_info_locked(bucket_name);
  if (info.max_deleted_per_key < 0) {
    return;
  }

  if (!ensure_bucket(bucket_name)) {
    return;
  }

  const auto table = table_name(bucket_name);
  if (info.max_deleted_per_key == 0) {
    const auto sql = "DELETE FROM \"" + table +
                     "\" WHERE key = ? AND deleted_at != 0;";
    sqlite3_stmt* mut_stmt = nullptr;
    const auto prepare_result =
        sqlite3_prepare_v2(mut_db_, sql.c_str(), -1, &mut_stmt, nullptr);
    if (prepare_result != SQLITE_OK) {
      set_last_error("Failed to prepare prune delete.");
      return;
    }
    const StatementFinalizer finalizer(mut_stmt);
    sqlite3_bind_text(mut_stmt, 1, key.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_step(mut_stmt);
    return;
  }

  const auto sql = "DELETE FROM \"" + table +
                   "\" WHERE key = ? AND deleted_at != 0 AND id NOT IN ("
                   "SELECT id FROM \"" +
                   table +
                   "\" WHERE key = ? AND deleted_at != 0 "
                   "ORDER BY id DESC LIMIT ?);";
  sqlite3_stmt* mut_stmt = nullptr;
  const auto prepare_result =
      sqlite3_prepare_v2(mut_db_, sql.c_str(), -1, &mut_stmt, nullptr);
  if (prepare_result != SQLITE_OK) {
    set_last_error("Failed to prepare prune delete.");
    return;
  }
  const StatementFinalizer finalizer(mut_stmt);

  sqlite3_bind_text(mut_stmt, 1, key.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_text(mut_stmt, 2, key.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_int64(mut_stmt, 3, static_cast<sqlite3_int64>(info.max_deleted_per_key));
  sqlite3_step(mut_stmt);
}

void Kvr::add_poll_server(const std::string &bucket_name, TcpSide tcp_side,
                          const std::string &host, uint16_t port,
                          uint32_t poll_max_reply_records_count) {
  const std::lock_guard<std::mutex> lock(mut_poll_mutex_);
  auto *mut_endpoint = find_poll_endpoint(tcp_side, host, port);
  if (mut_endpoint == nullptr) {
    mut_poll_endpoints_.push_back(
        PollEndpoint{tcp_side, host, port, {}, {}, false});
    mut_endpoint = &mut_poll_endpoints_.back();
  }

  add_poll_server_bucket(*mut_endpoint, bucket_name,
                         poll_max_reply_records_count);
  if (mut_started_) {
    if (mut_endpoint->tcp_side == TcpSide::server) {
      start_acceptor(*mut_endpoint);
    } else {
      start_connect(*mut_endpoint);
    }
  }
}

void Kvr::add_poll_client(const std::string &bucket_name, TcpSide tcp_side,
                          const std::string &host, uint16_t port,
                          std::chrono::milliseconds poll_interval,
                          std::chrono::milliseconds poll_interval_active,
                          uint32_t max_records_per_poll) {
  const std::lock_guard<std::mutex> lock(mut_poll_mutex_);
  auto *mut_endpoint = find_poll_endpoint(tcp_side, host, port);
  if (mut_endpoint == nullptr) {
    mut_poll_endpoints_.push_back(
        PollEndpoint{tcp_side, host, port, {}, {}, false});
    mut_endpoint = &mut_poll_endpoints_.back();
  }

  add_poll_client_bucket(*mut_endpoint, bucket_name, poll_interval,
                         poll_interval_active, max_records_per_poll);
  if (mut_started_) {
    if (mut_endpoint->tcp_side == TcpSide::server) {
      start_acceptor(*mut_endpoint);
    } else {
      start_connect(*mut_endpoint);
    }
  }
}

bool Kvr::start() {
  if (mut_started_) {
    return true;
  }

  mut_io_context_ = std::make_unique<asio::io_context>();
  mut_work_guard_ = std::make_unique<
      asio::executor_work_guard<asio::io_context::executor_type>>(
      asio::make_work_guard(*mut_io_context_));
  mut_started_ = true;

  bool result = true;
  {
    const std::lock_guard<std::mutex> lock(mut_poll_mutex_);
    for (auto &endpoint : mut_poll_endpoints_) {
      if (endpoint.tcp_side == TcpSide::server) {
        if (!start_acceptor(endpoint)) {
          result = false;
        }
      } else {
        if (!start_connect(endpoint)) {
          result = false;
        }
      }
    }
  }

  mut_io_thread_ = std::thread([this]() { mut_io_context_->run(); });

  return result;
}

void Kvr::stop() {
  if (!mut_started_) {
    return;
  }

  mut_started_ = false;

  for (const auto &connection : mut_connections_) {
    if (connection != nullptr && connection->mut_socket != nullptr) {
      asio::error_code mut_ec;
      connection->mut_socket->close(mut_ec);
    }
  }

  for (const auto &acceptor : mut_acceptors_) {
    if (acceptor != nullptr) {
      asio::error_code mut_ec;
      acceptor->close(mut_ec);
    }
  }

  mut_connections_.clear();
  mut_acceptors_.clear();
  for (auto &endpoint : mut_poll_endpoints_) {
    endpoint.mut_active = false;
  }

  if (mut_work_guard_ != nullptr) {
    mut_work_guard_.reset();
  }

  if (mut_io_context_ != nullptr) {
    mut_io_context_->stop();
  }

  if (mut_io_thread_.joinable()) {
    mut_io_thread_.join();
  }

  mut_io_context_.reset();
}

const std::string &Kvr::last_error() const { return mut_last_error_; }

bool Kvr::execute(const std::string &sql) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  char *mut_error = nullptr;
  const auto result =
      sqlite3_exec(mut_db_, sql.c_str(), nullptr, nullptr, &mut_error);
  if (result != SQLITE_OK) {
    if (mut_error != nullptr) {
      set_last_error(mut_error);
      sqlite3_free(mut_error);
    } else {
      set_last_error("SQLite execution failed.");
    }
    return false;
  }
  return true;
}

bool Kvr::is_valid_bucket_name(const std::string &bucket_name) const {
  if (bucket_name.empty()) {
    return false;
  }

  const auto first_char = static_cast<unsigned char>(bucket_name.front());
  if (!(std::isalpha(first_char) || bucket_name.front() == '_')) {
    return false;
  }

  for (const auto character : bucket_name) {
    const auto normalized = static_cast<unsigned char>(character);
    if (!(std::isalnum(normalized) || character == '_')) {
      return false;
    }
  }

  return true;
}

std::string Kvr::table_name(const std::string &bucket_name) {
  if (!is_valid_bucket_name(bucket_name)) {
    set_last_error("Invalid bucket name.");
    return {};
  }

  return bucket_name;
}

void Kvr::set_last_error(const std::string &message) {
  mut_last_error_ = message;
}

void Kvr::add_poll_server_bucket(PollEndpoint &endpoint,
                                 const std::string &bucket_name,
                                 uint32_t poll_max_reply_count) {
  const auto it =
      std::find_if(endpoint.mut_poll_server_buckets.begin(),
                   endpoint.mut_poll_server_buckets.end(),
                   [&bucket_name](const PollServerBucketConfig &config) {
                     return config.name == bucket_name;
                   });
  if (it == endpoint.mut_poll_server_buckets.end()) {
    endpoint.mut_poll_server_buckets.push_back(
        PollServerBucketConfig{bucket_name, poll_max_reply_count});
  } else {
    it->poll_max_reply_count = poll_max_reply_count;
  }
}

void Kvr::add_poll_client_bucket(PollEndpoint &endpoint,
                                 const std::string &bucket_name,
                                 std::chrono::milliseconds poll_interval,
                                 std::chrono::milliseconds poll_interval_active,
                                 uint32_t max_records_per_poll) {
  if (poll_interval_active.count() <= 0) {
    poll_interval_active = poll_interval;
  }
  const auto it =
      std::find_if(endpoint.mut_poll_client_buckets.begin(),
                   endpoint.mut_poll_client_buckets.end(),
                   [&bucket_name](const PollClientBucketConfig &config) {
                     return config.name == bucket_name;
                   });
  if (it == endpoint.mut_poll_client_buckets.end()) {
    endpoint.mut_poll_client_buckets.push_back(PollClientBucketConfig{
        bucket_name, poll_interval, poll_interval_active, max_records_per_poll});
  } else {
    it->poll_interval = poll_interval;
    it->poll_interval_active = poll_interval_active;
    it->max_records_per_poll = max_records_per_poll;
  }
}

Kvr::PollEndpoint *Kvr::find_poll_endpoint(TcpSide tcp_side,
                                           const std::string &host,
                                           uint16_t port) {
  for (auto &endpoint : mut_poll_endpoints_) {
    if (endpoint.tcp_side == tcp_side && endpoint.host == host &&
        endpoint.port == port) {
      return &endpoint;
    }
  }
  return nullptr;
}

bool Kvr::start_acceptor(PollEndpoint &endpoint) {
  if (mut_io_context_ == nullptr) {
    set_last_error("TCP context is not available.");
    return false;
  }
  if (endpoint.mut_active) {
    return true;
  }
  endpoint.mut_active = true;

  asio::error_code mut_ec;
  asio::ip::tcp::endpoint bind_endpoint;
  if (endpoint.host.empty()) {
    bind_endpoint = asio::ip::tcp::endpoint(asio::ip::tcp::v4(), endpoint.port);
  } else {
    const auto address = asio::ip::make_address(endpoint.host, mut_ec);
    if (mut_ec) {
      set_last_error("Invalid TCP server host.");
      return false;
    }
    bind_endpoint = asio::ip::tcp::endpoint(address, endpoint.port);
  }

  auto mut_acceptor =
      std::make_unique<asio::ip::tcp::acceptor>(*mut_io_context_);
  mut_acceptor->open(bind_endpoint.protocol(), mut_ec);
  if (mut_ec) {
    set_last_error("Failed to open TCP acceptor.");
    endpoint.mut_active = false;
    return false;
  }

  mut_acceptor->set_option(asio::ip::tcp::acceptor::reuse_address(true),
                           mut_ec);
  if (mut_ec) {
    set_last_error("Failed to set TCP reuse address.");
    endpoint.mut_active = false;
    return false;
  }

  mut_acceptor->bind(bind_endpoint, mut_ec);
  if (mut_ec) {
    set_last_error("Failed to bind TCP acceptor.");
    endpoint.mut_active = false;
    return false;
  }

  mut_acceptor->listen(asio::socket_base::max_listen_connections, mut_ec);
  if (mut_ec) {
    set_last_error("Failed to listen on TCP acceptor.");
    endpoint.mut_active = false;
    return false;
  }

  auto *mut_acceptor_ptr = mut_acceptor.get();
  mut_acceptors_.push_back(std::move(mut_acceptor));

  auto mut_accept_loop = std::make_shared<std::function<void()>>();
  *mut_accept_loop = [this, mut_acceptor_ptr,
                      poll_server = endpoint.mut_poll_server_buckets,
                      poll_client = endpoint.mut_poll_client_buckets,
                      mut_accept_loop]() {
    auto mut_socket = std::make_shared<asio::ip::tcp::socket>(*mut_io_context_);
    mut_acceptor_ptr->async_accept(
        *mut_socket, [this, poll_server, poll_client, mut_socket,
                      mut_accept_loop](const asio::error_code &ec) {
          if (!ec) {
            auto connection = std::make_shared<PollConnection>();
            connection->mut_socket = mut_socket;
            connection->mut_poll_server_buckets = poll_server;
            std::vector<PollClientState> client_states;
            client_states.reserve(poll_client.size());
            for (const auto &config : poll_client) {
              PollClientState state;
              state.name = config.name;
              state.idle_interval = config.poll_interval;
              state.active_interval = config.poll_interval_active;
              state.current_interval = config.poll_interval;
              state.using_active = false;
              state.empty_poll_streak = 0;
              state.had_reply_since_last_poll = false;
              state.max_records_per_poll = config.max_records_per_poll;
              client_states.push_back(std::move(state));
            }
            connection->mut_poll_client_states = std::move(client_states);
            mut_connections_.push_back(connection);
            start_reading(connection);
            start_polling(connection);
          }
          if (mut_started_) {
            (*mut_accept_loop)();
          }
        });
  };

  (*mut_accept_loop)();
  return true;
}

bool Kvr::start_connect(PollEndpoint &endpoint) {
  if (mut_io_context_ == nullptr) {
    set_last_error("TCP context is not available.");
    return false;
  }
  if (endpoint.mut_active) {
    return true;
  }
  endpoint.mut_active = true;

  auto mut_socket = std::make_shared<asio::ip::tcp::socket>(*mut_io_context_);
  auto mut_resolver =
      std::make_shared<asio::ip::tcp::resolver>(*mut_io_context_);
  const auto port_string = std::to_string(endpoint.port);
  mut_resolver->async_resolve(
      endpoint.host, port_string,
      [this, mut_socket, mut_resolver,
       poll_server = endpoint.mut_poll_server_buckets,
       poll_client = endpoint.mut_poll_client_buckets,
       endpoint_ptr =
           &endpoint](const asio::error_code &ec,
                      const asio::ip::tcp::resolver::results_type &results) {
        if (ec) {
          set_last_error("Failed to resolve TCP client host.");
          if (endpoint_ptr != nullptr) {
            endpoint_ptr->mut_active = false;
          }
          return;
        }

        asio::async_connect(
            *mut_socket, results,
            [this, mut_socket, poll_server, poll_client,
             endpoint_ptr](const asio::error_code &connect_ec,
                           const asio::ip::tcp::endpoint &) {
              if (connect_ec) {
                set_last_error("Failed to connect TCP client.");
                if (endpoint_ptr != nullptr) {
                  endpoint_ptr->mut_active = false;
                }
                return;
              }
              auto connection = std::make_shared<PollConnection>();
              connection->mut_socket = mut_socket;
              connection->mut_poll_server_buckets = poll_server;
              std::vector<PollClientState> client_states;
              client_states.reserve(poll_client.size());
              for (const auto &config : poll_client) {
                PollClientState state;
                state.name = config.name;
                state.idle_interval = config.poll_interval;
                state.active_interval = config.poll_interval_active;
                state.current_interval = config.poll_interval;
                state.using_active = false;
                state.empty_poll_streak = 0;
                state.had_reply_since_last_poll = false;
                state.max_records_per_poll = config.max_records_per_poll;
                client_states.push_back(std::move(state));
              }
              connection->mut_poll_client_states = std::move(client_states);
              mut_connections_.push_back(connection);
              start_reading(connection);
              start_polling(connection);
            });
      });

  return true;
}

void Kvr::start_polling(const std::shared_ptr<PollConnection> &connection) {
  if (connection->mut_poll_client_states.empty()) {
    return;
  }

  for (size_t index = 0; index < connection->mut_poll_client_states.size();
       ++index) {
    auto &state = connection->mut_poll_client_states[index];
    state.mut_timer = std::make_unique<asio::steady_timer>(*mut_io_context_);
    schedule_poll(connection, index);
  }
}

void Kvr::schedule_poll(const std::shared_ptr<PollConnection> &connection,
                        size_t index) {
  if (!mut_started_ || index >= connection->mut_poll_client_states.size()) {
    return;
  }

  auto &state = connection->mut_poll_client_states[index];
  if (state.mut_timer == nullptr) {
    return;
  }

  state.mut_timer->expires_after(state.current_interval);
  state.mut_timer->async_wait(
      [this, connection, index](const asio::error_code &ec) {
        if (ec || !mut_started_) {
          return;
        }

        if (index < connection->mut_poll_client_states.size()) {
          auto &state = connection->mut_poll_client_states[index];
          if (state.had_reply_since_last_poll) {
            state.empty_poll_streak = 0;
            state.using_active = true;
          } else if (state.using_active) {
            state.empty_poll_streak += 1;
            if (state.empty_poll_streak >= k_poll_empty_streak_to_idle) {
              state.using_active = false;
              state.empty_poll_streak = 0;
            }
          }

          state.current_interval =
              state.using_active ? state.active_interval : state.idle_interval;
          state.had_reply_since_last_poll = false;
          send_poll(connection, state.name, state.max_records_per_poll);
        }

        schedule_poll(connection, index);
      });
}

void Kvr::start_reading(const std::shared_ptr<PollConnection> &connection) {
  if (!mut_started_) {
    return;
  }

  asio::async_read(
      *connection->mut_socket, asio::buffer(connection->mut_header),
      [this, connection](const asio::error_code &ec, std::size_t) {
        if (ec) {
          return;
        }

        const uint32_t length =
            (static_cast<uint32_t>(connection->mut_header[0]) << 24) |
            (static_cast<uint32_t>(connection->mut_header[1]) << 16) |
            (static_cast<uint32_t>(connection->mut_header[2]) << 8) |
            static_cast<uint32_t>(connection->mut_header[3]);
        if (length == 0 || length > k_max_message_size) {
          return;
        }

        connection->mut_body.resize(length);
        asio::async_read(
            *connection->mut_socket, asio::buffer(connection->mut_body),
            [this, connection](const asio::error_code &body_ec, std::size_t) {
              if (body_ec) {
                return;
              }
              handle_message(connection);
              start_reading(connection);
            });
      });
}

void Kvr::handle_message(const std::shared_ptr<PollConnection> &connection) {
  if (connection->mut_body.empty()) {
    return;
  }

  size_t offset = 0;
  const auto type = connection->mut_body[offset++];

  if (type == k_message_poll) {
    uint16_t bucket_size = 0;
    if (!read_uint16(connection->mut_body, offset, bucket_size)) {
      return;
    }
    if (offset + bucket_size > connection->mut_body.size()) {
      return;
    }
    const auto bucket_name = std::string(
        reinterpret_cast<const char *>(connection->mut_body.data() + offset),
        bucket_size);
    offset += bucket_size;

    if (offset >= connection->mut_body.size()) {
      return;
    }
    const auto has_first = connection->mut_body[offset++] != 0;
    uint64_t first_id = 0;
    std::string first_key;
    std::string first_value;
    if (has_first) {
      uint32_t key_size = 0;
      uint32_t value_size = 0;
      if (!read_uint64(connection->mut_body, offset, first_id)) {
        return;
      }
      if (!read_uint32(connection->mut_body, offset, key_size)) {
        return;
      }
      if (!read_uint32(connection->mut_body, offset, value_size)) {
        return;
      }
      if (offset + key_size + value_size > connection->mut_body.size()) {
        return;
      }
      first_key.assign(
          reinterpret_cast<const char *>(connection->mut_body.data() + offset),
          key_size);
      offset += key_size;
      first_value.assign(
          reinterpret_cast<const char *>(connection->mut_body.data() + offset),
          value_size);
      offset += value_size;
    }

    uint64_t last_id = 0;
    if (!read_uint64(connection->mut_body, offset, last_id)) {
      return;
    }
    uint32_t requested_count = 0;
    if (offset < connection->mut_body.size()) {
      if (!read_uint32(connection->mut_body, offset, requested_count)) {
        return;
      }
    }

    const auto it =
        std::find_if(connection->mut_poll_server_buckets.begin(),
                     connection->mut_poll_server_buckets.end(),
                     [&bucket_name](const PollServerBucketConfig &config) {
                       return config.name == bucket_name;
                     });
    if (it == connection->mut_poll_server_buckets.end()) {
      return;
    }

    int64_t server_first_id = 0;
    std::string server_first_key;
    std::string server_first_value;
    const auto server_has_first = get_first_record(
        bucket_name, server_first_id, server_first_key, server_first_value);
    if (has_first) {
      if (!server_has_first ||
          static_cast<int64_t>(first_id) != server_first_id ||
          first_key != server_first_key || first_value != server_first_value) {
        send_reset(connection, bucket_name);
        return;
      }
    }

    std::vector<PollRecord> records;
    {
      const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
      if (!ensure_bucket(bucket_name)) {
        return;
      }

      const auto table = table_name(bucket_name);
      const auto sql = "SELECT id, created_at, deleted_at, key, value FROM \"" +
                       table +
                       "\" "
                       "WHERE id > ? ORDER BY id ASC LIMIT ?;";
      sqlite3_stmt *mut_stmt = nullptr;
      const auto prepare_result =
          sqlite3_prepare_v2(mut_db_, sql.c_str(), -1, &mut_stmt, nullptr);
      if (prepare_result != SQLITE_OK) {
        set_last_error("Failed to prepare poll query.");
        return;
      }
      const StatementFinalizer finalizer(mut_stmt);

      sqlite3_bind_int64(mut_stmt, 1, static_cast<sqlite3_int64>(last_id));
      auto limit = it->poll_max_reply_count;
      if (requested_count > 0) {
        limit = std::min(limit, requested_count);
      }
      sqlite3_bind_int(mut_stmt, 2, static_cast<int>(limit));

      while (sqlite3_step(mut_stmt) == SQLITE_ROW) {
        PollRecord record;
        record.id = sqlite3_column_int64(mut_stmt, 0);
        record.created_at = sqlite3_column_int64(mut_stmt, 1);
        record.deleted_at = sqlite3_column_int64(mut_stmt, 2);
        const auto mut_key =
            reinterpret_cast<const char *>(sqlite3_column_text(mut_stmt, 3));
        const auto mut_value =
            reinterpret_cast<const char *>(sqlite3_column_text(mut_stmt, 4));
        record.key = mut_key == nullptr ? std::string() : std::string(mut_key);
        record.value =
            mut_value == nullptr ? std::string() : std::string(mut_value);
        records.push_back(record);
      }
    }

    for (const auto &record : records) {
      send_reply(connection, bucket_name, record.id, record.created_at,
                 record.deleted_at, record.key, record.value);
    }
    return;
  }

  if (type == k_message_reply) {
    uint16_t bucket_size = 0;
    if (!read_uint16(connection->mut_body, offset, bucket_size)) {
      return;
    }
    if (offset + bucket_size > connection->mut_body.size()) {
      return;
    }
    const auto bucket_name = std::string(
        reinterpret_cast<const char *>(connection->mut_body.data() + offset),
        bucket_size);
    offset += bucket_size;

    uint64_t id = 0;
    uint64_t created_at = 0;
    uint64_t deleted_at = 0;
    if (!read_uint64(connection->mut_body, offset, id)) {
      return;
    }
    if (!read_uint64(connection->mut_body, offset, created_at)) {
      return;
    }
    if (!read_uint64(connection->mut_body, offset, deleted_at)) {
      return;
    }

    uint32_t key_size = 0;
    uint32_t value_size = 0;
    if (!read_uint32(connection->mut_body, offset, key_size)) {
      return;
    }
    if (!read_uint32(connection->mut_body, offset, value_size)) {
      return;
    }
    if (offset + key_size + value_size > connection->mut_body.size()) {
      return;
    }
    const auto key = std::string(
        reinterpret_cast<const char *>(connection->mut_body.data() + offset),
        key_size);
    offset += key_size;
    const auto value = std::string(
        reinterpret_cast<const char *>(connection->mut_body.data() + offset),
        value_size);

    const auto applied = apply_reply(bucket_name, static_cast<int64_t>(id),
                                     static_cast<int64_t>(created_at),
                                     static_cast<int64_t>(deleted_at), key,
                                     value);
    if (applied) {
      auto it = std::find_if(
          connection->mut_poll_client_states.begin(),
          connection->mut_poll_client_states.end(),
          [&bucket_name](const PollClientState &state) {
            return state.name == bucket_name;
          });
      if (it != connection->mut_poll_client_states.end()) {
        it->had_reply_since_last_poll = true;
      }
    }
    return;
  }

  if (type == k_message_reset) {
    uint16_t bucket_size = 0;
    if (!read_uint16(connection->mut_body, offset, bucket_size)) {
      return;
    }
    if (offset + bucket_size > connection->mut_body.size()) {
      return;
    }
    const auto bucket_name = std::string(
        reinterpret_cast<const char *>(connection->mut_body.data() + offset),
        bucket_size);
    reset_bucket(bucket_name);
  }
}

void Kvr::send_poll(const std::shared_ptr<PollConnection> &connection,
                    const std::string &bucket_name,
                    uint32_t max_records_per_poll) {
  int64_t first_id = 0;
  std::string first_key;
  std::string first_value;
  const auto has_first =
      get_first_record(bucket_name, first_id, first_key, first_value);
  const auto last_id = get_last_id(bucket_name);
  std::vector<uint8_t> payload;
  payload.reserve(1 + 2 + bucket_name.size() + 1 + 8 + 4 + 4 +
                  first_key.size() + first_value.size() + 8 + 4);
  payload.push_back(k_message_poll);
  append_uint16(payload, static_cast<uint16_t>(bucket_name.size()));
  payload.insert(payload.end(), bucket_name.begin(), bucket_name.end());
  payload.push_back(has_first ? 1 : 0);
  if (has_first) {
    append_uint64(payload, static_cast<uint64_t>(first_id));
    append_uint32(payload, static_cast<uint32_t>(first_key.size()));
    append_uint32(payload, static_cast<uint32_t>(first_value.size()));
    payload.insert(payload.end(), first_key.begin(), first_key.end());
    payload.insert(payload.end(), first_value.begin(), first_value.end());
  }
  append_uint64(payload, static_cast<uint64_t>(last_id));
  append_uint32(payload, max_records_per_poll);

  const uint32_t length = static_cast<uint32_t>(payload.size());
  std::vector<uint8_t> frame;
  frame.reserve(length + 4);
  append_uint32(frame, length);
  frame.insert(frame.end(), payload.begin(), payload.end());

  auto mut_data = std::make_shared<std::vector<uint8_t>>(std::move(frame));
  asio::async_write(*connection->mut_socket, asio::buffer(*mut_data),
                    [this, mut_data](const asio::error_code &ec, std::size_t) {
                      if (ec) {
                        set_last_error("Failed to send poll message.");
                      }
                    });
}

void Kvr::send_reply(const std::shared_ptr<PollConnection> &connection,
                     const std::string &bucket_name, int64_t id,
                     int64_t created_at, int64_t deleted_at,
                     const std::string &key, const std::string &value) {
  std::vector<uint8_t> payload;
  payload.reserve(1 + 2 + bucket_name.size() + 8 + 8 + 8 + 4 + 4 + key.size() +
                  value.size());
  payload.push_back(k_message_reply);
  append_uint16(payload, static_cast<uint16_t>(bucket_name.size()));
  payload.insert(payload.end(), bucket_name.begin(), bucket_name.end());
  append_uint64(payload, static_cast<uint64_t>(id));
  append_uint64(payload, static_cast<uint64_t>(created_at));
  append_uint64(payload, static_cast<uint64_t>(deleted_at));
  append_uint32(payload, static_cast<uint32_t>(key.size()));
  append_uint32(payload, static_cast<uint32_t>(value.size()));
  payload.insert(payload.end(), key.begin(), key.end());
  payload.insert(payload.end(), value.begin(), value.end());

  const uint32_t length = static_cast<uint32_t>(payload.size());
  std::vector<uint8_t> frame;
  frame.reserve(length + 4);
  append_uint32(frame, length);
  frame.insert(frame.end(), payload.begin(), payload.end());

  auto mut_data = std::make_shared<std::vector<uint8_t>>(std::move(frame));
  asio::async_write(*connection->mut_socket, asio::buffer(*mut_data),
                    [this, mut_data](const asio::error_code &ec, std::size_t) {
                      if (ec) {
                        set_last_error("Failed to send reply message.");
                      }
                    });
}

void Kvr::send_reset(const std::shared_ptr<PollConnection> &connection,
                     const std::string &bucket_name) {
  std::vector<uint8_t> payload;
  payload.reserve(1 + 2 + bucket_name.size());
  payload.push_back(k_message_reset);
  append_uint16(payload, static_cast<uint16_t>(bucket_name.size()));
  payload.insert(payload.end(), bucket_name.begin(), bucket_name.end());

  const uint32_t length = static_cast<uint32_t>(payload.size());
  std::vector<uint8_t> frame;
  frame.reserve(length + 4);
  append_uint32(frame, length);
  frame.insert(frame.end(), payload.begin(), payload.end());

  auto mut_data = std::make_shared<std::vector<uint8_t>>(std::move(frame));
  asio::async_write(*connection->mut_socket, asio::buffer(*mut_data),
                    [this, mut_data](const asio::error_code &ec, std::size_t) {
                      if (ec) {
                        set_last_error("Failed to send reset message.");
                      }
                    });
}

bool Kvr::get_first_record(const std::string &bucket_name, int64_t &id,
                           std::string &key, std::string &value) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  if (!ensure_bucket(bucket_name)) {
    return false;
  }

  const auto table = table_name(bucket_name);
  const auto sql =
      "SELECT id, key, value FROM \"" + table + "\" ORDER BY id ASC LIMIT 1;";
  sqlite3_stmt *mut_stmt = nullptr;
  const auto prepare_result =
      sqlite3_prepare_v2(mut_db_, sql.c_str(), -1, &mut_stmt, nullptr);
  if (prepare_result != SQLITE_OK) {
    set_last_error("Failed to prepare first record query.");
    return false;
  }
  const StatementFinalizer finalizer(mut_stmt);

  const auto step_result = sqlite3_step(mut_stmt);
  if (step_result != SQLITE_ROW) {
    return false;
  }

  id = sqlite3_column_int64(mut_stmt, 0);
  const auto mut_key =
      reinterpret_cast<const char *>(sqlite3_column_text(mut_stmt, 1));
  const auto mut_value =
      reinterpret_cast<const char *>(sqlite3_column_text(mut_stmt, 2));
  key = mut_key == nullptr ? std::string() : std::string(mut_key);
  value = mut_value == nullptr ? std::string() : std::string(mut_value);
  return true;
}

int64_t Kvr::get_last_id(const std::string &bucket_name) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  if (!ensure_bucket(bucket_name)) {
    return 0;
  }

  const auto table = table_name(bucket_name);
  const auto sql = "SELECT COALESCE(MAX(id), 0) FROM \"" + table + "\";";
  sqlite3_stmt *mut_stmt = nullptr;
  const auto prepare_result =
      sqlite3_prepare_v2(mut_db_, sql.c_str(), -1, &mut_stmt, nullptr);
  if (prepare_result != SQLITE_OK) {
    set_last_error("Failed to prepare last id query.");
    return 0;
  }
  const StatementFinalizer finalizer(mut_stmt);

  const auto step_result = sqlite3_step(mut_stmt);
  if (step_result != SQLITE_ROW) {
    set_last_error("Failed to read last id.");
    return 0;
  }

  return sqlite3_column_int64(mut_stmt, 0);
}

bool Kvr::apply_reply(const std::string &bucket_name, int64_t id,
                      int64_t created_at, int64_t deleted_at,
                      const std::string &key, const std::string &value) {
  const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
  if (!ensure_bucket(bucket_name)) {
    return false;
  }

  const auto table = table_name(bucket_name);
  if (!execute("BEGIN IMMEDIATE;")) {
    return false;
  }

  const auto delete_sql = "UPDATE \"" + table +
                          "\" "
                          "SET deleted_at = strftime('%s','now') "
                          "WHERE key = ? AND deleted_at = 0 AND id != ?;";
  sqlite3_stmt *mut_delete_stmt = nullptr;
  const auto delete_prepare = sqlite3_prepare_v2(mut_db_, delete_sql.c_str(),
                                                 -1, &mut_delete_stmt, nullptr);
  if (delete_prepare != SQLITE_OK) {
    set_last_error("Failed to prepare reply delete.");
    execute("ROLLBACK;");
    return false;
  }
  const StatementFinalizer delete_finalizer(mut_delete_stmt);

  sqlite3_bind_text(mut_delete_stmt, 1, key.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_int64(mut_delete_stmt, 2, static_cast<sqlite3_int64>(id));
  const auto delete_step = sqlite3_step(mut_delete_stmt);
  if (delete_step != SQLITE_DONE) {
    set_last_error("Failed to execute reply delete.");
    execute("ROLLBACK;");
    return false;
  }

  const auto sql = "INSERT INTO \"" + table +
                   "\" (id, created_at, deleted_at, key, value) "
                   "VALUES (?, ?, ?, ?, ?) "
                   "ON CONFLICT(id) DO UPDATE SET "
                   "created_at = excluded.created_at, "
                   "deleted_at = excluded.deleted_at, "
                   "key = excluded.key, "
                   "value = excluded.value;";
  sqlite3_stmt *mut_stmt = nullptr;
  const auto prepare_result =
      sqlite3_prepare_v2(mut_db_, sql.c_str(), -1, &mut_stmt, nullptr);
  if (prepare_result != SQLITE_OK) {
    set_last_error("Failed to prepare reply insert.");
    return false;
  }
  const StatementFinalizer finalizer(mut_stmt);

  sqlite3_bind_int64(mut_stmt, 1, static_cast<sqlite3_int64>(id));
  sqlite3_bind_int64(mut_stmt, 2, static_cast<sqlite3_int64>(created_at));
  sqlite3_bind_int64(mut_stmt, 3, static_cast<sqlite3_int64>(deleted_at));
  sqlite3_bind_text(mut_stmt, 4, key.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_text(mut_stmt, 5, value.c_str(), -1, SQLITE_TRANSIENT);

  const auto step_result = sqlite3_step(mut_stmt);
  if (step_result != SQLITE_DONE) {
    set_last_error("Failed to apply reply.");
    execute("ROLLBACK;");
    return false;
  }

  if (!execute("COMMIT;")) {
    execute("ROLLBACK;");
    return false;
  }

  if (is_meta_key(key)) {
    if (deleted_at != 0) {
      mut_meta_info_.erase(bucket_name);
    } else {
      mut_meta_info_[bucket_name] = parse_meta_info(value);
    }
    return true;
  }

  enforce_max_deleted_per_key(bucket_name, key);
  return true;
}

} // namespace kvr
