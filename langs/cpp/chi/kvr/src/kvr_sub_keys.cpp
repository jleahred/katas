#include "kvr.h"

#include <sqlite3.h>

namespace kvr {
namespace {

struct StatementFinalizer {
    explicit StatementFinalizer(sqlite3_stmt* stmt) : mut_stmt(stmt) {}
    ~StatementFinalizer() {
        if (mut_stmt != nullptr) {
            sqlite3_finalize(mut_stmt);
        }
    }
    sqlite3_stmt* mut_stmt;
};

std::string normalize_prefix(const std::string& sub_key) {
    if (sub_key.empty()) {
        return "/";
    }
    if (sub_key == "/") {
        return sub_key;
    }
    std::string prefix = sub_key;
    if (prefix.back() != '/') {
        prefix.push_back('/');
    }
    return prefix;
}

std::string extract_sub_key(const std::string& prefix, const std::string& full_key) {
    if (full_key.size() <= prefix.size()) {
        return std::string();
    }
    const std::string remainder = full_key.substr(prefix.size());
    if (remainder.empty()) {
        return std::string();
    }
    const auto slash_pos = remainder.find('/');
    if (slash_pos == std::string::npos) {
        return remainder;
    }
    return remainder.substr(0, slash_pos);
}

bool select_first_key(sqlite3* db, const std::string& table, const std::string& prefix,
    const std::string& after_key, std::string& out_key, std::string& error) {
    const std::string sql = "SELECT key FROM \"" + table + "\" "
        "WHERE deleted_at = 0 AND key LIKE ? AND key > ? "
        "ORDER BY key ASC LIMIT 1;";
    sqlite3_stmt* mut_stmt = nullptr;
    const auto prepare_result = sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt, nullptr);
    if (prepare_result != SQLITE_OK) {
        error = "Failed to prepare sub key first query.";
        return false;
    }
    const StatementFinalizer finalizer(mut_stmt);

    const std::string pattern = prefix + "%";
    sqlite3_bind_text(mut_stmt, 1, pattern.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(mut_stmt, 2, after_key.c_str(), -1, SQLITE_TRANSIENT);

    const auto step_result = sqlite3_step(mut_stmt);
    if (step_result == SQLITE_ROW) {
        const auto mut_value = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt, 0));
        out_key = mut_value == nullptr ? std::string() : std::string(mut_value);
        return true;
    }
    if (step_result == SQLITE_DONE) {
        return false;
    }

    error = "Failed to execute sub key first query.";
    return false;
}

bool select_last_key(sqlite3* db, const std::string& table, const std::string& prefix,
    std::string& out_key, std::string& error) {
    const std::string sql = "SELECT key FROM \"" + table + "\" "
        "WHERE deleted_at = 0 AND key LIKE ? "
        "ORDER BY key DESC LIMIT 1;";
    sqlite3_stmt* mut_stmt = nullptr;
    const auto prepare_result = sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt, nullptr);
    if (prepare_result != SQLITE_OK) {
        error = "Failed to prepare sub key last query.";
        return false;
    }
    const StatementFinalizer finalizer(mut_stmt);

    const std::string pattern = prefix + "%";
    sqlite3_bind_text(mut_stmt, 1, pattern.c_str(), -1, SQLITE_TRANSIENT);

    const auto step_result = sqlite3_step(mut_stmt);
    if (step_result == SQLITE_ROW) {
        const auto mut_value = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt, 0));
        out_key = mut_value == nullptr ? std::string() : std::string(mut_value);
        return true;
    }
    if (step_result == SQLITE_DONE) {
        return false;
    }

    error = "Failed to execute sub key last query.";
    return false;
}

}  // namespace

Kvr::SubKeyListResult Kvr::get_sub_keys(const std::string& bucket_name, const std::string& sub_key,
    const std::string& last_full_key, int64_t limit) {
    SubKeyListResult result;
    if (limit <= 0) {
        return result;
    }

    const std::lock_guard<std::recursive_mutex> lock(mut_db_mutex_);
    if (!ensure_bucket(bucket_name)) {
        return result;
    }

    const std::string prefix = normalize_prefix(sub_key);
    std::string current_last = last_full_key;
    const auto table = table_name(bucket_name);

    int64_t count = 0;
    while (count < limit) {
        std::string first_key;
        std::string error;
        if (!select_first_key(mut_db_, table, prefix, current_last, first_key, error)) {
            if (!error.empty()) {
                set_last_error(error);
            }
            break;
        }

        if (first_key.size() <= prefix.size()) {
            current_last = first_key;
            continue;
        }

        const std::string sub = extract_sub_key(prefix, first_key);
        if (sub.empty()) {
            current_last = first_key;
            continue;
        }

        std::string sub_prefix = prefix + sub;
        if (sub_prefix.back() != '/') {
            sub_prefix.push_back('/');
        }

        std::string last_key_for_sub;
        error.clear();
        if (!select_last_key(mut_db_, table, sub_prefix, last_key_for_sub, error)) {
            if (!error.empty()) {
                set_last_error(error);
                break;
            }
            current_last = first_key;
            continue;
        }

        result.sub_keys.push_back(sub);
        current_last = last_key_for_sub;
        ++count;
    }

    result.last_full_key = current_last;
    return result;
}

}  // namespace kvr
