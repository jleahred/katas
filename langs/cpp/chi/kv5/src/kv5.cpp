#include "kv5.h"

#include "../chi/ipcnet/src/ipc.h"
#include "../chi/ipcnet/src/netipc.h"

#include <algorithm>
#include <cctype>
#include <charconv>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <map>
#include <memory>
#include <optional>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

extern "C" {
#include <sqlite3.h>
}

namespace {

using SqlitePtr = sqlite3*;

struct Statement {
    sqlite3_stmt* stmt{nullptr};
    ~Statement() {
        if (stmt != nullptr) {
            sqlite3_finalize(stmt);
        }
    }
};

std::int64_t now_ms() {
    const auto now = std::chrono::system_clock::now();
    const auto since_epoch = now.time_since_epoch();
    return std::chrono::duration_cast<std::chrono::milliseconds>(since_epoch).count();
}

std::string trim_braces(std::string_view text) {
    if (text.size() >= 2 && text.front() == '{' && text.back() == '}') {
        return std::string(text.substr(1, text.size() - 2));
    }
    return std::string{text};
}

std::string trim_spaces(std::string_view text) {
    auto mut_begin = text.find_first_not_of(" \t\n\r");
    if (mut_begin == std::string_view::npos) {
        return {};
    }
    auto mut_end = text.find_last_not_of(" \t\n\r");
    return std::string{text.substr(mut_begin, mut_end - mut_begin + 1)};
}

std::string escape_literal_segment(std::string_view literal) {
    std::string mut_result;
    mut_result.reserve(literal.size());
    for (const auto ch : literal) {
        switch (ch) {
        case '.':
        case '^':
        case '$':
        case '|':
        case '(':
        case ')':
        case '[':
        case ']':
        case '{':
        case '}':
        case '*':
        case '+':
        case '?':
        case '\\':
            mut_result.push_back('\\');
            mut_result.push_back(ch);
            break;
        default:
            mut_result.push_back(ch);
            break;
        }
    }
    return mut_result;
}

std::vector<std::string> split_segments(std::string_view input) {
    std::vector<std::string> mut_segments;
    std::string mut_current;
    for (const auto ch : input) {
        if (ch == '/') {
            mut_segments.push_back(mut_current);
            mut_current.clear();
        } else {
            mut_current.push_back(ch);
        }
    }
    mut_segments.push_back(mut_current);
    return mut_segments;
}

std::regex build_key_regex(const std::string_view pattern,
                           const bool allow_value_selector,
                           std::string* value_selector_out) {
    const auto segments = split_segments(pattern);
    const bool leading_slash = !pattern.empty() && pattern.front() == '/';
    std::string mut_regex_text{"^"};
    if (leading_slash) {
        mut_regex_text.push_back('/');
    }

    const auto start_index = (leading_slash && !segments.empty() && segments.front().empty()) ? 1U : 0U;
    for (std::size_t i = start_index; i < segments.size(); ++i) {
        if (i > start_index) {
            mut_regex_text.push_back('/');
        }
        const auto& segment = segments.at(i);
        if (segment.size() >= 2 && segment.front() == '(' && segment.back() == ')') {
            const auto inside = trim_spaces(std::string_view(segment).substr(1, segment.size() - 2));
            mut_regex_text.append(inside);
        } else if (allow_value_selector && segment.size() >= 2 && segment.front() == '{' && segment.back() == '}') {
            const auto inside = trim_spaces(trim_braces(segment));
            if (!inside.empty() && inside.front() == '.') {
                if (value_selector_out != nullptr && value_selector_out->empty()) {
                    *value_selector_out = inside;
                }
                continue;
            }
            mut_regex_text.append(escape_literal_segment(segment));
        } else {
            mut_regex_text.append(escape_literal_segment(segment));
        }
    }

    mut_regex_text.push_back('$');
    return std::regex{mut_regex_text};
}

}  // namespace

namespace chi {

struct KvStore::JsonValue {
    using array_t = std::vector<JsonValue>;
    using object_t = std::map<std::string, JsonValue>;
    std::variant<std::nullptr_t, bool, double, std::string, array_t, object_t> payload;
};

struct KvStore::PathToken {
    std::string name;
    std::optional<std::size_t> index;
    std::optional<std::pair<std::size_t, std::size_t>> range;
};

struct KvStore::ValueSelector {
    std::vector<PathToken> tokens;
};

class JsonParser {
public:
    explicit JsonParser(std::string_view text) : text_(text) {}

    std::optional<KvStore::JsonValue> parse() {
        skip_ws();
        const auto value = parse_value();
        skip_ws();
        if (!value.has_value() || pos_ != text_.size()) {
            return std::nullopt;
        }
        return value;
    }

private:
    std::string_view text_;
    std::size_t pos_{0};

    void skip_ws() {
        while (pos_ < text_.size()) {
            const auto ch = text_[pos_];
            if (ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n') {
                ++pos_;
            } else {
                break;
            }
        }
    }

    std::optional<KvStore::JsonValue> parse_value() {
        skip_ws();
        if (pos_ >= text_.size()) {
            return std::nullopt;
        }
        const auto ch = text_[pos_];
        if (ch == '"') {
            return parse_string();
        }
        if (ch == '{') {
            return parse_object();
        }
        if (ch == '[') {
            return parse_array();
        }
        if (ch == 't' || ch == 'f') {
            return parse_bool();
        }
        if (ch == 'n') {
            return parse_null();
        }
        if (ch == '-' || (ch >= '0' && ch <= '9')) {
            return parse_number();
        }
        return std::nullopt;
    }

    std::optional<KvStore::JsonValue> parse_string() {
        if (text_[pos_] != '"') {
            return std::nullopt;
        }
        ++pos_;
        std::string mut_result;
        while (pos_ < text_.size()) {
            const auto ch = text_[pos_++];
            if (ch == '"') {
                KvStore::JsonValue mut_value;
                mut_value.payload = mut_result;
                return mut_value;
            }
            if (ch == '\\') {
                if (pos_ >= text_.size()) {
                    return std::nullopt;
                }
                const auto escaped = text_[pos_++];
                switch (escaped) {
                case '"':
                case '\\':
                case '/':
                    mut_result.push_back(escaped);
                    break;
                case 'b':
                    mut_result.push_back('\b');
                    break;
                case 'f':
                    mut_result.push_back('\f');
                    break;
                case 'n':
                    mut_result.push_back('\n');
                    break;
                case 'r':
                    mut_result.push_back('\r');
                    break;
                case 't':
                    mut_result.push_back('\t');
                    break;
                default:
                    return std::nullopt;
                }
            } else {
                mut_result.push_back(ch);
            }
        }
        return std::nullopt;
    }

    std::optional<KvStore::JsonValue> parse_number() {
        const auto start = pos_;
        if (text_[pos_] == '-') {
            ++pos_;
        }
        if (pos_ >= text_.size()) {
            return std::nullopt;
        }
        if (text_[pos_] == '0') {
            ++pos_;
        } else {
            while (pos_ < text_.size() && std::isdigit(static_cast<unsigned char>(text_[pos_]))) {
                ++pos_;
            }
        }
        if (pos_ < text_.size() && text_[pos_] == '.') {
            ++pos_;
            if (pos_ >= text_.size() || !std::isdigit(static_cast<unsigned char>(text_[pos_]))) {
                return std::nullopt;
            }
            while (pos_ < text_.size() && std::isdigit(static_cast<unsigned char>(text_[pos_]))) {
                ++pos_;
            }
        }
        if (pos_ < text_.size() && (text_[pos_] == 'e' || text_[pos_] == 'E')) {
            ++pos_;
            if (pos_ < text_.size() && (text_[pos_] == '+' || text_[pos_] == '-')) {
                ++pos_;
            }
            if (pos_ >= text_.size() || !std::isdigit(static_cast<unsigned char>(text_[pos_]))) {
                return std::nullopt;
            }
            while (pos_ < text_.size() && std::isdigit(static_cast<unsigned char>(text_[pos_]))) {
                ++pos_;
            }
        }
        const auto view = text_.substr(start, pos_ - start);
        double mut_number{0.0};
        const auto result = std::from_chars(view.data(), view.data() + view.size(), mut_number);
        if (result.ec != std::errc{}) {
            return std::nullopt;
        }
        KvStore::JsonValue mut_value;
        mut_value.payload = mut_number;
        return mut_value;
    }

    std::optional<KvStore::JsonValue> parse_bool() {
        if (text_.substr(pos_, 4) == "true") {
            pos_ += 4;
            KvStore::JsonValue mut_value;
            mut_value.payload = true;
            return mut_value;
        }
        if (text_.substr(pos_, 5) == "false") {
            pos_ += 5;
            KvStore::JsonValue mut_value;
            mut_value.payload = false;
            return mut_value;
        }
        return std::nullopt;
    }

    std::optional<KvStore::JsonValue> parse_null() {
        if (text_.substr(pos_, 4) != "null") {
            return std::nullopt;
        }
        pos_ += 4;
        KvStore::JsonValue mut_value;
        mut_value.payload = nullptr;
        return mut_value;
    }

    std::optional<KvStore::JsonValue> parse_array() {
        if (text_[pos_] != '[') {
            return std::nullopt;
        }
        ++pos_;
        skip_ws();
        KvStore::JsonValue::array_t mut_items;
        if (pos_ < text_.size() && text_[pos_] == ']') {
            ++pos_;
            KvStore::JsonValue mut_value;
            mut_value.payload = mut_items;
            return mut_value;
        }
        while (pos_ < text_.size()) {
            auto mut_item = parse_value();
            if (!mut_item.has_value()) {
                return std::nullopt;
            }
            mut_items.push_back(std::move(mut_item.value()));
            skip_ws();
            if (pos_ >= text_.size()) {
                return std::nullopt;
            }
            const auto ch = text_[pos_++];
            if (ch == ']') {
                KvStore::JsonValue mut_value;
                mut_value.payload = mut_items;
                return mut_value;
            }
            if (ch != ',') {
                return std::nullopt;
            }
            skip_ws();
        }
        return std::nullopt;
    }

    std::optional<KvStore::JsonValue> parse_object() {
        if (text_[pos_] != '{') {
            return std::nullopt;
        }
        ++pos_;
        skip_ws();
        KvStore::JsonValue::object_t mut_object;
        if (pos_ < text_.size() && text_[pos_] == '}') {
            ++pos_;
            KvStore::JsonValue mut_value;
            mut_value.payload = mut_object;
            return mut_value;
        }
        while (pos_ < text_.size()) {
            const auto key = parse_string();
            if (!key.has_value()) {
                return std::nullopt;
            }
            skip_ws();
            if (pos_ >= text_.size() || text_[pos_] != ':') {
                return std::nullopt;
            }
            ++pos_;
            const auto val = parse_value();
            if (!val.has_value()) {
                return std::nullopt;
            }
            const auto key_text = std::get<std::string>(key.value().payload);
            mut_object.emplace(key_text, std::move(val.value()));
            skip_ws();
            if (pos_ >= text_.size()) {
                return std::nullopt;
            }
            const auto ch = text_[pos_++];
            if (ch == '}') {
                KvStore::JsonValue mut_value;
                mut_value.payload = mut_object;
                return mut_value;
            }
            if (ch != ',') {
                return std::nullopt;
            }
            skip_ws();
        }
        return std::nullopt;
    }
};

std::string to_json_string(const KvStore::JsonValue& value);

std::string serialize_object(const KvStore::JsonValue::object_t& object) {
    std::string mut_result{"{"};
    bool mut_first{true};
    for (const auto& [key, val] : object) {
        if (!mut_first) {
            mut_result.append(",");
        }
        mut_first = false;
        mut_result.push_back('"');
        for (const auto ch : key) {
            if (ch == '"' || ch == '\\') {
                mut_result.push_back('\\');
            }
            mut_result.push_back(ch);
        }
        mut_result.push_back('"');
        mut_result.push_back(':');
        mut_result.append(to_json_string(val));
    }
    mut_result.push_back('}');
    return mut_result;
}

std::string serialize_array(const KvStore::JsonValue::array_t& array) {
    std::string mut_result{"["};
    for (std::size_t i = 0; i < array.size(); ++i) {
        if (i > 0) {
            mut_result.append(",");
        }
        mut_result.append(to_json_string(array.at(i)));
    }
    mut_result.push_back(']');
    return mut_result;
}

std::string to_json_string(const KvStore::JsonValue& value) {
    const auto& payload = value.payload;
    if (std::holds_alternative<std::nullptr_t>(payload)) {
        return "null";
    }
    if (std::holds_alternative<bool>(payload)) {
        return std::get<bool>(payload) ? "true" : "false";
    }
    if (std::holds_alternative<double>(payload)) {
        std::ostringstream mut_stream;
        mut_stream << std::get<double>(payload);
        return mut_stream.str();
    }
    if (std::holds_alternative<std::string>(payload)) {
        std::string mut_result{"\""};
        for (const auto ch : std::get<std::string>(payload)) {
            if (ch == '"' || ch == '\\') {
                mut_result.push_back('\\');
            }
            mut_result.push_back(ch);
        }
        mut_result.push_back('"');
        return mut_result;
    }
    if (std::holds_alternative<KvStore::JsonValue::array_t>(payload)) {
        return serialize_array(std::get<KvStore::JsonValue::array_t>(payload));
    }
    return serialize_object(std::get<KvStore::JsonValue::object_t>(payload));
}

KvStore::JsonValue clone_json(const KvStore::JsonValue& value) {
    const auto& payload = value.payload;
    if (std::holds_alternative<KvStore::JsonValue::array_t>(payload)) {
        KvStore::JsonValue::array_t mut_copy;
        for (const auto& item : std::get<KvStore::JsonValue::array_t>(payload)) {
            mut_copy.push_back(clone_json(item));
        }
        KvStore::JsonValue result;
        result.payload = std::move(mut_copy);
        return result;
    }
    if (std::holds_alternative<KvStore::JsonValue::object_t>(payload)) {
        KvStore::JsonValue::object_t mut_copy;
        for (const auto& [key, val] : std::get<KvStore::JsonValue::object_t>(payload)) {
            mut_copy.emplace(key, clone_json(val));
        }
        KvStore::JsonValue result;
        result.payload = std::move(mut_copy);
        return result;
    }
    return value;
}

std::optional<KvStore::JsonValue> select_path(const KvStore::JsonValue& root, const KvStore::ValueSelector& selector) {
    const KvStore::JsonValue* mut_current = &root;
    for (const auto& token : selector.tokens) {
        if (!std::holds_alternative<KvStore::JsonValue::object_t>(mut_current->payload)) {
            return std::nullopt;
        }
        const auto& object = std::get<KvStore::JsonValue::object_t>(mut_current->payload);
        const auto it = object.find(token.name);
        if (it == object.end()) {
            return std::nullopt;
        }
        mut_current = &it->second;
        if (token.index.has_value() || token.range.has_value()) {
            if (!std::holds_alternative<KvStore::JsonValue::array_t>(mut_current->payload)) {
                return std::nullopt;
            }
            const auto& array = std::get<KvStore::JsonValue::array_t>(mut_current->payload);
            if (token.index.has_value()) {
                const auto index = token.index.value();
                if (index >= array.size()) {
                    return std::nullopt;
                }
                mut_current = &array.at(index);
            } else if (token.range.has_value()) {
                const auto [start, end] = token.range.value();
                if (start >= array.size()) {
                    KvStore::JsonValue::array_t empty;
                    KvStore::JsonValue result;
                    result.payload = empty;
                    return result;
                }
                const auto safe_end = std::min<std::size_t>(end, array.size() - 1);
                KvStore::JsonValue::array_t mut_slice;
                for (auto mut_i = start; mut_i <= safe_end; ++mut_i) {
                    mut_slice.push_back(clone_json(array.at(mut_i)));
                }
                KvStore::JsonValue result;
                result.payload = mut_slice;
                return result;
            }
        }
    }
    return clone_json(*mut_current);
}

std::optional<std::vector<KvStore::ValueSelector>> parse_value_selectors(const std::string& selector_text) {
    std::vector<KvStore::ValueSelector> mut_selectors;
    std::size_t mut_pos{0};
    while (mut_pos < selector_text.size()) {
        while (mut_pos < selector_text.size() && std::isspace(static_cast<unsigned char>(selector_text[mut_pos]))) {
            ++mut_pos;
        }
        if (mut_pos >= selector_text.size()) {
            break;
        }
        auto next_delim = selector_text.find("&&", mut_pos);
        auto next_space = selector_text.find(' ', mut_pos);
        std::size_t delim_pos = std::min(next_delim == std::string::npos ? selector_text.size() : next_delim,
                                         next_space == std::string::npos ? selector_text.size() : next_space);
        const auto chunk = trim_spaces(selector_text.substr(mut_pos, delim_pos - mut_pos));
        if (chunk.empty() || chunk.front() != '.') {
            return std::nullopt;
        }
        KvStore::ValueSelector mut_selector;
        std::size_t mut_idx{0};
        while (mut_idx < chunk.size()) {
            if (chunk[mut_idx] != '.') {
                return std::nullopt;
            }
            ++mut_idx;
            std::string mut_name;
            while (mut_idx < chunk.size() &&
                   (std::isalnum(static_cast<unsigned char>(chunk[mut_idx])) || chunk[mut_idx] == '_')) {
                mut_name.push_back(chunk[mut_idx]);
                ++mut_idx;
            }
            if (mut_name.empty()) {
                return std::nullopt;
            }
            KvStore::PathToken mut_token;
            mut_token.name = mut_name;
            if (mut_idx < chunk.size() && chunk[mut_idx] == '[') {
                ++mut_idx;
                const auto close = chunk.find(']', mut_idx);
                if (close == std::string::npos) {
                    return std::nullopt;
                }
                const auto range_text = chunk.substr(mut_idx, close - mut_idx);
                const auto dots = range_text.find("..");
                if (dots == std::string::npos) {
                    std::size_t mut_index{0};
                    try {
                        mut_index = static_cast<std::size_t>(std::stoull(range_text));
                    } catch (...) {
                        return std::nullopt;
                    }
                    mut_token.index = mut_index;
                } else {
                    std::size_t mut_begin{0};
                    std::size_t mut_end{0};
                    try {
                        mut_begin = static_cast<std::size_t>(std::stoull(range_text.substr(0, dots)));
                        mut_end = static_cast<std::size_t>(std::stoull(range_text.substr(dots + 2)));
                    } catch (...) {
                        return std::nullopt;
                    }
                    if (mut_end < mut_begin) {
                        return std::nullopt;
                    }
                    mut_token.range = std::make_pair(mut_begin, mut_end);
                }
                mut_idx = close + 1;
            }
            mut_selector.tokens.push_back(std::move(mut_token));
        }
        mut_selectors.push_back(std::move(mut_selector));
        if (delim_pos >= selector_text.size()) {
            break;
        }
        if (next_delim != std::string::npos && next_delim == delim_pos) {
            mut_pos = delim_pos + 2;
        } else {
            mut_pos = delim_pos + 1;
        }
    }
    return mut_selectors;
}

class ScopedTransaction {
public:
    ScopedTransaction(SqlitePtr db, const bool active) : db_(db), active_(active) {}
    ~ScopedTransaction() {
        if (active_) {
            sqlite3_exec(db_, "ROLLBACK", nullptr, nullptr, nullptr);
        }
    }
    void commit() {
        if (active_) {
            sqlite3_exec(db_, "COMMIT", nullptr, nullptr, nullptr);
            active_ = false;
        }
    }

private:
    SqlitePtr db_{nullptr};
    bool active_{false};
};

}  // namespace chi

namespace {

SqlitePtr as_db(void* raw) {
    return static_cast<SqlitePtr>(raw);
}

SqlitePtr as_db_const(const void* raw) {
    return static_cast<SqlitePtr>(const_cast<void*>(raw));
}

bool exec_sql(SqlitePtr db, const std::string& sql) {
    char* mut_err{nullptr};
    const auto code = sqlite3_exec(db, sql.c_str(), nullptr, nullptr, &mut_err);
    if (mut_err != nullptr) {
        sqlite3_free(mut_err);
    }
    return code == SQLITE_OK;
}

std::string escape_json(std::string_view text) {
    std::string mut_result;
    mut_result.reserve(text.size());
    for (const auto ch : text) {
        switch (ch) {
        case '"':
        case '\\':
            mut_result.push_back('\\');
            mut_result.push_back(ch);
            break;
        case '\b':
            mut_result.append("\\b");
            break;
        case '\f':
            mut_result.append("\\f");
            break;
        case '\n':
            mut_result.append("\\n");
            break;
        case '\r':
            mut_result.append("\\r");
            break;
        case '\t':
            mut_result.append("\\t");
            break;
        default:
            mut_result.push_back(ch);
            break;
        }
    }
    return mut_result;
}

std::filesystem::path resolve_queue_path(const chi::KvStoreConfig& config, const std::string& queue_name) {
    if (config.queue_root.empty()) {
        return std::filesystem::path(queue_name);
    }
    const auto file_name = queue_name + ".queue";
    return config.queue_root / file_name;
}

bool upsert_record(SqlitePtr db, const std::string& table, std::int64_t id, std::int64_t updated, std::string_view key, std::string_view value) {
    const auto sql = "INSERT INTO " + table +
                     " (id, updated, key, value, is_deleted) VALUES (?1, ?2, ?3, ?4, ?5) ON CONFLICT(key) DO UPDATE SET id=excluded.id, "
                     "updated=excluded.updated, value=excluded.value, is_deleted=excluded.is_deleted";
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return false;
    }
    sqlite3_bind_int64(mut_stmt.stmt, 1, id);
    sqlite3_bind_int64(mut_stmt.stmt, 2, updated);
    sqlite3_bind_text(mut_stmt.stmt, 3, key.data(), static_cast<int>(key.size()), SQLITE_TRANSIENT);
    sqlite3_bind_text(mut_stmt.stmt, 4, value.data(), static_cast<int>(value.size()), SQLITE_TRANSIENT);
    sqlite3_bind_int(mut_stmt.stmt, 5, 0);
    const auto code = sqlite3_step(mut_stmt.stmt);
    return code == SQLITE_DONE;
}

std::optional<std::int64_t> get_meta_int(SqlitePtr db, const std::string& key) {
    const std::string sql = "SELECT v FROM kv_meta WHERE k=?1";
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return std::nullopt;
    }
    sqlite3_bind_text(mut_stmt.stmt, 1, key.c_str(), static_cast<int>(key.size()), SQLITE_TRANSIENT);
    const auto step = sqlite3_step(mut_stmt.stmt);
    if (step != SQLITE_ROW) {
        return std::nullopt;
    }
    const auto text_ptr = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 0));
    if (text_ptr == nullptr) {
        return std::nullopt;
    }
    try {
        return std::stoll(text_ptr);
    } catch (...) {
        return std::nullopt;
    }
}

void set_meta_int(SqlitePtr db, const std::string& key, std::int64_t value) {
    const std::string sql = "INSERT INTO kv_meta (k, v) VALUES (?1, ?2) ON CONFLICT(k) DO UPDATE SET v=excluded.v";
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return;
    }
    const auto value_text = std::to_string(value);
    sqlite3_bind_text(mut_stmt.stmt, 1, key.c_str(), static_cast<int>(key.size()), SQLITE_TRANSIENT);
    sqlite3_bind_text(mut_stmt.stmt, 2, value_text.c_str(), static_cast<int>(value_text.size()), SQLITE_TRANSIENT);
    sqlite3_step(mut_stmt.stmt);
}

void delete_meta_key(SqlitePtr db, const std::string& key) {
    const std::string sql = "DELETE FROM kv_meta WHERE k=?1";
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return;
    }
    sqlite3_bind_text(mut_stmt.stmt, 1, key.c_str(), static_cast<int>(key.size()), SQLITE_TRANSIENT);
    sqlite3_step(mut_stmt.stmt);
}

std::int64_t read_max_id(SqlitePtr db, const std::string& table) {
    const std::string sql = "SELECT MAX(id) FROM " + table;
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return 0;
    }
    if (sqlite3_step(mut_stmt.stmt) != SQLITE_ROW) {
        return 0;
    }
    return sqlite3_column_int64(mut_stmt.stmt, 0);
}

std::int64_t next_id(SqlitePtr db, const std::string& table) {
    const std::string sql = "SELECT COALESCE(MAX(id), 0) + 1 FROM " + table;
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return 1;
    }
    if (sqlite3_step(mut_stmt.stmt) != SQLITE_ROW) {
        return 1;
    }
    return sqlite3_column_int64(mut_stmt.stmt, 0);
}

bool column_exists(SqlitePtr db, const std::string& table, const std::string& column) {
    const std::string sql = "PRAGMA table_info(" + table + ")";
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return false;
    }
    while (sqlite3_step(mut_stmt.stmt) == SQLITE_ROW) {
        const auto col_name = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 1));
        if (col_name != nullptr && column == col_name) {
            return true;
        }
    }
    return false;
}

bool ensure_deleted_column(SqlitePtr db, const std::string& table) {
    if (column_exists(db, table, "is_deleted")) {
        return true;
    }
    const std::string sql = "ALTER TABLE " + table + " ADD COLUMN is_deleted INTEGER NOT NULL DEFAULT 0";
    return exec_sql(db, sql);
}

std::vector<std::string> build_path_segments(const std::string& key) {
    std::vector<std::string> paths;
    if (key.empty()) {
        return paths;
    }
    const bool leading_slash = !key.empty() && key.front() == '/';
    std::string mut_current;
    std::size_t mut_pos = 0;
    if (leading_slash) {
        mut_pos = 1;
    }
    while (mut_pos <= key.size()) {
        const auto next = key.find('/', mut_pos);
        const auto slice = key.substr(mut_pos, next == std::string::npos ? std::string::npos : next - mut_pos);
        if (slice.empty() && next == std::string::npos) {
            break;
        }
        if (mut_current.empty()) {
            mut_current = leading_slash ? "/" + slice : slice;
        } else {
            mut_current.append("/");
            mut_current.append(slice);
        }
        paths.push_back(mut_current);
        if (next == std::string::npos) {
            break;
        }
        mut_pos = next + 1;
    }
    return paths;
}

std::string normalize_key(std::string_view key) {
    std::string mut_key{key};
    if (mut_key.empty() || mut_key.front() != '/') {
        mut_key.insert(mut_key.begin(), '/');
    }
    while (mut_key.size() > 1 && mut_key.back() == '/') {
        mut_key.pop_back();
    }
    return mut_key;
}

void extract_selector(std::string& pattern, std::string& selector_out) {
    if (!selector_out.empty()) {
        return;
    }
    const auto open = pattern.find('{');
    const auto close = pattern.rfind('}');
    if (open != std::string::npos && close != std::string::npos && close > open) {
        const auto inside = pattern.substr(open + 1, close - open - 1);
        selector_out = trim_spaces(inside);
        pattern.erase(open, close - open + 1);
    }
}

}  // namespace

namespace chi {

bool KvStore::upsert_replication_config(const std::string& queue_name, const std::string& direction) {
    auto* const db = as_db(db_);
    const std::string sql =
        "INSERT INTO kv_replication_config (queue, direction) VALUES (?1, ?2) ON CONFLICT(queue) DO UPDATE SET direction=excluded.direction";
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return false;
    }
    sqlite3_bind_text(mut_stmt.stmt, 1, queue_name.c_str(), static_cast<int>(queue_name.size()), SQLITE_TRANSIENT);
    sqlite3_bind_text(mut_stmt.stmt, 2, direction.c_str(), static_cast<int>(direction.size()), SQLITE_TRANSIENT);
    return sqlite3_step(mut_stmt.stmt) == SQLITE_DONE;
}

bool KvStore::remove_replication_config(const std::string& queue_name) {
    auto* const db = as_db(db_);
    const std::string sql = "DELETE FROM kv_replication_config WHERE queue=?1";
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return false;
    }
    sqlite3_bind_text(mut_stmt.stmt, 1, queue_name.c_str(), static_cast<int>(queue_name.size()), SQLITE_TRANSIENT);
    return sqlite3_step(mut_stmt.stmt) == SQLITE_DONE;
}

bool KvStore::load_replication_config() {
    replica_outputs_.clear();
    replica_input_.reset();
    mut_replica_writers_.clear();
    mut_net_replica_writers_.clear();
    mut_net_replica_reader_.reset();
    use_import_replica_ = false;

    auto* const db = as_db(db_);
    const std::string sql = "SELECT queue, direction FROM kv_replication_config";
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return false;
    }
    while (sqlite3_step(mut_stmt.stmt) == SQLITE_ROW) {
        const auto queue_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 0));
        const auto direction_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 1));
        if (queue_text == nullptr || direction_text == nullptr) {
            continue;
        }
        const std::string queue{queue_text};
        const std::string direction{direction_text};
        const auto queue_path = resolve_queue_path(config_, queue);
        if (direction == "out") {
            replica_outputs_.push_back(queue);
            auto writer = std::make_unique<ClientWriter>(config_.replica_host, config_.replica_writer_port);
            writer->start();
            mut_net_replica_writers_[queue] = std::move(writer);
        } else if (direction == "in" && !replica_input_.has_value()) {
            replica_input_ = queue;
            auto reader = std::make_unique<ClientReader>(config_.replica_host, config_.replica_reader_port);
            reader->start();
            mut_net_replica_reader_ = std::move(reader);
            use_import_replica_ = true;
            ensure_import_table();
        }
    }
    return true;
}

}  // namespace chi

namespace {

std::optional<std::int64_t> read_json_int(const chi::KvStore::JsonValue& json, const std::string& field) {
    if (!std::holds_alternative<chi::KvStore::JsonValue::object_t>(json.payload)) {
        return std::nullopt;
    }
    const auto& object = std::get<chi::KvStore::JsonValue::object_t>(json.payload);
    const auto it = object.find(field);
    if (it == object.end()) {
        return std::nullopt;
    }
    if (std::holds_alternative<double>(it->second.payload)) {
        return static_cast<std::int64_t>(std::get<double>(it->second.payload));
    }
    return std::nullopt;
}

std::optional<std::string> read_json_string(const chi::KvStore::JsonValue& json, const std::string& field) {
    if (!std::holds_alternative<chi::KvStore::JsonValue::object_t>(json.payload)) {
        return std::nullopt;
    }
    const auto& object = std::get<chi::KvStore::JsonValue::object_t>(json.payload);
    const auto it = object.find(field);
    if (it == object.end()) {
        return std::nullopt;
    }
    if (std::holds_alternative<std::string>(it->second.payload)) {
        return std::get<std::string>(it->second.payload);
    }
    return std::nullopt;
}

}  // namespace

namespace chi {

KvStore::KvStore(KvStoreConfig config) : config_(std::move(config)) {
    SqlitePtr mut_db{nullptr};
    const auto open_code = sqlite3_open(config_.db_path.string().c_str(), &mut_db);
    if (open_code != SQLITE_OK) {
        throw std::runtime_error("Failed to open SQLite database");
    }
    sqlite3_busy_timeout(mut_db, 5000);
    exec_sql(mut_db, "PRAGMA journal_mode=WAL");
    exec_sql(mut_db, "PRAGMA synchronous=NORMAL");
    db_ = mut_db;
    if (!ensure_tables()) {
        throw std::runtime_error("Failed to initialize database schema");
    }
    load_replication_config();
}

KvStore::~KvStore() {
    if (db_ != nullptr) {
        sqlite3_close(as_db(db_));
        db_ = nullptr;
    }
}

bool KvStore::ensure_tables() {
    auto* const db = as_db(db_);
    const auto main_sql = "CREATE TABLE IF NOT EXISTS " + config_.table +
                          " (id INTEGER PRIMARY KEY AUTOINCREMENT, updated INTEGER NOT NULL, key TEXT NOT NULL UNIQUE, value TEXT NOT NULL, is_deleted INTEGER NOT NULL DEFAULT 0)";
    const auto index_sql = "CREATE UNIQUE INDEX IF NOT EXISTS idx_" + config_.table + "_key ON " + config_.table + "(key)";
    const auto meta_sql = "CREATE TABLE IF NOT EXISTS kv_meta (k TEXT PRIMARY KEY, v TEXT NOT NULL)";
    const auto replica_cfg_sql =
        "CREATE TABLE IF NOT EXISTS kv_replication_config (queue TEXT PRIMARY KEY, direction TEXT NOT NULL CHECK(direction IN ('out','in')))";
    const bool created_main = exec_sql(db, main_sql);
    const bool created_index = exec_sql(db, index_sql);
    const bool created_meta = exec_sql(db, meta_sql);
    const bool created_replica_cfg = exec_sql(db, replica_cfg_sql);
    const bool ensured_deleted = ensure_deleted_column(db, config_.table);
    const bool created_paths = ensure_paths_table();
    const bool ensured_paths = created_paths && rebuild_paths();
    return created_main && created_index && created_meta && created_replica_cfg && ensured_deleted && ensured_paths;
}

bool KvStore::ensure_import_table() const {
    auto* const db = as_db_const(db_);
    const auto sql = "CREATE TABLE IF NOT EXISTS " + config_.import_table +
                     " (id INTEGER PRIMARY KEY, updated INTEGER NOT NULL, key TEXT NOT NULL UNIQUE, value TEXT NOT NULL, is_deleted INTEGER NOT NULL DEFAULT 0)";
    const auto index_sql = "CREATE UNIQUE INDEX IF NOT EXISTS idx_" + config_.import_table + "_key ON " + config_.import_table + "(key)";
    const bool created_table = exec_sql(db, sql);
    const bool created_index = exec_sql(db, index_sql);
    const bool ensured_deleted = ensure_deleted_column(db, config_.import_table);
    return created_table && created_index && ensured_deleted;
}

bool KvStore::ensure_paths_table() {
    auto* const db = as_db(db_);
    const auto sql = "CREATE TABLE IF NOT EXISTS kv_paths (id INTEGER PRIMARY KEY AUTOINCREMENT, path TEXT NOT NULL UNIQUE)";
    return exec_sql(db, sql);
}

bool KvStore::rebuild_paths() {
    auto* const db = as_db(db_);
    const std::string count_sql = "SELECT COUNT(*) FROM kv_paths";
    Statement mut_count;
    if (sqlite3_prepare_v2(db, count_sql.c_str(), -1, &mut_count.stmt, nullptr) != SQLITE_OK) {
        return false;
    }
    if (sqlite3_step(mut_count.stmt) == SQLITE_ROW) {
        const auto count = sqlite3_column_int64(mut_count.stmt, 0);
        if (count > 0) {
            return true;
        }
    }
    exec_sql(db, "DELETE FROM kv_paths");
    const std::string select_sql = "SELECT key FROM " + config_.table + " WHERE is_deleted=0";
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, select_sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return false;
    }
    while (sqlite3_step(mut_stmt.stmt) == SQLITE_ROW) {
        const auto key_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 0));
        if (key_text == nullptr) {
            continue;
        }
        add_paths_for_key(key_text);
    }
    return true;
}

void KvStore::add_paths_for_key(const std::string& key) {
    auto* const db = as_db(db_);
    const auto paths = build_path_segments(key);
    if (paths.empty()) {
        return;
    }
    // skip last segment (full key) to avoid overpopulation.
    const auto end = paths.size() - 1;
    for (const auto& path : paths) {
        if (&path - &paths[0] >= static_cast<std::ptrdiff_t>(end)) {
            break;
        }
        const std::string sql = "INSERT OR IGNORE INTO kv_paths (path) VALUES (?1)";
        Statement mut_stmt;
        if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
            continue;
        }
        sqlite3_bind_text(mut_stmt.stmt, 1, path.c_str(), static_cast<int>(path.size()), SQLITE_TRANSIENT);
        sqlite3_step(mut_stmt.stmt);
    }
}

void KvStore::remove_paths_for_key_if_unused(const std::string& key) {
    auto* const db = as_db(db_);
    auto paths = build_path_segments(key);
    std::reverse(paths.begin(), paths.end());
    for (const auto& path : paths) {
        if (path == key) {
            continue;
        }
        const std::string check_sql = "SELECT 1 FROM " + config_.table + " WHERE key LIKE ?1 AND is_deleted=0 LIMIT 1";
        Statement mut_check;
        if (sqlite3_prepare_v2(db, check_sql.c_str(), -1, &mut_check.stmt, nullptr) != SQLITE_OK) {
            continue;
        }
        const auto like = path + "%";
        sqlite3_bind_text(mut_check.stmt, 1, like.c_str(), static_cast<int>(like.size()), SQLITE_TRANSIENT);
        const auto has_row = sqlite3_step(mut_check.stmt) == SQLITE_ROW;
        if (has_row) {
            continue;
        }
        const std::string delete_sql = "DELETE FROM kv_paths WHERE path=?1";
        Statement mut_delete;
        if (sqlite3_prepare_v2(db, delete_sql.c_str(), -1, &mut_delete.stmt, nullptr) != SQLITE_OK) {
            continue;
        }
        sqlite3_bind_text(mut_delete.stmt, 1, path.c_str(), static_cast<int>(path.size()), SQLITE_TRANSIENT);
        sqlite3_step(mut_delete.stmt);
    }
}

bool KvStore::set_value(std::string_view key, std::string_view value) {
    auto* const db = as_db(db_);
    const auto now = now_ms();
    const auto new_id = next_id(db, config_.table);
    const auto norm_key = normalize_key(key);
    const auto sql = "INSERT INTO " + config_.table +
                     " (id, updated, key, value, is_deleted) VALUES (?1, ?2, ?3, ?4, 0) ON CONFLICT(key) DO UPDATE SET id=excluded.id, updated=excluded.updated, value=excluded.value, is_deleted=0";
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return false;
    }
    sqlite3_bind_int64(mut_stmt.stmt, 1, new_id);
    sqlite3_bind_int64(mut_stmt.stmt, 2, now);
    sqlite3_bind_text(mut_stmt.stmt, 3, norm_key.c_str(), static_cast<int>(norm_key.size()), SQLITE_TRANSIENT);
    sqlite3_bind_text(mut_stmt.stmt, 4, value.data(), static_cast<int>(value.size()), SQLITE_TRANSIENT);
    const auto code = sqlite3_step(mut_stmt.stmt);
    if (code == SQLITE_DONE) {
        add_paths_for_key(norm_key);
        return true;
    }
    return false;
}

bool KvStore::mark_deleted(std::string_view key) {
    auto* const db = as_db(db_);
    const auto now = now_ms();
    const auto new_id = next_id(db, config_.table);
    const auto norm_key = normalize_key(key);
    const auto sql = "INSERT INTO " + config_.table +
                     " (id, updated, key, value, is_deleted) VALUES (?1, ?2, ?3, ?4, 1) ON CONFLICT(key) DO UPDATE SET id=excluded.id, updated=excluded.updated, is_deleted=1";
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return false;
    }
    sqlite3_bind_int64(mut_stmt.stmt, 1, new_id);
    sqlite3_bind_int64(mut_stmt.stmt, 2, now);
    sqlite3_bind_text(mut_stmt.stmt, 3, norm_key.c_str(), static_cast<int>(norm_key.size()), SQLITE_TRANSIENT);
    sqlite3_bind_text(mut_stmt.stmt, 4, "", 0, SQLITE_TRANSIENT);
    const auto code = sqlite3_step(mut_stmt.stmt);
    if (code == SQLITE_DONE) {
        remove_paths_for_key_if_unused(norm_key);
        return true;
    }
    return false;
}

std::optional<std::string> KvStore::get_value(std::string_view key) const {
    auto* const db = as_db_const(db_);
    const auto norm_key = normalize_key(key);
    const auto sql = "SELECT value FROM " + config_.table + " WHERE key=?1 AND is_deleted=0 LIMIT 1";
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return std::nullopt;
    }
    sqlite3_bind_text(mut_stmt.stmt, 1, norm_key.c_str(), static_cast<int>(norm_key.size()), SQLITE_TRANSIENT);
    const auto step = sqlite3_step(mut_stmt.stmt);
    if (step != SQLITE_ROW) {
        return std::nullopt;
    }
    const auto text_ptr = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 0));
    if (text_ptr == nullptr) {
        return std::nullopt;
    }
    return std::string{text_ptr};
}

// ScanResult KvStore::get_paths(std::string_view key_pattern, std::int64_t start_on) const {
//     std::string mut_pattern{key_pattern};
//     if (mut_pattern.size() > 1 && mut_pattern.back() == '/') {
//         mut_pattern.pop_back();
//     }
//     std::string mut_selector;
//     const auto regex = build_key_regex(mut_pattern, false, &mut_selector);
//     auto* const db = as_db_const(db_);

//     // prefix until first regex segment to tighten the LIKE query.
//     const auto segments = split_segments(mut_pattern);
//     const bool leading_slash = !mut_pattern.empty() && mut_pattern.front() == '/';
//     const bool has_regex_segment = std::any_of(segments.begin(), segments.end(), [](const std::string& seg) {
//         return seg.size() >= 2 && seg.front() == '{' && seg.back() == '}';
//     });
//     const auto base_depth = static_cast<int>(segments.size() - (leading_slash && !segments.empty() && segments.front().empty() ? 1 : 0));
//     const auto target_depth = has_regex_segment ? base_depth : base_depth + 1;
//     std::string mut_prefix;
//     const auto start_index = (leading_slash && !segments.empty() && segments.front().empty()) ? 1U : 0U;
//     for (std::size_t i = start_index; i < segments.size(); ++i) {
//         const auto& segment = segments.at(i);
//         if (segment.size() >= 2 && segment.front() == '{' && segment.back() == '}') {
//             break;
//         }
//         if (!mut_prefix.empty() || leading_slash) {
//             mut_prefix.push_back('/');
//         }
//         mut_prefix.append(segment);
//     }

//     const std::string sql = "SELECT id, path FROM kv_paths WHERE id > ?1 AND path LIKE ?2 ORDER BY id ASC LIMIT ?3";
//     Statement mut_stmt;
//     ScanResult result;
//     if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
//         result.continue_from = start_on;
//         return result;
//     }
//     sqlite3_bind_int64(mut_stmt.stmt, 1, start_on);
//     const auto like_filter = mut_prefix.empty() ? std::string("%") : mut_prefix + "%";
//     sqlite3_bind_text(mut_stmt.stmt, 2, like_filter.c_str(), static_cast<int>(like_filter.size()), SQLITE_TRANSIENT);
//     sqlite3_bind_int64(mut_stmt.stmt, 3, static_cast<std::int64_t>(config_.scan_window));
//     auto mut_last_id = start_on;
//     std::unordered_set<std::string> mut_seen;
//     while (sqlite3_step(mut_stmt.stmt) == SQLITE_ROW) {
//         mut_last_id = sqlite3_column_int64(mut_stmt.stmt, 0);
//         const auto path_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 1));
//         if (path_text == nullptr) {
//             continue;
//         }
//         const std::string path{path_text};
//         const auto path_segments = split_segments(path);
//         const int path_depth = static_cast<int>(path_segments.size() - (path_segments.size() > 0 && path_segments.front().empty() ? 1 : 0));
//         if (path_depth != target_depth) {
//             continue;
//         }
//         if (std::regex_match(path, regex)) {
//             if (mut_seen.insert(path).second) {
//                 result.values.push_back(path);
//             }
//         }
//     }
//     // Also include direct children from kv_items (one extra segment) and exact matches with values.
//     const auto child_prefix = mut_pattern;
//     const std::string child_like = child_prefix + "/%";
//     const std::string child_like_deeper = child_prefix + "/%/%";
//     const std::string items_sql =
//         "SELECT id, key FROM " + config_.table +
//         " WHERE id > ?1 AND is_deleted=0 AND key LIKE ?2 AND key NOT LIKE ?3 ORDER BY id ASC LIMIT ?4";
//     Statement mut_items;
//     if (sqlite3_prepare_v2(db, items_sql.c_str(), -1, &mut_items.stmt, nullptr) == SQLITE_OK) {
//         sqlite3_bind_int64(mut_items.stmt, 1, start_on);
//         sqlite3_bind_text(mut_items.stmt, 2, child_like.c_str(), static_cast<int>(child_like.size()), SQLITE_TRANSIENT);
//         sqlite3_bind_text(mut_items.stmt, 3, child_like_deeper.c_str(), static_cast<int>(child_like_deeper.size()), SQLITE_TRANSIENT);
//         sqlite3_bind_int64(mut_items.stmt, 4, static_cast<std::int64_t>(config_.scan_window));
//         while (sqlite3_step(mut_items.stmt) == SQLITE_ROW) {
//             const auto item_id = sqlite3_column_int64(mut_items.stmt, 0);
//             mut_last_id = std::max<std::int64_t>(mut_last_id, item_id);
//             const auto key_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_items.stmt, 1));
//             if (key_text == nullptr) {
//                 continue;
//             }
//             const std::string key{key_text};
//             const auto key_segments = split_segments(key);
//             const int key_depth = static_cast<int>(key_segments.size() - (key_segments.size() > 0 && key_segments.front().empty() ? 1 : 0));
//             if (key_depth != target_depth) {
//                 continue;
//             }
//             if (std::regex_match(key, regex)) {
//                 if (mut_seen.insert(key).second) {
//                     result.values.push_back(key);
//                 }
//             }
//         }
//     }
//     result.continue_from = mut_last_id;
//     return result;
// }

// ScanResult KvStore::get_values(std::string_view key_pattern, std::int64_t start_on) const {
//     std::string mut_pattern{key_pattern};
//     if (mut_pattern.size() > 1 && mut_pattern.back() == '/') {
//         mut_pattern.pop_back();
//     }
//     std::string mut_selector;
//     const auto regex = build_key_regex(mut_pattern, true, &mut_selector);
//     std::optional<std::vector<ValueSelector>> mut_selectors;
//     if (!mut_selector.empty()) {
//         mut_selectors = parse_value_selectors(mut_selector);
//     }
//     auto* const db = as_db_const(db_);

//     const std::string sql = "SELECT id, key, value FROM " + config_.table + " WHERE id > ?1 AND is_deleted=0 ORDER BY id ASC LIMIT ?2";
//     Statement mut_stmt;
//     ScanResult result;
//     if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
//         result.continue_from = start_on;
//         return result;
//     }
//     sqlite3_bind_int64(mut_stmt.stmt, 1, start_on);
//     sqlite3_bind_int64(mut_stmt.stmt, 2, static_cast<std::int64_t>(config_.scan_window));
//     auto mut_last_id = start_on;
//     while (sqlite3_step(mut_stmt.stmt) == SQLITE_ROW) {
//         mut_last_id = sqlite3_column_int64(mut_stmt.stmt, 0);
//         const auto key_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 1));
//         const auto value_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 2));
//         if (key_text == nullptr || value_text == nullptr) {
//             continue;
//         }
//         const std::string key{key_text};
//         if (!std::regex_match(key, regex)) {
//             continue;
//         }
//         if (!mut_selectors.has_value() || mut_selectors->empty()) {
//             result.values.emplace_back(value_text);
//             continue;
//         }
//         JsonParser parser(value_text);
//         const auto parsed = parser.parse();
//         if (!parsed.has_value()) {
//             continue;
//         }
//         if (mut_selectors->size() == 1) {
//             const auto selected = select_path(parsed.value(), mut_selectors->front());
//             if (selected.has_value()) {
//                 result.values.push_back(to_json_string(selected.value()));
//             }
//             continue;
//         }
//         KvStore::JsonValue::object_t mut_object;
//         for (const auto& selector : mut_selectors.value()) {
//             const auto selected = select_path(parsed.value(), selector);
//             if (selected.has_value() && !selector.tokens.empty()) {
//                 const auto& name = selector.tokens.front().name;
//                 mut_object[name] = selected.value();
//             }
//         }
//         if (!mut_object.empty()) {
//             KvStore::JsonValue composed;
//             composed.payload = mut_object;
//             result.values.push_back(to_json_string(composed));
//         }
//     }
//     result.continue_from = mut_last_id;
//     return result;
// }

LookupResult KvStore::get(std::string_view key_pattern, std::int64_t start_on) const {
    LookupResult result;
    std::string mut_pattern{key_pattern};
    if (mut_pattern.size() > 1 && mut_pattern.back() == '/') {
        mut_pattern.pop_back();
    }
    if (!mut_pattern.empty() && mut_pattern.front() != '/') {
        mut_pattern.insert(mut_pattern.begin(), '/');
    }
    std::string mut_selector;
    extract_selector(mut_pattern, mut_selector);
    const auto segments = split_segments(mut_pattern);
    const bool leading_slash = !mut_pattern.empty() && mut_pattern.front() == '/';
    const bool has_regex_segment = std::any_of(segments.begin(), segments.end(), [](const std::string& seg) {
        return seg.size() >= 2 && seg.front() == '(' && seg.back() == ')';
    });
    int mut_base_depth = static_cast<int>(segments.size() - (leading_slash && !segments.empty() && segments.front().empty() ? 1 : 0));
    if (mut_pattern == "/") {
        mut_base_depth = 0;
    }
    const auto target_depth = has_regex_segment ? mut_base_depth : mut_base_depth + 1;
    std::regex regex = has_regex_segment
                           ? build_key_regex(mut_pattern, true, &mut_selector)
                           : std::regex{
                                 "^" +
                                 escape_literal_segment(mut_pattern.empty() ? std::string{"/"} : std::string{mut_pattern}) +
                                 (mut_pattern == "/" ? "" : "/") + "[^/]+$"};
    std::optional<std::vector<ValueSelector>> mut_selectors;
    if (!mut_selector.empty()) {
        // selectors separated by space or &&
        std::string normalized_selector;
        normalized_selector.reserve(mut_selector.size());
        bool mut_space = false;
        for (const auto ch : mut_selector) {
            if (ch == '&') {
                mut_space = true;
                continue;
            }
            if (std::isspace(static_cast<unsigned char>(ch))) {
                if (!normalized_selector.empty() && normalized_selector.back() != ' ') {
                    normalized_selector.push_back(' ');
                }
            } else {
                if (mut_space && !normalized_selector.empty() && normalized_selector.back() != ' ') {
                    normalized_selector.push_back(' ');
                }
                mut_space = false;
                normalized_selector.push_back(ch);
            }
        }
        mut_selectors = parse_value_selectors(normalized_selector);
    }

    auto* const db = as_db_const(db_);

    // Paths from kv_paths.
    {
        std::string mut_prefix;
        const auto start_index = (leading_slash && !segments.empty() && segments.front().empty()) ? 1U : 0U;
        for (std::size_t i = start_index; i < segments.size(); ++i) {
            const auto& segment = segments.at(i);
            if (segment.size() >= 2 && segment.front() == '(' && segment.back() == ')') {
                break;
            }
            if (!mut_prefix.empty() || leading_slash) {
                mut_prefix.push_back('/');
            }
            mut_prefix.append(segment);
        }

        const std::string sql = "SELECT id, path FROM kv_paths WHERE id > ?1 AND path LIKE ?2 ORDER BY id ASC LIMIT ?3";
        Statement mut_stmt;
        if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) == SQLITE_OK) {
            sqlite3_bind_int64(mut_stmt.stmt, 1, start_on);
            const auto like_filter = mut_prefix.empty() ? std::string("%") : mut_prefix + "%";
            sqlite3_bind_text(mut_stmt.stmt, 2, like_filter.c_str(), static_cast<int>(like_filter.size()), SQLITE_TRANSIENT);
            sqlite3_bind_int64(mut_stmt.stmt, 3, static_cast<std::int64_t>(config_.scan_window));
            while (sqlite3_step(mut_stmt.stmt) == SQLITE_ROW) {
                const auto path_id = sqlite3_column_int64(mut_stmt.stmt, 0);
                result.continue_from = std::max<std::int64_t>(result.continue_from, path_id);
                const auto path_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 1));
                if (path_text == nullptr) {
                    continue;
                }
                const std::string path{path_text};
                const auto path_segments = split_segments(path);
                const int path_depth = static_cast<int>(path_segments.size() - (path_segments.size() > 0 && path_segments.front().empty() ? 1 : 0));
                if (path_depth != target_depth) {
                    continue;
                }
                if (std::regex_match(path, regex)) {
                    result.paths.push_back(path);
                }
            }
        }
    }

    // Values from kv_items.
    {
        const bool literal_only = !has_regex_segment;
        if (literal_only && (!mut_selectors.has_value() || mut_selectors->empty())) {
            const std::string sql = "SELECT id, value FROM " + config_.table + " WHERE key=?1 AND is_deleted=0 LIMIT 1";
            Statement mut_stmt;
            if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) == SQLITE_OK) {
                sqlite3_bind_text(mut_stmt.stmt, 1, mut_pattern.c_str(), static_cast<int>(mut_pattern.size()), SQLITE_TRANSIENT);
                if (sqlite3_step(mut_stmt.stmt) == SQLITE_ROW) {
                    const auto row_id = sqlite3_column_int64(mut_stmt.stmt, 0);
                    const auto value_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 1));
                    if (value_text != nullptr) {
                        result.values.emplace_back(mut_pattern, std::string{value_text});
                        result.continue_from = std::max<std::int64_t>(result.continue_from, row_id);
                    }
                }
            }
        } else if (literal_only && mut_selectors.has_value()) {
            const std::string sql = "SELECT id, value FROM " + config_.table + " WHERE key=?1 AND is_deleted=0 LIMIT 1";
            Statement mut_stmt;
            if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) == SQLITE_OK) {
                sqlite3_bind_text(mut_stmt.stmt, 1, mut_pattern.c_str(), static_cast<int>(mut_pattern.size()), SQLITE_TRANSIENT);
                if (sqlite3_step(mut_stmt.stmt) == SQLITE_ROW) {
                    const auto row_id = sqlite3_column_int64(mut_stmt.stmt, 0);
                    const auto value_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 1));
                    if (value_text != nullptr) {
                        JsonParser parser(value_text);
                        const auto parsed = parser.parse();
                        if (parsed.has_value()) {
                            if (mut_selectors->size() == 1) {
                                const auto selected = select_path(parsed.value(), mut_selectors->front());
                                if (selected.has_value()) {
                                    result.values.emplace_back(mut_pattern, to_json_string(selected.value()));
                                }
                            } else {
                                KvStore::JsonValue::object_t mut_object;
                                for (const auto& selector : mut_selectors.value()) {
                                    const auto selected = select_path(parsed.value(), selector);
                                    if (selected.has_value() && !selector.tokens.empty()) {
                                        const auto& name = selector.tokens.back().name;
                                        mut_object[name] = selected.value();
                                    }
                                }
                                if (!mut_object.empty()) {
                                    KvStore::JsonValue composed;
                                    composed.payload = mut_object;
                                    result.values.emplace_back(mut_pattern, to_json_string(composed));
                                }
                            }
                            result.continue_from = std::max<std::int64_t>(result.continue_from, row_id);
                        }
                    }
                }
            }
        } else {
            const std::string sql = "SELECT id, key, value FROM " + config_.table + " WHERE id > ?1 AND is_deleted=0 ORDER BY id ASC LIMIT ?2";
            Statement mut_stmt;
            if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) == SQLITE_OK) {
                sqlite3_bind_int64(mut_stmt.stmt, 1, start_on);
                sqlite3_bind_int64(mut_stmt.stmt, 2, static_cast<std::int64_t>(config_.scan_window));
                while (sqlite3_step(mut_stmt.stmt) == SQLITE_ROW) {
                    const auto row_id = sqlite3_column_int64(mut_stmt.stmt, 0);
                    result.continue_from = std::max<std::int64_t>(result.continue_from, row_id);
                    const auto key_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 1));
                    const auto value_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 2));
                    if (key_text == nullptr || value_text == nullptr) {
                        continue;
                    }
                    const std::string key{key_text};
                    const auto key_segments = split_segments(key);
                    const int key_depth = static_cast<int>(key_segments.size() - (key_segments.size() > 0 && key_segments.front().empty() ? 1 : 0));
                    if (key_depth != target_depth) {
                        continue;
                    }
                    if (!std::regex_match(key, regex)) {
                        continue;
                    }
                    if (!mut_selectors.has_value() || mut_selectors->empty()) {
                        result.values.emplace_back(key, std::string{value_text});
                        continue;
                    }
                    JsonParser parser(value_text);
                    const auto parsed = parser.parse();
                    if (!parsed.has_value()) {
                        continue;
                    }
                    if (mut_selectors->size() == 1) {
                        const auto selected = select_path(parsed.value(), mut_selectors->front());
                        if (selected.has_value()) {
                            result.values.emplace_back(key, to_json_string(selected.value()));
                        }
                        continue;
                    }
                    KvStore::JsonValue::object_t mut_object;
                    for (const auto& selector : mut_selectors.value()) {
                        const auto selected = select_path(parsed.value(), selector);
                        if (selected.has_value() && !selector.tokens.empty()) {
                            const auto& name = selector.tokens.back().name;
                            mut_object[name] = selected.value();
                        }
                    }
                    if (!mut_object.empty()) {
                        KvStore::JsonValue composed;
                        composed.payload = mut_object;
                        result.values.emplace_back(key, to_json_string(composed));
                    }
                }
            }
        }
    }

    return result;
}


PruneResult KvStore::prune_deleted(std::int64_t start_on) {
    PruneResult result;
    result.continue_from = start_on;
    auto* const db = as_db(db_);
    const auto select_sql = "SELECT id, key FROM " + config_.table + " WHERE id > ?1 AND is_deleted=1 ORDER BY id ASC LIMIT ?2";
    Statement mut_select;
    if (sqlite3_prepare_v2(db, select_sql.c_str(), -1, &mut_select.stmt, nullptr) != SQLITE_OK) {
        return result;
    }
    sqlite3_bind_int64(mut_select.stmt, 1, start_on);
    sqlite3_bind_int64(mut_select.stmt, 2, static_cast<std::int64_t>(config_.scan_window));
    while (sqlite3_step(mut_select.stmt) == SQLITE_ROW) {
        const auto id = sqlite3_column_int64(mut_select.stmt, 0);
        const auto key_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_select.stmt, 1));
        result.continue_from = id;
        if (key_text == nullptr) {
            continue;
        }
        const auto delete_sql = "DELETE FROM " + config_.table + " WHERE id=?1";
        Statement mut_delete;
        if (sqlite3_prepare_v2(db, delete_sql.c_str(), -1, &mut_delete.stmt, nullptr) != SQLITE_OK) {
            continue;
        }
        sqlite3_bind_int64(mut_delete.stmt, 1, id);
        if (sqlite3_step(mut_delete.stmt) == SQLITE_DONE) {
            ++result.pruned;
            remove_paths_for_key_if_unused(std::string{key_text});
        }
    }
    return result;
}

void KvStore::replicate_to(const std::string& queue_name) {
    delete_meta_key(as_db(db_), "last_send_" + queue_name);
    auto writer = std::make_unique<ClientWriter>(config_.replica_host, config_.replica_writer_port);
    writer->start();
    mut_net_replica_writers_[queue_name] = std::move(writer);
    upsert_replication_config(queue_name, "out");
    if (std::find(replica_outputs_.begin(), replica_outputs_.end(), queue_name) == replica_outputs_.end()) {
        replica_outputs_.push_back(queue_name);
    }
}

bool KvStore::send2replica(const std::string& queue_name, std::size_t limit) {
    const auto it = mut_net_replica_writers_.find(queue_name);
    if (it == mut_net_replica_writers_.end()) {
        return false;
    }
    auto* const db = as_db(db_);
    const auto max_rows = limit == 0 ? config_.scan_window : limit;
    const auto meta_key = "last_send_" + queue_name;
    const auto last_sent = get_meta_int(db, meta_key).value_or(0);
    const auto sql = "SELECT id, updated, key, value, is_deleted FROM " + config_.table + " WHERE id > ?1 ORDER BY id ASC LIMIT ?2";
    Statement mut_stmt;
    if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
        return false;
    }
    sqlite3_bind_int64(mut_stmt.stmt, 1, last_sent);
    sqlite3_bind_int64(mut_stmt.stmt, 2, static_cast<std::int64_t>(max_rows));
    auto mut_last_id = last_sent;
    bool mut_sent{false};
    while (sqlite3_step(mut_stmt.stmt) == SQLITE_ROW) {
        mut_last_id = sqlite3_column_int64(mut_stmt.stmt, 0);
        const auto updated = sqlite3_column_int64(mut_stmt.stmt, 1);
        const auto key_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 2));
        const auto value_text = reinterpret_cast<const char*>(sqlite3_column_text(mut_stmt.stmt, 3));
        const auto deleted_flag = sqlite3_column_int(mut_stmt.stmt, 4);
        if (key_text == nullptr || value_text == nullptr) {
            continue;
        }
        const auto record_json = build_record_json(mut_last_id, updated, key_text, value_text, deleted_flag != 0);
        it->second->enqueue(queue_name, record_json);
        mut_sent = true;
    }
    if (mut_sent) {
        set_meta_int(db, meta_key, mut_last_id);
    }
    return mut_sent;
}

bool KvStore::delete_replica_to(const std::string& queue_name) {
    mut_replica_writers_.erase(queue_name);
    auto it_net = mut_net_replica_writers_.find(queue_name);
    if (it_net != mut_net_replica_writers_.end()) {
        it_net->second->stop();
        mut_net_replica_writers_.erase(it_net);
    }
    replica_outputs_.erase(std::remove(replica_outputs_.begin(), replica_outputs_.end(), queue_name), replica_outputs_.end());
    remove_replication_config(queue_name);
    delete_meta_key(as_db(db_), "last_send_" + queue_name);
    return true;
}

void KvStore::replicate_from(const std::string& queue_name) {
    exec_sql(as_db(db_), "DROP TABLE IF EXISTS " + config_.import_table);
    ensure_import_table();
    if (replica_input_.has_value() && replica_input_.value() != queue_name) {
        remove_replication_config(replica_input_.value());
    }
    auto reader = std::make_unique<ClientReader>(config_.replica_host, config_.replica_reader_port);
    reader->start();
    mut_net_replica_reader_ = std::move(reader);
    use_import_replica_ = true;
    replica_input_ = queue_name;
    ensure_import_table();
    upsert_replication_config(queue_name, "in");
}

bool KvStore::get_from_replica(std::size_t limit) {
    if (!mut_net_replica_reader_) {
        return false;
    }
    const auto window = limit == 0 ? config_.scan_window : limit;
    auto future = mut_net_replica_reader_->request(replica_input_.value_or(""), window);
    const auto batch = future.get();
    return apply_replica_messages(batch.messages, true);
}

bool KvStore::replica2master() {
    auto* const db = as_db(db_);
    if (!ensure_import_table()) {
        return false;
    }
    const auto begin_ok = exec_sql(db, "BEGIN IMMEDIATE");
    ScopedTransaction mut_transaction(db, begin_ok);
    const auto clear_main = exec_sql(db, "DELETE FROM " + config_.table);
    const auto copy_sql = "INSERT INTO " + config_.table + " SELECT * FROM " + config_.import_table;
    const auto copied = exec_sql(db, copy_sql);
    const auto drop_import = exec_sql(db, "DROP TABLE IF EXISTS " + config_.import_table);
    if (begin_ok && clear_main && copied && drop_import) {
        mut_transaction.commit();
        use_import_replica_ = false;
        if (replica_input_.has_value()) {
            set_meta_int(db, "last_import_empty_" + replica_input_.value(), 0);
        }
        exec_sql(db, "DELETE FROM kv_paths");
        rebuild_paths();
        return true;
    }
    return false;
}

std::string KvStore::build_record_json(std::int64_t id, std::int64_t updated, std::string_view key, std::string_view value) const {
    std::string mut_json{"{"};
    mut_json.append("\"id\":");
    mut_json.append(std::to_string(id));
    mut_json.append(",\"updated\":");
    mut_json.append(std::to_string(updated));
    mut_json.append(",\"key\":\"");
    mut_json.append(escape_json(key));
    mut_json.append("\",\"value\":\"");
    mut_json.append(escape_json(value));
    mut_json.append("\",\"is_deleted\":0}");
    return mut_json;
}

std::string KvStore::build_record_json(std::int64_t id, std::int64_t updated, std::string_view key, std::string_view value, bool is_deleted) const {
    std::string mut_json{"{"};
    mut_json.append("\"id\":");
    mut_json.append(std::to_string(id));
    mut_json.append(",\"updated\":");
    mut_json.append(std::to_string(updated));
    mut_json.append(",\"key\":\"");
    mut_json.append(escape_json(key));
    mut_json.append("\",\"value\":\"");
    mut_json.append(escape_json(value));
    mut_json.append("\",\"is_deleted\":");
    mut_json.append(is_deleted ? "1" : "0");
    mut_json.append("}");
    return mut_json;
}

std::vector<OutReplicaStats> KvStore::replica_outputs_stats() const {
    std::vector<OutReplicaStats> mut_stats;
    auto* const db = as_db_const(db_);
    const auto max_id = read_max_id(db, config_.table);
    for (const auto& queue : replica_outputs_) {
        OutReplicaStats stat;
        stat.queue = queue;
        stat.last_sent = get_meta_int(db, "last_send_" + queue).value_or(0);
        stat.max_id = max_id;
        mut_stats.push_back(stat);
    }
    return mut_stats;
}

std::optional<InReplicaStats> KvStore::replica_input_stats() const {
    if (!replica_input_.has_value()) {
        return std::nullopt;
    }
    auto* const db = as_db_const(db_);
    InReplicaStats stat;
    stat.queue = replica_input_.value();
    stat.last_imported = get_meta_int(db, "last_import_" + stat.queue).value_or(0);
    const auto empty_flag = get_meta_int(db, "last_import_empty_" + stat.queue).value_or(0);
    stat.last_batch_empty = empty_flag != 0;
    return stat;
}

bool KvStore::apply_replica_messages(const std::vector<std::string>& messages, bool update_meta) {
    auto* const db = as_db(db_);
    bool mut_inserted{false};
    std::int64_t mut_last_imported{0};
    for (const auto& message : messages) {
        JsonParser parser(message);
        const auto parsed = parser.parse();
        if (!parsed.has_value()) {
            continue;
        }
        const auto id = read_json_int(parsed.value(), "id");
        const auto updated = read_json_int(parsed.value(), "updated");
        const auto key_raw = read_json_string(parsed.value(), "key");
        const auto value = read_json_string(parsed.value(), "value");
        const auto is_deleted = read_json_int(parsed.value(), "is_deleted").value_or(0) != 0;
        if (!id.has_value() || !updated.has_value() || !key_raw.has_value() || !value.has_value()) {
            continue;
        }
        const auto key = normalize_key(key_raw.value());
        mut_last_imported = id.value();
        const auto target_table = use_import_replica_ ? (ensure_import_table() ? config_.import_table : config_.table)
                                                     : config_.table;
        const auto sql = "INSERT INTO " + target_table +
                         " (id, updated, key, value, is_deleted) VALUES (?1, ?2, ?3, ?4, ?5) ON CONFLICT(key) DO UPDATE SET id=excluded.id, updated=excluded.updated, value=excluded.value, is_deleted=excluded.is_deleted";
        Statement mut_stmt;
        if (sqlite3_prepare_v2(db, sql.c_str(), -1, &mut_stmt.stmt, nullptr) != SQLITE_OK) {
            continue;
        }
        sqlite3_bind_int64(mut_stmt.stmt, 1, id.value());
        sqlite3_bind_int64(mut_stmt.stmt, 2, updated.value());
        sqlite3_bind_text(mut_stmt.stmt, 3, key.c_str(), static_cast<int>(key.size()), SQLITE_TRANSIENT);
        sqlite3_bind_text(mut_stmt.stmt, 4, value->c_str(), static_cast<int>(value->size()), SQLITE_TRANSIENT);
        sqlite3_bind_int(mut_stmt.stmt, 5, is_deleted ? 1 : 0);
        const auto applied = sqlite3_step(mut_stmt.stmt) == SQLITE_DONE;
        mut_inserted = applied || mut_inserted;
        if (applied && !use_import_replica_) {
            if (is_deleted) {
                remove_paths_for_key_if_unused(key);
            } else {
                add_paths_for_key(key);
            }
        }
    }
    if (update_meta && replica_input_.has_value()) {
        if (mut_last_imported > 0) {
            set_meta_int(db, "last_import_" + replica_input_.value(), mut_last_imported);
        }
        set_meta_int(db, "last_import_empty_" + replica_input_.value(), messages.empty() ? 1 : 0);
    }
    return mut_inserted;
}

}  // namespace chi
