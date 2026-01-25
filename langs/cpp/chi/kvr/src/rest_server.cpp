#include "rest_server.h"

#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <sstream>

#include "kvr.h"

namespace kvr {

RestServer::RestServer(Kvr& store, const std::string& host, uint16_t port)
    : mut_store_(store),
      mut_host_(host),
      mut_port_(port),
      mut_running_(false) {
    setup_routes();
}

RestServer::~RestServer() {
    stop();
}

bool RestServer::start() {
    if (mut_running_) {
        return true;
    }

    if (!mut_server_.bind_to_port(mut_host_.c_str(), mut_port_)) {
        mut_last_error_ = "Failed to bind REST server.";
        return false;
    }

    mut_running_ = true;
    mut_thread_ = std::thread([this]() {
        mut_server_.listen_after_bind();
    });
    return true;
}

void RestServer::stop() {
    if (!mut_running_) {
        return;
    }

    mut_running_ = false;
    mut_server_.stop();

    if (mut_thread_.joinable()) {
        mut_thread_.join();
    }
}

const std::string& RestServer::last_error() const {
    return mut_last_error_;
}

void RestServer::setup_routes() {
    mut_server_.Get("/config", [this](const httplib::Request&, httplib::Response& response) {
        const auto buckets = mut_store_.list_buckets();
        const auto poll = mut_store_.list_poll_configs();
        std::ostringstream json;
        json << "{";
        json << "\"buckets\":[";
        for (size_t index = 0; index < buckets.size(); ++index) {
            if (index > 0) {
                json << ",";
            }
            json << "\"" << json_escape(buckets[index]) << "\"";
        }
        json << "],";
        json << "\"poll\":[";
        for (size_t index = 0; index < poll.size(); ++index) {
            if (index > 0) {
                json << ",";
            }
            const auto& config = poll[index];
            json << "{";
            json << "\"side\":\"" << (config.tcp_side == TcpSide::server ? "server" : "client") << "\",";
            json << "\"host\":\"" << json_escape(config.host) << "\",";
            json << "\"port\":" << config.port << ",";
            json << "\"poll_server_buckets\":[";
            for (size_t bucket_index = 0; bucket_index < config.poll_server_buckets.size(); ++bucket_index) {
                if (bucket_index > 0) {
                    json << ",";
                }
                const auto& bucket = config.poll_server_buckets[bucket_index];
                json << "{";
                json << "\"name\":\"" << json_escape(bucket.name) << "\",";
                json << "\"poll_max_reply_count\":" << bucket.poll_max_reply_count;
                json << "}";
            }
            json << "],";
            json << "\"poll_client_buckets\":[";
            for (size_t bucket_index = 0; bucket_index < config.poll_client_buckets.size(); ++bucket_index) {
                if (bucket_index > 0) {
                    json << ",";
                }
                const auto& bucket = config.poll_client_buckets[bucket_index];
                json << "{";
                json << "\"name\":\"" << json_escape(bucket.name) << "\",";
                json << "\"poll_interval_ms\":" << bucket.poll_interval.count();
                json << "}";
            }
            json << "]";
            json << "}";
        }
        json << "]";
        json << "}";
        response.set_content(json.str(), "application/json");
    });

    mut_server_.Get("/status", [this](const httplib::Request&, httplib::Response& response) {
        const auto poll = mut_store_.list_poll_configs();
        std::ostringstream json;
        json << "{";
        json << "\"status\":[";
        bool first_item = true;
        for (const auto& config : poll) {
            for (const auto& bucket : config.poll_server_buckets) {
                if (!first_item) {
                    json << ",";
                }
                first_item = false;
                json << "{";
                json << "\"role\":\"poll_server\",";
                json << "\"bucket\":\"" << json_escape(bucket.name) << "\",";
                json << "\"host\":\"" << json_escape(config.host) << "\",";
                json << "\"port\":" << config.port << ",";
                json << "\"poll_max_reply_count\":" << bucket.poll_max_reply_count << ",";
                json << "\"last_id\":" << mut_store_.last_id_for_bucket(bucket.name);
                json << "}";
            }
            for (const auto& bucket : config.poll_client_buckets) {
                if (!first_item) {
                    json << ",";
                }
                first_item = false;
                json << "{";
                json << "\"role\":\"poll_client\",";
                json << "\"bucket\":\"" << json_escape(bucket.name) << "\",";
                json << "\"host\":\"" << json_escape(config.host) << "\",";
                json << "\"port\":" << config.port << ",";
                json << "\"poll_interval_ms\":" << bucket.poll_interval.count() << ",";
                json << "\"last_id\":" << mut_store_.last_id_for_bucket(bucket.name);
                json << "}";
            }
        }
        json << "]";
        json << "}";
        response.set_content(json.str(), "application/json");
    });

    mut_server_.Post(R"(/buckets/([A-Za-z0-9_]+)/set)",
        [this](const httplib::Request& request, httplib::Response& response) {
            const auto bucket_name = request.matches[1];
            std::string key;
            std::string value;
            if (!parse_json_field(request.body, "key", key)) {
                response.status = 400;
                response.set_content("{\"error\":\"Missing key\"}", "application/json");
                return;
            }
            if (!parse_json_field(request.body, "value", value)) {
                response.status = 400;
                response.set_content("{\"error\":\"Missing value\"}", "application/json");
                return;
            }
            if (!mut_store_.set(bucket_name, key, value)) {
                response.status = 500;
                response.set_content("{\"error\":\"Set failed\"}", "application/json");
                return;
            }
            response.set_content("{\"ok\":true}", "application/json");
        });

    mut_server_.Post(R"(/buckets/([A-Za-z0-9_]+)/del)",
        [this](const httplib::Request& request, httplib::Response& response) {
            const auto bucket_name = request.matches[1];
            std::string key;
            std::string value;
            if (!parse_json_field(request.body, "key", key)) {
                response.status = 400;
                response.set_content("{\"error\":\"Missing key\"}", "application/json");
                return;
            }
            if (!parse_json_field(request.body, "value", value)) {
                response.status = 400;
                response.set_content("{\"error\":\"Missing value\"}", "application/json");
                return;
            }
            if (!mut_store_.del(bucket_name, key, value)) {
                response.status = 500;
                response.set_content("{\"error\":\"Del failed\"}", "application/json");
                return;
            }
            response.set_content("{\"ok\":true}", "application/json");
        });

    mut_server_.Get(R"(/buckets/([A-Za-z0-9_]+)/get)",
        [this](const httplib::Request& request, httplib::Response& response) {
            const auto bucket_name = request.matches[1];
            if (!request.has_param("key")) {
                response.status = 400;
                response.set_content("{\"error\":\"Missing key\"}", "application/json");
                return;
            }
            const auto key = url_decode(request.get_param_value("key"));
            const auto value = mut_store_.get(bucket_name, key);
            if (!value.has_value()) {
                response.status = 404;
                response.set_content("{\"error\":\"Not found\"}", "application/json");
                return;
            }
            response.set_content("{\"value\":\"" + json_escape(value.value()) + "\"}", "application/json");
        });

    mut_server_.Get(R"(/buckets/([A-Za-z0-9_]+)/keys)",
        [this](const httplib::Request& request, httplib::Response& response) {
            const auto bucket_name = request.matches[1];
            if (!request.has_param("key")) {
                response.status = 400;
                response.set_content("{\"error\":\"Missing key\"}", "application/json");
                return;
            }
            const auto key_prefix = url_decode(request.get_param_value("key"));
            std::string start_key;
            int64_t skip = 0;
            int64_t limit = 50;
            int64_t next_id = 0;
            if (request.has_param("start_key")) {
                start_key = url_decode(request.get_param_value("start_key"));
            }
            if (request.has_param("next_id")) {
                next_id = std::strtoll(request.get_param_value("next_id").c_str(), nullptr, 10);
            }
            if (request.has_param("skip")) {
                skip = std::strtoll(request.get_param_value("skip").c_str(), nullptr, 10);
            }
            if (request.has_param("limit")) {
                limit = std::strtoll(request.get_param_value("limit").c_str(), nullptr, 10);
            }
            if (limit <= 0) {
                response.status = 400;
                response.set_content("{\"error\":\"Invalid limit\"}", "application/json");
                return;
            }
            if (limit > 100) {
                response.status = 400;
                response.set_content("{\"error\":\"Limit too large\"}", "application/json");
                return;
            }
            if (skip < 0) {
                skip = 0;
            }
            if (next_id < 0) {
                next_id = 0;
            }
            const auto result = mut_store_.list_active_keys(bucket_name, key_prefix, start_key,
                next_id, skip, limit);
            std::ostringstream json;
            json << "{";
            json << "\"keys\":[";
            for (size_t index = 0; index < result.keys.size(); ++index) {
                if (index > 0) {
                    json << ",";
                }
                json << "\"" << json_escape(result.keys[index]) << "\"";
            }
            json << "],";
            json << "\"next_id\":" << result.next_id << ",";
            json << "\"next_key\":\"" << json_escape(result.next_key) << "\"";
            json << "}";
            response.set_content(json.str(), "application/json");
        });
}

bool RestServer::parse_json_field(const std::string& body, const std::string& field, std::string& result) const {
    const auto quoted_field = "\"" + field + "\"";
    const auto field_pos = body.find(quoted_field);
    if (field_pos == std::string::npos) {
        return false;
    }

    auto colon_pos = body.find(':', field_pos + quoted_field.size());
    if (colon_pos == std::string::npos) {
        return false;
    }
    ++colon_pos;
    while (colon_pos < body.size() && std::isspace(static_cast<unsigned char>(body[colon_pos]))) {
        ++colon_pos;
    }

    if (colon_pos >= body.size() || body[colon_pos] != '"') {
        return false;
    }
    ++colon_pos;

    std::string value;
    while (colon_pos < body.size()) {
        const auto character = body[colon_pos];
        if (character == '"') {
            result = value;
            return true;
        }
        if (character == '\\') {
            if (colon_pos + 1 >= body.size()) {
                return false;
            }
            const auto escaped = body[colon_pos + 1];
            if (escaped == '"' || escaped == '\\' || escaped == '/') {
                value.push_back(escaped);
            } else if (escaped == 'b') {
                value.push_back('\b');
            } else if (escaped == 'f') {
                value.push_back('\f');
            } else if (escaped == 'n') {
                value.push_back('\n');
            } else if (escaped == 'r') {
                value.push_back('\r');
            } else if (escaped == 't') {
                value.push_back('\t');
            } else {
                return false;
            }
            colon_pos += 2;
            continue;
        }
        value.push_back(character);
        ++colon_pos;
    }

    return false;
}

std::string RestServer::url_decode(const std::string& value) const {
    std::string result;
    result.reserve(value.size());
    for (size_t index = 0; index < value.size(); ++index) {
        const auto character = value[index];
        if (character == '%' && index + 2 < value.size()) {
            const auto hex = value.substr(index + 1, 2);
            const auto decoded = static_cast<char>(std::strtol(hex.c_str(), nullptr, 16));
            result.push_back(decoded);
            index += 2;
        } else if (character == '+') {
            result.push_back(' ');
        } else {
            result.push_back(character);
        }
    }
    return result;
}

std::string RestServer::json_escape(const std::string& value) const {
    std::string result;
    result.reserve(value.size());
    for (const auto character : value) {
        switch (character) {
        case '"':
            result += "\\\"";
            break;
        case '\\':
            result += "\\\\";
            break;
        case '\b':
            result += "\\b";
            break;
        case '\f':
            result += "\\f";
            break;
        case '\n':
            result += "\\n";
            break;
        case '\r':
            result += "\\r";
            break;
        case '\t':
            result += "\\t";
            break;
        default:
            result.push_back(character);
            break;
        }
    }
    return result;
}

}  // namespace kvr
