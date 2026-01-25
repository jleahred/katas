#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <thread>

#include <httplib.h>

namespace kvr {

class Kvr;

class RestServer {
public:
    RestServer(Kvr& store, const std::string& host, uint16_t port);
    ~RestServer();

    RestServer(const RestServer&) = delete;
    RestServer& operator=(const RestServer&) = delete;

    bool start();
    void stop();
    const std::string& last_error() const;

private:
    void setup_routes();
    bool parse_json_field(const std::string& body, const std::string& field, std::string& result) const;
    std::string url_decode(const std::string& value) const;
    std::string json_escape(const std::string& value) const;

    Kvr& mut_store_;
    std::string mut_host_;
    uint16_t mut_port_;
    httplib::Server mut_server_;
    std::thread mut_thread_;
    bool mut_running_;
    std::string mut_last_error_;
};

}  // namespace kvr
