#include "ipcnet.h"

#define ASIO_STANDALONE

#include <algorithm>
#include <chrono>
#include <cstdint>
#include <optional>
#include <stop_token>
#include <thread>
#include <utility>

#include <asio.hpp>

#include "ipc.h"

namespace {

using namespace chi::ipcnet;

using asio::ip::tcp;

enum class MessageType : std::uint8_t {
    kWrite = 1,
    kReadRequest = 2,
    kReadResponse = 3,
    kError = 4
};
constexpr std::size_t kMaxQueueName = 4096;
constexpr std::size_t kMaxPayload = 64 * 1024 * 1024;  // Align with on-disk guardrail.

bool write_all(tcp::socket& socket,
               const void* data,
               const std::size_t size,
               std::stop_token stop_token) {
    const auto* mut_ptr = static_cast<const std::byte*>(data);
    std::size_t mut_sent = 0;
    while (mut_sent < size) {
        asio::error_code mut_ec;
        const auto mut_written = socket.write_some(
            asio::buffer(mut_ptr + mut_sent, size - mut_sent), mut_ec);
        if (mut_ec == asio::error::would_block || mut_ec == asio::error::try_again) {
            if (stop_token.stop_requested()) {
                return false;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(5));
            continue;
        }
        if (mut_ec) {
            return false;
        }
        mut_sent += mut_written;
    }
    return true;
}

bool read_all(tcp::socket& socket, void* data, const std::size_t size, std::stop_token stop_token) {
    auto* mut_ptr = static_cast<std::byte*>(data);
    std::size_t mut_read = 0;
    while (mut_read < size) {
        asio::error_code mut_ec;
        const auto mut_got = socket.read_some(
            asio::buffer(mut_ptr + mut_read, size - mut_read), mut_ec);
        if (mut_ec == asio::error::would_block || mut_ec == asio::error::try_again) {
            if (stop_token.stop_requested()) {
                return false;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(5));
            continue;
        }
        if (mut_ec) {
            return false;
        }
        mut_read += mut_got;
    }
    return true;
}

bool write_u8(tcp::socket& socket, const std::uint8_t value, std::stop_token stop_token) {
    return write_all(socket, &value, sizeof(value), stop_token);
}

bool write_u32(tcp::socket& socket, const std::uint32_t value, std::stop_token stop_token) {
    const std::uint8_t mut_buf[4] = {
        static_cast<std::uint8_t>(value & 0xFF),
        static_cast<std::uint8_t>((value >> 8) & 0xFF),
        static_cast<std::uint8_t>((value >> 16) & 0xFF),
        static_cast<std::uint8_t>((value >> 24) & 0xFF),
    };
    return write_all(socket, mut_buf, sizeof(mut_buf), stop_token);
}

bool read_u32(tcp::socket& socket, std::uint32_t& mut_value, std::stop_token stop_token) {
    std::uint8_t mut_buf[4];
    if (!read_all(socket, mut_buf, sizeof(mut_buf), stop_token)) {
        return false;
    }
    mut_value = static_cast<std::uint32_t>(mut_buf[0]) |
                (static_cast<std::uint32_t>(mut_buf[1]) << 8) |
                (static_cast<std::uint32_t>(mut_buf[2]) << 16) |
                (static_cast<std::uint32_t>(mut_buf[3]) << 24);
    return true;
}

bool write_buffer_chunked(tcp::socket& socket,
                          std::span<const std::byte> payload,
                          const NetThrottleConfig& throttle,
                          std::stop_token stop_token) {
    const std::size_t chunk_size = std::max<std::size_t>(
        1, throttle.max_chunk_size == 0 ? payload.size() : throttle.max_chunk_size);
    std::size_t mut_sent = 0;
    while (mut_sent < payload.size()) {
        const auto chunk_len = std::min(chunk_size, payload.size() - mut_sent);
        const auto* chunk_ptr = reinterpret_cast<const std::byte*>(payload.data() + mut_sent);
        if (!write_all(socket, chunk_ptr, chunk_len, stop_token)) {
            return false;
        }
        mut_sent += chunk_len;
        if (mut_sent < payload.size() && throttle.inter_chunk_delay.count() > 0) {
            std::this_thread::sleep_for(throttle.inter_chunk_delay);
        }
    }
    return true;
}

std::vector<std::byte> read_buffer(tcp::socket& socket,
                                   const std::size_t size,
                                   std::stop_token stop_token) {
    std::vector<std::byte> mut_buf(size);
    if (!mut_buf.empty() && !read_all(socket, mut_buf.data(), mut_buf.size(), stop_token)) {
        return {};
    }
    return mut_buf;
}

void set_non_blocking(tcp::socket& socket, const bool value) {
    asio::error_code mut_ec;
    socket.non_blocking(value, mut_ec);
}

void set_non_blocking(tcp::acceptor& acceptor, const bool value) {
    asio::error_code mut_ec;
    acceptor.non_blocking(value, mut_ec);
}

void try_invoke(const std::function<void()>& fn) {
    if (fn) {
        fn();
    }
}

void try_invoke_stats(const std::function<void(const NetStats&)>& fn, const NetStats& stats) {
    if (fn) {
        fn(stats);
    }
}

void try_invoke_client(const std::function<void(const std::string&)>& fn,
                       const std::string& queue_name) {
    if (fn) {
        fn(queue_name);
    }
}

void try_invoke_stats_client(const std::function<void(const std::string&, const NetStats&)>& fn,
                             const std::string& queue_name,
                             const NetStats& stats) {
    if (fn) {
        fn(queue_name, stats);
    }
}

}  // namespace

namespace chi::ipcnet {


ClientWriter::ClientWriter(std::string host,
                           std::uint16_t port,
                           NetThrottleConfig throttle,
                           ClientCallbacks callbacks)
    : host_(std::move(host)),
      port_(port),
      throttle_(throttle),
      callbacks_(std::move(callbacks)) {}

ClientWriter::~ClientWriter() {
    stop();
}

void ClientWriter::start() {
    if (mut_running_.exchange(true)) {
        return;
    }
    mut_worker_ = std::jthread([this](std::stop_token stop_token) { run(stop_token); });
}

void ClientWriter::stop() {
    if (!mut_running_.exchange(false)) {
        return;
    }
    if (mut_worker_.joinable()) {
        mut_worker_.request_stop();
        {
            std::lock_guard<std::mutex> lock(mut_mutex_);
            mut_cv_.notify_all();
        }
        mut_worker_.join();
    }
}

void ClientWriter::enqueue(const std::string& queue_name, std::span<const std::byte> payload) {
    if (!mut_running_) {
        return;
    }
    PendingWrite mut_item{};
    mut_item.mut_queue_name = queue_name;
    mut_item.mut_payload.assign(payload.begin(), payload.end());
    {
        std::lock_guard<std::mutex> lock(mut_mutex_);
        mut_queue_.push_back(std::move(mut_item));
    }
    mut_cv_.notify_all();
}

void ClientWriter::enqueue(const std::string& queue_name, std::string_view payload) {
    const auto* data_ptr = reinterpret_cast<const std::byte*>(payload.data());
    enqueue(queue_name, std::span<const std::byte>(data_ptr, payload.size()));
}

NetStats ClientWriter::stats() const {
    std::lock_guard<std::mutex> lock(mut_mutex_);
    return mut_stats_;
}

void ClientWriter::run(std::stop_token stop_token) {
    asio::io_context mut_io;
    tcp::resolver mut_resolver(mut_io);
    std::optional<tcp::socket> mut_socket;

    auto mut_mark_disconnect = [this]() {
        const bool was_connected = mut_connected_.exchange(false);
        if (was_connected) {
            try_invoke(callbacks_.on_th_disconnected);
        }
    };

    while (!stop_token.stop_requested()) {
        if (!mut_socket || !mut_socket->is_open()) {
            asio::error_code mut_ec;
            const auto mut_endpoints =
                mut_resolver.resolve(host_, std::to_string(port_), mut_ec);
            if (mut_ec) {
                std::this_thread::sleep_for(std::chrono::milliseconds(200));
                continue;
            }

            tcp::socket mut_new_socket(mut_io);
            asio::connect(mut_new_socket, mut_endpoints, mut_ec);
            if (mut_ec) {
                std::this_thread::sleep_for(std::chrono::milliseconds(200));
                continue;
            }
            set_non_blocking(mut_new_socket, true);
            mut_socket = std::move(mut_new_socket);
            const bool was_connected = mut_connected_.exchange(true);
            if (!was_connected) {
                try_invoke(callbacks_.on_th_connected);
            }
        }

        PendingWrite mut_item{};
        {
            std::unique_lock<std::mutex> lock(mut_mutex_);
            mut_cv_.wait(lock, [&] {
                return stop_token.stop_requested() || !mut_queue_.empty() || !mut_connected_.load();
            });
            if (stop_token.stop_requested()) {
                break;
            }
            if (!mut_connected_.load()) {
                continue;
            }
            if (mut_queue_.empty()) {
                continue;
            }
            mut_item = std::move(mut_queue_.front());
            mut_queue_.pop_front();
        }

        if (!mut_socket || !mut_socket->is_open()) {
            std::lock_guard<std::mutex> lock(mut_mutex_);
            mut_queue_.push_front(std::move(mut_item));
            continue;
        }

        const std::uint8_t mut_type = static_cast<std::uint8_t>(MessageType::kWrite);
        const std::uint32_t mut_name_len = static_cast<std::uint32_t>(mut_item.mut_queue_name.size());
        const std::uint32_t mut_payload_len =
            static_cast<std::uint32_t>(mut_item.mut_payload.size());

        if (!write_u8(*mut_socket, mut_type, stop_token) ||
            !write_u32(*mut_socket, mut_name_len, stop_token) ||
            !write_all(*mut_socket, mut_item.mut_queue_name.data(), mut_item.mut_queue_name.size(),
                       stop_token) ||
            !write_u32(*mut_socket, mut_payload_len, stop_token) ||
            !write_buffer_chunked(*mut_socket, mut_item.mut_payload, throttle_, stop_token)) {
            mut_mark_disconnect();
            if (mut_socket && mut_socket->is_open()) {
                asio::error_code mut_ec;
                mut_socket->close(mut_ec);
            }
            std::lock_guard<std::mutex> lock(mut_mutex_);
            mut_queue_.push_front(std::move(mut_item));
            continue;
        }

        NetStats mut_local{};
        mut_local.messages = 1;
        mut_local.bytes = mut_payload_len;
        const auto chunk_size = std::max<std::size_t>(
            1, throttle_.max_chunk_size == 0 ? mut_item.mut_payload.size() : throttle_.max_chunk_size);
        mut_local.chunks = (mut_item.mut_payload.size() + chunk_size - 1) / chunk_size;

        {
            std::lock_guard<std::mutex> lock(mut_mutex_);
            mut_stats_.merge(mut_local);
        }
        try_invoke_stats(callbacks_.on_th_stats, mut_stats_);
    }

    mut_mark_disconnect();
    if (mut_socket && mut_socket->is_open()) {
        asio::error_code mut_ec;
        mut_socket->close(mut_ec);
    }
}

bool ServerWriter::drop_queue(const std::string& queue_name) {
    {
        std::lock_guard<std::mutex> lock(mut_writers_mutex_);
        mut_writers_.erase(queue_name);
    }
    return drop_queue_files(resolver_(queue_name));
}

ClientReader::ClientReader(std::string host, std::uint16_t port, ClientCallbacks callbacks)
    : host_(std::move(host)), port_(port), callbacks_(std::move(callbacks)) {}

ClientReader::~ClientReader() {
    stop();
}

void ClientReader::start() {
    if (mut_running_.exchange(true)) {
        return;
    }
    mut_worker_ = std::jthread([this](std::stop_token stop_token) { run(stop_token); });
}

void ClientReader::stop() {
    if (!mut_running_.exchange(false)) {
        return;
    }
    if (mut_worker_.joinable()) {
        mut_worker_.request_stop();
        {
            std::lock_guard<std::mutex> lock(mut_mutex_);
            mut_cv_.notify_all();
        }
        mut_worker_.join();
    }
}

std::future<ReadBatch> ClientReader::request(const std::string& queue_name,
                                             std::size_t max_records) {
    PendingRead mut_req{};
    mut_req.mut_queue_name = queue_name;
    mut_req.mut_max_records = max_records;
    auto mut_result_future = mut_req.mut_promise.get_future();
    {
        std::lock_guard<std::mutex> lock(mut_mutex_);
        mut_queue_.push_back(std::move(mut_req));
    }
    mut_cv_.notify_all();
    return mut_result_future;
}

NetStats ClientReader::stats() const {
    std::lock_guard<std::mutex> lock(mut_mutex_);
    return mut_stats_;
}

void ClientReader::run(std::stop_token stop_token) {
    asio::io_context mut_io;
    tcp::resolver mut_resolver(mut_io);
    std::optional<tcp::socket> mut_socket;

    auto mut_mark_disconnect = [this]() {
        const bool was_connected = mut_connected_.exchange(false);
        if (was_connected) {
            try_invoke(callbacks_.on_th_disconnected);
        }
    };

    while (!stop_token.stop_requested()) {
        if (!mut_socket || !mut_socket->is_open()) {
            asio::error_code mut_ec;
            const auto mut_endpoints =
                mut_resolver.resolve(host_, std::to_string(port_), mut_ec);
            if (mut_ec) {
                std::this_thread::sleep_for(std::chrono::milliseconds(200));
                continue;
            }
            tcp::socket mut_new_socket(mut_io);
            asio::connect(mut_new_socket, mut_endpoints, mut_ec);
            if (mut_ec) {
                std::this_thread::sleep_for(std::chrono::milliseconds(200));
                continue;
            }
            set_non_blocking(mut_new_socket, true);
            mut_socket = std::move(mut_new_socket);
            const bool was_connected = mut_connected_.exchange(true);
            if (!was_connected) {
                try_invoke(callbacks_.on_th_connected);
            }
        }

        PendingRead mut_req{};
        {
            std::unique_lock<std::mutex> lock(mut_mutex_);
            mut_cv_.wait(lock, [&] {
                return stop_token.stop_requested() || !mut_queue_.empty() || !mut_connected_.load();
            });
            if (stop_token.stop_requested()) {
                break;
            }
            if (!mut_connected_.load()) {
                continue;
            }
            if (mut_queue_.empty()) {
                continue;
            }
            mut_req = std::move(mut_queue_.front());
            mut_queue_.pop_front();
        }

        if (!mut_socket || !mut_socket->is_open()) {
            ReadBatch mut_batch{};
            mut_batch.error = true;
            mut_batch.disconnected = true;
            mut_req.mut_promise.set_value(std::move(mut_batch));
            continue;
        }

        const std::uint8_t mut_type = static_cast<std::uint8_t>(MessageType::kReadRequest);
        const std::uint32_t mut_name_len =
            static_cast<std::uint32_t>(mut_req.mut_queue_name.size());
        const std::uint32_t mut_max = static_cast<std::uint32_t>(mut_req.mut_max_records);

        ReadBatch mut_batch{};

        if (!write_u8(*mut_socket, mut_type, stop_token) ||
            !write_u32(*mut_socket, mut_name_len, stop_token) ||
            !write_all(*mut_socket, mut_req.mut_queue_name.data(), mut_req.mut_queue_name.size(),
                       stop_token) ||
            !write_u32(*mut_socket, mut_max, stop_token)) {
            mut_mark_disconnect();
            if (mut_socket && mut_socket->is_open()) {
                asio::error_code mut_ec;
                mut_socket->close(mut_ec);
            }
            mut_batch.error = true;
            mut_batch.disconnected = true;
            mut_req.mut_promise.set_value(std::move(mut_batch));
            continue;
        }

        std::uint8_t mut_resp_type = 0;
        std::uint8_t mut_resp_error = 0;
        std::uint8_t mut_resp_sig_changed = 0;
        std::uint32_t mut_resp_count = 0;

        if (!read_all(*mut_socket, &mut_resp_type, sizeof(mut_resp_type), stop_token) ||
            mut_resp_type != static_cast<std::uint8_t>(MessageType::kReadResponse) ||
            !read_all(*mut_socket, &mut_resp_error, sizeof(mut_resp_error), stop_token) ||
            !read_all(*mut_socket, &mut_resp_sig_changed, sizeof(mut_resp_sig_changed),
                      stop_token) ||
            !read_u32(*mut_socket, mut_resp_count, stop_token)) {
            mut_mark_disconnect();
            if (mut_socket && mut_socket->is_open()) {
                asio::error_code mut_ec;
                mut_socket->close(mut_ec);
            }
            mut_batch.error = true;
            mut_batch.disconnected = true;
            mut_req.mut_promise.set_value(std::move(mut_batch));
            continue;
        }

        mut_batch.error = mut_resp_error != 0;
        mut_batch.disconnected = false;
        mut_batch.signature_changed = mut_resp_sig_changed != 0;

        for (std::uint32_t mut_i = 0; mut_i < mut_resp_count; ++mut_i) {
            std::uint32_t mut_msg_size = 0;
            if (!read_u32(*mut_socket, mut_msg_size, stop_token)) {
                mut_batch.error = true;
                mut_batch.disconnected = true;
                mut_mark_disconnect();
                if (mut_socket && mut_socket->is_open()) {
                    asio::error_code mut_ec;
                    mut_socket->close(mut_ec);
                }
                break;
            }
            const auto mut_payload = read_buffer(*mut_socket, mut_msg_size, stop_token);
            if (mut_payload.size() != mut_msg_size) {
                mut_batch.error = true;
                mut_batch.disconnected = true;
                mut_mark_disconnect();
                if (mut_socket && mut_socket->is_open()) {
                    asio::error_code mut_ec;
                    mut_socket->close(mut_ec);
                }
                break;
            }
            mut_batch.messages.emplace_back(
                reinterpret_cast<const char*>(mut_payload.data()), mut_payload.size());
        }

        {
            std::lock_guard<std::mutex> lock(mut_mutex_);
            mut_stats_.messages += mut_batch.messages.size();
            for (const auto& message : mut_batch.messages) {
                mut_stats_.bytes += message.size();
                const auto chunk_size = message.empty() ? 1U : message.size();
                mut_stats_.chunks += (message.size() + chunk_size - 1) / chunk_size;
            }
        }
        try_invoke_stats(callbacks_.on_th_stats, mut_stats_);
        mut_req.mut_promise.set_value(std::move(mut_batch));
    }

    mut_mark_disconnect();
    if (mut_socket && mut_socket->is_open()) {
        asio::error_code mut_ec;
        mut_socket->close(mut_ec);
    }
}

ServerWriter::ServerWriter(std::string bind_address,
                           std::uint16_t port,
                           QueuePathResolver resolver,
                           NetThrottleConfig throttle,
                           ServerCallbacks callbacks)
    : bind_address_(std::move(bind_address)),
      port_(port),
      resolver_(std::move(resolver)),
      throttle_(throttle),
      callbacks_(std::move(callbacks)) {}

ServerWriter::~ServerWriter() {
    stop();
}

void ServerWriter::start() {
    if (mut_running_.exchange(true)) {
        return;
    }
    mut_acceptor_ = std::jthread([this](std::stop_token stop_token) { run(stop_token); });
}

void ServerWriter::stop() {
    if (!mut_running_.exchange(false)) {
        return;
    }
    if (mut_acceptor_.joinable()) {
        mut_acceptor_.request_stop();
        mut_acceptor_.join();
    }
    {
        std::lock_guard<std::mutex> lock(mut_clients_mutex_);
        for (auto& mut_thread : mut_client_threads_) {
            if (mut_thread.joinable()) {
                mut_thread.request_stop();
                mut_thread.join();
            }
        }
        mut_client_threads_.clear();
    }
}

void ServerWriter::run(std::stop_token stop_token) {
    asio::io_context mut_io;
    asio::error_code mut_ec;
    const auto mut_addr = asio::ip::make_address(bind_address_, mut_ec);
    if (mut_ec) {
        return;
    }
    tcp::acceptor mut_acceptor(mut_io, tcp::endpoint(mut_addr, port_));
    set_non_blocking(mut_acceptor, true);

    while (!stop_token.stop_requested()) {
        tcp::socket mut_socket(mut_io);
        mut_acceptor.accept(mut_socket, mut_ec);
        if (mut_ec == asio::error::would_block || mut_ec == asio::error::try_again) {
            std::this_thread::sleep_for(std::chrono::milliseconds(50));
            continue;
        }
        if (mut_ec) {
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            continue;
        }
        set_non_blocking(mut_socket, true);
        std::lock_guard<std::mutex> lock(mut_clients_mutex_);
        mut_client_threads_.emplace_back([this, mut_socket = std::move(mut_socket)](
                                              std::stop_token client_stop_token) mutable {
            std::unordered_map<std::string, NetStats> mut_stats_by_queue;
            bool mut_announced = false;
            std::string mut_last_queue;
            while (!client_stop_token.stop_requested()) {
                std::uint8_t mut_type = 0;
                if (!read_all(mut_socket, &mut_type, sizeof(mut_type), client_stop_token)) {
                    break;
                }
                if (mut_type != static_cast<std::uint8_t>(MessageType::kWrite)) {
                    break;
                }

                std::uint32_t mut_name_len = 0;
                if (!read_u32(mut_socket, mut_name_len, client_stop_token) ||
                    mut_name_len == 0 || mut_name_len > kMaxQueueName) {
                    break;
                }
                std::string mut_queue_name(mut_name_len, '\0');
                if (!read_all(mut_socket, mut_queue_name.data(), mut_queue_name.size(),
                              client_stop_token)) {
                    break;
                }
                if (!mut_announced) {
                    try_invoke_client(callbacks_.on_th_client_connected, mut_queue_name);
                    mut_announced = true;
                    mut_last_queue = mut_queue_name;
                }
                std::uint32_t mut_payload_len = 0;
                if (!read_u32(mut_socket, mut_payload_len, client_stop_token) ||
                    mut_payload_len > kMaxPayload) {
                    break;
                }
                const auto mut_payload = read_buffer(mut_socket, mut_payload_len, client_stop_token);
                if (mut_payload.size() != mut_payload_len) {
                    break;
                }

                try {
                    std::lock_guard<std::mutex> writer_lock(mut_writers_mutex_);
                    auto iter = mut_writers_.find(mut_queue_name);
                    if (iter == mut_writers_.end()) {
                        auto mut_writer = std::make_unique<QueueWriter>(resolver_(mut_queue_name));
                            auto [new_iter, mut_inserted] =
                                mut_writers_.emplace(mut_queue_name, std::move(mut_writer));
                            iter = new_iter;
                            (void)mut_inserted;
                        }
                        iter->second->write(std::span<const std::byte>(mut_payload.data(),
                                                                   mut_payload.size()));
                    } catch (...) {
                        break;
                }

                NetStats mut_local{};
                mut_local.messages = 1;
                mut_local.bytes = mut_payload_len;
                const std::size_t chunk_size = std::max<std::size_t>(
                    1, throttle_.max_chunk_size == 0 ? mut_payload.size() : throttle_.max_chunk_size);
                mut_local.chunks = (mut_payload.size() + chunk_size - 1) / chunk_size;
                auto& mut_stats_ref = mut_stats_by_queue[mut_queue_name];
                mut_stats_ref.merge(mut_local);
                try_invoke_stats_client(callbacks_.on_th_stats, mut_queue_name, mut_stats_ref);
            }
            if (mut_announced) {
                try_invoke_client(callbacks_.on_th_client_disconnected, mut_last_queue);
            }
            asio::error_code mut_close_ec;
            mut_socket.close(mut_close_ec);
        });
    }
}

ServerReader::ServerReader(std::string bind_address,
                           std::uint16_t port,
                           QueuePathResolver resolver,
                           NetThrottleConfig throttle,
                           ServerCallbacks callbacks)
    : bind_address_(std::move(bind_address)),
      port_(port),
      resolver_(std::move(resolver)),
      throttle_(throttle),
      callbacks_(std::move(callbacks)) {}

ServerReader::~ServerReader() {
    stop();
}

void ServerReader::start() {
    if (mut_running_.exchange(true)) {
        return;
    }
    mut_acceptor_ = std::jthread([this](std::stop_token stop_token) { run(stop_token); });
}

void ServerReader::stop() {
    if (!mut_running_.exchange(false)) {
        return;
    }
    if (mut_acceptor_.joinable()) {
        mut_acceptor_.request_stop();
        mut_acceptor_.join();
    }
    {
        std::lock_guard<std::mutex> lock(mut_clients_mutex_);
        for (auto& mut_thread : mut_client_threads_) {
            if (mut_thread.joinable()) {
                mut_thread.request_stop();
                mut_thread.join();
            }
        }
        mut_client_threads_.clear();
    }
}

bool ServerReader::drop_queue(const std::string& queue_name) {
    return drop_queue_files(resolver_(queue_name));
}

void ServerReader::run(std::stop_token stop_token) {
    asio::io_context mut_io;
    asio::error_code mut_ec;
    const auto mut_addr = asio::ip::make_address(bind_address_, mut_ec);
    if (mut_ec) {
        return;
    }
    tcp::acceptor mut_acceptor(mut_io, tcp::endpoint(mut_addr, port_));
    set_non_blocking(mut_acceptor, true);

    while (!stop_token.stop_requested()) {
        tcp::socket mut_socket(mut_io);
        mut_acceptor.accept(mut_socket, mut_ec);
        if (mut_ec == asio::error::would_block || mut_ec == asio::error::try_again) {
            std::this_thread::sleep_for(std::chrono::milliseconds(50));
            continue;
        }
        if (mut_ec) {
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            continue;
        }
        set_non_blocking(mut_socket, true);
        std::lock_guard<std::mutex> lock(mut_clients_mutex_);
        mut_client_threads_.emplace_back([this, mut_socket = std::move(mut_socket)](
                                              std::stop_token client_stop_token) mutable {
            std::unordered_map<std::string, std::unique_ptr<QueueReader>> mut_readers;
            std::unordered_map<std::string, NetStats> mut_stats_by_queue;
            bool mut_announced = false;
            std::string mut_last_queue;
            while (!client_stop_token.stop_requested()) {
                std::uint8_t mut_type = 0;
                if (!read_all(mut_socket, &mut_type, sizeof(mut_type), client_stop_token)) {
                    break;
                }
                if (mut_type != static_cast<std::uint8_t>(MessageType::kReadRequest)) {
                    break;
                }
                std::uint32_t mut_name_len = 0;
                if (!read_u32(mut_socket, mut_name_len, client_stop_token) ||
                    mut_name_len == 0 || mut_name_len > kMaxQueueName) {
                    break;
                }
                std::string mut_queue_name(mut_name_len, '\0');
                if (!read_all(mut_socket, mut_queue_name.data(), mut_queue_name.size(),
                              client_stop_token)) {
                    break;
                }
                std::uint32_t mut_max_records = 0;
                if (!read_u32(mut_socket, mut_max_records, client_stop_token)) {
                    break;
                }
                if (!mut_announced) {
                    try_invoke_client(callbacks_.on_th_client_connected, mut_queue_name);
                    mut_announced = true;
                    mut_last_queue = mut_queue_name;
                }

                QueueReader* mut_reader_ptr = nullptr;
                auto iter = mut_readers.find(mut_queue_name);
                if (iter == mut_readers.end()) {
                    try {
                        auto mut_reader = std::make_unique<QueueReader>(resolver_(mut_queue_name));
                        auto [new_iter, mut_inserted] =
                            mut_readers.emplace(mut_queue_name, std::move(mut_reader));
                        iter = new_iter;
                        (void)mut_inserted;
                    } catch (...) {
                        iter = mut_readers.end();
                    }
                }
                if (iter != mut_readers.end()) {
                    mut_reader_ptr = iter->second.get();
                }

                ReadResult mut_result{};
                if (mut_reader_ptr) {
                    mut_result = mut_reader_ptr->read(mut_max_records);
                } else {
                    mut_result.inconsistent = true;
                }

                const std::uint8_t mut_resp_type = static_cast<std::uint8_t>(MessageType::kReadResponse);
                const std::uint8_t mut_resp_error = mut_result.inconsistent ? 1 : 0;
                const std::uint8_t mut_resp_sig_changed = mut_result.signature_changed ? 1 : 0;
                const std::uint32_t mut_resp_count =
                    mut_result.inconsistent ? 0 : static_cast<std::uint32_t>(mut_result.messages.size());

                if (!write_u8(mut_socket, mut_resp_type, client_stop_token) ||
                    !write_all(mut_socket, &mut_resp_error, sizeof(mut_resp_error),
                               client_stop_token) ||
                    !write_all(mut_socket, &mut_resp_sig_changed, sizeof(mut_resp_sig_changed),
                               client_stop_token) ||
                    !write_u32(mut_socket, mut_resp_count, client_stop_token)) {
                    break;
                }

                bool mut_ok = !mut_result.inconsistent;
                if (mut_ok) {
                    for (const auto& message : mut_result.messages) {
                        const std::uint32_t mut_size = static_cast<std::uint32_t>(message.size());
                        if (!write_u32(mut_socket, mut_size, client_stop_token)) {
                            mut_ok = false;
                            break;
                        }
                        const auto* mut_ptr = reinterpret_cast<const std::byte*>(message.data());
                        if (!write_buffer_chunked(mut_socket,
                                                  std::span<const std::byte>(mut_ptr, message.size()),
                                                  throttle_, client_stop_token)) {
                            mut_ok = false;
                            break;
                        }
                        auto& mut_stats_ref = mut_stats_by_queue[mut_queue_name];
                        mut_stats_ref.messages += 1;
                        mut_stats_ref.bytes += mut_size;
                        const std::size_t chunk_size = std::max<std::size_t>(
                            1, throttle_.max_chunk_size == 0 ? mut_size : throttle_.max_chunk_size);
                        mut_stats_ref.chunks += (mut_size + chunk_size - 1) / chunk_size;
                        try_invoke_stats_client(callbacks_.on_th_stats, mut_queue_name,
                                                mut_stats_ref);
                    }
                }
                if (!mut_ok) {
                    break;
                }
            }
            if (mut_announced) {
                try_invoke_client(callbacks_.on_th_client_disconnected, mut_last_queue);
            }
            asio::error_code mut_close_ec;
            mut_socket.close(mut_close_ec);
        });
    }
}

}  // namespace chi
