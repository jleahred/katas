
#include "ipc.h"

#include <chrono>
#include <cstdint>
#include <ctime>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <random>
#include <sstream>
#include <stdexcept>
#include <system_error>

namespace {

using chi::ipc::ReadResult;

constexpr std::byte kStartA{std::byte{0x00}};
constexpr std::byte kStartB{std::byte{0xFF}};
constexpr std::size_t kMaxSignatureSize = 4096;
constexpr std::size_t kMaxRecordSize = 64 * 1024 * 1024; // 64MB guardrail.

std::string make_signature() {
  const auto now = std::chrono::system_clock::now();
  const auto t = std::chrono::system_clock::to_time_t(now);
  std::tm mut_tm{};
#if defined(_WIN32)
  gmtime_s(&mut_tm, &t);
#else
  gmtime_r(&t, &mut_tm);
#endif
  std::ostringstream mut_oss;
  mut_oss << std::put_time(&mut_tm, "%Y-%m-%dT%H:%M:%SZ");
  std::random_device mut_rd{};
  std::uniform_int_distribution<int> mut_dist(0, 0xFFFF);
  const auto rand_val = mut_dist(mut_rd);
  mut_oss << "-" << std::hex << std::setw(4) << std::setfill('0') << rand_val;
  return mut_oss.str();
}

void write_u32(std::ostream &os, std::uint32_t value) {
  char mut_buf[4];
  mut_buf[0] = static_cast<char>(value & 0xFF);
  mut_buf[1] = static_cast<char>((value >> 8) & 0xFF);
  mut_buf[2] = static_cast<char>((value >> 16) & 0xFF);
  mut_buf[3] = static_cast<char>((value >> 24) & 0xFF);
  os.write(mut_buf, 4);
}

bool read_u32(std::istream &is, std::uint32_t &value) {
  char mut_buf[4];
  if (!is.read(mut_buf, 4)) {
    return false;
  }
  value = static_cast<std::uint8_t>(mut_buf[0]) |
          (static_cast<std::uint8_t>(mut_buf[1]) << 8) |
          (static_cast<std::uint8_t>(mut_buf[2]) << 16) |
          (static_cast<std::uint8_t>(mut_buf[3]) << 24);
  return true;
}

std::uint64_t file_size_or_zero(const std::filesystem::path &p) {
  std::error_code mut_ec;
  const auto sz = std::filesystem::file_size(p, mut_ec);
  if (mut_ec) {
    return 0;
  }
  return sz;
}

// Scan file to find the last valid offset. Returns false if the signature is
// invalid.
bool validate_and_trim(const std::filesystem::path &path,
                       std::string &signature_out,
                       std::uint64_t &header_end_out) {
  std::ifstream mut_in(path, std::ios::binary);
  if (!mut_in) {
    return false;
  }
  std::uint32_t mut_sig_len = 0;
  if (!read_u32(mut_in, mut_sig_len) || mut_sig_len == 0 ||
      mut_sig_len > kMaxSignatureSize) {
    return false;
  }
  std::string mut_signature(mut_sig_len, '\0');
  if (!mut_in.read(mut_signature.data(), mut_sig_len)) {
    return false;
  }
  signature_out = mut_signature;
  header_end_out = 4 + mut_sig_len;

  std::uint64_t mut_last_good = header_end_out;
  while (true) {
    char mut_start[2];
    if (!mut_in.read(mut_start, 2)) {
      break; // clean EOF right after the last valid record
    }
    if (static_cast<std::byte>(mut_start[0]) != kStartA ||
        static_cast<std::byte>(mut_start[1]) != kStartB) {
      return false;
    }
    std::uint32_t mut_size = 0;
    if (!read_u32(mut_in, mut_size) || mut_size > kMaxRecordSize) {
      return false;
    }
    std::vector<char> mut_payload(mut_size);
    if (!mut_in.read(mut_payload.data(), mut_size)) {
      break; // incomplete record; truncate
    }
    mut_last_good = static_cast<std::uint64_t>(mut_in.tellg());
  }

  const auto actual_size = file_size_or_zero(path);
  if (mut_last_good < actual_size) {
    // Truncate trailing incomplete bytes.
    std::fstream mut_out(path, std::ios::in | std::ios::out | std::ios::binary);
    if (!mut_out) {
      return false;
    }
    mut_out.seekp(static_cast<std::streamoff>(mut_last_good));
    mut_out.flush();
    mut_out.close();
    std::error_code mut_ec;
    std::filesystem::resize_file(path, mut_last_good, mut_ec);
    if (mut_ec) {
      return false;
    }
  }
  return true;
}

} // namespace

namespace chi::ipc {

QueueWriter::QueueWriter(std::filesystem::path queue_path)
    : queue_path_(std::move(queue_path)) {
  if (!loadExisting()) {
    reset();
  } else {
    openForAppend();
  }
}

void QueueWriter::reset() {
  signature_ = make_signature();
  file_.close();
  file_.open(queue_path_, std::ios::binary | std::ios::out | std::ios::trunc);
  if (!file_) {
    throw std::runtime_error("Unable to open queue for reset");
  }
  writeSignature(file_);
  file_.close();
  openForAppend();
}

void QueueWriter::writeSignature(std::ostream &out) {
  const auto len = static_cast<std::uint32_t>(signature_.size());
  write_u32(out, len);
  out.write(signature_.data(), static_cast<std::streamsize>(signature_.size()));
  header_end_ = 4 + len;
}

bool QueueWriter::loadExisting() {
  std::string mut_sig;
  std::uint64_t mut_header = 0;
  if (!std::filesystem::exists(queue_path_)) {
    return false;
  }
  if (!validate_and_trim(queue_path_, mut_sig, mut_header)) {
    return false;
  }
  signature_ = mut_sig;
  header_end_ = mut_header;
  return true;
}

void QueueWriter::openForAppend() {
  file_.close();
  file_.open(queue_path_, std::ios::in | std::ios::out | std::ios::binary);
  if (!file_) {
    throw std::runtime_error("Unable to open queue for writing");
  }
  file_.seekp(0, std::ios::end);
}

void QueueWriter::write(std::span<const std::byte> data) {
  if (!file_) {
    throw std::runtime_error("Queue not available for writing");
  }
  file_.write(reinterpret_cast<const char *>(&kStartA), 1);
  file_.write(reinterpret_cast<const char *>(&kStartB), 1);
  const auto len = static_cast<std::uint32_t>(data.size());
  write_u32(file_, len);
  if (!data.empty()) {
    file_.write(reinterpret_cast<const char *>(data.data()),
                static_cast<std::streamsize>(data.size()));
  }
  file_.flush();
  if (!file_) {
    throw std::runtime_error("Failed to write to queue");
  }
}

void QueueWriter::write(std::string_view data) {
  const auto *ptr = reinterpret_cast<const std::byte *>(data.data());
  write(std::span<const std::byte>(ptr, data.size()));
}

QueueReader::QueueReader(std::filesystem::path queue_path)
    : queue_path_(std::move(queue_path)),
      cursor_path_(queue_path_.string() + ".cursor") {
  ensureCursor();
}

void QueueReader::ensureCursor() {
  const bool cursor_exists = std::filesystem::exists(cursor_path_);
  loadCursor();
  bool mut_needs_store = !cursor_exists;

  if (cursor_signature_.empty()) {
    std::string mut_sig;
    std::uint64_t mut_header = 0;
    if (loadQueueSignature(mut_sig, mut_header)) {
      cursor_signature_ = mut_sig;
      cursor_offset_ = mut_header;
      header_end_ = mut_header;
    } else {
      cursor_signature_.clear();
      cursor_offset_ = 0;
      header_end_ = 0;
    }
    mut_needs_store = true;
  }

  if (mut_needs_store) {
    storeCursor();
  }
}

void QueueReader::loadCursor() {
  std::ifstream mut_in(cursor_path_, std::ios::binary);
  if (!mut_in) {
    return;
  }
  std::getline(mut_in, cursor_signature_);
  mut_in >> cursor_offset_;
}

void QueueReader::storeCursor() {
  std::filesystem::path tmp_path = cursor_path_;
  tmp_path += ".tmp";
  std::filesystem::path prev_path = cursor_path_;
  prev_path += ".prev";

  {
    std::ofstream mut_out(tmp_path, std::ios::binary | std::ios::trunc);
    if (!mut_out) {
      throw std::runtime_error("Unable to write cursor tmp");
    }
    mut_out << cursor_signature_ << "\n" << cursor_offset_;
    mut_out.flush();
    if (!mut_out) {
      throw std::runtime_error("Unable to flush cursor tmp");
    }
  }

  std::error_code mut_ec;
  std::filesystem::remove(prev_path, mut_ec);
  mut_ec.clear();

  if (std::filesystem::exists(cursor_path_)) {
    std::filesystem::rename(cursor_path_, prev_path, mut_ec);
    if (mut_ec) {
      throw std::runtime_error("Unable to rotate cursor to .prev: " +
                               mut_ec.message());
    }
  }

  mut_ec.clear();
  std::filesystem::rename(tmp_path, cursor_path_, mut_ec);
  if (mut_ec) {
    // Try to recover old cursor if rename fails.
    std::error_code mut_restore_ec;
    std::filesystem::rename(prev_path, cursor_path_, mut_restore_ec);
    throw std::runtime_error("Unable to finalize cursor: " + mut_ec.message());
  }
}

bool QueueReader::loadQueueSignature(std::string &signature_out,
                                     std::uint64_t &header_out) const {
  std::ifstream mut_in(queue_path_, std::ios::binary);
  if (!mut_in) {
    return false;
  }
  std::uint32_t mut_sig_len = 0;
  if (!read_u32(mut_in, mut_sig_len) || mut_sig_len == 0 ||
      mut_sig_len > kMaxSignatureSize) {
    return false;
  }
  std::string mut_sig(mut_sig_len, '\0');
  if (!mut_in.read(mut_sig.data(), mut_sig_len)) {
    return false;
  }
  signature_out = mut_sig;
  header_out = 4 + mut_sig_len;
  return true;
}

ReadResult QueueReader::read(const std::size_t max_records) {
  ReadResult result{};
  if (max_records == 0) {
    return result;
  }

  std::string mut_current_sig;
  if (!loadQueueSignature(mut_current_sig, header_end_)) {
    result.inconsistent = true;
    return result;
  }
  if (mut_current_sig != cursor_signature_) {
    result.signature_changed = true;
    cursor_signature_ = mut_current_sig;
    cursor_offset_ = header_end_;
  }

  std::ifstream mut_in(queue_path_, std::ios::binary);
  if (!mut_in) {
    result.inconsistent = true;
    return result;
  }
  mut_in.seekg(static_cast<std::streamoff>(cursor_offset_));

  std::uint64_t mut_position = cursor_offset_;
  while (result.messages.size() < max_records) {
    char mut_start[2];
    if (!mut_in.read(mut_start, 2)) {
      break; // clean EOF
    }
    mut_position += 2;
    if (static_cast<std::byte>(mut_start[0]) != kStartA ||
        static_cast<std::byte>(mut_start[1]) != kStartB) {
      result.inconsistent = true;
      break;
    }

    std::uint32_t mut_size = 0;
    if (!read_u32(mut_in, mut_size) || mut_size > kMaxRecordSize) {
      result.inconsistent = true;
      break;
    }
    mut_position += 4;

    std::string mut_payload(mut_size, '\0');
    if (!mut_in.read(mut_payload.data(), mut_size)) {
      // Incomplete record; not an error, just leave it.
      break;
    }
    mut_position += mut_size;
    result.messages.emplace_back(std::move(mut_payload));
  }

  if (!result.inconsistent && mut_position != cursor_offset_) {
    cursor_offset_ = mut_position;
    storeCursor();
  } else if (result.signature_changed) {
    storeCursor();
  }

  return result;
}

ReadResult QueueReader::peek(const std::size_t max_records) {
  ReadResult result{};
  if (max_records == 0) {
    return result;
  }

  std::string mut_current_sig;
  if (!loadQueueSignature(mut_current_sig, header_end_)) {
    result.inconsistent = true;
    return result;
  }
  if (mut_current_sig != cursor_signature_) {
    result.signature_changed = true;
    cursor_signature_ = mut_current_sig;
    cursor_offset_ = header_end_;
  }

  std::ifstream mut_in(queue_path_, std::ios::binary);
  if (!mut_in) {
    result.inconsistent = true;
    return result;
  }
  mut_in.seekg(static_cast<std::streamoff>(cursor_offset_));

  while (result.messages.size() < max_records) {
    char mut_start[2];
    if (!mut_in.read(mut_start, 2)) {
      break; // clean EOF
    }
    if (static_cast<std::byte>(mut_start[0]) != kStartA ||
        static_cast<std::byte>(mut_start[1]) != kStartB) {
      result.inconsistent = true;
      break;
    }

    std::uint32_t mut_size = 0;
    if (!read_u32(mut_in, mut_size) || mut_size > kMaxRecordSize) {
      result.inconsistent = true;
      break;
    }

    std::string mut_payload(mut_size, '\0');
    if (!mut_in.read(mut_payload.data(), mut_size)) {
      break; // incomplete record
    }
    result.messages.emplace_back(std::move(mut_payload));
  }

  if (result.signature_changed) {
    storeCursor();
  }

  return result;
}

bool QueueReader::consume(const std::size_t max_records) {
  if (max_records == 0) {
    return true;
  }

  std::string mut_current_sig;
  if (!loadQueueSignature(mut_current_sig, header_end_)) {
    return false;
  }
  if (mut_current_sig != cursor_signature_) {
    cursor_signature_ = mut_current_sig;
    cursor_offset_ = header_end_;
    storeCursor();
  }

  std::ifstream mut_in(queue_path_, std::ios::binary);
  if (!mut_in) {
    return false;
  }
  mut_in.seekg(static_cast<std::streamoff>(cursor_offset_));

  std::uint64_t mut_position = cursor_offset_;
  std::size_t consumed = 0;

  while (consumed < max_records) {
    char mut_start[2];
    if (!mut_in.read(mut_start, 2)) {
      return false;
    }
    if (static_cast<std::byte>(mut_start[0]) != kStartA ||
        static_cast<std::byte>(mut_start[1]) != kStartB) {
      return false;
    }

    std::uint32_t mut_size = 0;
    if (!read_u32(mut_in, mut_size) || mut_size > kMaxRecordSize) {
      return false;
    }

    if (!mut_in.seekg(static_cast<std::streamoff>(mut_size), std::ios::cur)) {
      return false;
    }

    mut_position += 2 + 4 + mut_size;
    consumed++;
  }

  cursor_offset_ = mut_position;
  storeCursor();
  return true;
}

} // namespace chi::ipc
