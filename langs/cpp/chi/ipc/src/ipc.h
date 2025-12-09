#pragma once

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace chi::ipc {

struct ReadResult {
  std::vector<std::string> messages;
  bool signature_changed{false};
  bool inconsistent{false};
};

class QueueWriter {
public:
  explicit QueueWriter(std::filesystem::path queue_path);

  // Drops the queue and starts from zero with a new signature.
  void reset();

  // Write a record with binary data.
  void write(std::span<const std::byte> data);
  void write(std::string_view data);

  const std::string &signature() const { return signature_; }

private:
  std::filesystem::path queue_path_;
  std::fstream file_;
  std::string signature_;
  std::uint64_t header_end_{0};

  void openForAppend();
  bool loadExisting();
  void writeSignature(std::ostream &out);
};

class QueueReader {
public:
  explicit QueueReader(std::filesystem::path queue_path);

  // Read at most max_records records.
  ReadResult read(const std::size_t max_records);
  // Inspect up to max_records without advancing the cursor.
  ReadResult peek(const std::size_t max_records);
  // Advance the cursor by discarding up to max_records records. Returns false
  // if unavailable.
  bool consume(const std::size_t max_records);

  const std::string &signature() const { return cursor_signature_; }
  std::filesystem::path cursor_path() const { return cursor_path_; }

private:
  std::filesystem::path queue_path_;
  std::filesystem::path cursor_path_;
  std::string cursor_signature_;
  std::uint64_t cursor_offset_{0};
  std::uint64_t header_end_{0};

  void ensureCursor();
  void loadCursor();
  void storeCursor();
  bool loadQueueSignature(std::string &signature_out,
                          std::uint64_t &header_out) const;
};

// Remove queue file and its cursor file. Returns true on success or if files do
// not exist.
bool drop_queue_files(const std::filesystem::path &queue_path);

} // namespace chi::ipc
