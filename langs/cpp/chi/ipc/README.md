# Inter Process Communication

Simple file-backed binary queue in C++20 (namespace `chi::ipc`) with `QueueWriter` and `QueueReader`.

`.queue` file format:
- Header: little-endian `uint32` with signature length, then the signature (ISO timestamp + random suffix).
- Records: bytes `0x00 0xFF`, little-endian `uint32` size, then the binary payload.
- On startup the writer trims any incomplete trailing record it detects.

Basic usage:
- Writer:
  - `chi::ipc::QueueWriter mut_writer("queue");`
  - `mut_writer.write("hello");`
  - `mut_writer.reset();` to start fresh with a new signature.
- Reader:
  - `chi::ipc::QueueReader mut_reader("queue");`
  - For multiple readers, give each one its own cursor file:
    `chi::ipc::QueueReader mut_reader("queue", "queue.reader1.cursor");`
  - `const auto result = mut_reader.read(10);`
  - `result.signature_changed` when the queue was recreated; `result.inconsistent` if the file is corrupt.
  - Use `result.at(i)` for bounds-checked access or `result.get(i)` to get an `std::optional<std::string_view>`.
  - `const auto preview = mut_reader.peek(1);` to inspect the next message without consuming it.
  - After processing a peeked message successfully, call `mut_reader.consume(1);` to advance the cursor.

The reader stores its position in `queue.cursor` by default. Use a distinct cursor file per reader to run multiple readers against the same queue. Works on Linux/Windows with C++20.

## Full example

Example file at `cmake/main.cpp`:

```cpp
#include <iostream>
#include "../src/ipc.h"

int main() {
    chi::ipc::QueueWriter mut_writer("example.queue");
    mut_writer.write("hello");
    mut_writer.write("world");

    chi::ipc::QueueReader mut_reader("example.queue");
    const auto result = mut_reader.read(2);
    if (result.signature_changed) {
        std::cout << "queue reset\n";
    }
    for (std::size_t i = 0; i < result.size(); ++i) {
        std::cout << "message: " << result.at(i) << "\n";
    }
}
```

Quick build (from repo root):

```bash
g++ -std=c++20 -I./src cmake/main.cpp src/ipc.cpp -o ipc_demo
./ipc_demo
```

### Binary payload example

```cpp
#include <array>
#include "../src/ipc.h"

int main() {
    chi::ipc::QueueWriter mut_writer("bin.queue");
    const auto bytes = std::array<std::byte, 4>{std::byte{0xDE}, std::byte{0xAD},
                                                std::byte{0xBE}, std::byte{0xEF}};
    mut_writer.write(std::span<const std::byte>(bytes));

    chi::ipc::QueueReader mut_reader("bin.queue");
    const auto result = mut_reader.read(1);
    if (!result.empty()) {
        const std::string& raw = result.at(0);  // contains 0xDE 0xAD 0xBE 0xEF
        // raw.size() == 4; may contain '\0' without truncation.
    }
}
```

### Example serializing a struct

Manual binary serialization of a struct (string + int + double) and reading it back:

```cpp
#include <cstring>
#include <string>
#include <vector>
#include "../src/ipc.h"

struct Record {
    std::string name;
    int id;
    double score;
};

std::vector<std::byte> to_bytes(const Record& r) {
    std::vector<std::byte> mut_buf;
    const auto append = [&](auto ptr, std::size_t n) {
        const auto* bytes = reinterpret_cast<const std::byte*>(ptr);
        mut_buf.insert(mut_buf.end(), bytes, bytes + n);
    };
    const auto len = static_cast<std::uint32_t>(r.name.size());
    append(&len, sizeof(len));
    append(r.name.data(), r.name.size());
    append(&r.id, sizeof(r.id));
    append(&r.score, sizeof(r.score));
    return mut_buf;
}

Record from_bytes(const std::string& raw) {
    Record result{};
    const std::byte* mut_bytes = reinterpret_cast<const std::byte*>(raw.data());
    const auto* end = mut_bytes + raw.size();
    const auto need = [&](std::size_t n) {
        if (static_cast<std::size_t>(end - mut_bytes) < n) throw std::runtime_error("incomplete");
    };
    need(4);
    std::uint32_t mut_len;
    std::memcpy(&mut_len, mut_bytes, 4);
    mut_bytes += 4;
    need(mut_len + sizeof(int) + sizeof(double));
    result.name.assign(reinterpret_cast<const char*>(mut_bytes), mut_len);
    mut_bytes += mut_len;
    std::memcpy(&result.id, mut_bytes, sizeof(int));
    mut_bytes += sizeof(int);
    std::memcpy(&result.score, mut_bytes, sizeof(double));
    return result;
}

int main() {
    chi::ipc::QueueWriter mut_writer("struct.queue");
    const auto rec = Record{"alice", 42, 9.5};
    const auto payload = to_bytes(rec);
    mut_writer.write(std::span<const std::byte>(payload.data(), payload.size()));

    chi::ipc::QueueReader mut_reader("struct.queue");
    const auto result = mut_reader.read(1);
    if (!result.empty()) {
        const auto got = from_bytes(result.at(0));
        // got.name == "alice", got.id == 42, got.score == 9.5
    }
}
```


## Hacking

- Cursor writes use `queue.cursor.tmp` and rotate the previous cursor to `queue.cursor.prev` as a safety net; if the final rename fails, the reader automatically restores the last known cursor state.
- Cursor files store the queue path, signature, and offset (in that order) to help avoid mixing cursors across queues.
