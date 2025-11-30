#include "ipc.h"

#include <system_error>

namespace chi {

bool drop_queue_files(const std::filesystem::path& queue_path) {
    const auto cursor_path = std::filesystem::path(queue_path.string() + ".cursor");

    std::error_code mut_ec;
    std::filesystem::remove(queue_path, mut_ec);
    std::filesystem::remove(cursor_path, mut_ec);
    // Consider removal successful even if files were missing; report failure only on hard errors.
    return !mut_ec;
}

}  // namespace chi
