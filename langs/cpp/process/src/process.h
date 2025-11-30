#pragma once

#include <functional>
#include <string>
#include <string_view>
#include <vector>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/types.h>
#endif

namespace chi {

struct OutputBatch
{
    std::vector<std::string> stdout_lines;
    std::vector<std::string> stderr_lines;

    [[nodiscard]] bool empty() const;
};

class Process
{
public:
    Process();
    ~Process();

    Process(const Process&) = delete;
    Process& operator=(const Process&) = delete;
    Process(Process&&) = delete;
    Process& operator=(Process&&) = delete;

    [[nodiscard]] long run(const std::vector<std::string>& command);
    [[nodiscard]] long run(const std::function<int()>& action);

    [[nodiscard]] bool is_running();
    [[nodiscard]] bool request_stop();
    [[nodiscard]] OutputBatch poll_output();
    [[nodiscard]] long pid() const;
    [[nodiscard]] bool write(std::string_view data);

private:
    [[nodiscard]] bool can_spawn() const;
#ifdef _WIN32
    [[nodiscard]] bool spawn_process(const std::vector<std::string>& command);
    void close_handles();
    void read_pipe(HANDLE pipe_handle, std::string& buffer, std::vector<std::string>& lines);

    PROCESS_INFORMATION mut_process_info{};
    HANDLE mut_stdout_read{nullptr};
    HANDLE mut_stderr_read{nullptr};
    HANDLE mut_stdin_write{nullptr};
#else
    [[nodiscard]] bool spawn_process(const std::vector<std::string>& command);
    [[nodiscard]] bool spawn_action(const std::function<int()>& action);
    void close_fds();
    void read_pipe(int fd, std::string& buffer, std::vector<std::string>& lines);

    pid_t mut_pid{0};
    int mut_stdout_fd{-1};
    int mut_stderr_fd{-1};
    int mut_stdin_fd{-1};
#endif
    std::string mut_stdout_buffer;
    std::string mut_stderr_buffer;
    std::vector<std::string> mut_stdout_pending;
    std::vector<std::string> mut_stderr_pending;
};

}
