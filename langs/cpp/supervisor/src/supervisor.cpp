#include "supervisor.h"

#include <array>
#include <chrono>
#include <cstdint>
#include <stdexcept>
#include <string_view>
#include <vector>

#ifdef _WIN32
#include <processthreadsapi.h>
#include <synchapi.h>
#include <winbase.h>
#else
#include <cerrno>
#include <csignal>
#include <fcntl.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

namespace
{
#ifndef _WIN32
bool make_non_blocking(const int fd)
{
    const auto current_flags = fcntl(fd, F_GETFL, 0);
    if (current_flags == -1)
    {
        return false;
    }

    return fcntl(fd, F_SETFL, current_flags | O_NONBLOCK) != -1;
}
#else
std::string build_command_line(const std::vector<std::string>& command)
{
    std::string result;
    for (const auto& part : command)
    {
        if (!result.empty())
        {
            result.push_back(' ');
        }

        const auto needs_quotes = part.find_first_of(" \t\"") != std::string::npos;
        if (!needs_quotes)
        {
            result += part;
            continue;
        }

        result.push_back('"');
        for (const auto ch : part)
        {
            if (ch == '"')
            {
                result.push_back('\\');
            }
            result.push_back(ch);
        }
        result.push_back('"');
    }
    return result;
}
#endif
} // namespace

bool OutputBatch::empty() const
{
    return stdout_lines.empty() && stderr_lines.empty();
}

Supervisor::Supervisor() = default;

Supervisor::~Supervisor()
{
    static_cast<void>(request_stop());
#ifdef _WIN32
    close_handles();
#else
    close_fds();
#endif
}

bool Supervisor::can_spawn() const
{
#ifdef _WIN32
    return mut_process_info.hProcess == nullptr;
#else
    return mut_pid == 0;
#endif
}

long Supervisor::run(const std::vector<std::string>& command)
{
    if (command.empty() || !can_spawn())
    {
        return 0;
    }

    const auto spawned = spawn_process(command);
    if (!spawned)
    {
        return 0;
    }

#ifdef _WIN32
    return static_cast<long>(mut_process_info.dwProcessId);
#else
    return static_cast<long>(mut_pid);
#endif
}

long Supervisor::run(const std::function<int()>& action)
{
#ifdef _WIN32
    (void)action;
    return 0;
#else
    if (!can_spawn())
    {
        return 0;
    }

    const auto spawned = spawn_action(action);
    if (!spawned)
    {
        return 0;
    }

    return static_cast<long>(mut_pid);
#endif
}

bool Supervisor::is_running()
{
#ifdef _WIN32
    if (mut_process_info.hProcess == nullptr)
    {
        return false;
    }

    const auto wait_result = WaitForSingleObject(mut_process_info.hProcess, 0);
    if (wait_result == WAIT_TIMEOUT)
    {
        return true;
    }

    read_pipe(mut_stdout_read, mut_stdout_buffer, mut_stdout_pending);
    read_pipe(mut_stderr_read, mut_stderr_buffer, mut_stderr_pending);
    close_handles();
    return false;
#else
    if (mut_pid == 0)
    {
        return false;
    }

    int mut_status = 0;
    const auto wait_result = waitpid(mut_pid, &mut_status, WNOHANG);
    if (wait_result == 0)
    {
        return true;
    }

    if (wait_result == mut_pid)
    {
        read_pipe(mut_stdout_fd, mut_stdout_buffer, mut_stdout_pending);
        read_pipe(mut_stderr_fd, mut_stderr_buffer, mut_stderr_pending);
        mut_pid = 0;
        close_fds();
    }

    return false;
#endif
}

bool Supervisor::request_stop()
{
#ifdef _WIN32
    if (mut_process_info.hProcess == nullptr)
    {
        return false;
    }

    const auto terminated = TerminateProcess(mut_process_info.hProcess, 1) != 0;
    if (terminated)
    {
        WaitForSingleObject(mut_process_info.hProcess, INFINITE);
        read_pipe(mut_stdout_read, mut_stdout_buffer, mut_stdout_pending);
        read_pipe(mut_stderr_read, mut_stderr_buffer, mut_stderr_pending);
        close_handles();
    }
    return terminated;
#else
    if (mut_pid == 0)
    {
        return false;
    }

    const auto result = kill(mut_pid, SIGTERM);
    if (result != 0)
    {
        return false;
    }

    int mut_status = 0;
    const auto wait_result = waitpid(mut_pid, &mut_status, WNOHANG);
    if (wait_result == mut_pid)
    {
        read_pipe(mut_stdout_fd, mut_stdout_buffer, mut_stdout_pending);
        read_pipe(mut_stderr_fd, mut_stderr_buffer, mut_stderr_pending);
        mut_pid = 0;
        close_fds();
    }

    return true;
#endif
}

OutputBatch Supervisor::poll_output()
{
    OutputBatch result;
    result.stdout_lines.swap(mut_stdout_pending);
    result.stderr_lines.swap(mut_stderr_pending);
#ifdef _WIN32
    read_pipe(mut_stdout_read, mut_stdout_buffer, result.stdout_lines);
    read_pipe(mut_stderr_read, mut_stderr_buffer, result.stderr_lines);
#else
    if (mut_stdout_fd != -1)
    {
        read_pipe(mut_stdout_fd, mut_stdout_buffer, result.stdout_lines);
    }
    if (mut_stderr_fd != -1)
    {
        read_pipe(mut_stderr_fd, mut_stderr_buffer, result.stderr_lines);
    }
#endif
    return result;
}

long Supervisor::pid() const
{
#ifdef _WIN32
    return mut_process_info.dwProcessId;
#else
    return static_cast<long>(mut_pid);
#endif
}

bool Supervisor::write(const std::string_view data)
{
#ifdef _WIN32
    if (mut_stdin_write == nullptr || data.empty())
    {
        return false;
    }

    DWORD mut_written = 0;
    const auto ok = WriteFile(mut_stdin_write, data.data(), static_cast<DWORD>(data.size()), &mut_written, nullptr);
    return ok != 0 && mut_written == data.size();
#else
    if (mut_stdin_fd == -1 || data.empty())
    {
        return false;
    }

    const auto mut_written = ::write(mut_stdin_fd, data.data(), data.size());
    return mut_written == static_cast<ssize_t>(data.size());
#endif
}

#ifdef _WIN32
bool Supervisor::spawn_process(const std::vector<std::string>& command)
{
    SECURITY_ATTRIBUTES mut_sa{};
    mut_sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    mut_sa.bInheritHandle = TRUE;
    mut_sa.lpSecurityDescriptor = nullptr;

    HANDLE mut_stdout_read_local = nullptr;
    HANDLE mut_stdout_write_local = nullptr;
    HANDLE mut_stderr_read_local = nullptr;
    HANDLE mut_stderr_write_local = nullptr;
    HANDLE mut_stdin_read_local = nullptr;
    HANDLE mut_stdin_write_local = nullptr;

    if (!CreatePipe(&mut_stdout_read_local, &mut_stdout_write_local, &mut_sa, 0))
    {
        return false;
    }

    if (!SetHandleInformation(mut_stdout_read_local, HANDLE_FLAG_INHERIT, 0))
    {
        CloseHandle(mut_stdout_read_local);
        CloseHandle(mut_stdout_write_local);
        return false;
    }

    if (!CreatePipe(&mut_stderr_read_local, &mut_stderr_write_local, &mut_sa, 0))
    {
        CloseHandle(mut_stdout_read_local);
        CloseHandle(mut_stdout_write_local);
        return false;
    }

    if (!SetHandleInformation(mut_stderr_read_local, HANDLE_FLAG_INHERIT, 0))
    {
        CloseHandle(mut_stdout_read_local);
        CloseHandle(mut_stdout_write_local);
        CloseHandle(mut_stderr_read_local);
        CloseHandle(mut_stderr_write_local);
        return false;
    }

    if (!CreatePipe(&mut_stdin_read_local, &mut_stdin_write_local, &mut_sa, 0))
    {
        CloseHandle(mut_stdout_read_local);
        CloseHandle(mut_stdout_write_local);
        CloseHandle(mut_stderr_read_local);
        CloseHandle(mut_stderr_write_local);
        return false;
    }

    if (!SetHandleInformation(mut_stdin_write_local, HANDLE_FLAG_INHERIT, 0))
    {
        CloseHandle(mut_stdout_read_local);
        CloseHandle(mut_stdout_write_local);
        CloseHandle(mut_stderr_read_local);
        CloseHandle(mut_stderr_write_local);
        CloseHandle(mut_stdin_read_local);
        CloseHandle(mut_stdin_write_local);
        return false;
    }

    STARTUPINFOA mut_startup_info{};
    mut_startup_info.cb = sizeof(STARTUPINFOA);
    mut_startup_info.dwFlags |= STARTF_USESTDHANDLES;
    mut_startup_info.hStdInput = mut_stdin_read_local;
    mut_startup_info.hStdOutput = mut_stdout_write_local;
    mut_startup_info.hStdError = mut_stderr_write_local;

    const auto command_line = build_command_line(command);
    PROCESS_INFORMATION mut_process_details{};

    const auto created = CreateProcessA(
        nullptr,
        const_cast<char*>(command_line.c_str()),
        nullptr,
        nullptr,
        TRUE,
        0,
        nullptr,
        nullptr,
        &mut_startup_info,
        &mut_process_details);

    CloseHandle(mut_stdout_write_local);
    CloseHandle(mut_stderr_write_local);
    CloseHandle(mut_stdin_read_local);

    if (!created)
    {
        CloseHandle(mut_stdout_read_local);
        CloseHandle(mut_stderr_read_local);
        CloseHandle(mut_stdin_write_local);
        return false;
    }

    mut_process_info = mut_process_details;
    mut_stdout_read = mut_stdout_read_local;
    mut_stderr_read = mut_stderr_read_local;
    mut_stdin_write = mut_stdin_write_local;
    return true;
}

void Supervisor::close_handles()
{
    if (mut_process_info.hProcess != nullptr)
    {
        CloseHandle(mut_process_info.hProcess);
        CloseHandle(mut_process_info.hThread);
        mut_process_info = PROCESS_INFORMATION{};
    }

    if (mut_stdout_read != nullptr)
    {
        CloseHandle(mut_stdout_read);
        mut_stdout_read = nullptr;
    }

    if (mut_stderr_read != nullptr)
    {
        CloseHandle(mut_stderr_read);
        mut_stderr_read = nullptr;
    }

    if (mut_stdin_write != nullptr)
    {
        CloseHandle(mut_stdin_write);
        mut_stdin_write = nullptr;
    }
}

void Supervisor::read_pipe(HANDLE pipe_handle, std::string& buffer, std::vector<std::string>& lines)
{
    if (pipe_handle == nullptr)
    {
        return;
    }

    DWORD mut_available = 0;
    const auto peek_result = PeekNamedPipe(pipe_handle, nullptr, 0, nullptr, &mut_available, nullptr);
    if (peek_result == 0 || mut_available == 0)
    {
        return;
    }

    std::array<char, 512> mut_chunk{};
    DWORD mut_read_bytes = 0;
    const auto read_result = ReadFile(pipe_handle, mut_chunk.data(), static_cast<DWORD>(mut_chunk.size()), &mut_read_bytes, nullptr);
    if (read_result == 0 || mut_read_bytes == 0)
    {
        return;
    }

    buffer.append(mut_chunk.data(), static_cast<std::size_t>(mut_read_bytes));

    for (auto mut_newline_pos = buffer.find('\n'); mut_newline_pos != std::string::npos; mut_newline_pos = buffer.find('\n'))
    {
        lines.emplace_back(buffer.substr(0, mut_newline_pos));
        buffer.erase(0, mut_newline_pos + 1);
    }
}

#else
bool Supervisor::spawn_process(const std::vector<std::string>& command)
{
    std::array<int, 2> mut_stdout_pipe{};
    std::array<int, 2> mut_stderr_pipe{};
    std::array<int, 2> mut_stdin_pipe{};

    if (pipe(mut_stdout_pipe.data()) != 0)
    {
        return false;
    }
    if (pipe(mut_stderr_pipe.data()) != 0)
    {
        close(mut_stdout_pipe[0]);
        close(mut_stdout_pipe[1]);
        return false;
    }

    if (pipe(mut_stdin_pipe.data()) != 0)
    {
        close(mut_stdout_pipe[0]);
        close(mut_stdout_pipe[1]);
        close(mut_stderr_pipe[0]);
        close(mut_stderr_pipe[1]);
        return false;
    }

    const auto child_pid = fork();
    if (child_pid == -1)
    {
        close(mut_stdout_pipe[0]);
        close(mut_stdout_pipe[1]);
        close(mut_stderr_pipe[0]);
        close(mut_stderr_pipe[1]);
        close(mut_stdin_pipe[0]);
        close(mut_stdin_pipe[1]);
        return false;
    }

    if (child_pid == 0)
    {
        dup2(mut_stdout_pipe[1], STDOUT_FILENO);
        dup2(mut_stderr_pipe[1], STDERR_FILENO);
        dup2(mut_stdin_pipe[0], STDIN_FILENO);
        close(mut_stdout_pipe[0]);
        close(mut_stdout_pipe[1]);
        close(mut_stderr_pipe[0]);
        close(mut_stderr_pipe[1]);
        close(mut_stdin_pipe[0]);
        close(mut_stdin_pipe[1]);

        std::vector<char*> mut_args;
        mut_args.reserve(command.size() + 1);
        for (const auto& part : command)
        {
            mut_args.push_back(const_cast<char*>(part.c_str()));
        }
        mut_args.push_back(nullptr);

        execvp(mut_args[0], mut_args.data());
        _exit(127);
    }

    close(mut_stdout_pipe[1]);
    close(mut_stderr_pipe[1]);
    close(mut_stdin_pipe[0]);

    if (!make_non_blocking(mut_stdout_pipe[0]) || !make_non_blocking(mut_stderr_pipe[0]))
    {
        close(mut_stdout_pipe[0]);
        close(mut_stderr_pipe[0]);
        close(mut_stdin_pipe[1]);
        return false;
    }

    mut_pid = child_pid;
    mut_stdout_fd = mut_stdout_pipe[0];
    mut_stderr_fd = mut_stderr_pipe[0];
    mut_stdin_fd = mut_stdin_pipe[1];
    return true;
}

bool Supervisor::spawn_action(const std::function<int()>& action)
{
    std::array<int, 2> mut_stdout_pipe{};
    std::array<int, 2> mut_stderr_pipe{};
    std::array<int, 2> mut_stdin_pipe{};

    if (pipe(mut_stdout_pipe.data()) != 0)
    {
        return false;
    }
    if (pipe(mut_stderr_pipe.data()) != 0)
    {
        close(mut_stdout_pipe[0]);
        close(mut_stdout_pipe[1]);
        return false;
    }

    if (pipe(mut_stdin_pipe.data()) != 0)
    {
        close(mut_stdout_pipe[0]);
        close(mut_stdout_pipe[1]);
        close(mut_stderr_pipe[0]);
        close(mut_stderr_pipe[1]);
        return false;
    }

    const auto child_pid = fork();
    if (child_pid == -1)
    {
        close(mut_stdout_pipe[0]);
        close(mut_stdout_pipe[1]);
        close(mut_stderr_pipe[0]);
        close(mut_stderr_pipe[1]);
        close(mut_stdin_pipe[0]);
        close(mut_stdin_pipe[1]);
        return false;
    }

    if (child_pid == 0)
    {
        dup2(mut_stdout_pipe[1], STDOUT_FILENO);
        dup2(mut_stderr_pipe[1], STDERR_FILENO);
        dup2(mut_stdin_pipe[0], STDIN_FILENO);
        close(mut_stdout_pipe[0]);
        close(mut_stdout_pipe[1]);
        close(mut_stderr_pipe[0]);
        close(mut_stderr_pipe[1]);
        close(mut_stdin_pipe[0]);
        close(mut_stdin_pipe[1]);
        const auto exit_code = action();
        _exit(exit_code);
    }

    close(mut_stdout_pipe[1]);
    close(mut_stderr_pipe[1]);
    close(mut_stdin_pipe[0]);

    if (!make_non_blocking(mut_stdout_pipe[0]) || !make_non_blocking(mut_stderr_pipe[0]))
    {
        close(mut_stdout_pipe[0]);
        close(mut_stderr_pipe[0]);
        close(mut_stdin_pipe[1]);
        return false;
    }

    mut_pid = child_pid;
    mut_stdout_fd = mut_stdout_pipe[0];
    mut_stderr_fd = mut_stderr_pipe[0];
    mut_stdin_fd = mut_stdin_pipe[1];
    return true;
}

void Supervisor::close_fds()
{
    if (mut_stdout_fd != -1)
    {
        close(mut_stdout_fd);
        mut_stdout_fd = -1;
    }

    if (mut_stderr_fd != -1)
    {
        close(mut_stderr_fd);
        mut_stderr_fd = -1;
    }

    if (mut_stdin_fd != -1)
    {
        close(mut_stdin_fd);
        mut_stdin_fd = -1;
    }
}

void Supervisor::read_pipe(const int fd, std::string& buffer, std::vector<std::string>& lines)
{
    std::array<char, 512> mut_chunk{};
    while (true)
    {
        const auto bytes_read = read(fd, mut_chunk.data(), mut_chunk.size());
        if (bytes_read > 0)
        {
            buffer.append(mut_chunk.data(), static_cast<std::size_t>(bytes_read));
            for (auto mut_newline_pos = buffer.find('\n'); mut_newline_pos != std::string::npos; mut_newline_pos = buffer.find('\n'))
            {
                lines.emplace_back(buffer.substr(0, mut_newline_pos));
                buffer.erase(0, mut_newline_pos + 1);
            }
            continue;
        }

        if (bytes_read == 0)
        {
            if (!buffer.empty())
            {
                lines.emplace_back(buffer);
                buffer.clear();
            }
            break;
        }

        if (errno == EAGAIN || errno == EWOULDBLOCK)
        {
            break;
        }

        break;
    }
}
#endif
