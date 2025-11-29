#include "supervisor.h"

#include <chrono>
#include <filesystem>
#include <iostream>
#include <random>
#include <string>
#include <string_view>
#include <thread>
#include <vector>

namespace
{
void run_self(const std::string_view name)
{
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<int> dist(200, 600);

    for (int mut_index = 0; mut_index < 10; ++mut_index)
    {
        std::cout << "I am " << name << " (" << mut_index + 1 << "/10)" << std::endl;
        const auto mut_delay_ms = dist(gen);
        std::this_thread::sleep_for(std::chrono::milliseconds(mut_delay_ms));
    }
    std::cout << "Goodbye, " << name << std::endl;
}
} // namespace

int main(const int argc, char* argv[])
{
    if (argc > 1 && std::string_view(argv[1]) == "--self")
    {
        const auto name = argc > 2 ? std::string_view(argv[2]) : std::string_view{"anonymous"};
        run_self(name);
        return 0;
    }

    const auto self_path = std::filesystem::absolute(argv[0]).string();
    struct Runner
    {
        std::string label;
        Supervisor supervisor;
    };

    Runner mut_first{"one", {}};
    Runner mut_second{"second", {}};

    const auto first_pid = mut_first.supervisor.run({self_path, "--self", mut_first.label});
    const auto second_pid = mut_second.supervisor.run({self_path, "--self", mut_second.label});

    if (first_pid == 0 || second_pid == 0)
    {
        std::cerr << "Processes could not be started" << std::endl;
        return 1;
    }

    std::cout << "PID (" << mut_first.label << "): " << first_pid << std::endl;
    std::cout << "PID (" << mut_second.label << "): " << second_pid << std::endl;

    while (mut_first.supervisor.is_running() || mut_second.supervisor.is_running())
    {
        const auto first_batch = mut_first.supervisor.poll_output();
        for (const auto& line : first_batch.stdout_lines)
        {
            std::cout << "[" << mut_first.label << " stdout] " << line << std::endl;
        }
        for (const auto& line : first_batch.stderr_lines)
        {
            std::cout << "[" << mut_first.label << " stderr] " << line << std::endl;
        }

        const auto second_batch = mut_second.supervisor.poll_output();
        for (const auto& line : second_batch.stdout_lines)
        {
            std::cout << "[" << mut_second.label << " stdout] " << line << std::endl;
        }
        for (const auto& line : second_batch.stderr_lines)
        {
            std::cout << "[" << mut_second.label << " stderr] " << line << std::endl;
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }

    const auto final_first = mut_first.supervisor.poll_output();
    for (const auto& line : final_first.stdout_lines)
    {
        std::cout << "[" << mut_first.label << " stdout] " << line << std::endl;
    }
    for (const auto& line : final_first.stderr_lines)
    {
        std::cout << "[" << mut_first.label << " stderr] " << line << std::endl;
    }

    const auto final_second = mut_second.supervisor.poll_output();
    for (const auto& line : final_second.stdout_lines)
    {
        std::cout << "[" << mut_second.label << " stdout] " << line << std::endl;
    }
    for (const auto& line : final_second.stderr_lines)
    {
        std::cout << "[" << mut_second.label << " stderr] " << line << std::endl;
    }

    return 0;
}
