#include "self_run.h"

#include <algorithm>
#include <chrono>
#include <deque>
#include <filesystem>
#include <iostream>
#include <optional>
#include <string_view>
#include <thread>
#include <vector>

#include "experimental/chi/process/process.h"

// ------------------------------------------------------------

namespace {

using namespace chi::self_run;

std::string tag_from_flag(std::string_view flag)
{
    // "--fix" -> "fix"
    if (flag.size() >= 2 && flag[0] == '-' && flag[1] == '-')
        return std::string(flag.substr(2));
    return std::string(flag);
}

std::vector<std::string> split_csv(std::string_view s)
{
    std::vector<std::string> out;
    size_t                   i = 0;
    while (i < s.size()) {
        size_t j = s.find(',', i);
        if (j == std::string_view::npos)
            j = s.size();
        auto part = s.substr(i, j - i);

        // trim básico
        while (!part.empty() && part.front() == ' ')
            part.remove_prefix(1);
        while (!part.empty() && part.back() == ' ')
            part.remove_suffix(1);

        if (!part.empty())
            out.emplace_back(part);
        i = j + 1;
    }
    return out;
}

struct WrapperArgs {
    std::optional<std::string> config_path; // primer positional
    std::vector<std::string>   only_tags;   // si no vacío => modo --only
    bool                       only_requested = false;
    bool                       help           = false;
};

// wrapper args admite:
//   prog [config] [--only fix,db] [--help]
//   prog --only fix,db [config]
//   prog --help
WrapperArgs parse_wrapper_args(int argc, char** argv)
{
    WrapperArgs w;

    for (int i = 1; i < argc; ++i) {
        std::string_view a = argv[i];

        if (a == "--help" || a == "-h") {
            w.help = true;
            continue;
        }

        if (a == "--only") {
            w.only_requested = true;
            if (i + 1 < argc && argv[i + 1][0] != '-') {
                auto items = split_csv(argv[i + 1]);
                w.only_tags.insert(w.only_tags.end(), items.begin(), items.end());
                ++i;
            }
            continue;
        }

        // ignoramos flags desconocidos del wrapper (puedes cambiarlo a error si prefieres)
        if (!a.empty() && a[0] == '-') {
            continue;
        }

        // positional: config
        if (!w.config_path)
            w.config_path = std::string(a);
    }

    // dedup
    std::sort(w.only_tags.begin(), w.only_tags.end());
    w.only_tags.erase(std::unique(w.only_tags.begin(), w.only_tags.end()), w.only_tags.end());

    return w;
}

std::optional<size_t> find_subcommand_by_flag(const std::vector<Subcommand>& cmds, std::string_view flag)
{
    for (size_t i = 0; i < cmds.size(); ++i) {
        if (cmds[i].flag == flag)
            return i;
    }
    return std::nullopt;
}

std::optional<size_t> find_subcommand_by_tag(const std::vector<Subcommand>& cmds, std::string_view tag)
{
    for (size_t i = 0; i < cmds.size(); ++i) {
        if (tag_from_flag(cmds[i].flag) == tag)
            return i;
    }
    return std::nullopt;
}

void print_help(std::string_view bin, const std::vector<Subcommand>& cmds)
{
    std::cout
        << "Usage:\n"
        << "  " << bin << " [config] [--only a,b,c]\n"
        << "  " << bin << " --only a,b,c [config]\n"
        << "  " << bin << " --version\n"
        << "  " << bin << " --<target> <config>\n"
        << "\n"
        << "Wrapper mode:\n"
        << "  - Without --<target>, the program launches one or more targets as subprocesses.\n"
        << "  - If --only is present, launches only the listed targets (comma-separated).\n"
        << "\n"
        << "Subcommand mode:\n"
        << "  - Use --<target> <config> to execute the target main() directly.\n"
        << "\n"
        << "Targets:\n";

    for (auto const& c : cmds) {
        const auto tag = tag_from_flag(c.flag);
        std::cout << "  " << tag
                  << "  (" << c.flag << ")"
                  << (c.launch_by_default ? " [default]" : "")
                  << (c.critical ? " [critical]" : "")
                  << " - " << c.help << "\n";
    }

    std::cout << "\nExamples:\n"
              << "  " << bin << "\n"
              << "  " << bin << " /path/to/config.cfg\n"
              << "  " << bin << " --only fix,db /path/to/config.cfg\n"
              << "  " << bin << " --fix /path/to/config.cfg\n";
}

void drain_and_print(chi::proc::Process& p, std::string_view tag)
{
    const auto batch = p.poll_output();
    for (const auto& line : batch.stdout_lines) {
        std::cout << "[" << tag << " stdout] " << line << "\n";
    }
    for (const auto& line : batch.stderr_lines) {
        std::cerr << "[" << tag << " stderr] " << line << "\n";
    }
}

// bool any_running(std::vector<chi::proc::Process*>& procs)
// {
//     for (auto* p : procs)
//         if (p && p->is_running())
//             return true;
//     return false;
// }

} // namespace

namespace chi::self_run {

// Devuelve true si consumió la ejecución (dispatch o wrapper)
bool dispatch_or_wrapper(int argc, char** argv, const std::vector<Subcommand>& cmds)
{
    const std::string bin = (argc > 0 && argv && argv[0]) ? std::string(argv[0]) : "program";

    // ---- help (wrapper)
    // Si el usuario pide help, lo mostramos aunque la invocación no sea perfecta
    for (int i = 1; i < argc; ++i) {
        std::string_view a = argv[i];
        if (a == "--help" || a == "-h") {
            print_help(bin, cmds);
            return true;
        }
    }

    // ---- --version (llama a todos los main)
    if (argc == 2 && std::string_view(argv[1]) == "--version") {
        for (auto const& c : cmds) {
            c.main_fn(argc, argv);
        }
        return true;
    }
    // if (argc == 2 && std::string_view(argv[1]) == "--version") {
    //     const auto self_path = std::filesystem::absolute(argv[0]).string();

    //     for (auto const& c : cmds) {
    //         chi::proc::Process p;
    //         p.run({ self_path, std::string(c.flag), "--version" });
    //         while (p.is_running()) {
    //             drain_and_print(p, tag_from_flag(c.flag));
    //         }
    //         drain_and_print(p, tag_from_flag(c.flag));
    //     }
    //     return true;
    // }

    // ---- subcommand mode:  <prog> --fix <param>
    if (argc == 3) {
        const auto maybe = find_subcommand_by_flag(cmds, std::string_view(argv[1]));
        if (maybe) {
            // Reescribe argv para que el sub-main vea argc=2 y argv[1]=param
            chi::proc::Argv args{ std::vector{ std::string{ argv[2] } } };
            cmds[*maybe].main_fn(args.argc(), args.argv());
            return true;
        }
    }

    // ---- si alguien pasa --fix/--db pero argc != 3 => error
    if (argc > 1) {
        const auto maybe = find_subcommand_by_flag(cmds, std::string_view(argv[1]));
        if (maybe) {
            std::cout << "invalid number of arguments cancelling\n";
            return true;
        }
    }

    // ---- wrapper mode: lanza N subprocesos
    const auto self_path = std::filesystem::absolute(argv[0]).string();
    const auto w         = parse_wrapper_args(argc, argv);

    // Validación de --only (sin tocar argv[])
    if (w.only_requested && w.only_tags.empty()) {
        std::cerr
            << "error: --only requires a comma-separated list, e.g. --only fix,db\n";
        return true; // consumimos la ejecución
    }

    if (w.help) {
        print_help(bin, cmds);
        return true;
    }

    // Validación de tags en --only
    if (!w.only_tags.empty()) {
        for (auto const& t : w.only_tags) {
            if (!find_subcommand_by_tag(cmds, t)) {
                std::cerr << "error: unknown target in --only: '" << t << "'\n";
                std::cerr << "known targets:";
                for (auto const& c : cmds)
                    std::cerr << " " << tag_from_flag(c.flag);
                std::cerr << "\n";
                return true;
            }
        }
    }

    const std::string param = w.config_path
                                  ? *w.config_path
                                  : "";

    auto should_launch = [&](Subcommand const& c) -> bool {
        if (!w.only_tags.empty()) {
            const auto tag = tag_from_flag(c.flag);
            return std::find(w.only_tags.begin(), w.only_tags.end(), tag) != w.only_tags.end();
        }
        return c.launch_by_default;
    };

    struct Child {
        const Subcommand*  cmd;
        std::string        tag;
        chi::proc::Process proc;
        bool               was_running;

        Child(const Subcommand* c,
            std::string         t,
            const std::string&  self_path,
            const std::string&  param)
            : cmd(c)
            , tag(std::move(t))
            , proc()
            , was_running(true)
        {
            proc.run({ self_path, std::string(cmd->flag), param });
        }

        Child(const Child&)            = delete;
        Child& operator=(const Child&) = delete;
        Child(Child&&)                 = delete;
        Child& operator=(Child&&)      = delete;
    };

    std::deque<Child> children;

    for (auto const& c : cmds) {
        if (!should_launch(c))
            continue;
        children.emplace_back(&c, tag_from_flag(c.flag), self_path, param);
    }

    if (children.empty()) {
        std::cerr << "error: no targets selected to run.\n";
        return true;
    }

    auto request_stop_all_except = [&](size_t except_i) {
        for (size_t i = 0; i < children.size(); ++i) {
            if (i == except_i)
                continue;
            (void)children[i].proc.request_stop();
        }
    };

    auto kill_all_except = [&](size_t except_i) {
        for (size_t i = 0; i < children.size(); ++i) {
            if (i == except_i)
                continue;
            if (children[i].proc.is_running()) {
                (void)children[i].proc.kill();
            }
        }
    };

    bool   abort_due_to_critical = false;
    size_t critical_index        = static_cast<size_t>(-1);

    // Poll loop
    while (true) {
        bool any = false;

        for (size_t i = 0; i < children.size(); ++i) {
            auto& ch = children[i];

            const bool running = ch.proc.is_running();
            any |= running;

            drain_and_print(ch.proc, ch.tag);

            // Detectar transición running -> stopped
            if (ch.was_running && !running) {
                ch.was_running = false;

                if (ch.cmd->critical) {
                    std::cerr << "[supervisor] critical target '" << ch.tag
                              << "' stopped; requesting stop for the rest...\n";
                    request_stop_all_except(i);
                    abort_due_to_critical = true;
                    critical_index        = i;
                }
            }
        }

        if (abort_due_to_critical)
            break;
        if (!any)
            break;

        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }

    // Si abortamos por critical, damos una ventana para apagado limpio y luego kill
    if (abort_due_to_critical) {
        const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(2);

        while (std::chrono::steady_clock::now() < deadline) {
            bool still = false;
            for (size_t i = 0; i < children.size(); ++i) {
                if (i == critical_index)
                    continue;
                if (children[i].proc.is_running())
                    still = true;
                drain_and_print(children[i].proc, children[i].tag);
            }
            if (!still)
                break;
            std::this_thread::sleep_for(std::chrono::milliseconds(50));
        }

        // Kill duro si queda alguien
        kill_all_except(critical_index);
    }

    // Drenaje final
    for (auto& ch : children) {
        drain_and_print(ch.proc, ch.tag);
    }

    return true;
}

} // namespace chi::self_run
