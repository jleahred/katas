#include <string>
#include <vector>

namespace chi::self_run {

struct Subcommand {
    std::string_view flag; // "--fix"
    std::string_view help; // descripción corta
    int (*main_fn)(int, char**);
    bool launch_by_default = true;  // wrapper default set
    bool critical          = false; // si cae, intentamos parar el resto
};

bool dispatch_or_wrapper(int argc, char** argv, const std::vector<Subcommand>& cmds);

} // namespace chi::self_run

//
//
//  ejemplo simple
//

// int main(int argc, char** argv) {
//     // Nota: marca critical los que realmente quieras que "tiren" del conjunto
//     constexpr Subcommand commands[] = {
//         { "--fix", "FIX service", &fix::main, true,  true  },  // critical
//         { "--db",  "DB service",  &db::main,  true,  false },  // non-critical
//         // { "--foo", "Foo worker", &foo::main, false, false }, // ejemplo: no default
//     };

//     (void)dispatch_or_wrapper(argc, argv, commands);
//     return 0;
// }
