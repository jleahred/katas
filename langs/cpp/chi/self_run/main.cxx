#include "run_procs.h"

int main(int argc, char** argv) {
    // Nota: marca critical los que realmente quieras que "tiren" del conjunto
    constexpr Subcommand commands[] = {
        { "--fix", "FIX service", &fix::main, true,  true  },  // critical
        { "--db",  "DB service",  &db::main,  true,  false },  // non-critical
        // { "--foo", "Foo worker", &foo::main, false, false }, // ejemplo: no default
    };

    (void)dispatch_or_wrapper(argc, argv, commands);
    return 0;
}
