# wkata


## Prepare

```sh
nix-shell -p gleam rebar3 watchexec

gleam new wkata

...

gleam add wisp
gleam add gleam_erlang
gleam add mist

watchexec -r -e gleam --stop-signal SIGKILL --  gleam run

```




----

[![Package Version](https://img.shields.io/hexpm/v/wkata)](https://hex.pm/packages/wkata)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/wkata/)

```sh
gleam add wkata
```
```gleam
import wkata

pub fn main() {
  // TODO: An example of the project in use
}
```

Further documentation can be found at <https://hexdocs.pm/wkata>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```
