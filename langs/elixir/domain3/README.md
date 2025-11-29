# Domain3

A small Elixir helper for declaring data models with metadata and generating structs with enforced required fields.

Domain3 provides a DSL to declare fields and their types, keeps field metadata available at runtime, and includes utilities to serialize/deserialize those structs to/from maps, JSON (via Jason), and query strings.

## Features

- Define structs using `use Domain3` with a `definition` block and `field/2` declarations.
- Field types: `:integer`, `:float`, `:string`, `:date` and nested structs via `{:struct, Module}`.
- Non-optional fields are enforced (must be provided when building the struct).
- Runtime access to field metadata via `__meta_data__/0`.
- Utilities:
  - `Domain3.Serializer` — convert structs to maps/JSON and back (dates are ISO8601 strings).
  - `Domain3.QueryString` — encode/decode structs to/from query strings (nested keys supported).

## Installation

Add to your `mix.exs` deps (if published to Hex):

```elixir
def deps do
  [
    {:domain3, "~> 0.1.0"}
  ]
end
```

For JSON support add Jason:

```elixir
{:jason, "~> 1.4"}
```

## Quick start

Define a schema:

```elixir
defmodule Person do
  use Domain3

  definition do
    field :id, type: :integer
    field :name, type: :string
    field :birth, type: :date, optional: true
    field :address, type: {:struct, Address}, optional: true
  end
end
```

Create and use the struct (required fields must be provided):

```elixir
%Person{id: 1, name: "Alice"}
Person.__meta_data__()
```

Serialize to a map or JSON:

```elixir
Domain3.Serializer.to_map(person)
Domain3.Serializer.to_json(person) # requires Jason
```

Encode as query string and parse back:

```elixir
qs = Domain3.QueryString.to_query(person)
Domain3.QueryString.from_query(Person, qs)
```

## Tests

Run the existing test suite with:

```bash
mix test
```

## Contributing

Issues and PRs are welcome. Keep changes small and include tests for new behavior.

## License

MIT — see LICENSE for details.

