defmodule Domain3.QueryString do
  @moduledoc """
  Encode and decode Domain3 structs to/from query string form.

  - `to_query(struct)` converts a Domain3-backed struct to a query string. Nested
    structs are encoded using bracketed keys (e.g. address[street]=Main).
  - `from_query(module, query)` parses the query string and builds the struct
    using `Domain3.Serde.Json.from_map/2` (so dates and nested structs are handled).
  """

  @doc """
  Convert a Domain3-backed struct to a query string.

  Nil values are skipped. Nested structs are represented with bracketed keys
  (for example `address[street]=Main`). Brackets are not percent-encoded.

  Examples

      iex> defmodule QueryStringExample.Person do
      ...>   use Domain3
      ...>   definition do
      ...>     field :name, type: :string
      ...>     field :age, type: :integer, optional: true
      ...>   end
      ...> end
      iex> p = %QueryStringExample.Person{name: "Alice", age: 30}
      iex> q = Domain3.QueryString.to_query(p)
      iex> is_binary(q)
      true
      iex> Domain3.QueryString.from_query(QueryStringExample.Person, q)
      %QueryStringExample.Person{name: "Alice", age: 30}
  """
  def to_query(struct) when is_struct(struct) do
    map = Domain3.Serde.Json.to_map(struct)
    flat = flatten_map(map)
    # build query manually to avoid percent-encoding brackets
    flat
    |> Enum.map(fn {k, v} -> "#{URI.encode(k)}=#{URI.encode(to_string(v))}" end)
    |> Enum.join("&")
  end

  @doc """
  Parse a query string and build a struct of the given Domain3 module.

  The query string is first decoded into a flat map, then nested keys using
  bracket notation (e.g. `address[street]=Main`) are rebuilt into maps and
  finally `Domain3.Serde.Json.from_map/2` is used to build the struct and
  convert field types (dates, integers, nested structs, etc.).

  Examples

      iex> q = "name=Alice&age=30"
      iex> defmodule QueryStringExample.Person2 do
      ...>   use Domain3
      ...>   definition do
      ...>     field :name, type: :string
      ...>     field :age, type: :integer, optional: true
      ...>   end
      ...> end
      iex> Domain3.QueryString.from_query(QueryStringExample.Person2, q)
      %QueryStringExample.Person2{name: "Alice", age: 30}
  """
  def from_query(module, query) when is_atom(module) and is_binary(query) do
    raw = URI.decode_query(query)
    nested = rebuild_nested_map(raw)
    Domain3.Serde.Json.from_map(module, nested)
  end

  @doc false
  defp flatten_map(map, parent_key \\ nil) when is_map(map) do
    Enum.flat_map(map, fn {k, v} ->
      key = if parent_key, do: "#{parent_key}[#{k}]", else: to_string(k)

      cond do
        is_map(v) -> flatten_map(v, key)
        v == nil -> []
        true -> [{key, v}]
      end
    end)
  end

  @doc false
  defp rebuild_nested_map(flat_map) when is_map(flat_map) do
    Enum.reduce(flat_map, %{}, fn {key, val}, acc ->
      segments = extract_segments(key)
      insert_nested(acc, segments, val)
    end)
  end

  @doc false
  # extract segments from keys like "address[street][name]" -> ["address","street","name"]
  defp extract_segments(key) when is_binary(key) do
    Regex.scan(~r/([^\[\]]+)/, key)
    |> Enum.map(&hd/1)
  end

  @doc false
  defp insert_nested(map, [seg], val) do
    Map.put(map, seg, val)
  end

  @doc false
  defp insert_nested(map, [seg | rest], val) do
    current = Map.get(map, seg, %{})
    updated = insert_nested(current, rest, val)
    Map.put(map, seg, updated)
  end
end
