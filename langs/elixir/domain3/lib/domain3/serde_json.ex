defmodule Domain3.Serde.Json do
  @moduledoc """
  Utilities to convert structs defined with Domain3 to maps/JSON and back using the
  metadata produced by the `Domain3` macros.

  - `to_map/2` converts a struct to a plain map, encoding Date fields as ISO8601 strings.
  - `from_map/2` builds a struct from a map (accepts string or atom keys for fields).
  - `to_json/1` and `from_json/2` use Jason when available; otherwise they raise with
    instructions to add a JSON library.
  """

  @doc """
  Convert a Domain3-backed struct to a map.

  By default keys are strings for JSON compatibility. Date fields are encoded as
  ISO8601 strings and nested Domain3 structs are converted to nested maps.

  Examples

      iex> defmodule SerdeExample.Person do
      ...>   use Domain3
      ...>   definition do
      ...>     field :name, type: :string
      ...>     field :age, type: :integer, optional: true
      ...>   end
      ...> end
      iex> p = %SerdeExample.Person{name: "Alice", age: 30}
      iex> m = Domain3.Serde.Json.to_map(p)
      iex> m["name"]
      "Alice"
      iex> m["age"]
      30
  """
  def to_map(struct, opts \\ []) when is_struct(struct) do
    string_keys = Keyword.get(opts, :string_keys, true)

    definition = struct.__struct__.__meta_data__()

    Enum.reduce(definition, %{}, fn %{field: f, type: t}, acc ->
      val = Map.get(struct, f)
      enc = encode_field_to_value(val, t)

      if string_keys do
        Map.put(acc, to_string(f), enc)
      else
        Map.put(acc, f, enc)
      end
    end)
  end

  @doc "Encode a struct to JSON string. Requires Jason in the project dependencies."
  def to_json(struct) when is_struct(struct) do
    if Code.ensure_loaded?(Jason) and function_exported?(Jason, :encode!, 1) do
      apply(Jason, :encode!, [to_map(struct, string_keys: true)])
    else
      raise "no JSON library available. Add {:jason, \"~> 1.4\"} to deps or use Domain3.Serde.Json.to_map/1"
    end
  end

  defp encode_field_to_value(nil, _type), do: nil
  defp encode_field_to_value(%Date{} = d, :date), do: Date.to_iso8601(d)

  defp encode_field_to_value(%struct_mod{} = s, {:struct, struct_mod}) when is_atom(struct_mod) do
    to_map(s)
  end

  defp encode_field_to_value(v, _type), do: v

  @doc """
  Build a Domain3-backed struct from a map.

  Accepts atom or string keys. Field values are decoded according to the field
  types declared in the Domain3 definition (dates, integers, floats and nested structs).

  Examples

      iex> defmodule SerdeExample.Person2 do
      ...>   use Domain3
      ...>   definition do
      ...>     field :name, type: :string
      ...>     field :age, type: :integer, optional: true
      ...>   end
      ...> end
      iex> Domain3.Serde.Json.from_map(SerdeExample.Person2, %{"name" => "Bob", "age" => 25})
      %SerdeExample.Person2{name: "Bob", age: 25}
  """
  def from_map(module, map) when is_atom(module) and is_map(map) do
    definition = module.__meta_data__()

    data =
      Enum.reduce(definition, %{}, fn %{field: f, type: t, optional: opt}, acc ->
        value_present? = Map.has_key?(map, f) or Map.has_key?(map, to_string(f))

        cond do
          value_present? ->
            raw = Map.get(map, f, Map.get(map, to_string(f)))
            Map.put(acc, f, decode_field_from_value(raw, t))

          opt ->
            Map.put(acc, f, nil)

          true ->
            acc
        end
      end)

    # use module.__struct__/1 to construct so any enforcement in Domain3 is applied
    module.__struct__(data)
  end

  @doc "Decode JSON to a struct of module. Requires Jason in the project dependencies."
  def from_json(module, json) when is_atom(module) and is_binary(json) do
    if Code.ensure_loaded?(Jason) and function_exported?(Jason, :decode!, 1) do
      map = apply(Jason, :decode!, [json])
      from_map(module, map)
    else
      raise "no JSON library available. Add {:jason, \"~> 1.4\"} to deps or use Domain3.Serde.Json.from_map/2"
    end
  end

  defp decode_field_from_value(nil, _type), do: nil

  defp decode_field_from_value(value, :date) when is_binary(value) do
    case Date.from_iso8601(value) do
      {:ok, d} -> d
      {:error, _} -> raise ArgumentError, "invalid date string: #{inspect(value)}"
    end
  end

  defp decode_field_from_value(value, :integer) when is_binary(value) do
    case Integer.parse(value) do
      {int, ""} -> int
      _ -> raise ArgumentError, "invalid integer value: #{inspect(value)}"
    end
  end

  defp decode_field_from_value(value, :float) when is_binary(value) do
    case Float.parse(value) do
      {f, ""} -> f
      _ -> raise ArgumentError, "invalid float value: #{inspect(value)}"
    end
  end

  defp decode_field_from_value(value, {:struct, struct_mod})
       when is_map(value) and is_atom(struct_mod) do
    from_map(struct_mod, value)
  end

  defp decode_field_from_value(v, _type), do: v
end
