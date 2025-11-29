defmodule Domain3.Serde.JsonTest do
  use ExUnit.Case
  alias Domain3.Serde.Json

  defmodule Address do
    use Domain3

    definition do
      field(:street, type: :string)
      field(:city, type: :string)
    end
  end

  defmodule Person do
    use Domain3

    definition do
      field(:id, type: :integer)
      field(:name, type: :string)
      field(:birth, type: :date, optional: true)
      field(:score, type: :float, optional: true)
      field(:address, type: {:struct, Address}, optional: true)
    end
  end

  test "to_map encodes date and uses string keys by default" do
    p = %Person{id: 1, name: "Alice", birth: ~D[1990-01-01], score: 3.14}
    map = Domain3.Serde.Json.to_map(p)

    assert map["id"] == 1
    assert map["name"] == "Alice"
    assert map["birth"] == "1990-01-01"
    assert map["score"] == 3.14
  end

  test "from_map builds struct from string keys and decodes date" do
    map = %{"id" => 2, "name" => "Bob", "birth" => "2000-02-02"}
    p = Domain3.Serde.Json.from_map(Person, map)
    assert %Person{id: 2, name: "Bob", birth: ~D[2000-02-02]} = p
  end

  test "from_map raises when required keys missing" do
    map = %{"name" => "NoId"}
    assert_raise ArgumentError, fn -> Domain3.Serde.Json.from_map(Person, map) end
  end

  test "to_map with atom keys returns atom-keyed map" do
    p = %Person{id: 3, name: "C", birth: nil}
    map = Domain3.Serde.Json.to_map(p, string_keys: false)

    assert map[:id] == 3
    assert map[:name] == "C"
    assert map[:birth] == nil
  end

  test "nested struct field serializes and deserializes" do
    addr = %Address{street: "Main St", city: "Metropolis"}
    p = %Person{id: 42, name: "Nested", address: addr}

    map = Domain3.Serde.Json.to_map(p)
    assert map["address"] == %{"street" => "Main St", "city" => "Metropolis"}

    p2 = Domain3.Serde.Json.from_map(Person, map)

    assert %Person{
             id: 42,
             name: "Nested",
             address: %Address{street: "Main St", city: "Metropolis"}
           } = p2
  end
end
