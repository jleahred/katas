defmodule Domain3Test do
  use ExUnit.Case

  defmodule Sample do
    use Domain3

    definition do
      field(:id, type: :integer)
      field(:name, type: :string)
      field(:price, type: :float, optional: true)
      field(:birth, type: :date, optional: true)
    end
  end

  defmodule Addr2 do
    use Domain3

    definition do
      field(:street, type: :string)
    end
  end

  defmodule User2 do
    use Domain3

    definition do
      field(:id, type: :integer)
      field(:address, type: {:struct, Addr2})
    end
  end

  test "struct raises when required non-optional fields are missing" do
    assert_raise ArgumentError, fn ->
      Sample.__struct__(%{})
    end
  end

  test "can construct struct when required keys are provided and optional defaults apply" do
    s = %Sample{id: 1, name: "John"}
    assert s.id == 1
    assert s.name == "John"
    assert s.price == nil
    assert s.birth == nil
  end

  test "__meta_data__ returns field metadata in definition order" do
    assert Sample.__meta_data__() == [
             %{field: :id, type: :integer, optional: false},
             %{field: :name, type: :string, optional: false},
             %{field: :price, type: :float, optional: true},
             %{field: :birth, type: :date, optional: true}
           ]
  end

  test "can assign and preserve a Date value in the birth field" do
    date = ~D[1990-05-23]
    s = %Sample{id: 1, name: "Jane", birth: date}
    assert s.birth == date
  end

  test "field can be a Domain3 struct and is enforced" do
    # missing required keys should raise
    assert_raise ArgumentError, fn -> User2.__struct__(%{}) end

    addr = %Addr2{street: "Main"}
    u = %User2{id: 10, address: addr}
    assert u.address.street == "Main"

    assert User2.__meta_data__() == [
             %{field: :id, type: :integer, optional: false},
             %{field: :address, type: {:struct, Addr2}, optional: false}
           ]
  end

  test "invalid field type raises at compile time" do
    code = """
    defmodule BadTypeModule do
      use Domain3

      definition do
        field :x, type: :unknown_type
      end
    end
    """

    assert_raise ArgumentError, fn ->
      Code.compile_string(code)
    end
  end
end
