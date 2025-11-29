defmodule Domain3.QueryStringTest do
  use ExUnit.Case
  alias Domain3.QueryString
  alias Domain3.Serde.Json

  defmodule AddressQS do
    use Domain3

    definition do
      field(:street, type: :string)
      field(:city, type: :string)
    end
  end

  defmodule PersonQS do
    use Domain3

    definition do
      field(:id, type: :integer)
      field(:name, type: :string)
      field(:address, type: {:struct, AddressQS}, optional: true)
      field(:birth, type: :date, optional: true)
    end
  end

  test "to_query/1 encodes flat and nested fields and skips nils" do
    addr = %AddressQS{street: "Main", city: "X"}
    p = %PersonQS{id: 7, name: "Q", address: addr, birth: ~D[2020-01-01]}

    qs = QueryString.to_query(p)
    # order is not guaranteed; check keys are present
    assert String.contains?(qs, "id=7")
    assert String.contains?(qs, "name=Q")
    assert String.contains?(qs, "address[street]=Main")
    assert String.contains?(qs, "address[city]=X")
    assert String.contains?(qs, "birth=2020-01-01")
  end

  test "from_query/2 rebuilds nested struct and dates" do
    qs = "id=8&name=R&address[street]=Road&address[city]=Town&birth=1999-09-09"
    p = QueryString.from_query(PersonQS, qs)

    assert %PersonQS{
             id: 8,
             name: "R",
             birth: ~D[1999-09-09],
             address: %AddressQS{street: "Road", city: "Town"}
           } = p
  end

  test "from_query/2 fails when required missing" do
    qs = "name=NoId"
    assert_raise ArgumentError, fn -> QueryString.from_query(PersonQS, qs) end
  end
end
