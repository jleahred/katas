defmodule Wui3Web.Counter.Params do
  use Ecto.Schema
  import Ecto.Changeset

  embedded_schema do
    field :count1, :integer, default: 0
    field :count2, :integer, default: 2
  end

  def changeset(params) do
    %__MODULE__{}
    |> cast(params, [:count1, :count2])
    |> validate_number(:count1, greater_than_or_equal_to: 0)
    |> validate_number(:count2, greater_than_or_equal_to: 2)
  end

  def to_query_params(params = %__MODULE__{}) do
    params
    |> Map.from_struct()
    |> Map.filter(fn {key, _} -> key != :id end)
    |> Enum.into(%{}, fn {key, value} -> {Atom.to_string(key), to_string(value)} end)
    |> URI.encode_query()
  end
end
