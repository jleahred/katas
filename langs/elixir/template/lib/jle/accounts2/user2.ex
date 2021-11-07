defmodule Jle.Accounts2.User2 do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users2" do
    field :age, :integer
    field :name, :string

    timestamps()
  end

  @doc false
  def changeset(user2, attrs) do
    user2
    |> cast(attrs, [:name, :age])
    |> validate_required([:name, :age])
  end
end
