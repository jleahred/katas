defmodule Wui6.Roles.Role do
  @moduledoc "Schema for system roles."

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :id, autogenerate: true}
  @timestamps_opts [type: :naive_datetime]

  schema "roles" do
    field :name, :string
    field :description, :string
    field :users_count, :integer, virtual: true, default: 0

    timestamps()
  end

  @doc false
  def changeset(role, attrs) do
    role
    |> cast(attrs, [:name, :description])
    |> validate_required([:name])
    |> update_change(:name, &String.trim/1)
    |> validate_length(:name, min: 2, max: 100)
    |> unique_constraint(:name)
  end
end
