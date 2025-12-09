defmodule Wui7.Accounts.Role do
  use Ecto.Schema
  import Ecto.Changeset

  schema "roles" do
    field :code, :string
    field :description, :string

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(role, attrs) do
    role
    |> cast(attrs, [:code, :description])
    |> update_change(:code, &normalize_code/1)
    |> validate_required([:code, :description])
    |> validate_length(:code, min: 2, max: 40)
    |> validate_length(:description, min: 3, max: 200)
    |> unique_constraint(:code)
  end

  defp normalize_code(code) when is_binary(code) do
    code
    |> String.trim()
    |> String.downcase()
  end

  defp normalize_code(code), do: code
end
