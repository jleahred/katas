defmodule Wui2.UsersRoles do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users_roles" do
    belongs_to :user, Wui2.Accounts.User
    field :role_id, :integer

    timestamps(updated_at: false)
  end

  @doc false
  def changeset(users_roles, attrs) do
    users_roles
    |> cast(attrs, [:user_id, :role_id])
    |> validate_required([:user_id, :role_id])
    |> unique_constraint([:user_id, :role_id])
    |> foreign_key_constraint(:user_id)
  end
end
