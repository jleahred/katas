defmodule Wui6.Accounts.UserRole do
  @moduledoc "Join schema between users and roles."

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key false
  schema "user_roles" do
    belongs_to :user, Wui6.Accounts.User
    belongs_to :role, Wui6.Roles.Role

    timestamps(updated_at: false)
  end

  @doc false
  def changeset(user_role, attrs) do
    user_role
    |> cast(attrs, [:user_id, :role_id])
    |> validate_required([:user_id, :role_id])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:role_id)
    |> unique_constraint([:user_id, :role_id], name: :user_roles_user_id_role_id_index)
  end
end
