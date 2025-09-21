defmodule Wui6.Accounts.User do
  @moduledoc """
  Lightweight user schema used for listing accounts in the admin area.

  The schema is intentionally minimal so it can back both real database
  records and the in-memory fallback data used during development.
  """

  use Ecto.Schema

  @primary_key {:id, :id, autogenerate: true}
  @timestamps_opts [type: :naive_datetime]

  schema "users" do
    field :email, :string
    field :enabled, :boolean, default: true
    field :last_activity, :naive_datetime

    many_to_many :roles, Wui6.Roles.Role,
      join_through: "user_roles",
      on_replace: :delete

    timestamps()
  end
end
