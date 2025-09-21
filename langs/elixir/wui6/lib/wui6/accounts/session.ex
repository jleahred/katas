defmodule Wui6.Accounts.Session do
  @moduledoc """
  Session schema storing authentication tokens per user.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :id, autogenerate: true}
  @foreign_key_type :id
  schema "sessions" do
    field :token, :string
    field :last_access, :naive_datetime
    field :context_info, :string
    field :created_at, :naive_datetime

    belongs_to :user, Wui6.Accounts.User
  end

  @doc false
  def changeset(session, attrs) do
    session
    |> cast(attrs, [:user_id, :token, :last_access, :context_info, :created_at])
    |> validate_required([:user_id, :token, :last_access, :context_info, :created_at])
    |> unique_constraint(:token)
    |> foreign_key_constraint(:user_id)
  end
end
