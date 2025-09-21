defmodule Wui6.Accounts.SessionRequest do
  @moduledoc """
  Pending session request generated during the email registration flow.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :id, autogenerate: true}
  @foreign_key_type :id

  schema "session_requests" do
    field :email, :string
    field :token, :string

    belongs_to :user, Wui6.Accounts.User

    timestamps(updated_at: false)
  end

  @required_fields ~w[user_id email token]a

  def changeset(request, attrs) do
    request
    |> cast(attrs, @required_fields)
    |> validate_required(@required_fields)
    |> unique_constraint(:token)
    |> foreign_key_constraint(:user_id)
  end
end
