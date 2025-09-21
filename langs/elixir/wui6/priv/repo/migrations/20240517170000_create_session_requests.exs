defmodule Wui6.Repo.Migrations.CreateSessionRequests do
  use Ecto.Migration

  def change do
    create table(:session_requests) do
      add :user_id, references(:users, on_delete: :delete_all), null: false
      add :email, :string, null: false
      add :token, :string, null: false

      timestamps(updated_at: false)
    end

    create unique_index(:session_requests, [:token])
    create index(:session_requests, [:user_id])
    create index(:session_requests, [:email])
  end
end
