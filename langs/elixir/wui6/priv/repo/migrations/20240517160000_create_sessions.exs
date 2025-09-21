defmodule Wui6.Repo.Migrations.CreateSessions do
  use Ecto.Migration

  def change do
    create table(:sessions) do
      add :user_id, references(:users, on_delete: :delete_all), null: false
      add :token, :string, null: false
      add :last_access, :naive_datetime, null: false
      add :context_info, :string, null: false
    end

    create unique_index(:sessions, [:token])
    create index(:sessions, [:user_id, :last_access])
  end
end
