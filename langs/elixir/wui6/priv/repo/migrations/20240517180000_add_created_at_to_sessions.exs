defmodule Wui6.Repo.Migrations.AddCreatedAtToSessions do
  use Ecto.Migration

  def up do
    rename table(:sessions), to: table(:sessions_old)

    create table(:sessions) do
      add :user_id, references(:users, on_delete: :delete_all), null: false
      add :token, :string, null: false
      add :last_access, :naive_datetime, null: false
      add :context_info, :string, null: false
      add :created_at, :naive_datetime, null: false, default: fragment("CURRENT_TIMESTAMP")
    end

    execute "INSERT INTO sessions (id, user_id, token, last_access, context_info, created_at) " \
            <> "SELECT id, user_id, token, last_access, context_info, " \
            <> "COALESCE(created_at_guess, CURRENT_TIMESTAMP) FROM (" \
            <> "SELECT id, user_id, token, last_access, context_info, " \
            <> "COALESCE(last_access, CURRENT_TIMESTAMP) AS created_at_guess FROM sessions_old" \
            <> ")"

    execute "DROP INDEX IF EXISTS sessions_user_id_last_access_index"
    execute "DROP INDEX IF EXISTS sessions_token_index"

    drop table(:sessions_old)

    create unique_index(:sessions, [:token])
    create index(:sessions, [:user_id, :last_access])
    create index(:sessions, [:created_at])
  end

  def down do
    rename table(:sessions), to: table(:sessions_new)

    create table(:sessions) do
      add :user_id, references(:users, on_delete: :delete_all), null: false
      add :token, :string, null: false
      add :last_access, :naive_datetime, null: false
      add :context_info, :string, null: false
    end

    execute "INSERT INTO sessions (id, user_id, token, last_access, context_info) " \
            <> "SELECT id, user_id, token, last_access, context_info FROM sessions_new"

    execute "DROP INDEX IF EXISTS sessions_user_id_last_access_index"
    execute "DROP INDEX IF EXISTS sessions_token_index"
    execute "DROP INDEX IF EXISTS sessions_created_at_index"

    drop table(:sessions_new)

    create unique_index(:sessions, [:token])
    create index(:sessions, [:user_id, :last_access])
  end
end
