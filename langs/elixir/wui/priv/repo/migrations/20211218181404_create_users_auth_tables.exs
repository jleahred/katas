defmodule Wui.Repo.Migrations.CreateUsersAuthTables do
  use Ecto.Migration

  def change do
    #
    create table(:users) do
      add :email, :string, null: false, collate: :nocase
      add :hashed_password, :string, null: false
      add :confirmed_at, :naive_datetime
      add :enabled, :boolean, null: false, default: false
      timestamps()
    end

    create unique_index(:users, [:email])

    #
    create table(:users_tokens) do
      add :user_id, references(:users, on_delete: :delete_all), null: false
      add :token, :binary, null: false, size: 32
      add :context, :string, null: false
      add :sent_to, :string
      timestamps(updated_at: false)
    end

    create index(:users_tokens, [:user_id])
    create unique_index(:users_tokens, [:context, :token])

    #
    # create table(:roles) do
    #   add :name, :string, null: false, collate: :nocase
    #   add :description, :string, null: false
    #   timestamps(updated_at: false)
    # end
    # create unique_index(:roles, [:name])

    #
    create table(:users_roles) do
      add :user_id, references(:users, on_delete: :delete_all), null: false
      add :role_id, :integer, null: false
      timestamps(updated_at: false)
    end

    create unique_index(:users_roles, [:user_id, :role_id])
  end
end
